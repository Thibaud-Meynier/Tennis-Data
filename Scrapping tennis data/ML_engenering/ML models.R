library(tidyverse)
library(caret)
library(glmnet)    
library(randomForest)
library(e1071)
library(scales)
library(class)       
library(gbm)         
library(lightgbm)    
library(pROC)
library(plotly)
library(nnet)
library(MASS)
library(MLmetrics)
library(xgboost)
library(NeuralNetTools)
library(ranger)
library(kknn)

features_mix <- c("P_F","P_s_F",
                  "Grass","Clay","Hard",
                  "Indoors",
                  "ATP_1000",
                  "ATP_250",
                  "ATP_500",
                  #"Grand_Slam",
                  "Diff_Rank",
                  "Diff_Age",
                  "Diff_Country_score","Diff_Hand_Score"
                  )


formule <- as.formula(paste0("Issue ~ ", paste0(features_mix, collapse = " + ")))

# Division train/test
set.seed(456) 
TABLE_MOMENTUM = na.omit(TABLE_MOMENTUM)

TABLE_MOMENTUM = TABLE_MOMENTUM %>% filter(Season>=2017)

#TABLE_MOMENTUM=TABLE_MOMENTUM %>% filter(Grand_Slam!=1)

index_train <- which(TABLE_MOMENTUM$Season < 2024) #createDataPartition(TABLE_MOMENTUM$Issue, p = 0.8, list = FALSE) 
train <- TABLE_MOMENTUM[index_train, ]
test <- TABLE_MOMENTUM[-index_train, ]
test_pred=TABLE_MOMENTUM[-index_train, ]
test_pred=test_pred %>% select(tournament,Surface_tournament,Categorie,Season,Round,Favori,Outsider,Odd_F,Odd_O,Issue,Diff_Rank,P_F_comb)

# Préparation des matrices pour glmnet
X_train <- model.matrix(formule, train %>% select(all_of(c(features_mix, "Issue"))))[, -1]
y_train <- ifelse(train$Issue=="Fav_W",1,0)
X_test <- model.matrix(formule, test %>% select(all_of(c(features_mix, "Issue"))))[, -1]
y_test <- ifelse(test$Issue=="Fav_W",1,0)


model_metrics <- function(prob_pred, truth, positive_class = "Fav_W", threshold = 0.5) {
  
  # Préparation
  pred_class <- as.factor(ifelse(prob_pred >= threshold, positive_class,
                                 setdiff(unique(truth), positive_class)))
  truth_fac  <- as.factor(truth)
  truth_bin  <- as.numeric(truth == positive_class)
  
  # Confusion matrix
  conf <- confusionMatrix(pred_class, truth_fac, positive = positive_class)
  auc  <- roc(truth_bin, prob_pred, quiet = TRUE)$auc
  
  # Affichage matrice
  cat("=== Matrice de Confusion ===\n")
  print(conf$table)
  cat("\n")
  
  # Métriques
  metrics <- data.frame(
    Metric = c("Accuracy", "Sensitivity", "Specificity",
               "PPV", "NPV", "F1",
               "Balanced_Accuracy", "AUC", "LogLoss", "Brier"),
    Value  = round(c(
      conf$overall["Accuracy"],
      conf$byClass["Sensitivity"],
      conf$byClass["Specificity"],
      conf$byClass["Pos Pred Value"],
      conf$byClass["Neg Pred Value"],
      conf$byClass["F1"],
      conf$byClass["Balanced Accuracy"],
      as.numeric(auc),
      LogLoss(prob_pred, truth_bin),
      mean((prob_pred - truth_bin)^2)
    ), 4)
  )
  
  cat("=== Métriques ===\n")
  print(metrics, row.names = FALSE)
  
  # Retourner invisiblement pour réutilisation
  invisible(list(
    confusion_matrix = conf$table,
    metrics          = metrics,
    conf_full        = conf
  ))
}

Start=Sys.time()

# ============================================
# 0. P_F_comb (elo_based)
# ============================================

pred_elo=test$P_F_comb

model_metrics(test_pred[["P_F_comb"]], test$Issue)

# ============================================
# 1. LOGIT LASSO
# ============================================
model_lasso <- cv.glmnet(X_train, y_train, 
                         family = "binomial", 
                         alpha = 1,  # alpha=1 pour lasso
                         type.measure = "deviance")

var_lasso=coef(model_lasso, s = "lambda.min")

pred_lasso <- predict(model_lasso, X_test, 
                      s = "lambda.min", 
                      type = "response")

test_pred$LASSO=pred_lasso[,1]

model_metrics(test_pred[["LASSO"]], test$Issue)

# ============================================
# 2. ELASTIC NET
# ============================================
model_elastic <- cv.glmnet(X_train, y_train, 
                           family = "binomial", 
                           alpha = 0.5,  # alpha=0.5 pour elastic net
                           type.measure = "deviance")

var_elastic=coef(model_elastic, s = "lambda.min")

pred_elastic <- predict(model_elastic, X_test, 
                        s = "lambda.min", 
                        type = "response")

test_pred$Elastic=pred_elastic[,1]

model_metrics(test_pred[["Elastic"]], test$Issue)

# ============================================
# 3. RIDGE
# ============================================
model_ridge <- cv.glmnet(X_train, y_train, 
                         family = "binomial", 
                         alpha = 0,  # alpha=0 pour ridge
                         type.measure = "deviance")

var_ridge <- coef(model_ridge, s = "lambda.min")

pred_ridge <- predict(model_ridge, X_test, 
                      s = "lambda.min", 
                      type = "response")

test_pred$Ridge=pred_ridge[,1]

model_metrics(test_pred[["Ridge"]], test$Issue)

# ============================================
# 4. LDA 
# ============================================

lda_model <- lda(
  x = X_train,
  grouping = as.factor(y_train)
)

# Prédictions
pred_lda <- predict(lda_model, X_test)$posterior[, 2]

test_pred$LDA=pred_lda

model_metrics(test_pred[["LDA"]], test$Issue)

lda_imp <- as.data.frame(lda_model$scaling)

lda_imp$Variable <- rownames(lda_imp)

# On calcule la valeur absolue pour avoir l'importance "brute"
lda_imp$Importance <- abs(lda_imp$LD1)

lda_imp <- lda_imp[order(-lda_imp$Importance), ]


# ============================================
# 5. RANDOM FOREST
# ============================================

train$Issue=as.factor(train$Issue)

test$Issue=as.factor(test$Issue)

ntrees_grid <- c(750, 1000, 1250, 1500)

# Entraîner les 3 séquentiellement — ranger utilise tous les cores sur chaque modèle
results <- lapply(ntrees_grid, function(n) {
  
  cat("Entraînement avec", n, "arbres...\n")
  
  model <- ranger(
    x           = as.data.frame(train[, features_mix, with = FALSE]),  # ← data.table syntax
    y           = as.factor(train$Issue),                              # ← factor obligatoire pour classification
    num.trees   = n,
    importance  = "permutation",
    probability = TRUE,
    num.threads = 15
  )
  
  list(
    ntrees    = n,
    model     = model,
    oob_error = model$prediction.error
  )
})

oob_errors <- sapply(results, function(r) r$oob_error)
best_idx   <- which.min(oob_errors)
best_model <- results[[best_idx]]

cat("Meilleur modèle :", best_model$ntrees, "arbres\n")
cat("OOB Error       :", round(best_model$oob_error, 4), "\n")

importance_df <- data.frame(
  Variable   = names(best_model$model$variable.importance),
  Importance = best_model$model$variable.importance
) %>% 
  arrange(desc(Importance))

print(importance_df[1:10,])

pred_rf <- predict(best_model$model, 
                   data = as.data.frame(test[, features_mix, with = FALSE]))$predictions[, "Fav_W"]

test_pred$Random_Forest <- pred_rf

model_metrics(test_pred[["Random_Forest"]], test_pred$Issue)

# ============================================
# 6. GRADIENT BOOSTING
# ============================================

train$Issue=ifelse(train$Issue=="Fav_W",1,0)

test$Issue=ifelse(test$Issue=="Fav_W",1,0)

model_gbm <- gbm(formule,
                 data = train,
                 distribution = "bernoulli",
                 n.trees = 1000,
                 interaction.depth = 3,
                 shrinkage = 0.05,
                 cv.folds = 5,
                 verbose=T,
                 n.cores = 10)

best_iter <- gbm.perf(model_gbm, method = "cv", plot.it = FALSE)

importance_gbm <- summary(model_gbm, plot = FALSE)

# Affiche le top 10 des variables
print(importance_gbm[1:10, ])

pred_gbm <- predict(model_gbm, test, 
                    n.trees = best_iter, 
                    type = "response")

test_pred$GBM=pred_gbm

model_metrics(test_pred[["GBM"]], test_pred$Issue)

# ============================================
# 7. Lightgbm
# ============================================

# Préparer les données
lgb_train <- lgb.Dataset(data = X_train, label = y_train)
lgb_test <- lgb.Dataset.create.valid(lgb_train, data = X_test, label = y_test)


# Paramètres
params_lgb <- list(
  objective = "binary",
  metric = "binary_logloss",
  learning_rate = 0.01,     # Remonter un peu
  num_leaves = 20,
  min_data_in_leaf = 25,
  feature_fraction = 0.7,
  lambda_l1 = 0.5,
  lambda_l2 = 0.5,
  
  # --- AJOUTS VITESSE ---
  num_threads = 10,
  max_bin = 63,             # Défaut 255 → réduit la précision des splits (x2-3 rapide)
  min_sum_hessian_in_leaf = 0.1,  # Élagage plus agressif
  bagging_fraction = 0.8,   # Sous-échantillonne les données à chaque itération
  bagging_freq = 5          # Active le bagging
)

# Entraînement avec early stopping auto-géré
lgb_model <- lgb.train(
  params = params_lgb,
  data = lgb_train,
  nrounds = 1000,            # Plus de rounds car le learning_rate est plus bas
  valids = list(test = lgb_test),
  early_stopping_rounds = 50, # Laisse-lui plus de temps pour converger
  eval_freq = 50
)

# Récupérer l'importance des variables
importance <- lgb.importance(lgb_model, percentage = TRUE)

# Afficher les 20 premières
print(importance[1:10, ])

# Prédictions
pred_lgb <- predict(lgb_model, X_test)

test_pred$LGB=pred_lgb

model_metrics(test_pred[["LGB"]], test_pred$Issue)

# ============================================
# 8. XGBOOST
# ============================================

# Préparer les données
dtrain <- xgb.DMatrix(data = X_train, label = y_train)
dtest <- xgb.DMatrix(data = X_test, label = y_test)

xgb_model <- xgb.train(
  params = list(
    objective = "binary:logistic",
    eval_metric = "logloss",    # Préférable pour le Brier Score final
    max_depth = 4,              # Arbres plus courts (évite de capturer des cas trop spécifiques)
    eta = 0.005,                 # Beaucoup plus bas pour une convergence fine
    subsample = 0.7,            # Utilise 70% des matchs pour chaque arbre
    colsample_bytree = 0.7,     # Utilise 70% des variables (force l'usage du Momentum/Speed Index)
    gamma = 1                   # Ajoute une pénalité pour la création de nouveaux nœuds
  ),
  data = dtrain,
  nrounds = 3000,               # Nécessite beaucoup plus de rounds à 0.01
  watchlist = list(train = dtrain, test = dtest),
  early_stopping_rounds = 200,  # Arrête si l'AUC ne progresse plus sur 100 rounds
  print_every_n = 100
)

importance_matrix <- xgb.importance(
  feature_names = colnames(X_train),
  model = xgb_model
)

importance_matrix[1:10]

# xgb.plot.importance(
#   importance_matrix = importance_matrix[1:10],
#   main = "Top 20 Features - XGBoost"
# )

# Prédictions
pred_xgb <- predict(xgb_model, dtest)

test_pred$XGB=pred_xgb

model_metrics(test_pred[["XGB"]], test_pred$Issue)

# ============================================
# 9. NN1
# ============================================

train_scaled <- train %>%
  mutate(across(all_of(features_mix), ~ as.numeric(scale(.))))

train_scaled$Issue <- train$Issue

test_scaled <- test %>%
  mutate(across(all_of(features_mix), ~ as.numeric(scale(.))))

nn_1layer <- nnet(
  formule, 
  data = train_scaled, 
  size = 3,       # Un peu moins que 15 pour éviter de capturer du "bruit"
  decay = 0.05,      # Augmente le decay (pénalité) pour forcer des poids plus lisses
  maxit = 1000,      # Souvent suffisant pour converger sur une couche
  MaxNWts = 5000,   # Sécurité pour autoriser plus de connexions si besoin
  trace = TRUE      # Pour surveiller la décroissance de l'erreur en temps réel
)


garson_nn1=garson(nn_1layer)

importance_nn <- garson_nn1$data %>%
  rename(Variable = x_names, Importance = rel_imp) %>%
  arrange(desc(Importance))

print(importance_nn[1:10,])

# Prédiction
# On doit scaler le test avec les mêmes paramètres que le train

pred_NN1 <- predict(nn_1layer, test_scaled, type = "raw")

test_pred$NN1=pred_NN1[,1]

model_metrics(test_pred[["NN1"]], test_pred$Issue)

# ============================================
# 10. NAIVE BAYES
# ============================================

# Préparer les données (pas besoin de scaling)

nb_model <- naiveBayes(
  x = X_train,
  y = as.factor(y_train)
)

# Prédictions
pred_nb <- predict(nb_model, X_test, type = "raw")[, 2]

test_pred$Naive_Bayes=pred_nb

model_metrics(test_pred[["Naive_Bayes"]], test_pred$Issue)

# ============================================
# 11. KNN
# ============================================
library(doParallel)

cl=25

registerDoParallel(cl)

y_train_factor <- factor(y_train, levels = c(0, 1), labels = c("Out_W", "Fav_W"))

trControl <- trainControl(
  method        = "cv",
  number        = 3,          # ← 3 folds au lieu de 5 (x1.67 plus rapide)
  allowParallel = TRUE,
  classProbs    = TRUE,
  summaryFunction = mnLogLoss
)

grid_knn <- expand.grid(
  kmax     = c(10,25,35),   # Nombre de voisins
  distance = c(2),        # 1=Manhattan, 2=Euclidienne, 3=Minkowski
  kernel   = c("rectangular"   # KNN classique — tous les voisins égaux
               #"triangular",    # Poids linéaire selon distance
               #"epanechnikov",  # Poids quadratique
               #"gaussian",      # Poids gaussien
               #"cos",           # Distance cosinus
               #"optimal"
               )       # Poids optimisés automatiquement
)

model_knn <- train(
  x         = X_train,
  y         = y_train_factor,
  method    = "kknn",            # ← kknn au lieu de knn
  trControl = trControl,
  tuneGrid  = grid_knn,
  preProcess = c("center", "scale","pca")
)

print(model_knn$bestTune)

# Prédictions
pred_knn <- predict(model_knn, X_test, type = "prob")[, 2]

test_pred$KNN=pred_knn

model_metrics(test_pred[["KNN"]], test_pred$Issue)

# ============================================
# 12. Aggrégation des prédictions 
# ============================================

model_list=c("Ridge","LASSO","Elastic","LDA","Naive_Bayes","P_F_comb",
             "NN1","Random_Forest","XGB","LGB","GBM")

test_pred=test_pred %>% 
  mutate(Ensemble_Pred=rowMeans(across(all_of(model_list)))
  )

model_metrics(test_pred[["Ensemble_Pred"]], test_pred$Issue)

#Votes 

test_pred=as.data.frame(test_pred)

votes <- test_pred[, model_list] > 0.5

test_pred$Vote_F<- rowSums(votes)  

test_pred=test_pred %>% 
  mutate(Ratio_F=Vote_F/length(model_list))


# ============================================
# 13. Le marché 
# ============================================

test_pred=test_pred %>% 
  mutate(P_F_market_raw = (1 / Odd_F),
         P_O_market_raw = (1 / Odd_O),
         Marge = P_F_market_raw + P_O_market_raw,
         P_F_market = P_F_market_raw / Marge) %>% 
  select(-c(P_F_market_raw,P_O_market_raw,Marge))


model_metrics(test_pred[["P_F_market"]], test_pred$Issue)

End=Sys.time()-Start

print(End)


# ============================================
# COMPARAISON DES RÉSULTATS
# ============================================

model_list=c("P_F_comb","P_F_market","Ensemble_Pred","KNN","Naive_Bayes","Ridge",
             "LASSO","Elastic","LDA","NN1","Random_Forest","XGB","LGB","GBM")

# Comparer tous les modèles
all_metrics <- bind_rows(lapply(model_list, function(m) {
  res <- model_metrics(test_pred[[m]], test_pred$Issue)
  res$metrics %>% 
    pivot_wider(names_from = Metric, values_from = Value) %>%
    mutate(Modele = m)
})) %>% arrange(desc(Accuracy))

print(all_metrics %>% arrange(LogLoss))

# ============================================
# CALIBRATION
# ============================================

df_calib <- data.frame(
  Actual = test$Issue,
  Market = as.numeric(test_pred$P_F_market),
  Elo = as.numeric(test_pred$P_F_comb),       
  Lasso = as.numeric(test_pred$LASSO),              
  Ridge = as.numeric(test_pred$Ridge),             
  ElasticNet = as.numeric(test_pred$Elastic),          
  RandomForest = as.numeric(test_pred$Random_Forest),      
  GBM = as.numeric(test_pred$GBM),
  NN1 = test_pred$NN1,
  LDA = test_pred$LDA,
  LGB = test_pred$LGB,
  XGB = test_pred$XGB,
  Ensemble_Pred = test_pred$Ensemble_Pred
  )

cal_obj <- calibration(as.factor(Actual) ~ Elo + Lasso + Ridge + ElasticNet + RandomForest + GBM  +
                         Market + NN1 + LDA + LGB + XGB + Ensemble_Pred, 
                       data = df_calib, 
                       cuts = 10) # Divise en 10 tranches de 10%


# 1. Préparation des données (Assure-toi que tes vecteurs de prédiction ont la même longueur)
# On part du principe que tu as un dataframe 'df_preds' avec une colonne 'Actual' (0/1)
# et une colonne par modèle.

df_long <- df_calib %>%
  pivot_longer(cols = -Actual, names_to = "Modèle", values_to = "Prob") %>%
  # 1. On réduit le pas à 0.05 pour les breaks
  # 2. On centre les labels au milieu de chaque nouveau bin (ex: milieu de [0, 0.05] = 0.025)
  mutate(bin = cut(Prob, 
                   breaks = seq(0, 1, by = 0.1), 
                   include.lowest = TRUE, 
                   labels = seq(0.05, 0.95, by = 0.1))) %>%
  mutate(bin = as.numeric(as.character(bin)))

# 2. Calcul des fréquences observées par bin et par modèle
df_calib <- df_long %>%
  group_by(Modèle, bin) %>%
  summarise(
    actual_freq = mean(Actual,na.rm=T),
    n = n(),
    .groups = 'drop'
  )

# 3. Création du ggplot

# 3. Création du ggplot
fig=ggplot(df_calib, aes(x = bin, y = actual_freq, color = Modèle)) +
  geom_line(data = df_calib %>% filter(Modèle != "Market"), size = 0.5) +  # Lignes continues pour les autres
  geom_line(data = df_calib %>% filter(Modèle == "Market"), linetype = "dashed", size = 0.5) +  # Ligne pointillée pour Market
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey50") + # La ligne idéale y = x
  scale_x_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1)) +
  scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1)) +
  labs(
    title = "Courbe de Calibration Comparative",
    subtitle = "Fréquence réelle vs Probabilité prédite (par tranches de 10%)",
    x = "Probabilité prédite par le modèle",
    y = "Fréquence réelle de victoire",
    color = "Modèle"
  ) +
  theme_classic() +
  theme(legend.position = "bottom")

ggplotly(fig)
