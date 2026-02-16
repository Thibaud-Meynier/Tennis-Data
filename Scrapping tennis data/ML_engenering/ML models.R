library(tidyverse)
library(caret)
library(glmnet)      # lasso, ridge, elastic net
library(randomForest)
library(e1071)
library(scales)
library(rpart)       # CART
library(class)       # KNN
library(gbm)         # Gradient Boosting
library(lightgbm)    # LightGBM
library(pROC)
library(plotly)
library(nnet)
library(MASS)
library(MLmetrics)

# Préparation des données
# Supposons que votre variable cible s'appelle 'victoire_favori' (0/1)
# et vos features sont dans un vecteur 'features'

features_diff <- c(
  # --- Caractéristiques du Match ---
  #"Categorie", "Round",
  #"Surface_tournament", 
  "Categ_Elite", "Categ_Mid", "Categ_low",
  "Round_RR", "Round_R1", "Round_R2", 
  "Round_R3", "Round_R16", "Round_QF", "Round_SF", "Round_F",
  
  # --- DIFFÉRENCES - Profils Joueurs ---
  "Diff_Score_Rank",
  "Diff_Rank_Class",
  "Diff_Points",
  "Diff_Age",
  "Diff_IMC",
  "Diff_Size",
  "Diff_Weight",
  "Diff_Hand_Score",
  "Diff_Country_score",
  
  # --- DIFFÉRENCES - ELO & Probabilités ---
  #"Diff_Elo",
  #"Diff_Elo_s",
  "P_F_comb",
  
  # --- DIFFÉRENCES - H2H (Historique Face à Face) ---
  "Diff_H2H_Win_Rate",
  "Diff_H2H_Set_Win_Rate",
  "Diff_H2H_Games_Win_Rate",
  
  "Diff_H2H_s_Win_Rate",
  "Diff_H2H_s_Set_Win_Rate",
  "Diff_H2H_s_Games_Win_Rate",
  
  "Diff_H2H_Win_Rate_3Y",
  "Diff_H2H_Set_Win_Rate_3Y",
  "Diff_H2H_Games_Win_Rate_3Y",
  
  "Diff_H2H_s_Win_Rate_3Y",
  "Diff_H2H_s_Set_Win_Rate_3Y",
  "Diff_H2H_s_Games_Win_Rate_3Y",
  
  # --- DIFFÉRENCES - Forme Récente (4 matchs) ---
  "Diff_Win_Rate_4",
  "Diff_Win_Rate_s_4",
  
  # --- DIFFÉRENCES - Forme Récente (12 matchs) ---
  "Diff_Win_Rate_12",
  "Diff_Win_Rate_s_12",
  
  # --- DIFFÉRENCES - Performance As Fav (12 mois) ---
  "Diff_as_Fav_12_Win_rate",
  
  # --- DIFFÉRENCES - Performance As Out (12 mois) ---
  "Diff_as_Out_12_Win_rate",
  
  # --- DIFFÉRENCES - Performance As Fav (4 mois) ---
  "Diff_as_Fav_4_Win_rate",
  
  # --- DIFFÉRENCES - Performance As Out (4 mois) ---
  "Diff_as_Out_4_Win_rate"
)

feature_momentum <-c(
  # --- Momentum H2H ---
  "Mom_H2H",
  "Mom_H2H_Set",
  "Mom_H2H_Games",
  
  "Mom_H2H_s",
  "Mom_H2H_s_Set",
  "Mom_H2H_s_Games",
  
  "Mom_H2H_3Y",
  "Mom_H2H_Set_3Y",
  "Mom_H2H_Games_3Y",
  
  "Mom_H2H_s_3Y",
  "Mom_H2H_s_Set_3Y",
  "Mom_H2H_s_Games_3Y",
  
  # --- Momentum Forme Récente ---
  "Mom_WR_4",
  "Mom_WR_s_4",
  "Mom_WR_12",
  "Mom_WR_s_12",
  
  # --- Momentum Statut ---
  "Mom_as_Fav_12",
  "Mom_as_Out_12",
  "Mom_as_Fav_4",
  "Mom_as_Out_4",
  
  # --- Score Global ---
  "Mom_H2H_global",
  "Mom_WR_global",
  "Mom_as_Fav",
  "Mom_as_Out",
  "Mom_Fatigue",
  "Global_Momentum_Score"
)

features_mix <- c(
  # --- Caractéristiques du Match ---
  #"Surface_tournament", 
  "Categ_Elite", "Categ_Mid", "Categ_low",
  "Round_RR", "Round_R1", "Round_R2", 
  "Round_R3", "Round_R16", "Round_QF", "Round_SF", "Round_F",
  "Diff_Score_Rank",
  "Diff_Rank_Class",
  "Diff_Age",
  "Diff_IMC",
  "Diff_Size",
  "Diff_Weight",
  "Diff_Hand_Score",
  "Diff_Country_score",
  "Diff_Fatigue",
  "P_F",
  "P_s_F",
  "Mom_H2H_global",
  "Mom_WR_global",
  "Mom_as_Fav",
  "Mom_as_Out",
  "Mom_Fatigue",
  "Global_Momentum_Score")

all_features = c(features_diff,feature_momentum)

formule <- as.formula(paste0("Issue ~ ", paste0(features_mix, collapse = " + ")))

# Division train/test
set.seed(456)
TABLE_MOMENTUM = na.omit(TABLE_MOMENTUM)

index_train <- createDataPartition(TABLE_MOMENTUM$Issue, p = 0.8, list = FALSE)
train <- TABLE_MOMENTUM[index_train, ]
test <- TABLE_MOMENTUM[-index_train, ]
test_pred=TABLE_MOMENTUM[-index_train, ]
test_pred=test_pred %>% dplyr::select(tournament,Surface_tournament,Categorie,Season,Round,Favori,Outsider,Odd_F,Odd_O,Issue,P_F_comb)

# Préparation des matrices pour glmnet
X_train <- model.matrix(formule, train %>% dplyr::select(all_of(c(features_mix, "Issue"))))[, -1]
y_train <- ifelse(train$Issue=="Fav_W",1,0)
X_test <- model.matrix(formule, test %>% dplyr::select(all_of(c(features_mix, "Issue"))))[, -1]
y_test <- ifelse(test$Issue=="Fav_W",1,0)

model_precision=function(confusion_maxtrix){
  
  sensitivity=confusion_maxtrix[1,1]/(confusion_maxtrix[1,1]+confusion_maxtrix[1,2])
  
  sensitivity*100
  
  specificity=confusion_maxtrix[2,2]/(confusion_maxtrix[2,1]+confusion_maxtrix[2,2])
  
  specificity*100
  
  accuracy=(confusion_maxtrix[2,2]+confusion_maxtrix[1,1])/(confusion_maxtrix[2,1]+confusion_maxtrix[2,2]+confusion_maxtrix[1,2]+confusion_maxtrix[1,1])
  
  PPV = confusion_maxtrix[1,1]/(confusion_maxtrix[1,1]+confusion_maxtrix[2,1])
  
  NPV = confusion_maxtrix[2,2]/(confusion_maxtrix[1,2]+confusion_maxtrix[2,2])
  
  
  return(list(sensitivity=sensitivity*100,
              specificity=specificity*100,
              accuracy=accuracy*100,
              PPV = PPV*100,
              NPV = NPV*100,
              confusion_maxtrix=confusion_maxtrix
  ))
}

Start=Sys.time()

# ============================================
# 0. P_F_comb (elo_based)
# ============================================

pred_elo=test$P_F_comb

auc_elo_comb <- roc(y_test, pred_elo)

auc_elo_comb$auc


# Tracer la courbe
#plot(auc_elo_comb, main="Courbe ROC du Modèle", col="#2c3e50", lwd=3)

# Si proba >= 0.55 alors "Fav_W" (1), sinon "Out_W" (0)

confusion_maxtrix=table(test$Issue,ifelse(test$P_F_comb>0.5,"Faw_W_Pred","Out_W_Pred"))

elo_diag=model_precision(confusion_maxtrix)

elo_diag

# ============================================
# 1. LOGIT LASSO
# ============================================
model_lasso <- cv.glmnet(X_train, y_train, 
                         family = "binomial", 
                         alpha = 1,  # alpha=1 pour lasso
                         type.measure = "auc")

var_lasso=coef(model_lasso, s = "lambda.min")

pred_lasso <- predict(model_lasso, X_test, 
                      s = "lambda.min", 
                      type = "response")

test_pred$LASSO=pred_lasso[,1]

confusion_maxtrix=table(test$Issue,ifelse(pred_lasso>0.5,"Faw_W_Pred","Out_W_Pred"))

lasso_diag=model_precision(confusion_maxtrix)

lasso_diag

auc_lasso <- roc(y_test, pred_lasso)

auc_lasso$auc

# Tracer la courbe
#plot(auc_lasso, main="Courbe ROC du Modèle", col="#2c3e50", lwd=3)

# ============================================
# 2. ELASTIC NET
# ============================================
model_elastic <- cv.glmnet(X_train, y_train, 
                           family = "binomial", 
                           alpha = 0.5,  # alpha=0.5 pour elastic net
                           type.measure = "auc")

var_elastic=coef(model_elastic, s = "lambda.min")

pred_elastic <- predict(model_elastic, X_test, 
                        s = "lambda.min", 
                        type = "response")

test_pred$Elastic=pred_elastic[,1]

confusion_maxtrix=table(test$Issue,ifelse(pred_elastic>0.5,"Faw_W_Pred","Out_W_Pred"))

elastic_diag=model_precision(confusion_maxtrix)

elastic_diag

auc_elastic <- roc(y_test, pred_elastic)

auc_elastic$auc

# Tracer la courbe
#plot(auc_elastic, main="Courbe ROC du Modèle", col="#2c3e50", lwd=3)

# ============================================
# 3. RIDGE
# ============================================
model_ridge <- cv.glmnet(X_train, y_train, 
                         family = "binomial", 
                         alpha = 0,  # alpha=0 pour ridge
                         type.measure = "auc")

var_ridge <- coef(model_ridge, s = "lambda.min")

pred_ridge <- predict(model_ridge, X_test, 
                      s = "lambda.min", 
                      type = "response")

test_pred$Ridge=pred_ridge[,1]

confusion_maxtrix=table(test$Issue,ifelse(pred_ridge>0.5,"Faw_W_Pred","Out_W_Pred"))

ridge_diag=model_precision(confusion_maxtrix)

ridge_diag

auc_ridge <- roc(y_test, pred_ridge)

auc_ridge$auc

#plot(auc_ridge, main="Courbe ROC du Modèle", col="#2c3e50", lwd=3)

# ============================================
# 4. RANDOM FOREST
# ============================================

train$Issue=as.factor(train$Issue)

test$Issue=as.factor(test$Issue)

model_rf <- randomForest(formule, 
                         data = train,
                         ntree = 1000,
                         mtry = sqrt(length(all_features)),
                         importance = TRUE)

varImpPlot(model_rf,type=1,n.var=20)

pred_rf <- predict(model_rf, test, type = "prob")[, 1]

test_pred$Random_Forest=pred_rf

confusion_maxtrix=table(test$Issue,ifelse(pred_rf>0.5,"Faw_W_Pred","Out_W_Pred"))

rf_diag=model_precision(confusion_maxtrix)

rf_diag

auc_rf <- roc(y_test, pred_rf)

auc_rf$auc

#plot(auc_rf, main="Courbe ROC du Modèle", col="#2c3e50", lwd=3)

# ============================================
# 5. GRADIENT BOOSTING
# ============================================

train$Issue=ifelse(train$Issue=="Fav_W",1,0)

test$Issue=ifelse(test$Issue=="Fav_W",1,0)

train$Surface_tournament=as.factor(train$Surface_tournament)

test$Surface_tournament=as.factor(test$Surface_tournament)

model_gbm <- gbm(formule,
                 data = train,
                 distribution = "bernoulli",
                 n.trees = 1000,
                 interaction.depth = 3,
                 shrinkage = 0.05,
                 cv.folds = 5)

best_iter <- gbm.perf(model_gbm, method = "cv", plot.it = FALSE)

importance_gbm <- summary(model_gbm)

# Affiche le top 10 des variables
print(importance_gbm[1:20, ])

pred_gbm <- predict(model_gbm, test, 
                    n.trees = best_iter, 
                    type = "response")

test_pred$GBM=pred_gbm

test$Issue=ifelse(test$Issue==1,"Fav_W","Out_W")

confusion_maxtrix=table(test$Issue,ifelse(pred_gbm>0.5,"Faw_W_Pred","Out_W_Pred"))

gbm_diag=model_precision(confusion_maxtrix)

gbm_diag

auc_gbm <- roc(y_test, pred_gbm)

auc_gbm$auc

#plot(auc_gbm, main="Courbe ROC du Modèle", col="#2c3e50", lwd=3)

# ============================================
# 6. NN1
# ============================================

train_scaled <- train %>%
  mutate(across(all_of(features_mix), ~ as.numeric(scale(.))))

train_scaled$Issue <- train$Issue

test_scaled <- test %>%
  mutate(across(all_of(features_mix), ~ as.numeric(scale(.))))

# Modèle à 1 couche cachée (size = 5 neurones)
nn_1layer <- nnet(formule, data = train_scaled, 
                  size = 15, 
                  decay = 0.1, # Régularisation pour éviter l'overfitting
                  maxit = 1000)

# Prédiction
# On doit scaler le test avec les mêmes paramètres que le train

pred_NN1 <- predict(nn_1layer, test_scaled, type = "raw")

test_pred$NN1=pred_NN1[,1]

confusion_maxtrix <- table(test$Issue,ifelse(test_pred$NN1>0.5,"Faw_W_Pred","Out_W_Pred"))

NN1_diag=model_precision(confusion_maxtrix)

NN1_diag

auc_NN1 <- roc(y_test, test_pred$NN1)

auc_NN1$auc


# ============================================
# NAIVE BAYES
# ============================================

# Préparer les données (pas besoin de scaling)
nb_model <- naiveBayes(
  x = X_train,
  y = as.factor(y_train)
)

# Prédictions
pred_nb <- predict(nb_model, X_test, type = "raw")[, 2]

test_pred$Naive_Bayes=pred_nb

auc_nb <- roc(y_test, pred_nb)

auc_nb$auc

confusion_maxtrix=table(test$Issue,ifelse(pred_nb>0.5,"Faw_W_Pred","Out_W_Pred"))

nb_diag=model_precision(confusion_maxtrix)

nb_diag

print(nb_model$tables)


# ============================================
# KNN
# ============================================

# Avec caret pour tuning
knn_grid <- expand.grid(k = c(3, 5, 7, 9, 11, 15, 21, 25, 30))

y_train_factor <- factor(y_train, levels = c(0, 1), labels = c("Out_W", "Fav_W"))

knn_model <- train(
  x = X_train,
  y = y_train_factor,
  method = "knn",
  trControl = trainControl(method = "cv", number = 5, classProbs = TRUE),
  tuneGrid = knn_grid,
  preProcess = c("center", "scale")  # Important pour KNN
)

knn_model$bestTune

# Prédictions
pred_knn <- predict(knn_model, X_test, type = "prob")[, 2]

test_pred$KNN=pred_knn

auc_knn <- roc(y_test, pred_knn)

auc_knn$auc

confusion_maxtrix=table(test$Issue,ifelse(pred_knn>0.5,"Faw_W_Pred","Out_W_Pred"))

knn_diag=model_precision(confusion_maxtrix)

knn_diag

# ============================================
# LDA 
# ============================================

lda_model <- lda(
  x = X_train,
  grouping = as.factor(y_train)
)

# Prédictions
pred_lda <- predict(lda_model, X_test)$posterior[, 2]

test_pred$LDA=pred_lda

auc_lda <- roc(y_test, pred_lda)

auc_lda$auc

confusion_maxtrix=table(test$Issue,ifelse(pred_lda>0.5,"Faw_W_Pred","Out_W_Pred"))

lda_diag=model_precision(confusion_maxtrix)

lda_diag


# ============================================
# Lightgbm
# ============================================

# Préparer les données
lgb_train <- lgb.Dataset(data = X_train, label = y_train)
lgb_test <- lgb.Dataset.create.valid(lgb_train, data = X_test, label = y_test)

# Paramètres
params <- list(
  objective = "binary",
  metric = "auc",
  learning_rate = 0.1,
  num_leaves = 31,
  max_depth = -1,
  min_data_in_leaf = 5,
  feature_fraction = 0.8,
  bagging_fraction = 0.8,
  bagging_freq = 5
)

# Entraînement
lgb_model <- lgb.train(
  params = params,
  data = lgb_train,
  nrounds = 500,
  valids = list(test = lgb_test),
  early_stopping_rounds = 20,
  eval_freq = 50
)

# Prédictions
pred_lgb <- predict(lgb_model, X_test)

test_pred$LGB=pred_lgb

auc_lgb <- roc(y_test, pred_lgb)

auc_lgb$auc

confusion_maxtrix=table(test$Issue,ifelse(pred_lgb>0.5,"Faw_W_Pred","Out_W_Pred"))

lgb_diag=model_precision(confusion_maxtrix)

lgb_diag

# ============================================
# XGBOOST
# ============================================

library(xgboost)

# Préparer les données
dtrain <- xgb.DMatrix(data = X_train, label = y_train)
dtest <- xgb.DMatrix(data = X_test, label = y_test)

# Entraînement
xgb_model <- xgb.train(
  params = list(
    objective = "binary:logistic",
    eval_metric = "auc",
    max_depth = 6,
    eta = 0.1,  # ou learning_rate = 0.3 (nouveau nom)
    subsample = 0.8,
    colsample_bytree = 0.8
  ),
  data = dtrain,
  nrounds = 1000,
  watchlist = list(train = dtrain, test = dtest),
  print_every_n = 10
)

importance_matrix <- xgb.importance(
  feature_names = colnames(X_train),
  model = xgb_model
)

xgb.plot.importance(
  importance_matrix = importance_matrix[1:20],
  main = "Top 20 Features - XGBoost"
)

# Prédictions
pred_xgb <- predict(xgb_model, dtest)

test_pred$XGB=pred_xgb

auc_xgb <- roc(y_test, pred_xgb)

auc_xgb$auc

confusion_maxtrix=table(test$Issue,ifelse(pred_xgb>0.5,"Faw_W_Pred","Out_W_Pred"))

xgb_diag=model_precision(confusion_maxtrix)

xgb_diag


# ============================================
# 6. Le marché 
# ============================================

test_pred=test_pred %>% 
  mutate(P_F_market_raw = (1 / Odd_F),
         P_O_market_raw = (1 / Odd_O),
         Marge = P_F_market_raw + P_O_market_raw,
         P_F_market = P_F_market_raw / Marge) %>% 
  dplyr::select(-c(P_F_market_raw,P_O_market_raw,Marge))


confusion_maxtrix=table(test$Issue,ifelse(test_pred$P_F_market>0.5,"Faw_W_Pred","Out_W_Pred"))

market_diag=model_precision(confusion_maxtrix)

market_diag

auc_market <- roc(y_test, test_pred$P_F_market)

auc_market$auc

#plot(auc_market, main="Courbe ROC du Modèle", col="#2c3e50", lwd=3)

End=Sys.time()-Start

print(End)

# ============================================
# COMPARAISON DES RÉSULTATS
# ============================================

resultats <- data.frame(
  Modele = c("Market","Elo","Lasso", "Elastic Net", "Ridge", "Random Forest", 
             "Gradient Boosting","NN1","Naives Bayes","KNN","LDA","LGB","XGB"),
  AUC = c(auc_market$auc,auc_elo_comb$auc,auc_lasso$auc, auc_elastic$auc, auc_ridge$auc, auc_rf$auc, 
          auc_gbm$auc,auc_NN1$auc,auc_nb$auc,auc_knn$auc,auc_lda$auc,auc_lgb$auc,auc_xgb$auc),
  
  Accuracy=c(market_diag$accuracy,elo_diag$accuracy,lasso_diag$accuracy,elastic_diag$accuracy,
             ridge_diag$accuracy,rf_diag$accuracy,gbm_diag$accuracy,NN1_diag$accuracy,nb_diag$accuracy,
             knn_diag$accuracy,lda_diag$accuracy,lgb_diag$accuracy,xgb_diag$accuracy
             ),
  
  Sensitivity=c(market_diag$sensitivity,elo_diag$sensitivity,lasso_diag$sensitivity,elastic_diag$sensitivity,
             ridge_diag$sensitivity,rf_diag$sensitivity,gbm_diag$sensitivity,NN1_diag$sensitivity,nb_diag$sensitivity,
             knn_diag$sensitivity,lda_diag$sensitivity,lgb_diag$sensitivity,xgb_diag$sensitivity
  ),
  
  Specificity=c(market_diag$specificity,elo_diag$specificity,lasso_diag$specificity,elastic_diag$specificity,
             ridge_diag$specificity,rf_diag$specificity,gbm_diag$specificity,NN1_diag$specificity,nb_diag$specificity,
             knn_diag$specificity,lda_diag$specificity,lgb_diag$specificity,xgb_diag$specificity
  ),
  
  PPV=c(market_diag$PPV,elo_diag$PPV,lasso_diag$PPV,elastic_diag$PPV,
        ridge_diag$PPV,rf_diag$PPV,gbm_diag$PPV,NN1_diag$PPV,nb_diag$PPV,
        knn_diag$PPV,lda_diag$PPV,lgb_diag$PPV,xgb_diag$PPV	
  ),
  
  NPV=c(market_diag$NPV,elo_diag$NPV,lasso_diag$NPV,elastic_diag$NPV,
        ridge_diag$NPV,rf_diag$NPV,gbm_diag$NPV,NN1_diag$NPV,nb_diag$NPV,
        knn_diag$NPV,lda_diag$NPV,lgb_diag$NPV,xgb_diag$NPV	
  ),
  
  Log_Loss=c(LogLoss(test_pred$P_F_market,ifelse(test_pred$Issue == "Fav_W", 1, 0)),
             LogLoss(test_pred$P_F_comb,ifelse(test_pred$Issue == "Fav_W", 1, 0)),
             LogLoss(test_pred$LASSO,ifelse(test_pred$Issue == "Fav_W", 1, 0)),
             LogLoss(test_pred$Elastic,ifelse(test_pred$Issue == "Fav_W", 1, 0)),
             LogLoss(test_pred$Ridge,ifelse(test_pred$Issue == "Fav_W", 1, 0)),
             LogLoss(test_pred$Random_Forest,ifelse(test_pred$Issue == "Fav_W", 1, 0)),
             LogLoss(test_pred$GBM,ifelse(test_pred$Issue == "Fav_W", 1, 0)),
             LogLoss(test_pred$NN1,ifelse(test_pred$Issue == "Fav_W", 1, 0)),
             LogLoss(test_pred$Naive_Bayes,ifelse(test_pred$Issue == "Fav_W", 1, 0)),
             LogLoss(test_pred$KNN,ifelse(test_pred$Issue == "Fav_W", 1, 0)),
             LogLoss(test_pred$LDA,ifelse(test_pred$Issue == "Fav_W", 1, 0)),
             LogLoss(test_pred$LGB,ifelse(test_pred$Issue == "Fav_W", 1, 0)),
             LogLoss(test_pred$XGB,ifelse(test_pred$Issue == "Fav_W", 1, 0))
  ),
  
  Brier_score = c(
    Market        = mean((test_pred$P_F_market - ifelse(test_pred$Issue == "Fav_W", 1, 0))^2),
    Elo_Comb      = mean((test_pred$P_F_comb - ifelse(test_pred$Issue == "Fav_W", 1, 0))^2),
    LASSO         = mean((test_pred$LASSO - ifelse(test_pred$Issue == "Fav_W", 1, 0))^2),
    Elastic       = mean((test_pred$Elastic - ifelse(test_pred$Issue == "Fav_W", 1, 0))^2),
    Ridge         = mean((test_pred$Ridge - ifelse(test_pred$Issue == "Fav_W", 1, 0))^2),
    Random_Forest = mean((test_pred$Random_Forest - ifelse(test_pred$Issue == "Fav_W", 1, 0))^2),
    GBM           = mean((test_pred$GBM - ifelse(test_pred$Issue == "Fav_W", 1, 0))^2),
    NN1           = mean((test_pred$NN1 - ifelse(test_pred$Issue == "Fav_W", 1, 0))^2),
    Naive_Bayes   = mean((test_pred$Naive_Bayes - ifelse(test_pred$Issue == "Fav_W", 1, 0))^2),
    KNN           = mean((test_pred$KNN - ifelse(test_pred$Issue == "Fav_W", 1, 0))^2),
    LDA           = mean((test_pred$LDA - ifelse(test_pred$Issue == "Fav_W", 1, 0))^2),
    LGB           = mean((test_pred$LGB - ifelse(test_pred$Issue == "Fav_W", 1, 0))^2),
    XGB           = mean((test_pred$XGB - ifelse(test_pred$Issue == "Fav_W", 1, 0))^2)
  ),
  
  Calc_ECE = c(
  calc_ece(test_pred$P_F_market,ifelse(test_pred$Issue == "Fav_W", 1, 0)),
  calc_ece(test_pred$P_F_comb,ifelse(test_pred$Issue == "Fav_W", 1, 0)),
  calc_ece(test_pred$LASSO,ifelse(test_pred$Issue == "Fav_W", 1, 0)),
  calc_ece(test_pred$Elastic,ifelse(test_pred$Issue == "Fav_W", 1, 0)),
  calc_ece(test_pred$Ridge,ifelse(test_pred$Issue == "Fav_W", 1, 0)),
  calc_ece(test_pred$Random_Forest,ifelse(test_pred$Issue == "Fav_W", 1, 0)),
  calc_ece(test_pred$GBM,ifelse(test_pred$Issue == "Fav_W", 1, 0)),
  calc_ece(test_pred$NN1,ifelse(test_pred$Issue == "Fav_W", 1, 0)),
  calc_ece(test_pred$Naive_Bayes,ifelse(test_pred$Issue == "Fav_W", 1, 0)),
  calc_ece(test_pred$KNN,ifelse(test_pred$Issue == "Fav_W", 1, 0)),
  calc_ece(test_pred$LDA,ifelse(test_pred$Issue == "Fav_W", 1, 0)),
  calc_ece(test_pred$LGB,ifelse(test_pred$Issue == "Fav_W", 1, 0)),
  calc_ece(test_pred$XGB,ifelse(test_pred$Issue == "Fav_W", 1, 0))
  )
)

print(resultats)


# ============================================
# 6. Aggrégation des prédictions 
# ============================================

aggreg_pred=glm(as.factor(Issue)~LASSO+Random_Forest,data=test_pred,
                family = binomial)

summary(aggreg_pred)

test_pred$aggreg_pred=predict(aggreg_pred, test_pred,type = "response")

auc_aggreg <- roc(y_test, test_pred$aggreg_pred)

auc_aggreg$auc

confusion_maxtrix=table(test$Issue,ifelse(test_pred$aggreg_pred<0.5,"Faw_W_Pred","Out_W_Pred"))

aggreg_diag=model_precision(confusion_maxtrix)

aggreg_diag

model_list=c("Ridge","KNN","Random_Forest","Naive_Bayes","LDA","P_F_comb","XGB","GBM","NN1")

test_pred=test_pred %>% 
  mutate(mean_pred=rowMeans(across(all_of(model_list)))
  )

auc_aggreg_mean <- roc(y_test, test_pred$mean_pred)

auc_aggreg_mean$auc

confusion_maxtrix=table(test$Issue,ifelse(test_pred$mean_pred>0.5,"Faw_W_Pred","Out_W_Pred"))

aggreg_mean_diag=model_precision(confusion_maxtrix)

aggreg_mean_diag


# Visualisation

df_calib <- data.frame(
  Actual = ifelse(test$Issue == "Fav_W", 1, 0),
  Market = as.numeric(test_pred$P_F_market),
  Elo = as.numeric(test$P_F_comb),        # Ton Elo combiné (baseline)
  Lasso = as.numeric(pred_lasso),              # Prédit via type="response"
  Ridge = as.numeric(pred_ridge),              # alpha = 0
  ElasticNet = as.numeric(pred_elastic),            # alpha = 0.5
  RandomForest = as.numeric(pred_rf),      # ranger predictions[, "1"]
  GBM = as.numeric(pred_gbm),
  NN1 = test_pred$NN1,
  Naive_Bayes = test_pred$Naive_Bayes,
  KNN = test_pred$KNN,
  LDA = test_pred$LDA,
  LGB = test_pred$LGB,
  XGB = test_pred$XGB,
  Mean = test_pred$mean_pred
  )

cal_obj <- calibration(as.factor(Actual) ~ Elo + Lasso + Ridge + ElasticNet + RandomForest + GBM  +
                         Market + NN1 + KNN + Naive_Bayes + LDA + LGB + XGB + Mean, 
                       data = df_calib, 
                       cuts = 20) # Divise en 10 tranches de 10%

# Affichage du graphique
library(ggplot2)
library(dplyr)
library(tidyr)

# 1. Préparation des données (Assure-toi que tes vecteurs de prédiction ont la même longueur)
# On part du principe que tu as un dataframe 'df_preds' avec une colonne 'Actual' (0/1)
# et une colonne par modèle.

df_long <- df_calib %>%
  pivot_longer(cols = -Actual, names_to = "Modèle", values_to = "Prob") %>%
  # 1. On réduit le pas à 0.05 pour les breaks
  # 2. On centre les labels au milieu de chaque nouveau bin (ex: milieu de [0, 0.05] = 0.025)
  mutate(bin = cut(Prob, 
                   breaks = seq(0, 1, by = 0.05), 
                   include.lowest = TRUE, 
                   labels = seq(0.025, 0.975, by = 0.05))) %>%
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
