library(tidyverse)
library(caret)
library(glmnet)      # lasso, ridge, elastic net
library(randomForest)
library(rpart)       # CART
library(class)       # KNN
library(gbm)         # Gradient Boosting
library(lightgbm)    # LightGBM

# Préparation des données
# Supposons que votre variable cible s'appelle 'victoire_favori' (0/1)
# et vos features sont dans un vecteur 'features'

features <- c("feature1", "feature2", "feature3")  # À remplacer par vos vraies features
formule <- as.formula(paste0("Issue ~ ", paste0(features, collapse = " + ")))

# Division train/test
set.seed(123)
index_train <- createDataPartition(data$victoire_favori, p = 0.8, list = FALSE)
train <- data[index_train, ]
test <- data[-index_train, ]

# Préparation des matrices pour glmnet
X_train <- model.matrix(formule, train)[, -1]
y_train <- train$victoire_favori
X_test <- model.matrix(formule, test)[, -1]
y_test <- test$victoire_favori

# ============================================
# 1. LOGIT LASSO
# ============================================
model_lasso <- cv.glmnet(X_train, y_train, 
                         family = "binomial", 
                         alpha = 1,  # alpha=1 pour lasso
                         type.measure = "auc")

pred_lasso <- predict(model_lasso, X_test, 
                      s = "lambda.min", 
                      type = "response")
auc_lasso <- roc(y_test, pred_lasso)$auc

# ============================================
# 2. ELASTIC NET
# ============================================
model_elastic <- cv.glmnet(X_train, y_train, 
                           family = "binomial", 
                           alpha = 0.5,  # alpha=0.5 pour elastic net
                           type.measure = "auc")

pred_elastic <- predict(model_elastic, X_test, 
                        s = "lambda.min", 
                        type = "response")
auc_elastic <- roc(y_test, pred_elastic)$auc

# ============================================
# 3. RIDGE
# ============================================
model_ridge <- cv.glmnet(X_train, y_train, 
                         family = "binomial", 
                         alpha = 0,  # alpha=0 pour ridge
                         type.measure = "auc")

pred_ridge <- predict(model_ridge, X_test, 
                      s = "lambda.min", 
                      type = "response")
auc_ridge <- roc(y_test, pred_ridge)$auc

# ============================================
# 4. RANDOM FOREST
# ============================================
model_rf <- randomForest(formule, 
                         data = train,
                         ntree = 500,
                         mtry = sqrt(length(features)),
                         importance = TRUE)

pred_rf <- predict(model_rf, test, type = "prob")[, 2]
auc_rf <- roc(y_test, pred_rf)$auc

# ============================================
# 5. ARBRE CART
# ============================================
model_cart <- rpart(formule, 
                    data = train,
                    method = "class",
                    control = rpart.control(cp = 0.01))

pred_cart <- predict(model_cart, test, type = "prob")[, 2]
auc_cart <- roc(y_test, pred_cart)$auc


# ============================================
# 7. GRADIENT BOOSTING
# ============================================
model_gbm <- gbm(formule,
                 data = train,
                 distribution = "bernoulli",
                 n.trees = 1000,
                 interaction.depth = 3,
                 shrinkage = 0.01,
                 cv.folds = 5)

best_iter <- gbm.perf(model_gbm, method = "cv", plot.it = FALSE)
pred_gbm <- predict(model_gbm, test, 
                    n.trees = best_iter, 
                    type = "response")
auc_gbm <- roc(y_test, pred_gbm)$auc

# ============================================
# 8. LIGHTGBM
# ============================================
# Préparation des données pour LightGBM
dtrain <- lgb.Dataset(data = as.matrix(train[, features]),
                      label = train$victoire_favori)
dtest <- lgb.Dataset.create.valid(dtrain, 
                                  data = as.matrix(test[, features]),
                                  label = test$victoire_favori)

params <- list(
  objective = "binary",
  metric = "auc",
  learning_rate = 0.05,
  num_leaves = 31,
  max_depth = -1,
  min_data_in_leaf = 20
)

model_lgbm <- lgb.train(params,
                        dtrain,
                        nrounds = 1000,
                        valids = list(test = dtest),
                        early_stopping_rounds = 50,
                        verbose = -1)

pred_lgbm <- predict(model_lgbm, as.matrix(test[, features]))
auc_lgbm <- roc(y_test, pred_lgbm)$auc

# ============================================
# COMPARAISON DES RÉSULTATS
# ============================================
resultats <- data.frame(
  Modele = c("Lasso", "Elastic Net", "Ridge", "Random Forest", 
             "CART", "KNN", "Gradient Boosting", "LightGBM"),
  AUC = c(auc_lasso, auc_elastic, auc_ridge, auc_rf, 
          auc_cart, auc_knn, auc_gbm, auc_lgbm)
) %>%
  arrange(desc(AUC))

print(resultats)

# Visualisation
ggplot(resultats, aes(x = reorder(Modele, AUC), y = AUC)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Comparaison des performances des modèles",
       x = "Modèle",
       y = "AUC") +
  theme_minimal()