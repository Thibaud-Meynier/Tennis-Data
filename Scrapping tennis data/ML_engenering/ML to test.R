# ============================================
# OPTION 1 : SVM linéaire
# ============================================

train$Issue_bin <- ifelse(train$Issue == "Fav_W", 1, 0)

test$Issue_bin <- ifelse(test$Issue == "Fav_W", 1, 0)

train_svm <- train %>% select(all_of(features_svm), Issue_bin)

test_svm <- test %>% select(all_of(features_svm), Issue_bin)

train_svm_rescale <- train_svm %>% 
  select(-Issue_bin) %>%
  mutate(across(everything(), ~rescale(.x, to = c(0, 1))))

test_svm_rescale <- test_svm %>% 
  select(-Issue_bin) %>%
  mutate(across(everything(), ~rescale(.x, to = c(0, 1))))
  
svm_linear <- svm(Issue_bin ~ ., 
                  data = train_svm_scaled,
                  kernel = "linear",
                  cost = 1,
                  probability = TRUE)

# Prédictions
pred_svml <- predict(svm_linear, test_svm_scaled, probability = TRUE)

test_pred$svml <- pred_svml

# Évaluation
conf_matrix_linear <- confusionMatrix(pred_svml, test_svm_scaled$Issue_bin)

# ============================================
# OPTION 2 : SVM avec noyau RBF (Radial)
# ============================================
svm_radial <- svm(Issue_bin ~ ., 
               data = train_svm_scaled,
               kernel = "radial",
               cost = 1,
               gamma = 0.1,
               probability = TRUE)

pred_svmr <- predict(svm_radial, test_svm_scaled, probability = TRUE)

test_pred$svmr <- pred_svmr


# ============================================
# NAIVE BAYES
# ============================================

library(e1071)

# Préparer les données (pas besoin de scaling)
nb_model <- naiveBayes(
  x = X_train,
  y = as.factor(y_train)
)

# Prédictions
pred_nb <- predict(nb_model, X_test, type = "raw")[, 2]


# ============================================
# KNN
# ============================================

library(class)

# Avec caret pour tuning
knn_grid <- expand.grid(k = c(3, 5, 7, 9, 11, 15, 21))

knn_model <- train(
  x = X_train,
  y = as.factor(y_train),
  method = "knn",
  trControl = trainControl(method = "cv", number = 5, classProbs = TRUE),
  tuneGrid = knn_grid,
  preProcess = c("center", "scale")  # Important pour KNN
)

# Prédictions
pred_knn <- predict(knn_model, X_test, type = "prob")[, 2]

# ============================================
# LDA vs. QDA
# ============================================

library(MASS)

lda_model <- lda(
  x = X_train,
  grouping = as.factor(y_train)
)

# Prédictions
pred_lda <- predict(lda_model, X_test)$posterior[, 2]

qda_model <- qda(
  x = X_train,
  grouping = as.factor(y_train)
)

# Prédictions
pred_qda <- predict(qda_model, X_test)$posterior[, 2]

# ============================================
# Catboost
# ============================================
library(catboost)

# Identifier les colonnes catégorielles
cat_features <- which(sapply(train[, features_mix], is.factor)) - 1

# Pool de données
train_pool <- catboost.load_pool(
  data = X_train,
  label = y_train,
  cat_features = cat_features
)

test_pool <- catboost.load_pool(
  data = X_test,
  cat_features = cat_features
)

# Entraînement
catboost_model <- catboost.train(
  train_pool,
  params = list(
    loss_function = "Logloss",
    eval_metric = "AUC",
    iterations = 100,
    depth = 6,
    learning_rate = 0.1,
    l2_leaf_reg = 3
  )
)

# Prédictions
pred_catboost <- catboost.predict(catboost_model, test_pool, prediction_type = "Probability")

# ============================================
# Lightgbm
# ============================================

library(lightgbm)

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
  min_data_in_leaf = 20,
  feature_fraction = 0.8,
  bagging_fraction = 0.8,
  bagging_freq = 5
)

# Entraînement
lgb_model <- lgb.train(
  params = params,
  data = lgb_train,
  nrounds = 100,
  valids = list(test = lgb_test),
  early_stopping_rounds = 10
)

# Prédictions
pred_lgb <- predict(lgb_model, X_test)

# ============================================
# XGBOOST
# ============================================

library(xgboost)

# Préparer les données
dtrain <- xgb.DMatrix(data = X_train, label = y_train)
dtest <- xgb.DMatrix(data = X_test, label = y_test)

# Entraînement
xgb_model <- xgboost(
  data = dtrain,
  objective = "binary:logistic",
  nrounds = 100,
  max_depth = 6,
  eta = 0.3,
  subsample = 0.8,
  colsample_bytree = 0.8,
  eval_metric = "auc",
  verbose = 0
)

# Prédictions
pred_xgb <- predict(xgb_model, dtest)

# Avec tuning via caret
xgb_grid <- expand.grid(
  nrounds = c(50, 100, 200),
  max_depth = c(3, 6, 9),
  eta = c(0.01, 0.1, 0.3),
  gamma = 0,
  colsample_bytree = c(0.6, 0.8, 1),
  min_child_weight = 1,
  subsample = c(0.6, 0.8, 1)
)

xgb_caret <- train(
  x = X_train,
  y = as.factor(y_train),
  method = "xgbTree",
  trControl = trainControl(method = "cv", number = 5, classProbs = TRUE),
  tuneGrid = xgb_grid
)

