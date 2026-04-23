# ============================================================
# FORWARD SELECTION GÉNÉRIQUE - MULTI-MODÈLE
# ============================================================

load(paste0(here(),"/Scrapping tennis data/ML_engenering/TABLE_MOMENTUM.RData"))

candidates <- c( "P_F",
                 "P_s_F",
                 "P_c_F",
                 "Diff_Age",
                 "Diff_IMC",
                 "Diff_Q",
                 "Diff_Giant_Kill",
                 "Diff_Run",
                 "Diff_Final",
                 "Diff_Points",
                 "Diff_Rank",
                 "Diff_Rank_Class",
                 "Diff_Score_Rank",
                 "Diff_From_Max",
                 "Diff_Rank_evol",
                 "Mom_Statut",
                 "Diff_H2H_Win_Rate",
                 "Diff_H2H_Games_Win_Rate",
                 "Diff_H2H_s_Win_Rate",
                 "Diff_H2H_s_Games_Win_Rate",
                 "Diff_WR",
                 "Diff_WR_Surface",
                 "Diff_Win_Rate_4",
                 "Diff_Win_Rate_12",
                 "Diff_Win_Rate_52",
                 "Diff_Win_Rate_s_52",
                 "Grand_Slam",
                 "ATP_1000",
                 "ATP_250",
                 "ATP_500",
                 "Round_R1",
                 "Round_R2",
                 "Round_R3",
                 "Round_R16",
                 "Round_QF",
                 "Round_SF",
                 "Round_F",
                 "Clay",
                 "Grass",
                 "Hard",
                 "Indoors")

formule <- as.formula(paste0("Issue ~ ", paste0(candidates, collapse = " + ")))

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
X_train <- model.matrix(formule, train %>% select(all_of(c(candidates, "Issue"))))[, -1]
y_train <- ifelse(train$Issue=="Fav_W",1,0)
X_test <- model.matrix(formule, test %>% select(all_of(c(candidates, "Issue"))))[, -1]
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

# ---- 1. Fonction d'évaluation par modèle --------------------
evaluate_model <- function(train, test, features, target = "Issue", model_type = "naive_bayes") {
  
  train <- as.data.table(train)
  test  <- as.data.table(test)
  
  formula <- as.formula(paste(target, "~", paste(features, collapse = " + ")))
  
  X_train <- as.matrix(train[, ..features])
  X_test  <- as.matrix(test[,  ..features])
  y_train <- as.integer(train[[target]] == "Fav_W")
  y_test  <- as.integer(test[[target]]  == "Fav_W")
  
  probs <- tryCatch({
    
    # --------------------------------------------------
    if (model_type == "naive_bayes") {
      fit <- naiveBayes(x = X_train, y = as.factor(y_train))
      predict(fit, newdata = X_test, type = "raw")[, 2]
      
      # --------------------------------------------------
    } else if (model_type == "lda") {
      fit <- lda(x = X_train, grouping = as.factor(y_train))
      predict(fit, newdata = X_test)$posterior[, 2]
      
      # --------------------------------------------------
    } else if (model_type == "logistic") {
      train_glm        <- as.data.frame(X_train)
      train_glm$y      <- as.factor(y_train)
      test_glm         <- as.data.frame(X_test)
      formula_glm      <- as.formula(paste("y ~", paste(features, collapse = " + ")))
      fit <- glm(formula_glm, data = train_glm, family = binomial())
      predict(fit, newdata = test_glm, type = "response")
      
      # --------------------------------------------------
    } else if (model_type == "random_forest") {
      fit <- ranger(
        x           = as.data.frame(X_train),
        y           = as.factor(y_train),
        num.trees   = 500,           # réduit vs prod (500-1500) pour la vitesse
        probability = TRUE,
        num.threads = 15
      )
      predict(fit, data = as.data.frame(X_test))$predictions[, 2]
      
      # --------------------------------------------------
    } else if (model_type == "xgboost") {
      dtrain <- xgb.DMatrix(data = X_train, label = y_train)
      dtest  <- xgb.DMatrix(data = X_test,  label = y_test)
      fit <- xgb.train(
        params = list(
          objective        = "binary:logistic",
          eval_metric      = "logloss",
          max_depth        = 4,
          eta              = 0.05,       # réduit vs prod (0.005) pour la vitesse
          subsample        = 0.7,
          colsample_bytree = 0.7,
          gamma            = 1
        ),
        data                  = dtrain,
        nrounds               = 3000,     # réduit vs prod (3000) pour la vitesse
        watchlist             = list(test = dtest),
        early_stopping_rounds = 30,
        verbose               = 0
      )
      predict(fit, dtest)
      
      # --------------------------------------------------
    } else if (model_type == "lightgbm") {
      dtrain <- lgb.Dataset(X_train, label = y_train)
      dtest_lgb <- lgb.Dataset.create.valid(dtrain, data = X_test, label = y_test)
      fit <- lgb.train(
        params = list(
          objective              = "binary",
          metric                 = "binary_logloss",
          learning_rate          = 0.01,  # réduit vs prod (0.01) pour la vitesse
          num_leaves             = 20,
          min_data_in_leaf       = 25,
          feature_fraction       = 0.7,
          lambda_l1              = 0.5,
          lambda_l2              = 0.5,
          num_threads            = 10,
          max_bin                = 63,
          bagging_fraction       = 0.8,
          bagging_freq           = 5,
          verbose               = -1
        ),
        data                  = dtrain,
        nrounds               = 1000,     # réduit vs prod (1000) pour la vitesse
        valids                = list(test = dtest_lgb),
        early_stopping_rounds = 30
      )
      predict(fit, X_test)
      
      # --------------------------------------------------
    } else if (model_type == "gbm") {
      train_gbm      <- as.data.frame(X_train)
      train_gbm[[target]] <- y_train
      test_gbm       <- as.data.frame(X_test)
      formula_gbm    <- as.formula(paste(target, "~", paste(features, collapse = " + ")))
      fit <- gbm(
        formula_gbm,
        data             = train_gbm,
        distribution     = "bernoulli",
        n.trees          = 500,          # réduit vs prod (1000) pour la vitesse
        interaction.depth = 3,
        shrinkage        = 0.05,
        cv.folds         = 3,            # réduit vs prod (5) pour la vitesse
        verbose          = FALSE,
        n.cores          = 15
      )
      best_iter <- gbm.perf(fit, method = "cv", plot.it = FALSE)
      predict(fit, test_gbm, n.trees = best_iter, type = "response")
      
      # --------------------------------------------------
    } else if (model_type == "knn") {
      # KNN retourne des classes → on approche la proba par vote majoritaire (0/1)
      fit_kknn <- kknn(
        formula  = y ~ .,
        train    = data.frame(X_train, y = as.factor(y_train)),
        test     = data.frame(X_test),
        k        = 50,
        kernel   = "optimal",
        distance = 2
      )
      
      fit_kknn$prob[, "1"]   # ou [, "Fav_W"] selon ton encodage
      # --------------------------------------------------
    } else if (model_type %in% c("lasso", "ridge", "elasticnet")) {
      alpha_val <- switch(model_type, lasso = 1, ridge = 0, elasticnet = 0.5)
      fit <- cv.glmnet(X_train, y_train, alpha = alpha_val, family = "binomial",
                       type.measure = "deviance")
      as.vector(predict(fit, X_test, s = "lambda.min", type = "response"))
      
      # --------------------------------------------------
    } else if (model_type == "nnet") {
      train_scaled <- as.data.frame(scale(X_train))
      test_scaled  <- as.data.frame(scale(X_test,
                                          center = attr(scale(X_train), "scaled:center"),
                                          scale  = attr(scale(X_train), "scaled:scale")))
      train_scaled[[target]] <- as.factor(y_train)
      formula_nn <- as.formula(paste(target, "~", paste(features, collapse = " + ")))
      fit <- nnet(
        formula_nn,
        data    = train_scaled,
        size    = 3,
        decay   = 0.05,
        maxit   = 1000,          # réduit vs prod (1000) pour la vitesse
        MaxNWts = 5000,
        trace   = FALSE
      )
      predict(fit, test_scaled, type = "raw")[, 1]
      
    } else {
      stop(paste("Modèle inconnu :", model_type))
    }
    
  }, error = function(e) {
    message("Erreur modèle [", model_type, "] : ", e$message)
    return(NULL)
  })
  
  if (is.null(probs)) return(list(logloss = Inf, auc = 0, accuracy = 0, predictions = NULL))
  
  probs      <- pmax(pmin(probs, 1 - 1e-7), 1e-7)
  pred_class <- as.integer(probs >= 0.5)

  predictions <- probs
  
  list(
    logloss     = MLmetrics::LogLoss(y_pred = probs,       y_true = y_test),
    auc         = MLmetrics::AUC(y_pred     = probs,       y_true = y_test),
    accuracy    = MLmetrics::Accuracy(y_pred = pred_class, y_true = y_test),
    predictions = predictions
  )
}



# ---- 2. Forward Selection ---------------------------------------

forward_selection <- function(train, test,
                              candidates,
                              forced_vars = c(),
                              target      = "Issue",
                              model_type  = "naive_bayes",
                              metric      = "logloss",
                              max_vars    = 25,
                              verbose     = TRUE) {
  
  selected   <- forced_vars
  candidates <- setdiff(candidates, forced_vars)
  
  if (length(forced_vars) > 0) {
    init_scores <- evaluate_model(train, test, forced_vars, target, model_type)
    best_score  <- init_scores[[metric]]
    if (verbose) message(sprintf("Score initial (%s) : %.4f | vars forcées : %s",
                                 metric, best_score, paste(forced_vars, collapse = ", ")))
  } else {
    best_score <- if (metric == "logloss") Inf else -Inf
  }
  
  results <- list()
  
  for (step in seq_len(min(max_vars, length(candidates)))) {
    
    remaining <- setdiff(candidates, selected)
    if (length(remaining) == 0) break
    
    if (verbose) message(sprintf("\n--- Step %d | selected: %d vars ---", step, length(selected)))
    
    # --- UN SEUL appel evaluate_model par candidat ---
    # On récupère toutes les métriques d'un coup pour éviter la double évaluation
    step_scores <- lapply(remaining, function(var) {
      evaluate_model(train, test, c(selected, var), target, model_type)
    })
    names(step_scores) <- remaining
    
    # Extraire la métrique cible pour le ranking
    metric_vals        <- sapply(step_scores, `[[`, metric)
    best_new_var       <- if (metric == "logloss") names(which.min(metric_vals)) else names(which.max(metric_vals))
    best_candidate     <- step_scores[[best_new_var]]   # scores complets du meilleur candidat
    best_new_score     <- best_candidate[[metric]]
    
    improves <- if (metric == "logloss") best_new_score < best_score else best_new_score > best_score
    
    if (!improves) {
      if (verbose) message("Pas d'amélioration — arrêt.")
      break
    }
    
    selected   <- c(selected, best_new_var)
    best_score <- best_new_score
    
    # Verbose et historique utilisent best_candidate (même évaluation que le choix)
    results[[step]] <- data.table(
      step     = step,
      variable = best_new_var,
      logloss  = best_candidate$logloss,
      auc      = best_candidate$auc,
      accuracy = best_candidate$accuracy,
      n_vars   = length(selected),
      vars     = paste(selected, collapse = ", ")
    )
    
    if (verbose) message(sprintf("  + %-35s | LogLoss: %.4f | AUC: %.4f | Acc: %.4f",
                                 best_new_var,
                                 best_candidate$logloss,
                                 best_candidate$auc,
                                 best_candidate$accuracy))
  }
  
  list(
    best_vars   = selected,
    forced_vars = forced_vars,
    best_score  = best_score,
    history     = rbindlist(results)
  )
}

# --- 5. Boucle sur la liste de modèles ------------

# ---- Modèles à optimiser ----

models_to_run <- c( "random_forest","xgboost", "lightgbm", "gbm", "knn","nnet",
                    "lasso", "ridge", "elasticnet","lda","naive_bayes","logistic")

forced_vars <-c("P_F","P_s_F")

model="random_forest"

metric="accuracy"

jobRunScript(
  path      = paste0(here(),"/Scrapping tennis data/ML_engenering/fs_job.R"),
  name      = paste0("fs_", model),
  importEnv = TRUE
)


for (model in models_to_run){
  
  model_type <- model
  
  jobRunScript(
    path      = paste0(here(),"/Scrapping tennis data/ML_engenering/fs_job.R"),
    name      = paste0("fs_", model),
    importEnv = TRUE
  )
  
}


all_results <- list()

for (model in models_to_run) {
  
  path <- paste0(here(), "/Scrapping tennis data/ML_engenering/forward_selection/result_", model, ".rds")
  
  if (file.exists(path)) {
    
    all_results[[model]] <- readRDS(path)
    
  } else {
    
    message("Manquant : ", model)
  }
}

for (model in names(all_results)) {
  
  best_vars = all_results[[model]][["best_vars"]]
 
  model_pred = evaluate_model(train,test,best_vars,"Issue",model) 

  test_pred[[paste0("P_F_", toupper(model))]] <- model_pred$predictions

}

# ============================================
# 12. Aggrégation des prédictions 
# ============================================

model_list <- c(paste0("P_F_", toupper(names(all_results))),"P_F_comb")

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

# ============================================
# COMPARAISON DES RÉSULTATS
# ============================================

model_list=c(model_list,"P_F_market","Ensemble_Pred")

# Comparer tous les modèles
all_metrics <- bind_rows(lapply(model_list, function(m) {
  res <- model_metrics(test_pred[[m]], test_pred$Issue)
  res$metrics %>% 
    pivot_wider(names_from = Metric, values_from = Value) %>%
    mutate(Modele = m)
})) %>% arrange(desc(Accuracy))

print(all_metrics %>% arrange(desc(Accuracy)))

