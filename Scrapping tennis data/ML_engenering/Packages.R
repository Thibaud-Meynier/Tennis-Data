if (!requireNamespace("pak", quietly = TRUE)) install.packages("pak")

pkgs <- c(
  # Socle
  "tidyverse",      # inclut dplyr, stringr, lubridate, etc.
  "data.table",
  "here",
  "conflicted",
  
  # Utilitaires
  "zoo",
  "scales",
  "sjmisc",
  "stringdist",
  
  # Web scraping / XML
  "rvest",
  "xml2",
  
  # Modélisation
  "caret",
  "glmnet",
  "randomForest",
  "ranger",
  "e1071",
  "class",
  "gbm",
  "lightgbm",
  "xgboost",
  "nnet",
  "NeuralNetTools",
  "kknn",
  "MASS",
  
  # Évaluation
  "pROC",
  "MLmetrics",
  
  # Visualisation
  "plotly",
  
  # Parallélisation
  "parallel",
  "foreach",
  "doParallel",
  "future",
  "furrr",
  "progressr",
  "progress"
)

pak::pkg_install(pkgs, ask = FALSE)


invisible(capture.output(suppressPackageStartupMessages({
  suppressWarnings({
    library(tidyverse)
    library(data.table)
    library(here)
    library(progress)
    library(conflicted)
    library(zoo)
    library(rvest)
    library(dplyr)
    library(stringr)
    library(lubridate)
    library(xml2)
    library(sjmisc)
    library(stringdist)
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
    library(progressr)
    library(foreach)
    library(doParallel)
    library(parallel)
    library(furrr)
    library(future)
  })
}))
)

# Définition des conflits 

conflicts_prefer(dplyr::filter)
conflicts_prefer(lubridate::month)
conflicts_prefer(lubridate::isoweek)
conflicts_prefer(lubridate::year)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::between)
conflicts_prefer(sjmisc::is_empty)

rm(pkgs)
