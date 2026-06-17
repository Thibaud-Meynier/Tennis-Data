
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
    library(rstudioapi)
  })
}))
)

dir.create(paste0(here(), "/Scrapping tennis data/ML_engenering/forward_selection/", scope, "/", metric),
           showWarnings = TRUE, 
           recursive = TRUE)

Start=Sys.time()

res_fs <- forward_selection(train, test, candidates,
                            forced_vars = forced_vars,
                            model_type = model, 
                            metric = metric)


Sys.time()-Start

saveRDS(res_fs,file=paste0(here(),"/Scrapping tennis data/ML_engenering/forward_selection/",scope,"/",metric,"/","result_",model,".rds"))
