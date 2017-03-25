rm(list = ls())
library(magrittr)
library(caret)
source("data.R")

my_auc <- function (data, lev = NULL, model = NULL) {
  if (!all(levels(data[, "pred"]) == levels(data[, "obs"]))) 
    stop("levels of observed and predicted data do not match")
  out <- try(Metrics::auc(data$obs, data$pred), silent = TRUE)
  names(out) <- c("AUC")
  out
}

validate_model <- function(model) {
  train_control <- trainControl(method="cv", number=10, summaryFunction = my_auc)
  train(x = train_x, y = train_y, method = model,
        trControl = train_control, metric = "AUC")
}

safe_validate <- purrr::safely(validate_model)


# Collect all classification algorithms in 'caret' package
models_list <- "caret_model_list.csv" %>% 
  read.csv(header = F) %>% 
  dplyr::filter(V3 != "Regression") %>% 
  `$`(V2)

run_caret_models <- function(models_list) {
  error <- array(list(), length(models_list))
  run_time <- array(list(), length(models_list))
  for (i in seq_along(models_list)) {
    model <- as.character(models_list[i])
    cat(i, model, "\n")
    run_time[[i]] <- system.time(m <- safe_validate(model))  
    error[[i]] <- ifelse(is.null(m$error), "Success.", m$error)
  }
  
  # To-do: Summarise and return results (organise them in column-view)
  list(run_time = run_time, error = error)
}


#==============================================================================
res <- run_caret_models(models_list[5:10])
