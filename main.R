rm(list = ls())
library(caret)
library(dplyr)
library(magrittr)
library(tibble)
library(tidyr)
source("data.R")

my_auc <- function (data, lev = NULL, model = NULL) {
  if (!all(levels(data[, "pred"]) == levels(data[, "obs"]))) 
    stop("levels of observed and predicted data do not match")
  out <- try(Metrics::auc(data$obs, data$pred), silent = TRUE)
  names(out) <- c("AUC")
  out
}

validate_model <- function(model, train_x, train_y) {
  train_control <- trainControl(method = "cv", number = 10,
                                summaryFunction = my_auc)
  train(x = train_x, y = train_y, method = model,
        trControl = train_control, metric = "AUC")
}

safe_validate <- purrr::safely(validate_model)


# Collect all classification algorithms in 'caret' package
models_list <- "caret_model_list.csv" %>% 
  read.csv(header = F) %>% 
  dplyr::filter(V3 != "Regression") %>% 
  `$`(V2)

run_caret_models <- function(models_list, x, y) {
  error <- array(list(), length(models_list))
  run_time <- array(list(), length(models_list))
  for (i in seq_along(models_list)) {
    model <- as.character(models_list[i])
    cat(i, model, "\n")
    run_time[[i]] <- system.time(m <- safe_validate(model, x, y))
    error[[i]] <- ifelse(is.null(m$error), "Success.", m$error)
  }
  
  # To-do: Summarise and return results (organise them in column-view)
  tibble(run_time = run_time, error = error)
}


#==============================================================================
# `caret_tibble` has on each row a different dataset.
caret_tibble

# For each dataset, we run `run_caret_models()`, which takes three arguments:
# `models_list`, `train_x`, and `train_y`. Here, `models` is exactly the
# same, but can be made to vary if desired.
models <- rep(list(models_list[c(78, 97)]), nrow(caret_tibble))

caret_tibble %<>% mutate(
  models_list = models,
  res = pmap(list(models_list, train_x, train_y), run_caret_models))

# Inspect the results.
caret_tibble %>% dplyr::select(n, p, models_list, res) %>% unnest
