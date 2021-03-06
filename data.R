library(glmSimData)
library(tibble)
library(purrr)

# helper function to get only resp_var from `generate_response`
hp_generate_response <- function(...) {
  res <- generate_response(...)
  return(as.factor(res$resp_var))
}

# Simulate datasets and store them in a tibble.
# Each row corresponds to one dataset.
caret_tibble <- tibble(
  n = rep(c(40, 200, 1000, 5000), c(4, 4, 4, 4)),
  p = rep(c(5, 50, 200, 1000), 4),
  beta = pmap(list(n = p, sd = 1 / p), rnorm),
  train_x = map2(n, p, generate_independent_covariates, "numerical"),
  train_y = map2(train_x, beta, hp_generate_response, family = binomial()))

caret_tibble
