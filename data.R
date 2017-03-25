library(glmSimData)

n <- c(40, 200, 1000, 5000)
p <- c(3, 30, 300, 3000)

train_x <- generate_independent_covariates(n[1], p[1], "numerical")
train_y <- generate_response(train_x, beta = rnorm(ncol(train_x), sd = 1 / p[1]), 
                             family = binomial())$resp %>% as.factor()
