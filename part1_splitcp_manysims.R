## ---------------------------------------
## Libraries
## ---------------------------------------
library(dplyr)
library(ggplot2)
library(ranger)     # fast tree model
set.seed(123)

## ---------------------------------------
## Helper: split (train/calib/test)
## ---------------------------------------
split_df <- function(df, p = c(0.6, 0.2, 0.2)) {
  df <- df %>%
    mutate(split = sample(c("train", "cal", "test"),
                          size = n(),
                          replace = TRUE,
                          prob = p))
  
  train_df <- df %>% filter(split == "train") %>% select(x, y)
  cal_df   <- df %>% filter(split == "cal")   %>% select(x, y)
  test_df  <- df %>% filter(split == "test")  %>% select(x, y)
  
  list(train = train_df,
       cal   = cal_df,
       test  = test_df)
}

## ---------------------------------------
## Model: fast & simple regression model
## ---------------------------------------
fit_model <- function(train_df) {
  nnet_wflow <- workflow(
    y ~ x,
    mlp(hidden_units = 4) %>% 
      set_mode("regression")
  )
  fit(nnet_wflow, data = train_df)
}

predict_model <- function(fit, newdata) {
  predict(fit, newdata) %>% pull(.pred)
}

## ---------------------------------------
## Conformal rank quantile (finite-sample)
##   target coverage = 1 - alpha  (e.g., 0.9)
## ---------------------------------------
conformal_quantile <- function(scores, alpha = 0.1) {
  s <- sort(as.numeric(scores))
  n <- length(s)
  k <- ceiling((n + 1) * (1 - alpha))  # (n+1)*(1-alpha)
  k <- min(max(k, 1), n)
  s[k]
}

## ---------------------------------------
## One simulation run
##   method = "abs" or "normalized"
## ---------------------------------------
simulate_once <- function(n = 1000,
                          hetero = FALSE,
                          method = c("abs","normalized"),
                          alpha = 0.1) {
  method <- match.arg(method)
  df <- if (hetero) make_data_heteroscedastic(n) else make_data(n)
  
  sp <- split_df(df)
  f  <- fit_model(sp$train)
  
  # predictions
  sp$train$pred <- predict_model(f, sp$train)
  sp$cal$pred   <- predict_model(f, sp$cal)
  sp$test$pred  <- predict_model(f, sp$test)
  
  # calibration scores
  if (method == "abs") {
    sp$cal$score <- abs(sp$cal$y - sp$cal$pred)
    qhat <- conformal_quantile(sp$cal$score, alpha)
    sp$test <- sp$test |>
      mutate(lower = pred - qhat, upper = pred + qhat)
  } else {
    # normalized: estimate sigma(x) on train residuals
    sp$train$resid <- abs(sp$train$y - sp$train$pred)
    sig_fit <- lm(resid ~ poly(x, 2), data = sp$train)
    
    sp$cal$sigma <- pmax(1e-6, as.numeric(predict(sig_fit, sp$cal)))
    sp$cal$score <- abs(sp$cal$y - sp$cal$pred) / sp$cal$sigma
    qhat <- conformal_quantile(sp$cal$score, alpha)
    
    sp$test$sigma <- pmax(1e-6, as.numeric(predict(sig_fit, sp$test)))
    sp$test <- sp$test |>
      mutate(lower = pred - qhat * sigma,
             upper = pred + qhat * sigma)
  }
  
  # coverage on test
  mean(sp$test$y > sp$test$lower & sp$test$y < sp$test$upper)
}

## ---------------------------------------
## Many simulations + quick plot
## ---------------------------------------
simulate_coverages <- function(n_sims = 1000,
                               n = 1000,
                               hetero = FALSE,
                               method = c("abs","normalized"),
                               alpha = 0.1,
                               show_plot = TRUE) {
  method <- match.arg(method)
  covs <- numeric(n_sims)
  for (i in seq_len(n_sims)) {
    covs[i] <- simulate_once(n = n, hetero = hetero,
                             method = method, alpha = alpha)
  }
  avg <- mean(covs)
  if (show_plot) {
    p <- tibble::tibble(coverage = covs) %>%
      ggplot(aes(coverage)) +
      geom_histogram(bins = 30, color = "white") +
      geom_vline(xintercept = 1 - alpha, linetype = "dashed", size = 1) +
      labs(
        title = sprintf("Empirical coverage across %d simulations", n_sims),
        subtitle = sprintf("method = %s, hetero = %s, mean = %.3f",
                           method, hetero, avg),
        x = "Coverage", y = "Count"
      )
    print(p)
  }
  invisible(list(coverage = covs, mean_coverage = avg))
}


# 1) Homoscedastic + absolute residuals
res1 <- simulate_coverages(n_sims = 1000, n = 800,
                           hetero = FALSE, method = "abs", alpha = 0.1)

# 2) Heteroscedastic + absolute residuals (expect miscalibration locally, but global ~ ok)
res2 <- simulate_coverages(n_sims = 1000, n = 800,
                           hetero = TRUE, method = "abs", alpha = 0.1)

# 3) Heteroscedastic + normalized residuals (adapts width; global calibration)
res3 <- simulate_coverages(n_sims = 1000, n = 800,
                           hetero = TRUE, method = "normalized", alpha = 0.1)
