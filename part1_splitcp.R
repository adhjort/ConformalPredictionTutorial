library(tidymodels)
library(nnet)
library(ggplot2)
library(zoo)  

#-----------------------------------------
# DATA GENERATION
#-----------------------------------------
#-----------------------------------------
# Homoscedastic data
#-----------------------------------------
make_data <- function(n, std_dev = 0.2) {
  tibble(x = runif(n, min = 0, max = 1)) %>%
    mutate(
      y_true = sin(2 * pi * x) + x,   # nonlinear + trend
      y = y_true + rnorm(n, sd = std_dev)
    )
}

#-----------------------------------------
# Heteroscedastic data
#-----------------------------------------
make_data_heteroscedastic <- function(n) {
  tibble(x = runif(n, min = 0, max = 1)) %>%
    mutate(
      y_true = sin(2 * pi * x) + x,
      sigma = 0.1 + 0.9 * x,          # noise grows with x
      y = y_true + rnorm(n, sd = sigma)
    )
}


#-----------------------------------------
# SIMULATE DATA
#-----------------------------------------
set.seed(123)
n <- 5000
df <- make_data(n)
df <- make_data_heteroscedastic(n)

df %>% 
  ggplot(aes(x, y)) + 
  geom_point(alpha = 0.1) +
  ggtitle("Simulated training data")

#-----------------------------------------
# SPLIT DATA (train / calibration / test)
#-----------------------------------------
df <- df %>% 
  mutate(split = sample(c("train", "calibration", "test"), 
                        size = n(), replace = TRUE, 
                        prob = c(0.6, 0.2, 0.2)))

train_df <- df %>% filter(split == "train") 
calibration_df <- df %>% filter(split == "calibration")
test_df <- df %>% filter(split == "test")

#-----------------------------------------
# TRAIN A SIMPLE MODEL
#-----------------------------------------
nnet_wflow <- 
  workflow(y ~ x, mlp(hidden_units = 4) %>% set_mode("regression"))

nnet_fit <- nnet_wflow %>% fit(train_df)


#-----------------------------------------
# CALIBRATION STEP (simplest version)
#-----------------------------------------
train_df <- train_df %>%
  mutate(pred = predict(nnet_fit, train_df) %>% pull(.pred))

calibration_df <- calibration_df %>% 
  mutate(pred = predict(nnet_fit, calibration_df) %>% pull(.pred),
         residuals = abs(y - pred))

# Histogram of residuals
calibration_df %>%
  ggplot(aes(residuals)) +
  geom_histogram(bins = 40, fill = "skyblue", color = "white") +
  geom_vline(xintercept = quantile(calibration_df$residuals, 0.9),
             color = "red", linetype = "dashed") +
  ggtitle("Calibration residuals and 90% quantile")

# Extract q90
q90 <- as.numeric(quantile(calibration_df$residuals, 0.9))

#-----------------------------------------
# DEPLOY TO TEST SET
#-----------------------------------------
test_df <- test_df %>% 
  mutate(pred = predict(nnet_fit, test_df) %>% pull(.pred),
         lower = pred - q90,
         upper = pred + q90)

ggplot(test_df) + 
  geom_point(aes(x, y), alpha = 0.2) + 
  geom_line(aes(x, pred), col = "red", size = 1) + 
  geom_ribbon(aes(x, ymin = lower, ymax = upper), alpha = 0.3, fill  = "blue") +
  ggtitle("Conformal interval with absolute residuals")

#-----------------------------------------
# COVERAGE
#-----------------------------------------
test_df <- test_df %>% 
  mutate(covered = (y > lower & y < upper))

mean(test_df$covered)  # empirical coverage

# Sort test set by x (important for rolling windows)
test_df <- test_df %>% arrange(x)

# Rolling coverage with window size k
k <- 200  # you can tweak this
test_df <- test_df %>%
  mutate(covered = (y > lower & y < upper),
         rolling_coverage = rollmean(as.numeric(covered), k, 
                                     fill = NA, align = "center"))

ggplot(test_df, aes(x, rolling_coverage)) +
  geom_line(color = "blue", size = 1) +
  geom_hline(yintercept = 0.9, color = "red", linetype = "dashed") +
  ylim(0,1) +
  ggtitle("Rolling coverage across x") +
  ylab("Proportion covered") +
  xlab("x") +
  theme_minimal()


