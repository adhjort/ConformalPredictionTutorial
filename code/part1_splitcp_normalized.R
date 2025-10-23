source("./code/setup.R")

# -------------------------------------
# SIMULATE DATA
#-----------------------------------------
set.seed(123)
n <- 5000
df <- make_data_heteroscedastic(n)

df %>% 
  ggplot(aes(x, y)) + 
  geom_point(alpha = 0.1) +
  ggtitle("Heteroscedastic data")

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
# TRAIN MODEL
#-----------------------------------------
nnet_wflow <- 
  workflow(y ~ x, mlp(hidden_units = 4) %>% set_mode("regression"))

nnet_fit <- nnet_wflow %>% fit(train_df)

#-----------------------------------------
# PREDICTIONS + RESIDUALS
#-----------------------------------------
train_df <- train_df %>%
  mutate(pred = predict(nnet_fit, train_df) %>% pull(.pred),
         residuals = abs(y - pred))

calibration_df <- calibration_df %>%
  mutate(pred = predict(nnet_fit, calibration_df) %>% pull(.pred))

#-----------------------------------------
# MODEL RESIDUAL SCALE (sigma(x))
#-----------------------------------------
sigma_model <- lm(residuals ~ 1 + x, data = train_df)

train_df = train_df %>% mutate(sigma_hat = predict(sigma_model, train_df))

ggplot() +
  geom_point(data = train_df, aes(x, residuals), 
             alpha = 0.2, color = "grey50") +
  geom_line(data = train_df, aes(x, sigma_hat), 
            color = "blue", size = 1.2) +
  ylab("Residuals / sigma") +
  theme_minimal()


calibration_df <- calibration_df %>% 
  mutate(sigma = predict(sigma_model, newdata = calibration_df),
         residuals = abs(y - pred),
         scores = residuals / sigma)

# Histogram of normalized residuals
ggplot(calibration_df, aes(scores)) +
  geom_histogram(bins = 40, fill = "skyblue", color = "white") +
  geom_vline(xintercept = quantile(calibration_df$scores, 0.9),
             color = "red", linetype = "dashed") 

# Quantile of normalized residuals
q90_norm <- as.numeric(quantile(calibration_df$scores, 0.9))

#-----------------------------------------
# DEPLOY TO TEST SET
#-----------------------------------------
test_df <- test_df %>% 
  mutate(pred = predict(nnet_fit, test_df) %>% pull(.pred),
         sigma = predict(sigma_model, newdata = test_df),
         lower = pred - sigma * q90_norm,
         upper = pred + sigma * q90_norm)

ggplot(test_df) + 
  geom_point(aes(x, y), alpha = 0.2) + 
  geom_line(aes(x, pred), col = "red", size = 1) + 
  geom_ribbon(aes(x, ymin = lower, ymax = upper), alpha = 0.3, fill  = "blue") +
  ggtitle("Conformal interval with normalized residuals")

#-----------------------------------------
# COVERAGE
#-----------------------------------------
test_df <- test_df %>% 
  mutate(covered = (y > lower & y < upper))

mean(test_df$covered)  # empirical coverage

#-----------------------------------------
# COVERAGE
#-----------------------------------------
test_df <- test_df %>% 
  mutate(covered = (y > lower & y < upper))

mean(test_df$covered) 



# Rolling coverage with window size k
k <- 200  
test_df <- test_df %>%
  arrange(x) %>% 
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





