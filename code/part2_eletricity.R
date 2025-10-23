library(tidyverse)
library(data.table)
library(dplyr)
library(ggplot2)

######## DISCLAIMER ######## 
# This code is highly inspired by the Python code used in the paper "Conformal prediction beyond exchangeability" by Barber et al. 
# Her code can be downloaded from: https://rinafb.github.io/code/nonexchangeable_conformal.zip


# --- 1. Read and prepare data ----

set.seed(123)
electricity <- fread("./data/electricity-normalized.csv")
df = electricity %>%
  as.data.frame() %>%
  # Barber et al (2020): remove first part where 'transfer' doesn't vary
  dplyr::slice(17761:n())  %>% 
  # Keep only between 9 and 12 in the morning
  filter(period > 18/48 & period < 24/48) %>% 
  # Add a time index
  mutate(Time = row_number())


ggplot(df, aes(x = Time, y = transfer)) + 
  geom_point()


covariate_col <- c("nswprice", "nswdemand", "vicprice", "vicdemand")
response_col  <- "transfer"


#GGally::ggpairs(df %>% dplyr::select(covariate_col, response_col))


################################
### CONFORMAL APPROACH #########
################################

X <- df %>% select(covariate_col) %>% as.matrix() 
Y <- df %>% select(response_col) %>% as.matrix()
N <- nrow(df)
alpha <- 0.1
train_lag <- 100

results_cp <- data.frame()

for (n in seq(train_lag, N)) {
  # --- Prepare subset ---
  df_subset = df[1:(n-1), ]
  N_subset = NROW(df_subset)
  
  # Split into train and calibration
  train_index = sample(x = 1:N_subset, size = floor(0.6*N_subset), replace = FALSE)
  
  df_train = df_subset[train_index,]
  df_calibration = df_subset[-train_index,]
  
  # --- Fit model ---
  model = lm(transfer ~ nswprice + nswdemand + vicprice + vicdemand, df_train)
  
  # --- Predict ---
  calibration_preds = predict(model, df_calibration)
  test_pred = predict(model, df[n,]) %>% as.numeric()
  
  # --- Nonconformity scores ---
  scores = abs(calibration_preds - df_calibration$transfer)
  
  # --- Conformal prediction ---
  q90 = quantile(scores, probs = 0.9) %>% as.numeric()
  lower = test_pred - q90
  upper = test_pred + q90
  true_y = df$transfer[n]
  

  # --- Bookkeeping ---
  res_tmp = data.frame(n = n, 
                      y = true_y, 
                      q90 = q90, 
                      preds = test_pred, 
                      method = "CP", 
                      lower = lower, 
                      upper = upper) 
  
  results_cp = rbind(results_cp, res_tmp)
  
  if (n %% 500 == 0) cat("Processed:", n, "of", N, "\n")
  
}




##################################
### WEIGHTED CP (Barber et al) ###
##################################

weighted_quantile <- function(scores, weights, probs) {
  ord <- order(scores)
  scores <- scores[ord]
  weights <- weights[ord] / sum(weights)
  cdf <- cumsum(weights)
  q = approx(cdf, scores, xout = probs, rule = 2)$y
  
  return(q)
}


results_weighted_cp <- data.frame()

for (n in seq(train_lag, N)) {
  # --- Prepare subset ---
  df_subset = df[1:(n-1), ]
  df_subset$index = 1:(n-1)
  N_subset = NROW(df_subset)
  
  # Split into train and calibration
  train_index = sample(x = 1:N_subset, size = floor(0.6*N_subset), replace = FALSE)
  
  df_train = df_subset[train_index,]
  df_calibration = df_subset[-train_index,]
  
  # --- Fit model ---
  model = lm(transfer ~ nswprice + nswdemand + vicprice + vicdemand, df_train)
  
  # --- Predict ---
  calibration_preds = predict(model, df_calibration)
  test_pred = predict(model, df[n,]) %>% as.numeric()
  
  # --- Nonconformity scores ---
  scores = abs(calibration_preds - df_calibration$transfer) %>% as.numeric()
  
  # --- Weighted quantile ---
  rho = 0.99
  w = rho^seq(from = NROW(df_calibration), to = 1, by = -1)
  q90_weighted = weighted_quantile(scores = scores, weights = w, probs = 0.9)
  
  
  lower = test_pred - q90_weighted
  upper = test_pred + q90_weighted
  true_y = df$transfer[n]
  
  
  # --- Bookkeeping ---
  res_tmp = data.frame(n = n, 
                      y = true_y, 
                      q90 = q90_weighted, 
                      preds = test_pred, 
                      method = "Weighted CP", 
                      lower = lower, 
                      upper = upper) 

  results_weighted_cp = rbind(results_weighted_cp, res_tmp)
  
  if (n %% 500 == 0) cat("Processed:", n, "of", N, "\n")
  
}



#####################################
### ADAPTIVE CP (Gibbs & Candès) ####
#####################################

results_adaptive_cp = data.frame()
alpha_vec = rep(NA, N - train_lag)


alpha_target = 0.1   # target miscoverage
eta = 0.01           # learning rate for alpha adaptation
alpha_t = alpha_target

for (n in seq(train_lag, N)) {
  
  # --- Prepare subset ---
  df_subset = df[1:(n-1), ]
  N_subset = NROW(df_subset)
  
  # Split into train and calibration
  train_index = sample(x = 1:N_subset, size = floor(0.6*N_subset), replace = FALSE)
  df_train = df_subset[train_index,]
  df_calibration = df_subset[-train_index,]
  
  # --- Fit model ---
  model = lm(transfer ~ nswprice + nswdemand + vicprice + vicdemand, df_train)
  
  # --- Predict ---
  calibration_preds = predict(model, df_calibration)
  test_pred = predict(model, df[n,]) %>% as.numeric()
  
  # --- Nonconformity scores ---
  scores = abs(calibration_preds - df_calibration$transfer)
  
  # --- Adaptive quantile ---
  q_dynamic = quantile(scores, probs = 1 - alpha_t) %>% as.numeric()
  
  lower = test_pred - q_dynamic
  upper = test_pred + q_dynamic
  true_y = df$transfer[n]
  
  # --- Coverage check ---
  covered = (true_y >= lower & true_y <= upper)
  err_t = ifelse(covered, 0, 1)
  
  # --- Update alpha_t ---
  alpha_t = alpha_t - eta * (err_t - alpha_target)
  alpha_t = max(min(alpha_t, 0.5), 0.001)  # keep it within reasonable bounds
  
  # --- Bookkeeping ---
  res_tmp = data.frame(
    n = n,
    y = true_y,
    q90 = q_dynamic,
    preds = test_pred,
    method = "Adaptive CP",
    lower = lower,
    upper = upper
  )
  
  alpha_vec[n] = alpha_t
  
  
  results_adaptive_cp = rbind(results_adaptive_cp, res_tmp)
  
  if (n %% 500 == 0) cat("Processed:", n, "of", N, "current alpha_t:", round(alpha_t, 4), "\n")
}




###################################
### PLOT AND INSPECT RESULTS ######
###################################

final_df <- rbind(results_cp, 
                  results_weighted_cp, 
                  results_adaptive_cp) %>% 
  mutate(Covered = (y >= lower & y <= upper),
         Width = upper - lower)

# --- Coverage --- 
final_df %>% 
  group_by(method) %>% 
  summarize(Coverage = mean(Covered))

window <- 300

final_df_rolling <- final_df %>%
  group_by(method) %>% 
  arrange(n) %>%
  mutate(
    RollingCoverage = zoo::rollapply(Covered, window, mean, fill = NA, align = "right"),
    RollingWidth = zoo::rollapply(Width, window, mean, fill = NA, align = "right")
  )



ggplot(final_df_rolling, aes(x = n, y = RollingCoverage, col = method)) +
  geom_line(size = 1.1) +
  geom_hline(yintercept = 1 - alpha, linetype = "dashed", color = "gray40") +
  labs(
    y = "Coverage (Rolling Mean)",
    x = "Time index"
  ) +
  theme_minimal(base_size = 14)

# --- Plot alpha --- 
alpha_df = data.frame(Time = 1:length(alpha_vec), alpha = alpha_vec)
ggplot(data = alpha_df, 
       aes(x = Time, y = alpha)) + 
  geom_line() +
  theme_minimal(base_size = 14) +
  geom_hline(yintercept = 0.1, col = "red", lwd = 1)







