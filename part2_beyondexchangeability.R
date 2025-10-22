# =========================================================
# Libraries
# =========================================================
library(tidyverse)
library(data.table)
library(dplyr)
library(tidyverse)
library(data.table)

# --- 1. Read and prepare data ----


set.seed(123)
electricity <- fread("./data/electricity-normalized.csv")
df = electricity %>%
  as.data.frame() %>%
  # Barber et al (2020): remove first part where 'transfer' doesn't vary
  slice(17761:n())  %>% 
  # Keep only between 9 and 12 in the morning
  filter(period > 18/48 & period < 24/48) %>% 
  # Add an index
  mutate(index = row_number())


ggplot(df, aes(x = index, y = vicprice)) + 
  geom_point()

GGally::ggpairs(df %>% dplyr::select(covariate_col, response_col))




covariate_col <- c("nswprice", "nswdemand", "vicprice", "vicdemand")
response_col  <- "transfer"


#Date: date between 7 May 1996 to 5 December 1998. Here normalized between 0 and 1
#Day: day of the week (1-7)
#Period: time of the measurement (1-48) in half hour intervals over 24 hours. Here normalized between 0 and 1
#NSWprice: New South Wales electricity price, normalized between 0 and 1
#NSWdemand: New South Wales electricity demand, normalized between 0 and 1
#VICprice: Victoria electricity price, normalized between 0 and 1
#VICdemand: Victoria electricity demand, normalized between 0 and 1
#transfer: scheduled electricity transfer between both states, normalized between 0 and 1


covariate_col <- c("nswprice", "nswdemand", "vicprice", "vicdemand")
response_col  <- "transfer"

N <- nrow(df)
alpha <- 0.1
train_lag <- 100
rho <- 0.99


##########
CP_LS <- function(X, Y, x, alpha = 0.1) {
  beta_hat <- solve(t(X) %*% X, t(X) %*% Y)
  residuals <- abs(Y - X %*% beta_hat)
  y_hat <- as.numeric(x %*% beta_hat)
  q <- quantile(residuals, 1 - alpha)
  tibble(pred = y_hat, lower = y_hat - q, upper = y_hat + q)
}

weighted_quantile <- function(values, probs, weights) {
  ord <- order(values)
  values <- values[ord]
  weights <- weights[ord] / sum(weights)
  cdf <- cumsum(weights)
  approx(cdf, values, xout = probs, rule = 2)$y
}

nexCP_LS <- function(X, Y, x, alpha = 0.1, weights = NULL) {
  if (is.null(weights)) weights <- rep(1, length(Y))
  beta_hat <- solve(t(X) %*% X, t(X) %*% Y)
  residuals <- abs(Y - X %*% beta_hat)
  y_hat <- as.numeric(x %*% beta_hat)
  q <- weighted_quantile(residuals, 1 - alpha, weights)
  tibble(pred = y_hat, lower = y_hat - q, upper = y_hat + q)
}


run_conformal <- function(df, alpha = 0.1, train_lag = 100, rho = 0.99, method = "CP+LS") {
  X <- as.matrix(df[, covariate_col])
  Y <- df[[response_col]]
  N <- nrow(df)
  
  results <- vector("list", N)
  
  for (n in seq(train_lag, N)) {
    X_train <- X[1:(n-1), , drop = FALSE]
    Y_train <- Y[1:(n-1)]
    x_new <- X[n, , drop = FALSE]
    
    if (method == "CP+LS") {
      res <- CP_LS(X_train, Y_train, x_new, alpha)
    } else if (method == "nexCP+LS") {
      w <- rho^rev(seq_len(n-1))
      res <- nexCP_LS(X_train, Y_train, x_new, alpha, w)
    } 
    
    
    results[[n]] <- res %>%
      mutate(i = n, y = Y[n], Method = method)
    
    if (n %% 500 == 0) cat("Processed:", n, "of", N, "\n")
  }
  
  bind_rows(results) %>% drop_na()
}

df_cp <- run_conformal(df, alpha, train_lag, rho, method = "CP+LS")
df_nex <- run_conformal(df, alpha, train_lag, rho, method = "nexCP+LS")

final_df <- bind_rows(df_cp, df_nex) %>%
  mutate(Covered = (y >= lower & y <= upper),
         Width = upper - lower)


summary_tbl <- final_df %>%
  group_by(Method) %>%
  summarise(
    MeanCoverage = mean(Covered, na.rm = TRUE),
    MeanWidth = mean(Width, na.rm = TRUE)
  )

summary_tbl


window <- 300

final_df <- final_df %>%
  group_by(Method) %>%
  arrange(i) %>%
  mutate(
    RollingCoverage = zoo::rollapply(Covered, window, mean, fill = NA, align = "right"),
    RollingWidth = zoo::rollapply(Width, window, mean, fill = NA, align = "right")
  )



ggplot(final_df, aes(x = i, y = RollingCoverage, color = Method)) +
  geom_line(size = 1.1) +
  geom_hline(yintercept = 1 - alpha, linetype = "dashed", color = "gray40") +
  labs(
    title = "Rolling Coverage over Time (ELEC2)",
    y = "Coverage (Rolling Mean)",
    x = "Time index"
  ) +
  theme_minimal(base_size = 14)


