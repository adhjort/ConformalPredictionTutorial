library(tidymodels)
library(nnet)

########

make_data <- function(n, std_dev = 1 / 5) {
  tibble(x = runif(n, min = -1)) %>%
    mutate(
      y = (x^3) + 2 * exp(-6 * (x - 0.3)^2),
      y = y + rnorm(n, sd = std_dev)
    )
}

n <- 1000
set.seed(8383)
train_data <- make_data(n)

train_data %>% 
  ggplot(aes(x, y)) + 
  geom_point(alpha = 1 / 10)


nnet_wflow <- 
  workflow(y ~ x, mlp(hidden_units = 4) %>% set_mode("regression"))

nnet_fit <- nnet_wflow %>% fit(train_data)
nnet_pred <- augment(nnet_fit, train_data)

train_data %>% 
  ggplot(aes(x)) + 
  geom_point(aes(y = y), alpha = 1 / 10) +
  geom_line(data = nnet_pred, aes(y = .pred),
            linewidth = 1, col = "blue")

cal_data  <- make_data(250)
test_data <- make_data(10000)

split_int <- int_conformal_split(nnet_fit, cal_data)
split_int

# Produce 90% prediction intervals
test_split_res <- 
  predict(split_int, test_data, level = 0.90) %>% 
  bind_cols(test_data)

test_split_res %>% slice(1:5)

coverage <- function(x) {
  x %>% 
    mutate(in_bound = .pred_lower <= y & .pred_upper >= y) %>% 
    summarise(coverage = mean(in_bound) * 100)
}

ggplot(test_split_res) + 
  geom_point(aes(x = x, y = y), alpha = 0.1) + 
  geom_line(aes(x = x, y = .pred), col = "blue", linewidth = 1) + 
  geom_ribbon(aes(x = x, ymin = .pred_lower, ymax = .pred_upper), col = "red", linewidth = 1, alpha = 0)

