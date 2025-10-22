library(tidymodels)
library(nnet)
library(ggplot2)
library(zoo)

#-----------------------------------------
# DATA GENERATION WITH 5 GROUPS
#-----------------------------------------
make_data_groups <- function(n) {
  groups <- LETTERS[1:5]   # "A","B","C","D","E"
  tibble(
    x = runif(n, min = 0, max = 1),
    group = sample(groups, size = n, replace = TRUE)
  ) %>%
    mutate(
      y_true = sin(2 * pi * x) + x,
      # group-specific noise levels
      sigma = case_when(
        group == "A" ~ 0.2,
        group == "B" ~ 0.3,
        group == "C" ~ 0.5,
        group == "D" ~ 0.7,
        group == "E" ~ 1.5
      ),
      y = y_true + rnorm(n, sd = sigma)
    )
}

#-----------------------------------------
# SIMULATE DATA
#-----------------------------------------
set.seed(123)
n <- 5000
df <- make_data_groups(n)

ggplot(df, aes(x, y, color = group)) +
  geom_point(alpha = 0.2) +
  ggtitle("Simulated data with group-specific noise")

#-----------------------------------------
# SPLIT DATA
#-----------------------------------------
df <- df %>%
  mutate(split = sample(c("train", "calibration", "test"),
                        size = n(), replace = TRUE,
                        prob = c(0.6, 0.2, 0.2)))

train_df <- df %>% filter(split == "train")
calibration_df <- df %>% filter(split == "calibration")
test_df <- df %>% filter(split == "test")

#-----------------------------------------
# TRAIN NEURAL NET MODEL
#-----------------------------------------
nnet_recipe <- recipe(y ~ x + group, data = train_df)

nnet_wflow <-
  workflow() %>%
  add_recipe(nnet_recipe) %>%
  add_model(mlp(hidden_units = 6) %>% set_mode("regression"))

nnet_fit <- nnet_wflow %>% fit(train_df)

#-----------------------------------------
# CALIBRATION RESIDUALS
#-----------------------------------------
calibration_df <- calibration_df %>%
  mutate(pred = predict(nnet_fit, .) %>% pull(.pred),
         residuals = abs(y - pred))

#-----------------------------------------
# GLOBAL CP
#-----------------------------------------
q90_global <- quantile(calibration_df$residuals, 0.9)

test_df <- test_df %>%
  mutate(pred = predict(nnet_fit, .) %>% pull(.pred),
         lower_global = pred - q90_global,
         upper_global = pred + q90_global,
         covered_global = (y > lower_global & y < upper_global))

#-----------------------------------------
# MONDRIAN CP (group-wise calibration)
#-----------------------------------------
q90_by_group <- calibration_df %>%
  group_by(group) %>%
  summarise(q90 = quantile(residuals, 0.9), .groups = "drop")

test_df <- test_df %>%
  left_join(q90_by_group, by = "group") %>%
  mutate(lower_mondrian = pred - q90,
         upper_mondrian = pred + q90,
         covered_mondrian = (y > lower_mondrian & y < upper_mondrian))

#-----------------------------------------
# COVERAGE COMPARISON
#-----------------------------------------
coverage_summary <- test_df %>%
  group_by(group) %>%
  summarise(
    coverage_global = mean(covered_global),
    coverage_mondrian = mean(covered_mondrian)
  )

print(coverage_summary)

#-----------------------------------------
# BAR PLOT: COVERAGE BY GROUP
#-----------------------------------------
coverage_summary %>%
  pivot_longer(cols = c(coverage_global, coverage_mondrian),
               names_to = "method", values_to = "coverage") %>%
  ggplot(aes(group, coverage, fill = method)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = 0.9, linetype = "dashed", color = "red") +
  ylim(0,1) +
  ggtitle("Coverage by group: Global CP vs Mondrian CP") +
  ylab("Empirical coverage") +
  theme_minimal()

#-----------------------------------------
# VISUALIZE INTERVALS FOR ONE GROUP (optional)
#-----------------------------------------
ggplot(test_df %>% filter(group == "E"),
       aes(x, y)) +
  geom_point(alpha = 0.3) +
  geom_line(aes(y = pred), col = "red") +
  geom_ribbon(aes(ymin = lower_global, ymax = upper_global),
              fill = "blue", alpha = 0.2) +
  ggtitle("Global CP intervals for Group E (high noise)") +
  theme_minimal()

ggplot(test_df %>% filter(group == "E"),
       aes(x, y)) +
  geom_point(alpha = 0.3) +
  geom_line(aes(y = pred), col = "red") +
  geom_ribbon(aes(ymin = lower_mondrian, ymax = upper_mondrian),
              fill = "blue", alpha = 0.2) +
  ggtitle("Mondrian CP intervals for Group E (high noise)") +
  theme_minimal()

