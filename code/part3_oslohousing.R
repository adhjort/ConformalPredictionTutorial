source("./code/part3_preparedata.R")
library(dplyr)
library(xgboost)
library(rsample)
library(matrixStats)
library(tidyverse)
library(ggplot2)



# Formulas
model_formula = as.formula(SalePrice ~ PRom + Longitude + 
                             Latitude + Altitude + NumberOfBedrooms + 
                             Floor + YearsSinceBuilt + CoastDistance + 
                             LakeDistance + 
                             NumberOfUnitsOnAddress + 
                             CityDistrict + HomesNearby + OtherBuildingsNearby + Balcony + Elevator)


df_oslo

ggplot(df_oslo, 
       aes(x = Longitude, y = Latitude, col = log(SalePrice))) + 
  geom_point() + 
  scale_color_viridis_c() +
  theme_minimal()


# --- Split data: train / calibration / test ---
set.seed(123)

df_oslo = df_oslo %>% 
  mutate(split = sample(c("train", "calibration", "test"), 
                        size = n(), replace = TRUE, 
                        prob = c(0.6, 0.2, 0.2)))

df_train <- df_oslo %>% filter(split == "train") 
df_calibration <- df_oslo %>% filter(split == "calibration")
df_test <- df_oslo %>% filter(split == "test")

# --- Prepare matrices for XGBoost ---
# Create model matrix from formula (handles factors automatically)
X_train <- model.matrix(model_formula, data = df_train)[, -1]
y_train <- df_train$SalePrice

X_cal <- model.matrix(model_formula, data = df_calibration)[, -1]
y_cal <- df_calibration$SalePrice

X_test <- model.matrix(model_formula, data = df_test)[, -1]
y_test <- df_test$SalePrice

# Convert to DMatrix (xgboost format)
dtrain <- xgb.DMatrix(data = X_train, label = y_train)
dcal   <- xgb.DMatrix(data = X_cal)
dtest  <- xgb.DMatrix(data = X_test)

# --- Fit XGBoost model ---
params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  max_depth = 6,
  eta = 0.1,
  subsample = 0.8,
  colsample_bytree = 0.8
)


xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 1000,
  verbose = TRUE
)

# --- Predict on calibration and test sets ---
pred_cal <- predict(xgb_model, dcal)
pred_test <- predict(xgb_model, dtest)

# --- Calibrate prediction intervals ---
scores <- abs(pred_cal - y_cal)
q90 <- quantile(scores, 0.9)

# --- Create prediction intervals on test set ---
test_results <- df_test %>%
  mutate(
    y_true = y_test,
    y_pred = pred_test,
    lower = y_pred - q90,
    upper = y_pred + q90,
    covered = (y_true >= lower & y_true <= upper)
  )

# --- Evaluate coverage ---
coverage <- mean(test_results$covered)
coverage


# --- Calculate coverage per CityDistrict ---
district_coverage <- test_results %>%
  group_by(CityDistrict) %>%
  summarise(
    coverage = mean(covered, na.rm = TRUE),
    width = mean(upper - lower), 
    n = n()
  ) %>%
  mutate(method = "CP")

# --- Bar plot of coverage per district ---
ggplot(district_coverage, aes(x = reorder(CityDistrict, coverage), y = coverage)) +
  geom_col(fill = "steelblue") +
  geom_hline(yintercept = 0.9, linetype = "dashed", color = "red", size = 1) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(
    x = "",
    y = "Empirical Coverage"
  ) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom")


### SPATIAL

rho <- 1e6  # meters
alpha <- 0.1

# Calibration residuals + coords
scores <- abs(pred_cal - y_cal)
cal_coords <- df_calibration %>%
  dplyr::select(Longitude, Latitude)

# Test coords + predictions
test_coords <- df_test %>%
  dplyr::select(Longitude, Latitude)

# --- Function to compute spatially weighted quantile (Euclidean distance) ---
spatial_q90 <- function(test_coords, cal_coords, scores, rho, q = 0.9) {

  dists <- sqrt((cal_coords$Longitude - test_coords$Longitude)^2 + (cal_coords$Latitude - test_coords$Latitude)^2)
  
  # Spatial weights
  w <- exp(-dists / rho)
  w <- w / sum(w)
  
  # Weighted quantile
  w = modi::weighted.quantile(scores, w, prob = q)
  return(w)
}

# ---- Loop over test set ----
q90_i <- numeric(nrow(test_coords))

for (i in seq_len(nrow(test_coords))) {
  if( i %% 500 == 0){cat("i: ", i, "\n")}
  q90_i[i] <- spatial_q90(
    test_coords[i, ],
    cal_coords,
    scores,
    rho = 500,
    q = 0.9
  )
}


# ---- Construct results ----
test_results_spatial <- df_test %>%
  mutate(
    y_true = y_test,
    y_pred = pred_test,
    q90_i = q90_i,
    lower = y_pred - q90_i,
    upper = y_pred + q90_i,
    covered = (y_true >= lower & y_true <= upper)
  )

# ---- Evaluate coverage ----
coverage_spatial <- mean(test_results_spatial$covered)
coverage_spatial


# --- Coverage per CityDistrict: Spatial CP ---
district_coverage_spatial <- test_results_spatial %>%
  group_by(CityDistrict) %>%
  dplyr::summarize(
    coverage = mean(covered, na.rm = TRUE),
    width = mean(upper - lower), 
    n = n()
  ) %>%
  mutate(method = "Spatial CP")

# --- Combine both ---
district_coverage_both <- bind_rows(district_coverage, 
                                    district_coverage_spatial)

# --- Bar plot: two bars per district ---
ggplot(district_coverage_both,
       aes(x = reorder(CityDistrict, coverage), y = coverage, fill = method)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_hline(yintercept = 0.9, linetype = "dashed", color = "red", size = 1) +
  coord_flip() +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  scale_fill_manual(
    values = c("steelblue", "darkorange"),
    name = "Method"
  ) +
  labs(x = "", y = "Empirical Coverage") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom")

