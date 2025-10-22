library(dplyr)

library(pintervals)
library(tibble)

x1 <- runif(1000)
x2 <- runif(1000)
y <- rlnorm(1000, meanlog = x1 + x2, sdlog = 0.5)
df <- tibble(x1, x2, y)

df_train <- df %>% slice(1:500)
df_cal <- df %>% slice(501:750)
df_test <- df %>% slice(751:1000)
mod <- lm(log(y) ~ x1 + x2, data=df_train)
calib <- exp(predict(mod, newdata=df_cal))
calib_truth <- df_cal$y
pred_test <- exp(predict(mod, newdata=df_test))

res = pinterval_conformal(pred_test,
                    calib = calib,
                    calib_truth = calib_truth,
                    alpha = 0.7,
                    lower_bound = 0,
                    grid_size = 10000) %>% 
  cbind(df_test) %>% 
  mutate(Coverage = (y > lower_bound) & (y < upper_bound))

mean(res$Coverage)



library(ggplot2)

ggplot(res) + 
  geom_point(aes(x = x1, y = y)) + 
  geom_line(aes(x = x1, y = pred)) + 
  geom_ribbon(aes(x = x1, ymin = lower_bound, ymax = upper_bound), alpha = 0.1, fill = "blue")
