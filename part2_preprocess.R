
electricity <- fread("./data/electricity-normalized.csv")

df = electricity %>%
  as.data.frame() %>%
  # Barber et al (2020): remove first part where 'transfer' doesn't vary
  slice(17761:n())  %>% 
  # Keep only between 9 and 12 in the morning
  filter(period > 18/48 & period < 24/48) %>% 
  dplyr::select(nswprice, nswdemand, vicprice, vicdemand, transfer)

#fwrite(x = df, file = "./data/electricity_cleaned.csv")

