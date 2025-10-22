#df_oslo_raw = fread("N:/EVAnalytics_Git_Data/ah/Paper 2/oslo_2019_2021.csv")

#df_oslo_raw = data.table::fread("N:/EVAnalytics_Git_Data/ah/Paper 2/oslo_2018_2019.csv")

df_oslo_raw = data.table::fread("V:/EVAnalytics_Git_Data/ah/Paper 2/oslo_2018_2019_with_unitID.csv")

df_oslo = df_oslo_raw %>% 
  filter(CityDistrict != "Ikke by",
         CityDistrict != "SENTRUM",
         EstateType == "Leilighet") %>%
  dplyr::rename(SalePrice = TargetPriceTotal) %>%
  mutate(HomesNearby = NumberOfFlatsInAdjoiningSquares + 
           NumberOfDetachedHousesInAdjoiningSquares + 
           NumberOfAttachedHousesInAdjoiningSquares + 
           NumberOfSemiDetachedHousesInAdjoiningSquares, 
         HomesNearbyX2 = NumberOfFlatsInAdjoiningSquaresX2 + 
           NumberOfDetachedHousesInAdjoiningSquaresX2 + 
           NumberOfAttachedHousesInAdjoiningSquaresX2 + 
           NumberOfSemiDetachedHousesInAdjoiningSquaresX2) %>%
  
  # Make OtherBuildingsNearby 
  mutate(OtherBuildingsNearby = NumberOfUnitsInAdjoiningSquares - HomesNearby, 
         OtherBuildingsNearbyX2 = NumberOfUnitsInAdjoiningSquaresX2 - HomesNearbyX2) %>%
  
  # Remove all the specific ones since we have now made the aggregated HomesNearby and OtherBuildingsNearby
  dplyr::select(-NumberOfUnitsInAdjoiningSquares,
                -NumberOfFlatsInAdjoiningSquares,
                -NumberOfDetachedHousesInAdjoiningSquares,
                -NumberOfAttachedHousesInAdjoiningSquares,
                -NumberOfSemiDetachedHousesInAdjoiningSquares,
                -NumberOfUnitsInAdjoiningSquaresX2,
                -NumberOfFlatsInAdjoiningSquaresX2,
                -NumberOfDetachedHousesInAdjoiningSquaresX2,
                -NumberOfAttachedHousesInAdjoiningSquaresX2,
                -NumberOfSemiDetachedHousesInAdjoiningSquaresX2, 
                -FootprintArea, 
                -SiteArea, 
                -Municipality, 
                -MunicipalityID, 
                -EstateType, 
                -KNr, 
                -SiteAreaUndeveloped, 
                -BuildYear) %>%
  
  # Make a month dummy 
  mutate(SaleMonth = lubridate::month(TargetPriceSaleDate), 
         SaleYear = lubridate::year(TargetPriceSaleDate)) %>% 
  #mutate(SaleMonth = SaleMonth*(1 + (SaleYear == 2019))) %>% <--- NOT GOOD!!
  mutate(SaleMonth = ifelse(SaleYear == 2019, 12 + SaleMonth, SaleMonth)) %>%
  
  # Divide price for convenience (?)
  mutate(SalePrice = SalePrice/1000000) %>%
  mutate(Floor = as.numeric(Floor), 
         NumberOfBedrooms = as.numeric(NumberOfBedrooms))


Encoding(df_oslo$CityDistrict) = "UTF-8"
Encoding(df_oslo$SubCityDistrictName) = "LATIN-1"

df_oslo = df_oslo %>%
  drop_na() %>% 
  mutate(SaleMonth = as.factor(SaleMonth)) 

