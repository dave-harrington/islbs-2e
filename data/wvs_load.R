load("./data/WVS_Cross-National_Wave_7_Rdata_v6_0.rdata")

wvs_working <- `WVS_Cross-National_Wave_7_v6_0` 


wvs_working <-  wvs_working |> 
  dplyr::select(B_COUNTRY_ALPHA,
                Q275,
                Q47) |> 
  rename(
    Country = B_COUNTRY_ALPHA
  ) |> 
  dplyr::filter(Country %in% c("ARG",
                               "BRA",
                               "CAN",  
                               "CHN",
                               "DEU",
                               "EGY",
                               "ETH",
                               "JPN",
                               "KEN",
                               "NZL",
                               "USA")
  ) 


wvs_edu_health <- wvs_working |> 
  mutate(Education_level = case_when(
    Q275 >= 0 & Q275 <= 2 ~ "Lower",
    Q275 >=3 & Q275 <= 5 ~ "Middle",
    Q275 > 5 ~ "High")
  ) |> 
  mutate(Education_level = factor(Education_level,
                                  levels = c("Lower",
                                             "Middle",
                                             "High"), 
                                  ordered = TRUE)) |> 
  mutate(Health_status = case_when(
    Q47 == 1 ~ "Very Good",
    Q47 == 2 ~ "Good",
    Q47 == 3 ~ "Fair",
    Q47 == 4 ~ "Poor",
    Q47 == 5 ~ "Very Poor"
  ))  |> 
  mutate(Health_status = factor(Health_status,
                                levels = c("Very Poor",
                                           "Poor",
                                           "Fair",
                                           "Good",
                                           "Very Good"),
                                ordered = TRUE)) |> 
  dplyr::select(-Q47, -Q275)

wvs_edu_health_cc <- wvs_edu_health |> 
  drop_na()

save(wvs_edu_health, file = "./data/wvs_edu_health.Rdata")
save(wvs_edu_health_cc, file = "./data/wvs_edu_health_cc.Rdata")

