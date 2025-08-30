

# remotes::install_github("kjhealy/gssrdoc")
# remotes::install_github("kjhealy/gssr")

library(gssr)
library(gssrdoc)
library(dplyr)
library(tidyverse)

#fetch data

data(gss_all)


gss_df <- gss_all |> 
  dplyr::select(year,
         age,
         sex,
         degree,
         wrkstat,
         health) |> 
  dplyr::filter(year >= 2018) 


save(gss_df, file = "gss_df.Rdata")



gen_ss<- gss_df |> 
  drop_na() |> 
  dplyr::mutate(health_status = factor(health,
                                levels = c(1, 2, 3, 4),
                                labels = c("excellent",
                                           "good",
                                           "fair",
                                           "poor"),
                                ordered = TRUE)) |> 
  dplyr::mutate(work_status = factor(wrkstat,
                              levels = c(1, 2, 3, 4, 5, 6, 7, 8),
                              labels = c("full time",
                                         "part time",
                                         "temp. not working",
                                         "unemployed",
                                         "retired",
                                         "in school",
                                         "keeping house",
                                         "other"),
                              ordered = TRUE)) |> 
  dplyr::mutate(sex = case_when(
    sex == 1 ~ "male",
    sex == 2 ~ "female"),
    sex = factor(sex)) |> 
  dplyr::mutate(age_group = cut(
    age,
    breaks = c(0, 25, 45, 65, Inf),       # cut points
    labels = c("18 to 25", "26 to 45", "46 to 65", "66+"),
    right = FALSE,
    ordered_result = TRUE))  |> 
  dplyr::mutate(degree_status = factor(degree,
                                levels = c(0, 1, 2, 3, 4),
                                labels = c("< high school",
                                           "high school",
                                           "assoc.",
                                           "bachelor's",
                                           "graduate"),
                                ordered = TRUE)) 

save(gen_ss, file = "gen_ss.Rdata")


