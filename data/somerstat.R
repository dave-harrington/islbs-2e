# loading Somerville data
# assumes ./data is working directory
library(readr)
Somerville_Happiness_Survey_Responses_20250412 <- read_csv("data/Somerville_Happiness_Survey_Responses_20250412.csv")

somerstat <- Somerville_Happiness_Survey_Responses_20250412

save(somerstat, file = "somerstat.Rdata")

somerstat_2023 <- somerstat |> 
  filter(Year == 2023)

save(somerstat_2023, file = "somerstat_2023.Rdata")



table(somerstat_2023$Survey.Method)
table(somerstat_2023$Difficulty.Paying.YN, useNA = "ifany")

df <- somerstat_2023

table(df$Race.Ethnicity, df$Difficulty.Paying.YN, useNA = "ifany")
table(df$Disability.YN, df$Difficulty.Paying.YN, useNA = "ifany")

table(df$Housing.Status, df$Difficulty.Paying.YN, useNA = "ifany")
table(df$COVID.Resources.Satisfaction.5pt.label, useNA = "ifany")

table(df$COVID.Resources.Satisfaction.5pt.label, df$Disability.YN, useNA = "ifany")
table(df$COVID.Resources.Satisfaction.5pt.num, df$Disability.YN, useNA = "ifany")

ggplot(somerstat_2023, aes(x = Disability.YN, fill = COVID.Resources.Satisfaction.5pt.label)) +
  geom_bar(position = "dodge") +
  labs(x = "Presence of Disability",
       y = "Count",
       fill = "Satisfaction Covid Response") +
  theme(legend.position = "bottom")

ggplot(somerstat_2023, aes(x = Disability.YN, fill = Difficulty.Paying.YN)) +
  geom_bar(position = "dodge") +
  labs(x = "Presence of Disability",
       y = "Count",
       fill = "Difficulty paying") +
  theme(legend.position = "bottom")

ggplot(somerstat_2023) +
  geom_mosaic(aes(x = product(Disability.YN), fill = Streets.Sidewalks.Maintenance.Satisfaction.5pt.label)) +
  labs(x = "Disability", y = "Sidewalk satisfaction") +
  guides(fill = FALSE)


table(df$Streets.Sidewalks.Maintenance.Satisfaction.5pt.label, useNA = "ifany")
table(df$Disability.YN, df$Streets.Sidewalks.Maintenance.Satisfaction.5pt.label, useNA = "ifany")

somerstat_sidewalks <-  somerstat_2023 |> 
  drop_na(Sidewalks.Accessibility.Satisfaction.5pt.label, Disability.YN) |> 
  mutate(Sidewalks.Accessibility = fct_relevel(Sidewalks.Accessibility.Satisfaction.5pt.label,
                                                                "Very Unsatisfied",
                                                                "Unsatisfied",
                                                                "Neutral",
                                                                "Satisfied",
                                                                "Very Satisfied"))|>
  mutate(Sidewalks.Accessibility = factor(Sidewalks.Accessibility,
                                         ordered = TRUE))  
    
ggplot(somerstat_sidewalks) +
  geom_mosaic(aes(x = product(Disability.YN), fill = Sidewalks.Accessibility)) +
  labs(x = "Disability", y = "Sidewalk satisfaction") +
  guides(fill = FALSE)
