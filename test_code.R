
### Draft code.  Final versions are in qmd files

# clear workspace

rm(list = ls())

# intro-to-data

LEAP_analyzed <- LEAP |> 
  dplyr::filter(stratum == "SPT-Negative") |> 
  dplyr::filter(!is.na(overall.V60.outcome))

LEAP_analyzed |> 
  count(overall.V60.outcome, treatment.group) |> 
  group_by(treatment.group) |> 
  pivot_wider(names_from = overall.V60.outcome, 
              values_from = n) |> 
  relocate(`FAIL OFC`, .after = `PASS OFC`) |>
  adorn_totals(where = c("row", "col"), name = "Sum") |> 
  kbl(linesep = "", booktabs = TRUE) |> 
  kable_styling(
    bootstrap_options = c("striped", "condensed"),
    latex_options = c("striped")
  )





# crabs


library(patchwork)
p1  <- ggplot(crabs, aes(x = satell, y = color)) +
  geom_boxplot() +
  scale_color_openintro("two") +
  labs(y = "Color", x = NULL) +
  scale_x_continuous(breaks = seq(0, 16, 4))

p2  <- ggplot(crabs, aes(x = satell, y = spine)) +
  geom_boxplot() +
  scale_color_openintro("two") +
  labs(y = "Spine condition", x = "Number of satellites") +
  scale_x_continuous(breaks = seq(0, 16, 4))

p1/p2 + plot_annotation(title = "Two stacked bar plots", tag_levels = "a")

library(cowplot)
p1  <- ggplot(crabs, aes(x = satell, y = color)) +
  geom_boxplot() +
  scale_color_openintro("two") +
  labs(y = "Color", x = NULL) +
  scale_x_continuous(breaks = seq(0, 16, 4))

p2  <- ggplot(crabs, aes(x = satell, y = spine)) +
  geom_boxplot() +
  scale_color_openintro("two") +
  labs(y = "Spine condition", x = "Number of satellites") +
  scale_x_continuous(breaks = seq(0, 16, 4))

p1/p2



# side by side box plots

ggplot(portland_park_trees, aes(
  y = Carbon_Storage_lb, 
  x = reorder(Condition, -Carbon_Storage_lb))) +
  geom_boxplot() +
  scale_color_openintro("two") +
  labs(y = "Tree Condition", x = "Carbon Storage (lbs") +
  scale_y_continuous(
    breaks = seq(0, 16000, 4000))

# frogs
# This dataset is not used, but some code kept for examples
df <- frog
library(beeswarm)
# alternative beeswarm, base R

beeswarm(df$clutch.volume,
         horizontal = TRUE,
         col = "blue",
         method = "compactswarm",
         cex = 0.7,
         axes = FALSE,
        # axis(side = 1),
         xlab = "Clutch volume")
axis(1, seq(0,2800, 500))

#  portland trees

library(pdxTrees)
set.seed(080546)
pdx <- get_pdxTrees_parks()
portland_park_trees  <- pdx  |> 
  select(Common_Name, 
         Condition, 
         Tree_Height, 
         Structural_Value, 
         Carbon_Storage_lb, 
         Pollution_Removal_value
         ) |> 
  dplyr::slice_sample(n = 500)

  # saving for easy use later

 
# reordering factor levels

trees_boxplot   <- portland_park_trees  |> 
	mutate(cond = as.factor(Condition))  
fct_relevel(trees_boxplot$cond, "Dead", "Poor", "Fair", "Good")

ggplot(trees_boxplot, aes(x = Carbon_Storage_lb, y = cond)) +
  geom_boxplot() +
  scale_color_openintro("two") +
  labs(y = "Tree Condition", x = "Carbon Storage") +
  scale_x_continuous(
  	breaks = seq(0, 16000, 4000))

# reordering the fill var



load("./data/WVS_Cross-National_Wave_7_Rdata_v6_0.rdata")

wvs_working<- `WVS_Cross-National_Wave_7_v6_0` 



wvs_working <-  wvs_working |> 
  dplyr::select(B_COUNTRY_ALPHA,
                Q275,
                Q47) |> 
  dplyr::rename(
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


df <- `WVS_Cross-National_Wave_7_v6_0` 
addmargins(table(df$Q47)) 
addmargins(table(df$Q275))




# Danish ED

df <- danish.ed.primary
summary(df$age)
hist(df$age^2)
table(df$sex, df$triage)
boxplot(df$age)


ggplot(portland_park_trees, aes(x = Tree_Height, y = Carbon_Storage_lb)) +
	geom_point(alpha = 0.3, fill = IMSCOL["black", "full"], shape = 21) +
	labs(x = "Tree Height (ft)", y = "Carbon Storage (lbs)")

# old style table; pipe tables are better than using kable below

ggplot(portland_park_trees, aes(x = Tree_Height, y = sqrt(Carbon_Storage_lb))) +
	geom_point(alpha = 0.3, fill = IMSCOL["black", "full"], shape = 21) +
	labs(x = "Tree Height (ft)", y = "Square root of Carbon Storage (lbs)")



# wvs tables
# 
df <- wvs_edu_health_cc

t <-  addmargins(table(df$Education_level, df$Health_status))
t["High","Very Good"]

hist(wdi_2022$gni_percap)
li <- log(wdi_2022$gni_percap)
hist(li)
lq <- sqrt(wdi_2022$gni_percap)
hist(lq)

# tables from TB data

names(tb.interruption)

tb.interruption  |> 
  na.omit(age.group)  |> 
  na.omit(hiv.test)  |> 
  dplyr::count(age.group, hiv.test)  |> 
  group_by(age.group) |> 
  pivot_wider(names_from = hiv.test, 
              values_from = n) |> 
  adorn_totals(where = c("row", "col")) |> 
  kbl(linesep = "", booktabs = TRUE) |> 
  kable_styling(
    bootstrap_options = c("striped", "condensed"),
    latex_options = c("striped")
  )

# tables from wvs
# 
df <- load("./data/wvs_subset")



```



tb.interruption  |> 
  na.omit(age.group)  |> 
  na.omit(hiv.test)  |> 
  dplyr::count(age.group, hiv.test)  |> 
  group_by(age.group) |> 
  pivot_wider(names_from = hiv.test, 
              values_from = n) |> 
  adorn_totals(where = c("row", "col")) |> 
  kbl(linesep = "", booktabs = TRUE) |> 
  kable_styling(
    bootstrap_options = c("striped", "condensed"),
    latex_options = c("striped")
  )
