
### Draft code.  Final versions are in qmd files

# clear workspace

rm(list = ls())



# crabs
# 
# methods for aligning plots.
#   Maybe us gridArrange()


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

p4 <- ggplot(crabs, aes(x = width, y = satell)) +
  geom_point() +
  geom_smooth(se = FALSE, color = IMSCOL["red", "full"])

p4

p5 <- ggplot(crabs, aes(x = weight, y = satell)) +
  geom_point() +
  geom_smooth()

p5



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


# Danish ED

# useful example for relationships

df <- danish.ed.primary
summary(df$age)
hist(df$age^2)
table(df$sex, df$triage)
boxplot(df$age)

library(RJSONIO)

# Fetch the metadata
metadata <- 
  fromJSON("https://ourworldindata.org/grapher/correlation-between-child-mortality-and-mean-years-of-schooling-for-those-aged-15-and-older.metadata.json?v=1&csvType=filtered&useColumnShortNames=true&tab=table")


mort_ed_owd <- read.csv("https://ourworldindata.org/grapher/correlation-between-child-mortality-and-mean-years-of-schooling-for-those-aged-15-and-older.csv?v=1&csvType=filtered&useColumnShortNames=true&tab=table&time=2020")


plot_data <- mort_ed_owd |> 
  filter(Year == 2020) |> 
  rename(c_mortality= obs_value__indicator_under_five_mortality_rate__sex_total__wealth_quintile_total__unit_of_measure_deaths_per_100_live_births) |> 
  rename(f_education = f_youth_and_adults__15_64_years__average_years_of_education) |> 
  drop_na(c_mortality, f_education) |> 
  dplyr::filter(Code != "" ) |> 
  dplyr::filter(Entity != "World")
 


