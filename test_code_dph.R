
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

# violin plots in dds
#

dds_reformat |> 
  filter(ethnicity == "Hispanic" | ethnicity == "White non-Hispanic") |> 
  ggplot(aes(
    y = expenditures, 
    x = age.cohort)) +
  geom_violin(alpha = 0.5) +
  labs(y = "Expenditures (USD)", x = "Age catetory") +
  stat_summary(fun.min = function(x) quantile(x, 0.25), 
               fun.max = function(x) quantile(x, 0.75), 
               geom = "linerange", color = "black", linewidth = 1.2) +
  stat_summary(fun = median, geom = "point", color = "red", size = 3) 
  

# gss data tables 
# 
# remotes::install_github("kjhealy/gssrdoc")
# remotes::install_github("kjhealy/gssr")

library(gssr)
library(gssrdoc)

install.packages("eulerr")
library(eulerr)

venn_model <- euler(c(
  A = 0.7,
  B = 0.6,
  "A&B" = 0.3
))

# covid 19 RAT example
# 


plot(venn_model,
     fills = c("cornflowerblue", "lightpink"), 
     quantities = list(type = percent),
     labels = TRUE)

sens_rat <- 0.80
spec_rat <- 0.98
prev_covid <- 0.01

venn_model <- euler(c(
covid19 = 0.01, # prev
positive_test = sens_rat * prev_covid + (1 - spec_rat) * (1 - prev_covid),
"covid19&positive_test" = sens_rat * prev_covid
))

plot(venn_model, fills = c("cornflowerblue", "lightpink"), labels = TRUE)

# tree diagrams from openintro function
# 
treeDiag(
  c("Flight on time?", "Luggage on time?"),
  c(0.8, 0.2), list(c(0.97, 0.03), c(0.15, 0.85))
)
treeDiag(c("Breakfast?", "Go to class"), c(.4, .6),
         list(c(0.4, 0.36, 0.34), c(0.6, 0.3, 0.1)), c("Yes", "No"),
         c("Statistics", "English", "Sociology"),
         showWork = TRUE
)
treeDiag(
  c("Breakfast?", "Go to class"), c(0.4, 0.11, 0.49),
  list(c(0.4, 0.36, 0.24), c(0.6, 0.3, 0.1), c(0.1, 0.4, 0.5)),
  c("one", "two", "three"), c("Statistics", "English", "Sociology")
)
treeDiag(c("Dow Jones rise?", "NASDAQ rise?"),
         c(0.53, 0.47), list(c(0.75, 0.25), c(0.72, 0.28)),
         solSub = list(c("(a)", "(b)"), c("(c)", "(d)")), solwd = 0.08
)

library(eulerr)

# this is the most promising

# Raw counts
raw_counts <- c("Female" = 50,          # 100 total in A minus 6 in overlap
                "85+" = 50,          #  total in B minus 30 in overlap
                "Female & 85+" = 10)        # intersection

# Convert to proportions
total <- sum(raw_counts)          # Total unique individuals: 110
proportions <- raw_counts / total

# Create Euler diagram from proportions
fit_prop <- euler(proportions)

# Plot with proportions shown as percentages
plot(fit_prop,
     fills = c("skyblue", "salmon"),
     quantities = list(type = "percent", digits = 1),
     labels = TRUE,
     main = "Venn Diagram with Proportions")



# example: A = 60, B = 50, overlap = 20
plot_venn2(60, 50, 20)

# Install if needed
install.packages("VennDiagram")

# Load package
library(VennDiagram)

# Draw Venn diagram with specified areas
draw.pairwise.venn(
  area1 = 100,     # set A
  area2 = 80,      # set B
  cross.area = 30, # overlap
  #  category = c("A", "B"),
  fill = c("skyblue", "orange"),
  lty = "blank",
  cex = 1.5,
  cat.cex = 1.5
)



n <- 1000000

library(eulerr)

plot(euler(c(A = n * 0.26, B = n * 0.113, "A&B" = n* 0.062 * 0.113)))

n <- 1000000
# Convert to proportions
raw <- c(A = n * 0.26, n * 0.112, "A&B" = n * 0.62 * 0.113)
total <- sum(
  raw["A"], raw["B"], -raw["A&B"]  # avoid double-counting the intersection
)

proportions <- raw / total

# Fit Euler diagram with proportions
fit_prop <- euler(proportions)




library(eulerr)

age

# Raw counts
raw_counts <- c("A" = 70,          # 100 total in A minus 30 in overlap
                "B" = 50,          # 80 total in B minus 30 in overlap
                "A&B" = 30)        # intersection

# Convert to proportions
total <- sum(raw_counts)          # Total unique individuals: 70 + 50 + 30 = 150
proportions <- raw_counts / total

# Create Euler diagram from proportions
fit_prop <- euler(proportions)

# Plot with proportions shown as percentages
plot(fit_prop,
     fills = c("skyblue", "salmon"),
     quantities = list(type = "percent", digits = 1),
     labels = TRUE,
     main = "Venn Diagram with Proportions")


# Load package
library(VennDiagram)

# Draw Venn diagram with specified areas
test.venn.plot <- draw.pairwise.venn(
  area1 = 0.505,     # set A
  area2 = 0.18,      # set B
  cross.area = 0.072, # overlap
  category = c("Female", "Age 80+"),
  fill = c(IMSCOL["blue", "full"], IMSCOL["red", "full"]),
  lty = "blank",
  cex = 1.5,
  cat.cex = 1.5,
  ext.text = FALSE
)
age-venn-plot

# are heights normal
# 
library(NHANES)
hist(NHANES$TotChol)
hist(NHANES$Height[Age > 20])
summary(NHANES$Age)
a <- NHANES$Age > 20 
b <- a & NHANES$Gender == female
hist(NHANES$Height[a])
hist(NHANES$Weight[a])
nrow(NHANES$Height[a])
h <- NHANES$Height[a]
nrow(h)
af <- NHANES$Age > 17 & NHANES$Gender == "female"
am <- NHANES$Age > 17 & NHANES$Gender == "male"
hist(NHANES$Height[af])
hist(NHANES$Height[am])
summary(NHANES$Height[af])
summary(NHANES$Height[am])
length(NHANES$Height[af])



#| label: fig-female-height-histogram-density-normal
#| fig-cap: |
#|   A histogram of the heights of adult females, density scale
#| fig-alt: | 
#|   To be written
#|   
#| fig-asp: 0.4

NHANES |> select(Age, Gender, Height) |>
  drop_na() |> 
  filter(Age > 17, Gender == "female") |>
  ggplot(aes(x = Height)) +
  geom_histogram(aes(y = after_stat(density)),
                 binwidth = 2.0) +
  geom_density(aes(y = after_stat(density)), color = "red") +
  stat_function(
    fun = function(Height) dnorm(Height, mean = mean(Height) + 2, sd = sd(Height) * .5),
    color = "red", linewidth = 1 ) +
  labs(x = "Adult female height (cm)", y = "Density") +
  scale_x_continuous(
    breaks = seq(120, 220, 10)
  )

library(ggplot2)

