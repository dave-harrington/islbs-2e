
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

library(ggplot2)
library(ggforce)

# Circles for A and B
df <- data.frame(
  x = c(0, 1),   # centers
  y = c(0, 0),
  r = c(1, 1),
  label = c("A", "B")
)

# Base plot with circles
ggplot(df) +
  geom_circle(aes(x0 = x, y0 = y, r = r, fill = label),
              alpha = 0.3, color = "black", size = 1) +
  # Intersection shading (A ∩ B)
  annotate("rect", xmin = 0, xmax = 0.5, ymin = -1, ymax = 1,
           fill = "steelblue", alpha = 0.2) +
  coord_fixed() +
  xlim(-1.5, 2.5) + ylim(-1.5, 1.5) +
  theme_void() +
  labs(
    title = "Rule of Total Probability",
    subtitle = "P(A) = P(A and B) + P(A and B^c)"
  ) +
  annotate("text", x = -0.5, y = 0.8, label = "A and Bᶜ", size = 5) +
  annotate("text", x = 0.7, y = 0.8, label = "A and B", size = 5)

# install.packages("ggforce")  # if needed
library(ggplot2)
library(ggforce)
library(grid)   # for unit() in the arrow

# -------- canvas & partition --------
xlim <- c(0, 10); ylim <- c(0, 6)
cuts <- c(3, 7)           # vertical boundaries for E1 | E2 | E3

# diagonal hatch for the sample space Ω
hatch <- {
  step <- 0.35
  t <- seq(-ylim[2], xlim[2], by = step)
  data.frame(x    = t,
             xend = t + ylim[2],
             y    = 0,
             yend = ylim[2])
}

# ellipse (event B) parameters
B <- data.frame(x0 = 5.8, y0 = 3.0, a = 3.2, b = 2.0, angle = 0)  # tweak if desired

ggplot() +
  # sample space border
  geom_rect(aes(xmin = xlim[1], xmax = xlim[2], ymin = ylim[1], ymax = ylim[2]),
            fill = NA, color = "black", linewidth = 1) +
  # hatch (light diagonal lines) clipped to panel
  geom_segment(data = hatch,
               aes(x = x, xend = xend, y = y, yend = yend),
               color = "#5DADE2", alpha = 0.25, linewidth = 0.6) +
  # ellipse for B
  geom_ellipse(data = B,
               aes(x0 = x0, y0 = y0, a = a, b = b, angle = angle),
               fill = "#2E86C1", alpha = 0.35, color = "#2E86C1", linewidth = 1) +
  # vertical partition lines (E1 | E2 | E3)
  geom_segment(aes(x = cuts[1], xend = cuts[1], y = ylim[1], yend = ylim[2]),
               color = "black", linewidth = 0.7) +
  geom_segment(aes(x = cuts[2], xend = cuts[2], y = ylim[1], yend = ylim[2]),
               color = "black", linewidth = 0.7) +
  # labels for intersections inside B
  annotate("text", x = 2.5, y = 3.1, label = expression(B %*cap% E[1]),
           size = 5.2, color = "black") +
  annotate("text", x = 5.6, y = 3.1, label = expression(B %*cap% E[2]),
           size = 5.2, color = "black") +
  annotate("text", x = 8.2, y = 3.1, label = expression(B %*cap% E[3]),
           size = 5.2, color = "black") +
  # labels for E1, E2, E3 at the bottom
  annotate("text", x = 1.0, y = 0.35, label = expression(E[1]), size = 6) +
  annotate("text", x = 5.0, y = 0.35, label = expression(E[2]), size = 6) +
  annotate("text", x = 9.0, y = 0.35, label = expression(E[3]), size = 6) +
  # arrow & label pointing to B
  annotate("segment", x = 9.2, y = 5.4, xend = 7.1, yend = 4.2,
           arrow = arrow(length = unit(0.25, "cm")), linewidth = 0.7) +
  annotate("text", x = 9.25, y = 5.55, label = "Event B", hjust = 0, size = 5) +
  coord_fixed(xlim = xlim, ylim = ylim, expand = FALSE) +
  theme_void()




# install.packages("ggforce")  # if needed
library(ggplot2)
library(ggforce)
library(grid)

# --- canvas & partition ---
xlim <- c(0, 10)
ylim <- c(0, 6)
cuts <- c(3, 7)

# diagonal hatch lines over the sample space
step <- 0.35
t <- seq(-ylim[2], xlim[2], by = step)
hatch <- data.frame(
  x    = t,
  xend = t + ylim[2],
  y    = 0,
  yend = ylim[2]
)

# ellipse (event B)
B <- data.frame(x0 = 5.8, y0 = 3.0, a = 3.2, b = 2.0, angle = 0)

ggplot() +
  # sample space border
  geom_rect(aes(xmin = xlim[1], xmax = xlim[2], ymin = ylim[1], ymax = ylim[2]),
            fill = NA, color = "black", linewidth = 1) +
  # hatch
  geom_segment(data = hatch,
               aes(x = x, xend = xend, y = y, yend = yend),
               color = "#5DADE2", alpha = 0.25, linewidth = 0.6) +
  # event B
  geom_ellipse(data = B,
               aes(x0 = x0, y0 = y0, a = a, b = b, angle = angle),
               fill = "#2E86C1", alpha = 0.35, color = "#2E86C1", linewidth = 1) +
  # partition lines
  geom_segment(aes(x = cuts[1], xend = cuts[1], y = ylim[1], yend = ylim[2]),
               color = "black", linewidth = 0.7) +
  geom_segment(aes(x = cuts[2], xend = cuts[2], y = ylim[1], yend = ylim[2]),
               color = "black", linewidth = 0.7) +
  # labels
  annotate("text", x = 2.5, y = 3.1, label = "B and E[1]", size = 5.2) +
  annotate("text", x = 5.6, y = 3.1, label = "B and E[2]", size = 5.2) +
  annotate("text", x = 8.2, y = 3.1, label = "B and E[3]", size = 5.2) +
  annotate("text", x = 1.0, y = 0.35, label = "E[1]", size = 6) +
  annotate("text", x = 5.0, y = 0.35, label = "E[2]", size = 6) +
  annotate("text", x = 9.0, y = 0.35, label = "E[3]", size = 6) +
  annotate("segment", x = 9.2, y = 5.4, xend = 7.1, yend = 4.2,
           arrow = arrow(length = unit(0.25, "cm")), linewidth = 0.7) +
  annotate("text", x = 9.25, y = 5.55, label = "Event B", hjust = 0, size = 5) +
  coord_fixed(xlim = xlim, ylim = ylim, expand = FALSE) +
  theme_void()
