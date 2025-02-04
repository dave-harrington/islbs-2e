
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


load("./data/WVS_Cross-National_Wave_7_Rdata_v6_0.rdata")



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



library(cowplot)
correlation_r_plot <- function(x_norm, z_norm, rho) {
  y <- rho * x_norm + sqrt(1 - rho^2) * z_norm # Cholesky decomposition
  correlation <- cor(x_norm, y)
  data = cbind(x_norm, y)
  p <- ggplot(data, aes(x = x_norm, y = y)) +
    geom_point() +
    labs(x = NULL,
         y = NULL) +
    theme_void() +
    theme(
      panel.border = element_rect(colour = "gray", fill = NA, linewidth = 1),
      strip.background = element_blank(),
      strip.text.x = element_blank(),
      plot.margin=unit(c(0.2, 0.2, 0.2, 0.2),"cm")
    ) 
  corr_list <- list("r" = correlation, "plot" = p)
  return(corr_list)
}
set.seed(080546)
n <- 100  
rho <- 0.33  
x <- rnorm(n)
z <- rnorm(n)


corr_list_1 <- correlation_r_plot(x, z, 0.33)
corr_list_2 <- correlation_r_plot(x, z, -0.40)
corr_list_3 <- correlation_r_plot(x, z, 0.80)
corr_list_4 <- correlation_r_plot(x, z, 0.05)


r1 <- corr_list_1$r
p1 <- corr_list_1$plot 

r2 <- corr_list_2$r
p2 <- corr_list_2$plot 

r3 <- corr_list_3$r
p3 <- corr_list_3$plot 

r4 <- corr_list_4$r
p4 <- corr_list_4$plot 
  


plot_grid(p1, p2, p3, p4, nrow = 1, 
          labels = c("1", "2", "3", "4"),
          label_colour = "blue",
          label_fontface = "plain",
          hjust = -2,
          vjust = 2)



p_all <- ggplot(data = hyperuricemia.samp, aes(x = uric.acid)) +
  geom_histogram(binwidth = 40)

p_males <- hyperuricemia.samp |> 
  filter(sex == "male") |> 
  ggplot(aes(x = uric.acid)) +
  geom_histogram(binwidth = 40)

p_males

p_females <- hyperuricemia.samp |> 
  filter(sex == "female") |> 
  ggplot(aes(x = uric.acid)) +
  geom_histogram(binwidth = 40)

p_females
