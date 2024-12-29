
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

par_og <- par(no.readonly = TRUE) # save original par
par(mar = rep(0, 4))
plot(c(-0.15, 1.3), 0:1, type = "n", axes = FALSE)

text(0.6, 0.9, "Variables")
rect(0.4, 0.8, 0.8, 1)

text(0.25, 0.5, "Numerical")
rect(0.1, 0.4, 0.4, 0.6)
arrows(0.45, 0.78, 0.34, 0.62, length = 0.08)

text(0.9, 0.5, "Categorical")
rect(0.73, 0.4, 1.07, 0.6)
arrows(0.76, 0.78, 0.85, 0.62, length = 0.08)

text(0, 0.1, "Discrete")
rect(-0.17, 0, 0.17, 0.2)
arrows(0.13, 0.38, 0.05, 0.22, length = 0.08)

text(0.39, 0.1, "Continuous")
rect(0.25, 0, 0.53, 0.2)
arrows(0.35, 0.38, 0.4, 0.22, length = 0.08)

text(0.77, 0.105, "Ordinal")
rect(0.64, 0, 0.9, 0.2)
arrows(0.82, 0.38, 0.77, 0.22, length = 0.08)

text(1.12, 0.1, "Nominal")
rect(0.99, 0, 1.25, 0.2)
arrows(1.02, 0.38, 1.1, 0.22, length = 0.08)
par(par_og) # restore original par


# dds
# 



library(patchwork)
p1 <- ggplot(dds.discr, 
       aes(x = expenditures)) +
  geom_boxplot(outlier.size = 2.5) +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  labs(x = "Expenditures (USD)") +
  scale_x_continuous(
    limits = c(0, 80000)
  )
p1




p2 <- ggplot(dds.discr,
       aes(x = expenditures)) +
  labs(x = "Expenditures (USD)", 
       y = "Count") +
  geom_histogram(binwidth = 5000, 
                 closed = "left") +
  scale_x_continuous(
   limits = c(0, 80000)
  )

p2

aligned_plot <- p1/p2
print(aligned_plot)



g1 <- ggplotGrob(p1)
g2 <- ggplotGrob(p2)
g <- rbind(g1, g2, size = "max")

# g$widths <- unit.pmax(g1$widths, g2$widths)
# grid.newpage()
grid.draw(g)
