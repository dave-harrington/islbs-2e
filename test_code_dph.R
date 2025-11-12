
### Draft code.  Final versions are in qmd files

# clear workspace

rm(list = ls())



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

