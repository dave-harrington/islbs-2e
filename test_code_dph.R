
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



# using NHANESA for heights
# 


# Negative Z table core (same structure, but no LaTeX markup)

z <- matrix(NA_real_, 39, 10)
for (i in 1:39) {
  for (j in 1:9) {
    z[i, j] <- -((39 - i) / 10 + (10 - j) / 100) + 0.01
  }
}

Z <- matrix(NA_character_, 39, 10)
for (i in 1:39) {
  for (j in 1:9) {
    # format the probability with 4 decimals, no LaTeX
    prob <- round(pnorm(z[i, j]), 4)
    Z[i, j] <- format(prob, nsmall = 4)
  }
  # last column: Z value itself, no $...$
  z_val   <- z[i, 9]
  Z[i, 10] <- format(z_val, nsmall = 1)
}

# row names = Z values along rows
tmp  <- c(round(pnorm(seq(-3.89, -0.09, 0.1)), 4), 0.0001)
hold <- as.character(format(tmp)[1:39])
rownames(Z) <- hold  # you might want format(..., nsmall = 4) depending on style

# column names = second decimal place of Z (you can choose order you like)
colnames(Z) <- format(seq(0.08, -0.01, -0.01))

library(knitr)

kable(
  Z[5:39, ],
  format = "pipe",          # GitHub-style Markdown table
  align = "r",              # right-align numbers
  col.names = c(colnames(Z)), # or customize header
  row.names = TRUE
)





