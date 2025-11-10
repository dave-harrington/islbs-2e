
### Draft code.  Final versions are in qmd files

# clear workspace

rm(list = ls())





# Danish ED

# useful example for relationships

df <- danish.ed.primary
summary(df$age)
hist(df$age^2)
table(df$sex, df$triage)
boxplot(df$age)



# gss data tables 
# 
# remotes::install_github("kjhealy/gssrdoc")
# remotes::install_github("kjhealy/gssr")

library(gssr)
library(gssrdoc)


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


#install.packages("fitdistrplus")
library(fitdistrplus)
library(ggplot2)

# Example: your data vector
# Replace with your actual numeric values
x <- adult_wt_ht$BMXWT

# Fit a Gamma distribution
fit_gamma <- fitdist(x, "gamma")

# View estimated parameters
fit_gamma
# shape and rate parameters are printed (Gamma(shape, rate))

# Plot diagnostic graphs
plot(fit_gamma)

library(ggplot2)


shape <- 14.8
rate  <- 0.18

x_max <- qgamma(0.999, shape = shape, rate = rate)
df <- data.frame(x = seq(0, x_max, length.out = 2000))
df$y <- dgamma(df$x, shape = shape, rate = rate)

p_band <- pgamma(60, shape = shape, rate = rate) -
  pgamma(50, shape = shape, rate = rate)

ggplot(df, aes(x = x, y = y)) +
  geom_line(linewidth = 1) +
  geom_ribbon(
    data = subset(df, x >= 50 & x <= 60),
    aes(ymin = 0, ymax = y),     # inherits x and y from ggplot()
    fill = "steelblue", alpha = 0.5
  ) +
  labs(
    title = "Gamma Density: shape = 14.8, rate = 0.18",
    subtitle = paste0("P(50 \u2264 X \u2264 60) = ", sprintf("%.4f", p_band)),
    x = "x", y = "Density"
  ) +
  theme_minimal(base_size = 14)


x <- seq(0, 22, 0.01)
y <- dchisq(x, 5)
M <- weighted.mean(x, y)

par(mar = c(1.65, 0, 0, 0), mgp = c(5, 0.5, 0))
plot(x, y + 0.035,
     type = 'l',
     ylim = range(c(0.025, y + 0.035)),
     axes = FALSE)
axis(1, at = c(-100, M, 100), labels = c('', expression(mu), ''))
lines(c(0, 22), rep(0.035, 2))
polygon(x, y + 0.035, col = COL[1])
polygon(c(M - 20, M + 20, M),
        c(-0.2, -0.2, 0.035),
        col = COL[4])

x <- seq(0, 22, 0.01)
y <- dchisq(x, 5)

# Mean (as you had it)
M <- weighted.mean(x, y)          # ~ df = 5

# Median for Chi-square(df = 5)
Med <- qchisq(0.5, df = 5)        # exact median

par(mar = c(1.65, 0, 0, 0), mgp = c(5, 0.5, 0))
plot(x, y + 0.035,
     type = 'l',
     ylim = range(c(0.025, y + 0.035)),
     axes = FALSE)
# Existing mean tick/label at mu
axis(1, at = c(-100, M, 100), labels = c('', expression(mu), ''))

# Add median tick/label (uses \tilde{x} for median)
axis(1, at = Med, labels = expression(tilde(x)))

# Baseline
lines(c(0, 22), rep(0.035, 2))

# Fill the density (as you had it)
polygon(x, y + 0.035, col = IMSCOL["blue", "full"])

# Triangle for the mean (your original)
polygon(c(M - 20, M + 20, M),
        c(-0.2, -0.2, 0.035),
        col = COL[4])

# Triangle for the median (choose a different color index if you like)

#arrows(x0 = Med, y0 = -0.15, y1 = 0.035,
#       length = 0.1, lwd = 2, col = COL[4])
#       
#       
ggplot(df, aes(x = x)) +
  geom_density(fill = "skyblue", alpha = 0.5, color = "darkblue") +
  labs(title = "Filled Density Curve", x = "x", y = "Density") +
  theme_minimal(base_size = 14)

# area between two points on geom density

p <- ggplot(adult_wt_ht, aes(BMXWT)) + geom_density(bw = 4.2)
pb <- ggplot_build(p)

dens_data <- pb$data[[1]]  # Contains x, y, and other columns

a <- 50; b <- 60
dx <- diff(dens_data$x)[1]
area_plot <- sum(dens_data$y[dens_data$x >= a & dens_data$x <= b]) * dx
area_plot

# shade area between two points

library(ggplot2)

mu_wt = mean(adult_wt_ht$BMXWT)
p <- ggplot(adult_wt_ht, aes(BMXWT)) +
        geom_density(bw = 4.8,
                     fill = IMSCOL["blue", "full"], alpha = 0.5)
pb <- ggplot_build(p)
curve <- pb$data[[1]][, c("x", "y")]  # density grid used by ggplot

a <- 50; b <- 60
dx <- diff(curve$x)[1]
area_ab <- sum(curve$y[curve$x >= a & curve$x <= b]) * dx

ggplot(curve, aes(x, y)) +
  geom_line(linewidth = 1.0) +
  geom_area(
    data = subset(curve, x >= a & x <= b),
    fill = IMSCOL["blue", "full"], alpha = 0.50
  ) +
  labs(
    x = "Weight (kg)", y = "Density"
  ) +
  theme_minimal(base_size = 12)


mu_wt = mean(adult_wt_ht$BMXWT)
p <- ggplot(adult_wt_ht, aes(BMXWT)) +
  geom_density(bw = 4.8,
               fill = IMSCOL["blue", "full"], alpha = 0.5)
tri_height <- max(adult_wt_ht$BMXWT) * 0.05
triangle_df <- data.frame(
  x = c(mu_wt - 0.2, mu_wt + 0.2, mu_wt),
  y = c(-0.2, 0.2 , 0)  # small height relative to the density
)

p + geom_polygon(data = triangle_df, aes(x, y),
                 fill = "red", color = "black")


library(ggplot2)

set.seed(123)
x <- rnorm(1000, mean = 10, sd = 2)
mean_x <- mean(x)

# Compute density for reference
dens <- density(x)
dens_df <- data.frame(x = dens$x, y = dens$y)

# Build base plot with filled density
p <- ggplot(dens_df, aes(x, y)) +
  geom_area(fill = "skyblue", alpha = 0.5) +
  geom_line(linewidth = 1.1, color = "black") +
  labs(
    title = "Density with Triangle Marker at the Mean",
    x = "x", y = "Density"
  ) +
  theme_minimal(base_size = 14)

# Add a small triangle (as a filled polygon)
# Here we define a triangle around the mean:
triangle_df <- data.frame(
  x = c(mean_x - 0.2, mean_x + 0.2, mean_x),
  y = c(0, 0, max(dens_df$y) * 0.05)  # small height relative to the density
)

p + geom_polygon(data = triangle_df, aes(x, y),
                 fill = "red", color = "black")

polygon(c(mu_wt - 20, mu_wt + 20, mu_wt),
        c(-0.2, -0.2, 0.035),
        col = COL[4])

library(ggplot2)

set.seed(123)
x <- rnorm(1000, mean = 10, sd = 2)
mean_x <- mean(x)

# Compute density for reference
dens <- density(x)
dens_df <- data.frame(x = dens$x, y = dens$y)

# Build base plot with filled density
p <- ggplot(dens_df, aes(x, y)) +
  geom_area(fill = "skyblue", alpha = 0.5) +
  geom_line(linewidth = 1.1, color = "black") +
  labs(
    title = "Density with Triangle Marker at the Mean",
    x = "x", y = "Density"
  ) +
  theme_minimal(base_size = 14)

# Add a small triangle (as a filled polygon)
# Here we define a triangle around the mean:
library(ggplot2)

set.seed(123)
x <- rnorm(1000, mean = 10, sd = 2)
mean_x <- mean(x)

# Compute density
dens <- density(x)
dens_df <- data.frame(x = dens$x, y = dens$y)

# Base density plot with shaded area
p <- ggplot(dens_df, aes(x, y)) +
  geom_area(fill = "skyblue", alpha = 0.5) +
  geom_line(linewidth = 1.1, color = "black") +
  labs(
    title = "Density with Downward Triangle at the Mean",
    x = "x", y = "Density"
  ) +
  theme_minimal(base_size = 14)

# Define a small triangle *below* the x-axis
triangle_df <- data.frame(
  x = c(mean_x - 0.2, mean_x + 0.2, mean_x),
  y = c(0, 0, -max(dens_df$y) * 0.05)   # below the axis, pointing downward
)

# Add the triangle polygon
p + geom_polygon(
  data = triangle_df, aes(x, y),
  fill = "red", color = "black"
)
triangle_df <- data.frame(
  x = c(mean_x - 0.2, mean_x + 0.2, mean_x),
  y = c(-0.2, - 0.2, 0.2)  # small height relative to the density
)

p + geom_polygon(data = triangle_df, aes(x, y),
                 fill = "red", color = "black")

library(ggplot2)

set.seed(123)
x <- rnorm(1000, mean = 10, sd = 2)
mean_x <- mean(x)

# Compute density
dens <- density(x)
dens_df <- data.frame(x = dens$x, y = dens$y)

# Base density plot with shaded area
p <- ggplot(dens_df, aes(x, y)) +
  geom_area(fill = "skyblue", alpha = 0.5) +
  geom_line(linewidth = 1.1, color = "black") +
  labs(
    title = "Density with Downward Triangle at the Mean",
    x = "x", y = "Density"
  ) 

# Define a small triangle *below* the x-axis
triangle_df <- data.frame(
  x = c(mean_x - 0.2, mean_x + 0.2, mean_x),
  y = c(0, 0, -max(dens_df$y) * 0.05)   # below the axis, pointing downward
)

# Add the triangle polygon
p + geom_polygon(
  data = triangle_df, aes(x, y),
  fill = "red", color = "black"
)


library(ggplot2)

# data & stats
x <- seq(0, 22, 0.01)
y <- dchisq(x, 5)
M <- weighted.mean(x, y)          # mean of the density grid (≈ df for chisq)

df <- data.frame(x = x, y = y, y_off = y + 0.035)

# optional color fallbacks if custom palettes aren't defined
fill_col <- if (exists("IMSCOL")) IMSCOL["blue", "full"] else "steelblue"
tri_col  <- if (exists("COL")) COL[4] else "tomato"

# triangle (upward) with tip at the baseline y = 0.035
tri_df <- data.frame(
  x = c(M - 20, M + 20, M),
  y = c(-0.20, -0.20, 0.035)   # base well below, tip touches baseline
)

ggplot(df, aes(x, y_off)) +
  # filled area under the offset curve
  geom_area(fill = fill_col, alpha = 1) +
  # curve outline (optional, remove if you want area-only)
  geom_line(linewidth = 0.8, color = "black") +
  # baseline at y = 0.035 (matches your base-R 'lines(c(0,22), rep(0.035,2))')
  geom_segment(aes(x = 0, xend = 22, y = 0.035, yend = 0.035)) +
  # upward triangle pointing to the mean (tip at baseline)
  geom_polygon(data = tri_df, aes(x, y), inherit.aes = FALSE,
               fill = tri_col, color = NA) +
  # x scale: show only one tick at the mean, labeled μ
  scale_x_continuous(limits = c(0, 22),
                     breaks = M,
                     labels = c(expression(mu))) +
  # y limits so the triangle base is visible
  scale_y_continuous(limits = c(-0.20, max(df$y_off))) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 13) +
  theme(
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank()
  )

# ---- Best-fitting chi-square and ggplot overlay ----
library(ggplot2)

# your data vector (nonnegative)
# x <- your_data_vector
x <- adult_wt_ht$BMXWT      # keep valid values

# 1) MLE for df via 1-D optimization of the log-likelihood
ll <- function(df) sum(dchisq(x, df = df, log = TRUE))
# method-of-moments for a sensible starting range
mm_df <- 2 * mean(x)^2 / var(x)

low  <- max(1e-6, mm_df / 10)
high <- max(low * 2, mm_df * 10)   # ensure high > low
opt  <- optimize(function(d) -ll(d), interval = c(low, high))
df_hat <- opt$minimum

# 2) Plot: histogram (density-scaled) + fitted chi-square density
ggplot(data.frame(x), aes(x)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 50, fill = "grey85", color = "grey40") +
  stat_function(fun = dchisq,
                args = list(df = df_hat),
                color = "red", linewidth = 1.2) +
  labs(
    title = sprintf("Fitted Chi-square Density (df ≈ %.2f)", df_hat),
    x = "x",
    y = "Density"
  ) +
  theme_minimal(base_size = 13)
