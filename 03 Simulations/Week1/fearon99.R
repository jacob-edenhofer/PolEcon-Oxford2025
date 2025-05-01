# Load required libraries
library(ggplot2)
library(gridExtra)

# Set up directory to save figures
# figures <- here::here("03 Simulations", "Week1", "Figures")
# if (!dir.exists(figures)) {
#   dir.create(figures)
# }

# Define x and constants
x <- seq(-2, 2, length.out = 400)
k <- 0.5

# Define R(x) and its derivative
R_x <- 1 - (k + x^2) / (1 + k)
R_x <- pmax(0, pmin(R_x, 1))
R_prime_x <- -2 * x / (1 + k)

# Create data frame
df <- data.frame(x = x, R_x = R_x, R_prime_x = R_prime_x)

# Plot 1: R(x)
p1 <- ggplot(df, aes(x = x, y = R_x)) +
  geom_line(size = 1.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  labs(title = "Reelection Probability", x = "Policy Choice x", y = "Reelection Probability R(x)") +
  theme_bw()

# Plot 2: R'(x)
p2 <- ggplot(df, aes(x = x, y = R_prime_x)) +
  geom_line(color = "orange", size = 1.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_area(data = subset(df, x < 0), aes(y = R_prime_x), fill = "green", alpha = 0.3) +
  geom_area(data = subset(df, x > 0), aes(y = R_prime_x), fill = "red", alpha = 0.3) +
  annotate("text", x = -1.5, y = 0.6, label = "Improves Reelection Chance") +
  annotate("text", x = 1.2, y = -0.6, label = "Hurts Reelection Chance") +
  labs(title = "Marginal Effect on Reelection", x = "Policy Choice x", y = "Marginal Effect R'(x)") +
  theme_bw()

# Arrange side-by-side and save
# png(filename = file.path(figures, "reelection_probability.png"), width = 1200, height = 500)
grid.arrange(p1, p2, ncol = 2)
# dev.off()
