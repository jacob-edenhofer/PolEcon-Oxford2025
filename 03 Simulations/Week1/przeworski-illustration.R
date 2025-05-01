# Load required libraries
library(ggplot2)
library(here)

# Set up figure directory
# figures <- here::here("03 Simulations", "Week1", "Figures")
# if (!dir.exists(figures)) {
#   dir.create(figures)
# }

# Parameters
delta <- 0.95   # Discount factor
p <- 0.3        # Probability of violent success
c <- 2          # One-time cost of fighting
k <- 1          # Cost of political engagement
L <- 2          # Payoff from being ruled

# Stakes and probability function
stakes <- seq(0.01, 20, length.out = 500)
a <- 0.9
b <- 0.02
mu <- 5
pi <- a * exp(-b * (stakes - mu)^2)

# Compute outcomes
R <- stakes + L
V_comply <- (pi * stakes + L - k) / (1 - delta)
V_fight <- (p * R) / (1 - delta) - c
compliance_gap <- V_comply - V_fight

df <- data.frame(stakes = stakes, gap = compliance_gap)

# Identify zero crossings (approximate)
zero_stakes <- df$stakes[abs(df$gap) < 1e-5]

# Plot
p <- ggplot(df, aes(x = stakes, y = gap)) +
  geom_line(size = 1.2, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_ribbon(data = subset(df, gap > 0), aes(ymin = 0, ymax = gap), fill = "lightblue", alpha = 0.5) +
  geom_ribbon(data = subset(df, gap < 0), aes(ymin = gap, ymax = 0), fill = "salmon", alpha = 0.5) +
  labs(title = "Effect of Stakes on Compliance Incentives",
       x = "Stakes of Election (R - L)",
       y = "Compliance Incentive") +
  theme_bw()

print(p)

# Save plot
# ggsave(filename = here::here("Figures", "compliance_incentives.png"), plot = p, width = 9, height = 6, dpi = 300)
