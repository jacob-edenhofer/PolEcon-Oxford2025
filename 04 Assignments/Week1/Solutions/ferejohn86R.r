# Load required library
library(tidyverse)
library(stats)

# Define the function to compute optimal effort
optimal_effort <- function(k_t, sigma = 0.5, R = 1.0) {
  # Define the objective function (to minimise)
  objective <- function(e) {
    prob <- 1 - pnorm((k_t - e) / sigma)
    return(-R * prob + 0.5 * e^2)
  }
  
  # Optimise over the range [0, 5]
  result <- optimize(objective, interval = c(0, 5))
  return(result$minimum)
}

# Example: compute optimal effort when k_t = 1.0
e_star <- optimal_effort(k_t = 1.0)
print(e_star)

# --- Parameters ---
T <- 20             # number of periods
k0 <- 1.0           # initial cutoff
alpha <- 0.02       # cutoff decline rate
sigma <- 0.5        # standard deviation of noise
R <- 1              # reward from reelection

# --- Storage vectors ---
cutoffs <- numeric(T)
efforts <- numeric(T)
reelect_probs <- numeric(T)

# --- Simulation loop ---
for (t in 1:T) {
  k_t <- k0 - alpha * (t - 1)
  cutoffs[t] <- k_t
  
  # Define payoff function
  incumbent_payoff <- function(e) {
    prob <- 1 - pnorm((k_t - e) / sigma)
    cost <- 0.5 * e^2
    return(-(R * prob - cost))
  }
  
  opt <- optimize(incumbent_payoff, interval = c(0, 5))
  e_star <- opt$minimum
  efforts[t] <- e_star
  reelect_probs[t] <- 1 - pnorm((k_t - e_star) / sigma)
}

# --- Combine into long-format dataframe for cleaner plotting ---
# --- Create tidy data frame ---
df <- data.frame(
  Time = 1:T,
  Cutoff = cutoffs,
  Effort = efforts,
  Reelection_Prob = reelect_probs
) %>%
  rename(`Reelection Probability` = Reelection_Prob) %>%
  pivot_longer(cols = c(Cutoff, Effort, `Reelection Probability`), names_to = "Variable", values_to = "Value")

# --- Plot ---
ggplot(df, aes(x = Time, y = Value, colour = Variable)) +
  geom_line(size = 1.2) +
  geom_point(size = 2, shape = 21, fill = "white", stroke = 0.8) +
  scale_colour_manual(values = c("Cutoff" = "#1b9e77", "Effort" = "#d95f02", "Reelection Probability" = "#7570b3")) +
  labs(
    title = "Effort, Cutoffs, and Reelection Probability Over Time",
    subtitle = "Ferejohn-style dynamic accountability model with declining voter standards",
    x = "Period",
    y = "Value",
    colour = "Metric"
  ) +
  expand_limits(y = c(0, 1)) +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, colour = "grey40"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )
