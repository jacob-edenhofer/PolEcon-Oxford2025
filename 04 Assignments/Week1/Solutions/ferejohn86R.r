# Load required library
library(ggplot2)

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

  # Define payoff function to minimize (negative of utility)
  incumbent_payoff <- function(e) {
    prob <- 1 - pnorm((k_t - e) / sigma)
    cost <- 0.5 * e^2
    return(-(R * prob - cost))
  }

  # Find optimal effort
  opt <- optimize(incumbent_payoff, interval = c(0, 5))
  e_star <- opt$minimum
  efforts[t] <- e_star

  # Compute reelection probability
  reelect_probs[t] <- 1 - pnorm((k_t - e_star) / sigma)
}

# --- Create data frame for plotting ---
df <- data.frame(
  Time = 1:T,
  Cutoff = cutoffs,
  Effort = efforts,
  Reelection_Prob = reelect_probs
)

# --- Plot ---
ggplot(df, aes(x = Time)) +
  geom_line(aes(y = Cutoff, color = "Cutoff")) +
  geom_line(aes(y = Effort, color = "Effort")) +
  geom_line(aes(y = Reelection_Prob, color = "Reelection Probability")) +
  labs(
    title = "Dynamic Cutoffs and Optimal Effort in Ferejohn Model",
    y = "Value",
    color = "Variable"
  ) +
  theme_minimal()
