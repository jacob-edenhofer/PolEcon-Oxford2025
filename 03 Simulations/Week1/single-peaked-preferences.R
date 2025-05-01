# Load libraries
library(ggplot2)
library(here)
library(gridExtra)

# Set up figure directory
# figures <- here::here("03 Simulations", "Week1", "Figures")
# if (!dir.exists(figures)) {
#   dir.create(figures)
# }

# Set up policy space
policy_space <- seq(0, 10, length.out = 500)

# Single-peaked utility
single_peaked_utility <- function(x, peak) {
  - (x - peak)^2
}

# Violating utility examples
double_peaked_utility <- function(x, peak1, peak2) {
  -pmin((x - peak1)^2, (x - peak2)^2)
}

flat_top_utility <- function(x, start, end) {
  -pmax(abs(x - (start + end)/2) - (end - start)/2, 0)^2
}

oscillating_utility <- function(x) {
  -cos(2 * pi * x / 5)^2
}

# Compute utilities
df_single <- data.frame(
  x = rep(policy_space, 3),
  utility = c(
    single_peaked_utility(policy_space, 3),
    single_peaked_utility(policy_space, 5),
    single_peaked_utility(policy_space, 7)
  ),
  voter = factor(rep(c("Voter 1 (Peak at 3)", "Voter 2 (Peak at 5)", "Voter 3 (Peak at 7)"), each = length(policy_space)))
)

df_violations <- data.frame(
  x = rep(policy_space, 3),
  utility = c(
    double_peaked_utility(policy_space, 3, 7),
    flat_top_utility(policy_space, 4, 6),
    oscillating_utility(policy_space)
  ),
  voter = factor(rep(c("Double-Peaked Voter", "Flat-Top Voter", "Oscillating Voter"), each = length(policy_space)))
)

# Plot single-peaked
p1 <- ggplot(df_single, aes(x = x, y = utility, color = voter)) +
  geom_line(size = 1) +
  labs(title = "Single-Peaked Preferences", x = "Policy Space", y = "Utility") +
  theme_bw() +
  theme(legend.title = element_blank())

# Plot violations
p2 <- ggplot(df_violations, aes(x = x, y = utility, color = voter)) +
  geom_line(size = 1) +
  labs(title = "Violations of Single-Peakedness", x = "Policy Space", y = "Utility") +
  theme_bw() +
  theme(legend.title = element_blank())

# Save side-by-side plot
# png(filename = here::here("Figures", "single_peakedness_illustration.png"), width = 1400, height = 600, res = 150)
grid.arrange(p1, p2, ncol = 2)
# dev.off()

# ---- Median Voter Theorem Plot ----

voters_ideal_points <- c(2, 4, 6, 7, 9)
median_voter <- median(voters_ideal_points)

df_median <- do.call(rbind, lapply(seq_along(voters_ideal_points), function(i) {
  data.frame(
    x = policy_space,
    utility = single_peaked_utility(policy_space, voters_ideal_points[i]),
    voter = paste("Voter", i, "(Peak at", voters_ideal_points[i], ")")
  )
}))

p_median <- ggplot(df_median, aes(x = x, y = utility, color = voter)) +
  geom_line(size = 1) +
  geom_vline(xintercept = median_voter, linetype = "dashed", color = "black") +
  annotate("text", x = median_voter + 0.3, y = -70, label = paste("Median Voter (Peak at", median_voter, ")"), hjust = 0) +
  labs(title = "Illustration of the Median Voter Theorem", x = "Policy Space", y = "Utility") +
  theme_bw() +
  theme(legend.title = element_blank())
print(p_median)

# ggsave(filename = here::here("Figures", "median_voter_theorem.png"), plot = p_median, width = 10, height = 6, dpi = 300)
