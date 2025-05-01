# Load libraries
library(ggplot2)
library(gridExtra)
library(dplyr)

# Set up figure directory
# figures <- here::here("03 Simulations", "Week1", "Figures")
# if (!dir.exists(figures)) {
#   dir.create(figures)
# }

# Set seed to ensure reproducibility
set.seed(1234)

# -------- Panel 1: Good CDF (Normal noise) --------
n <- 1000
e_good <- seq(0, 180, length.out = n)
epsilon_good <- rnorm(n)
o_good <- e_good + epsilon_good

# -------- Panel 2: Bad CDF (Gapped noise) --------
e_bad <- rep(90, n)
epsilon_bad <- c(runif(n/2, -5, -2), runif(n/2, 2, 5))
epsilon_bad <- sample(epsilon_bad)
o_bad <- e_bad + epsilon_bad

# -------- Plot both CDFs --------
df_good <- data.frame(o = o_good, type = "Good")
df_bad <- data.frame(o = o_bad, type = "Bad")
df_all <- rbind(df_good, df_bad)

p1 <- ggplot(df_good, aes(x = o)) +
  stat_ecdf(geom = "step", pad = FALSE, color = "blue") +
  labs(title = "Good CDF (Normal Noise)", x = "Outcome (o = e + ε)", y = "Cumulative Probability") +
  xlim(-30, 200) +
  ylim(0, 1) +
  theme_bw()

p2 <- ggplot(df_bad, aes(x = o)) +
  stat_ecdf(geom = "step", pad = FALSE, color = "orange") +
  labs(title = "Bad CDF (Gapped Noise)", x = "Outcome (o = e + ε)", y = "Cumulative Probability") +
  xlim(80, 100) +
  ylim(0, 1) +
  theme_bw()

# png(filename = file.path(figures, "CDF_Comparison.png"), width = 1600, height = 600, res = 150)
grid.arrange(p1, p2, ncol = 2)
# dev.off()

# -------- Tradeoff Cutoff Plot --------
n <- 300
o_star <- seq(0, 10, length.out = n)
lambda_e <- 0.8
e <- lambda_e * o_star
marginal_cost_good <- e
marginal_cost_bad <- 1.5 * e

# Marginal benefit (normal PDF)
R <- 5
marginal_benefit <- R * dnorm(o_star - e)

df_tradeoff <- data.frame(
  o_star = o_star,
  marginal_benefit = marginal_benefit,
  cost_good = marginal_cost_good,
  cost_bad = marginal_cost_bad
)

df_tradeoff <- df_tradeoff %>%
  mutate(region = case_when(
    marginal_benefit >= cost_bad ~ "Both types exert effort",
    marginal_benefit >= cost_good ~ "Only good type exerts effort",
    TRUE ~ "No effort exerted"
  ))

p <- ggplot(df_tradeoff, aes(x = o_star)) +
  geom_ribbon(aes(ymin = 0, ymax = Inf, fill = region), alpha = 0.4) +
  geom_line(aes(y = marginal_benefit, color = "Expected Marginal Benefit"), linetype = "dashed", size = 1) +
  geom_line(aes(y = cost_good, color = "Good Type: Marginal Cost"), size = 1) +
  geom_line(aes(y = cost_bad, color = "Bad Type: Marginal Cost"), size = 1) +
  labs(title = "Impact of Cutoff o* on Incumbent Behaviour",
       x = "Cutoff Level o*", y = "Marginal Values") +
  scale_fill_manual(values = c(
    "Both types exert effort" = "lightgreen",
    "Only good type exerts effort" = "lightcoral",
    "No effort exerted" = "lightgray"
  )) +
  scale_color_manual(values = c(
    "Expected Marginal Benefit" = "blue",
    "Good Type: Marginal Cost" = "green",
    "Bad Type: Marginal Cost" = "red"
  )) +
  theme_bw() +
  guides(fill = guide_legend(title = "Effort Region"),
         color = guide_legend(title = "Line"))
print(p)

# ggsave(filename = file.path(figures, "Tradeoff_Cutoff.png"), plot = p, width = 10, height = 6, dpi = 300)
