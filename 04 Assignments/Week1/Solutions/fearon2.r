# Load necessary library
library(dplyr)

# Set seed for reproducibility
set.seed(2024)

# Parameters
N <- 1000
type_probs <- c(0.5, 0.5)  # 50% good, 50% bad
mus <- list(good = 1.0, bad = -1.0)
sigmas <- c(0.5, 1.0, 2.0)
cutoff <- 0

# Store results
results <- list()

for (sigma in sigmas) {
  # Assign types
  is_good <- runif(N) < type_probs[1]
  
  # Generate z based on type
  z <- ifelse(is_good,
              rnorm(N, mean = mus$good, sd = sigma),
              rnorm(N, mean = mus$bad, sd = sigma))
  
  # Apply decision rule
  reelected <- z > cutoff
  
  # Compute metrics
  reelection_rate <- mean(reelected)
  true_positive_rate <- sum(is_good & reelected) / sum(is_good)
  false_positive_rate <- sum(!is_good & reelected) / sum(!is_good)
  
  # Store results
  results[[length(results) + 1]] <- data.frame(
    sigma = sigma,
    reelection_rate = reelection_rate,
    true_positives = true_positive_rate,
    false_positives = false_positive_rate
  )
}

# Combine results into a data frame
df_voter_noise <- bind_rows(results)
print(df_voter_noise)
