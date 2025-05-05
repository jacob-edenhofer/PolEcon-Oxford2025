# --- Parameters ---
n_politicians <- 1000
sigma <- 1
x_good <- 0
x_bad <- 0.5
cutoff_k <- -0.5

# --- Choose case: low or high share of bad types ---
share_bad <- 0.3  # Change to 0.7 for the second case

# --- Step 1: Generate types ---
set.seed(123)  # for reproducibility
types <- sample(c("good", "bad"), size = n_politicians, replace = TRUE, prob = c(1 - share_bad, share_bad))

# --- Step 2: Assign effort (x) based on type ---
x_vals <- ifelse(types == "good", x_good, x_bad)

# --- Step 3: Simulate signal z = -x^2 + ε ---
eps <- rnorm(n_politicians, mean = 0, sd = sigma)
z_vals <- -x_vals^2 + eps

# --- Step 4: Apply reelection rule ---
reelected <- z_vals > cutoff_k
n_reelected <- sum(reelected)

# --- Step 5: Analyse reelected politicians ---
reelected_types <- types[reelected]
reelected_x <- x_vals[reelected]

# --- Compute statistics ---
total_reelected <- n_reelected / n_politicians
bad_among_reelected <- mean(reelected_types == "bad")
voter_utility <- -mean(reelected_x^2)

# --- Output results ---
cat("=== Case:", share_bad * 100, "% bad types, cutoff =", cutoff_k, "===\n")
cat("Reelection rate:", round(total_reelected, 2), "\n")
cat("Share of bad types among reelected:", round(bad_among_reelected, 2), "\n")
cat("Voter welfare (U = -E[x^2]):", round(voter_utility, 3), "\n")
