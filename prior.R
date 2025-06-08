#Selecting a prior



# Visualization of prior --------------------------------------------------


# Create data for the prior distribution
x_values <- seq(-0.05, 0.17, by = 0.001)
prior_density <- dnorm(x_values, mean = 0.06, sd = 0.03)

# Combine into a data frame
prior_df <- data.frame(
  effect_size = x_values,
  density = prior_density
)

# Calculate points for specific probability regions
calculate_probability <- function(x, mean, sd, lower, upper) {
  pnorm(upper, mean, sd) - pnorm(lower, mean, sd)
}

# Create regions for different effect sizes
regions <- data.frame(
  label = c("Negative effect", "Negligible effect", "Moderate effect", "Strong effect"),
  lower = c(-Inf, 0, 0.03, 0.09),
  upper = c(0, 0.03, 0.09, Inf),
  probability = c(
    pnorm(0, 0.06, 0.03),
    pnorm(0.03, 0.06, 0.03) - pnorm(0, 0.06, 0.03),
    pnorm(0.09, 0.06, 0.03) - pnorm(0.03, 0.06, 0.03),
    1 - pnorm(0.09, 0.06, 0.03)
  )
)

# Create the visualization
ggplot() +
  # Plot the density curve
  geom_line(data = prior_df, aes(x = effect_size, y = density), linewidth = 1) +

  # Fill different regions with colors
  geom_area(data = filter(prior_df, effect_size < 0),
            aes(x = effect_size, y = density), fill = "#F8766D", alpha = 0.5) +
  geom_area(data = filter(prior_df, effect_size >= 0 & effect_size < 0.03),
            aes(x = effect_size, y = density), fill = "#00BA38", alpha = 0.3) +
  geom_area(data = filter(prior_df, effect_size >= 0.03 & effect_size < 0.09),
            aes(x = effect_size, y = density), fill = "#00BA38", alpha = 0.5) +
  geom_area(data = filter(prior_df, effect_size >= 0.09),
            aes(x = effect_size, y = density), fill = "#00BA38", alpha = 0.7) +

  # Add region labels with probabilities
  annotate("text", x = -0.02, y = 2, label = paste0("Negative\n(", round(regions$probability[1]*100), "%)"), size = 3) +
  annotate("text", x = 0.015, y = 4, label = paste0("Negligible\n(", round(regions$probability[2]*100), "%)"), size = 3) +
  annotate("text", x = 0.06, y = 8, label = paste0("Moderate\n(", round(regions$probability[3]*100), "%)"), size = 3) +
  annotate("text", x = 0.11, y = 4, label = paste0("Strong\n(", round(regions$probability[4]*100), "%)"), size = 3) +

  # Add educational milestone markers with odds ratios
#  geom_vline(xintercept = 0.06*12, linetype = "dashed", color = "darkblue", alpha = 0.5) +
#  geom_vline(xintercept = 0.06*16, linetype = "dashed", color = "darkblue", alpha = 0.5) +
#  annotate("text", x = 0.06*12 + 0.01, y = 11, label = "Secondary education\n(12 years, OR=2.0)", size = 3, hjust = 0) +
#  annotate("text", x = 0.06*16 + 0.01, y = 13, label = "University education\n(16 years, OR=2.6)", size = 3, hjust = 0) +

  # Add title and labels
  labs(
   # title = "Prior Distribution for Education Effect on Migration Aspirations",
  #  subtitle = "N(0.06, 0.03): 95% credible interval [0.00, 0.12]",
    x = "Log-odds effect per year of education",
    y = "Density",
    caption = "Note: Values represent change in log-odds of migration aspirations per additional year of education.\nPositive values indicate increased migration aspirations with more education."
  ) +

  # Secondary axis to show odds ratio equivalents
  scale_x_continuous(
    sec.axis = sec_axis(~ exp(.), name = "Odds ratio per year of education",
                        breaks = exp(seq(0, 0.15, by = 0.03)),
                        labels = round(exp(seq(0, 0.15, by = 0.03)),2))
  ) +

  # Theme adjustments
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "darkgrey")
  )

# Calculation of prior ----------------------------------------------------







# Define prior parameters
prior_mean <- 0.06
prior_sd <- 0.03

# Calculate 95% credible interval for the prior
prior_ci_lower <- prior_mean - 1.96 * prior_sd
prior_ci_upper <- prior_mean + 1.96 * prior_sd

# Function to convert log-odds to odds ratio
log_odds_to_odds <- function(log_odds) {
  return(exp(log_odds))
}

# Function to convert log-odds to probability
log_odds_to_prob <- function(log_odds) {
  odds <- exp(log_odds)
  return(odds / (1 + odds))
}

# Calculate effect at specific education levels
education_years <- c(0, 6, 12, 16, 21)
education_labels <- c("No formal education", "Primary education",
                      "Secondary education", "University degree", "Doctorate")

# Create results data frame
results <- data.frame(
  Education_Level = education_labels,
  Years = education_years,
  Log_Odds_Coefficient = education_years * prior_mean,
  Odds_Ratio = log_odds_to_odds(education_years * prior_mean),
  stringsAsFactors = FALSE
)

# Calculate incremental and cumulative effects
results$Incremental_Odds_Increase <- log_odds_to_odds(prior_mean)
results$Cumulative_Odds_vs_Baseline <- results$Odds_Ratio / results$Odds_Ratio[1]

# Calculate probability of migration aspiration
# Assuming a baseline probability of 0.3 for someone with no education
baseline_logodds <- log(0.3/(1-0.3))
results$Probability <- log_odds_to_prob(baseline_logodds + results$Log_Odds_Coefficient)

# Print prior information
cat("Prior Distribution: Normal(", prior_mean, ", ", prior_sd, ")\n", sep="")
cat("95% Credible Interval: [", round(prior_ci_lower, 2), ", ", round(prior_ci_upper, 2), "]\n", sep="")
cat("Per-year odds ratio: ", round(log_odds_to_odds(prior_mean), 3), "\n\n", sep="")

# Print results table
print(results[, c("Education_Level", "Years", "Odds_Ratio", "Cumulative_Odds_vs_Baseline", "Probability")])

# Calculate probability difference from 10th to 90th percentile of prior
effect_10th <- qnorm(0.1, prior_mean, prior_sd)
effect_90th <- qnorm(0.9, prior_mean, prior_sd)

cat("\nRange of effects from 10th to 90th percentile of prior:\n")
cat("10th percentile effect (per year): ", round(effect_10th, 3),
    " (OR = ", round(log_odds_to_odds(effect_10th), 3), ")\n", sep="")
cat("90th percentile effect (per year): ", round(effect_90th, 3),
    " (OR = ", round(log_odds_to_odds(effect_90th), 3), ")\n", sep="")

# Verify values specifically mentioned in our text
secondary_odds_ratio <- log_odds_to_odds(12 * prior_mean)
university_odds_ratio <- log_odds_to_odds(16 * prior_mean)

cat("\nSpecific milestones mentioned in text:\n")
cat("Secondary education (12 years) odds ratio vs. no education: ",
    round(secondary_odds_ratio, 2), "\n", sep="")
cat("University degree (16 years) odds ratio vs. no education: ",
    round(university_odds_ratio, 2), "\n", sep="")
