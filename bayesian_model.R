




# Prepare formula - same as original frequentist model
bayes_formula <- europe_aspiration ~ age_centered + marital_status + children +
  family_migration + gender + education + human_rights_europe +
  human_rights_country + migrant_conditions_europe + perception_jobs_country +
  perception_jobs_europe + wealth_index + ra




# Set Priors----------------------------------------------------------------

# Weakly Informative Priors for Bayesian Migration Aspirations Model
# Based on measurement characteristics and variable descriptions

# 1. Intercept and Overall Prior Structure
# For the neutral model, we'll use weakly informative priors that provide minimal
# regularization while maintaining computational stability

# Neutral prior specification
priors_neutral <- c(
  # Intercept: Normal(0, 1.5) provides moderate regularization for the baseline log-odds
  # A standard deviation of 1.5 on the log-odds scale allows for reasonable variation
  # in baseline probability while preventing extreme values
  prior(normal(0, 1.5), class = "Intercept"),

  # Research Area (Binary): Dinar vs Emirdag (reference)
  # Modest regularization for this key predictor of interest
  prior(normal(0, 1), class = "b", coef = "raDinar"),

  # Education: Neutral prior centered at 0 with wide variance
  # This contrasts with our informed positive prior in the other model
  prior(normal(0, 1), class = "b", coef = "education"),

  # Binary predictors: Use appropriate scale for log-odds
  # Marital status, children, family migration, gender
  prior(normal(0, 1), class = "b"),


  # Continuous predictors measured on 0-4 scales:
  # Human rights perceptions, job opportunities, migrant conditions
  # Scale priors to match the measurement scale (narrower SDs)
  prior(normal(0, 0.5), class = "b", coef = "human_rights_europe"),
  prior(normal(0, 0.5), class = "b", coef = "human_rights_country"),
  prior(normal(0, 0.5), class = "b", coef = "perception_jobs_europe"),
  prior(normal(0, 0.5), class = "b", coef = "perception_jobs_country"),
  prior(normal(0, 0.5), class = "b", coef = "migrant_conditions_europe"),
  prior(normal(0, 0.5), class = "b", coef = "wealth_index"),

  # Age (mean-centered): A small effect expected per year difference
  prior(normal(0, 0.1), class = "b", coef = "age_centered")
)

# 2. Positive prior specification - focused on education
# This follows your theoretical justification in the paper
priors_positive <- c(
  # Same general structure as neutral priors
  prior(normal(0, 1.5), class = "Intercept"),
  prior(normal(0.3, 0.5), class = "b", coef = "raDinar"),

  # Theoretically informed positive prior for education
  # N(0.06, 0.03) reflects expectation that higher education increases migration aspirations
  # Based on prior research as described in your paper
  prior(normal(0.06, 0.03), class = "b", coef = "education"),

  # Binary predictors
  prior(normal(0, 1), class = "b"),


  # Continuous predictors on 0-4 scales
  prior(normal(0, 0.5), class = "b", coef = "human_rights_europe"),
  prior(normal(0, 0.5), class = "b", coef = "human_rights_country"),
  prior(normal(0, 0.5), class = "b", coef = "perception_jobs_europe"),
  prior(normal(0, 0.5), class = "b", coef = "perception_jobs_country"),
  prior(normal(0, 0.5), class = "b", coef = "migrant_conditions_europe"),
  prior(normal(0, 0.5), class = "b", coef = "wealth_index"),

  # Age (mean-centered)
  prior(normal(0, 0.1), class = "b", coef = "age_centered")
)



# Fit models with different priors --------------------------------------

bayes_model_positive <- brm(
  formula = bayes_formula,
  data = eu,
  family = bernoulli(),
  prior = priors_positive,
  warmup = 1000,
  iter = 4000,
  chains = 4,
  cores = 4,
  seed = 123
)

bayes_model_neutral <- brm(
  formula = bayes_formula,
  data = eu,
  family = bernoulli(),
  prior = priors_neutral,
  warmup = 1000,
  iter = 4000,
  chains = 4,
  cores = 4,
  seed = 123
)


# -------------------------------------------------------------------------

#Analysis

# Extract posterior distributions for education effect
get_education_posterior <- function(model, prior_type) {
  posterior_samples <- as_draws_df(model) %>%
    select(b_education) %>%
    mutate(prior = prior_type)
  return(posterior_samples)
}

education_posteriors <- bind_rows(
  get_education_posterior(bayes_model_positive, "Theory-informed Prior"),
  get_education_posterior(bayes_model_neutral, "Uninformative Prior"),
)


# Plot Posteriors ---------------------------------------------------------



  # Create comparison plot with improved colors and legend positioning
  ggplot(education_posteriors, aes(x = b_education, fill = prior)) +
    # Use colorblind-friendly, publication-appropriate colors
    geom_density(alpha = 0.5) +
    scale_fill_manual(
      values = c("Positive Prior" = "#1b9e77", "Neutral Prior" = "#7570b3"),
      labels = c("Positive Prior" = "Theory-informed Prior",
                 "Neutral Prior" = "Uninformative Prior")) +
    # Add vertical line for frequentist estimate
    geom_vline(xintercept = -0.05,
               linetype = "dashed",
               color = "black") +
    annotate("text",
             x = -0.060,
             y = max(density(education_posteriors$b_education)$y) * 1.6,
             label = "Frequentist\nEstimate\n(OR = 0.95)",
             hjust = 1,
             size = 3.5) +
    labs(x = "Effect of Education on Migration Aspirations \n(Log-Odds Scale)",
         y = "Posterior Density") +
    theme_minimal() +
    # Position legend at bottom
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          #axis.title = element_text(size = 10),
          panel.grid.minor = element_blank())


# Model Summary Results ---------------------------------------------------


# # Please provide the following model outputs:
#
# 1. Model Summary Results
# For each model (positive, neutral, negative priors), paste the output from:
summary(bayes_model_positive)
summary(bayes_model_neutral)
summary(bayes_model_negative)
