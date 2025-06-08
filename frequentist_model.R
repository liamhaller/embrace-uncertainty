


formula <- europe_aspiration ~ age_centered + marital_status + children +
  family_migration + gender + education + human_rights_europe +
  human_rights_country + migrant_conditions_europe + perception_jobs_country +
  perception_jobs_europe + wealth_index + ra


model <- glm(formula, data = eu, family = binomial, weights = sweight)
summary(model)

odds_ratios <- exp(coef(model))
print(odds_ratios)
