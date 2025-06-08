

#Age (28.26)
eu %>%
  summarise(
    weighted_mean = sum(age * sweight, na.rm = TRUE) / sum(sweight, na.rm = TRUE)
  ) %>%
  pull(weighted_mean) # Extract the result as a scalar value


#Unmarried (0.387)
eu %>%
  filter(!is.na(marital_status)) %>% # Exclude rows where `children` is NA
  group_by(marital_status) %>%
  summarise(
    weighted_total = sum(sweight, na.rm = TRUE), # Sum of weights for each category
    .groups = "drop"
  ) %>%
  mutate(
    weighted_proportion = weighted_total / sum(weighted_total) # Calculate proportions
  )


#No Children (0.45)
eu %>%
filter(!is.na(children)) %>% # Exclude rows where `children` is NA
  group_by(children) %>%
  summarise(
    weighted_total = sum(sweight, na.rm = TRUE), # Sum of weights for each category
    .groups = "drop"
  ) %>%
  mutate(
    weighted_proportion = weighted_total / sum(weighted_total) # Calculate proportions
  )


#Family Migration Experience (0.48)
eu %>%
  filter(!is.na(family_migration)) %>% # Exclude rows where `children` is NA
  group_by(family_migration) %>%
  summarise(
    weighted_total = sum(sweight, na.rm = TRUE), # Sum of weights for each category
    .groups = "drop"
  ) %>%
  mutate(
    weighted_proportion = weighted_total / sum(weighted_total) # Calculate proportions
  )


#Male (0.58)
eu %>%
  filter(!is.na(gender)) %>% # Exclude rows where `children` is NA
  group_by(gender) %>%
  summarise(
    weighted_total = sum(sweight, na.rm = TRUE), # Sum of weights for each category
    .groups = "drop"
  ) %>%
  mutate(
    weighted_proportion = weighted_total / sum(weighted_total) # Calculate proportions
  )


#Years of Education (9.85)
eu %>%
  summarise(
    weighted_mean = sum(education * sweight, na.rm = TRUE) / sum(sweight, na.rm = TRUE)
  ) %>%
  pull(weighted_mean) # Extract the result as a scalar value


#perception of human rights in Europe (2.76)
mean(eu$human_rights_europe, na.rm = TRUE)

eu %>%
  summarise(
    weighted_mean = sum(human_rights_europe * sweight, na.rm = TRUE) / sum(sweight, na.rm = TRUE)
  ) %>%
  pull(weighted_mean) # Extract the result as a scalar value

#Perception of human rights own country (mean = 1.78, sd = 0.62)
mean(eu$human_rights_country, na.rm = TRUE)
sd(eu$human_rights_country, na.rm = TRUE)

eu %>%
  summarise(
    weighted_mean = sum(human_rights_country * sweight, na.rm = TRUE) / sum(sweight, na.rm = TRUE)
  ) %>%
  pull(weighted_mean) # Extract the result as a scalar value

#Perception of job opportunities in Europe (mean = 2.22, sd = 0.95)
mean(eu$perception_jobs_europe, na.rm = TRUE)
sd(eu$perception_jobs_europe, na.rm = TRUE)

eu %>%
  summarise(
    weighted_mean = sum(perception_jobs_europe * sweight, na.rm = TRUE) / sum(sweight, na.rm = TRUE)
  ) %>%
  pull(weighted_mean) # Extract the result as a scalar value


#Job opportunities own country (2.68)
mean(eu$perception_jobs_country, na.rm = TRUE)
sd(eu$perception_jobs_country, na.rm = TRUE)

eu %>%
  summarise(
    weighted_mean = sum(perception_jobs_country * sweight, na.rm = TRUE) / sum(sweight, na.rm = TRUE)
  ) %>%
  pull(weighted_mean) # Extract the result as a scalar value

## Wealth Index
mean(eu$wealth_index)

eu %>%
  summarise(
    weighted_mean = sum(wealth_index * sweight, na.rm = TRUE) / sum(sweight, na.rm = TRUE)
  ) %>%
  pull(weighted_mean) # Extract the result as a scalar value


#Perception of living and workign conditions (2.21)
mean(eu$migrant_conditions_europe, na.rm = TRUE)
sd(eu$migrant_conditions_europe, na.rm = TRUE)

eu %>%
  summarise(
    weighted_mean = sum(migrant_conditions_europe * sweight, na.rm = TRUE) / sum(sweight, na.rm = TRUE)
  ) %>%
  pull(weighted_mean) # Extract the result as a scalar value






