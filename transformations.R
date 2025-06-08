




# Dependent Variable ------------------------------------------------------


# Create the new variable europe_aspiration
eu <- eu %>%
  mutate(europe_aspiration = case_when(

    a1 == 0 ~ 0,
    a2 %in% c(40, #Austria
              56, #Belgium
              191, #Croatia
              208, #Denmark
              246, #Finland
              250, #France
              276, #Germany
              300, #Greece
              528, #Netherlands
              578, #Norway
              643, #Russia
              724, #Spain
              756, #Switzerland
              804, #Ukraine
              826 #United Kingdom
              ) ~ 1,  # Assign 1 if an EU country (Per paper's def.)
    TRUE ~ NA_integer_          # Assign 0 otherwise
  )) %>%
  select(!c(a1,a2))

#In the original article the following countires were interpreted as Europe
#Austria, Belgium, Bulgaria, Belarus, Europe,
#Western Europe, Croatia, Cyprus, Czech Republic,
#Denmark, Finland, France, Germany, Greece, Hungary,
#Iceland, Ireland, Italy, Latvia, Luxembourg, Netherlands,
#Norway, Poland, Portugal, Romania, Russia, Slovakia, Spain,
#Sweden, Switzerland, Ukraine, the UK and Lithuania

# Independent Variables ---------------------------------------------------

## Age.
#Filter below 19 & over 40, then mean center the variable
eu <- eu %>%
  filter(age >= 19, age <= 39) %>%  # Keep respondents between 19 and 39
  mutate(age_centered = age - mean(age, na.rm = TRUE))  # Mean-center the age variable


# ## Marital Status
# Create the new variable europe_aspiration
eu <- eu %>%
  mutate(marital_status = case_when(
    hh9 %in% c(1, #Unmaried
              5, #Divorced
              6, #widowed
              7 #seperated
             ) ~ "Not Married", # Label for unmarried categories
    hh9 %in% c(2, #Married, monogamous
               3, #Married, Polygamous
               4 # Living with partner
    ) ~ "Married", # Label for unmarried categories
    TRUE ~ NA_character_
  ) %>%
    factor(levels = c("Married", "Not Married")) # Set levels and reference category
  ) %>%
  select(!c(hh9))


## Children.
# Create a new variable `children` in the dataset `eu`
eu$children <- ifelse(
  # Check if either `cf6` or `cf7` is NA
  is.na(eu$cf6) | is.na(eu$cf7),
  # If either is NA, set `children` to NA
  NA,
  # Otherwise, check if both `cf6` and `cf7` are 0
  ifelse(
    eu$cf6 == 0 & eu$cf7 == 0,
    # If both are 0, set `children` to "No Children"
    "No Children",
    # If either or both are 1, set `children` to "Yes Children"
    "Yes Children"
  )
) %>% factor(levels = c("Yes Children", "No Children")) # Set reference category to "No Migration Experience"


# Explanation of the conditions:
# - `cf6`: Do you have children?
# - `cf7`: Do you have children that don't live in the house?
# Logic:
# - If both `cf6` and `cf7` are 0 -> "No Children"
# - If either or both `cf6` and `cf7` are 1 -> "Yes Children"
# - If either `cf6` or `cf7` is NA -> NA



# ## Family migration experience.
# # 0 =no mig. experience, 1 = family mig. experience
# eu <- eu %>%
#   rename(family_migration = mg1) %>%
#   mutate(
#     family_migration = case_when(
#       family_migration == 0 ~ "No Migration Experience",  # Label for no migration experience
#       family_migration == 1 ~ "Family Migration Experience"  # Label for family migration experience
#     ) %>%
#       factor(levels = c("No Migration Experience", "Family Migration Experience")) # Set reference category to "No Migration Experience"
#   )


# # Create a new variable `Family migration experience` in the dataset `eu`
# eu$family_migration <- ifelse(
#   # Check if either `cf6` or `cf7` is NA
#   is.na(eu$mg1) | is.na(eu$mg10),
#   # If either is NA, set `children` to NA
#   NA,
#   # Otherwise, check if both `cf6` and `cf7` are 0
#   ifelse(
#     eu$mg1 == 0 & eu$mg10 == 0,
#     # If both are 0, set `children` to "No Children"
#     "No Migration Experience",
#     # If either or both are 1, set `children` to "Yes Children"
#     "Family Migration Experience"
#   )
# ) %>% factor(levels = c("No Migration Experience", "Family Migration Experience"))
#


eu <- eu %>%
  mutate(family_migration = ifelse(is.na(mg1count),
                                   "No Migration Experience",
                                   "Family Migration Experience"))

eu$family_migration <- factor(eu$family_migration,
                              levels = c("No Migration Experience", "Family Migration Experience"))



## Gender.
# 0=male, 1=female
# Rename and recode gender variable
eu <- eu %>%
  rename(gender = hh3) %>%
  mutate(
    gender = case_when(
      gender == 0 ~ "Male",    # Label for male
      gender == 1 ~ "Female"  # Label for female
    ) %>%
      factor(levels = c("Female", "Male")) # Set reference category to "Female"
  )



## Education
eu <- eu %>%
  rename(education = hh7)

eu$education <- ifelse(eu$education > 21, 0, eu$education)



# Rights Indexes ----------------------------------------------------------

#Adjust scales from 1-5 to 0-4
eu <- eu %>%
 mutate(across(c(peu1:peu5, p1:p5, peu8, p8, a13:a16), ~ . - 1))


# Index for perceptions of human rights in Europe
# 0 = Very Bad, 4 = Very Good
eu <- eu %>%
  mutate(
    # Sum rows, but result is NA if any of the values in p1:p5 are NA
    human_rights_europe = rowSums(across(peu1:peu5), na.rm = FALSE)) %>%
  mutate(human_rights_europe = human_rights_europe/5)

# Perceptions of human rights in their own country
# Originally the variable is coded as 0 = Very Bad, 4 = Very Good
#However the reference for the model is that 0 =very good meaning
#it needs to be reversed
eu <- eu %>%
  mutate(across(p1:p5, ~ 4 - .))

eu <- eu %>%
  mutate(
    # Sum rows, but result is NA if any of the values in p1:p5 are NA
    human_rights_country = rowSums(across(p1:p5), na.rm = FALSE)) %>%
  mutate(human_rights_country = human_rights_country/5)


#Job Opportunities in Europe
# Rename perception of job opportunities in Europe

#Orignally the variable is coded as 0 = Strnogly agree, 4 = Strongly disagree
#In the paper Strongly disagree is the refrence category so it needs to be flipped
eu <- eu %>%
  mutate(across(peu8, ~ 4 - .))

eu <- eu %>%
  rename(perception_jobs_europe = peu8)


#Job opportunities in own country
eu <- eu %>%
  rename(perception_jobs_country = p8)


# Index for perceptions of living and working conditions of migrants in Europe
#0=Very bad, 4 = very good
# Reverse the scale for items a13 through a16
eu <- eu %>%
  mutate(across(a13:a16, ~ 4 - .))

eu <- eu %>%
  mutate(
    migrant_conditions_europe = rowSums(across(a13:a16), na.rm = FALSE)) %>%
  #normalize values
  mutate(migrant_conditions_europe = migrant_conditions_europe/4)


# Wealth Index ------------------------------------------------------------

# # Subset the wealth-related variables
# wealth_vars <- eu %>% select(w2:w20)
#
# # Check eigenvalues to verify components with eigenvalue > 1
# eigenvalues <- eigen(cor(wealth_vars, use = "pairwise.complete.obs"))$values
# num_components <- sum(eigenvalues > 1)
# cat("Number of components with eigenvalue > 1:", num_components, "\n")
#
# # Perform Principal Component Analysis (PCA)
# pca <- principal(
#   wealth_vars,
#   nfactors = num_components,          # Retain components with eigenvalue > 1
#   rotate = "varimax",                 # Varimax rotation
#   scores = TRUE                       # Compute regression factor scores
# )
#
# # Extract regression factor scores and explained variance
# factor_scores <- as.data.frame(pca$scores)  # Regression factor scores
# explained_variance <- pca$Vaccounted["Proportion Var", ]  # Explained variance for each component
#
# # Multiply each factor score by its explained variance
# weighted_scores <- factor_scores %>%
#   mutate(across(everything(), ~ . * explained_variance[colnames(factor_scores) == cur_column()]))
#
# # Sum the weighted scores into a single wealth index
# eu$wealth_index <- rowSums(weighted_scores, na.rm = TRUE)
#
# # Normalize the wealth index to a 0-4 categorical variable
# eu$wealth_index <- cut(
#   scale(eu$wealth_index, center = min(eu$wealth_index), scale = max(eu$wealth_index) - min(eu$wealth_index)) * 4,
#   breaks = 5,
#   labels = 0:4,
#   include.lowest = TRUE
# )
#
# # Display the resulting wealth index
# head(eu$wealth_index)

#
# # Subset the wealth-related variables
# wealth_vars <- eu %>% select(w2:w20)
#
# # Check eigenvalues to verify components with eigenvalue > 1
# eigenvalues <- eigen(cor(wealth_vars, use = "pairwise.complete.obs"))$values
# num_components <- sum(eigenvalues > 1)
# cat("Number of components with eigenvalue > 1:", num_components, "\n")
#
# # Perform Principal Component Analysis (PCA)
# pca <- principal(
#   wealth_vars,
#   nfactors = num_components,          # Retain components with eigenvalue > 1
#   rotate = "varimax",                 # Varimax rotation
#   scores = TRUE                       # Compute regression factor scores
# )
#
# # Extract regression factor scores and explained variance
# factor_scores <- as.data.frame(pca$scores)  # Regression factor scores
# explained_variance <- pca$Vaccounted["Proportion Var", ]  # Explained variance for each component
#
# # Multiply each factor score by its explained variance
# weighted_scores <- factor_scores %>%
#   mutate(across(everything(), ~ . * explained_variance[colnames(factor_scores) == cur_column()]))
#
# # Sum the weighted scores into a single wealth index
# eu$wealth_index <- rowSums(weighted_scores, na.rm = TRUE)
#
# # Normalize the wealth index to range from 0 to 4
# eu$wealth_index <- scale(eu$wealth_index, center = min(eu$wealth_index), scale = max(eu$wealth_index) - min(eu$wealth_index)) * 4
#
#
# # Normalize eu$wealth_index to a scale of 0 to 4
# eu$wealth_index_normalized <- 4 * (eu$wealth_index - min(eu$wealth_index, na.rm = TRUE)) /
#   (max(eu$wealth_index, na.rm = TRUE) - min(eu$wealth_index, na.rm = TRUE))
#
# eu$wealth_index_normalized <- 4-eu$wealth_index_normalized
#
# #eu$wealth_index <- 4-eu$wealth_index
#
#
# eu %>%
#   summarise(
#     weighted_mean = sum(wealth_index_normalized  * sweight, na.rm = TRUE) / sum(sweight, na.rm = TRUE)
#   ) %>%
#   pull(weighted_mean) # Extract the result as a scalar value


# mean(eu$wealth_index)
# # Convert to a categorical variable with reversed categories
# eu$wealth_index <- cut(
#   eu$wealth_index,
#   breaks = 5,
#   labels = 4:0,  # Reverse the categories
#   include.lowest = TRUE
# )
#
# # Ensure the wealth index is numeric
# eu$wealth_index <- as.numeric(as.character(eu$wealth_index))
#
# # Display the resulting wealth index
# head(eu$wealth_index)


# Load required package
library(psych)

# Extract the 19 relevant variables from the eu dataset
items <- eu[, paste0("w", 2:20)]

# Perform PCA with varimax rotation and Kaiser normalization.
# Here, we manually specify nfactors=6 because the paper states that six components were retained.
# If you want to confirm the number of components, you could run a scree plot or eigenvalue check first.
pca_result <- principal(items, nfactors = 6, rotate = "varimax", scores = TRUE)

# principal(...) returns factor scores, loadings, and variance accounted data.
# The factor scores are in pca_result$scores.
# The proportion of variance explained by each of the six retained components can be found in:
# pca_result$Vaccounted["Proportion Var", ]

prop_var <- pca_result$Vaccounted["Proportion Var", 1:6]

# Multiply each factor score by its respective component’s proportion of explained variance
weighted_scores <- sweep(pca_result$scores[, 1:6], 2, prop_var, "*")

# Sum these weighted factor scores into a single wealth index
wealth_index_raw <- rowSums(weighted_scores)

# Scale the wealth index to a 0–4 range
wealth_index_scaled <- (wealth_index_raw - min(wealth_index_raw)) / (max(wealth_index_raw) - min(wealth_index_raw)) * 4

# Add this new variable to the original data frame
eu$wealth_index <- wealth_index_scaled

eu$wealth_index <- 4-eu$wealth_index





# Ra ----------------------------------------------------------------------

eu$ra <- ifelse(eu$ra == 21, "Erirdag", "Dinar") %>%
  factor(levels = c("Erirdag", "Dinar"))


#Years of education is coded as 0 (no education, only Koranic school, only basic literacy or national language),
#1 (preschool),
#1–5 (primary school-old system),
#2 – 9 (primary school),
#6 – 8 (lower secondary school-old system),
#9 – 11 (higher voca- tional school),
#10–12 (upper secondary school-old system),
#10–13 (upper secondary school-old system), 1
#4–17 (university or polytechnic)
#to 18–21 (doctorate).

# Recode and set reference category for education_category
# eu <- eu %>%
#   mutate(
#     education_category = case_when(
#       hh7 == 0 ~ "No education/Koranic/Basic literacy",
#       hh7 == 1 ~ "Preschool",
#       hh7 >= 2 & hh7 <= 7 ~ "Primary school",
#       hh7 >= 8 & hh7 <= 11 ~ "Lower secondary school",
#       hh7 >= 12 & hh7 <= 14 ~ "Upper secondary school",
#       hh7 >= 15 & hh7 <= 17 ~ "University/Polytechnic",
#       hh7 >= 18 & hh7 <= 21 ~ "Doctorate",
#       TRUE ~ "Other/Unknown"  # Fallback category for unexpected values
#     ) %>%
#       factor(levels = c(
#         "No education/Koranic/Basic literacy",  # Reference category
#         "Preschool",
#         "Primary school",
#         "Lower secondary school",
#         "Upper secondary school",
#         "University/Polytechnic",
#         "Doctorate",
#         "Other/Unknown"
#       ))
#   )






# Generate NA breakdown
na_breakdown <- eu %>%
  summarise(across(everything(), ~ sum(is.na(.)), .names = "na_count_{col}")) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "na_count") %>%
  mutate(
    na_percentage = round((na_count / nrow(eu)) * 100, 2)
  ) %>%
  arrange(desc(na_count))

# View the NA breakdown
print(na_breakdown, n=100)

table(eu$ra, eu$europe_aspiration)


