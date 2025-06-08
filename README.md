# Embrace Uncertainty: Bayesian vs. Frequentist Migration Research

This repository contains R code and analysis for "A Call to Embrace Uncertainty: Rethinking Statistical Inference in Migration Research," which compares Bayesian and frequentist approaches using Turkish migration aspiration data from the EUMAGINE project.

## Overview

The paper demonstrates how Bayesian methods can address key limitations of frequentist statistics in migration research by:
- Directly quantifying hypothesis probabilities rather than producing binary significance tests
- Incorporating prior knowledge from existing literature into statistical models
- Providing intuitive uncertainty quantification for policy communication

## Replication

### Data Requirements
- EUMAGINE dataset: `Individual questionnaire - STUM 20121001 - incl hh and mgcount - mv.dta`
- Place the dataset file in the project root directory

### Running the Analysis
Execute `main_script.R` to reproduce all analyses. The workflow includes:

1.  **Data loading** (`dataloading.R`) - Import and filter EUMAGINE survey data
2. **Data transformation** (`transformations.R`) - Create variables matching original study
3. **Verification** (`verification.R`) - Confirm replication accuracy
4. **Frequentist model** (`frequentist_model.R`) - Reproduce original logistic regression
5. **Bayesian model** (`bayesian_model.R`) - Fit models with theory-informed and uninformative priors

### Key Files
- `main_script.R` - Master script to run complete analysis
- `bayesian_model.R` - Core Bayesian analysis comparing different prior specifications
- `prior.R` - Prior distribution construction and visualization
- `package_manager.R` - Automated package installation and loading

## Requirements

```r
# Key packages (auto-installed via package_manager.R)
c("tidyverse", "haven", "psych", "brms", "bayesplot", "tidybayes")
