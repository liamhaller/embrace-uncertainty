# Overview
The paper demonstrates how Bayesian methods can address key limitations of frequentist statistics in migration research by:

Directly quantifying hypothesis probabilities rather than producing binary significance tests
Incorporating prior knowledge from existing literature into statistical models
Providing intuitive uncertainty quantification for policy communication

# Replication
### Data Requirements

EUMAGINE dataset: Individual questionnaire - STUM 20121001 - incl hh and mgcount - mv.dta
Place the dataset file in the project root directory

### Running the Analysis
Execute main_script.R to reproduce all analyses. The workflow includes:

Data loading (dataloading.R) - Import and filter EUMAGINE survey data
Data transformation (transformations.R) - Create variables matching original study
Verification (verification.R) - Confirm replication accuracy
Frequentist model (frequentist_model.R) - Reproduce original logistic regression
Bayesian model (bayesian_model.R) - Fit models with theory-informed and uninformative priors

### Key Files

main_script.R - Master script to run complete analysis
bayesian_model.R - Core Bayesian analysis comparing different prior specifications
prior.R - Prior distribution construction and visualization
package_manager.R - Automated package installation and loading
