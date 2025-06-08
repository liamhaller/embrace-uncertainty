
### Purpose
  #1. reproduce analysis in The Relevance of a “Culture of Migration”
  #in Understanding Migration Aspirations in Contemporary Turkey

  #2. Reformat analysis as Bayesian


#Load Required Packages
source('package_manager.R')

#load custom functions
source('functions.R')

#Load EUMAGINE Data, and select relvant variables
source('dataloading.R')

# Output
  #1. dataset "eu"

#Reproduce indexs used in analysis & data wrangling
source('transformations.R')

#Verrify that the transformed data is the same that is in the paper
source('verrification.R')

source('frequentist_model.R')

source('bayesian_model.R')



modelsummary::modelsummary(model, exponentiate = TRUE)

