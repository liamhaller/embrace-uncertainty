#package manaagement

# List of required packages
required_packages <- c("tidyverse", "haven", "psych", "forcats",
                       "survey", "modelsummary", "brms", "bayesplot",
                       "tidybayes")




# Function to install and load necessary packages
manage_packages <- function(packages) {
  # Check if each package is installed
  installed_packages <- rownames(installed.packages())

  # Install missing packages
  for (pkg in packages) {
    if (!(pkg %in% installed_packages)) {
      message(paste("Installing", pkg))
      install.packages(pkg)
    } else {
      message(paste(pkg, "is already installed"))
    }
  }

  # Load all packages
  lapply(packages, library, character.only = TRUE)
}


# Call the function to manage packages
manage_packages(required_packages)


#Clean up global enviroment
rm(manage_packages)
rm(required_packages)

