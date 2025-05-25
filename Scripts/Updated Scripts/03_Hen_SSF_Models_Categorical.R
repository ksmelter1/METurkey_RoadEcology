
#'---
#' title: Context-Dependent Habitat selection of Roads by female wild turkeys
#' authors: "K. Smelter
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#'---
#'
knitr::opts_chunk$set(warning = FALSE, message = FALSE, cache = TRUE)
#'  
#' **Purpose**: This script creates generalized linear mixed effects models for the analysis
#' 


#####################
## Load Packages ##
#####################

#' Vector of package names
packages <- c("glmmTMB",
              "amt",
              "tidyverse",
              "sf")

#' Function to load a package or install it if not already installed
load_packages <- function(package_name) {
  if (!require(package_name, character.only = TRUE)) {
    install.packages(package_name, dependencies = TRUE)
    require(package_name, character.only = TRUE)
  }
}

#' Apply the function to each package name
lapply(packages, load_packages)


#' Load in RData with covs
load("RData/Updated SP25/02RandomStepsTODLanduse.RData")


##################
## Mixed SSFs ##
##################

#' Scale parameters
ssf.final <- random_steps.tod %>%
  st_drop_geometry() %>%
  dplyr::mutate(primary = scale(primary)) %>%
  dplyr::mutate(secondary = scale(secondary)) %>%
  dplyr::mutate(T2Sunset = scale(T2Sunset))

#' Change from matrix to numeric
ssf.final <- ssf.final %>%
  dplyr::mutate(primary = as.numeric(primary)) %>%
  dplyr::mutate(secondary = as.numeric(secondary)) %>%
  dplyr::mutate(T2Sunset = as.numeric(T2Sunset))


#' Model formula
#' Case: Whether a step was used or available
#' primary: Distance to primary road
#' secondary: Distance to secondary road
#' T2Sunset: Distance to sunset
#' (0 + primary|BirdID): Distance to primary road varies by each individual and that these parameters have non-negative covariances 
#' (1|step_id_): Included to implement the poisson trick making the model equivalent to a mixed conditional logistic regression 
#' Restricted Maximum Likelihood for mixed models 
#' Developed is the reference level


##############
## Winter ##
##############


Win.Prim.TMB <- glmmTMB(case_~ -1 + primary + Developed + Forest + T2Sunset + # Main Effects
                          primary*Developed + primary*Forest + primary*T2Sunset + # Interaction Effects for Primary Roads
                          (0 + primary|BirdID) +  # Random Slope for Primary Road
                          (1|step_id_),           # Stratum-Specific Random Intercept
                           family= poisson(), REML = T, data = ssf.final %>%
                           dplyr::filter(Season == "Winter")) 

summary(Win.Prim.TMB)


Win.Sec.TMB <- glmmTMB(case_~ -1 + secondary + Developed + Forest + T2Sunset + # Main Effects
                     secondary*Developed + secondary*Forest + secondary*T2Sunset + # Interaction Effects for Secondary Roads
                     (0 + secondary|BirdID) +  # Random Slope for Primary Road
                     (1|step_id_),           # Stratum-Specific Random Intercept
                   family= poisson(), REML = T, data = ssf.final %>%
                     dplyr::filter(Season == "Winter")) 

summary(Win.Sec.TMB)


#################
## Dispersal ##
#################


Spr.Prim.TMB <- glmmTMB(case_~ -1 + primary + Developed + Forest + T2Sunset + # Main Effects
                     primary*Developed + primary*Forest + primary*T2Sunset + # Interaction Effects for Primary Roads
                     (0 + primary|BirdID) +  # Random Slope for Primary Road
                     (1|step_id_),           # Stratum-Specific Random Intercept
                   family= poisson(), REML = T, data = ssf.final %>%
                     dplyr::filter(Season == "Spring Dispersal")) 

summary(Spr.Prim.TMB)


Spr.Sec.TMB <- glmmTMB(case_~ -1 + secondary + Developed + Forest + T2Sunset + # Main Effects
                     secondary*Developed + secondary*Forest + secondary*T2Sunset + # Interaction Effects for Secondary Roads
                     (0 + secondary|BirdID) +  # Random Slope for Primary Road
                     (1|step_id_),           # Stratum-Specific Random Intercept
                   family= poisson(), REML = T, data = ssf.final %>%
                     dplyr::filter(Season == "Spring Dispersal")) 

summary(Spr.Sec.TMB)



#################
## Pre-Nesting ##
#################


Pre.Prim.TMB <- glmmTMB(case_~ -1 + primary + Developed + Forest + T2Sunset + # Main Effects
                          primary*Developed + primary*Forest + primary*T2Sunset + # Interaction Effects for Primary Roads
                          (0 + primary|BirdID) +  # Random Slope for Primary Road
                          (1|step_id_),           # Stratum-Specific Random Intercept
                        family= poisson(), REML = T, data = ssf.final %>%
                          dplyr::filter(Season == "Pre-Nesting")) 

summary(Pre.Prim.TMB)


Pre.Sec.TMB <- glmmTMB(case_~ -1 + secondary + Developed + Forest + T2Sunset + # Main Effects
                         secondary*Developed + secondary*Forest + secondary*T2Sunset + # Interaction Effects for Secondary Roads
                         (0 + secondary|BirdID) +  # Random Slope for Primary Road
                         (1|step_id_),           # Stratum-Specific Random Intercept
                       family= poisson(), REML = T, data = ssf.final %>%
                         dplyr::filter(Season == "Pre-Nesting")) 

summary(Pre.Sec.TMB)



#################
## Summer ##
#################


Sum.Prim.TMB <- glmmTMB(case_~ -1 + primary + Developed + Forest + T2Sunset + # Main Effects
                          primary*Developed + primary*Forest + primary*T2Sunset + # Interaction Effects for Primary Roads
                          (0 + primary|BirdID) +  # Random Slope for Primary Road
                          (1|step_id_),           # Stratum-Specific Random Intercept
                        family= poisson(), REML = T, data = ssf.final %>%
                          dplyr::filter(Season == "Summer")) 

summary(Sum.Prim.TMB)


Sum.Sec.TMB <- glmmTMB(case_~ -1 + secondary + Developed + Forest + T2Sunset + # Main Effects
                         secondary*Developed + secondary*Forest + secondary*T2Sunset + # Interaction Effects for Secondary Roads
                         (0 + secondary|BirdID) +  # Random Slope for Primary Road
                         (1|step_id_),           # Stratum-Specific Random Intercept
                       family= poisson(), REML = T, data = ssf.final %>%
                         dplyr::filter(Season == "Summer")) 

summary(Sum.Sec.TMB)


############
## Fall ##
###########


Fall.Prim.TMB <- glmmTMB(case_~ -1 + primary + Developed + Forest + T2Sunset + # Main Effects
                          primary*Developed + primary*Forest + primary*T2Sunset + # Interaction Effects for Primary Roads
                          (0 + primary|BirdID) +  # Random Slope for Primary Road
                          (1|step_id_),           # Stratum-Specific Random Intercept
                        family= poisson(), REML = T, data = ssf.final %>%
                          dplyr::filter(Season == "Fall")) 

summary(Fall.Prim.TMB)


Fall.Sec.TMB <- glmmTMB(case_~ -1 + secondary + Developed + Forest + T2Sunset + # Main Effects
                         secondary*Developed + secondary*Forest + secondary*T2Sunset + # Interaction Effects for Secondary Roads
                         (0 + secondary|BirdID) +  # Random Slope for Primary Road
                         (1|step_id_),           # Stratum-Specific Random Intercept
                       family= poisson(), REML = T, data = ssf.final %>%
                         dplyr::filter(Season == "Fall")) 

summary(Fall.Sec.TMB)

################################################################################
###############################################################################X