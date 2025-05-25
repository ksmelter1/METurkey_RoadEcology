
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
load("RData/Updated FA24/02RandomStepsTODLanduse.RData")


##################
## Mixed SSFs ##
##################

#' Scale parameters
ssf.final <- random_steps.sf.landcov.tod %>%
  st_drop_geometry() %>%
  dplyr::mutate(primary = scale(primary)) %>%
  dplyr::mutate(secondary = scale(secondary)) %>%
  dplyr::mutate(pct_developed = scale(pct_developed)) %>%
  dplyr::mutate(pct_forest = scale(pct_forest)) %>%
  dplyr::mutate(pct_agriculture = scale(pct_agriculture)) 

#' Change from matrix to numeric
ssf.final <- ssf.final %>%
  dplyr::mutate(primary = as.numeric(primary)) %>%
  dplyr::mutate(secondary = as.numeric(secondary)) %>%
  dplyr::mutate(pct_developed = as.numeric(pct_developed)) %>%
  dplyr::mutate(pct_forest = as.numeric(pct_forest)) %>%
  dplyr::mutate(pct_agriculture = as.numeric(pct_agriculture)) 
table(ssf.final$Time)

ssf.final <- ssf.final %>%
  dplyr::mutate(
    Time = factor(Time, levels = c("Morning", "Midday", "Afternoon")),
    Time_Morning = ifelse(Time == "Morning", 1, 0),
    Time_Midday = ifelse(Time == "Midday", 1, 0),
    Time_Afternoon = ifelse(Time == "Afternoon", 1, 0)
  )

#' Model formula
#' Case: Whether a step was used or available
#' primary: Distance to primary road
#' pct_forest: Percent forest within a 200m buffer
#' primary*pct_forest: Interaction between distance to primary road and percent forest
#' primary*Time_Morning: Interaction between distance to road and morning 
#' primary*Time_Afternoon: Interaction between distance to road and Afternoon
#' (0 + primary|BirdID): Distance to primary road varies by each individual and that these parameters have non-negative covariances 
#' (1|step_id_): Included to implement the poisson trick making the model equivalent to a mixed conditional logistic regression 
#' Restricted Maximum Likelihood for mixed models 
#' Reference level: Time_Midday

##########################
## Winter- Primary Roads
##########################


Prim.For.Win.TMB <- glmmTMB(case_~ -1 + primary + pct_forest + Time_Morning + Time_Afternoon +  # Main Effects
                          primary*pct_forest + primary*Time_Morning + primary*Time_Afternoon +  # Interactions
                          (0 + primary|BirdID) +  # Random Slope
                          (1|step_id_),           # Random Intercept
                           family= poisson(), REML = T, data = ssf.final %>%
                           dplyr::filter(Season == "Winter")) 

summary(Prim.For.Win.TMB)


Prim.Ag.Win.TMB <- glmmTMB(case_~ -1 + primary + pct_agriculture + Time_Morning + Time_Afternoon +  # Main Effects
                              primary*pct_agriculture + primary*Time_Morning + primary*Time_Afternoon +  # Interactions
                              (0 + primary|BirdID) +  # Random Slope
                              (1|step_id_),           # Random Intercept
                              family= poisson(), REML = T, data = ssf.final %>%
                              dplyr::filter(Season == "Winter")) 

summary(Prim.Ag.Win.TMB)



Prim.Dev.Win.TMB <- glmmTMB(case_~ -1 + primary + pct_developed + Time_Morning + Time_Afternoon + # Main Effects
                            primary*pct_developed + primary*Time_Morning + primary*Time_Afternoon +  # Interactions
                              (0 + primary|BirdID) +  # Random Slope
                              (1|step_id_),           # Random Intercept
                              family= poisson(), REML = T, data = ssf.final %>% 
                              dplyr::filter(Season == "Winter"))

summary(Prim.Dev.Win.TMB)



######################
## Secondary Roads ##
######################

Sec.For.Win.TMB <- glmmTMB(case_~ -1 + secondary + pct_forest + Time_Morning + Time_Afternoon + # Main Effects
                                  secondary*pct_forest + secondary*Time_Morning + secondary*Time_Afternoon + # Interactions
                                  (0 + secondary|BirdID) +  # Random Slope
                                  (1|step_id_),           # Random Intercept
                                  family= poisson(), REML = T, data = ssf.final %>%
                                  dplyr::filter(Season == "Winter")) 

summary(Sec.For.Win.TMB)


Sec.Ag.Win.TMB <- glmmTMB(case_~ -1 + secondary + pct_agriculture + Time_Morning + Time_Afternoon + # Main Effects
                             secondary*pct_agriculture + secondary*Time_Morning + secondary*Time_Afternoon + # Interactions
                             (0 + secondary|BirdID) +  # Random Slope
                             (1|step_id_),           # Random Intercept
                             family= poisson(), REML = T, data = ssf.final %>%
                             dplyr::filter(Season == "Winter")) 

summary(Sec.Ag.Win.TMB)


Sec.Dev.Win.TMB <- glmmTMB(case_~ -1 + secondary + pct_developed + Time_Morning + Time_Afternoon + # Main Effects
                            secondary*pct_developed + secondary*Time_Morning + secondary*Time_Afternoon + # Interactions
                            (0 + secondary|BirdID) +  # Random Slope
                            (1|step_id_),           # Random Intercept
                            family= poisson(), REML = T, data = ssf.final %>%
                            dplyr::filter(Season == "Winter")) 

summary(Sec.Dev.Win.TMB)


######################################
## Spring Dispersal- Primary Roads ##
######################################

Prim.For.Spr.TMB <- glmmTMB(case_~ -1 + primary + pct_forest + Time_Morning + Time_Afternoon + # Main Effects
                                  primary*pct_forest + primary*Time_Morning + primary*Time_Afternoon +  # Interaction
                                  (0 + primary|BirdID) +  # Random Slope
                                  (1|step_id_),           # Random Intercept
                                  family= poisson(), REML = T, data = ssf.final %>%
                                  dplyr::filter(Season == "Spring Dispersal")) 

summary(Prim.For.Spr.TMB)


Prim.Ag.Spr.TMB <- glmmTMB(case_~ -1 + primary + pct_agriculture + Time_Morning + Time_Afternoon + # Main Effects
                              primary*pct_agriculture + primary*Time_Morning + primary*Time_Afternoon +  # Interaction
                              (0 + primary|BirdID) +  # Random Slope
                              (1|step_id_),           # Random Intercept
                              family= poisson(), REML = T, data = ssf.final %>%
                              dplyr::filter(Season == "Spring Dispersal")) 

summary(Prim.Ag.Spr.TMB)


Prim.Dev.Spr.TMB <- glmmTMB(case_~ -1 + primary + pct_developed + Time_Morning + Time_Afternoon + # Main Effects
                             primary*pct_developed + primary*Time_Morning + primary*Time_Afternoon +  # Interaction
                             (0 + primary|BirdID) +  # Random Slope
                             (1|step_id_),           # Random Intercept
                             family= poisson(), REML = T, data = ssf.final %>%
                             dplyr::filter(Season == "Spring Dispersal")) 

summary(Prim.Dev.Spr.TMB)


########################################
## Spring Dispersal-Secondary Roads ##
########################################


Sec.For.Spr.TMB <- glmmTMB(case_~ -1 + secondary + pct_forest + Time_Morning + Time_Afternoon + # Main Effects
                              secondary*pct_forest + secondary*Time_Morning + secondary*Time_Afternoon +  # Interaction
                              (0 + secondary|BirdID) +  # Random Slope
                              (1|step_id_),           # Random Intercept
                              family= poisson(), REML = T, data = ssf.final %>%
                              dplyr::filter(Season == "Spring Dispersal")) 

summary(Sec.For.Spr.TMB)


Sec.Ag.Spr.TMB <- glmmTMB(case_~ -1 + secondary + pct_agriculture + Time_Morning + Time_Afternoon + # Main Effects
                             secondary*pct_agriculture + secondary*Time_Morning + secondary*Time_Afternoon +  # Interaction
                             (0 + secondary|BirdID) +  # Random Slope
                             (1|step_id_),           # Random Intercept
                             family= poisson(), REML = T, data = ssf.final %>%
                             dplyr::filter(Season == "Spring Dispersal")) 

summary(Sec.Ag.Spr.TMB)


Sec.Dev.Spr.TMB <- glmmTMB(case_~ -1 + secondary + pct_developed + Time_Morning + Time_Afternoon + # Main Effects
                            secondary*pct_developed + secondary*Time_Morning + secondary*Time_Afternoon +  # Interaction
                            (0 + secondary|BirdID) +  # Random Slope
                            (1|step_id_),           # Random Intercept
                            family= poisson(), REML = T, data = ssf.final %>%
                            dplyr::filter(Season == "Spring Dispersal")) 

summary(Sec.Dev.Spr.TMB)


######################################
## Pre-Nesting- Primary Roads ##
######################################

Prim.For.Pre.TMB <- glmmTMB(case_~ -1 + primary + pct_forest + Time_Morning + Time_Afternoon + # Main Effects
                             primary*pct_forest + primary*Time_Morning + primary*Time_Afternoon +  # Interaction
                             (0 + primary|BirdID) +  # Random Slope
                             (1|step_id_),           # Random Intercept
                             family= poisson(), REML = T, data = ssf.final %>%
                             dplyr::filter(Season == "Pre-Nesting")) 

summary(Prim.For.Pre.TMB)


Prim.Ag.Pre.TMB <- glmmTMB(case_~ -1 + primary + pct_agriculture + Time_Morning + Time_Afternoon + # Main Effects
                              primary*pct_agriculture + primary*Time_Morning + primary*Time_Afternoon +  # Interaction
                              (0 + primary|BirdID) +  # Random Slope
                              (1|step_id_),           # Random Intercept
                              family= poisson(), REML = T, data = ssf.final %>%
                              dplyr::filter(Season == "Pre-Nesting")) 

summary(Prim.Ag.Pre.TMB)

Prim.Dev.Pre.TMB <- glmmTMB(case_~ -1 + primary + pct_developed + Time_Morning + Time_Afternoon + # Main Effects
                             primary*pct_developed + primary*Time_Morning + primary*Time_Afternoon +  # Interaction
                             (0 + primary|BirdID) +  # Random Slope
                             (1|step_id_),           # Random Intercept
                           family= poisson(), REML = T, data = ssf.final %>%
                             dplyr::filter(Season == "Pre-Nesting")) 

summary(Prim.Dev.Pre.TMB)


###################################
## Pre-Nesting Secondary Roads ##
###################################

Sec.For.Pre.TMB <- glmmTMB(case_~ -1 + secondary + pct_forest + Time_Morning + Time_Afternoon + # Main Effects
                              secondary*pct_forest + secondary*Time_Morning + secondary*Time_Afternoon +  # Interaction
                              (0 + secondary|BirdID) +  # Random Slope
                              (1|step_id_),           # Random Intercept
                              family= poisson(), REML = T, data = ssf.final %>%
                              dplyr::filter(Season == "Pre-Nesting")) 

summary(Sec.For.Pre.TMB)

Sec.Ag.Pre.TMB <- glmmTMB(case_~ -1 + secondary + pct_agriculture + Time_Morning + Time_Afternoon + # Main Effects
                             secondary*pct_agriculture + secondary*Time_Morning + secondary*Time_Afternoon +  # Interaction
                             (0 + secondary|BirdID) +  # Random Slope
                             (1|step_id_),           # Random Intercept
                             family= poisson(), REML = T, data = ssf.final %>%
                             dplyr::filter(Season == "Pre-Nesting")) 

summary(Sec.Ag.Pre.TMB)


Sec.Dev.Pre.TMB <- glmmTMB(case_~ -1 + secondary + pct_developed + Time_Morning + Time_Afternoon + # Main Effects
                            secondary*pct_developed + secondary*Time_Morning + secondary*Time_Afternoon +  # Interaction
                            (0 + secondary|BirdID) +  # Random Slope
                            (1|step_id_),           # Random Intercept
                            family= poisson(), REML = T, data = ssf.final %>%
                            dplyr::filter(Season == "Pre-Nesting")) 

summary(Sec.Dev.Pre.TMB)


##############################
## Summer- Primary Roads ##
##############################

Prim.For.Sum.TMB <- glmmTMB(case_~ -1 + primary + pct_forest + Time_Morning + Time_Afternoon + # Main Effects
                              primary*pct_forest + primary*Time_Morning + primary*Time_Afternoon +  # Interaction
                              (0 + primary|BirdID) +  # Random Slope
                              (1|step_id_),           # Random Intercept
                            family= poisson(), REML = T, data = ssf.final %>%
                              dplyr::filter(Season == "Summer")) 

summary(Prim.For.Sum.TMB)


Prim.Ag.Sum.TMB <- glmmTMB(case_~ -1 + primary + pct_agriculture + Time_Morning + Time_Afternoon + # Main Effects
                              primary*pct_agriculture + primary*Time_Morning + primary*Time_Afternoon +  # Interaction
                              (0 + primary|BirdID) +  # Random Slope
                              (1|step_id_),           # Random Intercept
                            family= poisson(), REML = T, data = ssf.final %>%
                              dplyr::filter(Season == "Summer")) 

summary(Prim.Ag.Sum.TMB)


Prim.Dev.Sum.TMB <- glmmTMB(case_~ -1 + primary + pct_developed + Time_Morning + Time_Afternoon + # Main Effects
                             primary*pct_developed + primary*Time_Morning + primary*Time_Afternoon +  # Interaction
                             (0 + primary|BirdID) +  # Random Slope
                             (1|step_id_),           # Random Intercept
                             family= poisson(), REML = T, data = ssf.final %>%
                             dplyr::filter(Season == "Summer")) 

summary(Prim.Dev.Sum.TMB)


###############################
## Summer- Secondary Roads ##
###############################

Sec.For.Sum.TMB <- glmmTMB(case_~ -1 + secondary + pct_forest + Time_Morning + Time_Afternoon + # Main Effects
                                 secondary*pct_forest + secondary*Time_Morning + secondary*Time_Afternoon +  # Interaction
                                 (0 + secondary|BirdID) +  # Random Slope
                                 (1|step_id_),           # Random Intercept
                                 family= poisson(), REML = T, data = ssf.final %>%
                                 dplyr::filter(Season == "Summer")) 

summary(Sec.For.Sum.TMB)


Sec.Ag.Sum.TMB <- glmmTMB(case_~ -1 + secondary + pct_agriculture + Time_Morning + Time_Afternoon + # Main Effects
                             secondary*pct_agriculture + secondary*Time_Morning + secondary*Time_Afternoon +  # Interaction
                             (0 + secondary|BirdID) +  # Random Slope
                             (1|step_id_),           # Random Intercept
                             family= poisson(), REML = T, data = ssf.final %>%
                             dplyr::filter(Season == "Summer")) 

summary(Sec.For.Sum.TMB)


Sec.Dev.Sum.TMB <- glmmTMB(case_~ -1 + secondary + pct_developed + Time_Morning + Time_Afternoon + # Main Effects
                            secondary*pct_developed + secondary*Time_Morning + secondary*Time_Afternoon +  # Interaction
                            (0 + secondary|BirdID) +  # Random Slope
                            (1|step_id_),           # Random Intercept
                          family= poisson(), REML = T, data = ssf.final %>%
                            dplyr::filter(Season == "Summer")) 

summary(Sec.Dev.Sum.TMB)

################################################################################
###############################################################################X