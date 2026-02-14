#'---
#' title: Context-Dependent Habitat selection of Roads by female wild turkeys
#' authors: "K. Smelter
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#'---
#'
knitr::opts_chunk$set(warning = FALSE, message = FALSE, cache = TRUE)
#'  
#' **Purpose**: This script creates tracks and extracts covariates using the amt package
#' ** Last Updated**: 12/27/2025

#####################
## Load Packages ##
#####################

# Vector of package names
packages <- c("glmmTMB",
              "amt",
              "tidyverse",
              "sf",
              "broom.mixed",
              "ggdist")

# Function to load a package or install it if not already installed
load_packages <- function(package_name) {
  if (!require(package_name, character.only = TRUE)) {
    install.packages(package_name, dependencies = TRUE)
    require(package_name, character.only = TRUE)
  }
}

# Apply the function to each package name
lapply(packages, load_packages)

# Read in data
load("RData/Updated SP25/02RandomStepsTODLanduse_Ag_Weather_Manuscript.RData")

################################################################################
# Model Structure

# Win.Prim.Mor.Dev.TMB <- glmmTMB(
#   case_ ~  # Response variable: Use
#     -1 +   # Drop Intercept                             
#     primary + Developed + # Main Effects
#     primary*Developed +  # Interaction Effects 
#     (1 | str_ID),       # Stratum-specific random intercept
#   family = poisson(),   # Poisson distribution               
#   data = ssf.final.winter %>% filter(tod_bin == "Morning"), # Filter by time of day  
#   map = list(theta = factor(c(NA))),  # High-fixed variance aids with convergence of random intercept
#   start = list(theta = c(log(1e3)))               
# ) 

################################################################################
## Winter-- Primary Roads

Win.Prim.TMB <- glmmTMB(
  case_ ~  
    -1 +                            
    primary*Developed +  primary*Agriculture + primary*Conifer +
    (1 | str_ID),       
  family = poisson(),        
  data = ssf.final.winter,
  map = list(theta = factor(c(NA))),
  start = list(theta = c(log(1e3)))               
)             

summary(Win.Prim.TMB)
confint(Win.Prim.TMB)

################################################################################
## Winter-- Secondary Roads

Win.Sec.TMB <- glmmTMB(
  case_ ~  
    -1 +                            
    secondary*Developed +  secondary*Agriculture + secondary*Conifer +
    (1 | str_ID),       
  family = poisson(),        
  data = ssf.final.winter,  
  map = list(theta = factor(c(NA))),
  start = list(theta = c(log(1e3)))               
)             

summary(Win.Sec.TMB)
confint(Win.Sec.TMB)

################################################################################
## Spring Movements-- Primary Roads

Spr.Prim.TMB <- glmmTMB(
  case_ ~  
    -1 +                            
    primary*Developed +  primary*Agriculture + primary*Conifer +
    (1 | str_ID),       
  family = poisson(),        
  data = ssf.final.spring,  
  map = list(theta = factor(c(NA))),
  start = list(theta = c(log(1e3)))               
)          

summary(Spr.Prim.TMB)
confint(Spr.Prim.TMB)

################################################################################
## Spring Movements-- Secondary Roads

Spr.Sec.TMB <- glmmTMB(
  case_ ~  
    -1 +                            
    secondary*Developed +  secondary*Agriculture + secondary*Conifer +   
    (1 | str_ID),       
  family = poisson(),        
  data = ssf.final.spring,
  map = list(theta = factor(c(NA))),
  start = list(theta = c(log(1e3)))               
)             

summary(Spr.Sec.TMB)
confint(Spr.Sec.TMB)

################################################################################
## Pre-Nesting-- Primary Roads

Pre.Prim.TMB <- glmmTMB(
  case_ ~  
    -1 +                            
    primary*Developed +  primary*Agriculture + primary*Conifer +
    (1 | str_ID),       
  family = poisson(),        
  data = ssf.final.pre, 
  map = list(theta = factor(c(NA))),
  start = list(theta = c(log(1e3)))               
)             

summary(Pre.Prim.TMB)
confint(Pre.Prim.TMB)

################################################################################
## Pre-Nesting-- Secondary Roads

Pre.Sec.TMB <- glmmTMB(
  case_ ~  
    -1 +                            
    secondary*Developed +  secondary*Agriculture + secondary*Conifer +   
    (1 | str_ID),       
  family = poisson(),        
  data = ssf.final.pre,  
  map = list(theta = factor(c(NA))),
  start = list(theta = c(log(1e3)))               
)             

summary(Pre.Sec.TMB)
confint(Pre.Sec.TMB)

################################################################################
## Summer-- Primary Roads

Sum.Prim.TMB <- glmmTMB(
  case_ ~  
    -1 +                            
    primary*Developed +  primary*Agriculture + primary*Conifer +
    (1 | str_ID),       
  family = poisson(),        
  data = ssf.final.sum, 
  map = list(theta = factor(c(NA))),
  start = list(theta = c(log(1e3)))               
)             

summary(Sum.Prim.TMB)
confint(Sum.Prim.TMB)


################################################################################
## Summer-- Secondary Roads

Sum.Sec.TMB <- glmmTMB(
  case_ ~  
    -1 +                            
    secondary*Developed +  secondary*Agriculture + secondary*Conifer +    
    (1 | str_ID),       
  family = poisson(),        
  data = ssf.final.sum,  
  map = list(theta = factor(c(NA))),
  start = list(theta = c(log(1e3)))               
)             
summary(Sum.Sec.TMB)
confint(Sum.Sec.TMB)

################################################################################
## Fall-- Primary Roads

Fall.Prim.TMB <- glmmTMB(
  case_ ~  
    -1 +                            
    primary*Developed +  primary*Agriculture + primary*Conifer +    
    (1 | str_ID),       
  family = poisson(),        
  data = ssf.final.fall,  
  map = list(theta = factor(c(NA))),
  start = list(theta = c(log(1e3)))               
)             
summary(Fall.Prim.TMB)
confint(Fall.Prim.TMB)

################################################################################
## Fall-- Secondary Roads

Fall.Sec.TMB <- glmmTMB(
  case_ ~  
    -1 +                            
    secondary*Developed +  secondary*Agriculture + secondary*Conifer +    
    (1 | str_ID),       
  family = poisson(),        
  data = ssf.final.fall,  
  map = list(theta = factor(c(NA))),
  start = list(theta = c(log(1e3)))               
)             
summary(Fall.Sec.TMB)
confint(Fall.Sec.TMB)

################################################################################
## Organize Data

models <- list(
  Win.Prim.TMB = Win.Prim.TMB,
  Win.Sec.TMB  = Win.Sec.TMB,
  
  Spr.Prim.TMB = Spr.Prim.TMB,
  Spr.Sec.TMB  = Spr.Sec.TMB,
  
  Pre.Prim.TMB = Pre.Prim.TMB,
  Pre.Sec.TMB  = Pre.Sec.TMB,
  
  Sum.Prim.TMB = Sum.Prim.TMB,
  Sum.Sec.TMB  = Sum.Sec.TMB,
  
  Fall.Prim.TMB = Fall.Prim.TMB,
  Fall.Sec.TMB = Fall.Sec.TMB
  
)

coefs.out <- purrr::map_df(names(models), function(model_name) {
  model <- models[[model_name]]
  coefs <- broom.mixed::tidy(model) %>%
    #dplyr::filter(!stringr::str_starts(term, "sd_")) %>%
    dplyr::select(term, estimate, std.error, statistic, p.value) %>%
    dplyr::mutate(
      model = model_name,
      
      # Season
      Season = stringr::str_sub(model_name, 1, 3),
      Season = dplyr::case_when(
        Season == "Win" ~ "Winter",
        Season == "Spr" ~ "Spring Movements",
        Season == "Pre" ~ "Pre-Nesting",
        Season == "Sum" ~ "Summer",
        Season == "Fal"~ "Fall",
        TRUE ~ NA_character_
      ),
      
      # Road Type
      Road_Type = stringr::str_extract(model_name, "(?<=\\.)[^.]+(?=\\.)"),
      Road_Type = dplyr::case_when(
        Road_Type == "Prim" ~ "Primary",
        Road_Type == "Sec" ~ "Secondary",
        TRUE ~ NA_character_
      ),
      
      # # Time of Day
      # Time_of_Day = stringr::str_split(model_name, "\\.", simplify = TRUE)[3],
      # Time_of_Day = dplyr::case_when(
      #   Time_of_Day == "Mor" ~ "Morning",
      #   Time_of_Day == "Mid" ~ "Midday",
      #   Time_of_Day == "Aft" ~ "Afternoon",
      #   TRUE ~ NA_character_
      # ),
      
      # Landuse
      Landuse = dplyr::case_when(
        stringr::str_detect(term, "Developed") ~ "Developed",
        stringr::str_detect(term, "Conifer") ~ "Conifer",
        stringr::str_detect(term, "Agriculture") ~ "Agriculture",
        TRUE ~ NA_character_
      ),
      
      # Confidence intervals
      Lower = estimate - 1.96 * std.error,
      Upper = estimate + 1.96 * std.error
    ) %>%
    dplyr::relocate(model)
  
  return(coefs)
})

# Create levels to order variables for plotting 
coefs.out$Season <- factor(coefs.out$Season, levels = c(
  "Winter", 
  "Spring Movements", 
  "Pre-Nesting", 
  "Summer",
  "Fall"
))

# Create a primary roads coefficient object
# Filter out the intercept since that won't be plotted
coefs.out.primary <- coefs.out %>%
  dplyr::filter(term != "sd__(Intercept)") %>%
  dplyr::mutate(
    Effect = if_else(
      stringr::str_detect(term, ":"),
      "Interaction",
      "Main"
    )
  ) %>%
  dplyr::filter(Road_Type == "Primary") %>%
  dplyr::mutate(
    Landuse = dplyr::if_else(
      is.na(Landuse),
      "Distance to primary road",
      Landuse
    )
  )

# Create a secondary roads coefficient object
# Filter out the intercept
coefs.out.secondary <- coefs.out %>%
  dplyr::filter(term != "sd__(Intercept)") %>%
  dplyr::mutate(
    Effect = if_else(
      stringr::str_detect(term, ":"),
      "Interaction",
      "Main"
    )
  ) %>%
  dplyr::filter(Road_Type == "Secondary") %>%
  dplyr::mutate(
    Landuse = dplyr::if_else(
      is.na(Landuse),
      "Distance to secondary road",
      Landuse
    )
  )

# Create coefs.plot object for primary roads
# Rename Landuse to variable since Distance to primary road is a main effect
coefs.plot.primary <- coefs.out.primary %>%
  dplyr::mutate(
    Effect = factor(Effect, levels = c("Main", "Interaction"))
  ) %>%
  dplyr::rename("Variable" = Landuse)

# Create coefs.plot object for secondary roads
# Rename Landuse to variable since Distance to secondary road is a main effect
coefs.plot.secondary <- coefs.out.secondary %>%
  dplyr::mutate(
    Effect = factor(Effect, levels = c("Main", "Interaction"))
  ) %>%
  dplyr::rename("Variable" = Landuse)

################################################################################
## Distance to Primary Roads Beta plot

Plot.primary <- ggplot(
  coefs.plot.primary,
  aes(
    x = Effect,
    y = estimate,
    ymin = Lower,
    ymax = Upper,
    color = Variable,
    shape = Effect
  )
) +
  
  geom_pointrange(
    position = position_jitterdodge(
      jitter.width = 0.1,
      dodge.width  = 0.8
    ),
    alpha = 0.9
  ) +
  
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "gray40"
  ) +
  
  facet_wrap(~ Season) +
  
  labs(
    x = "Effect type",
    y = "Selection relative to hardwood forest",
    color = "Variable",
    shape = "Effect type"
  ) +
  
  scale_color_manual(values = c(
    "Conifer"     = "#183a37",
    "Agriculture" = "#fb8b24",
    "Developed"   = "#9a031e",
    "Distance to primary road"= "grey60"
  )) +
  
  scale_shape_manual(values = c(
    "Main"        = 15,
    "Interaction" = 18
  )) +
  
  guides(
    color = guide_legend(order = 1),
    shape = "none"
  ) +
  
  theme_light() +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    strip.text   = element_text(face = "bold"),
    legend.position ="bottom",
    legend.box = "vertical",
    legend.direction = "horizontal",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text  = element_text(size = 9)
  ) 
Plot.primary

################################################################################
## Distance to Secondary Road Beta plot

Plot.secondary <- ggplot(
  coefs.plot.secondary,
  aes(
    x = Effect,
    y = estimate,
    ymin = Lower,
    ymax = Upper,
    color = Variable,
    shape = Effect
  )
) +
  
  geom_pointrange(
    position = position_jitterdodge(
      jitter.width = 0.1,
      dodge.width  = 0.8
    ),
    alpha = 0.9
  ) +
  
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "gray40"
  ) +
  
  facet_wrap(~ Season) +
  
  labs(
    x = "Effect type",
    y = "Selection relative to hardwood forest",
    color = "Variable",
    shape = "Effect type"
  ) +
  
  scale_color_manual(values = c(
    "Conifer"     = "#183a37",
    "Agriculture" = "#fb8b24",
    "Developed"   = "#9a031e",
    "Distance to secondary road"= "grey60"
  )) +
  
  scale_shape_manual(values = c(
    "Main"        = 15,
    "Interaction" = 18
  )) +
  
  guides(
    color = guide_legend(order = 1),
    shape = "none"
  ) +
  
  theme_light() +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    strip.text   = element_text(face = "bold"),
    legend.position ="bottom",
    legend.box = "vertical",
    legend.direction = "horizontal",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text  = element_text(size = 9)
  ) 
Plot.secondary


################################################################################
###############################################################################X



# Covariates
# primary_covariate <- seq(-0.78, 3.2, 0.1) #ssf.final.winter %>% filter(tod_bin == "Morning") %>% pull(primary) %>% summary()
# developed_covariate <- c(0, 1) # 0 or 1
# 
# covariate_tbl <- expand.grid(primary_covariate, developed_covariate) %>% 
#   rename(primary = Var1, developed = Var2)
# 
# # no link function
# prediction_bootstrapped <- pmap(covariate_tbl, function(primary, developed){
#   #covariate mean and sd estimates come from the summary function
#   output <- -1 + primary*rnorm(10000, -0.25032, 0.86220) + developed*rnorm(10000, -0.13263, 0.05083) + primary*developed*rnorm(10000, 0.56920, 0.06716)
#   tibble(primary = primary, developed = developed, prediction = output) %>% 
#     return()
# }) %>% 
#   bind_rows()
# 
# ggplot(prediction_bootstrapped, aes(x = primary, y = prediction)) +
#   stat_lineribbon(.width = c(.95), alpha = 0.5, point_interval = mean_qi) +
#   facet_wrap(vars(developed)) +
#   theme_bw()
# 
# #link function
# prediction_bootstrapped_link <- pmap(covariate_tbl, function(primary, developed){
#   #covariate mean and sd estimates come from the summary function
#   output <- exp(-1 + primary*rnorm(10000, -0.25032, 0.86220) + developed*rnorm(10000, -0.13263, 0.05083) + primary*developed*rnorm(10000, 0.56920, 0.06716))
#   tibble(primary = primary, developed = developed, prediction = output) %>% 
#     return()
# }) %>% 
#   bind_rows()
# 
# ggplot(prediction_bootstrapped_link, aes(x = primary, y = prediction)) +
#   stat_lineribbon(.width = c(.95), alpha = 0.5, point_interval = mean_qi) +
#   facet_wrap(vars(developed)) +
#   ylab("Distance to Primary Road") +
#   xlab("Deviations from the Mean") +
#   theme_bw()
