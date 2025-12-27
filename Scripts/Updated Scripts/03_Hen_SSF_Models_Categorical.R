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
load("RData/Updated SP25/02RandomStepsTODLanduse_Ag_Weather.RData")

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

Win.Prim.Mor.TMB <- glmmTMB(
  case_ ~  
    -1 +                            
    primary + Developed + Agriculture +
    primary*Developed +  primary*Agriculture +
    (1 | str_ID),       
  family = poisson(),        
  data = ssf.final.winter %>% filter(tod_bin == "Morning"),   
  map = list(theta = factor(c(NA))),
  start = list(theta = c(log(1e3)))               
)             

summary(Win.Prim.Mor.TMB)

Win.Prim.Mid.TMB <- glmmTMB(
  case_ ~ 
    -1 +                                
    primary + Developed + Agriculture +
    primary*Developed +  primary*Agriculture +           
    (1 | str_ID),
  family = poisson(),                  
  data = ssf.final.winter %>% filter(tod_bin == "Midday"),  
  map = list(theta = factor(c(NA))),             
  start = list(theta = c(log(1e3)))               
)             

summary(Win.Prim.Mid.TMB)

Win.Prim.Aft.TMB <- glmmTMB(
  case_ ~ 
    -1 +                                
    primary + Developed + Agriculture +
    primary*Developed +  primary*Agriculture +               
    (1 | str_ID),
  family = poisson(),                  
  data = ssf.final.winter %>% filter(tod_bin == "Afternoon"),  
  map = list(theta = factor(c(NA))),              
  start = list(theta = c(log(1e3)))               
)             
summary(Win.Prim.Aft.TMB)

################################################################################
## Winter-- Secondary Roads

Win.Sec.Mor.TMB <- glmmTMB(
  case_ ~  
    -1 +                            
    secondary + Developed + Agriculture +
    secondary*Developed +  secondary*Agriculture +
    (1 | str_ID),       
  family = poisson(),        
  data = ssf.final.winter %>% filter(tod_bin == "Morning"),   
  map = list(theta = factor(c(NA))),
  start = list(theta = c(log(1e3)))               
)             

summary(Win.Sec.Mor.TMB)

Win.Sec.Mid.TMB <- glmmTMB(
  case_ ~ 
    -1 +                                
    secondary + Developed + Agriculture +
    secondary*Developed +  secondary*Agriculture +      
    (1 | str_ID),
  family = poisson(),                  
  data = ssf.final.winter %>% filter(tod_bin == "Midday"),  
  map = list(theta = factor(c(NA))),             
  start = list(theta = c(log(1e3)))               
)             

summary(Win.Sec.Mid.TMB)

Win.Sec.Aft.TMB <- glmmTMB(
  case_ ~ 
    -1 +                                
    secondary + Developed + Agriculture +
    secondary*Developed +  secondary*Agriculture +         
    (1 | str_ID),
  family = poisson(),                  
  data = ssf.final.winter %>% filter(tod_bin == "Afternoon"),  
  map = list(theta = factor(c(NA))),              
  start = list(theta = c(log(1e3)))               
)             
summary(Win.Sec.Aft.TMB)


################################################################################
## Spring Movements-- Primary Roads

Spr.Prim.Mor.TMB <- glmmTMB(
  case_ ~  
    -1 +                            
    primary + Developed + Agriculture +
    primary*Developed +  primary*Agriculture + 
    (1 | str_ID),       
  family = poisson(),        
  data = ssf.final.spring %>% filter(tod_bin == "Morning"),   
  map = list(theta = factor(c(NA))),
  start = list(theta = c(log(1e3)))               
)             

summary(Spr.Prim.Mor.TMB)

Spr.Prim.Mid.TMB <- glmmTMB(
  case_ ~ 
    -1 +                                
    primary + Developed + Agriculture +
    primary*Developed +  primary*Agriculture +       
    (1 | str_ID),
  family = poisson(),                  
  data = ssf.final.spring %>% filter(tod_bin == "Midday"),  
  map = list(theta = factor(c(NA))),             
  start = list(theta = c(log(1e3)))               
)             

summary(Spr.Prim.Mid.TMB)

Spr.Prim.Aft.TMB <- glmmTMB(
  case_ ~ 
    -1 +   primary + Developed + Agriculture +
    primary*Developed +  primary*Agriculture +                               
    (1 | str_ID),
  family = poisson(),                  
  data = ssf.final.spring %>% filter(tod_bin == "Afternoon"),  
  map = list(theta = factor(c(NA))),              
  start = list(theta = c(log(1e3)))               
)             
summary(Spr.Prim.Aft.TMB)

################################################################################
## Spring Movements-- Secondary Roads

Spr.Sec.Mor.TMB <- glmmTMB(
  case_ ~  
    -1 +                            
    secondary + Developed + Agriculture +
    secondary*Developed + secondary*Agriculture + 
    (1 | str_ID),       
  family = poisson(),        
  data = ssf.final.spring %>% filter(tod_bin == "Morning"),   
  map = list(theta = factor(c(NA))),
  start = list(theta = c(log(1e3)))               
)             

summary(Spr.Sec.Mor.TMB)

Spr.Sec.Mid.TMB <- glmmTMB(
  case_ ~ 
    -1 +                                
    secondary + Developed + Agriculture +
    secondary*Developed + secondary*Agriculture +     
    (1 | str_ID),
  family = poisson(),                  
  data = ssf.final.spring %>% filter(tod_bin == "Midday"),  
  map = list(theta = factor(c(NA))),             
  start = list(theta = c(log(1e3)))               
)             

summary(Spr.Sec.Mid.TMB)

Spr.Sec.Aft.TMB <- glmmTMB(
  case_ ~ 
    -1 +   secondary + Developed + Agriculture +
    secondary*Developed + secondary*Agriculture +                              
    (1 | str_ID),
  family = poisson(),                  
  data = ssf.final.spring %>% filter(tod_bin == "Afternoon"),  
  map = list(theta = factor(c(NA))),              
  start = list(theta = c(log(1e3)))               
)             
summary(Spr.Sec.Aft.TMB)

################################################################################
## Pre-Nesting-- Primary Roads

Pre.Prim.Mor.TMB <- glmmTMB(
  case_ ~  
    -1 +                            
    primary + Developed + Agriculture +
    primary*Developed +  primary*Agriculture + 
    (1 | str_ID),       
  family = poisson(),        
  data = ssf.final.pre %>% filter(tod_bin == "Morning"),   
  map = list(theta = factor(c(NA))),
  start = list(theta = c(log(1e3)))               
)             

summary(Pre.Prim.Mor.TMB)

Pre.Prim.Mid.TMB <- glmmTMB(
  case_ ~ 
    -1 +                                
    primary + Developed + Agriculture +
    primary*Developed +  primary*Agriculture +       
    (1 | str_ID),
  family = poisson(),                  
  data = ssf.final.pre %>% filter(tod_bin == "Midday"),  
  map = list(theta = factor(c(NA))),             
  start = list(theta = c(log(1e3)))               
)             

summary(Pre.Prim.Mid.TMB)

Pre.Prim.Aft.TMB <- glmmTMB(
  case_ ~ 
    -1 +   primary + Developed + Agriculture +
    primary*Developed +  primary*Agriculture +                               
    (1 | str_ID),
  family = poisson(),                  
  data = ssf.final.pre %>% filter(tod_bin == "Afternoon"),  
  map = list(theta = factor(c(NA))),              
  start = list(theta = c(log(1e3)))               
)             

summary(Pre.Prim.Aft.TMB)

################################################################################
## Pre-Nesting-- Secondary Roads

Pre.Sec.Mor.TMB <- glmmTMB(
  case_ ~  
    -1 +                            
    secondary + Developed + Agriculture +
    secondary*Developed + secondary*Agriculture +  
    (1 | str_ID),       
  family = poisson(),        
  data = ssf.final.pre %>% filter(tod_bin == "Morning"),   
  map = list(theta = factor(c(NA))),
  start = list(theta = c(log(1e3)))               
)             

summary(Pre.Sec.Mor.TMB)

Pre.Sec.Mid.TMB <- glmmTMB(
  case_ ~ 
    -1 +                                
    secondary + Developed + Agriculture +
    secondary*Developed + secondary*Agriculture +         
    (1 | str_ID),
  family = poisson(),                  
  data = ssf.final.pre %>% filter(tod_bin == "Midday"),  
  map = list(theta = factor(c(NA))),             
  start = list(theta = c(log(1e3)))               
)             

summary(Pre.Sec.Mid.TMB)

Pre.Sec.Aft.TMB <- glmmTMB(
  case_ ~ 
    -1 + secondary + Developed + Agriculture +
    secondary*Developed + secondary*Agriculture +                               
    (1 | str_ID),
  family = poisson(),                  
  data = ssf.final.pre %>% filter(tod_bin == "Afternoon"),  
  map = list(theta = factor(c(NA))),              
  start = list(theta = c(log(1e3)))               
)             

summary(Pre.Sec.Aft.TMB)

################################################################################
## Summer-- Primary Roads

Sum.Prim.Mor.TMB <- glmmTMB(
  case_ ~  
    -1 +                            
    primary + Developed + Agriculture +
    primary*Developed +  primary*Agriculture + 
    (1 | str_ID),       
  family = poisson(),        
  data = ssf.final.sum %>% filter(tod_bin == "Morning"),   
  map = list(theta = factor(c(NA))),
  start = list(theta = c(log(1e3)))               
)             

summary(Sum.Prim.Mor.TMB)

Sum.Prim.Mid.TMB <- glmmTMB(
  case_ ~ 
    -1 +                                
    primary + Developed + Agriculture +
    primary*Developed +  primary*Agriculture +       
    (1 | str_ID),
  family = poisson(),                  
  data = ssf.final.sum %>% filter(tod_bin == "Midday"),  
  map = list(theta = factor(c(NA))),             
  start = list(theta = c(log(1e3)))               
)             

summary(Sum.Prim.Mid.TMB)

Sum.Prim.Aft.TMB <- glmmTMB(
  case_ ~ 
    -1 +   primary + Developed + Agriculture +
    primary*Developed +  primary*Agriculture +                               
    (1 | str_ID),
  family = poisson(),                  
  data = ssf.final.sum %>% filter(tod_bin == "Afternoon"),  
  map = list(theta = factor(c(NA))),              
  start = list(theta = c(log(1e3)))               
)             

summary(Sum.Prim.Aft.TMB)

################################################################################
## Summer-- Secondary Roads

Sum.Sec.Mor.TMB <- glmmTMB(
  case_ ~  
    -1 +                            
    secondary + Developed + Agriculture +
    secondary*Developed + secondary*Agriculture +  
    (1 | str_ID),       
  family = poisson(),        
  data = ssf.final.sum %>% filter(tod_bin == "Morning"),   
  map = list(theta = factor(c(NA))),
  start = list(theta = c(log(1e3)))               
)             

summary(Sum.Sec.Mor.TMB)

Sum.Sec.Mid.TMB <- glmmTMB(
  case_ ~ 
    -1 +                                
    secondary + Developed + Agriculture +
    secondary*Developed + secondary*Agriculture +         
    (1 | str_ID),
  family = poisson(),                  
  data = ssf.final.sum %>% filter(tod_bin == "Midday"),  
  map = list(theta = factor(c(NA))),             
  start = list(theta = c(log(1e3)))               
)             

summary(Sum.Sec.Mid.TMB)

Sum.Sec.Aft.TMB <- glmmTMB(
  case_ ~ 
    -1 + secondary + Developed + Agriculture +
    secondary*Developed + secondary*Agriculture +                               
    (1 | str_ID),
  family = poisson(),                  
  data = ssf.final.sum %>% filter(tod_bin == "Afternoon"),  
  map = list(theta = factor(c(NA))),              
  start = list(theta = c(log(1e3)))               
)             

summary(Sum.Sec.Aft.TMB)

################################################################################
## Organize Data

models <- list(
  
  Win.Prim.Mor = Win.Prim.Mor.TMB,
  Win.Prim.Mid = Win.Prim.Mid.TMB,
  Win.Prim.Aft = Win.Prim.Aft.TMB,
  
  Win.Sec.Mor  = Win.Sec.Mor.TMB,
  Win.Sec.Mid  = Win.Sec.Mid.TMB,
  Win.Sec.Aft  = Win.Sec.Aft.TMB,
  
  Spr.Prim.Mor = Spr.Prim.Mor.TMB,
  Spr.Prim.Mid = Spr.Prim.Mid.TMB,
  Spr.Prim.Aft = Spr.Prim.Aft.TMB,
  
  Spr.Sec.Mor  = Spr.Sec.Mor.TMB,
  Spr.Sec.Mid  = Spr.Sec.Mid.TMB,
  Spr.Sec.Aft  = Spr.Sec.Aft.TMB,
  
  Pre.Prim.Mor = Pre.Prim.Mor.TMB,
  Pre.Prim.Mid = Pre.Prim.Mid.TMB,
  Pre.Prim.Aft = Pre.Prim.Aft.TMB,
  
  Pre.Sec.Mor  = Pre.Sec.Mor.TMB,
  Pre.Sec.Mid  = Pre.Sec.Mid.TMB,
  Pre.Sec.Aft  = Pre.Sec.Aft.TMB,
  
  Sum.Prim.Mor = Sum.Prim.Mor.TMB,
  Sum.Prim.Mid = Sum.Prim.Mid.TMB,
  Sum.Prim.Aft = Sum.Prim.Aft.TMB,
  
  Sum.Sec.Mor  = Sum.Sec.Mor.TMB,
  Sum.Sec.Mid  = Sum.Sec.Mid.TMB,
  Sum.Sec.Aft  = Sum.Sec.Aft.TMB
  
  # Fall - Primary Roads
  # Fall.Prim.Mor.Dev.TMB = Fall.Prim.Mor.Dev.TMB,
  # Fall.Prim.Mid.Dev.TMB = Fall.Prim.Mid.Dev.TMB,
  # 
  # Fall.Prim.Mor.For.TMB = Fall.Prim.Mor.For.TMB,
  # Fall.Prim.Mid.For.TMB = Fall.Prim.Mid.For.TMB,
  # 
  # Fall.Prim.Mor.Open.TMB = Fall.Prim.Mor.Open.TMB,
  # Fall.Prim.Mid.Open.TMB = Fall.Prim.Mid.Open.TMB,
  # 
  # # Fall - Secondary Roads
  # Fall.Sec.Mor.Dev.TMB = Fall.Sec.Mor.Dev.TMB,
  # Fall.Sec.Mid.Dev.TMB = Fall.Sec.Mid.Dev.TMB,
  # 
  # Fall.Sec.Mor.For.TMB = Fall.Sec.Mor.For.TMB,
  # Fall.Sec.Mid.For.TMB = Fall.Sec.Mid.For.TMB,
  # 
  # Fall.Sec.Mor.Open.TMB = Fall.Sec.Mor.Open.TMB,
  # Fall.Sec.Mid.Open.TMB = Fall.Sec.Mid.Open.TMB
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
        Season == "Fal" ~ "Fall",
        TRUE ~ NA_character_
      ),
      
      # Road Type
      Road_Type = stringr::str_extract(model_name, "(?<=\\.)[^.]+(?=\\.)"),
      Road_Type = dplyr::case_when(
        Road_Type == "Prim" ~ "Primary",
        Road_Type == "Sec" ~ "Secondary",
        TRUE ~ NA_character_
      ),
      
      # Time of Day
      Time_of_Day = stringr::str_split(model_name, "\\.", simplify = TRUE)[3],
      Time_of_Day = dplyr::case_when(
        Time_of_Day == "Mor" ~ "Morning",
        Time_of_Day == "Mid" ~ "Midday",
        Time_of_Day == "Aft" ~ "Afternoon",
        TRUE ~ NA_character_
      ),
      
      # Landuse
      Landuse = dplyr::case_when(
        stringr::str_detect(term, "Developed") ~ "Developed",
        stringr::str_detect(term, "Forest") ~ "Forest",
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


coefs.out$Season <- factor(coefs.out$Season, levels = c(
  "Winter", 
  "Spring Movements", 
  "Pre-Nesting", 
  "Summer",
  "Fall"
))

ggplot(coefs.out %>%
         dplyr::filter(Season != "Fall") %>%
         dplyr::filter(stringr::str_detect(term, ":")) %>%
         dplyr::mutate(Time_of_Day = factor(Time_of_Day, levels = c("Morning", "Midday", "Afternoon"))),  
       aes(x = Time_of_Day, y = estimate, ymin = Lower, ymax = Upper, color = Landuse, shape = Road_Type)) +
  
  geom_pointrange(
    position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8),
    alpha = 0.9
  ) +
  
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  
  facet_wrap(~ Season) +
  
  labs(
    x = "Time of day",
    y = "Selection relative to forest",
    color = "Land cover type",
    shape = "Road type"  
  ) +
  
  scale_color_manual(values = c(
    "Forest" = "#606c38",
    "Agriculture" = "#Bc6c25",
    "Developed" = "brown"
  )) +
  
  scale_shape_manual(values = c(
    "Primary" = 16,    
    "Secondary" = 17   
  )) +
  
  guides(
    color = guide_legend(order = 1),
    shape = guide_legend(order = 2)
  ) +
  
  theme_light() +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    legend.position = "right",
    legend.box = "vertical",
    legend.direction = "vertical",
    legend.title = element_text(size = 10, face = "bold"),  
    legend.text = element_text(size = 9)
  )

################################################################################
## Creating Prediction Plots

# Covariates
primary_covariate <- seq(-0.78, 3.2, 0.1) #ssf.final.winter %>% filter(tod_bin == "Morning") %>% pull(primary) %>% summary()
developed_covariate <- c(0, 1) # 0 or 1

covariate_tbl <- expand.grid(primary_covariate, developed_covariate) %>% 
  rename(primary = Var1, developed = Var2)

# no link function
prediction_bootstrapped <- pmap(covariate_tbl, function(primary, developed){
  #covariate mean and sd estimates come from the summary function
  output <- -1 + primary*rnorm(10000, -0.25032, 0.86220) + developed*rnorm(10000, -0.13263, 0.05083) + primary*developed*rnorm(10000, 0.56920, 0.06716)
  tibble(primary = primary, developed = developed, prediction = output) %>% 
    return()
}) %>% 
  bind_rows()

ggplot(prediction_bootstrapped, aes(x = primary, y = prediction)) +
  stat_lineribbon(.width = c(.95), alpha = 0.5, point_interval = mean_qi) +
  facet_wrap(vars(developed)) +
  theme_bw()

#link function
prediction_bootstrapped_link <- pmap(covariate_tbl, function(primary, developed){
  #covariate mean and sd estimates come from the summary function
  output <- exp(-1 + primary*rnorm(10000, -0.25032, 0.86220) + developed*rnorm(10000, -0.13263, 0.05083) + primary*developed*rnorm(10000, 0.56920, 0.06716))
  tibble(primary = primary, developed = developed, prediction = output) %>% 
    return()
}) %>% 
  bind_rows()

ggplot(prediction_bootstrapped_link, aes(x = primary, y = prediction)) +
  stat_lineribbon(.width = c(.95), alpha = 0.5, point_interval = mean_qi) +
  facet_wrap(vars(developed)) +
  ylab("Distance to Primary Road") +
  xlab("Deviations from the Mean") +
  theme_bw()
