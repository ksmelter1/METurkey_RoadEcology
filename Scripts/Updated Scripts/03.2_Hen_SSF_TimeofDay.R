#'---
#' title: Context-Dependent Habitat selection of Roads by female wild turkeys
#' authors: "K. Smelter
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#'---
#'
knitr::opts_chunk$set(warning = FALSE, message = FALSE, cache = TRUE)
#'  
#' **Purpose**: This script creates time of day models (Objective 2)
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

Win.Prim.Mor.TMB <- glmmTMB(
  case_ ~  
    -1 +                            
    primary +
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
    primary +      
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
    primary +         
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
    secondary +
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
    secondary +
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
    secondary +       
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
    primary +
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
    primary +      
    (1 | str_ID),
  family = poisson(),                  
  data = ssf.final.spring %>% filter(tod_bin == "Midday"),  
  map = list(theta = factor(c(NA))),             
  start = list(theta = c(log(1e3)))               
)             

summary(Spr.Prim.Mid.TMB)

Spr.Prim.Aft.TMB <- glmmTMB(
  case_ ~ 
    -1 +   
    primary +
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
    secondary + 
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
    secondary +   
    (1 | str_ID),
  family = poisson(),                  
  data = ssf.final.spring %>% filter(tod_bin == "Midday"),  
  map = list(theta = factor(c(NA))),             
  start = list(theta = c(log(1e3)))               
)             

summary(Spr.Sec.Mid.TMB)

Spr.Sec.Aft.TMB <- glmmTMB(
  case_ ~ 
    -1 +   
    secondary +  
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
    primary +
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
    primary +   
    (1 | str_ID),
  family = poisson(),                  
  data = ssf.final.pre %>% filter(tod_bin == "Midday"),  
  map = list(theta = factor(c(NA))),             
  start = list(theta = c(log(1e3)))               
)             

summary(Pre.Prim.Mid.TMB)

Pre.Prim.Aft.TMB <- glmmTMB(
  case_ ~ 
    -1 +   
    primary +
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
    secondary +  
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
    secondary +          
    (1 | str_ID),
  family = poisson(),                  
  data = ssf.final.pre %>% filter(tod_bin == "Midday"),  
  map = list(theta = factor(c(NA))),             
  start = list(theta = c(log(1e3)))               
)             

summary(Pre.Sec.Mid.TMB)

Pre.Sec.Aft.TMB <- glmmTMB(
  case_ ~ 
    -1 + 
    secondary +
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
    primary +
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
    primary +   
    (1 | str_ID),
  family = poisson(),                  
  data = ssf.final.sum %>% filter(tod_bin == "Midday"),  
  map = list(theta = factor(c(NA))),             
  start = list(theta = c(log(1e3)))               
)             

summary(Sum.Prim.Mid.TMB)

Sum.Prim.Aft.TMB <- glmmTMB(
  case_ ~ 
    -1 +   
    primary +
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
    secondary +   
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
    secondary +          
    (1 | str_ID),
  family = poisson(),                  
  data = ssf.final.sum %>% filter(tod_bin == "Midday"),  
  map = list(theta = factor(c(NA))),             
  start = list(theta = c(log(1e3)))               
)             

summary(Sum.Sec.Mid.TMB)

Sum.Sec.Aft.TMB <- glmmTMB(
  case_ ~ 
    -1 + 
    secondary +   
    (1 | str_ID),
  family = poisson(),                  
  data = ssf.final.sum %>% filter(tod_bin == "Afternoon"),  
  map = list(theta = factor(c(NA))),              
  start = list(theta = c(log(1e3)))               
)             

summary(Sum.Sec.Aft.TMB)

# ################################################################################
# ## Fall-- Primary Roads
# 
# Fall.Prim.Mor.TMB <- glmmTMB(
#   case_ ~  
#     -1 +                            
#     primary +
#     (1 | str_ID),       
#   family = poisson(),        
#   data = ssf.final.fall %>% filter(tod_bin == "Morning"),   
#   map = list(theta = factor(c(NA))),
#   start = list(theta = c(log(1e3)))               
# )             
# 
# summary(Fall.Prim.Mor.TMB)
# confint(Fall.Prim.Mor.TMB)
# 
# Fall.Prim.Mid.TMB <- glmmTMB(
#   case_ ~ 
#     -1 +                                
#     primary +   
#     (1 | str_ID),
#   family = poisson(),                  
#   data = ssf.final.fall %>% filter(tod_bin == "Midday"),  
#   map = list(theta = factor(c(NA))),             
#   start = list(theta = c(log(1e3)))               
# )             
# 
# summary(Fall.Prim.Mid.TMB)
# confint(Fall.Prim.Mid.TMB)
# 
# Fall.Prim.Aft.TMB <- glmmTMB(
#   case_ ~ 
#     -1 +   
#     primary +
#     (1 | str_ID),
#   family = poisson(),                  
#   data = ssf.final.fall %>% filter(tod_bin == "Afternoon"),  
#   map = list(theta = factor(c(NA))),              
#   start = list(theta = c(log(1e3)))               
# )             
# 
# summary(Fall.Prim.Aft.TMB)
# confint(Fall.Prim.Aft.TMB)
# 
# ################################################################################
# ## Fall-- Secondary Roads
# 
# Fall.Sec.Mor.TMB <- glmmTMB(
#   case_ ~  
#     -1 +                            
#     secondary +   
#     (1 | str_ID),       
#   family = poisson(),        
#   data = ssf.final.fall %>% filter(tod_bin == "Morning"),   
#   map = list(theta = factor(c(NA))),
#   start = list(theta = c(log(1e3)))               
# )             
# 
# summary(Fall.Sec.Mor.TMB)
# confint(Fall.Sec.Mor.TMB)
# 
# Fall.Sec.Mid.TMB <- glmmTMB(
#   case_ ~ 
#     -1 +                                
#     secondary +          
#     (1 | str_ID),
#   family = poisson(),                  
#   data = ssf.final.fall %>% filter(tod_bin == "Midday"),  
#   map = list(theta = factor(c(NA))),             
#   start = list(theta = c(log(1e3)))               
# )             
# 
# summary(Fall.Sec.Mid.TMB)
# confint(Fall.Sec.Mid.TMB)
# 
# Fall.Sec.Aft.TMB <- glmmTMB(
#   case_ ~ 
#     -1 + 
#     secondary +   
#     (1 | str_ID),
#   family = poisson(),                  
#   data = ssf.final.fall %>% filter(tod_bin == "Afternoon"),  
#   map = list(theta = factor(c(NA))),              
#   start = list(theta = c(log(1e3)))               
# )             
# 
# summary(Fall.Sec.Aft.TMB)
# confint(Fall.Sec.Aft.TMB)


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
  
  # Fall.Prim.Mor =  Fall.Prim.Mor.TMB,
  # Fall.Prim.Mid =  Fall.Prim.Mid.TMB,
  # Fall.Prim.Aft =  Fall.Prim.Aft.TMB,
  # 
  # Fall.Sec.Mor  =  Fall.Sec.Mor.TMB,
  # Fall.Sec.Mid  =  Fall.Sec.Mid.TMB,
  # Fall.Sec.Aft  =  Fall.Sec.Aft.TMB,
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
        #Season == "Fal" ~ "Fall"
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
         drop_na() %>%
         dplyr::mutate(Time_of_Day = factor(Time_of_Day, levels = c("Morning", "Midday", "Afternoon"))),  
       aes(x = Time_of_Day, y = estimate, ymin = Lower, ymax = Upper, colour = Road_Type, shape = Road_Type)) +
  
  geom_pointrange(
    position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8),
    alpha = 0.9
  ) +
  
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5, color = "black") +
  
  facet_wrap(~ Season) +
  
  labs(
    x = "Time of day",
    y = "Probability of selection",
    color = "Land cover type",
    shape = "Road Type"  
  ) +
  
  scale_color_manual(
    values = c(
      "Primary"   = "#1b9e77",
      "Secondary" = "#d95f02"
    ),
    name = "Road Type"
  ) +
  
  scale_shape_manual(values = c(
    "Primary" = 16,    
    "Secondary" = 17   
  )) +
  theme_light(base_size = 12) +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 10, face = "bold"),  
    legend.text = element_text(size = 9),
  )

################################################################################
###############################################################################X
