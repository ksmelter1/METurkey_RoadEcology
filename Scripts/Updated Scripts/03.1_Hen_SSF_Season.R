#'---
# title: Context-Dependent Habitat selection of Roads by female wild turkeys
#' authors: "K. Smelter
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#' output:
#'   html_document: 
#'     toc: true
#'---
#'  
#' **Purpose**: This script creates season models (Objective 1)
#' **Last Updated**: 12/27/25
#' 

################################################################################
## Load Packages and Data

library(tidyverse)
library(glmmTMB)

load("RData/Updated SP25/02RandomStepsTODLanduse_Ag_Weather_Manuscript.RData")

################################################################################
## Winter

Win.Prim.TMB <- glmmTMB(
  case_ ~ 
    -1 +                                
    primary +
    (1 | str_ID),                 
  family = poisson(),                  
  data = ssf.final.winter,
  map = list(theta = factor(c(NA))),              
  start = list(theta = c(log(1e3)))               
) 
summary(Win.Prim.TMB)
confint(Win.Prim.TMB)

Win.Sec.TMB <- glmmTMB(
  case_ ~ 
    -1 +                                
    secondary + 
    (1 | str_ID),
  family = poisson(),                  
  data = ssf.final.winter,   
  map = list(theta = factor(c(NA))),              
  start = list(theta = c(log(1e3)))               
) 
summary(Win.Sec.TMB)
confint(Win.Sec.TMB)


################################################################################
## Spring Movements


Spr.Prim.TMB <- glmmTMB(
  case_ ~ 
    -1 +                                
    primary + 
    (1 | str_ID),                    
  family = poisson(),                  
  data = ssf.final.spring, 
  map = list(theta = factor(c(NA))),              
  start = list(theta = c(log(1e3)))               
) 
summary(Spr.Prim.TMB)
confint(Spr.Prim.TMB)

Spr.Sec.TMB <- glmmTMB(
  case_ ~ 
    -1 +                                
    secondary + 
    (1 | str_ID),
  family = poisson(),                  
  data = ssf.final.spring, 
  map = list(theta = factor(c(NA))),              
  start = list(theta = c(log(1e3)))               
) 
summary(Spr.Sec.TMB)
confint(Spr.Sec.TMB)

################################################################################
## Pre-Nesting


Pre.Prim.TMB <- glmmTMB(
  case_ ~ 
    -1 +                                
    primary +  
    (1 | str_ID),
  family = poisson(),                  
  data = ssf.final.pre, 
  map = list(theta = factor(c(NA))),              
  start = list(theta = c(log(1e3)))               
) 
summary(Pre.Prim.TMB)
confint(Pre.Prim.TMB)

Pre.Sec.TMB <- glmmTMB(
  case_ ~ 
    -1 +                                
    secondary + 
    (1 | str_ID),
  family = poisson(),                  
  data = ssf.final.pre, 
  map = list(theta = factor(c(NA))),              
  start = list(theta = c(log(1e3)))               
) 
summary(Pre.Sec.TMB)
confint(Pre.Sec.TMB)

################################################################################
## Summer

Sum.Prim.TMB <- glmmTMB(
  case_ ~ 
    -1 +                                
    primary + 
    (1 | str_ID),
  family = poisson(),                  
  data = ssf.final.sum, 
  map = list(theta = factor(c(NA))),              
  start = list(theta = c(log(1e3)))               
) 
summary(Sum.Prim.TMB)
confint(Sum.Prim.TMB)

Sum.Sec.TMB <- glmmTMB(
  case_ ~ 
    -1 +                                
    secondary + 
    (1 | str_ID),                     
  family = poisson(),                  
  data = ssf.final.sum,  
  map = list(theta = factor(c(NA))),              
  start = list(theta = c(log(1e3)))               
) 
summary(Sum.Sec.TMB)
confint(Sum.Sec.TMB)

################################################################################
## Fall

Fall.Prim.TMB <- glmmTMB(
  case_ ~ 
    -1 +                                
    primary + 
    (1 | str_ID),
  family = poisson(),                  
  data = ssf.final.fall, 
  map = list(theta = factor(c(NA))),              
  start = list(theta = c(log(1e3)))               
) 
summary(Fall.Prim.TMB)
confint(Fall.Prim.TMB)

Fall.Sec.TMB <- glmmTMB(
  case_ ~ 
    -1 +                                
    secondary + 
    (1 | str_ID),                     
  family = poisson(),                  
  data = ssf.final.fall,  
  map = list(theta = factor(c(NA))),              
  start = list(theta = c(log(1e3)))               
) 
summary(Fall.Sec.TMB)
confint(Fall.Sec.TMB)

################################################################################
## Data Prep

models_roads <- list(
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

coefs.roads <- purrr::map_df(names(models_roads), function(model_name) {
  
  model <- models_roads[[model_name]]
  
  broom.mixed::tidy(model) %>%
    dplyr::filter(term != "(Intercept)") %>%
    dplyr::select(term, estimate, std.error) %>%
    dplyr::mutate(
      
      model = model_name,
      
      # Season
      Season = stringr::str_sub(model_name, 1, 3),
      Season = dplyr::case_when(
        Season == "Win" ~ "Winter",
        Season == "Spr" ~ "Spring Movements",
        Season == "Pre" ~ "Pre-Nesting",
        Season == "Sum" ~ "Summer",
        Season == "Fal" ~ "Fall"
      ),
      
      # Road type
      Road_Type = stringr::str_extract(model_name, "(?<=\\.)[^.]+(?=\\.)"),
      Road_Type = dplyr::case_when(
        Road_Type == "Prim" ~ "Primary",
        Road_Type == "Sec"  ~ "Secondary"
      ),
      
      # 95% CI
      Lower = estimate - 1.96 * std.error,
      Upper = estimate + 1.96 * std.error
    )
})  %>%
  drop_na()

coefs.roads <- coefs.roads %>%
  dplyr::mutate(
    RSS   = exp(estimate),
    RSS_L = exp(Lower),
    RSS_U = exp(Upper)
  )


coefs.roads$Season <- factor(
  coefs.roads$Season,
  levels = c("Winter", "Spring Movements", "Pre-Nesting", "Summer", "Fall")
)

ggplot(
  coefs.roads,
  aes(
    x = Road_Type,
    y = estimate,
    ymin = Lower,
    ymax = Upper,
    color = Road_Type,
    shape = Road_Type
  )
) +
  
  geom_pointrange(
    position = position_dodge(width = 0.4),
    linewidth = 0.7
  ) +
  
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "black"
  ) +
  
  facet_wrap(~ Season) +
  
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
  ),
  name = "Road Type"
  ) +
  
  labs(
    x = NULL,
    y = NULL
  ) +
  
  theme_light(base_size = 12) +
  theme(
    axis.title.y = element_text(face = "bold"),
    axis.text.x  = element_blank(),
    axis.ticks.x  = element_blank(),
    strip.text   = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 10, face = "bold")
  ) 

################################################################################
###############################################################################X