#'---
# title: Context-Dependent Habitat selection of Roads by female wild turkeys
#' authors: "K. Smelter
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#' output:
#'   html_document: 
#'     toc: true
#'---
#'  
#' **Purpose**: This script creates weather and individual covariate models
#' **Last Updated**: 12/27/25

################################################################################
## Load Packages and Data

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


################################################################################
## Winter

Win.Prim.Weath.TMB <- glmmTMB(
  case_ ~ 
    -1 +                                
    primary + primary:tmin + primary:precip + primary:swe  +
    (1 | str_ID),                 
  family = poisson(),                  
  data = ssf.final.winter,
  map = list(theta = factor(c(NA))),              
  start = list(theta = c(log(1e3)))               
) 
summary(Win.Prim.Weath.TMB)
confint(Win.Prim.Weath.TMB)

Win.Sec.Weath.TMB <- glmmTMB(
  case_ ~ 
    -1 +                                
    secondary + secondary:tmin + secondary:precip + secondary:swe +
    (1 | str_ID),
  family = poisson(),                  
  data = ssf.final.winter,   
  map = list(theta = factor(c(NA))),              
  start = list(theta = c(log(1e3)))               
) 
summary(Win.Sec.Weath.TMB)


################################################################################
## Spring Movements


Spr.Prim.Weath.TMB <- glmmTMB(
  case_ ~ 
    -1 +                                
    primary + primary:tmin + primary:precip + 
    (1 | str_ID),                    
  family = poisson(),                  
  data = ssf.final.spring, 
  map = list(theta = factor(c(NA))),              
  start = list(theta = c(log(1e3)))               
) 
summary(Spr.Prim.Weath.TMB)
confint(Spr.Prim.Weath.TMB)

Spr.Sec.Weath.TMB <- glmmTMB(
  case_ ~ 
    -1 +                                
    secondary + secondary:tmin + secondary:precip +
    (1 | str_ID),
  family = poisson(),                  
  data = ssf.final.spring, 
  map = list(theta = factor(c(NA))),              
  start = list(theta = c(log(1e3)))               
) 
summary(Spr.Sec.Weath.TMB)
confint(Spr.Sec.Weath.TMB)

################################################################################
## Pre-Nesting


Pre.Prim.Weath.TMB <- glmmTMB(
  case_ ~ 
    -1 +                                
    primary + primary:tmin + primary:precip + 
    (1 | str_ID),
  family = poisson(),                  
  data = ssf.final.pre, 
  map = list(theta = factor(c(NA))),              
  start = list(theta = c(log(1e3)))               
) 
summary(Pre.Prim.Weath.TMB)
confint(Pre.Prim.Weath.TMB)

Pre.Sec.Weath.TMB <- glmmTMB(
  case_ ~ 
    -1 +                                
    secondary + secondary:tmin + secondary:precip + 
    (1 | str_ID),
  family = poisson(),                  
  data = ssf.final.pre, 
  map = list(theta = factor(c(NA))),              
  start = list(theta = c(log(1e3)))               
) 
summary(Pre.Sec.Weath.TMB)
confint(Pre.Sec.Weath.TMB)

################################################################################
## Summer

Sum.Prim.Weath.TMB <- glmmTMB(
  case_ ~ 
    -1 +                                
    primary + primary:tmin + primary:precip + 
    (1 | str_ID),
  family = poisson(),                  
  data = ssf.final.sum, 
  map = list(theta = factor(c(NA))),              
  start = list(theta = c(log(1e3)))               
) 
summary(Sum.Prim.Weath.TMB)
confint(Sum.Prim.Weath.TMB)

Sum.Sec.Weath.TMB <- glmmTMB(
  case_ ~ 
    -1 +                                
    secondary + secondary:tmin + secondary:precip +
    (1 | str_ID),                     
  family = poisson(),                  
  data = ssf.final.sum,  
  map = list(theta = factor(c(NA))),              
  start = list(theta = c(log(1e3)))               
) 
summary(Sum.Sec.Weath.TMB)
confint(Sum.Sec.Weath.TMB)

################################################################################
## Collect weather models

models_weather <- list(
  Win.Prim.Weath.TMB = Win.Prim.Weath.TMB,
  Win.Sec.Weath.TMB  = Win.Sec.Weath.TMB,
  
  Spr.Prim.Weath.TMB = Spr.Prim.Weath.TMB,
  Spr.Sec.Weath.TMB  = Spr.Sec.Weath.TMB,
  
  Pre.Prim.Weath.TMB = Pre.Prim.Weath.TMB,
  Pre.Sec.Weath.TMB  = Pre.Sec.Weath.TMB,
  
  Sum.Prim.Weath.TMB = Sum.Prim.Weath.TMB,
  Sum.Sec.Weath.TMB  = Sum.Sec.Weath.TMB
)


################################################################################
## Extract both main effects and interactions

coefs.weather.all <- purrr::map_df(names(models_weather), function(model_name) {
  model <- models_weather[[model_name]]
  
  broom.mixed::tidy(model) %>%
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
        TRUE ~ NA_character_
      ),
      
      # Road Type
      Road_Type = stringr::str_extract(model_name, "(?<=\\.)[^.]+(?=\\.)"),
      Road_Type = dplyr::case_when(
        Road_Type == "Prim" ~ "Primary",
        Road_Type == "Sec"  ~ "Secondary",
        TRUE ~ NA_character_
      ),
      
      # Effect type (main vs interaction)
      Effect_Type = dplyr::case_when(
        stringr::str_detect(term, ":") ~ "Interaction",
        TRUE ~ "Main"
      ),
      
      # Weather variable (if interaction)
      Weather = dplyr::case_when(
        stringr::str_detect(term, "tmin") ~ "Temperature (Tmin)",
        stringr::str_detect(term, "prcp") ~ "Precipitation",
        stringr::str_detect(term, "swe") ~ "Snow",
        TRUE ~ "Main Effect"
      ),
      
      # Confidence intervals
      Lower = estimate - 1.96 * std.error,
      Upper = estimate + 1.96 * std.error
    )
})

coefs.weather.all$Season <- factor(coefs.weather.all$Season,
                                   levels = c("Winter", "Spring Movements", 
                                              "Pre-Nesting", "Summer"))

coefs.weather.all <- coefs.weather.all %>%
  drop_na()

################################################################################
## Plot main effects and interactions side by side

ggplot(coefs.weather.all %>%
         dplyr::mutate(
           Weather = factor(
             Weather,
             levels = c("Precipitation", "Snow", "Temperature (Tmin)", "Main Effect")
           )
         ),
       aes(x = Weather, y = estimate, ymin = Lower, ymax = Upper,
           color = Road_Type, shape = Effect_Type)) +
  
  geom_pointrange(
    position = position_dodge(width = 0.6),
    alpha = 0.9
  ) +
  
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  
  facet_wrap(~ Season) +
  
  labs(
    x = NULL,
    y = "Estimate",
    color = "Road type",
    shape = "Effect type"
  ) +
  
  scale_color_manual(values = c(
    "Primary" = "#1b9e77",
    "Secondary" = "#d95f02"
  )) +
  
  scale_shape_manual(values = c(
    "Main" = 16,
    "Interaction" = 17
  )) +
  
  theme_light() +
  theme(
    axis.title.x.top = element_blank(),
    axis.text.x.top = element_blank(),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",                   
    legend.box = "vertical",                      
    legend.title = element_text(),
    legend.text = element_text(size = 10),
    legend.spacing.y = unit(2, "pt"),             
    legend.margin = margin(t = 5, b = 5)
  ) +
  
  coord_flip()


############################
## Individual Covariates ##
############################

################################################################################
## Winter

Win.Prim.Ind.TMB <- glmmTMB(
  case_ ~ 
    -1 +                                
    primary + primary:Age +
    (1 | str_ID),                    
  family = poisson(),                  
  data = ssf.final.winter, 
  map = list(theta = factor(c(NA))),              
  start = list(theta = c(log(1e3)))               
) 
summary(Win.Prim.Ind.TMB)
confint(Win.Prim.Ind.TMB)

Win.Sec.Ind.TMB <- glmmTMB(
  case_ ~ 
    -1 +                                
    secondary + secondary:Age +
    (1 | str_ID),
  family = poisson(),                  
  data = ssf.final.winter, 
  map = list(theta = factor(c(NA))),              
  start = list(theta = c(log(1e3)))               
) 
summary(Win.Sec.Ind.TMB)
confint(Win.Sec.Ind.TMB)


################################################################################
## Spring Movements

Spr.Prim.Ind.TMB <- glmmTMB(
  case_ ~ 
    -1 +                                
    primary + primary:Age + 
    (1 | str_ID),                    
  family = poisson(),                  
  data = ssf.final.spring, 
  map = list(theta = factor(c(NA))),              
  start = list(theta = c(log(1e3)))               
) 
summary(Spr.Prim.Ind.TMB)
confint(Spr.Prim.Ind.TMB)

Spr.Sec.Ind.TMB <- glmmTMB(
  case_ ~ 
    -1 +                                
    secondary + secondary:Age +
    (1 | str_ID),
  family = poisson(),                  
  data = ssf.final.spring, 
  map = list(theta = factor(c(NA))),              
  start = list(theta = c(log(1e3)))               
) 
summary(Spr.Sec.Ind.TMB)
confint(Spr.Sec.Ind.TMB)


################################################################################
## Pre-Nesting

Pre.Prim.Ind.TMB <- glmmTMB(
  case_ ~ 
    -1 +                                
    primary + primary:Age +
    (1 | str_ID),                    
  family = poisson(),                  
  data = ssf.final.pre, 
  map = list(theta = factor(c(NA))),              
  start = list(theta = c(log(1e3)))               
) 
summary(Pre.Prim.Ind.TMB)
confint(Pre.Prim.Ind.TMB)

Pre.Sec.Ind.TMB <- glmmTMB(
  case_ ~ 
    -1 +                                
    secondary + secondary:Age +
    (1 | str_ID),
  family = poisson(),                  
  data = ssf.final.pre, 
  map = list(theta = factor(c(NA))),              
  start = list(theta = c(log(1e3)))               
) 
summary(Pre.Sec.Ind.TMB)
confint(Pre.Sec.Ind.TMB)

################################################################################
## Summer

Sum.Prim.Ind.TMB <- glmmTMB(
  case_ ~ 
    -1 +                                
    primary + primary:Age +
    (1 | str_ID),                    
  family = poisson(),                  
  data = ssf.final.sum, 
  map = list(theta = factor(c(NA))),              
  start = list(theta = c(log(1e3)))               
) 
summary(Sum.Prim.Ind.TMB)
confint(Sum.Prim.Ind.TMB)

Sum.Sec.Ind.TMB <- glmmTMB(
  case_ ~ 
    -1 +                                
    secondary + secondary:Age +
    (1 | str_ID),
  family = poisson(),                  
  data = ssf.final.sum, 
  map = list(theta = factor(c(NA))),              
  start = list(theta = c(log(1e3)))               
) 
summary(Sum.Sec.Ind.TMB)
confint(Sum.Sec.Ind.TMB)

################################################################################
## Collect individual covariate models

models_individual <- list(
  Win.Prim.Ind.TMB = Win.Prim.Ind.TMB,
  Win.Sec.Ind.TMB  = Win.Sec.Ind.TMB,
  
  Spr.Prim.Ind.TMB = Spr.Prim.Ind.TMB,
  Spr.Sec.Ind.TMB  = Spr.Sec.Ind.TMB,
  
  Pre.Prim.Ind.TMB = Pre.Prim.Ind.TMB,
  Pre.Sec.Ind.TMB  = Pre.Sec.Ind.TMB,
  
  Sum.Prim.Ind.TMB = Sum.Prim.Ind.TMB,
  Sum.Sec.Ind.TMB  = Sum.Sec.Ind.TMB
)

################################################################################
## Extract both main effects and interactions

coefs.individual.all <- purrr::map_df(names(models_individual), function(model_name) {
  model <- models_individual[[model_name]]
  
  broom.mixed::tidy(model) %>%
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
        TRUE ~ NA_character_
      ),
      
      # Road Type
      Road_Type = stringr::str_extract(model_name, "(?<=\\.)[^.]+(?=\\.)"),
      Road_Type = dplyr::case_when(
        Road_Type == "Prim" ~ "Primary",
        Road_Type == "Sec"  ~ "Secondary",
        TRUE ~ NA_character_
      ),
      
      # Effect type
      Effect_Type = dplyr::case_when(
        stringr::str_detect(term, ":") ~ "Interaction",
        TRUE ~ "Main"
      ),
      
      # Variable type
      Variable = dplyr::case_when(
        stringr::str_detect(term, "Age") ~ "Juvenile",
        TRUE ~ "Main Effect"
      ),
      
      # Confidence intervals
      Lower = estimate - 1.96 * std.error,
      Upper = estimate + 1.96 * std.error
    )
})

coefs.individual.all$Season <- factor(coefs.individual.all$Season,
                                      levels = c("Winter", "Spring Movements",
                                                 "Pre-Nesting", "Summer"))

coefs.individual.all <- coefs.individual.all %>%
  drop_na()

################################################################################
## Plot main and interaction (Age) effects side by side

ggplot(coefs.individual.all %>%
         dplyr::mutate(
           Variable = factor(
             Variable,
             levels = c("Juvenile", "Main Effect")
           )
         ),
       aes(x = Variable, y = estimate, ymin = Lower, ymax = Upper,
           color = Road_Type, shape = Effect_Type)) +
  
  geom_pointrange(
    position = position_dodge(width = 0.6),
    alpha = 0.9
  ) +
  
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  
  facet_wrap(~ Season) +
  
  labs(
    x = NULL,
    y = "Estimate",
    color = "Road type",
    shape = "Effect type"
  ) +
  
  scale_color_manual(values = c(
    "Primary" = "#1b9e77",
    "Secondary" = "#d95f02"
  )) +
  
  scale_shape_manual(values = c(
    "Main" = 16,
    "Interaction" = 17
  )) +
  
  theme_light() +
  theme(
    axis.title.x.top = element_blank(),
    axis.text.x.top = element_blank(),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",                   
    legend.box = "vertical",              
    legend.title = element_text(),
    legend.text = element_text(size = 10),
    legend.spacing.y = unit(2, "pt"),             
    legend.margin = margin(t = 5, b = 5)
  ) +
  
  coord_flip()

