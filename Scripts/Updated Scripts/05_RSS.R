#'---
#' title: Context-Dependent Habitat selection of Roads by female wild turkeys
#' authors: "K. Smelter
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#'---
#'
knitr::opts_chunk$set(warning = FALSE, message = FALSE, cache = TRUE)
#'  
#' **Purpose**: This script uses relative selection strength to visualize the effect of land cover on selection
#' ** Last Updated**: 12/28/2025


################################################################################
## Load Packages and Data

library(tidyverse)
library(glmmTMB)
library(MASS)        
library(tidybayes)   

# Filter data to season and time of day of interest
ssf.final.winter.morning <- ssf.final.winter %>%
  dplyr::filter(tod_bin == "Morning")

# Match data to above season and time of day of interest
random.steps.tod.winter.morning <- random_steps.tod %>%
  dplyr::filter(tod_bin == "Morning") %>%
  dplyr::filter(Season == "Winter")

# Obtain mean and sd from untransformed data for primary roads
primary_center <- mean(random.steps.tod.winter.morning$primary, na.rm = TRUE)
primary_scale  <- sd(random.steps.tod.winter.morning$primary, na.rm = TRUE)

# Obtain mean and sd from untransformed data for secondary roads
secondary_center <- mean(random.steps.tod.winter.morning$secondary, na.rm = TRUE)
secondary_scale  <- sd(random.steps.tod.winter.morning$secondary, na.rm = TRUE)

# Create unscale functions for both road types
unscale_primary <- function(x) {
  x * primary_scale + primary_center
}

unscale_secondary <- function(x) {
  x * secondary_scale + secondary_center
}

# Create distance from primary road sequence to be plotted
primary_seq <- seq(
  from = min(ssf.final.winter.morning$primary, na.rm = TRUE),
  to   = max(ssf.final.winter.morning$primary, na.rm = TRUE),
  by   = 0.1
)

# Create prediction grid with all three habitat covariates
pred_grid <- expand.grid(
  primary   = primary_seq,
  Landscape = c("Agriculture", "Developed", "Conifer")
) %>%
  mutate(
    Agriculture = as.integer(Landscape == "Agriculture"),
    Developed   = as.integer(Landscape == "Developed"),
    Conifer     = as.integer(Landscape == "Conifer")
  )

# Extract model coefficients
# Make sure model matches season, time period, and road type of interest
beta_hat <- fixef(Win.Prim.Mor.TMB)$cond
vcov_mat <- vcov(Win.Prim.Mor.TMB)$cond

# Simulate 
set.seed(123)

# Draw from multivariate normal approximation of coefficient uncertainty
beta_draws <- mvrnorm(
  n     = 10000,
  mu    = beta_hat,
  Sigma = vcov_mat
) %>%
  as_tibble() %>%
  rename_with(~ paste0("b_", .x))

################################################################################
## Generate Relative Selection Strength

# Combine prediction grid with coefficient draws
prediction_draws <- pred_grid %>%
  crossing(beta_draws) %>%
  mutate(
    
    # Linear predictor including interactions with road distance
    eta =
      primary * b_primary +
      Developed * b_Developed +
      Agriculture * b_Agriculture +
      Conifer * b_Conifer +
      primary * Developed * `b_primary:Developed` +
      primary * Agriculture * `b_primary:Agriculture`+
      primary * Conifer * `b_primary:Conifer`,
    
    # Relative selection strength
    rss = exp(eta)
  )

# Median RSS and 95% uncertainty intervals
prediction_summary <- prediction_draws %>%
  group_by(primary, Developed, Conifer, Agriculture) %>%
  median_qi(rss, .width = 0.95) %>%
  mutate(
    # Back-transform primary road distance to original units
    primary_m = unscale_primary(primary)
  )

# Ensure landscape indicators are numeric
prediction_summary <- prediction_summary %>%
  mutate(
    Developed   = as.numeric(Developed),
    Agriculture = as.numeric(Agriculture),
    Conifer     = as.numeric(Conifer))

# Convert to long format for ggplot 
prediction_long <- prediction_summary %>%
  pivot_longer(
    cols = c(Agriculture, Developed, Conifer),
    names_to  = "Landscape",
    values_to = "presence"
  ) %>%
  filter(presence == 1)

################################################################################
## Generate Prediction Plots


ggplot(prediction_long,
       aes(x = primary_m,
           y = rss,
           color = Landscape,
           fill  = Landscape)) +
  
  # Neutral selection reference line
  geom_hline(yintercept = 1,
             linetype = "dashed",
             linewidth = 1,
             color = "grey40") +
  
  # Median prediction
  geom_line(linewidth = 1) +
  
  # 95% confidence interval
  geom_ribbon(aes(ymin = .lower, ymax = .upper),
              alpha = 0.25,
              color = NA) +
  
  # Manual color scales by landscape context
  scale_color_manual(values = c(
    "Agriculture" = "#fb8b24",
    "Conifer"     = "#183a37",
    "Developed"   = "#9a031e"
  )) +
  scale_fill_manual(values = c(
    "Agriculture" = "#fb8b24",
    "Conifer"     = "#183a37",
    "Developed"   = "#9a031e"
  )) +
  ylim(0, 50) +
  
  labs(
    y     = "Relative selection strength",
    x     = "Distance to primary road",
    color = "Landscape context",
    fill  = "Landscape context"
  ) +
  theme_light()

################################################################################
###############################################################################X