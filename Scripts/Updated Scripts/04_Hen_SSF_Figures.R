#'---
#' title: Context-Dependent Habitat selection of Roads by female wild turkeys
#' authors: "K. Smelter, M. Gonnerman, L. Berigan, F. Buderman, E. Blomberg
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#' outputs: Maine NLCD Raster, Distance from Primary Roads Raster, Distance from Secondary Roads Raster, ssf.covs.csv
#'   html_document: 
#'     toc: true
#'---
#'

library(tibble)
library(purrr)
library(tidyverse)
library(ggpubr)

load("RData/Updated SP25/03_Results.RData")

# Create a character vector of model names (optional, for clarity)
model_names <- c(
  "Win.Prim.TMB", "Win.Sec.TMB",
  "Spr.Prim.TMB", "Spr.Sec.TMB",
  "Pre.Prim.TMB", "Pre.Sec.TMB",
  "Sum.Prim.TMB", "Sum.Sec.TMB",
  "Fall.Prim.TMB", "Fall.Sec.TMB"
)

# Put model objects in a list
models_list <- list(
  Win.Prim.TMB,
  Win.Sec.TMB,
  Spr.Prim.TMB,
  Spr.Sec.TMB,
  Pre.Prim.TMB,
  Pre.Sec.TMB,
  Sum.Prim.TMB,
  Sum.Sec.TMB,
  Fall.Prim.TMB,
  Fall.Sec.TMB
)

# Extract estimates and standard errors into a tidy tibble
swept_results <- map2_dfr(models_list, model_names, function(model, name) {
  coefs_summary <- summary(model)$coefficients$cond
  tibble(
    model_name = name,
    coefficient_name = rownames(coefs_summary),
    estimate = coefs_summary[, "Estimate"],
    std_error = coefs_summary[, "Std. Error"]
  )
})

interactions <- swept_results %>%
  filter(str_detect(coefficient_name, ":")) %>%
  mutate(
    Season = str_sub(model_name, 1, 3),
    Season = case_when(
      Season == "Win" ~ "Winter",
      Season == "Spr" ~ "Spring Movements",
      Season == "Pre" ~ "Pre-Nesting",
      Season == "Sum" ~ "Summer",
      Season == "Fal" ~ "Fall",
      TRUE ~ NA_character_
    ),
    Road_Type = str_extract(model_name, "(?<=\\.)[^.]+(?=\\.)"),
    Road_Type = case_when(
      Road_Type == "Prim" ~ "Primary",
      Road_Type == "Sec" ~ "Secondary",
      TRUE ~ NA_character_
    ),
    Lower = estimate - 1.96 * std_error,
    Upper = estimate + 1.96 * std_error
  ) 

################################################################################
## Habitat Interaction Plot by Season

# Filter for "Developed" and "Forest" terms in coefficient_name and add "Landuse" column
filtered_interactions <- interactions %>%
  dplyr::filter(str_detect(coefficient_name, "Developed|Forest")) %>%
  dplyr::mutate(
    Landuse = case_when(
      str_detect(coefficient_name, "Developed") ~ "Developed",
      str_detect(coefficient_name, "Forest") ~ "Forest",
      TRUE ~ NA_character_  
    )
  )

filtered_interactions <- filtered_interactions %>%
  dplyr::mutate(
    coefficient_name = fct_reorder(coefficient_name, estimate)
  ) %>%
  mutate(
    Season = factor(Season, levels = c("Winter", "Spring Movements", "Pre-Nesting", "Summer", "Fall"))
  )

ggplot(filtered_interactions, aes(x = Season, y = estimate, ymin = Lower, ymax = Upper, color = Landuse, shape = Road_Type)) +
  geom_pointrange(position = position_jitterdodge(jitter.width = 0.01, dodge.width = 0.5), alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    x = "Season",
    y = "Estimate",
    color = "Land Cover Type",
    shape = "Road Type"
  ) +
  theme_light() +
  scale_shape_manual(values = c(16, 17)) +  
  scale_color_manual(values = c("Forest" = "#606c38", "Developed" = "#Bc6c25")) +  # Custom color mapping
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Tilt x-axis labels
    legend.position = "bottom",  # Move legends to bottom
    legend.box = "vertical",  # Stack legends
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    legend.spacing.y = unit(0.5, "lines"),
    legend.margin = margin(t = 0, b = 0, l = 0, r = 0)
  )


################################################################################
## Time of Day Prediction Plots

interactions_time <- interactions_time %>%
  mutate(
    Season = factor(Season, levels = c("Winter", "Spring Movements", "Pre-Nesting", "Summer", "Fall")),
    Road_Type = factor(Road_Type, levels = c("Primary", "Secondary"))
  )

road_colors <- c("Primary" = "#e36414", "Secondary" = "#9a031e")

scaled_time <- seq(-1, 1, length.out = 100)

unique_seasons <- unique(interactions_time$Season)

for (season in unique_seasons) {
  
  season_df <- interactions_time %>% filter(Season == season)
  
  pred_df <- bind_rows(lapply(1:nrow(season_df), function(i) {
    est <- season_df$estimate[i]
    se <- season_df$std_error[i]
    road_type <- season_df$Road_Type[i]
    
    data.frame(
      scaled_time = scaled_time,
      fit = est * scaled_time,
      lower = (est - 1.96 * se) * scaled_time,
      upper = (est + 1.96 * se) * scaled_time,
      Road_Type = road_type
    )
  }))
  
  p <- ggplot(pred_df, aes(x = scaled_time, color = Road_Type, fill = Road_Type)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.25, color = NA) +
    geom_line(aes(y = fit), size = 1.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    scale_color_manual(values = road_colors) +
    scale_fill_manual(values = road_colors) +
    labs(
      x = "Scaled Time to Sunset",
      y = "Relative Probability of Selection",
      title = paste("Predicted Effect of Time to Sunset -", season),
      color = "Road Type",
      fill = "Road Type"
    ) +
    theme_light()
  
  print(p)
  
  # Optionally save plot
  ggsave(filename = paste0("prediction_plot_", gsub(" ", "_", season), ".pdf"), plot = p, width = 8, height = 6)
}              

################################################################################
## Random Slopes

# Winter Primary Slopes
winter_primary_slopes <- as.data.frame(ranef(Win.Prim.TMB)$cond$BirdID) %>%
  dplyr::rename(primary_slope = primary) %>%
  tibble::rownames_to_column("BirdID") %>%
  dplyr::mutate(
    primary_slope_sd = sqrt(sapply(attr(ranef(Win.Prim.TMB)$cond$BirdID, "condVar"), function(x) x[1]))
  )

# Winter Secondary Slopes
winter_secondary_slopes <- as.data.frame(ranef(Win.Sec.TMB)$cond$BirdID) %>%
  dplyr::rename(secondary_slope = secondary) %>%
  tibble::rownames_to_column("BirdID") %>%
  dplyr::mutate(
    secondary_slope_sd = sqrt(sapply(attr(ranef(Win.Sec.TMB)$cond$BirdID, "condVar"), function(x) x[1]))
  )

# Spring Primary Slopes
spring_primary_slopes <- as.data.frame(ranef(Spr.Prim.TMB)$cond$BirdID) %>%
  dplyr::rename(spring_primary_slope = primary) %>%
  tibble::rownames_to_column("BirdID") %>%
  dplyr::mutate(
    spring_primary_slope_sd = sqrt(sapply(attr(ranef(Spr.Prim.TMB)$cond$BirdID, "condVar"), function(x) x[1]))
  )

# Spring Secondary Slopes
spring_secondary_slopes <- as.data.frame(ranef(Spr.Sec.TMB)$cond$BirdID) %>%
  dplyr::rename(spring_secondary_slope = secondary) %>%
  tibble::rownames_to_column("BirdID") %>%
  dplyr::mutate(
    spring_secondary_slope_sd = sqrt(sapply(attr(ranef(Spr.Sec.TMB)$cond$BirdID, "condVar"), function(x) x[1]))
  )

# Prebreeding Primary Slopes
prenesting_primary_slopes <- as.data.frame(ranef(Pre.Prim.TMB)$cond$BirdID) %>%
  dplyr::rename(prenesting_primary_slope = primary) %>%
  tibble::rownames_to_column("BirdID") %>%
  dplyr::mutate(
    prebreeding_primary_slope_sd = sqrt(sapply(attr(ranef(Pre.Prim.TMB)$cond$BirdID, "condVar"), function(x) x[1]))
  )

# Prebreeding Secondary Slopes
prenesting_secondary_slopes <- as.data.frame(ranef(Pre.Sec.TMB)$cond$BirdID) %>%
  dplyr::rename(prenesting_secondary_slope = secondary) %>%
  tibble::rownames_to_column("BirdID") %>%
  dplyr::mutate(
    prebreeding_secondary_slope_sd = sqrt(sapply(attr(ranef(Pre.Sec.TMB)$cond$BirdID, "condVar"), function(x) x[1]))
  )

# Summer Primary Slopes
summer_primary_slopes <- as.data.frame(ranef(Sum.Prim.TMB)$cond$BirdID) %>%
  dplyr::rename(summer_primary_slope = primary) %>%
  tibble::rownames_to_column("BirdID") %>%
  dplyr::mutate(
    summer_primary_slope_sd = sqrt(sapply(attr(ranef(Sum.Prim.TMB)$cond$BirdID, "condVar"), function(x) x[1]))
  )

# Summer Secondary Slopes
summer_secondary_slopes <- as.data.frame(ranef(Sum.Sec.TMB)$cond$BirdID) %>%
  dplyr::rename(summer_secondary_slope = secondary) %>%
  tibble::rownames_to_column("BirdID") %>%
  dplyr::mutate(
    summer_secondary_slope_sd = sqrt(sapply(attr(ranef(Sum.Sec.TMB)$cond$BirdID, "condVar"), function(x) x[1]))
  )

# Fall Primary Slopes
fall_primary_slopes <- as.data.frame(ranef(Fall.Prim.TMB)$cond$BirdID) %>%
  dplyr::rename(fall_primary_slope = primary) %>%
  tibble::rownames_to_column("BirdID") %>%
  dplyr::mutate(
    fall_primary_slope_sd = sqrt(sapply(attr(ranef(Fall.Prim.TMB)$cond$BirdID, "condVar"), function(x) x[1]))
  )

# Fall Secondary Slopes
fall_secondary_slopes <- as.data.frame(ranef(Fall.Sec.TMB)$cond$BirdID) %>%
  dplyr::rename(fall_secondary_slope = secondary) %>%
  tibble::rownames_to_column("BirdID") %>%
  dplyr::mutate(
    fall_secondary_slope_sd = sqrt(sapply(attr(ranef(Fall.Sec.TMB)$cond$BirdID, "condVar"), function(x) x[1]))
  )

# Combine all slopes into one long-format dataframe
combined_slopes <- winter_primary_slopes %>%
  dplyr::rename(slope = primary_slope, slope_sd = primary_slope_sd) %>%
  dplyr::mutate(type = "Primary", season = "Winter") %>%
  bind_rows(
    winter_secondary_slopes %>%
      dplyr::rename(slope = secondary_slope, slope_sd = secondary_slope_sd) %>%
      dplyr::mutate(type = "Secondary", season = "Winter"),
    
    spring_primary_slopes %>%
      dplyr::rename(slope = spring_primary_slope, slope_sd = spring_primary_slope_sd) %>%
      dplyr::mutate(type = "Primary", season = "Spring"),
    
    spring_secondary_slopes %>%
      dplyr::rename(slope = spring_secondary_slope, slope_sd = spring_secondary_slope_sd) %>%
      dplyr::mutate(type = "Secondary", season = "Spring"),
    
    prebreeding_primary_slopes %>%
      dplyr::rename(slope = prebreeding_primary_slope, slope_sd = prebreeding_primary_slope_sd) %>%
      dplyr::mutate(type = "Primary", season = "Pre-Nesting"),
    
    prebreeding_secondary_slopes %>%
      dplyr::rename(slope = prebreeding_secondary_slope, slope_sd = prebreeding_secondary_slope_sd) %>%
      dplyr::mutate(type = "Secondary", season = "Pre-Nesting"),
    
    summer_primary_slopes %>%
      dplyr::rename(slope = summer_primary_slope, slope_sd = summer_primary_slope_sd) %>%
      dplyr::mutate(type = "Primary", season = "Summer"),
    
    summer_secondary_slopes %>%
      dplyr::rename(slope = summer_secondary_slope, slope_sd = summer_secondary_slope_sd) %>%
      dplyr::mutate(type = "Secondary", season = "Summer"),
    
    fall_primary_slopes %>%
      dplyr::rename(slope = fall_primary_slope, slope_sd = fall_primary_slope_sd) %>%
      dplyr::mutate(type = "Primary", season = "Fall"),
    
    fall_secondary_slopes %>%
      dplyr::rename(slope = fall_secondary_slope, slope_sd = fall_secondary_slope_sd) %>%
      dplyr::mutate(type = "Secondary", season = "Fall")
  )

# Define dodge position
dodge <- position_dodge(width = 0.8)

# Create individual plots for each season
winter_plot <- ggplot(combined_slopes %>% filter(season == "Winter"), aes(x = BirdID, y = slope, color = type, shape = type)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(position = dodge, size = 3, alpha = 0.9) +
  geom_errorbar(
    aes(ymin = slope - slope_sd, ymax = slope + slope_sd),
    width = 0.2,
    position = dodge,
    alpha = 0.8
  ) +
  scale_color_manual(values = c("Primary" = "tan", "Secondary" = "salmon")) +
  theme_light() +
  labs(
    x = "BirdID",
    y = "Slope Estimate",
    color = "Road Type",
    shape = "Road Type",
    title = "Winter"
  )+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
winter_plot

spring_plot <- ggplot(combined_slopes %>% filter(season == "Spring"), aes(x = BirdID, y = slope, color = type, shape = type)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(position = dodge, size = 3, alpha = 0.9) +
  geom_errorbar(
    aes(ymin = slope - slope_sd, ymax = slope + slope_sd),
    width = 0.2,
    position = dodge,
    alpha = 0.8
  ) +
  scale_color_manual(values = c("Primary" = "tan", "Secondary" = "salmon")) +
  theme_light() +
  labs(
    x = "BirdID",
    y = "Slope Estimate",
    color = "Road Type",
    shape = "Road Type",
    title = "Spring Movements"
  )+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
spring_plot

prenesting_plot <- ggplot(combined_slopes %>% filter(season == "Pre-Nesting"), aes(x = BirdID, y = slope, color = type, shape = type)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(position = dodge, size = 3, alpha = 0.9) +
  geom_errorbar(
    aes(ymin = slope - slope_sd, ymax = slope + slope_sd),
    width = 0.2,
    position = dodge,
    alpha = 0.8
  ) +
  scale_color_manual(values = c("Primary" = "tan", "Secondary" = "salmon")) +
  theme_light() +
  labs(
    x = "BirdID",
    y = "Slope Estimate",
    color = "Road Type",
    shape = "Road Type",
    title = "Pre-Nesting"
  ) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
prenesting_plot

summer_plot <- ggplot(combined_slopes %>% filter(season == "Summer"), aes(x = BirdID, y = slope, color = type, shape = type)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(position = dodge, size = 3, alpha = 0.9) +
  geom_errorbar(
    aes(ymin = slope - slope_sd, ymax = slope + slope_sd),
    width = 0.2,
    position = dodge,
    alpha = 0.8
  ) +
  scale_color_manual(values = c("Primary" = "tan", "Secondary" = "salmon")) +
  theme_light() +
  labs(
    x = "BirdID",
    y = "Slope Estimate",
    color = "Road Type",
    shape = "Road Type",
    title = "Summer"
  ) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
summer_plot

fall_plot <- ggplot(combined_slopes %>% filter(season == "Fall"), 
                    aes(x = BirdID, y = slope, color = type, shape = type)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(position = dodge, size = 3, alpha = 0.9) +
  geom_errorbar(
    aes(ymin = slope - slope_sd, ymax = slope + slope_sd),
    width = 0.2,
    position = dodge,
    alpha = 0.8
  ) +
  scale_color_manual(values = c("Primary" = "tan", "Secondary" = "salmon")) +
  scale_shape_manual(values = c("Primary" = 16, "Secondary" = 17)) +  # Add shape scale
  theme_light() +
  labs(
    color = "Road Type",
    shape = "Road Type",
    title = "Fall"
  ) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# Arrange the plots with shared legend
combined_panel <- ggarrange(
  winter_plot, spring_plot, prenesting_plot,
  summer_plot, fall_plot, NULL,
  ncol = 2, nrow = 3,
  common.legend = TRUE,
  legend = "bottom"
)

# Add shared x and y axis labels
final_plot <- annotate_figure(
  combined_panel,
  left = text_grob("Slope Estimate", rot = 90, size = 14),
  bottom = text_grob("BirdID", size = 14)
)

# Display
final_plot

