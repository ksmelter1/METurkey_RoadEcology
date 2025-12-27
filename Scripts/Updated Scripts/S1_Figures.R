#'---
#' title: Context-Dependent Habitat selection of Roads by female wild turkeys
#' authors: "K. Smelter
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#'---
#'
#'  
#' **Purpose**: This script creates supplemental figures for the road ecology analysis


################################################################################
## Load Packages and Data

library(ggplot2)
library(scales)
library(ggpubr)

################################################################################
## Seasonal Comparison Boxplots

random_steps.tod <- random_steps.tod %>%
  dplyr::mutate(Season = dplyr::recode(Season, "Spring Dispersal" = "Spring Movements"))

random_steps.tod$Season <- factor(
  random_steps.tod$Season,
  levels = c("Winter", "Spring Movements", "Pre-Nesting", "Summer", "Fall")
)

p1 <- ggplot(data = random_steps.tod, aes(x = Season, y = secondary, fill = factor(case_))) +
  geom_boxplot(outlier.shape = NA) +  # suppress outliers
  labs(
    title = "Distance to Secondary Road",
    x= NULL,
    y = NULL,
    fill = "Case"
  ) +
  coord_cartesian(ylim = c(NA, 3000)) +  # sets only upper limit
  scale_fill_manual(values = c("0" = "#cbd5e8", "1" = "#fdcdac"), labels = c("Available", "Used")) +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),  # or angle = 45
    text = element_text(size = 12),
    plot.title = element_text(size = 13, hjust = 0.5)
  )
p1

p2 <- ggplot(data = random_steps.tod, aes(x = Season, y = primary, fill = factor(case_))) +
  geom_boxplot(outlier.shape = NA) +  # suppress outliers
  labs(
    title = "Distance to Primary Road",
    x = NULL,
    y = NULL,
    fill = "Case"
  ) +
  coord_cartesian(ylim = c(NA, 8000)) +  # sets only upper limit
  scale_fill_manual(values = c("0" = "#cbd5e8", "1" = "#fdcdac"), labels = c("Available", "Used")) +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),  # or angle = 45
    text = element_text(size = 12),
    plot.title = element_text(size = 13, hjust = 0.5)
  )
p2

# Combine plots side-by-side (or vertically) with shared legend
combined_plot <- p1 + p2 +
  plot_layout(guides = "collect") &  # collect shared legend
  theme(legend.position = "bottom")  # move legend to bottom

# Show the plot
combined_plot


################################################################################
## Land Cover Proportions

final <- random_steps.tod %>%
  as.data.frame() %>%
  dplyr::group_by(case_, Landuse, Season) %>%
  dplyr::summarize(n = n(), .groups = "drop") %>%
  dplyr::group_by(Season, case_) %>%   # <- calculate proportions within each Season-case combo
  dplyr::mutate(
    prop = n / sum(n),
    label = paste0(round(prop * 100, 1), "%")
  ) %>%
  na.omit()

ggplot(final, aes(x = Landuse, y = prop, fill = factor(case_))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    x = "Land Use Class",
    y = "Proportion of Steps",
    fill = "Case"
  ) +
  scale_fill_manual(
    values = c("0" = "#cbd5e8", "1" = "#fdcdac"),
    labels = c("Available", "Used"),
    name = "Case"
  ) +
  facet_wrap(~ Season) +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )
