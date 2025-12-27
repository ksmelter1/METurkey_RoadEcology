##'---
#' title: Study Area Map for Maine Wild Turkey Road Selection Manuscript
#' authors: "K. Smelter
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#' output:
#'   html_document: 
#'     toc: true
#'---
#'
#+ include = FALSE
#'  
#' **Purpose**: This script creates a study area map for the Maine wild turkey road selection manuscript
#' **Last Updated**: 11/9/2025

################################################################################
## Load Packages

library("sf")
library("ggplot2")
library("terra")
library("mapview")
library("dplyr")
library("ggspatial")

################################################################################
## US Map

# Us map shapefile without great lakes
us <- st_read("Shapefiles/USA/USA_adm1_without_great_lakes.shp")
us <- st_transform(us, crs = 5070)
mapview(us)
################################################################################
## Maine and Capture Sites 

maine <- us %>% dplyr::filter(NAME_1 == "Maine")
mapview(maine)

maine <- ms_simplify(maine, keep = 0.05)
mapview(maine)

cap <- read.csv("Csvs/Capture Sites/CaptureSites.csv")
cap <- cap %>%
  drop_na() %>%
  mutate(across(where(is.character), ~iconv(., from = "", to = "UTF-8", sub = ""))) %>%
  dplyr::rename("Lat" = "Latitude",
                "Long" = "Longitude") 

# Convert to sf
cap_sf <- cap %>%
  st_as_sf(coords = c("Long", "Lat"), crs = 4326) %>% st_transform(5070) 

# Mapview works on sf objects
mapview(cap_sf)

#################################################################
## Create Map

# Display PA
ggplot() + 
  geom_sf(data = maine) 

# Create object with all three states and create bounding box
all <- st_union(maine)
st_bbox(all)

# Create bounding box
box_zoom = (st_bbox(c(xmin= 1930897, ymin= 2500869, xmax= 2258358, ymax= 3012937))) %>%
  st_as_sfc() %>% st_set_crs(5070)
us_crop <- st_crop(us, box_zoom)

# Create state-only boundaries by dissolving internal WMUs/THAs
me_boundary <- st_union(maine)


# Create object with all other US states
us_crop2 <- us_crop[!us_crop$NAME_1 %in% c(
  "Maine"),]
sa_map <- ggplot() +
  
  geom_sf(data = me_boundary, fill = NA, color = "black", linewidth = 0.6) +
  
  # State boundaries for PA, NJ, and MD
  geom_sf(data = maine, fill = "grey90", color = "black", linewidth = 0.0001) +

  
  geom_sf(data = cap_sf, fill = "grey90", color = "black", linewidth = 0.0001, shape = 17, size = 2) +
  # Labels
  #geom_sf_label(data = pa.study.area, aes(label = WMU_ID_Map), size = 4, fontface = "bold", label.size = 0.3) +
  
  # Map styling
  theme_classic() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  annotation_north_arrow(location = 'br',
                         style = north_arrow_orienteering,
                         height = unit(1.25, "cm"), 
                         width = unit(1.25, "cm")) +
  annotation_scale(style = "bar",
                   pad_x = unit(0.05, "in"),
                   pad_y = unit(0.05, "in"))


plot(sa_map)

# Add capture sites to the map
sa_map <- sa_map +
  geom_sf(data = cap_sf,
          aes(),
          shape = 17,               # Circle with fill
          color = "black",          # Border color
          fill = "black",             # Fill color
          size = 2,                 # Point size
          stroke = 0.3)             # Border thickness
plot(sa_map)
