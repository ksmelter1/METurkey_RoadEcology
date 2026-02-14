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
#' **Last Updated**: 1/12/26

################################################################################
## Load Packages

library("sf")
library("ggplot2")
library("terra")
library("mapview")
library("tidyverse")
library("ggspatial")
library("rmapshaper")
library("lwgeom")

################################################################################
## US Map

# Us map shapefile without great lakes
us <- st_read("Shapefiles/USA/USA_adm1_without_great_lakes.shp")
us <- st_transform(us, crs = 5070)
mapview(us)

################################################################################
## Maine Projection 

# Get the Maine outline 
maine <- us %>% dplyr::filter(NAME_1 == "Maine")
mapview(maine)

# Simplify the Maine outline to avoid the jagged edges
maine <- ms_simplify(maine, keep = 0.05)
mapview(maine)

################################################################################
## Capture Site Consolidation

# Read in birdlist
birdlist <- read_csv("Csvs/Birdlist/BirdList.csv")
birdlist

# Rename and convert BirdID to character
birdlist <- birdlist %>%
  dplyr::rename("BirdID" = `Bird ID`) %>%
  dplyr::mutate(BirdID = as.character(BirdID))

# Read in trapping data
trapping <- read_csv("Csvs/Raw/Trapping/Trapping.csv")
trapping

# Rename, convert to character and join birdlist by BirdID
# This keeps only observations that exist in the birdlist csv to exist in the trapping data
trapping.birdlist <- trapping %>%
  dplyr::rename(BirdID = AlumBand) %>%
  dplyr::mutate(BirdID = as.character(BirdID)) %>%
  dplyr::semi_join(birdlist, by = "BirdID")

# Read in capture data
cap <- read_csv("Csvs/Capture Sites/CaptureSites.csv")
cap

# Configure coding style
# Rename columns to Lattitude and Longitude
cap <- cap %>%
  mutate(across(where(is.character), ~iconv(., from = "", to = "UTF-8", sub = ""))) %>%
  dplyr::rename("Lat" = "Latitude",
                "Long" = "Longitude") 

# Obtain study area information from only individuals that exist in birdlist
cap.study <- cap %>%
  dplyr::semi_join(
    trapping.birdlist %>% dplyr::select(Study.Area),
    by = "Study.Area"
  )

# Convert to sf
cap_sf <- cap.study %>%
  st_as_sf(coords = c("Long", "Lat"), crs = 4326) %>% st_transform(5070) 

# Mapview works on sf objects
mapview(cap_sf)

################################################################################
## Add in wildlife management districts

# Read in WMD shapefile
wmds <- st_read("Shapefiles/WMDs/Wildlife_Management_Districts.shp") %>%
  st_transform(st_crs(maine))
mapview(wmds)

# Make geometries valid
wmds_valid <- st_make_valid(wmds)
me_boundary_valid <- st_make_valid(maine)

# Re-run intersection
wmds_me <- st_intersection(wmds_valid, maine)

# Check
mapview(wmds_me)

# The study takes place in these WMDs
me.study.area <- subset(wmds_me, IDENTIFIER=="26"| IDENTIFIER=="23"| IDENTIFIER=="17"| IDENTIFIER=="18"| IDENTIFIER == "14")

# Ensure only one label for WMD 26
me.study.area <- wmds_me %>%
  dplyr::filter(IDENTIFIER %in% c("14", "17", "18", "23", "26")) %>%
  dplyr::group_by(IDENTIFIER) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

# Label map is just equal to the identifier column
me.study.area$WMU_ID_Map <- me.study.area$IDENTIFIER


#################################################################
## Create Map

# Display ME
ggplot() + 
  geom_sf(data = maine) 

# Create object with all three states and create bounding box
all <- st_union(maine)
st_bbox(all)

# Create bounding box
box_zoom = (st_bbox(c(xmin= 1930897, ymin= 2500869, xmax= 2258358, ymax= 3012937))) %>%
  st_as_sfc() %>% st_set_crs(5070)
us_crop <- st_crop(us, box_zoom)

# Create state-only boundaries by dissolving internal WMDs
me_boundary <- st_union(maine)


# Create object with Maine
us_crop2 <- us_crop[!us_crop$NAME_1 %in% c(
  "Maine"),]
sa_map <- ggplot() +
  geom_sf(data = me_boundary, fill = NA, color = "black", linewidth = 0.6) +
  geom_sf(data = maine, fill = "grey90", color = "black", linewidth = 0.0001) +
  geom_sf(data = wmds_me, fill = "grey90", color = "black") +
  geom_sf(data = cap_sf, shape = 17, color = "black", size = 2) +
  geom_sf(data = me.study.area, fill = "darkgray", color = "black", linewidth = 0.0001) +
  
  geom_sf_label(data = me.study.area, aes(label = WMU_ID_Map), size = 3, fontface = "bold", label.size = 0.3) +
  theme_classic() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  annotation_north_arrow(
    location = "br",
    style = north_arrow_orienteering,
    height = unit(1.25, "cm"),
    width  = unit(1.25, "cm")
  ) +
  annotation_scale(
    style = "bar",
    pad_x = unit(0.05, "in"),
    pad_y = unit(0.05, "in")
  )

plot(sa_map)

################################################################################
## Calculate proportion NLCD in each WMD

# Load NLCD raster
me.nlcd <- terra::rast("Rasters/nlcd/meNLCD.Ag.tiff")

# Ensure projection matches WMDs
me.nlcd <- project(me.nlcd, vect(me.study.area))

# Split WMDs
me.study.area.14 <- subset(me.study.area, WMU_ID_Map == "14")
me.study.area.17 <- subset(me.study.area, WMU_ID_Map == "17")
me.study.area.18 <- subset(me.study.area, WMU_ID_Map == "18")
me.study.area.23 <- subset(me.study.area, WMU_ID_Map == "23")
me.study.area.26 <- subset(me.study.area, WMU_ID_Map == "26")

# Extract land cover counts in WMD 14
# Obtain proportions
landcov_counts.14 <- terra::extract(x = me.nlcd,y = me.study.area.14,fun = table,ID = TRUE)
landcov_matrix.14 <- landcov_counts.14[, -1]
landcov_matrix.14 <- sweep(
  landcov_matrix.14,
  1,
  rowSums(landcov_matrix.14, na.rm = TRUE),
  "/"
)
landcov.df.14 <- as.data.frame(landcov_matrix.14)
landcov.df.14$WMU <- "14"

# Extract land cover counts in WMD 17
# Obtain proportions
landcov_counts.17 <- terra::extract(me.nlcd, me.study.area.17, fun = table, ID = TRUE)
landcov_matrix.17 <- sweep(landcov_counts.17[, -1], 1,
                           rowSums(landcov_counts.17[, -1], na.rm = TRUE), "/")
landcov.df.17 <- as.data.frame(landcov_matrix.17)
landcov.df.17$WMU <- "17"

# Extract land cover counts in WMD 18
# Obtain proportions
landcov_counts.18 <- terra::extract(me.nlcd, me.study.area.18, fun = table, ID = TRUE)
landcov_matrix.18 <- sweep(landcov_counts.18[, -1], 1,
                           rowSums(landcov_counts.18[, -1], na.rm = TRUE), "/")
landcov.df.18 <- as.data.frame(landcov_matrix.18)
landcov.df.18$WMU <- "18"

# Extract land cover counts in WMD 23
# Obtain proportions
landcov_counts.23 <- terra::extract(me.nlcd, me.study.area.23, fun = table, ID = TRUE)
landcov_matrix.23 <- sweep(landcov_counts.23[, -1], 1,
                           rowSums(landcov_counts.23[, -1], na.rm = TRUE), "/")
landcov.df.23 <- as.data.frame(landcov_matrix.23)
landcov.df.23$WMU <- "23"

# Extract land cover counts in WMD 26
# Obtain proportions
landcov_counts.26 <- terra::extract(me.nlcd, me.study.area.26, fun = table, ID = TRUE)
landcov_matrix.26 <- sweep(landcov_counts.26[, -1], 1,
                           rowSums(landcov_counts.26[, -1], na.rm = TRUE), "/")
landcov.df.26 <- as.data.frame(landcov_matrix.26)
landcov.df.26$WMU <- "26"

# Create single df with all proportions
me.study.area.nlcd <- bind_rows(
  landcov.df.14,
  landcov.df.17,
  landcov.df.18,
  landcov.df.23,
  landcov.df.26
) %>%
  dplyr::mutate(State = "Maine")

################################################################################
###############################################################################X