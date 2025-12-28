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
packages <- c("sf",
              "amt",
              "tigris",
              "FedData",
              "dplyr",
              "terra",
              "ggplot2",
              "suncalc",
              "lubridate",
              "tidyr",
              "mapview")

# Function to load a package or install it if not already installed
load_packages <- function(package_name) {
  if (!require(package_name, character.only = TRUE)) {
    install.packages(package_name, dependencies = TRUE)
    require(package_name, character.only = TRUE)
  }
}

# Apply the function to each package name
lapply(packages, load_packages)


###############################
## Prepare NLCD Covariates ##
###############################

# GEOIDs above 60 are territories and islands so they are being removed for scaling
   st <- tigris::states() 
 
# Transform to albers equal area conic projection, epsg code is 5070
   st <- st_transform(st, 5070)

# Grab outline of Maine
   me.outline <- subset(st, st$NAME=="Maine") 
   
# Obtain Maine NLCD raster
  me.nlcd <- FedData::get_nlcd(template= me.outline, year = 2019, 
                              label = 'me', 
                             force.redo = T)

# Reclassify NLCD -- Skyrockets RAM
# See covertyps for NLCD here: https://www.mrlc.gov/data/legends/national-land-cover-database-class-legend-and-description
# Liam note: consider using the terra::subset function in the future- should fix your RAM issue
terra::values(me.nlcd) <- ifelse(terra::values(me.nlcd) %in% c(21:24), yes = "Developed", ## Developed Open, low
                                 no = ifelse(terra::values(me.nlcd) %in% c(41, 90), yes = "Hardwood", ## Hardwood
                                             no = ifelse(terra::values(me.nlcd) %in% c(42:43), yes = "Conifer",
                                             no = ifelse(terra::values(me.nlcd) %in% c(81:82), yes = "Agriculture", ## Agriculture
                                             no = "Other"))))

# Crop ME NLCD raster to the Maine outline
me.nlcd <- crop(me.nlcd, vect(me.outline))

# Mask the ME NLCD raster according to the Pennsylvania outline 
me.nlcd <- mask(me.nlcd, vect(me.outline))
plot(me.nlcd)

# Save raster
writeRaster(me.nlcd, "Rasters/nlcd/meNLCD.Ag.tiff", overwrite=T)

# Load Raster
me.nlcd <- terra::rast("Rasters/nlcd/meNLCD.Ag.tiff")

#################################
## Distance from Primary Road ##
#################################

# Read in roads shapefile
me.roads <- st_read("Shapefiles/Roads (Raw)/MaineDOT_Public_Roads.shp") %>%
  sf::st_transform(5070) 

# Check number of road classifications and categories
table(me.roads$fedfunccls)

# Classify primary and secondary roads
# Secondary roads are the local roads
# Primary roads are everything else
me.roads$Road_ID <- ifelse((me.roads$fedfunccls) %in% c("Local"), yes = "Secondary",
                           no= "Primary")
# Check updated
table(me.roads$Road_ID)

# Create primary road df
me.roads.prim <- me.roads %>%
  dplyr::filter(Road_ID=="Primary") 

# Create secondary roads df
me.roads.sec <- me.roads %>%
  dplyr::filter(Road_ID=="Secondary") 

# Plot out
mapview(me.roads, zcol = "Road_ID", legend = TRUE)

# Create skeleton raster
# To call the raster just type in the name in the console 
me.nlcd

# Format skeleton raster dimensions based on Maine NLCD
 r <- terra::rast(nrow= 17504, ncol= 11101, xmin= 1930755, ymin= 2487915, nlyr=1,
          xmax= 2263785, ymax= 3013035, crs= "epsg:5070")
 
 #####################
 ## Primary Roads ##
 #####################
 
# Rasterize road layer
me.roads.prim.rast <- terra::rasterize(me.roads.prim, r, fun=min)
 
# Calculate distance to each 30m x 30m cell
dist.prim <- distance(me.roads.prim.rast)

# Crop and mask to skeleton raster
dist.prim <- crop(dist.prim, me.nlcd)
dist.prim <- mask(dist.prim, me.nlcd)

# Check raster crs
raster_crs <- crs(dist.prim)

# Save raster
# writeRaster(dist.prim, "meroadrast.prim.tiff")

# Load Raster
dist.prim<- terra::rast("Rasters/roads/meroadrast.prim.tiff")

########################
## Secondary Roads ##
#######################

# Rasterize road layer
me.roads.sec.rast <- terra::rasterize(me.roads.sec, r, fun=min)
 
# Calculate distance to each 30m x 30m cell
dist.sec <- distance(me.roads.sec.rast)

# Crop and mask to skeleton raster
dist.sec<- crop(dist.sec, me.nlcd)
dist.sec <- mask(dist.sec, me.nlcd)

# Check raster crs
raster_crs <- crs(dist.sec)

# Save raster
# writeRaster(dist.sec, "meroadrast.sec.tiff")

# Load Raster
dist.sec<- terra::rast("Rasters/roads/meroadrast.sec.tiff")

##############################
## Create and Save Tracks ##
##############################

# I follow the AMT Vignette with multiple individuals
# https://cran.r-project.org/web/packages/amt/vignettes/p1_getting_started.html
# 1. Load in data from movebank prep script
# 2. Make a track using dataset with multiple individuals
# 3. Nest the track by id
# 4. Take one individual's data, view its sampling rate and adjust using steps by burst
# 5. Use the map function to apply the same steps by burst parameters across the marked population

# Load in RData
load ("RData/Updated FA24/01MovementDataDownloaded.RData") 

# Complete hen movement dataset from movebank
dat <- turkeys_subset
summary(dat)

# Create a track
# Keep all columns
# Project to Albers
trk <- amt::make_track(tbl=dat, .x= Long, .y=Lat, .t=timestamp, id=BirdID,
                       crs= 4326, all_cols = T) %>% 
                        amt::transform_coords(5070)
# Check
class(trk)
summary(trk)

# Group the track by id and nest the track
# Check
trk1 <- trk %>% nest(data = -"BirdID")
trk1

# Check
class(trk1)
summary(trk1)

# Get the data for the first animal
x <- trk1$data[[1]]

# Set criteria for steps 
# Rate: sampling rate
# Tolerance: the tolerance of deviations of the sampling rate
# Steps by burst: Returns NA for zero step lengths and calculates step lengths for points on a path
# Time of day: Was the fix taken during the day or night? we could filter out night locations
x %>% track_resample(rate = minutes(60), tolerance = hours(5)) %>%
  amt::steps_by_burst(keep_cols = T)

# Check
class(x)
summary(x)

# Summarize sampling rate
summarize_sampling_rate(x)

# Apply the same track resampling format to each hen within the dataset
# This done by using the map function
# Steps dataframe is the newly created column
trk2 <- trk1 %>%
  mutate(steps = map(data, ~ .x %>%
                       track_resample(rate = minutes(60), tolerance = hours(5)) %>%
                       amt::steps_by_burst(keep_cols = "start") %>%
                       amt::time_of_day(include.crepuscule = TRUE, where = "end")))

# Check
class(trk2)
glimpse(trk2)

# Visualize step length distribution following vignette
trk2 |> dplyr::select(BirdID, steps) |> unnest(cols = steps) |> 
  ggplot(aes(sl_, fill = factor(BirdID))) + geom_density(alpha = 0.4)

# Create object with BirdID#' Create object with all used steps for analysis
stps <- trk2 %>% dplyr::select(BirdID,steps) %>% unnest(cols = steps)

# Check
glimpse(stps)

# Create random steps
# Exponential step length used due to issues with formatting gamma
# Extract covariates at the end of each used and available step
# Random step lengths drawn from gamma distribution
# Random turning angles drawn from a vonmises distribution
# Include_observed: Include all used steps in the analysis
names(dist.sec) <- "secondary"
names(dist.prim) <- "primary"
random_steps <-amt::random_steps(
  stps,
  n_control = 10,
  sl_distr = amt::fit_distr(stps$sl_, "gamma"),
  ta_distr = amt::fit_distr(stps$ta_, "vonmises"),
  include_observed = T) %>%
  extract_covariates(dist.sec, where= "end") %>%
  extract_covariates(dist.prim, where= "end") %>%
  extract_covariates(me.nlcd, where= "end") %>%
  dplyr::rename("Landuse" = Class) %>%
  dplyr::mutate(Developed = ifelse(Landuse == "Developed", 1, 0)) %>%
  dplyr::mutate(Hardwood = ifelse(Landuse == "Hardwood", 1, 0)) %>%
  dplyr::mutate(Conifer = ifelse(Landuse == "Conifer", 1, 0)) %>%
  dplyr::mutate(Agriculture = ifelse(Landuse == "Agriculture", 1, 0)) %>%
  dplyr::mutate(Other = ifelse(Landuse == "Other", 1, 0))

# Check
which(random_steps$step_id_==5)
table(random_steps$case_)
table(random_steps$Season)
table(random_steps$tod_end_)
table(random_steps$Landuse)

#############################
## Time of Day Analysis ##
#############################

# Rename columns 
# Use as.date to create dates
random_steps <- random_steps %>%
  dplyr::rename(lon= "x2_") %>%
  dplyr::rename(lat= "y2_") %>%
  dplyr::mutate(date = as.Date(t2_)) %>%
  dplyr::rename(timestamp= "t2_")

# Remove all roost locations (Birds are stationary)
random_steps <- random_steps %>%
  dplyr::filter(tod_end_ != "night")

# Assign GPS locations to time of day
random_steps.tod <- random_steps %>%
  dplyr::mutate(
    timestamp = ymd_hms(timestamp, tz = "GMT"),
    t2_local = with_tz(timestamp, tzone = "America/New_York"),
    hour = hour(t2_local),
    tod_bin = case_when(
      hour >= 5  & hour < 12 ~ "Morning",
      hour >= 12 & hour < 16 ~ "Midday",
      hour >= 16 & hour < 20 ~ "Afternoon",
      hour >= 20 & hour < 24 ~ "Evening",
      hour >= 0  & hour < 5  ~ "Other",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::filter(tod_bin != "Other") %>%
  dplyr::mutate(case_ = as.numeric(case_))

# Check
table(random_steps.tod$tod_bin)

################################################################################
## Weather variables

library(sf)
library(dplyr)
library(lubridate)
library(purrr)
library(daymetr)

# Prepare sites (unique locations and years)
sites <- random_steps.tod %>%
  mutate(year = year(date)) %>%
  dplyr::filter(case_ == "1") %>%
  distinct(BirdID, lon, lat, date, .keep_all = T) %>%
  filter(!is.na(lon), !is.na(lat)) 

# Convert to sf using UTM Zone 19N (EPSG:32619, correct for Maine)
sites_sf <- st_as_sf(
  sites,
  coords = c("lon", "lat"),
  crs = 5070
)

# Transform to WGS84 decimal degrees
sites_ll <- st_transform(sites_sf, 4326)

# Add back coordinates
sites <- sites %>%
  mutate(
    lon_dd = st_coordinates(sites_ll)[,1],
    lat_dd = st_coordinates(sites_ll)[,2]
  )

################################################################################
## Weather variables with SWE

all_weather <- purrr::map_dfr(1:nrow(sites), function(i) {
  s <- sites[i, ]
  
  dat <- download_daymet(
    site  = paste0("pt", i),
    lat   = s$lat_dd,
    lon   = s$lon_dd,
    start = s$year,
    end   = s$year,
    internal = TRUE
  )
  
  dat$data %>%
    transmute(
      BirdID = s$BirdID,
      lon    = s$lon_dd,
      lat    = s$lat_dd,
      date   = as.Date(yday, origin = paste0(s$year, "-01-01")),
      tmin   = tmin..deg.c.,
      prcp   = prcp..mm.day.,
      swe    = swe..kg.m.2.  # Add Snow Water Equivalent
    ) %>%
    filter(date == s$date)   # keep only the GPS date
})

all_weather_unique <- all_weather %>%
  group_by(BirdID, date) %>%
  summarise(
    tmin = mean(tmin, na.rm = TRUE),
    prcp = mean(prcp, na.rm = TRUE),
    swe  = mean(swe, na.rm = TRUE),  # ✅ include SWE
    .groups = "drop"
  )

random_steps.weather <- random_steps.tod %>%
  left_join(all_weather_unique, by = c("BirdID", "date")) 

################################################################################
## Prepare Data for Models

random_steps.tod <- random_steps.weather

# Scale variables
ssf.final <- random_steps.tod %>%
  st_drop_geometry() %>%
  dplyr::mutate(primary = scale(primary)) %>%
  dplyr::mutate(secondary = scale(secondary)) %>%
  dplyr::mutate(tmin = scale (tmin)) %>%
  dplyr::mutate(precip = scale(prcp)) %>%
  dplyr::mutate(swe = scale(swe))

# Change from matrix to numeric
ssf.final <- ssf.final %>%
  dplyr::mutate(primary = as.numeric(primary)) %>%
  dplyr::mutate(secondary = as.numeric(secondary)) %>%
  dplyr::mutate(tmin = as.numeric (tmin)) %>%
  dplyr::mutate(precip = as.numeric(precip)) %>%
  dplyr::mutate(swe = as.numeric(swe)) %>%
  drop_na(lat)

# Create a numeric animal identifier column
ssf.final$ANIMAL_ID <- as.numeric(as.factor(ssf.final$BirdID))

# Create strata column by merging BirdID and step_id
ssf.final$NA_ID <- paste(ssf.final$ANIMAL_ID,
                         ssf.final$step_id_,
                         sep = "")

# Stratum ID is given as "NA_ID" in the data; 
# It is easier to have sequential enumeration, so let's generate a new stratum-ID variable str_ID:
d.map <- data.frame(NA_ID=unique(ssf.final$NA_ID),
                    str_ID=1:length(unique(ssf.final$NA_ID)))
ssf.final$str_ID <- d.map[match(ssf.final$NA_ID,d.map$NA_ID),"str_ID"]
ssf.final <- ssf.final[order(ssf.final$str_ID),] 

# Bring in age data 
age <- read.csv("Csvs/Raw/Trapping/Age.csv")
head(age)

# Convert to character
age$BirdID <- as.character(age$BirdID)

# Add Age to ssf.final
ssf.final<- left_join(ssf.final, age, by = c("BirdID"))

# Recode Age: Adult (A) = 0, Juvenile (J) = 1
ssf.final <- ssf.final %>%
  mutate(Age = case_when(
    Age == "A" ~ 0,
    Age == "J" ~ 1,
    TRUE ~ NA_real_  # Handle any missing or unexpected values
  )) %>%
  # Remove rows with missing Age
  filter(!is.na(Age))

# Check result
table(ssf.final$Age, useNA = "ifany")

ssf.final$ta_ <- scale(as.numeric(ssf.final$ta_))
ssf.final$sl_ <- scale(as.numeric(ssf.final$sl_))

# Create separate datasets for each season
ssf.final.winter <- ssf.final %>%
  dplyr::filter(Season == "Winter") 
ssf.final.spring <- ssf.final %>%
  dplyr::filter(Season == "Spring Dispersal") 
ssf.final.pre <- ssf.final %>%
  dplyr::filter(Season == "Pre-Nesting")
ssf.final.sum <- ssf.final %>%
  dplyr::filter(Season == "Summer")
ssf.final.fall <- ssf.final %>%
  dplyr::filter(Season == "Fall")

# Output data by saving the window

################################################################################
###############################################################################X

#' ##############################
#' ## Moving Window Analysis ##
#' ##############################
#' 
#' #' Create random_steps as an Sf object
#' #' Project to NAD Zone 19
#' #' Transform to Albers to match the NLCD projection
#' #' Create a 200m buffer around the end of each step (x2_, y2_)
#'   st_buffer(200) 
#' 
#' #' Use terra to extract the proportion of land cover categories within each buffer
#' #' Use mutate to create new columns for each land cover category
#' #' Developed, Forest, Agriculture, Other
#' #' Check variability between used and available steps
#' landcov_table <- terra::extract(x = me.nlcd, y = vect(random_steps.sf), fun = "table")%>%
#'   dplyr::mutate(pct_forest = Forest/(Forest+other+Developed+Agriculture)) %>%
#'   dplyr::mutate(pct_developed = Developed/(Developed+other+Forest+Agriculture)) %>%
#'   dplyr::mutate(pct_agriculture= Agriculture/(Agriculture+other+Forest+Developed)) %>%
#'   dplyr::mutate(pct_other = other/(other+Forest+Developed+Agriculture))
#' 
#' #' Create dataframe with all covs except for hen age and distance from road
#' #' Rename Bin to Time
#' #' Select columns for later analysis
#' #' Create duplicate lat and lon columns
#' random_steps.sf.landcov.tod <- cbind(random_steps.tod, landcov_table) %>%
#'   dplyr::rename("Time"= Bin) %>%
#'   dplyr::select(BirdID, Season, Time, T2Sunset,
#'                 step_id_, pct_forest, 
#'                 pct_agriculture, pct_developed, Developed,
#'                 Forest, Agriculture,
#'                 lat, lon, case_, 
#'                 secondary, primary) %>%
#'   dplyr::mutate("lat1"= lat) %>%
#'   dplyr::mutate("lon1"= lon) %>%
#'   sf::st_as_sf(coords = c("lon1", "lat1"), crs = 5070) %>%
#'   dplyr::select(-lat, -lon)
#' table(random_steps.sf.landcov.tod$Time)

#' Check
#' Looks good
#' mapview(random_steps.sf.landcov.tod)