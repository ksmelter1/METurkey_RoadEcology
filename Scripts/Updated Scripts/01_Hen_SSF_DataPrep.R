#'---
#' title: Context-Dependent Habitat selection of Roads by female wild turkeys
#' authors: K. Smelter, M. Gonnerman
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
packages <- c("move",
              "dplyr",
              "lubridate",
              "dplyr",
              "mapview",
              "sf")

# Function to load a package or install it if not already installed
load_packages <- function(package_name) {
  if (!require(package_name, character.only = TRUE)) {
    install.packages(package_name, dependencies = TRUE)
    require(package_name, character.only = TRUE)
  }
}

# Apply the function to each package name
lapply(packages, load_packages)

# Login to download data from Movebank directly
login <- movebankLogin(username = "matthew.gonnerman", password="26qPDLY9YN")

# Load csv with BirdID and associated time frame
birdlist <- read.csv("Csvs/SeasonShifts.csv")

# Create move object with fall turkeys from 2018
turkeys2018.move <- getMovebankData(study = "Eastern Wild Turkey, Gonnerman, Maine", login = login,  
                                   timestamp_start = "201808010500000", timestamp_end = "201811010500000")

# Convert move object to a dataframe and rename columns
turkeys2018 <- turkeys2018.move@data %>%
  mutate(BirdID = gsub(x = turkeys2018.move@trackId, pattern = "X", replacement = "")) %>% 
  dplyr::rename( Lat=location_lat, Long=location_long) %>%
  mutate(Year = lubridate::year(timestamp), TrackID = paste(BirdID, Year, sep = "_"))

# Visualize distinct BirdIDs
turkeys2018 %>% select(BirdID, tag_id) %>% distinct()

# Create move object with fall turkeys from 2019
turkeys2019.move <- getMovebankData(study = "Eastern Wild Turkey, Gonnerman, Maine", login = login,  
                                     timestamp_start = "201908010500000", timestamp_end = "201911010500000")

# Convert move object to a dataframe and rename columns
turkeys2019 <- turkeys2019.move@data %>%
  mutate(BirdID = gsub(x = turkeys2019.move@trackId, pattern = "X", replacement = "")) %>% 
  dplyr::rename( Lat=location_lat, Long=location_long) %>%
  mutate(Year = lubridate::year(timestamp), TrackID = paste(BirdID, Year, sep = "_"))

# Visualize distinct BirdIDs
turkeys2019 %>% select(BirdID, tag_id) %>% distinct()

# Create move object with fall turkeys from 2020
turkeys2020.move <- getMovebankData(study = "Eastern Wild Turkey, Gonnerman, Maine", login = login,  
                                    timestamp_start = "202008010500000", timestamp_end = "202011010500000")

# Convert move object to a dataframe and rename columns
turkeys2020 <- turkeys2020.move@data %>%
  mutate(BirdID = gsub(x = turkeys2020.move@trackId, pattern = "X", replacement = "")) %>% 
  dplyr::rename(Lat=location_lat, Long=location_long) %>%
  mutate(Year = lubridate::year(timestamp), TrackID = paste(BirdID, Year, sep = "_"))

# Visualize distinct BirdIDs
turkeys2020 %>% select(BirdID, tag_id) %>% distinct()

# Creating the fall turkeys dataframe, here I bind rows with the spatial data from 2018, 2019, and 2020
turkeys.fall.df <- rbind(turkeys2018, turkeys2019, turkeys2020)%>%
  mutate(Season ="Fall") %>%
  dplyr::mutate(BirdID= as.character(BirdID))

# For loop written by M. Gonnerman
# This for loop downloads specified seasonal shift data from the seasonshifts.csv file
# I copied the criteria from above under the getmovebank command to achieve a uniform data structure
# This for loop creates a dataframe 
rm(full_all)# Clear objects so you don't duplicate data (for if not starting new session)###Downloading data for each timestamp 
#for(i in 1:4){
for(i in 1:nrow(birdlist)){
  BirdID <- as.character(birdlist[i,1])
  Season <- birdlist[i,3] 
  StartDate <- gsub("\\D","", birdlist$Start[i]) #(Format for time is YYYYMMDDHHSSMM000)
  EndDate <- gsub("\\D","", birdlist$End[i])
  Year <-birdlist$Year[i]

  points_ind <- getMovebankData(study = "Eastern Wild Turkey, Gonnerman, Maine", 
                                login = login, 
                                 animalName = BirdID, 
                                timestamp_start = StartDate, 
                                timestamp_end = EndDate)
  
  points_ind@data$BirdID <- paste(BirdID)
  points_ind@data$Year <- Year
  points_ind@data$Season <- Season
  points_ind@data$TrackID <- paste(BirdID, birdlist$Year[i], Season, sep = "_")
  points_ind@data$Long <- (coordinates(points_ind))[,1]
  points_ind@data$Lat <- (coordinates(points_ind))[,2]
  
  
  if(exists("full_all")){ # rbind ind bird data to create one large df
    full_all <- rbind(full_all, points_ind@data)
  
    
  }else{
    full_all <- points_ind@data
  }
}

# Bind fall points with the birdlist dataset to get the complete dataframe
turkeys.fall<-data.frame(turkeys.fall.df)

# Change BirdID to numeric
full_all$BirdID <- as.character(full_all$BirdID)

# Change BirdID to numeric
turkeys.fall.df$BirdID<- as.numeric(turkeys.fall.df$BirdID)

# Bind together fall and other seasonal datasets
full_all1 <- bind_rows(full_all, turkeys.fall)

# Consolidate different prenesting attempts to a single "Pre-Nesting" Variable
full_all1$Season <- recode(full_all1$Season, PreNest1= "Pre-Nesting", PreNest2= "Pre-Nesting", PreNest3= "Pre-Nesting", Spring.Dispersal= "Spring Dispersal")

# Create subsetted dataframe 
# Remove unneeded columns 
# Change tag_id to character
turkeys_subset <- subset(select(full_all1, 
                                -manually_marked_outlier, 
                                -sensor_type,
                                -location_lat,
                                -location_long,
                                -sensor_type_id, 
                                -event_id,
                                -update_ts,
                                -visible,
                                -tag_local_identifier,
                                -deployment_id)) %>%
                      dplyr::mutate(tag_id= as.character(tag_id))


###################
## Explore Data ##
###################

# Check structure
str(turkeys_subset)

# Obtain summary statistics
summary(turkeys_subset)

# Get counts for variables of interest
table(turkeys_subset$Season)
table(turkeys_subset$tag_id)
table(turkeys_subset$Year)

######################################
## Write Shapefile and Create Csv ##
######################################

# Create spatial dataframe and extract coordinates
# 26919 is UTM Zone 19 for Maine 
turkeys.sf <- st_as_sf(turkeys_subset, coords= c("Long", "Lat"), 
                       crs= 4326) %>%
                      st_transform(26919)
# Check projection
mapview(turkeys.sf)

# Create a csv file and an ESRI shapefile of all used locations 
st_write(turkeys.sf, ".", layer= "Raw_Points", driver= "ESRI Shapefile", overwrite_layer =T )
write.csv(turkeys_subset,"Raw_Points.csv")

################################################################################
###############################################################################X