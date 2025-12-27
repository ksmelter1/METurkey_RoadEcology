#'---
#' title: Context-Dependent Habitat selection of Roads by female wild turkeys
#' authors: "K. Smelter, A. Weber
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#'---
#'
knitr::opts_chunk$set(warning = FALSE, message = FALSE, cache = TRUE)
#'  
#' **Purpose**: This script uses relative selection strength to visualize the effect of weather on selection
#' ** Last Updated**: 12/27/2025


######---------------------------------------------------------------------#####
# Step 3: Bootstrap the top model (model 7)
######---------------------------------------------------------------------#####

library(parallel)
library(doSNOW)

# Set up the front end
set.seed(8)
r <- 1000 # number bootstraps

# Check the number of clusters
parallel::detectCores() # 28 available
cl <- makeCluster(10)
registerDoSNOW(cl)

# Progress bar
pb <- txtProgressBar(max = r, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

# Parallel loop
results <- foreach(i = 1:r, .combine = rbind, .options.snow = opts,
                   .packages = c('glmmTMB')) %dopar% {
                     IDs <- sample(unique(ssf.final.winter$BirdID), size = length(unique(ssf.final.winter$BirdID)), replace = TRUE)
                     
                     resampdata <- data.frame()
                     for(k in seq_along(IDs)){
                       sub <- subset(ssf.final.winter, BirdID == IDs[k])
                       sub$ID2 <- paste(sub$BirdID, k, sep = "_")
                       resampdata <- rbind(resampdata, sub)
                     }
                     
                     
                     ## Model comparison for top model
                     # Set model structure with doFit = F
                     mod7.structure <- glmmTMB(case_ ~ -1 + primary + primary:tmin + primary:prcp + primary:swe +
                                                 (1 | str_ID),            
                                               family = poisson, data = ssf.final.winter, doFit = F)
                     
                     # Set large standard deviation for first random effect (for random intercept by step_id)
                     mod7.structure$parameters$theta[1] <- log(1e3) 
                     
                     # Force the first standard deviation, all other values are freely estimated (add random slopes here, assigning each a different integer)
                     mod7.structure$mapArg <- list(theta=factor(c(NA)))
                     
                     # Fit the model and look at the summary
                     mod7 <- fitTMB(mod7.structure)
                     
                     # Store important parts of model
                     beta.primary <- fixef(mod7)$cond[1]
                     beta.int.tmin <- fixef(mod7)$cond[2]
                     beta.int.precip <- fixef(mod7)$cond[3]
                     beta.int.swe <- fixef(mod7)$cond[4]
                     # Store everything in a data frame
                     x <- data.frame(beta.primary = beta.primary,
                                     beta.int.tmin = beta.int.tmin,
                                     beta.int.precip = beta.int.precip,
                                     beta.int.swe = beta.int.swe,
                                     ID = paste(unique(resampdata$ID2), collapse = "|"))
                     # Return the dataframe
                     return(x)
                   }

# Stop the progress bar and clusters
close(pb)
stopCluster(cl) 

# View the results dataframe
View(results)


######---------------------------------------------------------------------#####
# Step 3: Bootstrap the basic model (Model 1)
######---------------------------------------------------------------------#####



# Store the results
# write.csv(results, "data/glmmTMB/bootstrapped_top_model7_glmmTMB.csv”)

#------------------------------------------------------------------------------#
# Create prediction function for single covariate and another for interactions
#------------------------------------------------------------------------------#

# NOTE: The functions (specifically "sequence") will have to be updated based
# on the variable being considered.

# Store yearly covariate means of the scaled data 
# Store yearly covariate means of the scaled data
cov_means <- c(
  mean(ssf.final.winter$primary),
  mean(ssf.final.winter$prcp),
  mean(ssf.final.winter$tmin),
  mean(ssf.final.winter$swe)
)

names(cov_means) <- c("primary", "prcp", "tmin", "swe")


################################################################################
## Single Covariate of Interest

predict_RSS_single <- function(samples, sequence, cov_means){
  
  # Create a matrix with nrow = samples, ncol = length(sequence)
  mat <- matrix(NA, nrow = nrow(samples), ncol = length(sequence))
  
  # Calculate the predictive value for each cell
  for(i in 1:nrow(mat)){
    for(j in 1:ncol(mat)){
      mat[i,j] <- exp(
        samples[i,1] * cov_means["primary"]      +  # Distance to primary road
          samples[i,2] * cov_means["prcp"]  +  # Precipitation
          samples[i,3] * cov_means["tmin"]  +  # Minimum Temperature
          samples[i,4] * cov_means["swe"]     # SWE
      )
    }
  }
  
  # Store the mean, lower 2.5%, and upper 97.5% percentiles
  data.frame(
    seq   = sequence,
    mean  = apply(mat, 2, mean),
    lower = apply(mat, 2, quantile, 0.025),
    upper = apply(mat, 2, quantile, 0.975)
  )
}

primary.seq <- seq(from = range(ssf.final.winter$primary)[1],
                   to = range(ssf.final.winter$primary)[2], length.out = 1000)

evergreen_RSS <- predict_RSS_single(samples = results[,1:4],
                                    sequence = primary.seq,
                                    cov_means = cov_means)

predict_RSS_inx <- function(samples, sequence, cov_means, percentile){
  # Create a matrix with nrow = samples, ncol = length(sequence)
  mat <- matrix(NA, nrow = nrow(samples), ncol = length(sequence))
  
  # Calculate the predictive value for each cell
  for(i in 1:nrow(mat)){
    for(j in 1:ncol(mat)){
      mat[i,j] <- exp(
        samples[i,1] * cov_means["primary"] +
          samples[i,2] * cov_means["prcp_scaled"] +
          samples[i,3] * cov_means["tmin_scaled"] +
          samples[i,4] * cov_means["swe_scaled"] +
          samples[i,5] * sequence[j] +                                   # the interaction term you are varying
          samples[i,6] * cov_means["primary"] * cov_means["prcp_scaled"] +
          samples[i,7] * cov_means["primary"] * cov_means["tmin_scaled"] +
          samples[i,8] * cov_means["primary"] * percentile               # primary * snow interaction
      )
    }
  }
  
  # Store the mean, lower 2.5%, and upper 97.5% percentiles
  mean  <- apply(mat, 2, mean)
  lower <- apply(mat, 2, quantile, 0.025)
  upper <- apply(mat, 2, quantile, 0.975)
  
  # Return dataframe
  x <- data.frame(
    seq = sequence,
    mean = mean,
    lower = lower, 
    upper = upper
  )
  
  return(x)
}

results_numeric <- as.data.frame(apply(results[,1:5], 2, as.numeric))


int_RSS <- predict_RSS_inx(
  samples   = results[,1:4],
  sequence  = primary.seq,
  cov_means = cov_means,
  percentile = 0.25
)


mod7 <- try(fitTMB(mod7.structure), silent = TRUE)

if (inherits(mod7, "try-error")) {
  return(data.frame(
    beta.primary = NA,
    beta.int.tmin = NA,
    beta.int.precip = NA,
    beta.int.swe = NA,
    ID = NA
  ))
}


#------------------------------------------------------------------------------#
# Distance to evergreen #
#------------------------------------------------------------------------------#
# 
# # Function
# predict_RSS_single <- function(samples, sequence, cov_means){
#   # Create a matrix with nrow = samples, ncol = length(sequence)
#   mat <- matrix(NA, nrow = nrow(samples), ncol = length(sequence))
#   # Calculate the predictive value for each cell
#   for(i in 1:nrow(mat)){
#     for(j in 1:ncol(mat)){
#       mat[i,j] <- exp(samples[i,1] * cov_means["s_dist_to_road"]  + # Distance to road
#                         samples[i,2] * cov_means["s_elevation"] + # Elevation
#                         samples[i,3] * cov_means["s_slope"] + # Slope
#                         samples[i,4] * cov_means["cos_aspect"] + # Northing
#                         samples[i,5] * cov_means["sin_aspect"] + # Easting
#                         samples[i,6] * cov_means["s_sl"] + # Step length
#                         samples[i,7] * cov_means["s_dist_to_herb"] +  # Distance to herbaceous
#                         samples[i,8] * sequence[j] + # Distance to evergreen
#                         samples[i,9] * cov_means["s_snow_depth"] + # Snow depth
#                         samples[i,10] * cov_means["cos_aspect"] * cov_means["s_dist_to_herb"] + # Northing*Herbaceous
#                         samples[i,11] * cov_means["sin_aspect"] * cov_means["s_dist_to_herb"] + # Easting*Herbaceous
#                         samples[i,12] * cov_means["s_dist_to_herb"] * cov_means["s_snow_depth"] + # Herbaceous*Snow
#                         samples[i,13] * sequence[j] * cov_means["s_snow_depth"] + # Evergreen*Snow
#                         samples[i,14] * cov_means["cos_aspect"] * cov_means["s_snow_depth"] + # Northing*Snow
#                         samples[i,15] * cov_means["sin_aspect"] * cov_means["s_snow_depth"] + # Easting*Snow
#                         samples[i,16] * cov_means["s_elevation"] * cov_means["s_snow_depth"]) # Elevation*Snow
#     } #j
#   } #i
#   # Store the mean, lower 2.5%, and upper 97.5% percentiles
#   mean  <- apply(mat, 2, mean)
#   lower <- apply(mat, 2, quantile, 0.025)
#   upper <- apply(mat, 2, quantile, 0.975)
#   # Store these as a dataframe and return it
#   x <- data.frame(seq = sequence,
#                   mean = mean,
#                   lower = lower, 
#                   upper = upper)
#   return(x)
# }
# 
# # Store sequence across all years, captures full range of values
# evergreen.seq <- seq(from = range(data$s_dist_to_evergreen)[1],
#          to = range(data$s_dist_to_evergreen)[2], length.out = 1000)
# 
# # Get RSS
# evergreen_RSS <- predict_RSS_single(samples = bootstrap, sequence = evergreen.seq,
#                                       cov_means = cov_means)
# write.csv(evergreen_RSS, "data/glmmTMB/bootstrap_CIs/pred_dist_evergreen.csv")

# Plot the output
#evergreen_RSS <- read.csv("data/glmmTMB/bootstrap_CIs/pred_dist_evergreen.csv")

# Store sequence across all years, captures full range of values
 primary.seq <- seq(from = range(ssf.final.winter$primary)[1],
          to = range(ssf.final.winter$primary)[2], length.out = 1000)
 

p1 <- ggplot(data = evergreen_RSS) +
  geom_line(aes(x = (seq*sd(ssf.final.winter$primary) + mean(ssf.final.winter$primary)), 
                y = mean), linewidth = 1.1) +
  geom_ribbon(aes(x = (seq*sd(ssf.final.winter$primary) + mean(ssf.final.winter$primary)), 
                  ymin = lower, ymax = upper), alpha = 0.4, fill = "#4C9B82") + 
  labs(y = "Relative Selection Strength",
       x = "Distance to Evergreen (m)") +
  theme_classic()
plot(p1)

# Save the plot
# ggsave(plot = p1, filename = "figures/glmmTMB/dist_to_evergreen_predicted_plot.jpg")