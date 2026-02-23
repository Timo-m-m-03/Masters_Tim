#### Generating LiDAR metrics ####
# Big job for monday, you need to change all the functions such that they are only looking at the unclassified group of points

setwd("C:/Git repository/Masters_Tim")

# Load packages

library(lidR)
library(rgl)
library(gstat)
library(tidyverse)
library(silviculture)
library(sf)
library(leafR)
library(forestr)
install.packages("leafR")


# Loading in data 

las <- readLAS("data/raw/2021/W57D_4.las")
print(las)

las_check(las)

writeLAS(nlas, "data/output/W57D_4_norm.las")

#### Standard metrics ####

st_metrics <- cloud_metrics(nlas, func = .stdmetrics)
print(st_metrics)
#### Canopy Height #### 

tnlas <- normalize_height(las, tin())
nlas <- filter_poi(tnlas, Classification != 2)
plot(nlas, color = "Z", axis = TRUE, legend = TRUE)


lidR::plot(nlas, color = "Z")

## Exploration 

## Ground - ground classification works perfectly

hist(filter_ground(nlas)$Z, breaks = seq(-0.6, 0.6, 0.01), main = "", xlab = "Elevation")

ground <- filter_ground(nlas)$Z

stats_g <- data.frame(
  Statistic = c("Mean", "Median", "Std Dev", "Min", "Max", "Range",
                "P25", "P75", "P95"),
  Value = round(c(
    mean(ground),
    median(ground),
    sd(ground),
    min(ground),
    max(ground),
    max(ground) - min(ground),
    quantile(ground, 0.25),
    quantile(ground, 0.75),
    quantile(ground, 0.95)
  ), 3)
)

print(stats_g)

## Canopy metrics #

canopy <- filter_poi(nlas, Classification == 1L)
z <- canopy@data$Z

stats_c <- data.frame(
  Statistic = c("Mean", "Median", "Std Dev", "Min", "Max", "Range",
                "P25", "P75", "P95"),
  Value = round(c(
    mean(z),
    median(z),
    sd(z),
    min(z),
    max(z),
    max(z) - min(z),
    quantile(z, 0.25),
    quantile(z, 0.75),
    quantile(z, 0.95)
  ), 3)
)

print(stats_c)
      
canopy_df <- data.frame(Z = z)


ggplot(canopy_df, aes(x = Z)) +
  geom_histogram(binwidth = 0.5, fill = "#2d7d46", colour = "white", alpha = 0.85)     

## Pixel metrics 

pmet <- pixel_metrics(nlas, .stdmetrics, 10, filter = ~Classification == 1) 
plot(pmet, col = height.colors(50))
names(pmet)

plot(pmet$zmean)


## Voxel metrics

vmet <- voxel_metrics(nlas, .stdmetrics, 10, filter = ~Classification == 1)

plot(vmet$zmean)

## Canopy height and standard deviation  

sdH <- cloud_metrics(nlas,.stdmetrics, filter = ~Classification == 1)
print(sdH)

f_canopy_height_sd <- function(z, threshold = 2) {
  z <- as.numeric(z)
  
  # Filter to canopy points only (Hi values)
  canopy_heights <- z[z >= threshold]
  
  # Check if there are any canopy points
  if (length(canopy_heights) == 0) {
    return(list(
      canopy_sd   = NA,
      canopy_mean = NA,
      n_canopy    = 0
    ))
  }
  
  # Calculate mean canopy height (H̄)
  H_bar <- mean(canopy_heights)
  
  # Calculate the number of points
  n <- length(canopy_heights)
  
  # Calculate standard deviation using the formula
  sigma_H <- sqrt(sum((canopy_heights - H_bar)^2) / (n - 1))
  
  return(list(
    canopy_sd   = round(sigma_H, 3),
    canopy_mean = round(H_bar, 3)
  ))
}

# Apply to whole plot
canopy_height_sd <- cloud_metrics(nlas, ~f_canopy_height_sd(Z, threshold = 2))
print(canopy_height_sd)

# Canopy height ans standard deviation at different thresholds

f_canopy_height_sd_multi <- function(z, thresholds = c(0.75,1,2)) {
  z <- as.numeric(z)
  
  results <- list()
  for (thresh in thresholds) {
  # Filter to canopy points only (Hi values)
  canopy_heights <- z[z >= thresh]
  
  # Calculate mean canopy height (H̄)
  H_bar <- mean(canopy_heights)
  
  # Calculate the number of points
  n <- length(canopy_heights)
  
  # Calculate standard deviation using the formula
  sigma_H <- sqrt(sum((canopy_heights - H_bar)^2) / (n - 1))
  
  results[[paste0("canopy_mean_", thresh, "m")]] <- round(H_bar, 3)
  results[[paste0("canopy_sd_", thresh, "m")]] <- round(sigma_H, 3)
 
  }
  
  return(results)
}

# Apply to whole plot
height_multi <- cloud_metrics(nlas, ~f_canopy_height_sd_multi(Z))
print(height_multi)

#### Canopy height density (CHD) ####

## Canopy point density

f_canopy_height_density <- function(z, threshold = 2,area){
  z <- as.numeric(z)
  
  # Filter canopy points
  n <- sum(z>= threshold)
  
  # Canopy height density
  points <- n/area
  
  return(points)
}

plot_area <- as.numeric(st_area(st_as_sfc(st_bbox(nlas))))

canopy_height_density <- cloud_metrics(nlas, ~f_canopy_height_density(Z, threshold = 2, area = plot_area))
print(canopy_height_density)

## Canopy point density at multiple thresholds

f_canopy_height_density_multi <- function(z, threshold = c(0.75,1,2),area){
  z <- as.numeric(z)
  
  #Calculate for each threshold
  results <- list()
  for (thresh in threshold){
 
     # Filter canopy points
  n <- sum(z>= thresh)
  
  # Canopy height density
  points <- n/area
 
  results[[paste0("density_", thresh, "m")]] <- points
    
  }
  
  return(results)
}

plot_area <- as.numeric(st_area(st_as_sfc(st_bbox(nlas))))

canopy_height_density <- cloud_metrics(nlas, ~f_canopy_height_density_multi(Z, area = plot_area))
print(canopy_height_density)

## Canopy point density within threshold bands 

f_vegetation_density_band <- function(z, classification, canopy = 1, ranges = list(c(0, 0.75), c(0.75, 1), c(1, 2), c(2, 9)),area){
  z <- as.numeric(z)
  
  
  results <- list()
  for (i in seq_along(ranges)) {
    min_h <- ranges[[i]][1]
    max_h <- ranges[[i]][2]
    
    range_points <- sum(z >= min_h & z < max_h)
    
    # Density of points
    points <- range_points/area
    
    # Create name using the current range 
    results[[paste0("density_", min_h, "_", max_h, "m")]] <- points
   
    
  }
  
  return(results)
}

plot_area <- as.numeric(st_area(st_as_sfc(st_bbox(nlas))))

canopy_height_density_band <- cloud_metrics(nlas, ~f_vegetation_density_band(Z, area = plot_area, ranges = list(c(0.75, 1), c(1, 2), c(2, 9))))
print(canopy_height_density_band)



#### Percentage of returns above mean ####

f_percentage_above <- function(z){
  z <- as.numeric(z)
  
  # Mean canopy heights
  H_bar <- mean(z)
  
  # Total number of points 
  total_points  <- length(z)
  
  # Returns above mean
  return_above <- sum(z >= H_bar)
  
  #Calculate percentage of returns above mean
  percent_above <- return_above/total_points
  
  return(percentage_above = round(percent_above * 100, 2))
  
}

percentage_above <- cloud_metrics(nlas, ~f_percentage_above(Z))
print(percentage_above)

#### Distribution of canopy (skewness/kurtoisis) ####
# Idk if this is based on canopy points or all points


## Skewness

f_canopy_skewness<- function(z, threshold = 2) {
  z <- as.numeric(z)
  
  # Filter to canopy points only (Hi values)
  canopy_heights <- z[z >= threshold]
  
  # Calculate mean canopy height (H̄)
  H_bar <- mean(canopy_heights)
  
  # Number of points
  n <- length(canopy_heights)
  
  #Standard deviation
  sd_val <- sd(canopy_heights)
  
  # Calculate standard deviation using the formula
  sym <- sum((canopy_heights - H_bar)^3)/(sd_val^3*(n-1))
  
  return(sym)
}

canopy_skewness <- cloud_metrics(nlas, ~f_canopy_skewness(Z, threshold = 2))
print(canopy_skewness)

## Skewness at multiple canopy thresholds

f_canopy_skewness_multi<- function(z, threshold = c(0.75,1,2)) {
  z <- as.numeric(z)
  
  results <- list()
  for(thresh in threshold){
  # Filter to canopy points only (Hi values)
  canopy_heights <- z[z >= thresh]
  
  # Calculate mean canopy height (H̄)
  H_bar <- mean(canopy_heights)
  
  # Number of points
  n <- length(canopy_heights)
  
  #Standard deviation
  sd_val <- sd(canopy_heights)
  
  # Calculate standard deviation using the formula
  sym <- sum((canopy_heights - H_bar)^3)/(sd_val^3*(n-1))
  
  results[[paste0("skewness_", thresh, "m")]] <- sym
  
  }
  return(results)
}

canopy_skewness_multi <- cloud_metrics(nlas, ~f_canopy_skewness_multi(Z))
print(canopy_skewness_multi)

## Kurtosis 

f_canopy_kurtosis<- function(z, threshold = 2) {
  z <- as.numeric(z)
  
  # Filter to canopy points only (Hi values)
  canopy_heights <- z[z >= threshold]
  
  # Calculate mean canopy height (H̄)
  H_bar <- mean(canopy_heights)
  
  # Number of points
  n <- length(canopy_heights)
  
  #Standard deviation
  sd_val <- sd(canopy_heights)
  
  # Calculate standard deviation using the formula
  kur <- sum((canopy_heights - H_bar)^4)/(sd_val^4*(n-1))
  
  return(kur)
}

canopy_kurtosis <- cloud_metrics(nlas, ~f_canopy_kurtosis(Z, threshold = 2))
print(canopy_kurtosis)

## Kurtosis at multiple canopy thresholds

f_canopy_kurtosis_multi<- function(z, threshold = c(0.75,1,2)) {
  z <- as.numeric(z)
  
  results <- list()
  for (thresh in threshold) {
  # Filter to canopy points only (Hi values)
  canopy_heights <- z[z >= thresh]
  
  # Calculate mean canopy height (H̄)
  H_bar <- mean(canopy_heights)
  
  # Number of points
  n <- length(canopy_heights)
  
  #Standard deviation
  sd_val <- sd(canopy_heights)
  
  # Calculate standard deviation using the formula
  kur <- sum((canopy_heights - H_bar)^4)/(sd_val^4*(n-1))
  
  results[[paste0("kurtosis_", thresh, "m")]] <- kur
  
  }
  
  return(results)
}

canopy_kurtosis_multi <- cloud_metrics(nlas, ~f_canopy_kurtosis_multi(Z))
print(canopy_kurtosis_multi)


#### Canopy Cover ####
# For all these you need to set the threshold height for what you consider the canopy

## Online function

# Define the f_metrics function in the global environment
f_metrics <- function(Z, n) {
  # Strata: Proportion of Z values in the range of 2 to 5 meters
  strata = length(Z[Z > 2 & Z < 5]) / length(Z)
  # Canopy Cover: Proportion of returns in the first return
  Zcov = length(Z[Z >= 0.75 & n == 1]) / length(Z[n == 1])
 
  # Create a list of computed metrics
  list_metrics = list(
    COV = Zcov,            # Canopy cover
    Hmean = mean(Z),       # Mean canopy height
    HSD = sd(Z),           # Standard deviation of height
    HMAX = max(Z),         # Maximum height
    S = strata             # Strata value
  )
  
  return(list_metrics)  # Return the list of metrics
}

canopy_metrics_grid <- pixel_metrics(nlas, func = ~f_metrics(Z, ReturnNumber), res = 1) 
plot(canopy_metrics_grid$COV)


cc <- data.frame(canopy_metrics_grid)

canopy_cover_o <- cloud_metrics(nlas, func = ~f_metrics(Z, ReturnNumber))
print(canopy_cover_o)

## Canopy cover 

f_canopy_cover <- function(z, threshold = 0.75) {
  z <- as.numeric(z)
  
  # Total number of points 
  total_points  <- length(z)           
  
  # Canopy points
  canopy_points <- sum(z >= threshold)   
  
  #Calculate canopy cover
  cover <- canopy_points / total_points  # Canopy Cover ratio (0–1)
  
  return(list(
    canopy_cover    = cover,
    canopy_cover_pct = round(cover * 100, 2)  # as a percentage
  ))
}

cover_result <- cloud_metrics(nlas, func = ~f_canopy_cover(Z))
print(cover_result)

## Canopy cover - 1st returns (this works the same as the online)

f_canopy_cover_first <- function(z, rn, threshold = 0.75) {
  z  <- as.numeric(z)
  rn <- as.numeric(rn)
  
  first <- rn == 1                        # identify first returns
  
  canopy_first <- sum(z[first] >= threshold)  # ∑ CHM_canopy  (first returns above threshold)
  total_first  <- sum(first)                  # ∑ CHM_total   (all first returns)
  
  cover <- canopy_first / total_first
  
  return(list(
    canopy_cover     = cover,
    canopy_cover_pct = round(cover * 100, 2)
  ))
}

cover_result_1 <- cloud_metrics(nlas, ~f_canopy_cover_first(Z, ReturnNumber))

print(cover_result_1)

## Canopy cover at different canopy heights

f_canopy_cover_multi <- function(z, thresholds = c(0.75, 1, 2)) {
  z <- as.numeric(z)
  
  # Total number of points in the area
  total_points <- length(z)
  
  # Calculate for each threshold
  results <- list()
  for (thresh in thresholds) {
    canopy_points <- sum(z >= thresh)
    cover <- canopy_points / total_points
    
    # Store with descriptive names
    results[[paste0("cover_", thresh, "m")]] <- cover
    results[[paste0("cover_pct_", thresh, "m")]] <- round(cover * 100, 2)
  }
  
  return(results)
}

# Apply once, get all thresholds
cover_all <- cloud_metrics(nlas, ~f_canopy_cover_multi(Z))
print(cover_all)

## Canopy cover for height threshold bands (cover of vegetation) ##

f_vegetation_percentage_band <- function(z, ranges = list(c(0, 0.75), c(0.75, 1), c(1, 2), c(2, 9))){
  z <- as.numeric(z)
  
  #Total points
  total_points <- length(z)
  
  results <- list()
  for (i in seq_along(ranges)) {
    min_h <- ranges[[i]][1]
    max_h <- ranges[[i]][2]
    
    range_points <- sum(z >= min_h & z < max_h)
    
    # Canopy height percentage
    percentage <- (range_points/total_points)*100
    
    # Create name using the current range 
    range_name <- paste0("percentage_", min_h, "_", max_h, "m")
    results[[range_name]] <- round(percentage, 4)
    
  }
  
  return(results)
}

vegetation_percentage_band <- cloud_metrics(nlas, ~f_vegetation_percentage_band(Z, ranges = list(c(0, 0.75),c(0.75, 1), c(1, 2), c(2, 9))))
print(vegetation_percentage_band)


#### Canopy Cover: Intensity ####

# Canopy cover

f_canopy_cover_intensity <- function(z, intensity, threshold = 2){
  
  z <- as.numeric(z)
  intensity <- as.numeric(intensity)
  
  #Total intensity
  total_intensity <- sum(intensity)
  
  # Filter intensity by canopy points
  canopy_mask <- z>=threshold
  ci<- intensity[canopy_mask]
  canopy_intensity <- sum(ci)
  
  # Calculate Canopy cover
  
  canopy_cover = canopy_intensity/total_intensity
  
  return(list(canopy_cover_intensity = canopy_cover,
              canopy_cover_intensity_pct = round(canopy_cover * 100, 2)))
  
  
}

canopy_cover_intensity <- cloud_metrics(nlas, ~f_canopy_cover_intensity(Z, Intensity ,threshold = 2 ))
print(canopy_cover_intensity)

# Canopy cover at multiple thresholds

f_canopy_cover_intensity_multi <- function(z, intensity, threshold = c(0.75,1,2)){
  
  z <- as.numeric(z)
  intensity <- as.numeric(intensity)
  
  #Total intensity
  total_intensity <- sum(intensity)
  
  #Calculate for each threshold
  results <- list()
  for(thresh in threshold){
  
  # Filter intensity by canopy points
  canopy_mask <- z>=thresh
  ci<- intensity[canopy_mask]
  canopy_intensity <- sum(ci)
  
  # Calculate Canopy cover
  
  canopy_cover = canopy_intensity/total_intensity
  
  results[[paste0("intensity_", thresh, "m" )]] <- canopy_cover
  results[[paste0("intensity(%)_", thresh, "m" )]] = round(canopy_cover * 100, 2)
  
  }
  return(results)
}

canopy_cover_intensity_multi <- cloud_metrics(nlas, ~f_canopy_cover_intensity_multi(Z, Intensity))
print(canopy_cover_intensity_multi)

#### Canopy cover by intensity for height threshold bands (cover of vegetation) ####

f_vegetation_intensity_percentage_band <- function(
    z, intensity,
    ranges = list(c(0, 0.75), c(0.75, 1), c(1, 2), c(2, 9))
) {
  
  z <- as.numeric(z)
  intensity <- as.numeric(intensity)
  
  total_intensity <- sum(intensity, na.rm = TRUE)
  
  # Initialize as a LIST (not numeric vector)
  results <- list()
  
  # If no intensity in this cell, return empty list with NA values
  if (total_intensity == 0) {
    for (i in seq_along(ranges)) {
      min_h <- ranges[[i]][1]
      max_h <- ranges[[i]][2]
      results[[paste0("percentage_", min_h, "_", max_h, "m")]] <- NA
    }
    return(results)
  }
  
  for (i in seq_along(ranges)) {
    min_h <- ranges[[i]][1]
    max_h <- ranges[[i]][2]
    
    band_intensity <- sum(intensity[z >= min_h & z < max_h], na.rm = TRUE)
    
    percentage <- (band_intensity / total_intensity) * 100
    
    # Store in list with proper naming
    results[[paste0("percentage_", min_h, "_", max_h, "m")]] <- round(percentage, 4)
  }
  
  return(results)
}

vegetation_intensity_percentage_band <- cloud_metrics(nlas, ~f_vegetation_intensity_percentage_band(Z, Intensity,ranges = list(c(0, 0.75),c(0.75, 1), c(1, 2), c(2, 9))))
print(vegetation_intensity_percentage_band)

#### Intensity of returns
#### LiDAR-derived Height Diversity Index (LHDI) ####

lhdi<- cloud_metrics(nlas, func = ~lid_lhdi(Z, interval = 0.5), res = 1) 

#### Canopy Relief Ratio (CRR) ####

## CRR

f_canopy_relief_ratio <- function(z, threashold = 2 ){
  z <- as.numeric(z)
  
  # Filter out canopy points
  heights <- z[z >= threashold]
  
  # Calculate mean height
  H_mean <- mean(heights)
  
  # Get minimum height
  H_min <- min(heights)
  
  # Get maximum height
  H_max <- max(heights)
  
  # Calculating Canopy relief ratio
  CRR = (H_mean - H_min)/(H_max - H_min)
  
  return(CRR)
  
}

canopy_relief_ratio <- cloud_metrics(nlas, func = ~f_canopy_relief_ratio(Z, threashold = 2))
print(canopy_relief_ratio)

# CRR at multiple thresholds

f_canopy_relief_ratio_multi <- function(z, thresholds = c(0.75,1,2) ){
  z <- as.numeric(z)
  
  results <- list()
  for (thresh in thresholds){

  # Filter out canopy points
  heights <- z[z >= thresh]
  
  # Calculate mean height
  H_mean <- mean(heights)
  
  # Get minimum height
  H_min <- min(heights)
  
  # Get maximum height
  H_max <- max(heights)
  
  # Calculating Canopy relief ratio
  CRR = (H_mean - H_min)/(H_max - H_min)
  
  results[[paste0("CRR_", thresh, "m")]] <- CRR
  }
  
  return(results)
  
}

canopy_relief_ratio_multi <- cloud_metrics(nlas, func = ~f_canopy_relief_ratio_multi(Z))
print(canopy_relief_ratio_multi)

#### Testing functions from "leafR" ####

lad_voxel <- lad.voxels("data/output/W57D_4_norm.las", grain.size = 1, k = 1)

lad_profile <- lad.profile(lad_voxel, relative = TRUE)

# Leaf area index

lai <- lai(lad_profile, min = 1, max = 9)

# Foliage height density: calculated from abundances considered as per-voxel relative LAD values

fhd <- FHD(lad_profile, evenness = TRUE, LAD.threshold = -1)

# Gini coefficient of foliage structural diversity

gc_fsd <- GC("data/output/W57D_4_norm.las", threshold = 1)

# Gini-Simpson index of foliage structural diversity

gs_fsd <- GS(lad_profile, evenness = TRUE, LAD.threshold = -1)

# Leaf area height volume:  cumulative product of canopy height and vegetation density

LAHV(lad_profile, LAI.weighting = FALSE, height.weighting = FALSE)

