#### Generating LiDAR metrics ####

setwd("C:/Git repository/Masters_Tim")

# Load packages

library(lidR)
library(rgl)
library(gstat)
library(tidyverse)
install.packages("silviculture")
library(silviculture)
library(sf)
# Loading in data 

las <- readLAS("data/raw/2021/W57D_4.las")
print(las)

las_check(las)

##### Canopy Height #### 

nlas <- normalize_height(las, tin())

plot(nlas, color = ("Classification"))

## Exploration 

# Ground - ground classification works perfectly

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

# Canopy metrics #

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

# Pixel metrics 

pmet <- pixel_metrics(nlas, .stdmetrics, 10, filter = ~Classification == 1) 
plot(pmet, col = height.colors(50))
names(pmet)

plot(pmet$zmean)


# Voxel metrics

?voxel_metrics

vmet <- voxel_metrics(nlas, .stdmetrics, 10, filter = ~Classification == 1)

plot(vmet$zmean)

# Standard deviation of height for the plot

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
  
  # Calculate standard deviation using the formula
  n <- length(canopy_heights)
  sigma_H <- sqrt(sum((canopy_heights - H_bar)^2) / (n - 1))
  
  return(list(
    canopy_sd   = round(sigma_H, 3),
    canopy_mean = round(H_bar, 3),
    n_canopy    = n
  ))
}

# Apply to whole plot
height_sd_result <- cloud_metrics(nlas, ~f_canopy_height_sd(Z, threshold = 2))
print(height_sd_result)

#### Canopy height density (CHD) ####

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

#### Symmetry of canopy ####
#Need to work on this

f_canopy_symmetry <- function(z, threshold = 2) {
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
  sym <- sum((canopy_heights - H_bar)/(sd_val^3*(n-1)))
  
  return(sym)
}

canopy_symmetry <- cloud_metrics(nlas, ~f_canopy_symmetry(Z, threshold = 2))
print(canopy_symmetry)

#### Canopy Cover of plot ####
# For all these you need to set the threshold height for what you consider the canopy

# Online function

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

# Canopy cover 

f_canopy_cover <- function(z, threshold = 0.75) {
  z <- as.numeric(z)
  
  canopy_points <- sum(z >= threshold)   # ∑ CHM_canopy
  total_points  <- length(z)             # ∑ CHM_total
  
  cover <- canopy_points / total_points  # Canopy Cover ratio (0–1)
  
  return(list(
    canopy_cover    = cover,
    canopy_cover_pct = round(cover * 100, 2)  # as a percentage
  ))
}

cover_result <- cloud_metrics(nlas, func = ~f_canopy_cover(Z))
print(cover_result)

# Canopy cover - 1st returns (this works the same as the online)

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


#### LiDAR-derived Height Diversity Index (LHDI) ####

lhdi<- cloud_metrics(nlas, func = ~lid_lhdi(Z, interval = 0.5), res = 1) 

####  Canopy Relief Ratio (CRR) ####

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
