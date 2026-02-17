#### Generating LiDAR metrics ####

setwd("C:/Git repository/Masters_Tim")

# Load packages

library(lidR)
library(rgl)
library(gstat)
library(tidyverse)

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

#### Canopy Cover of plot ####
# For all these you need to set the threashold height for what you consider the canopy

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

