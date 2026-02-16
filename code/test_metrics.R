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

# Ground - grund classification works perfectly

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

# Canopy

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
