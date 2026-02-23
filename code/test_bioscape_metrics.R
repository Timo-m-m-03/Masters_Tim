
setwd("C:/Git repository/Masters_Tim")

# Loading packages 

library(lidR)
library(data.table)
library(sf)

# Reading in data

raw_lines <- readLines("data/LVISF2_BioSCape2023_1113_R2404_039467.txt")
h_no <- grep("LFID", raw_lines)
col_names <- strsplit(trimws(gsub("^#\\s*", "", raw_lines[h_no])), "\\s+")[[1]]
lvis_dat <- fread("data/LVISF2_BioSCape2023_1113_R2404_039467.txt", skip = h_no, header = FALSE, fill = TRUE)
setnames(lvis_dat, col_names)

lvis_sf <- st_as_sf(lvis_dat, coords = c("GLON", "GLAT"), crs = 4326)


head(lvis_dat)

# Plotting

plot(lvis_sf["ZG])
