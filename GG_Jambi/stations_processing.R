# loading required packages====
library(vegan)
library(DBI)
library(RSQLite)
library(SDMTools)
library(rtf)
library(spatial.tools)
library(ggplot2)
library(plyr)
library(grid)
library(tiff)
library(RColorBrewer)
library(rasterVis)
library(reshape2)
library(foreign)
library(dplyr)
library(gridExtra)
library(pracma)
library(rgeos)
library(zoo)
library(RPostgreSQL)
library(rpostgis)
library(magick)
library(stringr)




input_dir <- "D:/GG_Jambi/Ecosystem/Hydrology/data/77289_2018-08-02-04-23-11/"
setwd(input_dir)

station_data <- list.files(pattern = "^weatherdata", full.names = FALSE, recursive = FALSE)
stat_coordinates <- data.frame(stat_name = "", x_coord=0, y_coord = 0, stringsAsFactors = FALSE)
# column names "Longitude" and "Latitude" to be kept along with the station code
for(s in 1:length(station_data)){
  station_table <- read.csv(station_data[s], row.names = NULL)
  names(station_table) <- colnames(station_table)[2:ncol(station_table)]
  station_table <- unique(station_table[, c("Longitude", "Latitude")])
  station_table$stat_name <- paste0(gsub("weatherdata--", "", station_data[s]))
  station_table <- station_table[, c("stat_name", "Longitude", "Latitude")]
  names(station_table)[2:3] <- c("x_coord", "y_coord")
  station_table$x_coord <- as.numeric(station_table$x_coord)
  station_table$y_coord <- as.numeric(station_table$y_coord)
  stat_coordinates <- data.frame(rbind(stat_coordinates, station_table), stringsAsFactors = FALSE)
}

write.csv(stat_coordinates[2:nrow(stat_coordinates),], "D:/GG_Jambi/Ecosystem/Hydrology/data/compiled_coordinates.csv", row.names = FALSE)

# selecting csv files contained within the watershed boundary

# INPUT====

 