# PURPOSE -------------------------------------------------------------------------------------------------------------------
# 1) To create a master strata table for EBS crab stocks up through 2024 using the strata tables generated in SQL for the 2024 survey year.  
#    The table this script creates will have each new year's data appended to it in [separate script TBD], and the new strata tables for 
#    each successive year will be created from the ongoing master strata table.
#
# Author: Shannon Hennessey, NOAA-AFSC

rm(list=ls())

# INSTALL PACKAGES ----------------------------------------------------------------------------------------------------------
# install.packages(c("tidyverse"))


# LOAD PACKAGES -------------------------------------------------------------------------------------------------------------
library(tidyverse)


# LOAD AND PROCESS DATA -----------------------------------------------------------------------------------------------------

# haul_table <- read.csv("Y:/KOD_Survey/EBS Shelf/2024/Tech Memo/Data/Haul Data/haul_newtimeseries.csv", header = TRUE)

# need this section of code for now to update haul table, will be worked into haul table generation in the future
haul_table <- haul_table %>%
  mutate(DATE = format(strptime(START_TIME, format = "%d-%b-%y"), "%Y-%m-%d")) %>%
  dplyr::arrange(DATE) %>%
  group_by(SURVEY_YEAR, GIS_STATION) %>%
  mutate(num_dups = n(), 
         dup_id = row_number()) %>% 
  ungroup() %>%
  mutate(is_duplicated = (dup_id == 1 & num_dups == 2)) %>%
  filter(!is_duplicated) %>%
  dplyr::select(!c(DATE, num_dups, dup_id, is_duplicated))

# # check assigned duplicates
# # unique(duplicates$DATE[which(duplicates$dup_id == 1 & duplicates$num_dups == 2)]) # initial tows
# # unique(duplicates$DATE[which(duplicates$dup_id == 2)]) # retows
# unique(duplicates$DATE[which(duplicates$is_duplicated == TRUE)])

# set file path
path <- "Y:/KOD_Survey/EBS Shelf/Data_Processing/Data/legacy_strata_tables/"
strata_files <- list.files(path, full.names = TRUE)

# create empty object to bind input files to
strata_table <- c()
stations <- read.csv("Y:/KOD_Survey/EBS Shelf/Data_Processing/Data/lookup_tables/survey_stations.csv")
dist_stations <- read.csv("Y:/KOD_Survey/EBS Shelf/Data_Processing/Data/lookup_tables/district_stations.csv")

# loop across species strata tables, ass a SPECIES id column, bind rows
for(i in 1:length(strata_files)){
  table <- read.csv(strata_files[i]) # read in file
  table$SPECIES <- strsplit(strata_files[i], "_")[[1]][6] # extract species name from file name
  
  ## district, stratum, area
  # print(unique(table[,c(2, 7, 8)])) 
  
  ## district, year
  # table <- table %>% dplyr::arrange(SURVEY_YEAR)
  # print(unique(table[,c(2, 4)])) 
  
  ## station, district
  table <- table %>% dplyr::arrange(STATION_ID)
  table <- table[which(table$SURVEY_YEAR == 2023),]
  # print(unique(table[,c(1,2)])) # stations in district
  temp_stations <- unique(table[,c(1,2)])
  names(temp_stations) <- c("STATION_ID", strsplit(strata_files[i], "_")[[1]][6])
  stations <- left_join(stations, temp_stations)
  
  strata_table <- rbind(strata_table, table)
}

# write.csv(stations, "Y:/KOD_Survey/EBS Shelf/Data_Processing/Data/lookup_tables/district_stations.csv", row.names = FALSE)

# pivot dataframe so each column with species name is the relevant districts to that species
strata_master <- strata_table %>% pivot_wider(id_cols = c(STATION_ID, SURVEY_YEAR, LATITUDE, LONGITUDE), #, TOWS , STRATUM, TOTAL_AREA
                                              names_from = "SPECIES", values_from = "DISTRICT")
strata_master2 <- strata_table %>% pivot_wider(id_cols = c(STATION_ID, SURVEY_YEAR, LATITUDE, LONGITUDE, STRATUM, TOTAL_AREA),
                                              names_from = "SPECIES", values_from = "DISTRICT")
## matching joined strata table against haul table
# strata_master$GIS_STATION <- strata_master$STATION_ID
# 
# test <- left_join(haul_table2, strata_master, by = c("SURVEY_YEAR", "GIS_STATION"))
# 
# test2 <- test %>%
#   group_by(SURVEY_YEAR) %>%
#   summarise(count=n())
# names(test2) <- c("SURVEY_YEAR", "combo_count")
# 
# strata2 <- strata_master %>%
#   group_by(SURVEY_YEAR) %>%
#   summarise(count=n())
# names(strata2) <- c("SURVEY_YEAR", "strata_count")
# 
# haul2 <- haul_table %>%
#   group_by(SURVEY_YEAR) %>%
#   summarise(count=n())
# names(haul2) <- c("SURVEY_YEAR", "haul_count")
# 
# test2 <- left_join(test2, haul2)
# test2 <- left_join(test2, strata2)


## OK, now that we know this is the master strata table we want to generate, see if can take haul table and add relevant stocks based on each station
# and then can further append TOTAL_AREA, STRATUM, (and TOWS? -- do we need tows?) in the bio/abund script to get the multiplication factors


## NOTES:
# different districts have different total areas.....so to combine strata tables, need a stratum definition for each,
# with a district for each stock (named columns), and then we need a district/area lookup table to join by to get total area 
# for each species....
#
# difference between stratum 10, 15, and 90?? same survey station but different stratum depending on stock....
#
# ok, so, if we don't need strata table until calculating biomass and abundance, maybe we just have a table defining all the strata 
# (for a given year? ie. create timeseries...), 
# and then can combine with the area lookup table and do it that way? ie. need strata timeseries (also defined based on what's in the 
# haul table) but then subset based on stock too, because not all strata are represented for each stock??
#
# strata for the most part have constant total area, stratum ID per stock, and lat/lon -- 2016 on, use start lat/lon as defined by haul table
# rather than the standard # across years...

# WRITE OUTPUT FILE --------------------------------------------------------------------------------------------------------
# output master file
write.csv(strata_master, "Y:/KOD_Survey/EBS Shelf/Data_Processing/Data/strata_table_master.csv", row.names = FALSE)




