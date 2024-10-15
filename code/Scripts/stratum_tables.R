# PURPOSE -------------------------------------------------------------------------------------------------------------------
# 1) To create a master strata table for EBS crab stocks up through 2023 using the strata tables generated in SQL for the 2023 survey year.  
#    The table this script creates will have each new year's data appended to it in [separate script TBD], and the new strata tables for 
#    each successive year will be created from the ongoing master strata table.
#
# Author: Shannon Hennessey, NOAA-AFSC

# NOTES:
# Stratum lookup tables for:
# stock, station, district, stratum
# year, district/stratum, spatial area

rm(list=ls())

# INSTALL PACKAGES ----------------------------------------------------------------------------------------------------------
# install.packages(c("tidyverse"))


# LOAD PACKAGES -------------------------------------------------------------------------------------------------------------
library(tidyverse)


# LOAD AND PROCESS DATA -----------------------------------------------------------------------------------------------------

# set file path
path <- "Y:/KOD_Survey/EBS Shelf/Data_Processing/Data/legacy_strata_tables/"
# strata_files <- list.files(path, full.names = TRUE)

print('making strata tables')

strata_rkc <- list.files(path, pattern = "_RKC_NEW", ignore.case = TRUE) %>%
              map_df(~read.csv(paste0(path, .x))) %>%
              mutate(SPECIES = "rkc")

strata_bkc <- list.files(path, pattern = "_BKC_NEW", ignore.case = TRUE) %>%
              map_df(~read.csv(paste0(path, .x))) %>%
              mutate(SPECIES = "bkc")

strata_bairdi <- list.files(path, pattern = "_BAIRDI_NEW", ignore.case = TRUE) %>%
                 map_df(~read.csv(paste0(path, .x))) %>%
                 mutate(SPECIES = "bairdi")

strata_opilio <- list.files(path, pattern = "_OPILIO_NEW", ignore.case = TRUE) %>%
                 map_df(~read.csv(paste0(path, .x))) %>%
                 mutate(SPECIES = "opilio")

strata_hybrid <- list.files(path, pattern = "_HYBRID_NEW", ignore.case = TRUE) %>%
                 map_df(~read.csv(paste0(path, .x))) %>%
                 mutate(SPECIES = "hybrid")

strata_ei <- list.files(path, pattern = "_EI_NEW", ignore.case = TRUE) %>%
             map_df(~read.csv(paste0(path, .x))) %>%
             mutate(SPECIES = "ei")


strata_master <- read.csv("Y:/KOD_Survey/EBS Shelf/Data_Processing/Data/strata_table_master.csv")


rm(path)
                

## Look at differences in stations sampled vs. area expanded over
# Load haul table
haul_table <- read.csv("Y:/KOD_Survey/EBS Shelf/2024/Tech Memo/Data/haul_newtimeseries.csv") 
n_stations <- haul_table %>% filter(!HAUL_TYPE == 17) %>% group_by(SURVEY_YEAR) %>% summarize(N_grid = n()) %>%
              left_join(strata_bairdi %>% 
                          mutate(N_area = TOTAL_AREA/401) %>% 
                          group_by(SURVEY_YEAR) %>% 
                          summarize(N_stratum = n())) %>% 
              mutate(N_diff = N_grid - N_stratum)

strata_all <- rbind(strata_rkc, strata_bkc, strata_bairdi, strata_opilio, strata_hybrid, strata_ei)
differences <- strata_all %>% 
               mutate(N_area = TOTAL_AREA/401) %>% 
               group_by(SPECIES, SURVEY_YEAR, DISTRICT, N_area) %>% 
               summarize(N_station = n()) %>% 
               mutate(N_diff = N_area - N_station)

## OK so, for the most part (excluding "unstratified"), all strata have a fixed area
## This fixed area changes from year to year in earlier years, but after a certain point, becomes constant
# RKC:
#   -- BB: 1980+: 136 stations (except for 1982, 135)
#   -- Prib MTCA: est. 1981, always 25 stations
#   -- Prib single: 1981-2023, 36 stations, 2024 = 61

# BKC:
#   -- Prib MTCA: est. 1981, always 25 stations
#   -- Prib single: 1981-2023, 45 stations, 2024 = 70
#   -- St Matt MTCA: est. 1983, always 18 stations
#   -- St Matt Single: 1987+: 29 stations; 2024: 47

# BAIRDI:
#   -- E166: 1980+: 120 stations (except for 1981, 104)
#   -- W166: 1988-1991, 1993-2023: 187 stations; 1992: 169; 2024: 230
#   -- Prib MTCA: est. 1981-2023, always 25 stations
#   -- St Matt MTCA: est. 1983-2023, always 18 stations

# OPILIO: (start 1980)
#   -- Prib MTCA: est. 1981, always 25 stations
#   -- St Matt MTCA: est. 1983, always 18 stations
#   -- Single: 1993+: 307 stations, also 1988-1991, but 1992 = 289; 2024 = 350

# HYBRID: (start 1980)
#   -- Prib MTCA: est. 1981, always 25 stations
#   -- St Matt MTCA: est. 1983, always 18 stations
#   -- Single: 1993+: 307 stations, also 1988-1991, but 1992 = 289; 2024 = 350

# EI:
#   -- BB: 1980+: 136 stations (except for 1982, 135)
#   -- Prib MTCA: est. 1981, always 25 stations
#   -- Prib single: 1981-2023, 36 stations, 2024 = 61


#       1975: 1/136: Pribs (TannerW, Prib Single bkc ei rkc)
#       1976: 0/214
#       1977: 0/155
#       1978: 1/230: St Matt (TannerW, St Matt single bkc)
#       1979: 0/307
#       1980: 1/320: BB (Tanner E, single hybrid opilio, BB rkc ei; start opilio, hybrid TS)
#       1981:  /305 (PribMTCA established)
#       1982:  /342
#       1983:  /353 (StMattMTCA established)



# haul_table_OLD <- read.csv("Y:/KOD_Survey/EBS Shelf/2023/Tech Memo/Data/Haul Data/haul_newtimeseries.csv", header = TRUE)

# # need this section of code for now to update haul table, will be worked into haul table generation in the future
# haul_table <- haul_table %>%
#               mutate(DATE = format(strptime(START_TIME, format = "%d-%b-%y"), "%Y-%m-%d")) %>%
#               dplyr::arrange(DATE) %>%
#               group_by(SURVEY_YEAR, GIS_STATION) %>%
#               mutate(num_dups = n(), 
#                      dup_id = row_number()) %>% 
#               ungroup() %>%
#               mutate(is_duplicated = (dup_id == 1 & num_dups == 2)) %>%
#               filter(!is_duplicated) %>%
#               dplyr::select(!c(DATE, num_dups, dup_id, is_duplicated))

# ** if AZ0504 --> rename Z-04

## could all 'TOWS' column back in for retow stations if needed, somewhere within script...


# Read in master strata table
# Create and append rows for new year
# Reformat for individual species and save outputs for AKFIN
# -- can we use just lookups for the bio//abund/cpue script at some point? Can at least use combined stratum table for now (1 file)
# ---- ^^ caveat, early years - area_sampled really hard to define....need to think about more (see notes below)

## PRIBILOF SINGLE/MTCA:
# for ALL species, if between 1975-1980, 2024+, PribMTCA --> Prib single
# (Prib MTCA only for stations designated that in 'stock_stations_lookup.csv' between 1981-2023)

## St MATT SINGLE/MTCA:
# for BOK, tanner, snow, hybrid, if between 1976-1982, 2024+, StMattMTCA --> StMatt single
# (StMatt MTCA only for stations designated that in 'stock_stations_lookup.csv' between 1983-2023)



### SPECIES-SPECIFIC TIMESERIES/DISTRICT NOTES:
# 
## BAIRDI:
# - E166:
#       1975: 76 stations
#       1976: 106 stations
#       1977: 75 stations
#       1978: 89 stations
#       1979: 119 stations
#       1980: 120 stations (FINAL GRID)
#       1981: 104 stations (??)
#       1982+ --> 120 stations, constant area
#   
# - W166:
#       1975: 61 stations
#       1976: 108 stations
#       1977: 80 stations
#       1978: 142 stations
#       1979: 188 stations
#       1980: 201 stations
#       1981: 168 stations (PribMTCA established)
#       1982: 185 stations
#       1983: 165 stations (StMattMTCA established)
#       1984: 167 stations
#       1985: 166 stations
#       1986: 166 stations
#       1987: 174 stations
#       1988+ --> 187 stations, constant area
#         exception: 1992 - 169 stations (??)
#   
# - PribMTCA: started in 1981, 25 stations (plus 16 corners), constant area
# 
# - StMattMTCA: started in 1983, 18 stations (plus 10 corners), constant area


## BKC:
# - Prib Single:
#       1975: 46 stations
#       1976: 59 stations
#       1977: 58 stations
#       1978: 58 stations
#       1979: 58 stations
#       1980: 70 stations
#       1981+ --> 45 stations (PribMTCA established)
# 
# - PribMTCA: started in 1981, 25 stations (plus 16 corners), constant area
# 
# - StMatt Single:
#       1975: ns
#       1976: 12 stations
#       1977: ns
#       1978: 40 stations
#       1979: 37 stations
#       1980: 38 stations
#       1981: 30 stations (PribMTCA established)
#       1982: 40 stations
#       1983: 27 stations (StMattMTCA established)
#       1984: 27 stations
#       1985: 26 stations
#       1986: 27 stations
#       1987+ --> 29 stations, constant area
# 
# - StMattMTCA: started in 1983, 18 stations (plus 10 corners), constant area
# 
# - BKC Unstr:
#       1975: 1 station
#       1976: 3 stations
#       1977: ns
#       1978: 1 station
#       1979: 1 station
#       1980: 13 stations
#       1981: 4 stations (PribMTCA established)
#       1982: 7 stations
#       1983: 2 stations (StMattMTCA established)
#       1984: 3 stations
#       1985: ns
#       1986: 2 stations
#       1987: 1 station 
#       1988: 6 stations
#       1989: 3 stations
#       1990: 3 stations
#       1991: 4 stations
#       1992: 4 stations
#       1993: 5 stations
#       1994: 5 stations
#       1995: 4 stations
#       1996: 5 stations
#       1997: 1 station
#       1998: 2 stations
#       1999: 3 stations
#       2000: 3 stations
#       2001: 3 stations
#       2002: 3 stations
#       2003: 3 stations
#       2004: 2 stations
#       2005: 2 stations
#       2006: 1 station
#       2007: ns
#       2008: 2 stations
#       2009: 2 stations
#       2010: 6 stations
#       2011: 3 stations
#       2012: 5 stations
#       2013: 9 stations
#       2014: 2 stations
#       2015: 1 station
#       2016: 4 stations
#       2017+ --> not sampled

## RKC: 
# - BB: 
#       1975: 98 stations
#       1976: 129 stations
#       1977: 98 stations
#       1978: 115 stations
#       1979: 135 stations ..
#       1980: 136 stations
#       1981: 136 stations
#       1982: 135 stations
#       1983+ --> 136 stations, constant area
# 
# - Prib Single:
#       1975: 39 stations
#       1976: 51 stations
#       1977: 51 stations
#       1978: 51 stations
#       1979: 49 stations
#       1980: 61 stations
#       1981+ --> 36 stations, constant area (PribMTCA established)
# 
# - PribMTCA: started in 1981, 25 stations (plus 16 corners), constant area 
# 
# - Nothern Unstr: highly variable, every year since 1980


## OPILIO:
# - Single: ** ARE SOME STATIONS MISSING??? Shouldn't it be 375-25-18...but getting 68 (42 if account for actual corners) left over
#       1980: 321 stations
#       1981: 272 stations (PribMTCA established)
#       1982: 305 stations
#       1983: 285 stations (StMattMTCA established)
#       1984: 287 stations
#       1985: 285 stations
#       1986: 285 stations
#       1987: 294 stations
#       1988+ --> 307 stations, constant area
#         exception: 1992 - 289 stations (??)
# 
# - PribMTCA: started in 1981, 25 stations (plus 16 corners), constant area
# 
# - StMattMTCA: started in 1983, 18 stations (plus 10 corners), constant area
# 


## HYBRID (same as OPILIO):
# - Single:
# 
# - PribMTCA: started in 1981, 25 stations (plus 16 corners), constant area
# 
# - StMattMTCA: started in 1983, 18 stations (plus 10 corners), constant area


## EI (same as RKC):
# - BB: 
#       1975: 98 stations
#       1976: 129 stations
#       1977: 98 stations
#       1978: 115 stations
#       1979: 135 stations ..
#       1980: 136 stations
#       1981: 136 stations
#       1982: 135 stations
#       1983+ --> 136 stations, constant area
# 
# - Prib Single:
#       1975: 39 stations
#       1976: 51 stations
#       1977: 51 stations
#       1978: 51 stations
#       1979: 49 stations
#       1980: 61 stations
#       1981+ --> 36 stations, constant area (PribMTCA established)
# 
# - PribMTCA: started in 1981, 25 stations (plus 16 corners), constant area 
# 
# - Nothern Unstr: highly variable, every year since 1980








print("updated stratum tables")

## OK,  see if can take haul table and add relevant stocks based on each station
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
# write.csv(strata_master, "Y:/KOD_Survey/EBS Shelf/Data_Processing/Data/strata_table_master.csv", row.names = FALSE)
print("stratum tables written for Oracle and Tech Memo")
