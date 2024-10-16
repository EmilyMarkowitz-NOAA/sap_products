
# PURPOSE -------------------------------------------------------------------------------------------------------------------
# To create and export all output files specifically formatted for AKFIN
# Files include:
# - (#) lookup tables
# - (7) Strata tables
# - (6) CrabHaul tables
# ... more notes to be filled in as script is developed
# OTHERS THAT I NEED TO FIGURE OUT HOW TO PRODUCE: 
#   "ebscrab_size_group.csv"
#   "ebscrab_size_group_district.csv"
#
# Author: Shannon Hennessey, NOAA-AFSC


# add line to write to .csv and bundle with AKFIN products?
# 'ebscrab_chela_cutline_param.csv'




## SPECIES lookup table ------------------------------------------------------------ 
species_lookup <- tibble(SPECIES_ID = c(1:6),
                         SPECIES_CODE = c(68560,
                                          68580,
                                          68590,
                                          69322,
                                          69323,
                                          69400),
                         SPECIES_NAME = c("Bairdi Tanner Crab",
                                          "Opilio Crab",
                                          "Hybrid Tanner Crab",
                                          "Red King Crab",
                                          "Blue King Crab",
                                          "Horsehair Crab"))

# add lines to add LOAD_DATE column (in DD-Mon-YY; does this get updated??), write to .csv, and bundle with AKFIN products?
# 'ebscrab_species.csv'


## STRATUM lookup table ------------------------------------------------------------ 
stratum_lookup <- tibble(STRATUM_ID = c(1:3, 5, 7:14, 17),
                         STRATUM_NAME = c("BKC Unstratified",
                                          "Between 166W and 173W Single",
                                          "Bristol Bay",
                                          "East 166",
                                          "Northern Unstratified",
                                          "Pribilof MTCA",
                                          "Pribilof Single",
                                          "Single",
                                          "St. Matthew MTCA",
                                          "St. Matthew MTCA East",
                                          "St. Matthew Single",
                                          "West 166",
                                          "Multiple"))

# add lines to add LOAD_DATE column (in DD-Mon-YY; does this get updated??), write to .csv, and bundle with AKFIN products?
# 'ebscrab_stratum.csv'

## STRATUM DISTRICT lookup table ------------------------------------------------------------ 
## NOTE: We don't really use the numieric ID columns for anything....
stratum_dist_lookup <- tibble(STRATUM_DISTRICT_ID = c(1:2, 6:24, 27:32),
                              SPECIES_ID = c(5, 1, 6, 4, 1, 4, 5, 6, 2, 3, 4, 5, 6, 2, 3, 1, 5, 6, 2, 3, 5, 2, 3, 1, 1, 6, 4),
                              STRATUM_ID = c(1, 2, rep(7,2), rep(8,6), rep(9,3), rep(10,2), rep(11,5), 13, rep(17,2), 5, 14, rep(3,2)),
                              DISTRICT_ID = c(7, 1, 9, 9, 8, 5, 5, 5, 2, 2, 5, 5, 5, 2, 2, 8, 6, 6, 2, 2, 6, 2, 2, 4, 8, 3, 3)) 

# join with species_lookup, stratum_lookup, and dist_lookup to get SPECIES_CODE, STRATUM_NAME, and DISTRICT_CODE
stratum_dist_lookup <- stratum_dist_lookup %>%
  left_join(species_lookup) %>%
  left_join(stratum_lookup) %>%
  left_join(dist_lookup) %>%
  select("STRATUM_DISTRICT_ID",
         "SPECIES_ID",
         "SPECIES_CODE",
         # "SPECIES_NAME",
         "STRATUM_ID",
         "STRATUM_NAME",
         "DISTRICT_ID",
         "DISTRICT_CODE")

# add lines to add LOAD_DATE column (in DD-Mon-YY; does this get updated??), write to .csv, and bundle with AKFIN products?
# 'ebscrab_stratum_district.csv'


## WEIGHT AREA lookup table ------------------------------------------------------------ 
weight_area_lookup <- tibble(WEIGHT_AREA_ID = c(3, 5, 4, 7, 2, 1, 6),
                             SPECIES_ID = c(1:5, 5:6),
                             SPECIES_CODE = c(68560,
                                              68580,
                                              68590,
                                              69322,
                                              69323,
                                              69323,
                                              69400),
                             MIN_LATITUDE = c(rep(0, 4), 58.65, rep(0, 2)),
                             MAX_LATITUDE = c(rep(90, 5), 58.65, 90),
                             WEIGHT_AREA = c(rep("ALL", 4), "STMATT", "PRIB", "ALL")) 

# add lines to add LOAD_DATE column (in DD-Mon-YY; does this get updated??), write to .csv, and bundle with AKFIN products?
# 'ebscrab_weight_area.csv'


## WEIGHT REGRESSION lookup table ------------------------------------------------------------ 
weight_reg_lookup <- tibble(WEIGHT_REG_ID = c(31:33, 37:39, 34:36, 22:24, 28:30, 25:27, 40:42),
                            MIN_YEAR = c(1975),
                            MAX_YEAR = c(NA),
                            SPECIES_ID = rep(c(1:5, 5:6), each = 3),
                            # SPECIES_CODE = rep(c(68560,
                            #                      68580,
                            #                      68590,
                            #                      69322,
                            #                      69323,
                            #                      69400), each = 3),
                            WEIGHT_AREA_ID = rep(c(3, 5, 4, 7, 2, 1, 6), each = 3),
                            SEX = rep(c(1, 2, 2), 7),
                            MIN_CLUTCH_SZ = rep(c(NA, 0, 2), 7),
                            MAX_CLUTCH_SZ = rep(c(NA, 1, NA), 7),
                            OVIGEROUS = rep(c(NA, "NONOVIG", "OVIG"), 7),
                            VARIABLE_A = c(0.00027,
                                           0.000562,
                                           0.000441,
                                           0.000267,
                                           0.001047,
                                           0.001158,
                                           0.000267,
                                           0.001047,
                                           0.001158,
                                           0.000403,
                                           0.000408,
                                           0.003593,
                                           0.000502,
                                           0.02065,
                                           0.02065,
                                           0.000508,
                                           0.02065,
                                           0.02065,
                                           0.00071731,
                                           0.001194533,
                                           0.001194533),
                            VARIABLE_B = c(3.022134,
                                           2.816928,
                                           2.898686,
                                           3.097253,
                                           2.708367,
                                           2.708793,
                                           3.097253,
                                           2.708367,
                                           2.708793,
                                           3.141334,
                                           3.127956,
                                           2.666076,
                                           3.107158,
                                           2.27,
                                           2.27,
                                           3.106409,
                                           2.27,
                                           2.27,
                                           3.02,
                                           2.86,
                                           2.86)) 


# add weight_area, species_code
# join with weight_area_lookup and species_lookup to get WEIGHT_AREA and SPECIES_CODE
weight_reg_lookup <- weight_reg_lookup %>%
  left_join(weight_area_lookup) %>%
  left_join(species_lookup) %>%
  select("WEIGHT_REG_ID",
         "MIN_YEAR",
         "MAX_YEAR",
         "SPECIES_ID",
         "SPECIES_CODE",
         "WEIGHT_AREA_ID",
         "WEIGHT_AREA",
         "SEX",
         "MIN_CLUTCH_SZ",
         "MAX_CLUTCH_SZ",
         "OVIGEROUS",
         "VARIABLE_A",
         "VARIABLE_B")

# add lines to add LOAD_DATE column (in DD-Mon-YY; does this get updated??), write to .csv, and bundle with AKFIN products?
# 'ebscrab_weight_regression.csv'







# # STRATA TABLES ------------------------------------------------------------------------------------------------------------
# ## LOAD DATA ----------------------------------------------------------------------------------------------------------------
# # set year
# current_year <- 2024
# 
# # load master strata table from previous year
# strata_master <- read.csv(paste0("./Data/strata_table_master_", current_year - 1, ".csv"))
# 
# # load haul table
# haul <- read.csv(paste0("Y:/KOD_Survey/EBS Shelf/", current_year, "/Tech Memo/Data/Haul Data/haul_newtimeseries.csv"))
# 
# 
# ## CREATE NEW MASTER STRATA TABLE -------------------------------------------------------------------------------------------
# # replicate strata from previous year for current year
# new_strata <- strata_master %>% 
#               filter(SURVEY_YEAR == current_year - 1) %>%
#               mutate(SURVEY_YEAR = current_year) %>% 
#               filter(STATION_ID %in% unique(haul$STATIONID)) # subset to make sure only including stations that are represented in haul table
# ## SH note: may need to specify haul types too, depending on if we want strata tables with only good hauls?
# 
# # re-append that year if no changes to sampling stations
# strata_master_new <- rbind(strata_master, new_strata)
# 
# # paste year in output file, so we know when it's been last updated (and have record of the one used in previous years....)
# write.csv(strata_master_new, paste0("./Data/strata_table_master_", current_year, ".csv"), row.names = FALSE)
# 
# 
# ## OUTPUT SPECIES-SPECIFIC STRATA TABLES -------------------------------------------------------------------------------------------
# # select all species
# species <- tail(colnames(strata_master_new), 6)
# labs <- c("STATION_ID", "TOWS", "SURVEY_YEAR", "LATITUDE", "LONGITUDE", "STRATUM", "TOTAL_AREA")
# new_labs <- c("STATION_ID", "DISTRICT", "TOWS", "SURVEY_YEAR", "LATITUDE", "LONGITUDE", "STRATUM", "TOTAL_AREA")
# 
# # subset table by species, write .csv
# for(i in 1:length(species)){
#   table <- strata_master_new[, c(labs, species[i])]
#   table <- table[,c(1, 8, 2:7)] # reorder columns to match previous strata tables
#   colnames(table) <- new_labs
#   
#   # remove rows with DISTRICT == NA
#   table <- table[-which(is.na(table$DISTRICT)),]
#   
#   write.csv(table, paste0("./Output/strata_tables/strata_", species[i], "_newtimeseries.csv"), row.names = FALSE)
#   
#   # if 'bairdi', subset to create additional strata table just between -166 and -176 LONGITUDE
#   if(species[i] == "bairdi"){
#     # subset LONGITUDE range
#     table_sub <- table[which(table$LONGITUDE <= -166 & table$LONGITUDE >= -173),]
#     
#     # include 2 L-25 observations that have a LONGITUDE < -173.05 (small variation in coordinates for 2018 and 2022)
#     L25 <- table[which(table$STATION_ID == "L-25" & table$LONGITUDE < -173),]
#     table_sub <- rbind(table_sub, L25)
#     
#     # remove 2 extraneous stations that have just a handful of year-dependent observations within the LONGITUDE range
#     table_sub <- table_sub[-which(table_sub$STATION_ID %in% c("NM2524", "QP2524")),] 
#     
#     # ** OR, could I just make a list of which stations we need and subset the df based on those?? 
#     #    Might be more consistent from year to year if there are any other slight variations in station location outside the 166-173W
#     
#     # rename district for consistency with previous file iterations
#     table_sub$DISTRICT <- ifelse(table_sub$DISTRICT == "West 166", "Between 166W and 173W", table_sub$DISTRICT)
#     
#     # save file output
#     write.csv(table_sub, paste0("./Output/strata_tables/strata_", species[i], "_betw166173_newtimeseries.csv"), row.names = FALSE)
#   } # end O. bairdi subset loop
# }
