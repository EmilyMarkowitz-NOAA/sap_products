# PURPOSE -------------------------------------------------------------------------------------------------------------------
# To automate processing specimen tables and catch summaries of crab stocks from the NMFS summer Bering Sea bottom trawl survey
# -- append new specimen data to EBSCRAB.EBSCRAB and re-upload to Oracle
# -- create a Catch Summary table for the current year
# -- produce CrabHaul files for each species (append current year), and save for Tech Memo uses as well as upload to Oracle for AKFIN 

# Authors: Shannon Hennessey, NOAA-AFSC

# LOAD DATA -----------------------------------------------------------------------------------------------------------------

print("loading data")

# # Read in haul table
# ## make this depend on 'current_year' so we're getting most current version
# ## or set using data_dir and work out of most current version always in the Data_Processing folder
# haul_table <- read.csv("Y:/KOD_Survey/EBS Shelf/2023/Tech Memo/Data/Haul Data/haul_newtimeseries.csv") 
# # ^^ actually shouldn't have to do this once we start using the 'main' script? will already be loaded....

# Load haul table
if (production) {
  haul_table <- read.csv("Y:/KOD_Survey/EBS Shelf/2024/Tech Memo/Data/haul_newtimeseries.csv")
} else {
  haul_table <- read.csv(here::here("temp/Tables", "haul_table.csv"))
}

## Commented section = old R workflow iteration, going from tablet output .csv to EBSCRAB
## This has changed, and will probably change again with incorporation of GIDES
# # Load summary catch and specimen tables
# catch <- list.files(path, pattern = "_CRAB_CATCH_", recursive = TRUE) %>%
#          purrr::map_df(~read.csv(paste0(path, .x))) %>%
#          dplyr::rename(CATCH_WEIGHT = WEIGHT)
# 
# specimen <- list.files(path, pattern = "_CRAB_SPECIMEN_", recursive = TRUE) %>% 
#                        purrr::map_df(~read.csv(paste0(path, .x))) %>%
#             left_join(., haul_table, by = c("CRUISE", "VESSEL", "HAUL")) 
#             # DO NOT join by STATION, we go by the haul/station definitions from the haul table only



# PROCESS SPECIMEN DATA FOR EBSCRAB ----------------------------------------------------------------------------------------

print("processing specimen data for EBSCRAB")

# Aggregate and reformat specimen tables to append to EBSCRAB.EBSCRAB


# Download most recent version of EBSCRAB.EBSCRAB
if(ORACLE == TRUE){
  print("connecting to Oracle")
  
  # Connect to Oracle
  channel <- gapindex::get_connected(check_access = FALSE) # will need to input SQL Developer username and password
  
  # Download EBSCRAB file from EBSCRAB and save locally
  # - crab_haul for entire time series -- 2.4 GB.....maybe too large to work with??
  # - Keep in Data_Processing folder? and use fread?
  # - Or have an option somewhere (if(xx is TRUE)....) whether we want to connect to Oracle or work locally
  a <- RODBC::sqlQuery(channel, paste0("SELECT * FROM ", "EBSCRAB.EBSCRAB"))
  write.csv(x = a, file = paste0(data_dir, "/EBSCRAB_EBSCRAB.csv"), row.names = FALSE) # save locally for easier processing
  
  print("downloaded EBSCRAB_EBSCRAB")
}

# Load EBSCRAB.EBSCRAB 
ebscrab <- read.csv(paste0(data_dir, "/EBSCRAB_EBSCRAB.csv")) #%>% filter(CRUISE == 202401)

print("loaded EBSCRAB.EBSCRAB")


# Add specimen numbers and append to EBSCRAB table
## is there a systematic way these are added?


# Write to Oracle...
# print("successfully written EBSCRAB to Oracle")


## OLD from tablet .csvs! Specimen table for stock assessment authors just wrangled from EBSCRAB table for now
# # CREATE CATCH SUMMARY TABLE -------------------------------------------------
# print("creating Catch Summary table")
# 
# 
# # Create catch summary table
# catch_summary <- specimen %>%
#                  dplyr::group_by(CRUISE, VESSEL, HAUL, STATION, SPECIES_CODE) %>%
#                  dplyr::reframe(NUMBER_CRAB = sum(SAMPLING_FACTOR)) %>%
#                  dplyr::right_join(., catch %>%
#                                       group_by(CRUISE, VESSEL, HAUL, STATION, SPECIES_CODE) %>%
#                                       dplyr::rename(N_ENTRIES = NUMBER_CRAB) %>%
#                                       dplyr::reframe(N_ENTRIES = sum(N_ENTRIES))) %>%
#                  dplyr::select(CRUISE, VESSEL, HAUL, STATION, SPECIES_CODE, NUMBER_CRAB, N_ENTRIES) %>%
#                  na.omit() # remove bad/gear testing hauls - NA for STATION and # crab
# 
# # Print lines where N_CRAB =/= N_ENTRIES
# discrepancies <- catch_summary %>% 
#                  filter(NUMBER_CRAB != N_ENTRIES) %>%
#                  mutate(DIFF = NUMBER_CRAB - N_ENTRIES)
# 
# if(nrow(discrepancies) > 0){
#   print(paste0(nrow(discrepancies), " discrepancies, check Catch Summary"))
# } 
# 
# 
# # Save catch summary table **FILE NAME??
# # catch_summary %>%
# #   dplyr::select(!N_ENTRIES) %>%
# #   write.csv("./Processed_Trawl_Catch_Summary.csv", row.names = FALSE)
# 
# print("saved Catch Summary table")


# CREATE SPECIMEN TABLE OUTPUT  --------------------------------------------------------------------------------------------------

print("creating Specimen Table from updated EBSCRAB")

# Reformat specimen data, calculate weight based on sex/size relationships, [ETC.] for OLD WORKFLOW CrabHaul files
specimen_table <- ebscrab %>%
  dplyr::right_join(., haul_table, by = c('HAULJOIN')) %>%
  dplyr::filter(c(is.na(MID_LATITUDE) & is.na(MID_LONGITUDE) & is.na(STATIONID)) == FALSE) %>% # bad/gear testing hauls will have NA
  # calculate weights (based on length/width regression)
  ## **THIS COULD BE TIGHTENED UP - lookup table with covariates, ifelse for length vs. width based on spp**
  dplyr::mutate(
    CALCULATED_WEIGHT = case_when(# BAIRDI
      (SPECIES_CODE == 68560 & SEX == 1) ~ (0.00027*(WIDTH)^3.022134), # male
      (SPECIES_CODE == 68560 & SEX == 2 & CLUTCH_SIZE > 1) ~ (0.000441*(WIDTH)^2.898686), # mat female
      (SPECIES_CODE == 68560 & SEX == 2 & CLUTCH_SIZE <= 1) ~ (0.000562*(WIDTH)^2.816928), # imm/barren female
      # OPILIO/HYBRID
      (SPECIES_CODE %in% c(68580, 68590) & SEX == 1) ~ (0.000267*(WIDTH)^3.097253), # male
      (SPECIES_CODE %in% c(68580, 68590) & SEX == 2 & CLUTCH_SIZE > 1) ~ (0.001158*(WIDTH)^2.708793), # mat female
      (SPECIES_CODE %in% c(68580, 68590) & SEX == 2 & CLUTCH_SIZE <= 1) ~ (0.001047*(WIDTH)^2.708367), # imm/barren female
      # RKC
      (SPECIES_CODE == 69322 & SEX == 1) ~ (0.000403*(LENGTH)^3.141334), # male
      (SPECIES_CODE == 69322 & SEX == 2 & CLUTCH_SIZE > 1) ~ (0.003593*(LENGTH)^2.666076), # mat female
      (SPECIES_CODE == 69322 & SEX == 2 & CLUTCH_SIZE <= 1) ~ (0.000408*(LENGTH)^3.127956), # imm/barren female
      # BKC
      (SPECIES_CODE == 69323 & SEX == 1 & MID_LATITUDE < 58.65) ~ (0.000508*(LENGTH)^3.106409), # S male
      (SPECIES_CODE == 69323 & SEX == 1 & MID_LATITUDE > 58.65) ~ (0.000502*(LENGTH)^3.107158), # N male
      (SPECIES_CODE == 69323 & SEX == 2) ~ (0.02065*(LENGTH)^2.27), # female
      # EI
      (SPECIES_CODE == 69400 & SEX == 1) ~ (0.00071731*(LENGTH)^3.02), # male
      (SPECIES_CODE == 69400 & SEX == 2) ~ (0.001194533*(LENGTH)^2.86), # female
      TRUE ~ 0), 
    CALCULATED_WEIGHT = format(CALCULATED_WEIGHT, scientific = F),
    CALCULATED_WEIGHT = as.numeric(CALCULATED_WEIGHT),
    # create 1mm size bins
    LENGTH_1MM = case_when(SPECIES_CODE %in% c(69322, 69323, 69400) ~ floor(LENGTH)),
    WIDTH_1MM = case_when(SPECIES_CODE %in% c(68560, 68580, 68590) ~ floor(WIDTH)),
    CALCULATED_WEIGHT_1MM = case_when(# BAIRDI
      (SPECIES_CODE == 68560 & SEX == 1) ~ (0.00027*(WIDTH_1MM)^3.022134), # male
      (SPECIES_CODE == 68560 & SEX == 2 & CLUTCH_SIZE > 1) ~ (0.000441*(WIDTH_1MM)^2.898686), # mat female
      (SPECIES_CODE == 68560 & SEX == 2 & CLUTCH_SIZE <= 1) ~ (0.000562*(WIDTH_1MM)^2.816928), # imm/barren female
      # OPILIO/HYBRID
      (SPECIES_CODE %in% c(68580, 68590) & SEX == 1) ~ (0.000267*(WIDTH_1MM)^3.097253), # male
      (SPECIES_CODE %in% c(68580, 68590) & SEX == 2 & CLUTCH_SIZE > 1) ~ (0.001158*(WIDTH_1MM)^2.708793), # mat female
      (SPECIES_CODE %in% c(68580, 68590) & SEX == 2 & CLUTCH_SIZE <= 1) ~ (0.001047*(WIDTH_1MM)^2.708367), # imm/barren female
      # RKC
      (SPECIES_CODE == 69322 & SEX == 1) ~ (0.000403*(LENGTH_1MM)^3.141334), # male
      (SPECIES_CODE == 69322 & SEX == 2 & CLUTCH_SIZE > 1) ~ (0.003593*(LENGTH_1MM)^2.666076), # mat female
      (SPECIES_CODE == 69322 & SEX == 2 & CLUTCH_SIZE <= 1) ~ (0.000408*(LENGTH_1MM)^3.127956), # imm/barren female
      # BKC
      (SPECIES_CODE == 69323 & SEX == 1 & MID_LATITUDE < 58.65) ~ (0.000508*(LENGTH_1MM)^3.106409), # S male
      (SPECIES_CODE == 69323 & SEX == 1 & MID_LATITUDE > 58.65) ~ (0.000502*(LENGTH_1MM)^3.107158), # N male
      (SPECIES_CODE == 69323 & SEX == 2) ~ (0.02065*(LENGTH_1MM)^2.27), # female
      # EI
      (SPECIES_CODE == 69400 & SEX == 1) ~ (0.00071731*(LENGTH_1MM)^3.02), # male
      (SPECIES_CODE == 69400 & SEX == 2) ~ (0.001194533*(LENGTH_1MM)^2.86), # female 
      TRUE ~ 0), 
    CALCULATED_WEIGHT_1MM = format(CALCULATED_WEIGHT_1MM, scientific = F),
    CALCULATED_WEIGHT_1MM = as.numeric(CALCULATED_WEIGHT_1MM),
    # calculate area swept
    AREA_SWEPT = ((NET_WIDTH/1000) * DISTANCE_FISHED) * 0.29155335,
    #!! calculate start date and start hour
    START_DATE = as.Date(START_DATE, "%d-%b-%y"),
    START_HOUR = NA) %>% #to_char(start_time,'HH24MI')start_hour -- format(as.POSIXct(as.Date(specimen$START_TIME[6], "%d-%b-%y")), format = "%H%M")
  dplyr::select(!HAULJOIN) %>%
  dplyr::filter(!(HAUL_TYPE == 17 & SPECIES_CODE != 69322)) %>%
  dplyr::right_join(haul_table %>% 
                      dplyr::select(-START_DATE)) %>%
  dplyr::select(HAULJOIN, #VESSEL, CRUISE, HAUL, HAUL_TYPE, PERFORMANCE, START_DATE, START_HOUR, DURATION,
                # DISTANCE_FISHED, NET_WIDTH, NET_MEASURED, NET_HEIGHT, MID_LATITUDE, MID_LONGITUDE, GIS_STATION,
                # GEAR_DEPTH, BOTTOM_DEPTH, SURFACE_TEMPERATURE, GEAR_TEMPERATURE, WIRE_LENGTH, GEAR, ACCESSORIES,
                # SUBSAMPLE, AREA_SWEPT, 
                # SPECIMEN_ID = SPECIMENID, # should exist
                SPECIES_CODE, 
                SEX, 
                LENGTH, #LENGTH_1MM, 
                WIDTH, WIDTH_1MM, 
                SHELL_CONDITION,
                EGG_COLOR, 
                EGG_CONDITION, 
                LENGTH_MM_MERUS = MERUS_LENGTH, 
                HEIGHT_CHELA_MM = CHELA_HEIGHT, 
                CHELA_HEIGHT, 
                DISEASE_CODE, 
                DISEASE_DORSAL,
                DISEASE_VENTRAL, 
                DISEASE_LEGS, 
                WEIGHT_G_CALCULATED = CALCULATED_WEIGHT, #CALCULATED_WEIGHT_1MM, 
                WEIGHT_G = WEIGHT, 
                # GONAD_G = GONAD_WT, 
                # SPECIMEN_SUBSAMPLE_METHOD,
                SAMPLING_FACTOR) # does this actually get used for anything?

specimen_table <- specimen_table %>%
  dplyr::mutate(SPECIMEN_ID = NA, 
                # AGE = NA, 
                # MATURITY = NA, 
                # GONAD_G = NA, 
                # SPECIMEN_SUBSAMPLE_METHOD = NA, 
                # AGE_DETERMINATION_METHOD = NA, 
                LENGTH_MM = ifelse(is.na(LENGTH), LENGTH, WIDTH), 
                LENGTH_TYPE = ifelse(is.na(LENGTH), 7, 8)) %>% 
  dplyr::select(-LENGTH, -WIDTH) # -LENGTH_1MM, -WIDTH_1MM, -CALCULATED_WEIGHT_1MM, 
  # dplyr::rename(HAULJOIN, 
  #               SPECIMENID AS SPECIMEN_ID, 
  #               SPECIES_CODE, 
  #               LENGTH AS LENGTH_MM, 
  #               SEX, 
  #               WEIGHT AS WEIGHT_G, 
  #               AGE, 
  #               MATURITY, 
  #               GONAD_WT AS GONAD_G, 
  #               SPECIMEN_SUBSAMPLE_METHOD, 
  #               SPECIMEN_SAMPLE_TYPE, 
  #               AGE_DETERMINATION_METHOD ) # likely just null here, right?
  
  metadata_column <- metadata_column %>% 
    dplyr::add_row(data.frame(colname = "SHELL_CONDITION", 
                              colname_long = "Condition of carapace shell", 
                              units = "Catagory", 
                              datatype = "NUMBER(10,0)", 
                              colname_desc = "Condition of carapace shell")) %>% 
    dplyr::add_row(data.frame(colname = "EGG_COLOR", 
                              colname_long = "Color of eggs on gravid females", 
                              units = "Catagory", 
                              datatype = "NUMBER(10,0)", 
                              colname_desc = "Color of eggs on gravid females")) %>% 
    dplyr::add_row(data.frame(colname = "EGG_CONDITION", 
                              colname_long = "Condition of eggs on gravid females", 
                              units = "Catagory", 
                              datatype = "NUMBER(10,0)", 
                              colname_desc = "Condition of eggs on gravid females")) %>% 
    dplyr::add_row(data.frame(colname = "CLUTCH_SIZE", 
                              colname_long = "Eggs clutch size on gravid females", 
                              units = "Catagory", 
                              datatype = "NUMBER(10,0)", 
                              colname_desc = "Eggs clutch size on gravid females")) %>% 
    dplyr::add_row(data.frame(colname = "SAMPLING FACTOR", 
                              colname_long = "Sampling factor", 
                              datatype = "NUMBER(10,0)", 
                              colname_desc = "Sampling factor")) %>% 
    dplyr::add_row(data.frame(colname = "LENGTH_MM_MERUS", 
                              colname_long = "Length of merus in milimeters", 
                              units = "millimeters", 
                              datatype = "NUMBER(10,0)", 
                              colname_desc = "Length in millimeters of merus.")) %>% 
    dplyr::add_row(data.frame(colname = "HEIGHT_CHELA_MM", 
                              colname_long = "Length of chela in milimeters", 
                              units = "millimeters", 
                              datatype = "NUMBER(10,0)", 
                              colname_desc = "Length in millimeters of chela.")) %>% 
    dplyr::add_row(data.frame(colname = "DISEASE_CODE", 
                              colname_long = "Overall disease code", 
                              units = "Catagory", 
                              datatype = "NUMBER(10,0)", 
                              colname_desc = "....")) %>% 
    dplyr::add_row(data.frame(colname = "DISEASE_DORSAL", 
                              colname_long = "Disease on dorsal code", 
                              units = "Catagory", 
                              datatype = "NUMBER(10,0)", 
                              colname_desc = "....")) %>% 
    dplyr::add_row(data.frame(colname = "DISEASE_VENTRAL", 
                              colname_long = "Disease on ventral code", 
                              units = "Catagory", 
                              datatype = "NUMBER(10,0)",  
                              colname_desc = "....")) %>% 
    dplyr::add_row(data.frame(colname = "DISEASE_LEGS", 
                              colname_long = "Desease in legs code", 
                              units = "Catagory", 
                              datatype = "NUMBER(10,0)",  
                              colname_desc = "....")) %>% 
    dplyr::add_row(data.frame(colname = "WEIGHT_G_CALCULATED", 
                              colname_long = "Weight in grams", 
                              units = "Weight", 
                              datatype = "NUMBER(10,0)", 
                              colname_desc = "Weight in grams"))  
  
write.csv(x = specimen_table, file = here::here("temp", "specimen_table.csv"), row.names = FALSE)

gapindex::upload_oracle(x = specimen_table, 
                        table_name = "SPECIMEN", 
                        channel = channel_ehm, 
                        schema = "markowitze", 
                        metadata_column = metadata_column %>% dplyr::filter(colname %in% names(specimen_table)), 
                        table_metadata = paste0("Modifed of GAP_PRODUCTS.SPECIMEN OR AKFIN_SPECIMEN. ", legal_disclaimer),
                        share_with_all_users = TRUE)


# ## OLD WORKFLOW: parse specimen table by species and save for Tech Memo and Oracle
# # BAIRDI ------------------------------------
# # save for AKFIN
# specimen_table %>%
#   filter(SPECIES_CODE == 68560) %>%
#   # select(-c(LENGTH, LENGTH_1MM)) %>%
#   # right_join(., haul_table %>% 
#   #              select(-c(CRUISEJOIN, REGION, START_TIME, STRATUM, BOTTOM_TYPE,
#   #                        START_LATITUDE, END_LATITUDE, START_LONGITUDE, END_LONGITUDE, STATIONID))) %>%
#   write.csv(., paste0(out_dir, "crabhaul_bairdi.csv"), row.names = FALSE)
# 
# # save for Tech Memo
# specimen_table %>%
#   filter(SPECIES_CODE == 68560) %>%
#   mutate(SPECIES_NAME = "Bairdi Tanner Crab") %>%
#   write.csv(., paste0(tm_out_dir, "crabhaul_bairdi.csv"), row.names = FALSE)
# 
# 
# # OPILIO -----------------------------------
# # save for AKFIN
# specimen_table %>%
#   filter(SPECIES_CODE == 68580) %>%
#   # select(-c(LENGTH, LENGTH_1MM)) %>%
#   # right_join(., haul_table %>% 
#   #              filter(SURVEY_YEAR == current_year) %>%
#   #              select(-c(CRUISEJOIN, REGION, SURVEY_YEAR, START_TIME, STRATUM, BOTTOM_TYPE,
#   #                        START_LATITUDE, END_LATITUDE, START_LONGITUDE, END_LONGITUDE, STATIONID))) %>%
#   write.csv(., paste0(out_dir, "crabhaul_opilio.csv"), row.names = FALSE)
# 
# # save for Tech Memo
# specimen_table %>%
#   filter(SPECIES_CODE == 68580) %>%
#   mutate(SPECIES_NAME = "Opilio Crab") %>%
#   write.csv(., paste0(tm_out_dir, "crabhaul_opilio.csv"), row.names = FALSE)
# 
# 
# # RKC --------------------------------------
# # save for AKFIN
# specimen_table %>%
#   filter(SPECIES_CODE == 69322) %>%
#   # select(-c(WIDTH, WIDTH_1MM)) %>%
#   # right_join(., haul_table %>% 
#   #              filter(SURVEY_YEAR == current_year) %>%
#   #              select(-c(CRUISEJOIN, REGION, SURVEY_YEAR, START_TIME, STRATUM, BOTTOM_TYPE,
#   #                        START_LATITUDE, END_LATITUDE, START_LONGITUDE, END_LONGITUDE, STATIONID))) %>%
#   write.csv(., paste0(out_dir, "crabhaul_rkc.csv"), row.names = FALSE)
# 
# # save for Tech Memo
# specimen_table %>%
#   filter(SPECIES_CODE == 69322) %>%
#   mutate(SPECIES_NAME = "Red King Crab") %>%
#   write.csv(., paste0(tm_out_dir, "crabhaul_rkc.csv"), row.names = FALSE)
# 
# 
# # BKC -------------------------------------
# # save for AKFIN
# specimen_table %>%
#   filter(SPECIES_CODE == 69323) %>%
#   # select(-c(WIDTH, WIDTH_1MM)) %>%
#   # right_join(., haul_table %>% 
#   #               filter(SURVEY_YEAR == current_year) %>%
#   #               select(-c(CRUISEJOIN, REGION, SURVEY_YEAR, START_TIME, STRATUM, BOTTOM_TYPE,
#   #                                        START_LATITUDE, END_LATITUDE, START_LONGITUDE, END_LONGITUDE, STATIONID))) %>%
#   write.csv(., paste0(out_dir, "crabhaul_bkc.csv"), row.names = FALSE)
# 
# # save for Tech Memo
# specimen_table %>%
#   filter(SPECIES_CODE == 69323) %>%
#   mutate(SPECIES_NAME = "Blue King Crab") %>%
#   write.csv(., paste0(tm_out_dir, "crabhaul_bkc.csv"), row.names = FALSE)
# 
# 
# # EI --------------------------------------
# # save for AKFIN
# specimen_table %>%
#   filter(SPECIES_CODE == 69400) %>%
#   # select(-c(WIDTH, WIDTH_1MM)) %>%
#   # right_join(., haul_table %>% 
#   #              filter(SURVEY_YEAR == current_year) %>%
#   #              select(-c(CRUISEJOIN, REGION, SURVEY_YEAR, START_TIME, STRATUM, BOTTOM_TYPE,
#   #                        START_LATITUDE, END_LATITUDE, START_LONGITUDE, END_LONGITUDE, STATIONID))) %>%
#   write.csv(., paste0(out_dir, "crabhaul_ei.csv"), row.names = FALSE)
# 
# # save for Tech Memo
# specimen_table %>%
#   filter(SPECIES_CODE == 69400) %>%
#   mutate(SPECIES_NAME = "Erimacrus Hair Crab") %>%
#   write.csv(., paste0(tm_out_dir, "crabhaul_ei.csv"), row.names = FALSE)
# 
# 
# # HYBRID ---------------------------------
# # save for AKFIN
# specimen_table %>%
#   filter(SPECIES_CODE == 68590) %>%
#   # select(-c(LENGTH, LENGTH_1MM)) %>%
#   # right_join(., haul_table %>% 
#   #              filter(SURVEY_YEAR == current_year) %>%
#   #              select(-c(CRUISEJOIN, REGION, SURVEY_YEAR, START_TIME, STRATUM, BOTTOM_TYPE,
#   #                        START_LATITUDE, END_LATITUDE, START_LONGITUDE, END_LONGITUDE, STATIONID))) %>%
#   write.csv(., paste0(out_dir, "crabhaul_hybrid.csv"), row.names = FALSE)
# 
# # save for Tech Memo
# specimen_table %>%
#   filter(SPECIES_CODE == 68590) %>%
#   mutate(SPECIES_NAME = "Chionoecetes Hybrid Crab") %>%
#   write.csv(., paste0(tm_out_dir, "crabhaul_hybrid.csv"), row.names = FALSE)
# 
# 
# print("CrabHaul files written")
# 
# # rename specimen table to keep for further downstream processing
# crabhaul_table <- specimen_table
# 
# # remove items from environment
# rm(catch, catch_summary, dat, discrepancies, ebscrab, specimen, specimen_table, path)
# gc()
# 
# 
