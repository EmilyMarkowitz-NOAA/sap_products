# PURPOSE -------------------------------------------------------------------------------------------------------------------
# 1) Central script to run Annual EBS Bottom Trawl Survey data processing

# Author: Shannon Hennessey, NOAA-AFSC

# NOTES:
# - think about how the scripts are working together (ie. what's loaded in each script vs. just using the central...)
# --- have key directories/file dependencies in the scripts but commented out, or with an 'if' to be a standalone if need be?
# --- rm() unnecessary interim tables at the end of each script so the processing workflow doesn't get clogged? Also an 'if' for that?
# --- have scripts print out step-by-step confirmations as they work through the process? Just to keep track of progress as we run workflow...

PKG <- c("devtools", 
         # "googledrive", 
         "tidyverse", #TOLDEDO - in my experience, tidyverse can load older versions of packages sometimes
         "magrittr", 
         "here", 
         "dplyr", 
         "sf", 
         "readxl", 
         "janitor", 
         "RODBC", 
         "gapindex", # devtools::install_github("afsc-gap-products/gapindex")
         "stringr")

for (p in PKG) {
  if(!require(p,character.only = TRUE)) {  
    install.packages(p, verbose = FALSE)
    require(p,character.only = TRUE)}
}

# Set current year
current_year <- 2023
production <- TRUE # otherwise treated as test

# Set directories
if (production) {
  data_dir <- "Y:/KOD_Survey/EBS Shelf/Data_Processing/Data/"    # writing base files from Oracle for local processing (RACEBASE.HAUL, EBSCRAB.EBSCRAB)
  out_dir <- "Y:/KOD_Survey/EBS Shelf/Data_Processing/Outputs/"  # writing processed files for AKFIN upload
  path <- paste0("Y:/KOD_Survey/EBS Shelf/", current_year, "/Tech Memo/Data/")   # writing processed files for Tech Memo use
} else {
  data_dir <- out_dir <- tm_out_dir <- here::here("temp")
}

# RUN SCRIPTS ---------------------------------------------------------------------------------------------------------------

# Load lookup tables
source("./Scripts/lookup_tables.R")
source("./Scripts/akfin_tables.R")


# Process RACEBASE.HAUL to create Haul Table (Tech Memo)
# - need to initially connect to Oracle and download updated table each year (set ORACLE --> TRUE)
# - need to add in current year's CRUISE and VESSEL IDs and HAUL_TYPE (3, and 17 if retow year)
ORACLE = FALSE
source("./Scripts/haul_table.R")


# OLD WORKFLOW -- these are now just lookups, add new year to stratum_years definition table
# # Create Stratum Tables from Haul Table (Tech Memo and Oracle)
# # - make combined stratum table across species for Tech Memo use
# # - reformat into individual species tables for AKFIN (for now)
# # - Eventual plan: Make these tables a series of lookup tables rather than timeseries!!
# source("./Scripts/stratum_tables.R")

## OLD WORKFLOW - creating new year to append to EBSCRAB from tablet output .csvs in R
# Process CATCH and SPECIMEN files from tablet to:
# - format and append current_year specimen data to EBSCRAB.EBSCRAB (Oracle)
# --- need to initially connect to Oracle and download updated table (set ORACLE --> TRUE)
# --- ** Will have local copy ^^ in Data_Processing folder, only necessary for 2023!
# - create Catch Summary table for the current year (Oracle?)
# - produce CrabHaul files for each species and append to existing timeseries (Tech Memo and Oracle)
# --- *keeping species-specific files because too unwieldy with entire timeseries and all spp.
# **still need to calculate START_DATE and START_HOUR
# ORACLE = FALSE # only need for 2023 initial appending
# source("./Scripts/raw_process.R")

## NEW PROPOSED TABLEs
# Process EBSCRAB specimen info from Oracle to create Specimen Table for stock assessment authors
ORACLE = FALSE 
source("./Scripts/raw_process.R")


# Load biomass-abundance-cpue function
source("./Scripts/bio_abund_cpue.R")

# Calculating biomass-abundance and CPUE timeseries...
# This is picked up by the Tech Memo workflow for internal needs....

# **SCRIPT TO WRITE TO ORACLE -- should maybe be separate or kept commented out just so we don't accidentally overwrite??
# Also, this could be rethought, maybe we create the specimen table in Oracle instead of taking things in and out via R


# **what other things need to be created for AKFIN/tech memo??**
# For ADF&G??
# - Files generated using lbs instead of kgs –use old code found under Population/Old Method/ADFG
# - For Jie specifically
# - RKC GE65 combined



# OLD WORKFLOW: TABLES TO BE SENT TO AKFIN ---------------------------------------------------------------------------------------------------

# - Stratum tables (x7)
# --- EBSCRAB.STRATA_BAIRDI_BETW166173_NEWTS
# --- EBSCRAB.STRATA_BAIRDI_NEWTIMESERIES
# --- EBSCRAB.STRATA_BKC_NEWTIMESERIES
# --- EBSCRAB.STRATA_EI_NEWTIMESERIES
# --- EBSCRAB.STRATA_HYBRID_NEWTIMESERIES
# --- EBSCRAB.STRATA_OPILIO_NEWTIMESERIES
# --- EBSCRAB.STRATA_RKC_NEWTIMESERIES

# - CrabHaul tables (x6)
# --- EBSCRAB.CRABHAUL_BAIRDI
# --- EBSCRAB.CRABHAUL_BKC
# --- EBSCRAB.CRABHAUL_EI
# --- EBSCRAB.CRABHAUL_HYBRID
# --- EBSCRAB.CRABHAUL_OPILIO
# --- EBSCRAB.CRABHAUL_RKC

# - Abundance_Biomass table
# --- EBSCRAB.EBSCRAB_ABUNDANCE_BIOMASS

# - CPUE table
# --- EBSCRAB.EBSCRAB_CPUE

# - Other tables (don't actually get updated; are recreated here in 'akfin_tables.R')
# --- EBSCRAB.EBSCRAB_DISTRICT
# --- EBSCRAB.EBSCRAB_SPECIES
# --- EBSCRAB.EBSCRAB_SIZE_GROUP
# --- EBSCRAB.EBSCRAB_SIZE_GROUP_DISTRICT
# --- EBSCRAB.EBSCRAB_WEIGHT_REGRESSION
