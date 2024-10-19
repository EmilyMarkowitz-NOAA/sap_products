##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Project:       SAP_PRODUCTS standard table production workflow
##                NOAA AFSC SAP and GAP Survey Team
## PoC:           Shannon Hennessey (shannon.hennessey@noaa.gov)
##                Emily Markowitz (emily.markowitz@noaa.gov)
##                
## Description:   This script houses a sequence of programs that calculates
##                the standard data products resulting from the NOAA AFSC 
##                Groundfish Assessment Program bottom trawl surveys and 
##                Standard GAP survey data products in this repository include
##                CPUE, Biomass, Size Composition, and Age Composition. Tables
##                that are served to the Alaska Fisheries Information Network
##                (AKFIN) and Fisheries One Stop Shop (FOSS) data portals are
##                also housed here as materialized views that are often 
##                mirrors of these standard data tables or queries of tables in 
##                RACEBASE/RACE_DATA. 
##                
##                The GAP_PRODUCTS Oracle schema houses the four standard data
##                product tables and views and will be updated at least twice a
##                year: once prior to the survey season to incorporate new age
##                data and vouchered specimens that were processed after the 
##                prior year's survey and at least once after the survey season
##                following the conclusion of each region's survey. 
##                
##                **DISCLAIMER**: Each script is self-contained. Do not source 
##                this script. Each of the following scripts needs to be run 
##                line-by-line with caution. The file.edit() function simply
##                opens the script in a new tab within RStudio.
##
# NOTES:
# - think about how the scripts are working together (ie. what's loaded in each script vs. just using the central...)
# --- have key directories/file dependencies in the scripts but commented out, or with an 'if' to be a standalone if need be?
# --- rm() unnecessary interim tables at the end of each script so the processing workflow doesn't get clogged? Also an 'if' for that?
# --- have scripts print out step-by-step confirmations as they work through the process? Just to keep track of progress as we run workflow...
##
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Restart R Session before running
rm(list = ls())


# Load libaries and funcitons --------------------------------------------------
PKG <- c("devtools", 
         "magrittr", 
         "here", 
         "data.table", 
         "dplyr", 
         "sf", 
         "readxl", 
         "janitor", 
         "RODBC", 
         "gapindex", # devtools::install_github("afsc-gap-products/gapindex")
         "stringr", 
         "data.table", 
         "RCurl", 
         "XML")

for (p in PKG) {
  if(!require(p,character.only = TRUE)) {  
    install.packages(p, verbose = FALSE)
    require(p,character.only = TRUE)}
}

source(here::here("content/functions.R"))

temp_oracle_push <- function(x, table_name, channel, schema, metadata_column, table_metadata, share_with_all_users) {
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   Update Metadata
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cat("Updating Metadata ...\n")
  ## Add column metadata 
  
  for (i in 1:nrow(x = metadata_column)) {
    
    desc <- gsub(pattern = "<sup>2</sup>",
                 replacement = "2",
                 x = metadata_column$colname_long[i], 
                 fixed = TRUE)
    short_colname <- gsub(pattern = "<sup>2</sup>", 
                          replacement = "2",
                          x = metadata_column$colname[i], 
                          fixed = TRUE)
    
    RODBC::sqlQuery(
      channel = channel,
      query = paste0('comment on column ', 
                     schema, '.', table_name,'.',
                     short_colname,' is \'',
                     desc, ". ", # remove markdown/html code
                     gsub(pattern = "'", replacement ='\"',
                          x = metadata_column$colname_desc[i]),'\';'))
    
  }
  
  ## Add table metadata 
  RODBC::sqlQuery(
    channel = channel,
    query = paste0('comment on table ', schema,'.', table_name, ' is \'',
                   table_metadata,'\';'))
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   Grant select access to all users
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (share_with_all_users) {
    
    cat("Granting select access to all users ... \n")
    all_schemas <- RODBC::sqlQuery(channel = channel,
                                   query = paste0('SELECT * FROM all_users;'))
    
    for (iname in sort(all_schemas$USERNAME)) {
      RODBC::sqlQuery(channel = channel,
                      query = paste0('grant select on ', schema,'.', table_name,
                                     ' to ', iname, ';'))
    }
    
  }
  cat("Finished.\n\n")
}

## Query Oracle and write to csv in the temp folder
metadata_table <- RODBC::sqlQuery(
  channel = channel_products, 
  query = "SELECT * FROM GAP_PRODUCTS.METADATA_TABLE")

legal_disclaimer <- paste0("Tables are provided ", 
                           gsub(pattern = "Groundfish Assessment Program (GAP)", 
                                replacement = "Shellfish Assessment Program (SAP)", 
                                x = metadata_table$METADATA_SENTENCE[metadata_table$METADATA_SENTENCE_NAME == "survey_institution"], 
                                fixed = TRUE),
                           metadata_table$METADATA_SENTENCE[metadata_table$METADATA_SENTENCE_NAME == "legal_restrict"],
                           gsub(x = metadata_table$METADATA_SENTENCE[metadata_table$METADATA_SENTENCE_NAME == "github"],
                                pattern = "https://github.com/afsc-gap-products/gap_products",
                                replacement = link_repo)
)


metadata_column <- RODBC::sqlQuery(
  channel = channel_products, 
  query = "SELECT * FROM GAP_PRODUCTS.METADATA_COLUMN") 

names(metadata_column) <- tolower(names(metadata_column))
names(metadata_column) <- gsub(pattern = "metadata_", replacement = "", x = names(metadata_column))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Setup ----
##   Make sure a local temp/ directory is created, save R version data, 
##   and install packages if not available on your machine or if outdated.
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1. load functions from gap_products repository (no reason to have them cross posted)

fnts <- RCurl::getURL("https://github.com/afsc-gap-products/gap_products/tree/main/functions")
fnts <- readLines(tc <- textConnection(fnts)); close(tc)
fnts <- fnts[which(grepl(pattern = "calc_diff", x = fnts))]
fnts <- strsplit(x = fnts, split = "functions/", fixed = TRUE)
fnts <- fnts[[1]]
fnts <- fnts[-1]
fnts <- strsplit(x = fnts, split = ".R", fixed = TRUE)
fnts <- sapply(fnts, "[[", 1)

for (i in fnts) {
  source(paste0("https://raw.githubusercontent.com/afsc-gap-products/gap_products/refs/heads/main/functions/",i,".R"))
}

# 2. sets up temp/ folder
output_r_session(path = "temp/") 

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Create lookup tables ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
file.edit("code/Scripts/lookup_tables.R")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Process RACEBASE.HAUL to create Haul Table (Tech Memo)
# - need to initially connect to Oracle and download updated table each year (set ORACLE --> TRUE)
# - need to add in current year's CRUISE and VESSEL IDs and HAUL_TYPE (3, and 17 if retow year)
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ORACLE = FALSE
file.edit("code/Scripts/haul_table.R")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   ## NEW PROPOSED TABLEs
# Process EBSCRAB specimen info from Oracle to create Specimen Table for stock assessment authors
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ORACLE = FALSE 
file.edit("code/Scripts/raw_process.R")


# Load biomass-abundance-cpue function
source("./Scripts/bio_abund_cpue.R")

# ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ##   Pull Existing GAP_PRODUCTS Tables and Views ----
# ##   Import current versions of the data tables in GAP_PRODUCTS locally within 
# ##   the gap_products repository in temp/ folder. These local versions of the 
# ##   tables are used to compare against the updated production tables that we 
# ##   create in a later step to what is currently in the GAP_PRODUCTS schema.
# ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# file.edit("code/pull_existing_tables.R")
# 
# ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ##   Create Production Tables and Compare Data Table----
# ##   Calculate the four major standard data products: CPUE, BIOMASS, SIZECOMP, 
# ##   AGECOMP for all taxa, survey years, survey regions and compare to what
# ##   is on GAP_PRODUCTS currently 
# ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# file.edit("code/production.R")
# 
# ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ##   Update Production Tables and Update AKFIN and FOSS Tables----
# ##   Removed, new, and modified records are updated in GAP_PRODUCTS.
# ##   Once GAP_PRODUCTS tables are updated, run queries for the materialized 
# ##   views created for AKFIN and FOSS.
# ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# file.edit("code/update_production_tables.R")
# 
# ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ##   Archive GAP_PRODUCTS  ----
# ##   Archive the bits that would allow one to reproduce the standard data 
# ##   tables. The session info and package versions are also .csv files in the 
# ##   temp/folder.
# ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# source("functions/archive_gap_products.R") 
# archive_gap_products(path = "temp/", archive_path = "G:/GAP_PRODUCTS_Archives/")
