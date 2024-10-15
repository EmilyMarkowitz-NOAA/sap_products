##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Project:       GAP_PRODUCTS Utility Scripts
##                NOAA AFSC GAP Survey Team
## PoC:           Zack Oyafuso (zack.oyafuso@noaa.gov)
##                Emily Markowitz (emily.markowitz@noaa.gov)
##                
## Description:   This script houses a hodgepodge of R scripts used to perform
##                miscellaneous functions to maintain the GAP_PRODUCTS 
##                Oracle schema outside of the standard production run of 
##                CPUE, Biomass, Size Composition, and Age Composition. In
##                each section header, I put the frequency at which one would
##                need to run each script. 
##                
##                **DISCLAIMER**: Each script is self-contained. Do not source 
##                this script. The script for each step needs to be run 
##                line-by-line with caution. The file.edit() function simply
##                opens the script in a new tab within RStudio. 
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   In case you need recreate audit tables. 
##   Frequency: As Needed (mostly in case of emergency)
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
file.edit("code/utility_scripts/create_audit_tables.R")
