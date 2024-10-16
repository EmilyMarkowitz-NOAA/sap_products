
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Project:       Pull Existing GAP_PRODUCTS production tables
## Author:        Shannon Hennessey, district and male cutlines tables created by Emily Ryznar
## Description:   Update lookup tables; 
##                to serve as a central source script to load lookup tables for use in other scripts and to accompany AKFIN products

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

## SPECIES lookup table --------------------------------------------------------
species_lookup <- tibble(SPECIES_ID = c(1:6, rep(NA, 3)), # TOLDEO - does this really ever get used? we have to get rid of this
                         SPECIES_CODE = c(68560,
                                          68580,
                                          68590,
                                          69322,
                                          69323,
                                          69400,
                                          69310,
                                          68550,
                                          68541),
                         SPECIES_NAME = c("Tanner Crab",
                                          "Snow Crab",
                                          "Hybrid Crab",
                                          "Red King Crab",
                                          "Blue King Crab",
                                          "Horsehair Crab",
                                          "Golden King Crab",
                                          "Grooved Tanner Crab",
                                          "Chinocetes Mix")) %>% 
  dplyr::rename(COMMON_NAME = SPECIES_NAME)

gapindex::upload_oracle(x = species_lookup, 
                        table_name = "XCRAB_SPECIES", 
                        channel = channel_ehm, 
                        schema = "markowitze", 
                        metadata_column = metadata_column %>% dplyr::filter(colname %in% names(species_lookup)), 
                        table_metadata = paste0("Crab specific SPECIES_ID for crab ids. ", legal_disclaimer),
                        share_with_all_users = TRUE)

temp_oracle_push(x = species_lookup, 
                 table_name = "XCRAB_SPECIES", 
                 channel = channel_ehm, 
                 schema = "markowitze", 
                 metadata_column = metadata_column %>% dplyr::filter(colname %in% names(species_lookup)), 
                 table_metadata = paste0("Crab specific SPECIES_ID for crab ids. ", legal_disclaimer),
                 share_with_all_users = TRUE)

## DISTRICT AREA lookup table by stock, pull districts by stock specified ------

# add lines to add LOAD_DATE column (in DD-Mon-YY; does this get updated??), write to .csv, and bundle with AKFIN products?
# 'ebscrab_district.csv'
# seems redundant - I think this could simply be combined into the AKFIN_STRATUM_GROUP0 
# table (just add the "stock" column somehow)
# and some districts here aren't represented in AKFIN_STRATUM_GROUP0 - that seems like a problem??

# dist_stock_lookup0 <- data.frame(STOCK = c("BBRKC", 
#                                            rep("PribRKC", 2),
#                                            "NSRKC", "NorthRKC",
#                                            rep("PribBKC", 2), 
#                                            rep("StMattBKC", 2),
#                                            "BKCNBS",
#                                            rep("TannerW", 3),
#                                            "TannerWNBS",
#                                            "TannerE", 
#                                            "TannerENBS",
#                                            rep("Snow", 3), 
#                                            "SnowNBS",
#                                            rep("Hybrid", 3), 
#                                            "HybridNBS",
#                                            rep("Hair", 4), 
#                                            "HairNBS",
#                                            rep("Allstations", 10)),
#                                  DISTRICT = c("Bristol Bay", 
#                                               "Pribilof MTCA", "Pribilof Single",
#                                               "Norton Sound", "Northern Unstratified",
#                                               "Pribilof MTCA", "Pribilof Single",
#                                               "St. Matthew MTCA", "St. Matthew Single",
#                                               "NBS All",
#                                               "Pribilof MTCA", "St. Matthew MTCA", "West 166",
#                                               "NBS All",
#                                               "East 166",
#                                               "NBS All",
#                                               "Pribilof MTCA", "Single", "St. Matthew MTCA",
#                                               "NBS All",
#                                               "Pribilof MTCA", "Single", "St. Matthew MTCA",
#                                               "NBS All",
#                                               "Bristol Bay", "Northern Unstratified", "Pribilof MTCA", "Pribilof Single",
#                                               "NBS All",
#                                               "Bristol Bay", "Northern Unstratified", "Pribilof MTCA","Pribilof Single",      
#                                               "BKC Unstratified", "St. Matthew MTCA", "St. Matthew Single", "East 166",             
#                                               "West 166", "Single")) 

dist_lookup <- tibble(DISTRICT_ID = c(1:9),
                      DISTRICT_CODE = c("166TO173", 
                                        "ALL", 
                                        "BB", 
                                        "E166", 
                                        "PRIB", 
                                        "STMATT", 
                                        "UNSTRAT", 
                                        "W166", 
                                        "NORTH"),
                      DISTRICT_NAME = c("Between 166W and 173W",
                                        "All Areas",
                                        "Bristol Bay",
                                        "East of 166",
                                        "Pribilof Islands",
                                        "St. Matthew",
                                        "Unstratified",
                                        "West of 166",
                                        "Northern"),
                      DISTRICT_COMMENTS = c("Exception area that lies between 166W and 173W, used for Bairdi only",
                                            "Used for Opilio, which is evaluated for one area only. Added Bairdi 5/22/2015. Added hair crab 1/5/2016.",
                                            "Bristol Bay area used for Red King Crab",
                                            "Area East of 166 longitude, used for Bairdi only",
                                            "Pribilof Islands district, used for multiple species",
                                            "St. Matthew district, used for multiple species",
                                            "Unstratified used for Blue King Crab outside of Pribilof and St. Matthew Districts",
                                            "Area West of 166 longitude, used for Bairdi only",
                                            "Northern district for Red King Crab and Korean Horsehair Crab"))

stratum_nstations <- read.csv("./temp/Tables/stratum_nstations.csv") %>% # TOLDEO - what... is this?
  dplyr::left_join(read.csv("./temp/Tables/stratum_years.csv"), relationship = "many-to-many") %>% 
  dplyr::rename(SPECIES_CODE = SPECIES, 
                N_COUNT = N_STATIONS, # is this what this is refering to??? 
                # AREA_ID = YEAR_BLOCK_ID, 
                AREA_KM2 = TOTAL_AREA, 
                DESIGN_YEAR = SURVEY_YEAR) %>% 
  dplyr::mutate(    SPECIES_CODE = dplyr::case_when(
    SPECIES_CODE == "bairdi" ~  68560, 
    SPECIES_CODE == "bkc" ~  69323, 
    SPECIES_CODE == "ei" ~  69400, 
    SPECIES_CODE == "hybrid" ~  68590, 
    SPECIES_CODE == "opilio" ~  68580, 
    SPECIES_CODE == "rkc" ~  69322
  )) %>% 
  # dplyr::left_join(species_lookup %>% 
  #                    dplyr::select(SPECIES_CODE) %>% 
  #                    dplyr::mutate(AREA_ID = 1:nrow(species_lookup))) %>%  # simplistic, but maintains order and it seems that stratification is (at this point) solely based on species_code, though in the future could be species code and new survey design/districts?
  dplyr::mutate(
    # AREA_ID = AREA_ID + 88000, # can be anything - GAP designs are 99###
    AREA_KM2 = AREA_KM2/100) # TOLEDO, just guessing that this is in hectares and converting it to km2

dist_stock_lookup <- read.csv(file = './temp/Tables/district_stratum_lookup.csv') %>% 
  dplyr::select(-SPECIES_NAME)  %>% 
  dplyr::rename(DISTRICT_ABBREV = DISTRICT_CODE, 
                STRATUM_ABBREV = STRATUM_CODE) %>% 
  dplyr::left_join(dist_lookup %>% 
                     dplyr::rename(DISTRICT_ABBREV = DISTRICT_CODE, 
                                   DISTRICT_CODE = DISTRICT_ID) ) %>% 
  dplyr::mutate(
    SURVEY_DEFINITION_ID = dplyr::case_when(
      DISTRICT_NAME %in% c("All Northern Bering Sea", "Northern") ~ 143, # TOLEDO - guessing
      TRUE ~ 98
    ), 
    DISTRICT_CODE = dplyr::case_when(
    !is.na(DISTRICT_CODE) ~ DISTRICT_CODE, 
    DISTRICT_ABBREV == "NS" ~ 10, # made up
    DISTRICT_ABBREV == "NBS" ~ 880002, # matches with GAP stuff, though may be the same as our 99902 
    DISTRICT_ABBREV == "EBS" ~ 880000 # matches with GAP stuff, though may be the same as our 99900 ro 99901
  )) 

dist_stock_lookup <- dist_stock_lookup %>% 
  dplyr::left_join(dist_stock_lookup %>% # assign STRATUM #s - TOLEDO, I cant tell if these exist already
                     dplyr::select(STRATUM_ABBREV) %>% 
                     dplyr::distinct() %>% 
                     dplyr::mutate(STRATUM_CODE = 100 + as.numeric(factor(STRATUM_ABBREV)))) # +100 to distinguish it from districts, make them unique

dist_stock_lookup <- dist_stock_lookup %>% 
  dplyr::left_join(stratum_nstations %>% 
                     dplyr::rename(DISTRICT_NAME = DISTRICT, 
                                   DISTRICT_AREA_KM2 = AREA_KM2, 
                                   DISTRICT_DESIGN_YEAR = DESIGN_YEAR) %>% 
                     dplyr::mutate(STRATUM_DESIGN_YEAR = 1975)) # TOLEDO - may not be right, but works for now

### AKFIN_AREA -----------------------------------------------------------------

AKFIN_AREA <- dplyr::bind_rows(
  dist_stock_lookup %>% 
    dplyr::rename(AREA_NAME = STRATUM_NAME,
                  AREA_ID = STRATUM_CODE, 
                  DESIGN_YEAR = STRATUM_DESIGN_YEAR) %>% 
    dplyr::mutate(AREA_TYPE = "STRATUM", 
                  DESCRIPTION = paste0(STRATUM_ABBREV)) %>% 
    dplyr::select(-dplyr::starts_with("DISTRICT_"), -STRATUM_ABBREV) %>% 
    dplyr::distinct(), 
  dist_stock_lookup %>% 
    dplyr::rename(AREA_NAME = DISTRICT_NAME, 
                  AREA_ID = DISTRICT_CODE, 
                  AREA_KM2 = DISTRICT_AREA_KM2, 
                  DESIGN_YEAR = DISTRICT_DESIGN_YEAR) %>% 
    dplyr::mutate(AREA_TYPE = "DISTRICT", 
                  DESCRIPTION = paste0(DISTRICT_ABBREV, ifelse(is.na(DISTRICT_COMMENTS), "", paste0(": ", DISTRICT_COMMENTS))))  %>% 
    dplyr::select(-dplyr::starts_with("STRATUM_"), -DISTRICT_ABBREV, -DISTRICT_COMMENTS) %>% 
    dplyr::distinct() ) %>% 
  dplyr::mutate(
    DEPTH_MIN_M = NA, 
    DEPTH_MAX_M = NA) #%>% 
  # dplyr::select(-DESCRIPTION)
# N_COUNT is here, but I don't really understand it...

# Removing non-breaking space characters in R
# for (i in 1:ncol(AKFIN_AREA)) {
#   AKFIN_AREA[,i] <- gsub("[^ -~]+", "", AKFIN_AREA[,i])
# }

gapindex::upload_oracle(x = AKFIN_AREA, 
                        table_name = "AKFIN_AREA", 
                        channel = channel_ehm, 
                        schema = "markowitze", 
                        metadata_column = metadata_column %>% 
                          # dplyr::mutate(datatype = ifelse(datatype == "VARCHAR2(4000 BYTE)", "CLOB", datatype)) %>% 
                          # dplyr::mutate(datatype = ifelse(datatype == "VARCHAR2(4000 BYTE)", "VARCHAR2(4000 CHAR)", datatype)) %>%
                          dplyr::filter(colname %in% names(AKFIN_AREA)),  
                        table_metadata = paste0("Lookup table for which area are contained within a given AREA_ID for each DESIGN_YEAR. This table can be used in tandom with the GAP_PRODCUTS.STARTUM_GROUPS or AKFIN_STARTUM_GROUPS tables. ", 
                                                legal_disclaimer),
                        share_with_all_users = TRUE)  

temp_oracle_push(x = AKFIN_AREA, 
                 table_name = "AKFIN_AREA", 
                 channel = channel_ehm, 
                 schema = "markowitze", 
                 metadata_column = metadata_column %>% dplyr::filter(colname %in% names(AKFIN_AREA)), 
                 table_metadata = paste0("Lookup table for which area are contained within a given AREA_ID for each DESIGN_YEAR. This table can be used in tandom with the GAP_PRODCUTS.STARTUM_GROUPS or AKFIN_STARTUM_GROUPS tables. ", 
                                         legal_disclaimer),
                 share_with_all_users = TRUE)

### AKFIN_STRATUM_GROUP --------------------------------------------------------

AKFIN_STRATUM_GROUP <- dist_stock_lookup %>% 
  dplyr::select(SPECIES_CODE, AREA_ID = DISTRICT_CODE, STRATUM = STRATUM_CODE, DESIGN_YEAR = DISTRICT_DESIGN_YEAR) %>% 
  dplyr::distinct()

gapindex::upload_oracle(x = AKFIN_STRATUM_GROUP, 
                        table_name = "AKFIN_STRATUM_GROUP", 
                        channel = channel_ehm, 
                        schema = "markowitze", 
                        metadata_column = metadata_column %>% dplyr::filter(colname %in% names(AKFIN_STRATUM_GROUP)), 
                        table_metadata = paste0("Lookup table for which strata are contained within a given AREA_ID. This table can be used in tandom with the GAP_PRODCUTS.AREA or AKFIN_AREA tables. ", 
                                                legal_disclaimer),
                        share_with_all_users = TRUE)

# SURVEY_YEAR ------------------------------------------------------------------

# SURVEY_YEAR <- read.csv("./temp/Tables/stratum_years.csv")
AKFIN_SURVEY_YEAR <- AKFIN_AREA %>% 
  dplyr::filter(AREA_TYPE == "DISTRICT") %>%
  dplyr::select(SURVEY_DEFINITION_ID, DESIGN_YEAR) %>% 
  dplyr::distinct() %>% 
  dplyr::mutate(YEAR = DESIGN_YEAR) # could be important later in the survey dev and is important for processing through gapindex

gapindex::upload_oracle(x = AKFIN_SURVEY_YEAR, 
                        table_name = "AKFIN_SURVEY_YEAR", 
                        channel = channel_ehm, 
                        schema = "markowitze", 
                        metadata_column = metadata_column %>% dplyr::filter(colname %in% names(AKFIN_SURVEY_YEAR)), 
                        table_metadata = paste0("Lookup table for DESIGN_YEAR should be used for which survey YEAR. This table can be used in tandom with the GAP_PRODCUTS.SURVEY_YEAR or AKFIN_SURVEY_YEAR tables. ", 
                                                legal_disclaimer),
                        share_with_all_users = TRUE)

## STOCK STRATUM lookups -------------------------------------------------------

# # TOLEDO - why is this separate? I'm joining it with stratum_nstations because it doesn't seem to come up anywhere else - why have a seperate reference table for it if its only used here?
# # stratum_years <- read.csv("./temp/Tables/stratum_years.csv")  # TOLDEO - what... is this?
# 
# stratum_nstations <- read.csv("./temp/Tables/stratum_nstations.csv") %>% # TOLDEO - what... is this?
#   dplyr::left_join(read.csv("./temp/Tables/stratum_years.csv"), relationship = "many-to-many") %>% 
#   dplyr::rename(SPECIES_CODE = SPECIES, 
#                 N_COUNT = N_STATIONS, # is this what this is refering to??? 
#                 # AREA_ID = YEAR_BLOCK_ID, 
#                 AREA_KM2 = TOTAL_AREA, 
#                 DESIGN_YEAR = SURVEY_YEAR) %>% 
#   dplyr::mutate(    SPECIES_CODE = dplyr::case_when(
#     SPECIES_CODE == "bairdi" ~  68560, 
#     SPECIES_CODE == "bkc" ~  69323, 
#     SPECIES_CODE == "ei" ~  69400, 
#     SPECIES_CODE == "hybrid" ~  68590, 
#     SPECIES_CODE == "opilio" ~  68580, 
#     SPECIES_CODE == "rkc" ~  69322
#   )) %>% 
#   dplyr::left_join(species_lookup %>% 
#                      dplyr::select(SPECIES_CODE) %>% 
#                      dplyr::mutate(AREA_ID = 1:nrow(species_lookup))) %>%  # simplistic, but maintains order and it seems that stratification is (at this point) solely based on species_code, though in the future could be species code and new survey design/districts?
#   dplyr::mutate(
#     AREA_ID = AREA_ID + 88000, # can be anything - GAP designs are 99###
#     AREA_KM2 = AREA_KM2/100) # toledo, just guessing that this is in hectares and converting it to km2
# 
# metadata_column <- metadata_column %>% 
#   dplyr::add_row(data.frame(colname = "DISTRICT", 
#                             colname_long = "Crab stock districts", 
#                             units = "text", 
#                             datatype = "VARCHAR2(255 BYTE)", 
#                             colname_desc = "Districts used to subset stock boundaries."))
# 
# gapindex::upload_oracle(x = stratum_nstations, 
#                         table_name = "STRATUM_NSTATIONS", 
#                         channel = channel_ehm, 
#                         schema = "markowitze", 
#                         metadata_column = metadata_column %>% dplyr::filter(colname %in% names(stratum_nstations)), 
#                         table_metadata = paste0("Number of stations for each stratum and other metadata. ", legal_disclaimer),
#                         share_with_all_users = TRUE)
# 
# temp_oracle_push(x = stratum_nstations, 
#                  table_name = "STRATUM_NSTATIONS", 
#                  channel = channel_ehm, 
#                  schema = "markowitze", 
#                  metadata_column = metadata_column %>% dplyr::filter(colname %in% names(stratum_nstations)), 
#                  table_metadata = paste0("Number of stations for each stratum and other metadata. ", legal_disclaimer),
#                  share_with_all_users = TRUE)

# AKFIN_STRATUM_GROUP0

# AKFIN_STRATUM_GROUP0 <- stratum_nstations %>% 
#   dplyr::mutate(
#     # SURVEY_DEFINITION_ID = dplyr::case_when( # guessing on all of these
#     #   DISTRICT == "Pribilof MTCA" ~ 98,
#     #     DISTRICT == "St. Matthew MTCA" ~ 98,
#     #     DISTRICT == "West 166" ~ 98,
#     #     DISTRICT == "East 166" ~ 98,
#     #     DISTRICT == "BKC Unstratified" ~ 98,
#     #     DISTRICT == "Pribilof Single" ~ 98,
#     #     DISTRICT == "St. Matthew Single" ~ 98,
#     #     DISTRICT == "Northern Unstratified" ~ 143,
#     #     DISTRICT == "Bristol Bay" ~ 98,
#     #     DISTRICT == "Single" ~ 98,
#     #     DISTRICT == "Norton Sound" ~ 98,
#     #     DISTRICT == "NBS All" ~ 143
#     # ), 
#     # DESCRIPTION = paste0(SPECIES_CODE, " ", DISTRICT), 
#     SURVEY_DEFINITION_ID = NA, 
#     DESCRIPTION = paste0(SPECIES_CODE, " ", DISTRICT), 
#     STRATUM = as.numeric(as.factor(paste0(SPECIES_CODE, " ", DISTRICT)))) %>% 
#   dplyr::distinct() %>% 
#   dplyr::select(AREA_ID, SURVEY_DEFINITION_ID, DESIGN_YEAR, STRATUM, DESCRIPTION, SPECIES_CODE, DISTRICT) # should remove SPECIES_CODE, DISTRICT
# 
# # AKFIN_STRATUM_GROUP0 %>% dplyr::select(STRATUM, SPECIES_CODE, DISTRICT) %>% dplyr::distinct()
# 
# gapindex::upload_oracle(x = AKFIN_STRATUM_GROUP0, 
#                         table_name = "XAKFIN_STRATUM_GROUP0", 
#                         channel = channel_ehm, 
#                         schema = "markowitze", 
#                         metadata_column = metadata_column %>% dplyr::filter(colname %in% names(AKFIN_STRATUM_GROUP0)), 
#                         table_metadata = paste0("Lookup table for which strata are contained within a given AREA_ID. This table can be used in tandom with the GAP_PRODCUTS.AREA or AKFIN_AREA tables. ", 
#                                                 legal_disclaimer),
#                         share_with_all_users = TRUE)
# 
# temp_oracle_push(x = AKFIN_STRATUM_GROUP0, 
#                  table_name = "XAKFIN_STRATUM_GROUP0", 
#                  channel = channel_ehm, 
#                  schema = "markowitze", 
#                  metadata_column = metadata_column %>% dplyr::filter(colname %in% names(AKFIN_STRATUM_GROUP0)), 
#                  table_metadata = paste0("Lookup table for which strata are contained within a given AREA_ID. This table can be used in tandom with the GAP_PRODCUTS.AREA or AKFIN_AREA tables. ", 
#                                          legal_disclaimer),
#                  share_with_all_users = TRUE)
# 
# # AKFIN_AREA
# 
# AKFIN_AREA <- stratum_nstations %>% 
#   dplyr::mutate(SURVEY_DEFINITION_ID = NA, 
#                 AREA_TYPE = "CRAB_STOCK", 
#                 DESCRIPTION = paste0(SPECIES_CODE, " ", DISTRICT), 
#                 DEPTH_M_MIN = NA, 
#                 DEPTH_M_MAX = NA) %>% 
#   dplyr::select(SURVEY_DEFINITION_ID, DESIGN_YEAR, AREA_ID, AREA_TYPE, 
#                 AREA_NAME = DISTRICT, 
#                 DESCRIPTION, AREA_KM2, DEPTH_M_MIN, DEPTH_M_MAX) %>% 
#   dplyr::distinct()
# 
# gapindex::upload_oracle(x = AKFIN_AREA, 
#                         table_name = "AKFIN_AREA", 
#                         channel = channel_ehm, 
#                         schema = "markowitze", 
#                         metadata_column = metadata_column %>% dplyr::filter(colname %in% names(AKFIN_AREA)), 
#                         table_metadata = paste0("Lookup table for which area are contained within a given AREA_ID for each DESIGN_YEAR. This table can be used in tandom with the GAP_PRODCUTS.STARTUM_GROUPS or AKFIN_STARTUM_GROUPS tables. ", 
#                                                 legal_disclaimer),
#                         share_with_all_users = TRUE)  
# 
# temp_oracle_push(x = AKFIN_AREA, 
#                  table_name = "AKFIN_AREA", 
#                  channel = channel_ehm, 
#                  schema = "markowitze", 
#                  metadata_column = metadata_column %>% dplyr::filter(colname %in% names(AKFIN_AREA)), 
#                  table_metadata = paste0("Lookup table for which area are contained within a given AREA_ID for each DESIGN_YEAR. This table can be used in tandom with the GAP_PRODCUTS.STARTUM_GROUPS or AKFIN_STARTUM_GROUPS tables. ", 
#                                          legal_disclaimer),
#                  share_with_all_users = TRUE)

## STOCK STATIONS lookups ------------------------------------------------------

# TOLDEO - this table could be useful to both SAP and GAP! We don't currrently have a table for this, but desperately need one

library("akgfmaps")
ebs <- akgfmaps::get_base_layers(select.region = "bs.south", include.corners = FALSE)$survey.grid
ebs_c <- akgfmaps::get_base_layers(select.region = "bs.south", include.corners = TRUE)$survey.grid
nbs <- akgfmaps::get_base_layers(select.region = "bs.north")$survey.grid
goa <- akgfmaps::get_base_layers(select.region = "goa")$survey.grid
ai <- akgfmaps::get_base_layers(select.region = "ai")$survey.grid
# bbs <- akgfmaps::get_base_layers(select.region = "ebs.slope")$survey.grid # doesn't exist

station <- data.frame(STATION = ebs$STATIONID,
                      # TYPE = "STANDARD",
                      SURVEY_DEFINITION_ID = 98) %>%
  dplyr::bind_rows(data.frame(STATION = ebs_c$STATIONID[!(ebs_c$STATIONID %in% ebs$STATIONID)],
                              # TYPE = "CORNER",
                              SURVEY_DEFINITION_ID = 98)) %>%
  dplyr::bind_rows(data.frame(STATION = nbs$STATIONID,
                              # TYPE = "STANDARD",
                              SURVEY_DEFINITION_ID = 143)) %>%
  dplyr::bind_rows(data.frame(STATION = goa$ID,
                              STRATUM = goa$STRATUM,
                              # TYPE = "STANDARD",
                              SURVEY_DEFINITION_ID = 47)) %>%
  dplyr::bind_rows(data.frame(STATION = ai$ID,
                              STRATUM = ai$STRATUM,
                              # TYPE = "STANDARD",
                              SURVEY_DEFINITION_ID = 52)) 
# 
# station <- station %>%
#   dplyr::right_join(old_station, by = c("STATION", "SURVEY_DEFINITION_ID"))

# station_gap <- RODBC::sqlQuery(
#   channel = channel_products, 
#   query = "SELECT * FROM GAP_PRODUCTS.OLD_STATION") %>% 
#   dplyr::select(STRATUM, STATION, SURVEY_DEFINITION_ID = SRVY, DESIGN_YEAR) %>% 
#   dplyr::mutate(
#     SURVEY_DEFINITION_ID = dplyr::case_when(
#       SURVEY_DEFINITION_ID == "GOA" ~ 47, 
#       SURVEY_DEFINITION_ID == "AI" ~ 52, 
#       SURVEY_DEFINITION_ID == "EBS" ~ 98, 
#       SURVEY_DEFINITION_ID == "NBS" ~ 143, 
#       SURVEY_DEFINITION_ID == "BSS" ~ 78, 
#     ), 
#     AREA_ID = dplyr::case_when(
#       SURVEY_DEFINITION_ID == "47" ~ 99903, 
#       SURVEY_DEFINITION_ID == "52" ~ 99904, 
#       SURVEY_DEFINITION_ID == "98" ~ 99900, 
#       SURVEY_DEFINITION_ID == "143" ~ 99902, 
#       SURVEY_DEFINITION_ID == "78" ~ 99905, 
#     ), 
#     STATION_TYPE = "STANDARD") # sort of true

stock_stations_lookup <- 
  read.csv("./temp/Tables/stock_stations_lookup.csv") %>% 
  tidyr::pivot_longer(cols = bairdi:rkc, 
                      names_to = "SPECIES_CODE", 
                      values_to = "DISTRICT") %>% 
  dplyr::rename(STATION = STATION_ID, 
                LONGITUDE_DD = LONGITUDE, 
                LATITUDE_DD = LATITUDE, 
                # DESCRIPTION = DISTRICT, 
                STATION_TYPE = TYPE) %>%
  dplyr::mutate(
    SPECIES_CODE = dplyr::case_when(
      SPECIES_CODE == "bairdi" ~  68560, 
      SPECIES_CODE == "bkc" ~  69323, 
      SPECIES_CODE == "ei" ~  69400, 
      SPECIES_CODE == "hybrid" ~  68590, 
      SPECIES_CODE == "opilio" ~  68580, 
      SPECIES_CODE == "rkc" ~  69322
    ), 
    STATION_TYPE = toupper(STATION_TYPE), 
    DESIGN_YEAR = 1975) %>% # I think?
  dplyr::left_join(station %>% dplyr::select(STATION, SURVEY_DEFINITION_ID)) %>% # station are unique in EBS, NBS, and BSS
  dplyr::left_join(AKFIN_STRATUM_GROUP %>% 
                     dplyr::select(-DESIGN_YEAR) %>% # , -SURVEY_DEFINITION_ID
                     dplyr::distinct(), 
                   relationship = "many-to-many") %>% 
  dplyr::bind_rows(station) %>% # GAP content
  dplyr::mutate(SURVEY_DEFINITION_ID = ifelse(is.na(SURVEY_DEFINITION_ID), 98, SURVEY_DEFINITION_ID) ) %>% #  == "MISC"
  dplyr::select(SURVEY_DEFINITION_ID, AREA_ID, STRATUM, STATION, STATION_TYPE, LONGITUDE_DD, LATITUDE_DD, DESIGN_YEAR) %>% # , DISTRICT, SPECIES_CODE
  dplyr::arrange(desc(STATION))

metadata_column <- metadata_column %>% 
  dplyr::add_row(data.frame(colname = "STATION_TYPE", 
                            colname_long = "Station type description", 
                            units = "text", 
                            datatype = "VARCHAR2(255 BYTE)", 
                            colname_desc = "Type of station."))

gapindex::upload_oracle(x = stock_stations_lookup, 
                        table_name = "AKFIN_STATION", 
                        channel = channel_ehm, 
                        schema = "markowitze", 
                        metadata_column = metadata_column %>% dplyr::filter(colname %in% names(stock_stations_lookup)), 
                        table_metadata = paste0("Lookup table of stations. ", legal_disclaimer),
                        share_with_all_users = TRUE)

temp_oracle_push(x = stock_stations_lookup, 
                 table_name = "AKFIN_STATION", 
                 channel = channel_ehm, 
                 schema = "markowitze", 
                 metadata_column = metadata_column %>% dplyr::filter(colname %in% names(stock_stations_lookup)), 
                 table_metadata = paste0("Lookup table of stations. ", legal_disclaimer),
                 share_with_all_users = TRUE)

## STOCK lookup table by species -----------------------------------------------

# this table doesn't seem to have much value... very redundant to other tables already here 

stock_lookup <- tibble(SPECIES = c(rep("rkc", 4),
                                   rep("bkc", 3),
                                   rep("bairdi", 4),
                                   rep("opilio", 2),
                                   rep("hybrid", 2),
                                   rep("ei", 2)),
                       SPECIES_NAME = c(rep("Red King Crab", 4),
                                        rep("Blue King Crab", 3),
                                        rep("Bairdi Tanner Crab", 4),
                                        rep("Opilio Crab", 2),
                                        rep("Chionoecetes Hybrid Crab", 2),
                                        rep("Erimacrus Hair Crab", 2)),
                       STOCK = c("BBRKC","PribRKC",
                                 "NSRKC", "NorthRKC",
                                 "PribBKC", "StMattBKC",
                                 "BKCNBS", "TannerW",
                                 "TannerWNBS", "TannerE", 
                                 "TannerENBS", "Snow", 
                                 "SnowNBS", "Hybrid",  
                                 "HybridNBS", "Hair", 
                                 "HairNBS")) %>%
  # dplyr::rename(COMMON_NAME = SPECIES_NAME) %>% 
  dplyr::mutate(
    SPECIES_CODE = dplyr::case_when(
      SPECIES == "bairdi" ~  68560, 
      SPECIES == "bkc" ~  69323, 
      SPECIES == "ei" ~  69400, 
      SPECIES == "hybrid" ~  68590, 
      SPECIES == "opilio" ~  68580, 
      SPECIES == "rkc" ~  69322
    )) %>% 
  dplyr::select(SPECIES_CODE, STOCK)

# gapindex::upload_oracle(x = stock_lookup, 
#                         table_name = "XSTOCK", 
#                         channel = channel_ehm, 
#                         schema = "markowitze", 
#                         metadata_column = metadata_column %>% dplyr::filter(colname %in% names(stock_lookup)), 
#                         table_metadata = paste0("Stock district lookup table. ", legal_disclaimer),
#                         share_with_all_users = TRUE)
# 
# temp_oracle_push(x = stock_lookup, 
#                  table_name = "XSTOCK", 
#                  channel = channel_ehm, 
#                  schema = "markowitze", 
#                  metadata_column = metadata_column %>% dplyr::filter(colname %in% names(stock_lookup)), 
#                  table_metadata = paste0("Stock district lookup table. ", legal_disclaimer),
#                  share_with_all_users = TRUE)


# # AKFIN_STRATUM_GROUP0 repise ---------------------------------------------------
# 
# # Seems that there are some mismatches we need to combine
# 
# AKFIN_STRATUM_GROUP <- AKFIN_STRATUM_GROUP0 %>% 
#   dplyr::ungroup() %>% 
#   dplyr::full_join(dist_stock_lookup) %>% 
#   dplyr::ungroup() %>% 
#   dplyr::mutate(DESCRIPTION = paste0(STOCK, " ", DESCRIPTION)) %>% 
#   dplyr::select(-SURVEY_DEFINITION_ID) %>%
#   dplyr::left_join(stock_stations_lookup %>% 
#                      dplyr::select(AREA_ID, SURVEY_DEFINITION_ID, STRATUM) %>% 
#                      dplyr::distinct()) %>% 
#   dplyr::ungroup()  
# 
# temp <- AKFIN_STRATUM_GROUP %>% 
#   dplyr::filter(is.na(AREA_ID)) %>% 
#   dplyr::distinct() %>% 
#   dplyr::mutate(STRATUM = (1:nrow(.))+max(AKFIN_STRATUM_GROUP$STRATUM, na.rm = TRUE), 
#                 # STARTUM = STARTUM + ifelse(is.na(SPECIES_CODE), 100, 0), 
#                 AREA_ID = STRATUM + 88000, 
#                 DESIGN_YEAR = 1975)
# 
# AKFIN_STRATUM_GROUP <- dplyr::left_join(AKFIN_STRATUM_GROUP %>% 
#                                           dplyr::filter(is.na(AREA_ID)) %>% 
#                                           dplyr::select(-AREA_ID, -STRATUM), 
#                                         temp) %>%
#   dplyr::bind_rows(AKFIN_STRATUM_GROUP %>% 
#                      dplyr::filter(!is.na(AREA_ID)) )
# 
# gapindex::upload_oracle(x = AKFIN_STRATUM_GROUP, 
#                         table_name = "AKFIN_STRATUM_GROUP", 
#                         channel = channel_ehm, 
#                         schema = "markowitze", 
#                         metadata_column = metadata_column %>% dplyr::filter(colname %in% names(AKFIN_STRATUM_GROUP)), 
#                         table_metadata = paste0("Lookup table for which strata are contained within a given AREA_ID. This table can be used in tandom with the GAP_PRODCUTS.AREA or AKFIN_AREA tables. ", 
#                                                 legal_disclaimer),
#                         share_with_all_users = TRUE)
# 
# temp_oracle_push(x = AKFIN_STRATUM_GROUP, 
#                  table_name = "AKFIN_STRATUM_GROUP", 
#                  channel = channel_ehm, 
#                  schema = "markowitze", 
#                  metadata_column = metadata_column %>% dplyr::filter(colname %in% names(AKFIN_STRATUM_GROUP)), 
#                  table_metadata = paste0("Lookup table for which strata are contained within a given AREA_ID. This table can be used in tandom with the GAP_PRODCUTS.AREA or AKFIN_AREA tables. ", 
#                                          legal_disclaimer),
#                  share_with_all_users = TRUE)

## MALE CUTLINES lookup table --------------------------------------------------
mat_lookup <- tibble(stock = c("BBRKC", "PribRKC", "PribBKC", "StMattBKC", "TannerE", "TannerW","Snow", "Hybrid", "Hair", "NorthRKC","NSRKC", "BKCNBS", "SnowNBS"),
                     cutline = c(120, 120, 120, 105, 113, 103, 95, 95, NA, 120, 94, 94, 68),
                     legal = c(135, 135, 135, 120, 120, 110, 78, 78, 83, 135, 104, 104, 78),
                     recruit = c(134, 134, 134, 119, 124, 124, 101, 101, NA, 134, NA, NA, NA),
                     preferred = c(NA, NA, NA, NA, 125, 125, 102, 102, NA, NA, NA, NA, 102),
                     title = c("Bristol Bay Red King Crab", "Pribilof Islands Red King Crab", "Pribilof Islands Blue King Crab",
                               "St. Matthew Island Blue King Crab", "Tanner Crab East", "Tanner Crab West", "Snow Crab", 
                               "Hybrid Tanner-Snow Crab","Hair Crab","Northern District Red King Crab", "Norton Sound Red King Crab","Northern Bering Sea Blue King Crab","Northern Bering Sea Snow Crab")) %>% 
  dplyr::select(STOCK = stock, 
                STOCK_LONG = title, 
                LENGTH_MM_CUTLINE = cutline, # What does this mean?
                LENGTH_MM_LEGAL = legal, 
                LENGTH_MM_PREFERED = preferred, 
                LENGTH_MM_RECRUIT = recruit) %>% 
  dplyr::full_join(stock_lookup)
  # dplyr::left_join(dist_stock_lookup)
# TOLEDO - really needs DISTRICT, right?

metadata_column <- metadata_column %>% 
  dplyr::add_row(data.frame(colname = "LENGTH_MM_PREFERED", 
                            colname_long = "Prefered length of stock males", 
                            units = "millimeters", 
                            datatype = "NUMBER(10,0)", 
                            colname_desc = "Length bin in millimeters of prefered length of stock males.")) %>% 
  dplyr::add_row(data.frame(colname = "LENGTH_MM_LEGAL", 
                            colname_long = "Legal length of stock males", 
                            units = "millimeters", 
                            datatype = "NUMBER(10,0)", 
                            colname_desc = "Length bin in millimeters of legal length of stock males.")) %>% 
  dplyr::add_row(data.frame(colname = "LENGTH_MM_RECRUIT", 
                            colname_long = "Recruit length of stock males", 
                            units = "millimeters", 
                            datatype = "NUMBER(10,0)", 
                            colname_desc = "Length bin in millimeters of recruit length of stock males.")) %>% 
  dplyr::add_row(data.frame(colname = "LENGTH_MM_CUTLINE", 
                            colname_long = "Cutline length of stock males", 
                            units = "millimeters", 
                            datatype = "NUMBER(10,0)", 
                            colname_desc = "Length bin in millimeters of cutline length of stock males.")) %>% 
  dplyr::add_row(data.frame(colname = "STOCK_LONG", 
                            colname_long = "Name of stock", 
                            units = "text", 
                            datatype = "VARCHAR2(255 BYTE)", 
                            colname_desc = "Name of stock.")) %>% 
  dplyr::add_row(data.frame(colname = "STOCK", 
                            colname_long = "Abbreviated name of stock males", 
                            units = "text", 
                            datatype = "VARCHAR2(255 BYTE)", 
                            colname_desc = "Abbreviated name of stock."))


# ## CLUTCH CODES lookup table ------------------------------------------------------------ 
# # *double check clutch definitions/maturity "names"
# clutch_code_maturity <- tibble(EGG_COLOR = c(0, 0, rep(seq(from = 2, to = 6), 3), 0, seq(from = 2, to = 6), 9),
#                                EGG_CONDITION = c(0, 0, rep(1, 5), rep(2, 5), rep(3, 5), 4, rep(5, 5), 9),
#                                CLUTCH_SIZE = c(0, 1, rep(seq(from = 2, to = 6), 3), 1, seq(from = 2, to = 6), 9),
#                                MATURITY = c("Immature", "No eggs", rep("Uneyed", 5), rep("Eyed", 5), rep("Dead", 5), 
#                                             "Barren", rep("Hatching", 5), "Missing abdominal flap"))

# LOOKUP TABLES --------------------------------------------------------------------------------
## CHELA CUTLINE PARAMETER lookup table ------------------------------------------------------------ 
chela_param_lookup <- tibble(SPECIES = c("Bairdi", 
                                         "Opilio"),
                             SLOPE = c(1.189, 
                                       1.143),
                             INTERCEPT = c(-2.674, 
                                           -2.241),
                             SPECIES_CODE = c(68560, 
                                              68580),
                             NOTES = c("natural log transform chela and carapace ln(Ch) = m*ln(CW) + b",
                                       "natural log transform chela and carapace ln(Ch) = m*ln(CW) + b")) %>% 
  dplyr::select(-SPECIES) %>% 
  dplyr::rename(DESCRIPTION = NOTES)

mat_lookup0 <- dplyr::left_join(mat_lookup, chela_param_lookup) %>% 
  dplyr::arrange(desc(STOCK))

gapindex::upload_oracle(x = mat_lookup0, 
                        table_name = "STOCK_MATURITY_CUTOFF", 
                        channel = channel_ehm, 
                        schema = "markowitze", 
                        metadata_column = metadata_column %>% dplyr::filter(colname %in% names(mat_lookup0)), 
                        table_metadata = paste0("MALE cutlines lookup table", legal_disclaimer),
                        share_with_all_users = TRUE)
