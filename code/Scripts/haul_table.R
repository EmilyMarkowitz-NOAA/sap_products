# PURPOSE -------------------------------------------------------------------------------------------------------------------
# Below notes from SQL script:
# -- This script creates a table for haul data, by survey year, of the standard survey tows successfully completed during the 
# -- 1975 - present eastern Bering Sea crab-groundfish trawl surveys for analysis. Data are retrieved from the racebase.haul 
# -- Oracle schema. Only haul type 3 (standard) hauls are included in these data sets (with some exceptions for earlier years).
# -- Mid-latitude and mid-longitude are calculated for each haul, and station id's are standardized using **Oracle Locator.
#
# Author: Shannon Hennessey, NOAA-AFSC, based on SQL code from Jon Richar and Claire Armistead

# LOAD DATA -----------------------------------------------------------------------------------------------------------------

# Set data directory
# data_dir <- "Y:/KOD_Survey/EBS Shelf/Data_Processing/Data/"
data_dir <- here::here("temp")

# Download most recent version of RACEBASE.HAUL
if(ORACLE == TRUE){
  print("connecting to Oracle")
  
  # Connect to Oracle
  channel <- gapindex::get_connected(db = "AFSC", check_access = FALSE) # will need to input SQL Developer username and password
  
  # Download haul file from RACEBASE and save locally
  a <- RODBC::sqlQuery(channel, paste0("SELECT * FROM ", "RACEBASE.HAUL"), as.is = TRUE) # use "as.is = TRUE" to get full start date/time in START_TIME column
  write.csv(x = a, file = paste0(data_dir, "/RACEBASE_HAUL.csv"), row.names = FALSE) # save locally for easier processing
  
  print("downloaded RACEBASE.HAUL")
}

# Read in unfiltered haul table
dat <- read.csv(paste0(data_dir, "/RACEBASE_HAUL.csv"))

print("loaded RACEBASE.HAUL")

# Read in EBS stations grid to assign GIS_STATION
# grid <- st_read(paste0(data_dir, "spatial_layers/EBS_NBS_grid.gdb"), layer = 'ebs_grid_with_corners') # NAD83
grid <- sf::st_read("./temp/Tables/EBS_NBS_grid.gdb", layer = 'ebs_grid_with_corners') # NAD83

# Read in 1979 stations lookup table
# stations_1979 <- read.csv(paste0(data_dir, "lookup_tables/station_lookup_1979.csv"))
stations_1979 <- read.csv("./temp/Tables/station_lookup_1979.csv") %>% 
  dplyr::rename(YEAR = SURVEY_YEAR, 
                VESSEL_ID = VESSEL, 
                STATION = STATIONID) %>% 
  dplyr::select(-GIS_STATION) # redundant and unhelpful

# PROCESS HAUL TABLE --------------------------------------------------------------------------------------------------------
# Coarse filtering of haul file based on broadly applicable parameters across years
haul <- dat %>% 
  dplyr::filter(REGION == "BS") %>%
  dplyr::mutate(SURVEY_YEAR = as.integer(sub("-.*", "", START_TIME))) %>% # create 'SURVEY_YEAR' column
  dplyr::filter(SURVEY_YEAR >= 1975, # start timeseries at 1975
                PERFORMANCE >= 0) #%>% 
# dplyr::rename(YEAR = SURVEY_YEAR)

# LOOPING THROUGH EACH YEAR ----------------------------------------------------------------------------------------------

current_year <- 2024
years <- c(1975:current_year)
haul_table <- c()

for(i in 1:length(years)){
  temp <- haul %>% dplyr::filter(SURVEY_YEAR == years[i])
  
  if(years[i] == 1975){ 
    temp <- temp %>%
      dplyr::filter(CRUISE == 197502,
                    VESSEL == 14,
                    HAUL_TYPE == 3,
                    HAUL <= 145,
                    # -- don't include the tow at B-18 (outside survey grid, beyond 200m contour)
                    # -- or tows at D-11, D-21, and E-23 (outside grid)
                    !HAUL %in% c(9,92,97,101))
  }
  
  if(years[i] == 1976){ 
    temp <- temp %>%
      dplyr::filter(CRUISE %in% c(197601,197602),
                    VESSEL %in% c(14,17,19,21),
                    (HAUL_TYPE == 3 & DURATION < 1.0) |
                      (HAUL_TYPE == 0 & HAUL %in% c(5,8,19,26)),
                    # -- don't include tows outside survey grid (inshore or beyond 200m contour: D-11, A-01, C-19, D-21, E-23)
                    !(VESSEL == 14 & HAUL %in% c(40,131,135,137,140)),
                    # -- don't include extra tows at stations previously sampled
                    !(VESSEL == 14 & HAUL %in% c(152,153,154,155,157,161,163,167,169,170,172,173,
                                                 175,176,177,178,179,180,181,182,183,184,186)),
                    !(VESSEL == 17 & HAUL %in% c(1:71,76:80,82,91:99,101:115,117:122,126:129,131,
                                                 133,135,138,140,161)),
                    !(VESSEL == 19 & HAUL %in% c(1:75,77,79,81:92,98:108,110:161,179,182,184:186,
                                                 188,190,192,194,196,197)),
                    !(VESSEL == 21 & CRUISE == 197602),
                    !(VESSEL == 21 & HAUL %in% c(1:105,107:258))) 
    
  }
  
  if(years[i] == 1977){ 
    temp <- temp %>%
      dplyr::filter(CRUISE == 197703,
                    VESSEL == 14, 
                    HAUL_TYPE == 3,
                    # -- don't include tows at D-11, B-18, E-23 (outside survey grid)
                    !HAUL %in% c(5,109,152))
  }
  
  if(years[i] == 1978){ 
    temp <- temp %>%
      dplyr::filter(HAUL_TYPE == 3,
                    # -- some tows outside the survey grid need to be removed (some deep, some inshore)
                    # -- and don't include tow at station Z-04 (haul 88)
                    (CRUISE == 197802 & VESSEL == 14 & !HAUL %in% c(27,43,89,90,88)) |
                      (CRUISE == 197801 & VESSEL == 28 & !HAUL %in% c(168,169,186,194,196,197,198,199,200))) 
  }
  
  if(years[i] == 1979){ 
    # -- use 1979 haul table created by Gary Walters to get as close as possible
    # -- to evenly spaced tows (selecting for haul type 3 only will result in a
    # -- data set with lots of multiple tows at station. Also need to use Gary's table
    # -- to assign corner station id's to non-Prib/St Matt corners. 
    # -- 1979 was a flaky survey!!
    temp <- temp %>%
      dplyr::filter(CRUISE %in% c(197901,197902),
                    VESSEL %in% c(12,14,28),
                    HAUL_TYPE == 3,
                    # -- don't include tows at D-11, B-18, E-23 (outside survey grid)
                    !(VESSEL == 12 & HAUL %in% c(5,9,31,32,40:79,109:158)),
                    !(VESSEL == 14 & HAUL %in% c(23:26,33,161:165)),
                    !(VESSEL == 28 & HAUL %in% c(2:14,16,25,26,31:33,35:96,98:100,102,103,105:108,
                                                 110,112,113,115:117,119,120,122:126,128,132,134:140,
                                                 143,146,148:153,155:158,161,162,164:168,170,171,173,
                                                 175,177:179,205,212,214,226,236:240,273:297,300,301)))
  }
  
  if(years[i] == 1980){ 
    # -- This data set comprises the 338 standard survey tows taken during the 1980 EBS crab-groundfish 
    # -- survey.  It does not include the 14 hauls conducted by vessel 14 in July (hauls 112-123,126-127)
    # -- at stations previously towed, or the 26 comparative trawling tows conducted by vessel 31 and 
    # -- a Japanese Fisheries Agency vessel.
    # SH note: based on currently used haul table, pared to 320 tows
    temp <- temp %>%
      dplyr::filter(HAUL_TYPE == 3,
                    (CRUISE == 198002 & VESSEL == 14 & !HAUL %in% c(112:123,126,127, 47:49,53:56,58,61,63,65,70,72,76)) | # added second sequence of #s to filter 14 extra stations when compared to old haul table
                      (CRUISE == 198001 & VESSEL == 31 & !HAUL %in% c(75,190,196,221))) 
  }
  
  if(years[i] == 1981){ 
    # -- This dataset comprises the 355 hauls used for analysis of the 1981 EBS crab data. 280 hauls 
    # -- were coded haul type 3 and 75 were gear comparison hauls that were also used for standard data analysis.
    # -- Selecting only haul type 3 would leave a 36-station block in Bristol Bay unsampled.
    # SH note: there were originally 355, but the HT we use now only has 305 hauls -- unclear why some were omitted
    temp <- temp %>%
      dplyr::filter(HAUL_TYPE %in% c(3,4),
                    (CRUISE == 198103 & VESSEL == 1 & !HAUL %in% c(1,147,148,153,154,164,165)) |
                      (CRUISE == 198101 & VESSEL == 37 & !HAUL %in% c(46,47,132,154:156)),
                    !(VESSEL == 1 & HAUL_TYPE == 4)) 
  }
  
  if(years[i] == 1982){ 
    # -- This dataset comprises the 336 standard EBS shelf survey tows completed during the 1982 triennial EBS 
    # -- crab-groundfish trawl survey. It does not include the 18 gear comparison tows that were made during 
    # -- this survey. North shelf and Norton Sound surveys were conducted by vessel 21, cruise 198202 
    # -- (hauls 2-19 = north shelf, hauls 20-77 = Norton Sound). The EBS slope survey east of the US-USSR Convention 
    # -- Line of 1867 was conducted by the Japanese research vessel Ryujin Maru No. 8 (vessel 515 cruise 198201), 
    # -- and the slope waters west of the Convention Line were surveyed by the Soviet research vessel SRTM-8459 
    # -- (vessel 784 cruise 198201). 
    # SH note: based on currently used haul table, there are 342 tows
    temp <- temp %>%
      dplyr::filter(HAUL_TYPE %in% c(3,4),
                    (CRUISE == 198203 & VESSEL == 1) |
                      # -- don't include hauls outside survey grid
                      (CRUISE == 198201 & VESSEL == 19 & !HAUL %in% c(160:166,178,179,180,202,203))) 
  }
  
  if(years[i] == 1983){ 
    # -- This dataset comprises the 354 standard survey tows completed during the 1983 EBS crab-groundfish trawl 
    # -- survey. It does not include the 20 special project tows (gear comparison) that were made during this survey. 
    # SH note: based on currently used haul table, pared to 353 tows
    temp <- temp %>%
      dplyr::filter(HAUL_TYPE == 3,
                    (CRUISE == 198303 & VESSEL == 1) |
                      # -- don't include hauls outside survey grid
                      (CRUISE == 198301 & VESSEL == 37 & !HAUL == 79)) 
  }
  
  if(years[i] == 1984){ 
    # -- This dataset comprises the 355 standard survey tows completed during the 1984 EBS crab-groundfish trawl 
    # -- survey. It does not include the 103 special project tows (23 red king crab assessment tows, and 80 gear 
    # -- comparison tows) that were made during this survey.
    temp <- temp %>%
      dplyr::filter(HAUL_TYPE == 3,
                    (CRUISE == 198402 & VESSEL == 1) |
                      (CRUISE == 198401 & VESSEL == 37)) 
  }
  
  if(years[i] == 1985){ 
    # -- This dataset comprises the 355 standard EBS shelf survey tows completed during the 1985 triennial EBS 
    # -- crab-groundfish trawl survey. It does not include the 5 opportunistic, 27 gear comparison tows, and 22 
    # -- walleye pollock distribution study tows that were made during this survey. North shelf and Norton Sound 
    # -- surveys were conducted by vessel 60, cruise 198501 (hauls 211-289 = Norton Sound, hauls 208-210,291-303 = north shelf).
    # -- The EBS slope survey was conducted by the Daikichi Maru No. 32 (vessel 556).
    # SH note: based on currently used haul table, pared to 353 tows
    temp <- temp %>%
      dplyr::filter(HAUL_TYPE == 3,
                    CRUISE == 198501,
                    VESSEL == 37 |
                      (VESSEL == 60 & !HAUL >= 208)) 
  }
  
  if(years[i] == 1986){ 
    # -- This dataset comprises the 354 standard survey tows completed during the 1986 EBS crab-groundfish trawl 
    # -- survey. It does not include the 29 special project tows (6 inshore to investigate king crab by-catch rates 
    # -- in the area of a proposed Pacific cod fishery, and 23 additional tows to more precisely delineate the distribution
    # -- of red king crab) that were made during this survey. Station M-32 was not towed.
    # SH note: based on currently used haul table, pared to 353 tows
    temp <- temp %>%
      dplyr::filter(HAUL_TYPE == 3,
                    (CRUISE == 198602 & VESSEL == 57) |
                      (CRUISE == 198601 & VESSEL == 37))
  }
  
  if(years[i] == 1987){ 
    # -- This dataset comprises the 362 standard survey tows completed during the 1987 EBS crab-groundfish trawl 
    # -- survey. It does not include the 21 special project tows (12 inshore to assess the abundance of king crab, 
    # -- 6 slope transects (haul type 6), and 3 scallop mortality tows) that were made during this survey. Stations 
    # -- PO2524, P25, Q25, and Q02 were poor performance and not re-towed; station K13 was poor performance and re-tow 
    # -- wound up in J13; station QP2625 was not towed. Stations L-N30 and L-N31 were omitted from the survey.
    # SH note: based on currently used haul table, pared to 355 tows
    temp <- temp %>%
      dplyr::filter(HAUL_TYPE == 3,
                    CRUISE == 198701,
                    (VESSEL == 37 & !HAUL %in% c(190,191)) | 
                      (VESSEL == 19 & !HAUL %in% c(156,157,159,10))) # -- 2 tows at J-13, get rid of 1 in the corner (haul 10)
  }
  
  if(years[i] == 1988){ 
    # -- This dataset comprises the 373 standard EBS shelf survey tows completed during the 1988 triennial EBS 
    # -- crab-groundfish trawl survey. It does not include the 9 inshore, 44 comparative tows (Alaska and Darvin), 
    # -- and 7 inshore crab and halibut bycatch tows that were made during this survey. Stations B08, K13, and QP2625 
    # -- were not towed; 2 tows are attributed to J13, possibly a result of untrawlable bottom at K13 forcing the 
    # -- vessel further south. North shelf, Norton Sound, and EBS slope surveys were conducted by vessel 21, cruise 
    # -- 198808 (hauls 1-85 = Norton Sound, hauls 86-127 = north shelf, hauls 128-230 = slope). 
    # SH note: based on currently used haul table, pared to 370 tows
    temp <- temp %>%
      dplyr::filter(HAUL_TYPE == 3, 
                    CRUISE == 198801,
                    # -- 2 tows wind up in J-13, get rid of one not at center (haul 13)
                    # -- 2 tows wind up in K-25, get rid of one not at center (haul 161)
                    (VESSEL == 78 & !HAUL %in% c(13,161)) | 
                      (VESSEL == 37)) 
  }
  
  if(years[i] == 1989){ 
    # -- This dataset comprises the 374 survey tows completed during the 1989 EBS crab-groundfish trawl survey. 
    # -- It does not include the 11 inshore, 36 comparative tows, 11 inshore crab and halibut bycatch tows, 
    # -- and 5 additional northwest tows for C. opilio that were made during this survey. 
    # -- Station E04 was changed to a poor performance code after the survey was over and was not re-towed.
    # SH note: based on currently used haul table, pared to 353 tows (rm Z-04/AZ0504)
    temp <- temp %>%
      dplyr::filter(HAUL_TYPE == 3, 
                    CRUISE == 198901,
                    VESSEL %in% c(37,78)) 
  }
  
  if(years[i] == 1990){ 
    # -- This dataset comprises the 371 survey tows completed during the 1990 EBS crab-groundfish trawl survey. 
    # -- It does not include the 6 inshore and 19 opportunistic tows that were made during this survey.
    # -- Stations A06, N05, Q25, P25, and QP2625 were not sampled.
    # SH note: based on currently used haul table, pared to 370 tows (rm Z-04/AZ0504)
    temp <- temp %>%
      dplyr::filter(HAUL_TYPE == 3, 
                    CRUISE == 199001,
                    VESSEL %in% c(37,78))  
  }
  
  if(years[i] == 1991){ 
    # -- This dataset comprises the 372 standard EBS survey stations sampled during the 1991 triennial survey.  
    # -- Stations A06, F20, Q25, and P25 were not sampled. 6 opportunistic (haul type 0) and 3 in-shore 
    # -- (haul type 21) tows were also made, but are not included in this standard station dataset. Vessel 21 
    # -- cruise 199110 conducted the EBS slope survey; vessel 78 cruise 199101 hauls 211-263 comprise the 
    # -- Norton Sound survey, and vessels 37 (hauls 71-72,102-104,153-157) and 78 (hauls 93-94,154-157,193-210) 
    # -- conducted the north shelf survey.
    # SH note: based on currently used haul table, pared to 371 tows (rm Z-04/AZ0504)
    temp <- temp %>%
      dplyr::filter(HAUL_TYPE == 3, 
                    CRUISE == 199101,
                    (VESSEL == 37 & !HAUL %in% c(71,72,102:104,153:157)) | 
                      (VESSEL == 78 & !HAUL %in% c(93,94,154:157,193:263))) 
  }
  
  if(years[i] == 1992){ 
    # -- This dataset comprises the 356 standard survey tows completed during the 1992 crab-groundfish trawl survey. 
    # -- 18 stations had to be dropped in order to complete the standard survey (vessel 87 had to re-fuel in Dutch Harbor
    # -- mid-leg each leg, losing sampling days). SAP and GAP staff decided to drop a block of stations west of Nunivak I.
    # -- Station J12 (vessel 87, haul 15) was determined to be poor performance after the survey and was not re-towed.
    # SH note: based on currently used haul table, pared to 355 tows (rm Z-04/AZ0504)
    temp <- temp %>%
      dplyr::filter(HAUL_TYPE == 3, 
                    CRUISE == 199201,
                    VESSEL %in% c(37,87)) 
  }
  
  if(years[i] == 1993){ 
    # -- This dataset comprises the 375 survey tows completed during the 1993 crab-groundfish trawl survey. 
    # -- Station HG2120 was not sampled.
    # SH note: based on currently used haul table, pared to 374 tows (rm Z-04/AZ0504)
    temp <- temp %>%
      dplyr::filter(HAUL_TYPE == 3, 
                    CRUISE == 199301,
                    VESSEL %in% c(88,89)) 
  }
  
  if(years[i] == 1994){ 
    # -- This dataset comprises the 375 standard survey tows completed during the 1994 crab-groundfish trawl survey. 
    # -- Station QP2524 was poor performance and not re-towed. This dataset does not include the 8 North Shelf tows 
    # -- made by vessel 89 (hauls 169-176), or the 40 special project tows made by vessel 89 hauls 401-440, haul type 4. 
    # -- Haul data from the trawl herding experiment conducted by vessel 88 are not available on racebase.haul.
    # SH note: based on currently used haul table, pared to 374 tows (rm Z-04/AZ0504)
    temp <- temp %>%
      dplyr::filter(HAUL_TYPE == 3, 
                    CRUISE == 199401,
                    VESSEL == 88 | 
                      (VESSEL == 89 & !HAUL %in% c(169:176))) 
  }
  
  if(years[i] == 1995){ 
    # -- This dataset comprises the 376 standard survey tows completed during the 1995 crab-groundfish trawl survey.
    # SH note: based on currently used haul table, pared to 375 tows (rm Z-04/AZ0504)
    temp <- temp %>%
      dplyr::filter(HAUL_TYPE == 3, 
                    CRUISE == 199501,
                    VESSEL %in% c(88,89)) 
  }  
  
  if(years[i] == 1996){ 
    # -- This dataset comprises the 375 standard survey tows completed during the 1996 crab-groundfish trawl survey. 
    # -- Station Q-25 was not sampled.
    # SH note: based on currently used haul table, pared to 374 tows (rm Z-04/AZ0504)
    temp <- temp %>%
      dplyr::filter(HAUL_TYPE == 3, 
                    CRUISE == 199601,
                    VESSEL %in% c(88,89))
  } 
  
  if(years[i] == 1997){ 
    # -- This dataset contains haul data from the 376 standard survey tows successfully made during the 1997 EBS trawl survey.
    # SH note: based on currently used haul table, pared to 375 tows (rm Z-04/AZ0504)
    temp <- temp %>%
      dplyr::filter(HAUL_TYPE == 3, 
                    CRUISE == 199701,
                    VESSEL %in% c(88,89))
  } 
  
  if(years[i] == 1998){ 
    # -- This dataset contains haul data from the 375 standard survey tows successfully made during the 1998 EBS trawl survey. 
    # -- Station HG2221 was designated poor performance and was not re-towed.
    # SH note: based on currently used haul table, pared to 374 tows (rm Z-04/AZ0504)
    temp <- temp %>%
      dplyr::filter(HAUL_TYPE == 3, 
                    CRUISE == 199801,
                    VESSEL %in% c(88,89)) 
  }
  
  if(years[i] == 1999){ 
    # -- This dataset contains haul data from the 373 standard survey tows successfully made during the 1999 EBS trawl survey. 
    # -- Stations J15 and J16 were ice-covered and could not be towed; station P25 was not sampled.
    # SH note: based on currently used haul table, pared to 372 tows (rm Z-04/AZ0504) - 403 with BB resampling
    temp <- temp %>%
      dplyr::filter(HAUL_TYPE %in% c(3,17), 
                    CRUISE == 199901,
                    VESSEL %in% c(88,89)) 
  }  
  
  if(years[i] == 2000){ 
    # -- This dataset contains haul data from the 372 standard survey tows successfully made during the 2000 EBS trawl survey.
    # -- Stations E11 and I13 were poor performance and not re-towed; stations HG2221 and Q02 were not sampled.
    # SH note: based on currently used haul table, pared to 371 tows (rm Z-04/AZ0504) - 394 with BB resampling
    temp <- temp %>%
      dplyr::filter(HAUL_TYPE %in% c(3,17), 
                    CRUISE == 200001,
                    VESSEL %in% c(88,89)) 
  }  
  
  if(years[i] == 2001){ 
    # -- This dataset contains haul data from the 375 standard survey tows successfully made during the 2001 EBS trawl survey.
    # -- Station Q02 was not sampled.
    # SH note: based on currently used haul table, pared to 374 tows (rm Z-04/AZ0504)
    temp <- temp %>%
      dplyr::filter(HAUL_TYPE == 3, 
                    CRUISE == 200101,
                    VESSEL %in% c(88,89),
                    !(VESSEL == 88 & HAUL %in% c(129:134,168:175)),
                    !(VESSEL == 89 & HAUL %in% c(135:140,179:183))) 
  } 
  
  if(years[i] == 2002){ 
    # -- This dataset contains haul data from the 375 standard survey tows successfully made during the 2002 EBS trawl survey. 
    # -- Station D03 was coded poor performance and was not sampled.
    # -- Additional notes: vessel 88 cruise 200202 haul type 7 was an underbag crab experiment. Haul data is located 
    # -- on RACEBASE.RKC02HAUL; crab data is located on CRAB.CRAB2002_UNDERBAG. Do not use for standard analysis.  
    # SH note: based on currently used haul table, pared to 374 tows (rm Z-04/AZ0504)
    temp <- temp %>%
      dplyr::filter(HAUL_TYPE == 3, 
                    CRUISE == 200201,
                    VESSEL %in% c(88,89)) 
  } 
  
  if(years[i] == 2003){ 
    # -- This dataset contains haul data from the 376 standard EBS survey stations sampled in 2003.
    # -- Additional notes: vessel 88 cruise 200302 haul type 7 was an underbag skate experiment (92 good hauls);
    # -- vessel 89 cruise 200302 haul type 6 was a Bering Sea rockfish exploration. Both are special projects, 
    # -- not to be considered for standard analysis.   
    # SH note: based on currently used haul table, pared to 375 tows (rm Z-04/AZ0504)
    temp <- temp %>%
      dplyr::filter(HAUL_TYPE == 3, 
                    CRUISE == 200301,
                    VESSEL %in% c(88,89)) 
  }
  
  if(years[i] == 2004){ 
    # -- This dataset contains haul data from the 375 standard survey tows successfully made during the 2004 
    # -- EBS trawl survey. Station I25 was reclassified poor performance and not towed.
    # SH note: based on currently used haul table, pared to 374 tows (rm Z-04/AZ0504)
    temp <- temp %>%
      dplyr::filter(HAUL_TYPE == 3, 
                    CRUISE == 200401,
                    VESSEL %in% c(88,89)) 
  }
  
  if(years[i] == 2005){ 
    # -- This dataset contains haul data from the 373 standard EBS survey stations sampled in 2005. 
    # -- Stations F23, G24, and Q25  had to be dropped due to poor performance.
    # SH note: based on currently used haul table, pared to 372 tows (rm Z-04/AZ0504)
    temp <- temp %>%
      dplyr::filter(HAUL_TYPE == 3, 
                    CRUISE == 200501,
                    VESSEL %in% c(88,89),
                    !(VESSEL == 88 & HAUL %in% c(155:170,174,175)),
                    !(VESSEL == 89 & HAUL %in% c(157:163,165:168))) 
  }
  
  if(years[i] == 2006){ 
    # -- This dataset contains haul data from the 376 standard EBS survey stations sampled in 2006. 
    # -- Crab from haul 111 vessel 134 were inadvertently discarded before being measured. Station 
    # -- was re-towed for crab only (vessel 134 haul 112 haul type 20). Haul 111 should not be used 
    # -- for crab data analysis purposes - use haul 112.
    # SH note: based on currently used haul table, pared to 375 tows (rm Z-04/AZ0504) - 405 with BB resampling
    temp <- temp %>%
      dplyr::filter(HAUL_TYPE %in% c(3,17,20), 
                    CRUISE == 200601,
                    VESSEL %in% c(88,134),
                    !(VESSEL == 88 & HAUL %in% c(93:98,167:175)),
                    !(VESSEL == 134 & HAUL %in% c(95:100,111,179:186))) 
  }
  
  if(years[i] == 2007){ 
    # -- This haul dataset comprises the 376 standard survey tows successfully made during the 2007 EBS trawl survey.
    # SH note: based on currently used haul table, pared to 375 tows (rm Z-04/AZ0504) - 407 with BB resampling
    temp <- temp %>%
      dplyr::filter(HAUL_TYPE %in% c(3,17),
                    CRUISE == 200701,
                    VESSEL %in% c(88,89))
  }
  
  if(years[i] == 2008){ 
    # -- This haul dataset comprises the 375 standard survey tows successfully made during the 2008 EBS trawl survey. 
    # -- Station M08 was dropped due to poor performance (determined after the survey was over).
    # SH note: based on currently used haul table, pared to 374 tows (rm Z-04/AZ0504) - 406 with BB resampling
    temp <- temp %>%
      dplyr::filter(HAUL_TYPE %in% c(3,17), 
                    CRUISE == 200801,
                    VESSEL %in% c(88,89))
  }
  
  if(years[i] == 2009){ 
    # -- This haul dataset comprises the 376 standard survey tows successfully made during the 2009 EBS trawl survey.
    # SH note: based on currently used haul table, pared to 375 tows (rm Z-04/AZ0504) - 407 with BB resampling
    temp <- temp %>%
      dplyr::filter(HAUL_TYPE %in% c(3,17),
                    CRUISE == 200901,
                    VESSEL %in% c(88,89)) 
  }
  
  if(years[i] == 2010){ 
    # -- This haul dataset comprises the 376 standard survey tows successfully made during the 2010 EBS trawl survey.
    # SH note: based on currently used haul table, pared to 375 tows (rm Z-04/AZ0504) - 398 with BB resampling
    temp <- temp %>%
      dplyr::filter(HAUL_TYPE %in% c(3,17),
                    CRUISE == 201001,
                    VESSEL %in% c(89,162)) 
  }
  
  if(years[i] == 2011){ 
    # -- This haul dataset comprises the 376 standard survey tows successfully made during the 2011 EBS trawl survey.
    # SH note: based on currently used haul table, pared to 375 tows (rm Z-04/AZ0504) - 395 with BB resampling
    temp <- temp %>%
      dplyr::filter(HAUL_TYPE %in% c(3,17),
                    CRUISE == 201101,
                    VESSEL %in% c(89,162)) 
  }
  
  if(years[i] == 2012){ 
    # -- This haul dataset comprises the 376 standard survey tows successfully made during the 2012 EBS trawl survey.
    # SH note: based on currently used haul table, pared to 375 tows (rm Z-04/AZ0504) - 395 with BB resampling
    temp <- temp %>%
      dplyr::filter(HAUL_TYPE %in% c(3,17),
                    CRUISE == 201201,
                    VESSEL %in% c(89,162)) 
  }
  
  if(years[i] == 2013){ 
    # -- This haul dataset comprises the 376 standard survey tows successfully made during the 2013 EBS trawl survey.
    # SH note: based on currently used haul table, pared to 375 tows (rm Z-04/AZ0504)
    temp <- temp %>%
      dplyr::filter(HAUL_TYPE == 3,
                    CRUISE == 201301,
                    VESSEL %in% c(89,162)) 
  }
  
  if(years[i] == 2014){ 
    # -- This haul dataset comprises the 376 standard survey tows successfully made during the 2014 EBS trawl survey.
    # SH note: based on currently used haul table, pared to 375 tows (rm Z-04/AZ0504)
    temp <- temp %>%
      dplyr::filter(HAUL_TYPE == 3,
                    CRUISE == 201401,
                    VESSEL %in% c(94,162)) 
  }
  
  if(years[i] == 2015){ 
    # -- This haul dataset comprises the 376 standard survey tows successfully made during the 2015 EBS trawl survey.
    # SH note: based on currently used haul table, pared to 375 tows (rm Z-04/AZ0504)
    temp <- temp %>%
      dplyr::filter(HAUL_TYPE == 3,
                    CRUISE == 201501, 
                    VESSEL %in% c(94,162))
  }
  
  if(years[i] == 2016){ 
    # -- This haul dataset comprises the 376 standard survey tows successfully made during the 2016 EBS trawl survey.
    # SH note: based on currently used haul table, pared to 375 tows (rm Z-04/AZ0504)
    temp <- temp %>%
      dplyr::filter(HAUL_TYPE == 3,
                    CRUISE == 201601, 
                    VESSEL %in% c(94,162)) 
  }
  
  if(years[i] == 2017){ 
    # -- This haul dataset comprises the 376 standard survey tows successfully made during the 2017 EBS trawl survey.
    # SH note: based on currently used haul table, pared to 375 tows (rm Z-04/AZ0504) - 395 with BB resampling
    temp <- temp %>%
      dplyr::filter(HAUL_TYPE %in% c(3, 17),
                    CRUISE == 201701,
                    VESSEL %in% c(94,162))
  }
  
  if(years[i] == 2018){
    # -- This haul dataset comprises the 376 standard survey tows successfully made during the 2018 EBS trawl survey.
    # SH note: based on currently used haul table, pared to 375 tows (rm Z-04/AZ0504)
    temp <- temp %>%
      dplyr::filter(HAUL_TYPE == 3,
                    CRUISE == 201801,
                    VESSEL %in% c(94,162))
  }
  
  if(years[i] == 2019){
    # -- This haul dataset comprises the 376 standard survey tows successfully made during the 2019 EBS trawl survey.
    # SH note: based on currently used haul table, pared to 375 tows (rm Z-04/AZ0504)
    temp <- temp %>%
      dplyr::filter(HAUL_TYPE == 3,
                    CRUISE == 201901,
                    VESSEL %in% c(94,162))
  }
  
  if(years[i] == 2020){ 
    # -- No EBS trawl survey this year due to COVID-19 pandemic
    next
  }
  
  if(years[i] == 2021){ 
    # -- This haul dataset comprises the 376 standard survey tows successfully made during the 2021 EBS trawl survey.
    # SH note: based on currently used haul table, pared to 375 tows (rm Z-04/AZ0504) - 395 with BB resampling
    temp <- temp %>%
      dplyr::filter(HAUL_TYPE %in% c(3, 17),
                    CRUISE == 202101,
                    VESSEL %in% c(94,162))
  }
  
  if(years[i] == 2022){
    # -- This haul dataset comprises the 376 standard survey tows successfully made during the 2022 EBS trawl survey.
    # SH note: based on currently used haul table, pared to 375 tows (rm Z-04/AZ0504)
    temp <- temp %>%
      dplyr::filter(HAUL_TYPE == 3,
                    CRUISE == 202201,
                    VESSEL %in% c(94,162))
  }
  
  if(years[i] == 2023){
    # -- This haul dataset comprises the 376 standard survey tows successfully made during the 2023 EBS trawl survey.
    # SH note: based on currently used haul table, pared to 375 tows (rm Z-04/AZ0504)
    temp <- temp %>%
      dplyr::filter(HAUL_TYPE == 3,
                    CRUISE == 202301,
                    VESSEL %in% c(134,162))
  }
  
  if(years[i] == 2024){
    # -- This haul dataset comprises the 376 standard survey tows successfully made during the 2024 EBS trawl survey.
    # SH note: based on currently used haul table, pared to 375 tows (rm Z-04/AZ0504)
    temp <- temp %>%
      dplyr::filter(HAUL_TYPE == 3,
                    CRUISE == 202401,
                    VESSEL %in% c(134,162))
  }
  
  haul_table <- rbind(haul_table, temp)
}


# Add END_LATITUDE and END_LONGITUDE if not recorded, calculate MID_LATITUDE and MID_LONGITUDE
haul_table <- haul_table %>% 
  dplyr::mutate(STATIONID = ifelse(is.na(STATIONID), "", STATIONID),
                END_LATITUDE = ifelse(is.na(END_LATITUDE), START_LATITUDE, END_LATITUDE), # TOLEDO what are these even used for? this is also not likely very accurate, because curves?
                END_LONGITUDE = ifelse(is.na(END_LONGITUDE), START_LONGITUDE, END_LONGITUDE), # TOLEDO what are these even used for? this is also not likely very accurate, because curves?
                MID_LATITUDE = (START_LATITUDE + END_LATITUDE)/2, # TOLEDO what are these even used for? this is also not likely very accurate, because curves?
                MID_LONGITUDE = (START_LONGITUDE + END_LONGITUDE)/2, # TOLEDO what are these even used for? this is also not likely very accurate, because curves?
                AREA_SWEPT = NET_WIDTH/1000 * DISTANCE_FISHED * 0.29155335, # TOLEDO - what is 0.29155335? We don't use this. also area_swept is in our CPUE table
                START_DATE = as.Date(START_TIME))

# Assign GIS_STATION based on EBS_grid
pnts_sf <- sf::st_as_sf(haul_table, 
                        coords = c('END_LONGITUDE', 'END_LATITUDE'),
                        crs = st_crs(grid))

haul_table_pnts <- pnts_sf %>% 
  dplyr::mutate(intersection = as.numeric(sf::st_intersects(geometry, grid)), 
                GIS_STATION = if_else(is.na(intersection), '', grid$STATION_ID[intersection])) %>%
  sf::st_drop_geometry() %>%
  dplyr::left_join(., haul_table) %>%
  # remove tows at Z-04/AZ0504
  dplyr::filter(! STATIONID %in% c("Z-04", "AZ0504")) %>%
  # fix one GIS_STATION that should match the STATIONID IH2221 not I-21
  dplyr::mutate(GIS_STATION = ifelse(SURVEY_YEAR == 1985 & 
                                       VESSEL == 37 & 
                                       HAUL == 207,
                                     STATIONID, GIS_STATION))

# Update 1979 station names from lookup table
haul_table_pnts2 <- haul_table_pnts %>% 
  dplyr::select(-c("STATIONID", "GIS_STATION")) %>%
  dplyr::inner_join(., stations_1979) %>% # TOLEDO ℹ If a many-to-many relationship is expected, set `relationship = "many-to-many"` to silence this warning.

  dplyr::bind_rows(haul_table_pnts %>% filter(SURVEY_YEAR != 1979), .) %>%
  dplyr::arrange(SURVEY_YEAR, CRUISE, VESSEL, START_TIME)

# Select columns in final order
haul_table <- as.data.frame(haul_table_pnts2) %>%
  dplyr::select(CRUISEJOIN, HAULJOIN, REGION, SURVEY_YEAR, VESSEL, CRUISE, HAUL, HAUL_TYPE, PERFORMANCE, START_DATE, DURATION, 
                DISTANCE_FISHED, NET_WIDTH, NET_MEASURED, NET_HEIGHT, AREA_SWEPT, STRATUM, START_LATITUDE, END_LATITUDE, START_LONGITUDE, 
                END_LONGITUDE, MID_LATITUDE, MID_LONGITUDE, STATIONID, GIS_STATION, GEAR_DEPTH, BOTTOM_DEPTH, BOTTOM_TYPE, 
                SURFACE_TEMPERATURE, GEAR_TEMPERATURE, WIRE_LENGTH, GEAR, ACCESSORIES, SUBSAMPLE, START_TIME) 
  
haul_table <- haul_table %>% 
  dplyr::select(
    CRUISEJOIN,
    HAULJOIN,
    HAUL,
    HAUL_TYPE,
    VESSEL_ID = VESSEL, # duplicated in AKFIN_CRUISE
    PERFORMANCE,
    DATE_TIME_START = START_TIME,
    DURATION_HR = DURATION,
    DISTANCE_FISHED_KM = DISTANCE_FISHED,
    NET_WIDTH_M = NET_WIDTH,
    NET_MEASURED, 
                NET_HEIGHT_M = NET_HEIGHT,
                STRATUM,
                LATITUDE_DD_START = START_LATITUDE,
                LATITUDE_DD_END = END_LATITUDE,
                LONGITUDE_DD_START = START_LONGITUDE,
                LONGITUDE_DD_END = END_LONGITUDE,
                STATION = STATIONID,
                DEPTH_GEAR_M = GEAR_DEPTH,
                DEPTH_M = BOTTOM_DEPTH,
                BOTTOM_TYPE,
                SURFACE_TEMPERATURE_C = SURFACE_TEMPERATURE,
                GEAR_TEMPERATURE_C = GEAR_TEMPERATURE,
                WIRE_LENGTH_M = WIRE_LENGTH,
                GEAR,
                ACCESSORIES) %>% 
  dplyr::mutate(NET_MEASURED = dplyr::case_when(
    NET_MEASURED == "Y" ~ 1,
    NET_MEASURED == "N" ~ 0,
    TRUE ~ NA))

print("processed haul table")


# write output file to be used for subsequent data processing and Tech Memo
# write.csv(haul_table, "Y:/KOD_Survey/EBS Shelf/Data_Processing/Outputs/haul_table.csv", row.names = FALSE)
write.csv(x = haul_table, file = here::here("temp", "haul_table.csv"), row.names = FALSE)
# write.csv(haul_table, paste0("Y:/KOD_Survey/EBS Shelf/", current_year, "/Tech Memo/Data/Haul Data/haul_table.csv"), row.names = FALSE)

print("updated haul table written to Data_Processing and Tech Memo folders")

# Simply, we should just add these observations to GAP_PRODUCTS.AKFIN_HAUL

gapindex::upload_oracle(x = haul_table, 
                        table_name = "AKFIN_HAUL", 
                        channel = channel_ehm, 
                        schema = "markowitze", 
                        metadata_column = metadata_column %>% dplyr::filter(colname %in% names(haul_table)), 
                        table_metadata = paste0("Modifed of GAP_PRODUCTS.HAUL OR AKFIN_HAUL. ", legal_disclaimer),
                        share_with_all_users = TRUE)

# remove extraneous items from environment
rm(haul_table_OLD, grid, haul, haul_table_pnts, pnts_sf, temp, years, i)
gc()

