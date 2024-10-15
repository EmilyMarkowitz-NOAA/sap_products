<!-- README.md is generated from README.Rmd. Please edit that file -->

# [Basic Design-Based SAP Data Products](https://github.com/EmilyMarkowitz-NOAA/sap_products) <img src="https://avatars.githubusercontent.com/u/94422627?s=200&v=4" alt="Logo." align="right" width="139" height="139"/>

The scripts therein reproducibly produce our typical data products.

> This code is always in development. Find code used for various reports
> in the code
> [releases](https://github.com/EmilyMarkowitz-NOAA/sap_products/releases).

## This code is primarally maintained by:

**Emily Markowitz** (Emily.Markowitz AT noaa.gov;
[@EmilyMarkowitz-NOAA](https://github.com/EmilyMarkowitz-NOAA))

Groundfish Assessment Program

NOAA \| Alaska Fisheries Science Center

Seattle, WA 98195

**Shannon Hennessey** (Shannon.Hennessey AT noaa.gov;
[@ShannonHennessey](https://github.com/ShannonHennessey))

Kodiak Fisheries Research Center

NOAA \| Alaska Fisheries Science Center

Kodiak, AK 99615

> The code in this repository is regularly being updated and improved.
> Please refer to
> [releases](https://github.com/afsc-gap-products/gap_products/releases)
> for finalized products and project milestones.

# Table of contents

``` r
toc <- strsplit(x = readtext::readtext(file = here::here("content/README.Rmd"), verbosity = 0)[[2]], split = "\n")
toc <- toc[[1]][substr(x = toc[[1]], start = 1, stop = 1) == "#"]
toc <- toc[-c(1:3)]
toc_list <- toc
toc_list <- gsub(pattern = "### ", replacement = ">      - [*", x = toc_list, fixed = TRUE)
toc_list <- gsub(pattern = "## ", replacement = ">    - [*", x = toc_list, fixed = TRUE)
toc_list <- gsub(pattern = "# ", replacement = ">  - [*", x = toc_list, fixed = TRUE)
toc_link <- tolower(gsub(pattern = " ", replacement = "-", 
                          x = gsub(pattern = "#", replacement = "", 
                                   x = gsub(pattern = "# ", replacement = "", 
                                            x = toc, fixed = TRUE), fixed = TRUE)))
toc <- paste0(toc_list, "*](#", toc_link, ")", collapse = "\n")
```

> - [*User Resources*](#user-resources)
> - [*Cite this data*](#cite-this-data)
>   - [*Access Constraints*](#access-constraints)
> - [*Relevant publications*](#relevant-publications)
> - [*Suggestions and Comments*](#suggestions-and-comments)
>   - [*R Version Metadata*](#r-version-metadata)
>   - [*NOAA README*](#noaa-readme)
>   - [*NOAA License*](#noaa-license)

## User Resources

- [Fisheries One Stop Shop (FOSS)](https://www.fisheries.noaa.gov/foss)

- [AFSC’s Resource Assessment and Conservation Engineering
  Division](https://www.fisheries.noaa.gov/about/resource-assessment-and-conservation-engineering-division)

- [Publications and Data Reports](https://repository.library.noaa.gov/)

- [Research Surveys conducted at
  AFSC](https://www.fisheries.noaa.gov/alaska/ecosystems/alaska-fish-research-surveys)

# Cite this data

Use the below [bibtext
citations](https://github.com/afsc-gap-products/gap_products/blob/main/CITATION.bib),
as cited in our group’s [citation
repository](https://github.com/afsc-gap-products/citations/blob/main/cite/bibliography.bib)
for citing the data created and maintained in this repo. Add “note =
{Accessed: mm/dd/yyyy}” to append the day this data was accessed.
Included here are AFSC RACE Groundfish and Shellfish Assessment
Program’s:

- Design-Based Production Data (NOAA Fisheries Alaska Fisheries Science
  Center, Goundfish Assessment Program, 2024).

- Public Data hosted on the Fisheries One Stop Shop (FOSS) Data Platform
  (NOAA Fisheries Alaska Fisheries Science Center, 2024).

## Access Constraints

There are no legal restrictions on access to the data. They reside in
public domain and can be freely distributed.

**User Constraints:** Users must read and fully comprehend the metadata
prior to use. Data should not be used beyond the limits of the source
scale. Acknowledgement of AFSC Groundfish Assessment Program, as the
source from which these data were obtained, in any publications and/or
other representations of these data, is suggested.

**General questions and more specific data requests** can be sent to
<nmfs.afsc.gap.metadata@noaa.gov> or submitted as an [issue on our
GitHub
Organization](https://github.com/afsc-gap-products/data-requests). The
version of this data used for stock assessments can be found through the
Alaska Fisheries Information Network (AKFIN). For questions about the
eastern Bering Sea surveys, contact Duane Stevenson
(<Duane.Stevenson@noaa.gov>). For questions about the Gulf of Alaska or
Aleutian Islands surveys, contact Ned Laman (<Ned.Laman@noaa.gov>). For
questions specifically about crab data in any region, contact Mike
Litzow (<Mike.Litzow@noaa.gov>), the Shellfish Assessment Program lead.

For questions, comments, and concerns specifically about the [Fisheries
One Stop Shop (FOSS)](https://www.fisheries.noaa.gov/foss) platform,
please contact us using the Comments page on the
[FOSS](https://www.fisheries.noaa.gov/foss) webpage.

# Relevant publications

``` r
source("https://raw.githubusercontent.com/afsc-gap-products/citations/main/cite/current_data_tm.r") # srvy_cite

srvy_cite <- srvy_cite %>% 
  dplyr::filter(SRVY %in% c("EBS", "NBS", "BSS", "CRAB") )
```

**Learn more about these surveys** (**2023NEBS?**; **2023NEBS?**;
**RN979?**; **SAPcrab2024?**).

<div id="refs">

</div>

# Suggestions and Comments

If you see that the data, product, or metadata can be improved, you are
invited to create a [pull
request](https://github.com/EmilyMarkowitz-NOAA/sap_products/pulls),
[submit an issue to the GitHub
organization](https://github.com/afsc-gap-products/data-requests/issues),
or [submit an issue to the code’s
repository](https://github.com/EmilyMarkowitz-NOAA/sap_products/issues).

## R Version Metadata

``` r
sessionInfo()
```

    ## R version 4.4.1 (2024-06-14 ucrt)
    ## Platform: x86_64-w64-mingw32/x64
    ## Running under: Windows 10 x64 (build 19045)
    ## 
    ## Matrix products: default
    ## 
    ## 
    ## locale:
    ## [1] LC_COLLATE=English_United States.utf8 
    ## [2] LC_CTYPE=English_United States.utf8   
    ## [3] LC_MONETARY=English_United States.utf8
    ## [4] LC_NUMERIC=C                          
    ## [5] LC_TIME=English_United States.utf8    
    ## 
    ## time zone: America/Los_Angeles
    ## tzcode source: internal
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] akfingapdata_0.1.0 DBI_1.2.3          RODBC_1.3-23       ftExtra_0.6.4     
    ##  [5] badger_0.2.4       scales_1.3.0       stringr_1.5.1      here_1.0.1        
    ##  [9] flextable_0.9.6    kableExtra_1.4.0   janitor_2.2.0      readxl_1.4.3      
    ## [13] tidyr_1.3.1        readr_2.1.5        magrittr_2.0.3     googledrive_2.1.1 
    ## [17] akgfmaps_3.5.3     terra_1.7-78       stars_0.6-6        abind_1.4-8       
    ## [21] sf_1.0-17          gstat_2.1-2        ggplot2_3.5.1      dplyr_1.1.4       
    ## [25] classInt_0.4-10    gapindex_2.2.0     distill_1.6        devtools_2.4.5    
    ## [29] usethis_3.0.0     
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] remotes_2.5.0           rlang_1.1.4             snakecase_0.11.1       
    ##  [4] e1071_1.7-14            compiler_4.4.1          systemfonts_1.1.0      
    ##  [7] vctrs_0.6.5             profvis_0.4.0           pkgconfig_2.0.3        
    ## [10] fastmap_1.2.0           ellipsis_0.3.2          utf8_1.2.4             
    ## [13] promises_1.3.0          rmarkdown_2.28          sessioninfo_1.2.2      
    ## [16] tzdb_0.4.0              ragg_1.3.3              purrr_1.0.2            
    ## [19] rvcheck_0.2.1           xfun_0.47               cachem_1.1.0           
    ## [22] jsonlite_1.8.9          later_1.3.2             uuid_1.2-1             
    ## [25] parallel_4.4.1          R6_2.5.1                RColorBrewer_1.1-3     
    ## [28] stringi_1.8.4           pkgload_1.4.0           lubridate_1.9.3        
    ## [31] cellranger_1.1.0        Rcpp_1.0.13             knitr_1.48             
    ## [34] zoo_1.8-12              readtext_0.91           dlstats_0.1.7          
    ## [37] FNN_1.1.4.1             httpuv_1.6.15           timechange_0.3.0       
    ## [40] tidyselect_1.2.1        yaml_2.3.10             rstudioapi_0.16.0      
    ## [43] codetools_0.2-20        miniUI_0.1.1.1          pkgbuild_1.4.4         
    ## [46] lattice_0.22-6          tibble_3.2.1            intervals_0.15.5       
    ## [49] shiny_1.9.1             withr_3.0.1             askpass_1.2.1          
    ## [52] evaluate_1.0.1          units_0.8-5             proxy_0.4-27           
    ## [55] urlchecker_1.0.1        zip_2.3.1               xts_0.14.0             
    ## [58] xml2_1.3.6              BiocManager_1.30.25     pillar_1.9.0           
    ## [61] KernSmooth_2.23-24      generics_0.1.3          rprojroot_2.0.4        
    ## [64] sp_2.1-4                spacetime_1.3-2         hms_1.1.3              
    ## [67] munsell_0.5.1           xtable_1.8-4            class_7.3-22           
    ## [70] glue_1.7.0              gdtools_0.4.0           tools_4.4.1            
    ## [73] data.table_1.16.0       fs_1.6.4                grid_4.4.1             
    ## [76] colorspace_2.1-1        cli_3.6.3               textshaping_0.4.0      
    ## [79] officer_0.6.7           fontBitstreamVera_0.1.1 fansi_1.0.6            
    ## [82] gargle_1.5.2            viridisLite_0.4.2       svglite_2.1.3          
    ## [85] downlit_0.4.4           gtable_0.3.5            yulab.utils_0.1.7      
    ## [88] digest_0.6.37           fontquiver_0.2.1        htmlwidgets_1.6.4      
    ## [91] memoise_2.0.1           htmltools_0.5.8.1       lifecycle_1.0.4        
    ## [94] httr_1.4.7              mime_0.12               fontLiberation_0.1.0   
    ## [97] openssl_2.2.1

## NOAA README

This repository is a scientific product and is not official
communication of the National Oceanic and Atmospheric Administration, or
the United States Department of Commerce. All NOAA GitHub project code
is provided on an ‘as is’ basis and the user assumes responsibility for
its use. Any claims against the Department of Commerce or Department of
Commerce bureaus stemming from the use of this GitHub project will be
governed by all applicable Federal law. Any reference to specific
commercial products, processes, or services by service mark, trademark,
manufacturer, or otherwise, does not constitute or imply their
endorsement, recommendation or favoring by the Department of Commerce.
The Department of Commerce seal and logo, or the seal and logo of a DOC
bureau, shall not be used in any manner to imply endorsement of any
commercial product or activity by DOC or the United States Government.

## NOAA License

Software code created by U.S. Government employees is not subject to
copyright in the United States (17 U.S.C. §105). The United
States/Department of Commerce reserve all rights to seek and obtain
copyright protection in countries other than the United States for
Software authored in its entirety by the Department of Commerce. To this
end, the Department of Commerce hereby grants to Recipient a
royalty-free, nonexclusive license to use, copy, and create derivative
works of the Software outside of the United States.

<img src="https://raw.githubusercontent.com/nmfs-general-modeling-tools/nmfspalette/main/man/figures/noaa-fisheries-rgb-2line-horizontal-small.png" alt="NOAA Fisheries" height="75"/>

[U.S. Department of Commerce](https://www.commerce.gov/) \| [National
Oceanographic and Atmospheric Administration](https://www.noaa.gov) \|
[NOAA Fisheries](https://www.fisheries.noaa.gov/)
