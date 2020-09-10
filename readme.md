# rw_sim
Estimating uncertainty in right whale location following visual or acoustic detection. 

## Description

This project accompanies the following manuscript:

Johnson HD, Baumgartner MF, Taggart CT. 2020. Estimating North Atlantic right whale (Eubalaena glacialis) location uncertainty following visual or acoustic detection to inform dynamic management. Conservation Science and Practice. 

The article is available [here](https://conbio.onlinelibrary.wiley.com/doi/full/10.1111/csp2.267).

## Project structure

`master.R` - master script (with simulation parameters) that executes entire analysis  

`src/` - all source code for analysis, including functions (`functions.R`) and scripts to produce various figures (`f_*.R`)  

`runs/` - data from each model run   

`cache/` - cached data used across runs or figures  

`figures/` - figures used in the manuscript

## Reproducing the analysis

The entire simulation and figures should be reproducible by running the `master.R` script. On doing so you may be prompted to download and install the libraries required in `src/functions.R`. Note that the default number of simulated right whales is 1e5 (`nrws = 1e5`), and this will likely take hours to run depending on your computer specs. I suggest using something smaller, like 1e2, to start.

## System specifications

Below is the output from `sessionInfo()` on the machine I used for the analysis:

```
R version 3.6.2 (2019-12-12)
Platform: x86_64-apple-darwin15.6.0 (64-bit)
Running under: macOS Mojave 10.14.6

Matrix products: default
BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
LAPACK: /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRlapack.dylib

locale:
[1] en_CA.UTF-8/en_CA.UTF-8/en_CA.UTF-8/C/en_CA.UTF-8/en_CA.UTF-8

attached base packages:
[1] parallel  stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] raster_3.0-7    sp_1.3-2        gridExtra_2.3   forcats_0.4.0   stringr_1.4.0  
 [6] dplyr_0.8.3     purrr_0.3.3     readr_1.3.1     tidyr_1.0.0     tibble_2.1.3   
[11] ggplot2_3.2.1   tidyverse_1.3.0 oce_1.1-1       gsw_1.0-5       testthat_2.3.1 

loaded via a namespace (and not attached):
 [1] tidyselect_0.2.5 haven_2.2.0      lattice_0.20-38  colorspace_1.4-1 vctrs_0.2.1     
 [6] generics_0.0.2   rlang_0.4.2      pillar_1.4.3     glue_1.3.1       withr_2.1.2     
[11] DBI_1.1.0        dbplyr_1.4.2     modelr_0.1.5     readxl_1.3.1     lifecycle_0.1.0 
[16] munsell_0.5.0    gtable_0.3.0     cellranger_1.1.0 rvest_0.3.5      codetools_0.2-16
[21] fansi_0.4.0      broom_0.5.3      Rcpp_1.0.3       scales_1.1.0     backports_1.1.5 
[26] jsonlite_1.6     fs_1.3.1         hms_0.5.2        stringi_1.4.3    grid_3.6.2      
[31] cli_2.0.0        tools_3.6.2      magrittr_1.5     lazyeval_0.2.2   crayon_1.3.4    
[36] pkgconfig_2.0.3  zeallot_0.1.0    xml2_1.2.2       reprex_0.3.0     lubridate_1.7.4 
[41] assertthat_0.2.1 httr_1.4.1       rstudioapi_0.10  R6_2.4.1         nlme_3.1-142    
[46] compiler_3.6.2  
```

