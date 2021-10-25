

# COVID-19-Associated Orphanhood and Caregiver Death in the United States

This repository includes the code and underlying data, where possible, to recreate the analyses in [Hillis, Blenkinsop, Villaveces et al. (2021)]( https://doi.org/10.1542/peds.2021-053760).  

Two files need to be run to recreate the analysis:
1) run_US.R
2) submit-jobs.R.

These methods use COVID-19 or excess deaths and fertility rates for US states, stratified by race and ethnicity. run_US.R runs the primary analysis and produces the manuscript tables and figures. submit-jobs.R generates a bash script to run the bootstrapped analysis and generates uncertainty intervals for the estimates, and is intended to run in a UNIX environment.

Raw data is provided in the repository.

The following R packages are necessary for running analysis and producing figures:
- readxl
- data.table
- tidyverse
- rjson
- reshape2
- lubridate
- repr
- ggpubr
- scales
- usmap
- RColorBrewer
- ggsci
- geofacet


## Warranty
Imperial makes no representation or warranty about the accuracy or completeness of the data nor that the results will not constitute in infringement of third-party rights. Imperial accepts no liability or responsibility for any use which may be made of any results, for the results, nor for any reliance which may be placed on any such work or results.
