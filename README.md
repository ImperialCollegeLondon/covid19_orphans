# Minimum estimates of children affected by COVID-19-associated orphanhood and deaths of caregivers

This repository includes code and data to recreate the following analyses:
1) [Hillis, Unwin, Chen et al. (2021)](http://www.thelancet.com/journals/lancet/article/PIIS0140-6736(21)01253-8/fulltext) [![DOI](https://zenodo.org/badge/360452208.svg)](https://zenodo.org/badge/latestdoi/360452208),
2) [Hillis, Blenkinsop et al. (2021)]()

## Global minimum estimates of children affected by COVID-19-associated orphanhood and deaths of caregivers: a modelling study

TheLancet_global_minimum_estimates_2021_run.R is used to recreate this analysis.  It runs two scrips:
1) TheLancet_global_minimum_estimates_2021/extract_study_data.R
2) TheLancet_global_minimum_estimates_2021/run_global_extrapolation.R.

These methods use COVID-19 or excess deaths and fertility rates for our 21 study countries to provide minimum estimates of orphanhood and loss of primary and secondary caregivers in these countries.  A scaled logistic model is then used to extrapolate ratios of loss of care to deaths based on total fertility rates of all the countries in the world.

Please note all data necessary to run the analyses from scratch are not provided for the following reasons:
1) A free account is required to access the DHS datasets.  If you obtain these datasets, code is provided in [TheLancet_global_minimum_estimates_2021/R/DHS/male_fertillity_function.R](TheLancet_global_minimum_estimates_2021/R/DHS/male_fertility_function.R) and [TheLancet_global_minimum_estimates_2021/R/DHS/female_fertility_function.R](TheLancet_global_minimum_estimates_2021/R/DHS/female_fertility_function.R) to recreate our implementation of the own child method to calculate male and female fertility rates.  We provide our calculations of fertility rates for Brazil, Colombia, India, Kenya, Malawi, Nigeria, Peru, South Africa and Zimbabwe since these are needed for the subsequent analyses.
2) Raw data from Brazil is too large to be added to the repository.  Internet access is required to run the *process_brazil* function in [TheLancet_global_minimum_estimates_2021/extract_study_data.R](TheLancet_global_minimum_estimates_2021/extract_study_data.R) since it downloads this data.  We have commented out this line of code to reduce the processing time and so it can be run offline.
3) Raw data from the Philippines is also too large to be added to the repository.  This can be downloaded from the [Philippines Department of Health](https://doh.gov.ph/covid19tracker) data drop. As this gets updated, additional cases may be added to our time frames of interest so numbers may change slightly.  We have therefore commented out the *process_philippines* function in [TheLancet_global_minimum_estimates_2021/extract_study_data.R](TheLancet_global_minimum_estimates_2021/extract_study_data.R) where the COVID-19 data is extracted and added our saved file where the age/sex disaggregated deaths for COVID-19 in the Philippines are pre calculated from the raw data.

## COVID-19-Associated Orphanhood and Caregiver Death in the United States

This repository includes the code and underlying data, where possible, to recreate the analyses in [Hillis, Blenkinsop, Villaveces et al. (2021)]( https://doi.org/10.1542/peds.2021-053760).  

Two files need to be run to recreate the analysis:
1) Pediatrics_US_estimates_disparities_2021.R
2) Pediatrics_US_estimates_disparities_2021/submit-jobs.R.

These methods use COVID-19 or excess deaths and fertility rates for US states, stratified by race and ethnicity.

Pediatrics_US_estimates_disparities_2021.R runs the primary analysis and produces the manuscript tables and figures. It can be run locally in R, or run through the command line from the local repository directory as follows:
Rscript Pediatrics_US_estimates_disparities_2021.R -source_dir '~/git/covid19_orphans/Pediatrics_US_estimates_disparities_2021' -rep 0 

Pediatrics_US_estimates_disparities_2021/submit-jobs.R generates a bash script to run the bootstrapped analysis and generates uncertainty intervals for the estimates, and is intended to run in a UNIX environment.

Raw data is provided in the repository, with the exception of mortality data, which is too large to be added to the repository (75mb). To run the analysis, the data can be downloaded from :
https://data.cdc.gov/NCHS/AH-Quarterly-Excess-Deaths-by-State-Sex-Age-and-Ra/jqg8-ycmh
Save the data with the file path name:
data/USA/AH_Quarterly_Excess_Deaths_by_State__Sex__Age__and_Race_Q2_2021.csv

## R packages
The following R packages are necessary:
- readxl
- data.table
- tidyverse
- rjson
- reshape2
- lubridate
- repr
- ggpubr

and rdhs for the DHS data.

To run the analysis for the United States state results, the following packages are also needed:
- scales
- usmap
- RColorBrewer
- ggsci
- geofacet

## Warranty
Imperial makes no representation or warranty about the accuracy or completeness of the data nor that the results will not constitute in infringement of third-party rights. Imperial accepts no liability or responsibility for any use which may be made of any results, for the results, nor for any reliance which may be placed on any such work or results.


