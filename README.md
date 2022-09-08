# Minimum estimates of children affected by COVID-19-associated orphanhood and deaths of caregivers

This repository includes code and data to recreate the following analyses:
1) [Hillis, Unwin, Chen et al. (2021)](http://www.thelancet.com/journals/lancet/article/PIIS0140-6736(21)01253-8/fulltext) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5595419.svg)](https://doi.org/10.5281/zenodo.5595419),
2) [Hillis, Blenkinsop, Villaveces et al. (2021)](https://publications.aap.org/pediatrics/article/148/6/e2021053760/183446/COVID-19-Associated-Orphanhood-and-Caregiver-Death)[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5901319.svg)](https://doi.org/10.5281/zenodo.5901319),
3) [Unwin, Hillis et al. (2022)](https://www.thelancet.com/journals/lanchi/article/PIIS2352-4642(22)00005-0/fulltext)[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.6284634.svg)](https://doi.org/10.5281/zenodo.6284634)
4) [Hillis et al. (2022)](https://jamanetwork.com/journals/jamapediatrics/fullarticle/2795650)

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

## Global, regional, and national minimum estimates of children affected by COVID-19-associated orphanhood and caregiver death, by age and family circumstance up to Oct 31, 2021: an updated modelling study
TheLancetCAH_global_age_analysis_2022.R script is used to recreate the analysis in this paper. It uses the same fertility data as the Hillis, Unwin, Chen et al. as described above.  Updates of the mortality data for Brazil and the Philippines can also be found from the same links as above.

Please be warned the script "TheLancetCAH_global_age_analysis_2022/R/run_age_analysis_uncertainty.R" takes quite a while to run since it implements the bootstrapping for the uncertainty in the age calculation.  All necessary output is included in the repository so this step can be skipped if necessary. 

## Orphanhood and Caregiver Loss Among Children Based on New Global Excess COVID-19 Death Estimates
The JAMAPeds_excess_deaths_update_2022.R script is used to recreate the analysis in this paper.  It updates our analysis using excess deaths.

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

To run the analysis for the global age predictions, the following packages are also needed:
- brms
- xtable
- ggrepel
- matrixStats
- gridExtra
- scales
- ggsci
- cowplot
- RCurl
- stringr

## Warranty
Imperial makes no representation or warranty about the accuracy or completeness of the data nor that the results will not constitute in infringement of third-party rights. Imperial accepts no liability or responsibility for any use which may be made of any results, for the results, nor for any reliance which may be placed on any such work or results.


