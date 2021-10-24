# Minimum estimates of children affected by COVID-19-associated orphanhood and deaths of caregivers

This repository includes code and data to recreate the following analyses:
1) [Hillis, Unwin, Chen et al. (2021)](http://www.thelancet.com/journals/lancet/article/PIIS0140-6736(21)01253-8/fulltext),
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

## Pediatrics paper

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

## Warranty
Imperial makes no representation or warranty about the accuracy or completeness of the data nor that the results will not constitute in infringement of third-party rights. Imperial accepts no liability or responsibility for any use which may be made of any results, for the results, nor for any reliance which may be placed on any such work or results.
