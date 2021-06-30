# Under the Radar: Global Minimum Estimates for COVID-19-associated Orphanhood and Deaths among Caregivers 

This repository includes the code and underlying data, where possible, to recreate the analyses in [Hillis et al. (2021)](https://privpapers.ssrn.com/sol3/papers.cfm?abstract_id=3782441).  

Two files need to be run to recreate the analysis:
1) extract_study_data.R
2) run_global_extrapolation.R.

These methods use COVID-19 or excess deaths and fertility rates for our 21 study countries to provide minimum estimates of orphanhood and loss of primary and secondary caregivers in these countries.  A scaled logistic model is then used to extrapolate ratios of loss of care to deaths based on total fertility rates of all the countries in the world.

Please note all data necessary to run the analyses from scratch are not provided for the following reasons:
1) A free account is required to access the DHS datasets.  If you obtain these datasets, code is provided in [R/DHS/male_fertillity_function.R](R/DHS/male_fertility_function.R) and [R/DHS/female_fertility_function.R](R/DHS/female_fertility_function.R) to recreate our implementation of the own child method to calculate male and female fertility rates.  We provide our calculations of fertility rates for Brazil, Colombia, India, Kenya, Malawi, Nigeria, Peru, South Africa and Zimbabwe since these are needed for the subsequent analyses.
2) Raw data from Brazil is too large to be added to the repository.  Internet access is required to run the *process_brazil* function in [extract_study_data.R](extract_study_data.R) since it downloads this data.  We have commented out this line of code to reduce the processing time and so it can be run offline.
3) Raw data from the Philippines is also too large to be added to the repository.  This can be downloaded from the [Philippines Department of Health](https://doh.gov.ph/covid19tracker) data drop. As this gets updated, additional cases may be added to our time frames of interest so numbers may change slightly.  We have therefore commented out the *process_philippines* function in [extract_study_data.R](extract_study_data.R) where the COVID-19 data is extracted and added our saved file where the age/sex disaggregated deaths for COVID-19 in the Phillipiines are pre calculated from the raw data.

The following R packages are necessary:
- readxl
- data.table
- tidyverse
- rjson
- reshape2
- lubridate
- repr
- ggpubr
- xlsx

and rdhs for the DHS data.

