![Status](https://img.shields.io/badge/Study_status-ongoing-red) 

![Logo](https://www.imi-conception.eu/wp-content/uploads/2019/07/ConcePTION-logo-plain-new-e1564480200525.png)

# :wave: Work Package 1 Demostration project 4 :pregnant_woman: :breast_feeding:
This demonstration project will show how we can include intensity and timing of use of medications during pregnancy. This project will use novel techniques including longitudinal trajectories and drug burden index which may offer great advantages as they can model real-life medication usage patterns across pregnancy. We will incorporate novel data visualisation tools with analytical techniques to study complex patterns of medication utilisation before and throughout pregnancy. The example for this study is migraine and included both immediate maternal and child outcomes and child long-term neurodevelopmental outcome. 

# Statistical Analysis Plan (SAP)

## :point_right: SAP Preeclampsia & Gestational Diabetes
Algorithms to identify gestational diabetes & preeclampsia in ConcePTION population-based data sources
## :point_right: SAP Migraine 
Algorithms for characterizing migraine in healthcare databases for use in perinatal studies: A Europen multi-database study
## :point_right: SAP Migraine Drug Utilization
to describe drug utilization patterns in women with migraine over the course of pregnancy, focusing especially on intermittent migraine medication use, using triptans as the motivating example. Medication utilisation before, during and after pregnancy will be reviewed and compared across data sources. 

# Parameters
| Folder                               | Parameter                                                          |
| -------------------------------------| ------------------------------------------------------------------ |
| p_steps                              | study_dates.xlsx |
| p_steps/codelists                    | migraine_codelist.csv |
|                                      | migraine_medication_codelist.csv|
|                                      | gestational_diabetes_codelist.csv |
|                                      | antidiabetic_medications_codelist.csv | 
|                                      | pre_eclampsia_codelist.csv |
| p_parameters                         | additional_concepts.xlsx |

# How to run
1. Download the repository in your local setting.
2. Check whether the information in the additional_concepts.xlsx is correct. Check for types, values, column names, etc. This is important to avoid the script stopping. 
3. Open the porject in RStudio.
4. Open `to_run.R` script and set `CDM_dir` and `PregnancyAlgorithm_g_output_dir` in lines 7 and 10. `CDM_dir` is the path of ConcePTION CDM instance and `PregnancyAlgorithm_g_output_dir` is the path of the Conception PregnancyAlgorithm's output (the path should end with /g_output/). 
5. Run the `to_run.R` script. 
7. Once the script has ran, please cross-check the outputs generated in g_output folder.
8. Zip the g_output folder (or g_output_masked folder for masked results) and upload it to myDRE.
## License 
[![GPLv3 License](https://img.shields.io/badge/License-GPL%20v3-yellow.svg)](https://opensource.org/licenses/)

## Repository citation
[![DOI](https://zenodo.org/badge/594797897.svg)](https://doi.org/10.5281/zenodo.14412549)

<!-- ABOUT-THE-PROJECT -->
## About the project
[ConcePTION](https://www.imi-conception.eu) aims to build an ecosystem that can use Real World Data (RWD) to generate Real World Evidence (RWE) that may be used for clinical and regulatory decision making, closing the big information gap of medication safety in pregnancy. As part of WP7, level checks were design to assess the quality of the data supporting RWE. Level checks described here has been successfully implemented in VAC4EU, EMA-tendered risk minimization studies, COVID vaccines effectiveness study, Post-Authorization Safety Studies, and CONSIGN.

<!-- FUNDER -->
## Funder
The ConcePTION project has received funding from the Innovative Medicines Initiative 2 Joint Undertaking under grant agreement No 821520. This Joint Undertaking receives support from the European Union’s Horizon 2020 research and innovation programme and EFPIA

<!-- CONTACT -->
## Contact
Vjola Hoxhaj - v.hoxhaj@umcutrecht.nl

