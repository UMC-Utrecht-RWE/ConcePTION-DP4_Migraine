![Status](https://img.shields.io/badge/Study_status-ongoing-red) 

![Logo](https://www.imi-conception.eu/wp-content/uploads/2019/07/ConcePTION-logo-plain-new-e1564480200525.png)

# :wave: Work Package 1 Demostration project 4 :pregnant_woman: :breast_feeding:
This demonstration project will show how we can include intensity and timing of use of medications during pregnancy. This project will use novel techniques including longitudinal trajectories and drug burden index which may offer great advantages as they can model real-life medication usage patterns across pregnancy. We will incorporate novel data visualisation tools with analytical techniques to study complex patterns of medication utilisation before and throughout pregnancy. The example for this study is migraine and included both immediate maternal and child outcomes and child long-term neurodevelopmental outcome. 

# Statistical Analysis Plan (SAP)
## :point_right: SAP Migraine 
Algorithms for characterizing migraine in healthcare databases for use in perinatal studies: A Europen multi-database study
## :point_right: SAP Preeclampsia & Gestational Diabetes
Algorithms to identify gestational diabetes & preeclampsia in ConcePTION population-based data sources
## :point_right: SAP Safety 
Demonstrating solutions for studying intermittent medication exposures in diseases with episodic manifestations during pregnancy: application to medication for migraine in pregnancy
## :point_right: SAP Drug Utilization 
Demostrating solutions for studying intermittent medication exposures in diseases with episodic manifestations during pregnancy: application to medication for migraine in pregnancy.

# Parameters
| Folder                               | Parameter                                                          |
| -------------------------------------| ------------------------------------------------------------------ |
| p_steps                              | study_dates.xlsx |
| p_steps/Pregnancy algorithm/g_output | D3.RData outputs from the Pregnancy algorithm | 
| p_steps/codelists                    | migraine_codelist.csv |
|                                      | migraine_medication_codelist.csv|
|                                      | gestational_diabetes_codelist.csv |
|                                      | antidiabetic_medications_codelist.csv | 
|                                      | pre_eclampsia_codelist.csv |
| p_parameters                         | additional_concepts.xlsx |

# How to run
1. Download the repository in your local setting.
2. Check whether the information in the additional_concepts.xlsx is correct. Check for types, values, column names, etc. This is important to avoid the script stopping. 
3. Copy the outputs from the Pregnancy algorithm in the p_steps/Pregnancy algorithm/g_output/ folder.
4. Run the to_run script. 
5. Avoid running multiple scripts at the same time in your server. This can make this script take 10x times longer to produce results.
6. Once the script has ran, please cross-check the six reports generated in the g_output folder:
   * YYYYMMDD_DAPNAME_Report_0
   * YYYYMMDD_DAPNAME_Report_1
   * YYYYMMDD_DAPNAME_Report_2
   * YYYYMMDD_DAPNAME_Report_3
   * YYYYMMDD_DAPNAME_Report_4
   * YYYYMMDD_DAPNAME_Report_4

## License 
[![GPLv3 License](https://img.shields.io/badge/License-GPL%20v3-yellow.svg)](https://opensource.org/licenses/)
