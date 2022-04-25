## CHA Dash ##
library(reactable)
library(shiny)
library(shinydashboard)
library(fontawesome)
library(tidyr)
library(dplyr)
library(plotly)
library(scales)
library(formattable)
library(data.table)


load("dashdata.Rdata")
#Dem
dem_table <- bind_cols(`2020 Population Parameter` = c('Total Population','Percent Hispanic Ethnicity','Percent Non-White Race','Birth Rate (per 1,000)','Crude Death Rate (per 100,000)', 'Age-Adusted Death Rate (per 100,000)'),
                       `Bear River` = c(as.character(last(dem_wide$br_tot_pop)),
                                        paste(last(dem_wide$br_ethn_hisp_perc),'%', sep = ''),
                                        paste(last(dem_wide$br_nonwhite_perc),'%', sep = ''),
                                        as.character(last(dem_wide$br_birth_rate)),
                                        as.character(last(dem_wide$br_death_rate_cr)),
                                        as.character(last(dem_wide$br_death_rate_aar))),
                       `Box Elder` = c(as.character(last(dem_wide$be_tot_pop)),
                                       paste(last(dem_wide$be_ethn_hisp_perc),'%', sep = ''),
                                       paste(last(dem_wide$be_nonwhite_perc),'%', sep = ''),
                                       as.numeric(last(dem_wide$be_birth_rate)),
                                       as.character(last(dem_wide$be_death_rate_cr)),
                                       as.character(last(dem_wide$be_death_rate_aar))),
                       `Cache` = c(as.character(last(dem_wide$ca_tot_pop)),
                                   paste(last(dem_wide$ca_ethn_hisp_perc),'%', sep = ''),
                                   paste(last(dem_wide$ca_nonwhite_perc),'%', sep = ''),
                                   as.character(last(dem_wide$ca_birth_rate)),
                                   as.character(last(dem_wide$ca_death_rate_cr)),
                                   as.character(last(dem_wide$ca_death_rate_aar))))

dem_react <- reactable(dem_table, resizable = TRUE, showPageSizeOptions = TRUE, searchable = TRUE,
                       onClick = 'expand', highlight = TRUE, rowStyle = list(cursor = 'pointer'),
                       details = function(index) {
                           if (index == 1) {
                               htmltools::div(style = "padding: 25px", pop)
                           } else if (index == 2) {
                               htmltools::div(style = "padding: 25px", ethn)
                           } else if (index == 3) {
                               htmltools::div(style = "padding: 25px", race)
                           } else if (index == 4) {
                               htmltools::div(style = "padding: 25px", birth_rate)
                           } else if (index == 5) {
                               htmltools::div(style = "padding: 25px", cr_dr)
                           } else if (index == 6) {
                               htmltools::div(style = "padding: 25px", aa_dr)
                           }
                       })

#Hlth
hlth_table <- bind_cols(`2020 Access to Care Indicators` = c('Percent of Adults with Health Insurance',
                                                             'Percent of Adults Unable to Afford Needed Care',
                                                             'Percent of Adults with a Usual Primary Care Provider',
                                                             'Percent of Adults who Received a Routine Medical Check-Up in the Last 12 months',
                                                             'Percent of Adults who Received a Routine Dental Check-Up in the Last 12 months',
                                                             'Percent of Women 40+ Who Had a Mammogram in the Previous Two Years',
                                                             'Percent of Adults 50+ who were up-to-date with Colorectal Cancer Screening',
                                                             'Percent of Births Where the Mother Received First Trimester Prenatal Care',
                                                             'Percent of Adults Receiving Flu Vaccination in Last 12 Months'),
                        `Bear River` = c(paste(last(hlth_wide$br_hlcov_cr),'%', sep = ''),
                                         paste(last(hlth_wide$br_no_care_cr),'%', sep = ''),
                                         paste(last(hlth_wide$br_hlth_pro_cr),'%', sep = ''),
                                         paste(last(hlth_wide$br_med_check_cr),'%', sep = ''),
                                         paste(last(hlth_wide$br_dent_check_cr),'%', sep = ''),
                                         paste(last(hlth_wide$br_mammo_cr),'%', sep = ''),
                                         paste(last(hlth_wide$br_col_scree_cr),'%', sep = ''),
                                         paste(last(hlth_wide$br_pn_perc),'%', sep = ''),
                                         paste(last(hlth_wide$br_fluvac_cr),'%', sep = '')),
                        `Box Elder` = c(paste(last(hlth_wide$be_hlcov_cr),'%', sep = ''),
                                        paste(last(hlth_wide$be_no_care_cr),'%', sep = ''),
                                        paste(last(hlth_wide$be_hlth_pro_cr),'%', sep = ''),
                                        paste(last(hlth_wide$be_med_check_cr),'%', sep = ''),
                                        paste(last(hlth_wide$be_dent_check_cr),'%', sep = ''),
                                        paste(last(hlth_wide$be_mammo_cr),'%', sep = ''),
                                        paste(last(hlth_wide$be_col_scree_cr),'%', sep = ''),
                                        paste(last(hlth_wide$be_pn_perc),'%', sep = ''),
                                        paste(last(hlth_wide$be_fluvac_cr),'%', sep = '')),
                        `Cache` = c(paste(last(hlth_wide$ca_hlcov_cr),'%', sep = ''),
                                    paste(last(hlth_wide$ca_no_care_cr),'%', sep = ''),
                                    paste(last(hlth_wide$ca_hlth_pro_cr),'%', sep = ''),
                                    paste(last(hlth_wide$ca_med_check_cr),'%', sep = ''),
                                    paste(last(hlth_wide$ca_dent_check_cr),'%', sep = ''),
                                    paste(last(hlth_wide$ca_mammo_cr),'%', sep = ''),
                                    paste(last(hlth_wide$ca_col_scree_cr),'%', sep = ''),
                                    paste(last(hlth_wide$ca_pn_perc),'%', sep = ''),
                                    paste(last(hlth_wide$ca_fluvac_cr),'%', sep = '')))

hlth_react <- reactable(hlth_table, resizable = TRUE, showPageSizeOptions = TRUE, searchable = TRUE,
                        onClick = 'expand', highlight = TRUE, rowStyle = list(cursor = 'pointer'),
                        details = function(index) {
                            if (index == 1) {
                                htmltools::div(style = "padding: 25px", ins)
                            } else if (index == 2) {
                                htmltools::div(style = "padding: 25px", no_care)
                            } else if (index == 3) {
                                htmltools::div(style = "padding: 25px", hlth_pro)
                            } else if (index == 4) {
                                htmltools::div(style = "padding: 25px", med_check)
                            } else if (index == 5) {
                                htmltools::div(style = "padding: 25px", dent_check)
                            } else if (index == 6) {
                                htmltools::div(style = "padding: 25px", mammo)
                            } else if (index == 7) {
                                htmltools::div(style = "padding: 25px", col_scree)
                            } else if (index == 8) {
                                htmltools::div(style = "padding: 25px", pn_perc)
                            } else if (index == 9) {
                                htmltools::div(style = "padding: 25px", fluvac)
                            }
                        })

#Risk Factors
risk_table <- bind_cols(`2020 Adult Risk Factors` = c('Rate of Births among Adolescents',
                                                      'Percent of Adults Consuming the Recommended Amount of Fruit',
                                                      'Percent of Adults Consuming the Recommended Amount of Vegetables',
                                                      'Percent of Adults Getting the Recommended Amount of Aerobic Physical Activity and Muscle Strengthening',
                                                      'Percent of Adults Currently Smoking',
                                                      'Percent of Percent of Adults Currently Using E-cigarettes',
                                                      'Percent of Adults Currently at Risk for Binge Drinking',
                                                      'Percent of Adults Who Always or Nearly Always Wear a Seat Belt'),
                        `Bear River` = c(paste(max(risk_wide$br_adol_brths)),
                                         paste(tail(risk_wide$br_fruit_cr[!is.na(risk_wide$br_fruit_cr)],1),'%', sep = ''),
                                         paste(tail(risk_wide$br_veg_cr[!is.na(risk_wide$br_veg_cr)],1),'%', sep = ''),
                                         paste(tail(risk_wide$br_phy_act_cr[!is.na(risk_wide$br_phy_act_cr)],1),'%', sep = ''),
                                         paste(tail(risk_wide$br_cig_smkng_cr[!is.na(risk_wide$br_cig_smkng_cr)],1),'%', sep = ''),
                                         paste(tail(risk_wide$br_ecig_cr[!is.na(risk_wide$br_ecig_cr)],1),'%', sep = ''),
                                         paste(tail(risk_wide$br_bnge_drnkng_cr[!is.na(risk_wide$br_bnge_drnkng_cr)],1),'%', sep = ''),
                                         paste(tail(risk_wide$br_st_blt_cr[!is.na(risk_wide$br_st_blt_cr)],1),'%', sep = '')),
                        `Box Elder` = c(paste(max(risk_wide$be_adol_brths)),
                                        paste(tail(risk_wide$be_fruit_cr[!is.na(risk_wide$be_fruit_cr)],1),'%', sep = ''),
                                        paste(tail(risk_wide$be_veg_cr[!is.na(risk_wide$be_veg_cr)],1),'%', sep = ''),
                                        paste(tail(risk_wide$be_phy_act_cr[!is.na(risk_wide$be_phy_act_cr)],1),'%', sep = ''),
                                        paste(tail(risk_wide$be_cig_smkng_cr[!is.na(risk_wide$be_cig_smkng_cr)],1),'%', sep = ''),
                                        paste(NA),
                                        paste(tail(risk_wide$be_bnge_drnkng_cr[!is.na(risk_wide$be_bnge_drnkng_cr)],1),'%', sep = ''),
                                        paste(tail(risk_wide$be_st_blt_cr[!is.na(risk_wide$be_st_blt_cr)],1),'%', sep = '')),
                        `Cache` = c(paste(max(risk_wide$ca_adol_brths)),
                                    paste(tail(risk_wide$ca_fruit_cr[!is.na(risk_wide$ca_fruit_cr)],1),'%', sep = ''),
                                    paste(tail(risk_wide$ca_veg_cr[!is.na(risk_wide$ca_veg_cr)],1),'%', sep = ''),
                                    paste(tail(risk_wide$ca_phy_act_cr[!is.na(risk_wide$ca_phy_act_cr)],1),'%', sep = ''),
                                    paste(tail(risk_wide$ca_cig_smkng_cr[!is.na(risk_wide$ca_cig_smkng_cr)],1),'%', sep = ''),
                                    paste(NA),
                                    paste(tail(risk_wide$ca_bnge_drnkng_cr[!is.na(risk_wide$ca_bnge_drnkng_cr)],1),'%', sep = ''),
                                    paste(tail(risk_wide$ca_st_blt_cr[!is.na(risk_wide$ca_st_blt_cr)],1),'%', sep = '')))


risk_react <- reactable(risk_table, resizable = TRUE, showPageSizeOptions = TRUE, searchable = TRUE,
                        onClick = 'expand', highlight = TRUE, rowStyle = list(cursor = 'pointer'),
                        details = function(index) {
                            if (index == 1) {
                                htmltools::div(style = "padding: 25px", adol_brths)
                            } else if (index == 2) {
                                htmltools::div(style = "padding: 25px", fruit_cr)
                            } else if (index == 3) {
                                htmltools::div(style = "padding: 25px", veg_cr)
                            } else if (index == 4) {
                                htmltools::div(style = "padding: 25px", phy_act_cr)
                            } else if (index == 5) {
                                htmltools::div(style = "padding: 25px", cig_smkng_cr)
                            } else if (index == 6) {
                                htmltools::div(style = "padding: 25px", ecig_cr)
                            } else if (index == 7) {
                                htmltools::div(style = "padding: 25px", bnge_drnkng_cr)
                            } else if (index == 8) {
                                htmltools::div(style = "padding: 25px", st_blt_cr)
                            }  
                        })

#Age-Adjusted Risk Indicators
risk_table_aa <- bind_cols(`2020 Adult Risk Factors` = c('Age-Adjusted Percent of Adults Consuming the Recommended Amount of Fruit',
                                                         'Age-Adjusted Percent of Adults Consuming the Recommended Amount of Vegetables',
                                                         'Age-Adjusted Percent of Adults Getting the Recommended Amount of Aerobic Physical Activity and Muscle Strengthening',
                                                         'Age-Adjusted Percent of Adults Currently Smoking',
                                                         'Age-Adjusted Percent of Percent of Adults Currently Using E-cigarettes',
                                                         'Age-Adjusted Percent of Adults Currently at Risk for Binge Drinking',
                                                         'Age-Adjusted Percent of Adults Who Always or Nearly Always Wear a Seat Belt'),
                           `Bear River` = c(paste(tail(risk_wide$br_fruit_aar[!is.na(risk_wide$br_fruit_aar)],1),'%', sep = ''),
                                            paste(tail(risk_wide$br_veg_aar[!is.na(risk_wide$br_veg_aar)],1),'%', sep = ''),
                                            paste(tail(risk_wide$br_phy_act_aar[!is.na(risk_wide$br_phy_act_aar)],1),'%', sep = ''),
                                            paste(tail(risk_wide$br_cig_smkng_aar[!is.na(risk_wide$br_cig_smkng_aar)],1),'%', sep = ''),
                                            paste(tail(risk_wide$br_ecig_aar[!is.na(risk_wide$br_ecig_aar)],1),'%', sep = ''),
                                            paste(tail(risk_wide$br_bnge_drnkng_aar[!is.na(risk_wide$br_bnge_drnkng_aar)],1),'%', sep = ''),
                                            paste(tail(risk_wide$br_st_blt_aar[!is.na(risk_wide$br_st_blt_aar)],1),'%', sep = '')),
                           `Box Elder` = c(paste(tail(risk_wide$be_fruit_aar[!is.na(risk_wide$be_fruit_aar)],1),'%', sep = ''),
                                           paste(tail(risk_wide$be_veg_aar[!is.na(risk_wide$be_veg_aar)],1),'%', sep = ''),
                                           paste(tail(risk_wide$be_phy_act_aar[!is.na(risk_wide$be_phy_act_aar)],1),'%', sep = ''),
                                           paste(tail(risk_wide$be_cig_smkng_aar[!is.na(risk_wide$be_cig_smkng_aar)],1),'%', sep = ''),
                                           paste(NA),
                                           paste(tail(risk_wide$be_bnge_drnkng_aar[!is.na(risk_wide$be_bnge_drnkng_aar)],1),'%', sep = ''),
                                           paste(tail(risk_wide$be_st_blt_aar[!is.na(risk_wide$be_st_blt_aar)],1),'%', sep = '')),
                           `Cache` = c(paste(tail(risk_wide$ca_fruit_aar[!is.na(risk_wide$ca_fruit_aar)],1),'%', sep = ''),
                                       paste(tail(risk_wide$ca_veg_aar[!is.na(risk_wide$ca_veg_aar)],1),'%', sep = ''),
                                       paste(tail(risk_wide$ca_phy_act_aar[!is.na(risk_wide$ca_phy_act_aar)],1),'%', sep = ''),
                                       paste(tail(risk_wide$ca_cig_smkng_aar[!is.na(risk_wide$ca_cig_smkng_aar)],1),'%', sep = ''),
                                       paste(NA),
                                       paste(tail(risk_wide$ca_bnge_drnkng_aar[!is.na(risk_wide$ca_bnge_drnkng_aar)],1),'%', sep = ''),
                                       paste(tail(risk_wide$ca_st_blt_aar[!is.na(risk_wide$ca_st_blt_aar)],1),'%', sep = '')))



risk_react_aa <- reactable(risk_table_aa, resizable = TRUE, showPageSizeOptions = TRUE, searchable = TRUE,
                           onClick = 'expand', highlight = TRUE, rowStyle = list(cursor = 'pointer'),
                           details = function(index) {
                               if (index == 1) {
                                   htmltools::div(style = "padding: 25px", aa_fruit)
                               } else if (index == 2) {
                                   htmltools::div(style = "padding: 25px", aa_veg)
                               } else if (index == 3) {
                                   htmltools::div(style = "padding: 25px", aa_phy_act)
                               } else if (index == 4) {
                                   htmltools::div(style = "padding: 25px", aa_cig_smkng)
                               } else if (index == 5) {
                                   htmltools::div(style = "padding: 25px", aa_ecig)
                               } else if (index == 6) {
                                   htmltools::div(style = "padding: 25px", aa_bnge_drnkng)
                               } else if (index == 7) {
                                   htmltools::div(style = "padding: 25px", aa_st_blt)
                               }  
                           })


#Outcomes
outcomes_table <- bind_cols(`2020 Outcomes` = c('Infant Mortality Rate per 1,000 births',
                                                'Unintentional Injury Mortality Rates by County',
                                                'Motor Vehicle Mortality Rates by County',
                                                'Drug Poisoning Mortality Rates by County',
                                                'Falls Mortality Rates by County',
                                                'Suicide Mortality Rates by County',
                                                'Diabetes Mortality Rates by County',
                                                'Cerebrovascular Mortality Rates by County',
                                                'Cancer Mortality Rates by County',
                                                'Lung Cancer Mortality Rates by County',
                                                'Breast Cancer Mortality Rates by County',
                                                'Colorectal Cancer Mortality Rates by County',
                                                'Prostate Cancer Mortality Rates by County',
                                                'Skin Cancer Mortality Rates by County',
                                                'Percent of Infants with Low Birth Weight',
                                                'Percent of Infants Born Preterm',
                                                'Percent of Adults Who have Experienced Rape or Attempted Rape',
                                                'Percent of Adults Who had <7 days of Poor Physical Health in the Last 30 Days',
                                                'Percent of Adults Who had <7 days of Poor Mental Health in the Last 30 Days',
                                                'Percent of Adults Who have Diagnosed Arthritis',
                                                'Percent of Adults Who Currently have Diagnosed Asthma',
                                                'Percent of Adults Who have been Told they have Diabetes',
                                                'Percent of Adults Who have been Told they have Pre-diabetes'),
                            `Bear River` = c(paste(tail(outcomes_wide$br_inf_mort_rate[!is.na(outcomes_wide$br_inf_mort_rate)],1)),
                                             paste(tail(outcomes_wide$br_unint_inj_cr[!is.na(outcomes_wide$br_unint_inj_cr)],1)),
                                             paste(tail(outcomes_wide$br_mvc_cr[!is.na(outcomes_wide$br_mvc_cr)],1)),
                                             paste(tail(outcomes_wide$br_drug_poi_cr[!is.na(outcomes_wide$br_drug_poi_cr)],1)),
                                             paste(tail(outcomes_wide$br_falls_cr[!is.na(outcomes_wide$br_falls_cr)],1)),
                                             paste(tail(outcomes_wide$br_sui_cr[!is.na(outcomes_wide$br_sui_cr)],1)),
                                             paste(tail(outcomes_wide$br_mort_diab_cr[!is.na(outcomes_wide$br_mort_diab_cr)],1)),
                                             paste(tail(outcomes_wide$br_mort_cvs_cr[!is.na(outcomes_wide$br_mort_cvs_cr)],1)),
                                             paste(tail(outcomes_wide$br_mort_can_cr[!is.na(outcomes_wide$br_mort_can_cr)],1)),
                                             paste(tail(outcomes_wide$br_mort_lung_can_cr[!is.na(outcomes_wide$br_mort_lung_can_cr)],1)),
                                             paste(tail(outcomes_wide$br_mort_brst_can_cr[!is.na(outcomes_wide$br_mort_brst_can_cr)],1)),
                                             paste(tail(outcomes_wide$br_mort_colrect_can_cr[!is.na(outcomes_wide$br_mort_colrect_can_cr)],1)),
                                             paste(tail(outcomes_wide$br_mort_pros_can_cr[!is.na(outcomes_wide$br_mort_pros_can_cr)],1)),
                                             paste(tail(outcomes_wide$br_mort_skin_can_cr[!is.na(outcomes_wide$br_mort_skin_can_cr)],1)),
                                             paste(tail(outcomes_wide$br_lw_brth_inf[!is.na(outcomes_wide$br_lw_brth_inf)],1),'%', sep = ''),
                                             paste(tail(outcomes_wide$br_preterm_births[!is.na(outcomes_wide$br_preterm_births)],1)),
                                             paste(tail(outcomes_wide$br_rape_cr[!is.na(outcomes_wide$br_rape_cr)],1),'%', sep = ''),
                                             paste(tail(outcomes_wide$br_phy_hlth_cr[!is.na(outcomes_wide$br_phy_hlth_cr)],1),'%', sep = ''),
                                             paste(tail(outcomes_wide$br_ment_hlth_cr[!is.na(outcomes_wide$br_ment_hlth_cr)],1),'%', sep = ''),
                                             paste(tail(outcomes_wide$br_arthrits_cr[!is.na(outcomes_wide$br_arthrits_cr)],1),'%', sep = ''),
                                             paste(tail(outcomes_wide$br_astma_cr[!is.na(outcomes_wide$br_astma_cr)],1),'%', sep = ''),
                                             paste(tail(outcomes_wide$br_diab_cr[!is.na(outcomes_wide$br_diab_cr)],1),'%', sep = ''),
                                             paste(tail(outcomes_wide$br_prediabets_cr[!is.na(outcomes_wide$br_prediabets_cr)],1),'%', sep = '')),
                            `Box Elder` = c(paste(tail(outcomes_wide$be_inf_mort_rate[!is.na(outcomes_wide$be_inf_mort_rate)],1)),
                                            paste(tail(outcomes_wide$be_unint_inj_cr[!is.na(outcomes_wide$be_unint_inj_cr)],1)),
                                            paste(tail(outcomes_wide$be_mvc_cr[!is.na(outcomes_wide$be_mvc_cr)],1)),
                                            paste(tail(outcomes_wide$be_drug_poi_cr[!is.na(outcomes_wide$be_drug_poi_cr)],1)),
                                            paste(tail(outcomes_wide$be_falls_cr[!is.na(outcomes_wide$be_falls_cr)],1)),
                                            paste(tail(outcomes_wide$be_sui_cr[!is.na(outcomes_wide$be_sui_cr)],1)),
                                            paste(tail(outcomes_wide$be_mort_diab_cr[!is.na(outcomes_wide$be_mort_diab_cr)],1)),
                                            paste(tail(outcomes_wide$be_mort_cvs_cr[!is.na(outcomes_wide$be_mort_cvs_cr)],1)),
                                            paste(tail(outcomes_wide$be_mort_can_cr[!is.na(outcomes_wide$be_mort_can_cr)],1)),
                                            paste(tail(outcomes_wide$be_mort_lung_can_cr[!is.na(outcomes_wide$be_mort_lung_can_cr)],1)),
                                            paste(tail(outcomes_wide$be_mort_brst_can_cr[!is.na(outcomes_wide$ca_mort_brst_can_cr)],1)),
                                            paste(tail(outcomes_wide$be_mort_colrect_can_cr[!is.na(outcomes_wide$be_mort_colrect_can_cr)],1)),
                                            paste(tail(outcomes_wide$be_mort_pros_can_cr[!is.na(outcomes_wide$be_mort_pros_can_cr)],1)),
                                            paste(tail(outcomes_wide$be_mort_skin_can_cr[!is.na(outcomes_wide$be_mort_skin_can_cr)],1)),
                                            paste(tail(outcomes_wide$be_lw_brth_inf[!is.na(outcomes_wide$be_lw_brth_inf)],1),'%', sep = ''),
                                            paste(tail(outcomes_wide$be_preterm_births[!is.na(outcomes_wide$be_preterm_births)],1)),
                                            paste(tail(outcomes_wide$be_rape_cr[!is.na(outcomes_wide$be_rape_cr)],1),'%', sep = ''),
                                            paste(tail(outcomes_wide$be_phy_hlth_cr[!is.na(outcomes_wide$be_phy_hlth_cr)],1),'%', sep = ''),
                                            paste(tail(outcomes_wide$be_ment_hlth_cr[!is.na(outcomes_wide$be_ment_hlth_cr)],1),'%', sep = ''),
                                            paste(tail(outcomes_wide$be_arthrits_cr[!is.na(outcomes_wide$be_arthrits_cr)],1),'%', sep = ''),
                                            paste(tail(outcomes_wide$be_astma_cr[!is.na(outcomes_wide$be_astma_cr)],1),'%', sep = ''),
                                            paste(tail(outcomes_wide$be_diab_cr[!is.na(outcomes_wide$be_diab_cr)],1),'%', sep = ''),
                                            paste(tail(outcomes_wide$be_prediabets_cr[!is.na(outcomes_wide$be_prediabets_cr)],1),'%', sep = '')),
                            `Cache` = c(paste(tail(outcomes_wide$ca_inf_mort_rate[!is.na(outcomes_wide$ca_inf_mort_rate)],1)),
                                        paste(tail(outcomes_wide$ca_unint_inj_cr[!is.na(outcomes_wide$ca_unint_inj_cr)],1)),
                                        paste(tail(outcomes_wide$ca_mvc_cr[!is.na(outcomes_wide$ca_mvc_cr)],1)),
                                        paste(tail(outcomes_wide$ca_drug_poi_cr[!is.na(outcomes_wide$ca_drug_poi_cr)],1)),
                                        paste(tail(outcomes_wide$ca_falls_cr[!is.na(outcomes_wide$ca_falls_cr)],1)),
                                        paste(tail(outcomes_wide$ca_sui_cr[!is.na(outcomes_wide$ca_sui_cr)],1)),
                                        paste(tail(outcomes_wide$ca_mort_diab_cr[!is.na(outcomes_wide$ca_mort_diab_cr)],1)),
                                        paste(tail(outcomes_wide$ca_mort_cvs_cr[!is.na(outcomes_wide$ca_mort_cvs_cr)],1)),
                                        paste(tail(outcomes_wide$ca_mort_can_cr[!is.na(outcomes_wide$ca_mort_can_cr)],1)),
                                        paste(tail(outcomes_wide$ca_mort_lung_can_cr[!is.na(outcomes_wide$ca_mort_lung_can_cr)],1)),
                                        paste(tail(outcomes_wide$ca_mort_brst_can_cr[!is.na(outcomes_wide$ca_mort_brst_can_cr)],1)),
                                        paste(tail(outcomes_wide$ca_mort_colrect_can_cr[!is.na(outcomes_wide$ca_mort_colrect_can_cr)],1)),
                                        paste(tail(outcomes_wide$ca_mort_pros_can_cr[!is.na(outcomes_wide$ca_mort_pros_can_cr)],1)),
                                        paste(tail(outcomes_wide$ca_mort_skin_can_cr[!is.na(outcomes_wide$ca_mort_skin_can_cr)],1)),
                                        paste(tail(outcomes_wide$ca_lw_brth_inf[!is.na(outcomes_wide$ca_lw_brth_inf)],1),'%', sep = ''),
                                        paste(tail(outcomes_wide$ca_preterm_births[!is.na(outcomes_wide$ca_preterm_births)],1)),
                                        paste(tail(outcomes_wide$ca_rape_cr[!is.na(outcomes_wide$ca_rape_cr)],1),'%', sep = ''),
                                        paste(tail(outcomes_wide$ca_phy_hlth_cr[!is.na(outcomes_wide$ca_phy_hlth_cr)],1),'%', sep = ''),
                                        paste(tail(outcomes_wide$ca_ment_hlth_cr[!is.na(outcomes_wide$ca_ment_hlth_cr)],1),'%', sep = ''),
                                        paste(tail(outcomes_wide$ca_arthrits_cr[!is.na(outcomes_wide$ca_arthrits_cr)],1),'%', sep = ''),
                                        paste(tail(outcomes_wide$ca_astma_cr[!is.na(outcomes_wide$ca_astma_cr)],1),'%', sep = ''),
                                        paste(tail(outcomes_wide$ca_diab_cr[!is.na(outcomes_wide$ca_diab_cr)],1),'%', sep = ''),
                                        paste(tail(outcomes_wide$ca_prediabets_cr[!is.na(outcomes_wide$ca_prediabets_cr)],1),'%', sep = '')))

outcomes_react <- reactable(outcomes_table, resizable = TRUE, showPageSizeOptions = TRUE, defaultPageSize = 25, searchable = TRUE,
                            onClick = 'expand', highlight = TRUE, rowStyle = list(cursor = 'pointer'),
                            details = function(index) {
                                if (index == 1) {
                                    htmltools::div(style = "padding: 25px", inf_mort_rate)
                                } else if (index == 2) {
                                    htmltools::div(style = "padding: 25px", unint_inj_cr)
                                } else if (index == 3) {
                                    htmltools::div(style = "padding: 25px", mvc_cr)
                                } else if (index == 4) {
                                    htmltools::div(style = "padding: 25px", drug_poi_cr)
                                } else if (index == 5) {
                                    htmltools::div(style = "padding: 25px", falls_cr)
                                } else if (index == 6) {
                                    htmltools::div(style = "padding: 25px", sui_cr)
                                } else if (index == 7) {
                                    htmltools::div(style = "padding: 25px", mort_diab_cr)
                                } else if (index == 8) {
                                    htmltools::div(style = "padding: 25px", mort_cvs_cr)
                                } else if (index == 9) {
                                    htmltools::div(style = "padding: 25px", mort_can_cr)
                                } else if (index == 10) {
                                    htmltools::div(style = "padding: 25px", mort_lung_can)
                                } else if (index == 11) {
                                    htmltools::div(style = "padding: 25px", mort_brst_can_cr)
                                } else if (index == 12) {
                                    htmltools::div(style = "padding: 25px", mort_colrect_can_cr)
                                } else if (index == 13) {
                                    htmltools::div(style = "padding: 25px", mort_pros_can_cr)
                                } else if (index == 14) {
                                    htmltools::div(style = "padding: 25px", mort_skin_can_cr)
                                } else if (index == 15) {
                                    htmltools::div(style = "padding: 25px", lw_brth_inf)
                                } else if (index == 16) {
                                    htmltools::div(style = "padding: 25px", preterm_births)
                                } else if (index == 17) {
                                    htmltools::div(style = "padding: 25px", rape_cr)
                                } else if (index == 18) {
                                    htmltools::div(style = "padding: 25px", phy_hlth_cr)
                                } else if (index == 19) {
                                    htmltools::div(style = "padding: 25px", ment_hlth_cr)
                                } else if (index == 20) {
                                    htmltools::div(style = "padding: 25px", arthrits_cr)
                                } else if (index == 21) {
                                    htmltools::div(style = "padding: 25px", astma_cr)
                                } else if (index == 22) {
                                    htmltools::div(style = "padding: 25px", diab_cr)
                                } else if (index == 23) {
                                    htmltools::div(style = "padding: 25px", prediabets_cr)
                                }
                            })
#Age-Adjusted Outcome Data
outcomes_table_aa <- bind_cols(`2020 Outcomes` = c('Age-Adjusted Unintentional Injury Mortality Rates by County',
                                                   'Age-Adjusted Motor Vehicle Mortality Rates by County',
                                                   'Age-Adjusted Drug Poisoning Mortality Rates by County',
                                                   'Age-Adjusted Falls Mortality Rates by County',
                                                   'Age-Adjusted Suicide Mortality Rates by County',
                                                   'Age-Adjusted Diabetes Mortality Rates by County',
                                                   'Age-Adjusted Cerebrovascular Mortality Rates by County',
                                                   'Age-Adjusted Cancer Mortality Rates by County',
                                                   'Age-Adjusted Lung Cancer Mortality Rates by County',
                                                   'Age-Adjusted Breast Cancer Mortality Rates by County',
                                                   'Age-Adjusted Colorectal Cancer Mortality Rates by County',
                                                   'Age-Adjusted Prostate Cancer Mortality Rates by County',
                                                   'Age-Adjusted Skin Cancer Mortality Rates by County',
                                                   'Age-Adjusted Percent of Adults Who have Experienced Rape/Attempted Rape',
                                                   'Age-Adjusted Percent of Adults Who had <7 days of Poor Physical Health in the Last 30 Days',
                                                   'Age-Adjusted Percent of Adults Who had <7 days of Poor Mental Health in the Last 30 Days',
                                                   'Age-Adjusted Percent of Adults Who have Diagnosed Arthritis',
                                                   'Age-Adjusted Percent of Adults Who Currently have Diagnosed Asthma',
                                                   'Age-Adjusted Percent of Adults Who have been Told they have Diabetes',
                                                   'Age-Adjusted Percent of Adults Who have been Told they have Pre-diabetes'),
                               `Bear River` = c(paste(tail(outcomes_wide$br_unint_inj_aar[!is.na(outcomes_wide$br_unint_inj_aar)],1)),
                                                paste(tail(outcomes_wide$br_mvc_aar[!is.na(outcomes_wide$br_mvc_aar)],1)),
                                                paste(tail(outcomes_wide$br_drug_poi_aar[!is.na(outcomes_wide$br_drug_poi_aar)],1)),
                                                paste(tail(outcomes_wide$br_falls_aar[!is.na(outcomes_wide$br_falls_aar)],1)),
                                                paste(tail(outcomes_wide$br_sui_aar[!is.na(outcomes_wide$br_sui_aar)],1)),
                                                paste(tail(outcomes_wide$br_mort_diab_aar[!is.na(outcomes_wide$br_mort_diab_aar)],1)),
                                                paste(tail(outcomes_wide$br_mort_cvs_aar[!is.na(outcomes_wide$br_mort_cvs_aar)],1)),
                                                paste(tail(outcomes_wide$br_mort_can_aar[!is.na(outcomes_wide$br_mort_can_aar)],1)),
                                                paste(tail(outcomes_wide$br_mort_lung_can_aar[!is.na(outcomes_wide$br_mort_lung_can_aar)],1)),
                                                paste(tail(outcomes_wide$br_mort_brst_can_aar[!is.na(outcomes_wide$br_mort_brst_can_aar)],1)),
                                                paste(tail(outcomes_wide$br_mort_colrect_can_aar[!is.na(outcomes_wide$br_mort_colrect_can_aar)],1)),
                                                paste(tail(outcomes_wide$br_mort_pros_can_aar[!is.na(outcomes_wide$br_mort_pros_can_aar)],1)),
                                                paste(tail(outcomes_wide$br_mort_skin_can_aar[!is.na(outcomes_wide$br_mort_skin_can_aar)],1)),
                                                paste(tail(outcomes_wide$br_rape_aar[!is.na(outcomes_wide$br_rape_aar)],1),'%', sep = ''),
                                                paste(tail(outcomes_wide$br_phy_hlth_aar[!is.na(outcomes_wide$br_phy_hlth_aar)],1),'%', sep = ''),
                                                paste(tail(outcomes_wide$br_ment_hlth_aar[!is.na(outcomes_wide$br_ment_hlth_aar)],1),'%', sep = ''),
                                                paste(tail(outcomes_wide$br_arthrits_aar[!is.na(outcomes_wide$br_arthrits_aar)],1),'%', sep = ''),
                                                paste(tail(outcomes_wide$br_astma_aar[!is.na(outcomes_wide$br_astma_aar)],1),'%', sep = ''),
                                                paste(tail(outcomes_wide$br_diab_aar[!is.na(outcomes_wide$br_diab_aar)],1),'%', sep = ''),
                                                paste(tail(outcomes_wide$br_prediabets_aar[!is.na(outcomes_wide$br_prediabets_aar)],1),'%', sep = '')),
                               `Box Elder` = c(paste(tail(outcomes_wide$be_unint_inj_aar[!is.na(outcomes_wide$be_unint_inj_aar)],1)),
                                               paste(tail(outcomes_wide$be_mvc_aar[!is.na(outcomes_wide$be_mvc_aar)],1)),
                                               paste(tail(outcomes_wide$be_drug_poi_aar[!is.na(outcomes_wide$be_drug_poi_aar)],1)),
                                               paste(tail(outcomes_wide$be_falls_aar[!is.na(outcomes_wide$be_falls_aar)],1)),
                                               paste(tail(outcomes_wide$be_sui_aar[!is.na(outcomes_wide$be_sui_aar)],1)),
                                               paste(tail(outcomes_wide$be_mort_diab_aar[!is.na(outcomes_wide$be_mort_diab_aar)],1)),
                                               paste(tail(outcomes_wide$be_mort_cvs_aar[!is.na(outcomes_wide$be_mort_cvs_aar)],1)),
                                               paste(tail(outcomes_wide$be_mort_can_aar[!is.na(outcomes_wide$be_mort_can_aar)],1)),
                                               paste(tail(outcomes_wide$be_mort_lung_can_aar[!is.na(outcomes_wide$be_mort_lung_can_aar)],1)),
                                               paste(tail(outcomes_wide$be_mort_brst_can_aar[!is.na(outcomes_wide$ca_mort_brst_can_aar)],1)),
                                               paste(tail(outcomes_wide$be_mort_colrect_can_aar[!is.na(outcomes_wide$be_mort_colrect_can_aar)],1)),
                                               paste(tail(outcomes_wide$be_mort_pros_can_aar[!is.na(outcomes_wide$be_mort_pros_can_aar)],1)),
                                               paste(tail(outcomes_wide$be_mort_skin_can_aar[!is.na(outcomes_wide$be_mort_skin_can_aar)],1)),
                                               paste(tail(outcomes_wide$be_rape_aar[!is.na(outcomes_wide$be_rape_aar)],1),'%', sep = ''),
                                               paste(tail(outcomes_wide$be_phy_hlth_aar[!is.na(outcomes_wide$be_phy_hlth_aar)],1),'%', sep = ''),
                                               paste(tail(outcomes_wide$be_ment_hlth_aar[!is.na(outcomes_wide$be_ment_hlth_aar)],1),'%', sep = ''),
                                               paste(tail(outcomes_wide$be_arthrits_aar[!is.na(outcomes_wide$be_arthrits_aar)],1),'%', sep = ''),
                                               paste(tail(outcomes_wide$be_astma_aar[!is.na(outcomes_wide$be_astma_aar)],1),'%', sep = ''),
                                               paste(tail(outcomes_wide$be_diab_aar[!is.na(outcomes_wide$be_diab_aar)],1),'%', sep = ''),
                                               paste(tail(outcomes_wide$be_prediabets_aar[!is.na(outcomes_wide$be_prediabets_aar)],1),'%', sep = '')),
                               `Cache` = c(paste(tail(outcomes_wide$ca_unint_inj_aar[!is.na(outcomes_wide$ca_unint_inj_aar)],1)),
                                           paste(tail(outcomes_wide$ca_mvc_aar[!is.na(outcomes_wide$ca_mvc_aar)],1)),
                                           paste(tail(outcomes_wide$ca_drug_poi_aar[!is.na(outcomes_wide$ca_drug_poi_aar)],1)),
                                           paste(tail(outcomes_wide$ca_falls_aar[!is.na(outcomes_wide$ca_falls_aar)],1)),
                                           paste(tail(outcomes_wide$ca_sui_aar[!is.na(outcomes_wide$ca_sui_aar)],1)),
                                           paste(tail(outcomes_wide$ca_mort_diab_aar[!is.na(outcomes_wide$ca_mort_diab_aar)],1)),
                                           paste(tail(outcomes_wide$ca_mort_cvs_aar[!is.na(outcomes_wide$ca_mort_cvs_aar)],1)),
                                           paste(tail(outcomes_wide$ca_mort_can_aar[!is.na(outcomes_wide$ca_mort_can_aar)],1)),
                                           paste(tail(outcomes_wide$ca_mort_lung_can_aar[!is.na(outcomes_wide$ca_mort_lung_can_aar)],1)),
                                           paste(tail(outcomes_wide$ca_mort_brst_can_aar[!is.na(outcomes_wide$ca_mort_brst_can_aar)],1)),
                                           paste(tail(outcomes_wide$ca_mort_colrect_can_aar[!is.na(outcomes_wide$ca_mort_colrect_can_aar)],1)),
                                           paste(tail(outcomes_wide$ca_mort_pros_can_aar[!is.na(outcomes_wide$ca_mort_pros_can_aar)],1)),
                                           paste(tail(outcomes_wide$ca_mort_skin_can_aar[!is.na(outcomes_wide$ca_mort_skin_can_aar)],1)),
                                           paste(tail(outcomes_wide$ca_rape_aar[!is.na(outcomes_wide$ca_rape_aar)],1),'%', sep = ''),
                                           paste(tail(outcomes_wide$ca_phy_hlth_aar[!is.na(outcomes_wide$ca_phy_hlth_aar)],1),'%', sep = ''),
                                           paste(tail(outcomes_wide$ca_ment_hlth_aar[!is.na(outcomes_wide$ca_ment_hlth_aar)],1),'%', sep = ''),
                                           paste(tail(outcomes_wide$ca_arthrits_aar[!is.na(outcomes_wide$ca_arthrits_aar)],1),'%', sep = ''),
                                           paste(tail(outcomes_wide$ca_astma_aar[!is.na(outcomes_wide$ca_astma_aar)],1),'%', sep = ''),
                                           paste(tail(outcomes_wide$ca_diab_aar[!is.na(outcomes_wide$ca_diab_aar)],1),'%', sep = ''),
                                           paste(tail(outcomes_wide$ca_prediabets_aar[!is.na(outcomes_wide$ca_prediabets_aar)],1),'%', sep = '')))
#Creates the drop down menu that shows the graph
outcomes_react_aa <- reactable(outcomes_table_aa, resizable = TRUE, showPageSizeOptions = TRUE, defaultPageSize = 25, searchable = TRUE,
                               onClick = 'expand', highlight = TRUE, rowStyle = list(cursor = 'pointer'),
                               details = function(index) {
                                   if (index == 1) {
                                       htmltools::div(style = "padding: 25px", aa_unint_inj)
                                   } else if (index == 2) {
                                       htmltools::div(style = "padding: 25px", aa_mvc)
                                   } else if (index == 3) {
                                       htmltools::div(style = "padding: 25px", aa_drug_poi)
                                   } else if (index == 4) {
                                       htmltools::div(style = "padding: 25px", aa_falls)
                                   } else if (index == 5) {
                                       htmltools::div(style = "padding: 25px", aa_sui)
                                   } else if (index == 6) {
                                       htmltools::div(style = "padding: 25px", aa_mort_diab)
                                   } else if (index == 7) {
                                       htmltools::div(style = "padding: 25px", aa_mort_cvs)
                                   } else if (index == 8) {
                                       htmltools::div(style = "padding: 25px", aa_mort_can)
                                   } else if (index == 9) {
                                       htmltools::div(style = "padding: 25px", aa_mort_lung_can)
                                   } else if (index == 10) {
                                       htmltools::div(style = "padding: 25px", aa_mort_brst_can)
                                   } else if (index == 11) {
                                       htmltools::div(style = "padding: 25px", aa_mort_colrect_can)
                                   } else if (index == 12) {
                                       htmltools::div(style = "padding: 25px", aa_mort_pros_can)
                                   } else if (index == 13) {
                                       htmltools::div(style = "padding: 25px", aa_mort_skin_can)
                                   } else if (index == 14) {
                                       htmltools::div(style = "padding: 25px", aa_rape)
                                   } else if (index == 15) {
                                       htmltools::div(style = "padding: 25px", aa_phy_hlth)
                                   } else if (index == 16) {
                                       htmltools::div(style = "padding: 25px", aa_ment_hlth)
                                   } else if (index == 17) {
                                       htmltools::div(style = "padding: 25px", aa_arthrits)
                                   } else if (index == 18) {
                                       htmltools::div(style = "padding: 25px", aa_astma)
                                   } else if (index == 19) {
                                       htmltools::div(style = "padding: 25px", aa_diab)
                                   } else if (index == 20) {
                                       htmltools::div(style = "padding: 25px", aa_prediabets)
                                   }
                               })

### Possibly add logo?
#header <- dashboardHeader()
#anchor <- tags$a(tags$img(src='BRHDlogo.png', height='50', width='70'),
#'COVID-19')

#header$children[[2]]$children <- tags$div(
#tags$head(tags$style(HTML(".name { background-color: white }"))),
#anchor,
#class = 'name')

ui <- dashboardPage(
    dashboardHeader(title = 'Healthier BRHD'),
    ## Sidebar content
    dashboardSidebar(
        sidebarMenu(
            id = "sidebar",
            menuItem("Demographics", tabName = "Demographics", icon = icon("users")),
            menuItem("Healthcare", tabName = "Healthcare", icon = icon("notes-medical")),
            menuItem("Risk Factors", tabName = "riskfactors", icon = icon("heartbeat")),
            menuItem("Outcomes", tabName = "Outcomes", icon = icon("universal-access"))
        )
    ),
    ## Body content
    dashboardBody(
        tabItems(
            tabItem(tabName = "Demographics",
                    mainPanel(
                        h4("Last updated 4/18/2022 at 11:00 a.m.")
                    ),
                    fluidRow(
                        tabBox(
                            width = 12,
                            tabPanel('Overview', reactableOutput('dem_react')
                            )
                        ),
                    )
            ),
            tabItem(tabName = "Healthcare",
                    fluidRow(
                        tabBox(
                            width = 12,
                            tabPanel('Crude Rates & Percentages', reactableOutput('hlth_react')
                            )
                        ),
                    )
            ),
            tabItem(tabName = "riskfactors",
                    fluidRow(
                        tabBox(
                            width = 12,
                            tabPanel('Crude Rates & Percentages', reactableOutput('risk_react')
                            ),
                            tabPanel('Age-Adjusted Rates & Percentages', reactableOutput('risk_react_aa')
                            ),
                        ),
                    )
            ),
            tabItem(tabName = "Outcomes",
                    fluidRow(
                        tabBox(
                            width = 12,
                            tabPanel('Crude Rates & Percentages', reactableOutput('outcomes_react')
                            ),
                            tabPanel('Age-Adjusted Rates & Percentages', reactableOutput('outcomes_react_aa')
                            ),
                        ),
                    )
            )
        )
    )
)




server <- function(input, output) {
    output$dem_react <- renderReactable({
        dem_react
    })
    
    output$hlth_react <- renderReactable({
        hlth_react
    })
    
    output$risk_react <- renderReactable({
        risk_react
    })
    
    output$outcomes_react <- renderReactable({
        outcomes_react
    })
    
    output$risk_react_aa <- renderReactable({
        risk_react_aa
    })
    
    output$outcomes_react_aa <- renderReactable({
        outcomes_react_aa
    })
    
    
}

shinyApp(ui, server)