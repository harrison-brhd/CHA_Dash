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
#Health Poverty#
hlth_table_pov <- bind_cols(`Access to Care Indicators by Poverty` = c('Percent of Adults with Health Insurance by Poverty',
                                                                       'Percent of Adults Unable to Afford Needed Care by Poverty',
                                                                       'Percent of Adults with a Usual Primary Care Provider by Poverty',
                                                                       'Percent of Adults who Received a Routine Medical Check-Up in the Last 12 months by Poverty',
                                                                       'Percent of Adults who Received a Routine Dental Check-Up in the Last 12 months by Poverty',
                                                                       'Percent of Women 40+ Who Had a Mammogram in the Previous Two Years by Poverty',
                                                                       'Percent of Adults 50+ who were up-to-date with Colorectal Cancer Screening by Poverty',
                                                                       'Percent of Adults Receiving Flu Vaccination in Last 12 Months by Poverty'),
                            `Below` = c(paste(first(hlth_pov$hlcov_cr),'%', sep = ''),
                                        paste(first(hlth_pov$no_care_cr),'%', sep = ''),
                                        paste(first(hlth_pov$hlth_pro_cr),'%', sep = ''),
                                        paste(first(hlth_pov$med_check_cr),'%', sep = ''),
                                        paste(first(hlth_pov$dent_check_cr),'%', sep = ''),
                                        paste(first(hlth_pov$mammo_cr),'%', sep = ''),
                                        paste(first(hlth_pov$col_scree_cr),'%', sep = ''),
                                        paste(first(hlth_pov$fluvac_cr),'%', sep = '')),
                            `Above` = c(paste(last(hlth_pov$hlcov_cr),'%', sep = ''),
                                        paste(last(hlth_pov$no_care_cr),'%', sep = ''),
                                        paste(last(hlth_pov$hlth_pro_cr),'%', sep = ''),
                                        paste(last(hlth_pov$med_check_cr),'%', sep = ''),
                                        paste(last(hlth_pov$dent_check_cr),'%', sep = ''),
                                        paste(last(hlth_pov$mammo_cr),'%', sep = ''),
                                        paste(last(hlth_pov$col_scree_cr),'%', sep = ''),
                                        paste(last(hlth_pov$fluvac_cr),'%', sep = '')))

#Creates the drop down menu that shows the graph
hlth_react_pov <- reactable(hlth_table_pov, resizable = TRUE, showPageSizeOptions = TRUE,
                            onClick = 'expand', highlight = TRUE, rowStyle = list(cursor = 'pointer'),
                            details = function(index) {
                                if (index == 1) {
                                    htmltools::div(style = "padding: 50px", ggplotly(ins_pov))
                                } else if (index == 2) {
                                    htmltools::div(style = "padding: 25px", ggplotly(no_care_pov))
                                } else if (index == 3) {
                                    htmltools::div(style = "padding: 25px", ggplotly(hlth_pro_pov))
                                } else if (index == 4) {
                                    htmltools::div(style = "padding: 25px", ggplotly(med_check_pov))
                                } else if (index == 5) {
                                    htmltools::div(style = "padding: 25px", ggplotly(dent_check_pov))
                                } else if (index == 6) {
                                    htmltools::div(style = "padding: 25px", ggplotly(mammo_pov))
                                } else if (index == 7) {
                                    htmltools::div(style = "padding: 25px", ggplotly(col_scree_pov)) 
                                } else if (index == 8) {
                                    htmltools::div(style = "padding: 25px", ggplotly(fluvac_pov))
                                }
                            })
###Race/Ethnicity###
hlth_table_race <- bind_cols(`Access to Care Indicators by Race/Ethnicity` = c('Percent of Adults with Health Insurance by Race/Ethnicity',
                                                                               'Percent of Adults Unable to Afford Needed Care by Race/Ethnicity',
                                                                               'Percent of Adults with a Usual Primary Care Provider by Race/Ethnicity',
                                                                               'Percent of Adults who Received a Routine Medical Check-Up in the Last 12 months by Race/Ethnicity',
                                                                               'Percent of Adults who Received a Routine Dental Check-Up in the Last 12 months by Race/Ethnicity',
                                                                               'Percent of Women 40+ Who Had a Mammogram in the Previous Two Years by Race/Ethnicity',
                                                                               'Percent of Adults 50+ who were up-to-date with Colorectal Cancer Screening by Race/Ethnicity',
                                                                               'Percent of Adults Receiving Flu Vaccination in Last 12 Months by Race/Ethnicity'),
                             `American Indian` = c(paste(hlth_race[1, 6],'%', sep = ''),
                                                   paste(hlth_race[1, 12],'%', sep = ''),
                                                   paste(hlth_race[1, 18],'%', sep = ''),
                                                   paste(hlth_race[1, 24],'%', sep = ''),
                                                   paste(hlth_race[1, 30],'%', sep = ''),
                                                   paste(hlth_race[1, 36],'%', sep = ''),
                                                   paste(hlth_race[1, 42],'%', sep = ''),
                                                   paste(hlth_race[1, 51],'%', sep = '')),
                             `Asian` = c(paste(hlth_race[2, 6],'%', sep = ''),
                                         paste(hlth_race[2, 12],'%', sep = ''),
                                         paste(hlth_race[2, 18],'%', sep = ''),
                                         paste(hlth_race[2, 24],'%', sep = ''),
                                         paste(hlth_race[2, 30],'%', sep = ''),
                                         paste(hlth_race[2, 36],'%', sep = ''),
                                         paste(hlth_race[2, 42],'%', sep = ''),
                                         paste(hlth_race[2, 51],'%', sep = '')),
                             `Black` = c(paste(hlth_race[3, 6],'%', sep = ''),
                                         paste(hlth_race[3, 12],'%', sep = ''),
                                         paste(hlth_race[3, 18],'%', sep = ''),
                                         paste(hlth_race[3, 24],'%', sep = ''),
                                         paste(hlth_race[3, 30],'%', sep = ''),
                                         paste(hlth_race[3, 36],'%', sep = ''),
                                         paste(hlth_race[3, 42],'%', sep = ''),
                                         paste(hlth_race[3, 51],'%', sep = '')),
                             `Hispanic` = c(paste(hlth_race[4, 6],'%', sep = ''),
                                            paste(hlth_race[4, 12],'%', sep = ''),
                                            paste(hlth_race[4, 18],'%', sep = ''),
                                            paste(hlth_race[4, 24],'%', sep = ''),
                                            paste(hlth_race[4, 30],'%', sep = ''),
                                            paste(hlth_race[4, 36],'%', sep = ''),
                                            paste(hlth_race[4, 42],'%', sep = ''),
                                            paste(hlth_race[4, 51],'%', sep = '')),
                             `Pacific Islander` = c(paste(hlth_race[5, 6],'%', sep = ''),
                                                    paste(hlth_race[5, 12],'%', sep = ''),
                                                    paste(hlth_race[5, 18],'%', sep = ''),
                                                    paste(hlth_race[5, 24],'%', sep = ''),
                                                    paste(hlth_race[5, 30],'%', sep = ''),
                                                    paste(hlth_race[5, 36],'%', sep = ''),
                                                    paste(hlth_race[5, 42],'%', sep = ''),
                                                    paste(hlth_race[5, 51],'%', sep = '')),
                             `Two or More` = c(paste(hlth_race[6, 6],'%', sep = ''),
                                               paste(hlth_race[6, 12],'%', sep = ''),
                                               paste(hlth_race[6, 18],'%', sep = ''),
                                               paste(hlth_race[6, 24],'%', sep = ''),
                                               paste(hlth_race[6, 30],'%', sep = ''),
                                               paste(hlth_race[6, 36],'%', sep = ''),
                                               paste(hlth_race[6, 42],'%', sep = ''),
                                               paste(hlth_race[6, 51],'%', sep = '')),
                             `White` = c(paste(hlth_race[7, 6],'%', sep = ''),
                                         paste(hlth_race[7, 12],'%', sep = ''),
                                         paste(hlth_race[7, 18],'%', sep = ''),
                                         paste(hlth_race[7, 24],'%', sep = ''),
                                         paste(hlth_race[7, 30],'%', sep = ''),
                                         paste(hlth_race[7, 36],'%', sep = ''),
                                         paste(hlth_race[7, 42],'%', sep = ''),
                                         paste(hlth_race[7, 51],'%', sep = '')),
                             `Unknown` = c(paste(hlth_race[8, 6],'%', sep = ''),
                                           paste(hlth_race[8, 12],'%', sep = ''),
                                           paste(hlth_race[8, 18],'%', sep = ''),
                                           paste(hlth_race[8, 24],'%', sep = ''),
                                           paste(hlth_race[8, 30],'%', sep = ''),
                                           paste(hlth_race[8, 36],'%', sep = ''),
                                           paste(hlth_race[8, 42],'%', sep = ''),
                                           paste(hlth_race[8, 51],'%', sep = '')))



#Creates the drop down menu that shows the graph
hlth_react_race <- reactable(hlth_table_race, resizable = TRUE, showPageSizeOptions = TRUE,
                             onClick = 'expand', highlight = TRUE, rowStyle = list(cursor = 'pointer'),
                             details = function(index) {
                                 if (index == 1) {
                                     htmltools::div(style = "padding: 25px", ggplotly(ins_race))
                                 } else if (index == 2) {
                                     htmltools::div(style = "padding: 25px", ggplotly(no_care_race))
                                 } else if (index == 3) {
                                     htmltools::div(style = "padding: 25px", ggplotly(hlth_pro_race))
                                 } else if (index == 4) {
                                     htmltools::div(style = "padding: 25px", ggplotly(med_check_race))
                                 } else if (index == 5) {
                                     htmltools::div(style = "padding: 25px", ggplotly(dent_check_race))
                                 } else if (index == 6) {
                                     htmltools::div(style = "padding: 25px", ggplotly(mammo_race))
                                 } else if (index == 7) {
                                     htmltools::div(style = "padding: 25px", ggplotly(col_scree_race)) 
                                 } else if (index == 8) {
                                     htmltools::div(style = "padding: 25px", ggplotly(fluvac_race))
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
#Poverty
risk_table_pov <- bind_cols(`Access to Care Indicators by Poverty` = c('Rate of Births among Adolescents by Poverty',
                                                                       'Percent of Adults Consuming the Recommended Amount of Fruit by Poverty',
                                                                       'Percent of Adults Consuming the Recommended Amount of Vegetables by Poverty',
                                                                       'Percent of Adults Getting the Recommended Amount of Aerobic Physical Activity and Muscle Strengthening by Poverty',
                                                                       'Percent of Adults Currently Smoking by Poverty',
                                                                       'Percent of Percent of Adults Currently Using E-cigarettes by Poverty',
                                                                       'Percent of Adults Currently at Risk for Binge Drinking by Poverty',
                                                                       'Percent of Adults Who Always or Nearly Always Wear a Seat Belt by Poverty'),
                            `Below` = c(paste(first(risk_pov$adol_brths),'%', sep = ''),
                                        paste(first(risk_pov$fruit_cr),'%', sep = ''),
                                        paste(first(risk_pov$veg_cr),'%', sep = ''),
                                        paste(first(risk_pov$phy_act_cr),'%', sep = ''),
                                        paste(first(risk_pov$cig_smkng_cr),'%', sep = ''),
                                        paste(first(risk_pov$ecig_cr),'%', sep = ''),
                                        paste(first(risk_pov$bnge_drnkng_cr),'%', sep = ''),
                                        paste(first(risk_pov$st_blt_cr),'%', sep = '')),
                            `Above` = c(paste(last(risk_pov$adol_birth_cr),'%', sep = ''),
                                        paste(last(risk_pov$fruit_cr),'%', sep = ''),
                                        paste(last(risk_pov$veg_pro_cr),'%', sep = ''),
                                        paste(last(risk_pov$phy_act_cr),'%', sep = ''),
                                        paste(last(risk_pov$cig_smkng_cr),'%', sep = ''),
                                        paste(last(risk_pov$ecig_cr),'%', sep = ''),
                                        paste(last(risk_pov$bnge_drnkng_cr),'%', sep = ''),
                                        paste(last(risk_pov$st_blt_cr),'%', sep = '')))


#Creates the drop down menu that shows the graph
risk_react_pov <- reactable(risk_table_pov, resizable = TRUE, showPageSizeOptions = TRUE,
                            onClick = 'expand', highlight = TRUE, rowStyle = list(cursor = 'pointer'),
                            details = function(index) {
                                if (index == 1) {
                                    htmltools::div(style = "padding: 25px", ggplotly(adol_brths_pov))
                                } else if (index == 2) {
                                    htmltools::div(style = "padding: 25px", ggplotly(fruit_pov))
                                } else if (index == 3) {
                                    htmltools::div(style = "padding: 25px", ggplotly(veg_pov))
                                } else if (index == 4) {
                                    htmltools::div(style = "padding: 25px", ggplotly(phy_act_pov))
                                } else if (index == 5) {
                                    htmltools::div(style = "padding: 25px", ggplotly(cig_smkng_pov))
                                } else if (index == 6) {
                                    htmltools::div(style = "padding: 25px", ggplotly(ecig_pov))
                                } else if (index == 7) {
                                    htmltools::div(style = "padding: 25px", ggplotly(bnge_drnkng_pov))
                                } else if (index == 8) {
                                    htmltools::div(style = "padding: 25px", ggplotly(st_blt_pov))
                                }  
                            })
#Race
risk_table_race <- bind_cols(`Access to Care Indicators by Race/Ethnicity` = c('Rate of Births among Adolescents by Race/Ethnicity',
                                                                               'Percent of Adults Consuming the Recommended Amount of Fruit by Race/Ethnicity',
                                                                               'Percent of Adults Consuming the Recommended Amount of Vegetables by Race/Ethnicity',
                                                                               'Percent of Adults Getting the Recommended Amount of Aerobic Physical Activity and Muscle Strengthening by Race/Ethnicity',
                                                                               'Percent of Adults Currently Smoking by Race/Ethnicity',
                                                                               'Percent of Percent of Adults Currently Using E-cigarettes by Race/Ethnicity',
                                                                               'Percent of Adults Currently at Risk for Binge Drinking by Race/Ethnicity',
                                                                               'Percent of Adults Who Always or Nearly Always Wear a Seat Belt by Race/Ethnicity'),
                             `American Indian` = c(paste(risk_race[1, 4]),
                                                   paste(risk_race[1, 9],'%', sep = ''),
                                                   paste(risk_race[1, 15],'%', sep = ''),
                                                   paste(risk_race[1, 21],'%', sep = ''),
                                                   paste(risk_race[1, 27],'%', sep = ''),
                                                   paste(risk_race[1, 33],'%', sep = ''),
                                                   paste(risk_race[1, 39],'%', sep = ''),
                                                   paste(risk_race[1, 45],'%', sep = '')),
                             `Asian` = c(paste(risk_race[2, 4]),
                                         paste(risk_race[2, 9],'%', sep = ''),
                                         paste(risk_race[2, 15],'%', sep = ''),
                                         paste(risk_race[2, 21],'%', sep = ''),
                                         paste(risk_race[2, 27],'%', sep = ''),
                                         paste(risk_race[2, 33],'%', sep = ''),
                                         paste(risk_race[2, 39],'%', sep = ''),
                                         paste(risk_race[2, 45],'%', sep = '')),
                             `Black` = c(paste(risk_race[3, 4]),
                                         paste(risk_race[3, 9],'%', sep = ''),
                                         paste(risk_race[3, 15],'%', sep = ''),
                                         paste(risk_race[3, 21],'%', sep = ''),
                                         paste(risk_race[3, 27],'%', sep = ''),
                                         paste(risk_race[3, 33],'%', sep = ''),
                                         paste(risk_race[3, 39],'%', sep = ''),
                                         paste(risk_race[3, 45],'%', sep = '')),
                             `Hispanic` = c(paste(risk_race[4, 4]),
                                            paste(risk_race[4, 9],'%', sep = ''),
                                            paste(risk_race[4, 15],'%', sep = ''),
                                            paste(risk_race[4, 21],'%', sep = ''),
                                            paste(risk_race[4, 27],'%', sep = ''),
                                            paste(risk_race[4, 33],'%', sep = ''),
                                            paste(risk_race[4, 39],'%', sep = ''),
                                            paste(risk_race[4, 45],'%', sep = '')),
                             `Pacific Islander` = c(paste(risk_race[5, 4]),
                                                    paste(risk_race[5, 9],'%', sep = ''),
                                                    paste(risk_race[5, 15],'%', sep = ''),
                                                    paste(risk_race[5, 21],'%', sep = ''),
                                                    paste(risk_race[5, 27],'%', sep = ''),
                                                    paste(risk_race[5, 33],'%', sep = ''),
                                                    paste(risk_race[5, 39],'%', sep = ''),
                                                    paste(risk_race[5, 45],'%', sep = '')),
                             `Two or More` = c(paste(risk_race[6, 4]),
                                               paste(risk_race[6, 9],'%', sep = ''),
                                               paste(risk_race[6, 15],'%', sep = ''),
                                               paste(risk_race[6, 21],'%', sep = ''),
                                               paste(risk_race[6, 27],'%', sep = ''),
                                               paste(risk_race[6, 33],'%', sep = ''),
                                               paste(risk_race[6, 39],'%', sep = ''),
                                               paste(risk_race[6, 45],'%', sep = '')),
                             `White` = c(paste(risk_race[7, 4]),
                                         paste(risk_race[7, 9],'%', sep = ''),
                                         paste(risk_race[7, 15],'%', sep = ''),
                                         paste(risk_race[7, 21],'%', sep = ''),
                                         paste(risk_race[7, 27],'%', sep = ''),
                                         paste(risk_race[7, 33],'%', sep = ''),
                                         paste(risk_race[7, 39],'%', sep = ''),
                                         paste(risk_race[7, 45],'%', sep = '')),
                             `Unknown` = c(paste(risk_race[8, 4]),
                                           paste(risk_race[8, 9],'%', sep = ''),
                                           paste(risk_race[8, 15],'%', sep = ''),
                                           paste(risk_race[8, 21],'%', sep = ''),
                                           paste(risk_race[8, 27],'%', sep = ''),
                                           paste(risk_race[8, 33],'%', sep = ''),
                                           paste(risk_race[8, 39],'%', sep = ''),
                                           paste(risk_race[8, 45],'%', sep = '')))

risk_react_race <- reactable(risk_table_race, resizable = TRUE, showPageSizeOptions = TRUE,
                             onClick = 'expand', highlight = TRUE, rowStyle = list(cursor = 'pointer'),
                             details = function(index) {
                                 if (index == 1) {
                                     htmltools::div(style = "padding: 25px", ggplotly(adol_brths_race))
                                 } else if (index == 2) {
                                     htmltools::div(style = "padding: 25px", ggplotly(fruit_race))
                                 } else if (index == 3) {
                                     htmltools::div(style = "padding: 25px", ggplotly(veg_race))
                                 } else if (index == 4) {
                                     htmltools::div(style = "padding: 25px", ggplotly(phy_act_race))
                                 } else if (index == 5) {
                                     htmltools::div(style = "padding: 25px", ggplotly(cig_smkng_race))
                                 } else if (index == 6) {
                                     htmltools::div(style = "padding: 25px", ggplotly(ecig_race))
                                 } else if (index == 7) {
                                     htmltools::div(style = "padding: 25px", ggplotly(bnge_drnkng_race))
                                 } else if (index == 8) {
                                     htmltools::div(style = "padding: 25px", ggplotly(st_blt_race))
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
#Poverty
outcomes_table_pov <- bind_cols(`Access to Care Indicators by Poverty` = c('Percent of Adults Who have Experienced Rape/Attempted Rape',
                                                                           'Percent of Adults Who had <7 days of Poor Physical Health in the Last 30 Days',
                                                                           'Percent of Adults Who had <7 days of Poor Mental Health in the Last 30 Days',
                                                                           'Percent of Adults Who have Diagnosed Arthritis',
                                                                           'Percent of Adults Who Currently have Diagnosed Asthma',
                                                                           'Percent of Adults Who have been Told they have Diabetes',
                                                                           'Percent of Adults Who have been Told they have Pre-diabetes',
                                                                           'Percent of Adults Who have been Told they have Depressive Disorder',
                                                                           'Percent of Adults Who have been Told they have Depressive Disorder'),
                                `Below` = c(paste(first(outcomes_pov$rape_cr),'%', sep = ''),
                                            paste(first(outcomes_pov$phy_hlth_cr),'%', sep = ''),
                                            paste(first(outcomes_pov$ment_hlth_cr),'%', sep = ''),
                                            paste(first(outcomes_pov$arthrits_cr),'%', sep = ''),
                                            paste(first(outcomes_pov$astma_cr),'%', sep = ''),
                                            paste(first(outcomes_pov$diab_cr),'%', sep = ''),
                                            paste(first(outcomes_pov$prediabets_cr),'%', sep = ''),
                                            paste(first(outcomes_pov$dep_cr),'%', sep = ''),
                                            paste(first(outcomes_pov$hd_cr),'%', sep = '')),
                                `Above` = c(paste(last(outcomes_pov$rape_cr),'%', sep = ''),
                                            paste(last(outcomes_pov$phy_hlth_cr),'%', sep = ''),
                                            paste(last(outcomes_pov$ment_hlth_cr),'%', sep = ''),
                                            paste(last(outcomes_pov$arthrits_cr),'%', sep = ''),
                                            paste(last(outcomes_pov$astma_cr),'%', sep = ''),
                                            paste(last(outcomes_pov$diab_cr),'%', sep = ''),
                                            paste(last(outcomes_pov$prediabets_cr),'%', sep = ''),
                                            paste(last(outcomes_pov$dep_cr),'%', sep = ''),
                                            paste(last(outcomes_pov$hd_cr),'%', sep = '')))

outcomes_react_pov <- reactable(outcomes_table_pov, resizable = TRUE, showPageSizeOptions = TRUE,
                                onClick = 'expand', highlight = TRUE, rowStyle = list(cursor = 'pointer'),
                                details = function(index) {
                                    if (index == 1) {
                                        htmltools::div(style = "padding: 25px", ggplotly(rape_pov))
                                    } else if (index == 2) {
                                        htmltools::div(style = "padding: 25px", ggplotly(phy_hlth_pov))
                                    } else if (index == 3) {
                                        htmltools::div(style = "padding: 25px", ggplotly(ment_hlth_pov))
                                    } else if (index == 4) {
                                        htmltools::div(style = "padding: 25px", ggplotly(arthrits_pov))
                                    } else if (index == 5) {
                                        htmltools::div(style = "padding: 25px", ggplotly(astma_pov))
                                    } else if (index == 6) {
                                        htmltools::div(style = "padding: 25px", ggplotly(diab_pov))
                                    } else if (index == 7) {
                                        htmltools::div(style = "padding: 25px", ggplotly(prediabets_pov))
                                    } else if (index == 8) {
                                        htmltools::div(style = "padding: 25px", ggplotly(dep_pov))
                                    } else if (index == 9) {
                                        htmltools::div(style = "padding: 25px", ggplotly(hd_pov))
                                    }
                                    
                                })

#Race
outcomes_table_race <- bind_cols(`Access to Care Indicators by Race/Ethnicity` = c('Infant Mortality Rate per 1,000 births',
                                                                                   'Unintentional Injury Mortality Rates by Race/Ethnicity',
                                                                                   'Motor Vehicle Mortality Rates by Race/Ethnicity',
                                                                                   'Drug Poisoning Mortality Rates by Race/Ethnicity',
                                                                                   'Falls Mortality Rates by Race/Ethnicity',
                                                                                   'Suicide Mortality Rates by Race/Ethnicity',
                                                                                   'Diabetes Mortality Rates by Race/Ethnicity',
                                                                                   'Cerebrovascular Mortality Rates by Race/Ethnicity',
                                                                                   'Cancer Mortality Rates by Race/Ethnicity',
                                                                                   'Lung Cancer Mortality Rates by Race/Ethnicity',
                                                                                   'Breast Cancer Mortality Rates by Race/Ethnicity',
                                                                                   'Colorectal Cancer Mortality Rates by Race/Ethnicity',
                                                                                   'Prostate Cancer Mortality Rates by Race/Ethnicity',
                                                                                   'Skin Cancer Mortality Rates by Race/Ethnicity',
                                                                                   'Alzheimers Mortality Rates by Race/Ethnicity',
                                                                                   'Heart Disease Mortality Rates by Race/Ethnicity',
                                                                                   'Percent of Infants with Low Birth Weight by Race/Ethnicity',
                                                                                   'Percent of Infants Born Preterm by Race/Ethnicity',
                                                                                   'Percent of Adults Who have Experienced Rape/Attempted Rape by Race/Ethnicity',
                                                                                   'Percent of Adults Who had <7 days of Poor Physical Health in the Last 30 Days by Race/Ethnicity',
                                                                                   'Percent of Adults Who had <7 days of Poor Mental Health in the Last 30 Days by Race/Ethnicity',
                                                                                   'Percent of Adults Who have Diagnosed Arthritis by Race/Ethnicity',
                                                                                   'Percent of Adults Who Currently have Diagnosed Asthma by Race/Ethnicity',
                                                                                   'Percent of Adults Who have been Told they have Diabetes by Race/Ethnicity',
                                                                                   'Percent of Adults Who have been Told they have Prediabetes by Race/Ethnicity',
                                                                                   'Percent of Adults Who have been Told they have Depressive Disorder by Race/Ethnicity',
                                                                                   'Percent of Adults Who have been Told they have Heart Disease by Race/Ethnicity'),
                                 `AIAN` = c(paste(outcomes_race[1, 3],'%', sep = ''),
                                            paste(outcomes_race[1, 9],'%', sep = ''),
                                            paste(outcomes_race[1, 15],'%', sep = ''),
                                            paste(outcomes_race[1, 21],'%', sep = ''),
                                            paste(outcomes_race[1, 27],'%', sep = ''),
                                            paste(outcomes_race[1, 33],'%', sep = ''),
                                            paste(outcomes_race[1, 39],'%', sep = ''),
                                            paste(outcomes_race[1, 45],'%', sep = ''),
                                            paste(outcomes_race[1, 51],'%', sep = ''),
                                            paste(outcomes_race[1, 57],'%', sep = ''),
                                            paste(outcomes_race[1, 63],'%', sep = ''),
                                            paste(outcomes_race[1, 69],'%', sep = ''),
                                            paste(outcomes_race[1, 75],'%', sep = ''),
                                            paste(outcomes_race[1, 81],'%', sep = ''),
                                            paste(outcomes_race[1, 84],'%', sep = ''),
                                            paste(outcomes_race[1, 90],'%', sep = ''),
                                            paste(outcomes_race[1, 96],'%', sep = ''),
                                            paste(outcomes_race[1, 99],'%', sep = ''),
                                            paste(outcomes_race[1, 105],'%', sep = ''),
                                            paste(outcomes_race[1, 111],'%', sep = ''),
                                            paste(outcomes_race[1, 117],'%', sep = ''),
                                            paste(outcomes_race[1, 123],'%', sep = ''),
                                            paste(outcomes_race[1, 129],'%', sep = ''),
                                            paste(outcomes_race[1, 135],'%', sep = ''),
                                            paste(outcomes_race[1, 141],'%', sep = ''),
                                            paste(outcomes_race[1, 144],'%', sep = ''),
                                            paste(outcomes_race[1, 150],'%', sep = '')),
                                 `Asian` = c(paste(outcomes_race[2, 3]),
                                             paste(outcomes_race[2, 9],'%', sep = ''),
                                             paste(outcomes_race[2, 15],'%', sep = ''),
                                             paste(outcomes_race[2, 22],'%', sep = ''),
                                             paste(outcomes_race[2, 27],'%', sep = ''),
                                             paste(outcomes_race[2, 33],'%', sep = ''),
                                             paste(outcomes_race[2, 39],'%', sep = ''),
                                             paste(outcomes_race[2, 45],'%', sep = ''),
                                             paste(outcomes_race[2, 51],'%', sep = ''),
                                             paste(outcomes_race[2, 57],'%', sep = ''),
                                             paste(outcomes_race[2, 63],'%', sep = ''),
                                             paste(outcomes_race[2, 69],'%', sep = ''),
                                             paste(outcomes_race[2, 75],'%', sep = ''),
                                             paste(outcomes_race[2, 81],'%', sep = ''),
                                             paste(outcomes_race[2, 84],'%', sep = ''),
                                             paste(outcomes_race[2, 90],'%', sep = ''),
                                             paste(outcomes_race[2, 96],'%', sep = ''),
                                             paste(outcomes_race[2, 99],'%', sep = ''),
                                             paste(outcomes_race[2, 105],'%', sep = ''),
                                             paste(outcomes_race[2, 111],'%', sep = ''),
                                             paste(outcomes_race[2, 117],'%', sep = ''),
                                             paste(outcomes_race[2, 123],'%', sep = ''),
                                             paste(outcomes_race[2, 129],'%', sep = ''),
                                             paste(outcomes_race[2, 135],'%', sep = ''),
                                             paste(outcomes_race[2, 141],'%', sep = ''),
                                             paste(outcomes_race[2, 144],'%', sep = ''),
                                             paste(outcomes_race[2, 150],'%', sep = '')),
                                 `AAB` = c(paste(outcomes_race[3, 3]),
                                           paste(outcomes_race[3, 9],'%', sep = ''),
                                           paste(outcomes_race[3, 15],'%', sep = ''),
                                           paste(outcomes_race[3, 22],'%', sep = ''),
                                           paste(outcomes_race[3, 27],'%', sep = ''),
                                           paste(outcomes_race[3, 33],'%', sep = ''),
                                           paste(outcomes_race[3, 39],'%', sep = ''),
                                           paste(outcomes_race[3, 45],'%', sep = ''),
                                           paste(outcomes_race[3, 51],'%', sep = ''),
                                           paste(outcomes_race[3, 57],'%', sep = ''),
                                           paste(outcomes_race[3, 63],'%', sep = ''),
                                           paste(outcomes_race[3, 69],'%', sep = ''),
                                           paste(outcomes_race[3, 75],'%', sep = ''),
                                           paste(outcomes_race[3, 81],'%', sep = ''),
                                           paste(outcomes_race[3, 84],'%', sep = ''),
                                           paste(outcomes_race[3, 90],'%', sep = ''),
                                           paste(outcomes_race[3, 96],'%', sep = ''),
                                           paste(outcomes_race[3, 99],'%', sep = ''),
                                           paste(outcomes_race[3, 105],'%', sep = ''),
                                           paste(outcomes_race[3, 111],'%', sep = ''),
                                           paste(outcomes_race[3, 117],'%', sep = ''),
                                           paste(outcomes_race[3, 123],'%', sep = ''),
                                           paste(outcomes_race[3, 129],'%', sep = ''),
                                           paste(outcomes_race[3, 135],'%', sep = ''),
                                           paste(outcomes_race[3, 141],'%', sep = ''),
                                           paste(outcomes_race[3, 144],'%', sep = ''),
                                           paste(outcomes_race[3, 150],'%', sep = '')),
                                 `HisLat` = c(paste(outcomes_race[4, 3]),
                                              paste(outcomes_race[4, 9],'%', sep = ''),
                                              paste(outcomes_race[4, 15],'%', sep = ''),
                                              paste(outcomes_race[4, 22],'%', sep = ''),
                                              paste(outcomes_race[4, 27],'%', sep = ''),
                                              paste(outcomes_race[4, 33],'%', sep = ''),
                                              paste(outcomes_race[4, 39],'%', sep = ''),
                                              paste(outcomes_race[4, 45],'%', sep = ''),
                                              paste(outcomes_race[4, 51],'%', sep = ''),
                                              paste(outcomes_race[4, 57],'%', sep = ''),
                                              paste(outcomes_race[4, 63],'%', sep = ''),
                                              paste(outcomes_race[4, 69],'%', sep = ''),
                                              paste(outcomes_race[4, 75],'%', sep = ''),
                                              paste(outcomes_race[4, 81],'%', sep = ''),
                                              paste(outcomes_race[4, 84],'%', sep = ''),
                                              paste(outcomes_race[4, 90],'%', sep = ''),
                                              paste(outcomes_race[4, 96],'%', sep = ''),
                                              paste(outcomes_race[4, 99],'%', sep = ''),
                                              paste(outcomes_race[4, 105],'%', sep = ''),
                                              paste(outcomes_race[4, 111],'%', sep = ''),
                                              paste(outcomes_race[4, 117],'%', sep = ''),
                                              paste(outcomes_race[4, 123],'%', sep = ''),
                                              paste(outcomes_race[4, 129],'%', sep = ''),
                                              paste(outcomes_race[4, 135],'%', sep = ''),
                                              paste(outcomes_race[4, 141],'%', sep = ''),
                                              paste(outcomes_race[4, 144],'%', sep = ''),
                                              paste(outcomes_race[4, 150],'%', sep = '')),
                                 `PINH` = c(paste(outcomes_race[5, 3]),
                                            paste(outcomes_race[5, 9],'%', sep = ''),
                                            paste(outcomes_race[5, 15],'%', sep = ''),
                                            paste(outcomes_race[5, 22],'%', sep = ''),
                                            paste(outcomes_race[5, 27],'%', sep = ''),
                                            paste(outcomes_race[5, 33],'%', sep = ''),
                                            paste(outcomes_race[5, 39],'%', sep = ''),
                                            paste(outcomes_race[5, 45],'%', sep = ''),
                                            paste(outcomes_race[5, 51],'%', sep = ''),
                                            paste(outcomes_race[5, 57],'%', sep = ''),
                                            paste(outcomes_race[5, 63],'%', sep = ''),
                                            paste(outcomes_race[5, 69],'%', sep = ''),
                                            paste(outcomes_race[5, 75],'%', sep = ''),
                                            paste(outcomes_race[5, 81],'%', sep = ''),
                                            paste(outcomes_race[5, 84],'%', sep = ''),
                                            paste(outcomes_race[5, 90],'%', sep = ''),
                                            paste(outcomes_race[5, 96],'%', sep = ''),
                                            paste(outcomes_race[5, 99],'%', sep = ''),
                                            paste(outcomes_race[5, 105],'%', sep = ''),
                                            paste(outcomes_race[5, 111],'%', sep = ''),
                                            paste(outcomes_race[5, 117],'%', sep = ''),
                                            paste(outcomes_race[5, 123],'%', sep = ''),
                                            paste(outcomes_race[5, 129],'%', sep = ''),
                                            paste(outcomes_race[5, 135],'%', sep = ''),
                                            paste(outcomes_race[5, 141],'%', sep = ''),
                                            paste(outcomes_race[5, 144],'%', sep = ''),
                                            paste(outcomes_race[5, 150],'%', sep = '')),
                                 `Two` = c(paste(outcomes_race[6, 3]),
                                           paste(outcomes_race[6, 9],'%', sep = ''),
                                           paste(outcomes_race[6, 15],'%', sep = ''),
                                           paste(outcomes_race[6, 22],'%', sep = ''),
                                           paste(outcomes_race[6, 27],'%', sep = ''),
                                           paste(outcomes_race[6, 33],'%', sep = ''),
                                           paste(outcomes_race[6, 39],'%', sep = ''),
                                           paste(outcomes_race[6, 45],'%', sep = ''),
                                           paste(outcomes_race[6, 51],'%', sep = ''),
                                           paste(outcomes_race[6, 57],'%', sep = ''),
                                           paste(outcomes_race[6, 63],'%', sep = ''),
                                           paste(outcomes_race[6, 69],'%', sep = ''),
                                           paste(outcomes_race[6, 75],'%', sep = ''),
                                           paste(outcomes_race[6, 81],'%', sep = ''),
                                           paste(outcomes_race[6, 84],'%', sep = ''),
                                           paste(outcomes_race[6, 90],'%', sep = ''),
                                           paste(outcomes_race[6, 96],'%', sep = ''),
                                           paste(outcomes_race[6, 99],'%', sep = ''),
                                           paste(outcomes_race[6, 105],'%', sep = ''),
                                           paste(outcomes_race[6, 111],'%', sep = ''),
                                           paste(outcomes_race[6, 117],'%', sep = ''),
                                           paste(outcomes_race[6, 123],'%', sep = ''),
                                           paste(outcomes_race[6, 129],'%', sep = ''),
                                           paste(outcomes_race[6, 135],'%', sep = ''),
                                           paste(outcomes_race[6, 141],'%', sep = ''),
                                           paste(outcomes_race[6, 144],'%', sep = ''),
                                           paste(outcomes_race[6, 150],'%', sep = '')),
                                 `White` = c(paste(outcomes_race[7, 3]),
                                             paste(outcomes_race[7, 9],'%', sep = ''),
                                             paste(outcomes_race[7, 15],'%', sep = ''),
                                             paste(outcomes_race[7, 22],'%', sep = ''),
                                             paste(outcomes_race[7, 27],'%', sep = ''),
                                             paste(outcomes_race[7, 33],'%', sep = ''),
                                             paste(outcomes_race[7, 39],'%', sep = ''),
                                             paste(outcomes_race[7, 45],'%', sep = ''),
                                             paste(outcomes_race[7, 51],'%', sep = ''),
                                             paste(outcomes_race[7, 57],'%', sep = ''),
                                             paste(outcomes_race[7, 63],'%', sep = ''),
                                             paste(outcomes_race[7, 69],'%', sep = ''),
                                             paste(outcomes_race[7, 75],'%', sep = ''),
                                             paste(outcomes_race[7, 81],'%', sep = ''),
                                             paste(outcomes_race[7, 84],'%', sep = ''),
                                             paste(outcomes_race[7, 90],'%', sep = ''),
                                             paste(outcomes_race[7, 96],'%', sep = ''),
                                             paste(outcomes_race[7, 99],'%', sep = ''),
                                             paste(outcomes_race[7, 105],'%', sep = ''),
                                             paste(outcomes_race[7, 111],'%', sep = ''),
                                             paste(outcomes_race[7, 117],'%', sep = ''),
                                             paste(outcomes_race[7, 123],'%', sep = ''),
                                             paste(outcomes_race[7, 129],'%', sep = ''),
                                             paste(outcomes_race[7, 135],'%', sep = ''),
                                             paste(outcomes_race[7, 141],'%', sep = ''),
                                             paste(outcomes_race[7, 144],'%', sep = ''),
                                             paste(outcomes_race[7, 150],'%', sep = '')),
                                 `Unknown` = c(paste(outcomes_race[8, 3]),
                                               paste(outcomes_race[8, 9],'%', sep = ''),
                                               paste(outcomes_race[8, 15],'%', sep = ''),
                                               paste(outcomes_race[8, 22],'%', sep = ''),
                                               paste(outcomes_race[8, 27],'%', sep = ''),
                                               paste(outcomes_race[8, 33],'%', sep = ''),
                                               paste(outcomes_race[8, 39],'%', sep = ''),
                                               paste(outcomes_race[8, 45],'%', sep = ''),
                                               paste(outcomes_race[8, 51],'%', sep = ''),
                                               paste(outcomes_race[8, 57],'%', sep = ''),
                                               paste(outcomes_race[8, 63],'%', sep = ''),
                                               paste(outcomes_race[8, 69],'%', sep = ''),
                                               paste(outcomes_race[8, 75],'%', sep = ''),
                                               paste(outcomes_race[8, 81],'%', sep = ''),
                                               paste(outcomes_race[8, 84],'%', sep = ''),
                                               paste(outcomes_race[8, 90],'%', sep = ''),
                                               paste(outcomes_race[8, 96],'%', sep = ''),
                                               paste(outcomes_race[8, 99],'%', sep = ''),
                                               paste(outcomes_race[8, 105],'%', sep = ''),
                                               paste(outcomes_race[8, 111],'%', sep = ''),
                                               paste(outcomes_race[8, 117],'%', sep = ''),
                                               paste(outcomes_race[8, 123],'%', sep = ''),
                                               paste(outcomes_race[8, 129],'%', sep = ''),
                                               paste(outcomes_race[8, 135],'%', sep = ''),
                                               paste(outcomes_race[8, 141],'%', sep = ''),
                                               paste(outcomes_race[8, 144],'%', sep = ''),
                                               paste(outcomes_race[8, 150],'%', sep = '')))




outcomes_react_race <- reactable(outcomes_table_race, resizable = TRUE, showPageSizeOptions = TRUE,
                                 onClick = 'expand', highlight = TRUE, rowStyle = list(cursor = 'pointer'),
                                 details = function(index) {
                                     if (index == 1) {
                                         htmltools::div(style = "padding: 25px", ggplotly(inf_mort_rate_race))
                                     } else if (index == 2) {
                                         htmltools::div(style = "padding: 25px", ggplotly(unint_inj_race))
                                     } else if (index == 3) {
                                         htmltools::div(style = "padding: 25px", ggplotly(mvc_race))
                                     } else if (index == 4) {
                                         htmltools::div(style = "padding: 25px", ggplotly(drug_poi_race))
                                     } else if (index == 5) {
                                         htmltools::div(style = "padding: 25px", ggplotly(falls_race))
                                     } else if (index == 6) {
                                         htmltools::div(style = "padding: 25px", ggplotly(sui_race))
                                     } else if (index == 7) {
                                         htmltools::div(style = "padding: 25px", ggplotly(mort_diab_race))
                                     } else if (index == 8) {
                                         htmltools::div(style = "padding: 25px", ggplotly(mort_cvs_race))
                                     } else if (index == 9) {
                                         htmltools::div(style = "padding: 25px", ggplotly(mort_can_race))
                                     } else if (index == 10) {
                                         htmltools::div(style = "padding: 25px", ggplotly(mort_lung_can_race))
                                     } else if (index == 11) {
                                         htmltools::div(style = "padding: 25px", ggplotly(mort_brst_can_race))
                                     } else if (index == 12) {
                                         htmltools::div(style = "padding: 25px", ggplotly(mort_colrect_can_race))
                                     } else if (index == 13) {
                                         htmltools::div(style = "padding: 25px", ggplotly(mort_pros_can_race))
                                     } else if (index == 14) {
                                         htmltools::div(style = "padding: 25px", ggplotly(mort_skin_can_race))
                                     } else if (index == 15) {
                                         htmltools::div(style = "padding: 25px", ggplotly(mort_alz_race))
                                     } else if (index == 16) {
                                         htmltools::div(style = "padding: 25px", ggplotly(mort_hd_race))
                                     } else if (index == 17) {
                                         htmltools::div(style = "padding: 25px", ggplotly(lw_brth_inf_race))
                                     } else if (index == 18) {
                                         htmltools::div(style = "padding: 25px", ggplotly(preterm_births_race))
                                     } else if (index == 19) {
                                         htmltools::div(style = "padding: 25px", ggplotly(rape_race))
                                     } else if (index == 20) {
                                         htmltools::div(style = "padding: 25px", ggplotly(phy_hlth_race))
                                     } else if (index == 21) {
                                         htmltools::div(style = "padding: 25px", ggplotly(ment_hlth_race))
                                     } else if (index == 22) {
                                         htmltools::div(style = "padding: 25px", ggplotly(arthrits_race))
                                     } else if (index == 23) {
                                         htmltools::div(style = "padding: 25px", ggplotly(astma_race))
                                     } else if (index == 24) {
                                         htmltools::div(style = "padding: 25px", ggplotly(diab_race))
                                     } else if (index == 25) {
                                         htmltools::div(style = "padding: 25px", ggplotly(prediabets_race))
                                     } else if (index == 26) {
                                         htmltools::div(style = "padding: 25px", ggplotly(dep_race))
                                     } else if (index == 27) {
                                         htmltools::div(style = "padding: 25px", ggplotly(hd_race))
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
                            tabPanel('Crude Rates & Percentages', reactableOutput('hlth_react')),
                            tabPanel('Poverty', reactableOutput('hlth_react_pov')),
                            tabPanel('Race/Ethnicity', reactableOutput('hlth_react_race')),
                            
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
                            tabPanel('Poverty', reactableOutput('risk_react_pov')
                            ),
                            tabPanel('Race/Ethnicity', reactableOutput('risk_react_race')
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
                            tabPanel('Poverty', reactableOutput('outcomes_react_pov')
                            ),
                            tabPanel('Race/Ethnicity', reactableOutput('outcomes_react_race')
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
    
    output$hlth_react_pov <- renderReactable({
        hlth_react_pov
    })
    
    output$hlth_react_race <- renderReactable({
        hlth_react_race
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
    
    output$risk_react_pov <- renderReactable({
        risk_react_pov
    })
    
    output$outcomes_react_pov <- renderReactable({
        outcomes_react_pov
    })
    output$risk_react_race <- renderReactable({
        risk_react_race
    })
    
    output$outcomes_react_race <- renderReactable({
        outcomes_react_race
    })
    
}

shinyApp(ui, server)