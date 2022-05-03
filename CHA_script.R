setwd("C:/Users/tstoker/Desktop/RWD")
rm(list=ls())
library(tidyr)
library(dplyr)
library(plotly)
library(reactable)
library(formattable)
library(data.table)
library(lubridate)
library(ggplot2)

#Testing GITHUB 2

dem <- read.csv("dem.csv", header = T)
hlth <- read.csv("healthcare.csv", header = T)
risk <- read.csv("risk.csv", header = T)
outcomes <- read.csv("outcomes.csv", header = T)

hlth_pov <- read.csv("healthcare_pov.csv", header = T)
risk_pov <- read.csv("risk_pov.csv", header = T)
outcomes_pov <- read.csv("outcomes_pov.csv", header = T)

hlth_race <- read.csv("healthcare_race.csv", header = T)
risk_race <- read.csv("risk_race.csv", header = T)
outcomes_race <- read.csv("outcomes_race.csv", header = T)


#Color List
BRHD_cols <- list(rgb(0, 141, 168, maxColorValue = 255), rgb(241, 227, 197, maxColorValue = 255),
                  rgb(212, 69, 29, maxColorValue = 255), rgb(102, 51, 52, maxColorValue = 255),
                  rgb(255, 206, 113, maxColorValue = 255), rgb(109, 39, 106, maxColorValue = 255),
                  rgb(231, 65, 122, maxColorValue = 255), rgb(9,121,105, maxColorValue = 255), rgb(0,0,0, maxColorValue = 255))

####################
# Demographic Info #
####################
dem <- dem %>%
  mutate(ethn_non_hisp_perc = round(100*ethn_non_hisp/tot_pop,2),
         ethn_hisp_perc = round(100*ethn_hisp/tot_pop,2),
         race_wh_perc = round(100*race_wh/tot_pop,2),
         race_afr_amer_perc = round(100*race_afr_amer/tot_pop,2),
         race_ai_an_perc = round(100*race_ai_an/tot_pop,2),
         race_asi_perc = round(100*race_asi/tot_pop,2),
         race_nhw_pi_perc = round(100*race_nhw_pi/tot_pop,2),
         race_2ormore_perc = round(100*race_2ormore/tot_pop,2),
         nonwhite_perc = 100-race_wh_perc)


#Get newest Data
dem_new <- dem %>% 
  filter(Year == max(Year))

#Pivot wider...kind of
br_dem <- dem %>% 
  filter(Region == 'BRHD') %>% 
  dplyr::select(-Region) %>% 
  dplyr::rename_with(function(x) paste0("br_", x), -Year)

be_dem <- dem %>% 
  filter(Region == 'Box Elder') %>% 
  dplyr::select(-Region) %>% 
  dplyr::rename_with(function(x) paste0("be_", x), -Year)

ca_dem <- dem %>% 
  filter(Region == 'Cache') %>% 
  dplyr::select(-Region) %>% 
  dplyr::rename_with(function(x) paste0("ca_", x), -Year)

ri_dem <- dem %>% 
  filter(Region == 'Rich') %>% 
  dplyr::select(-Region) %>% 
  dplyr::rename_with(function(x) paste0("ri_", x), -Year)

dem_wide <- merge(br_dem, be_dem, by = 'Year')
dem_wide <- merge(dem_wide, ca_dem, by = 'Year')
dem_wide <- merge(dem_wide, ri_dem, by = 'Year')


#Reformat data types
dem_wide[,2:ncol(dem_wide)] <- as.numeric(as.matrix(dem_wide[,2:ncol(dem_wide)]))
dem_wide$Year <- as.integer(dem_wide$Year)


####################
# Health Care Data #
####################
#treated it as a numeric rather than an integer for year
hlth$Year <- as.integer(hlth$Year)

#from column 3 to the end make into a percentage
hlth[3:ncol(hlth)] <- hlth[3:ncol(hlth)] %>% 
  mutate_if(is.numeric, ~ . * 100)

hlth_race[3:ncol(hlth_race)] <- hlth_race[3:ncol(hlth_race)] %>% 
  mutate_if(is.numeric, ~ . * 100)

hlth_pov[3:ncol(hlth_pov)] <- hlth_pov[3:ncol(hlth_pov)] %>% 
  mutate_if(is.numeric, ~ . * 100)

#Pivot wider...kind of- Create new data set that is the data just for BRHD and take of region variable and rename each variable br_hlth
br_hlth <- hlth %>% 
  filter(Region == 'BRHD') %>% 
  dplyr::select(-Region) %>% 
  dplyr::rename_with(function(x) paste0("br_", x), -Year)

be_hlth <- hlth %>% 
  filter(Region == 'Box Elder') %>% 
  dplyr::select(-Region) %>% 
  dplyr::rename_with(function(x) paste0("be_", x), -Year)

ca_hlth <- hlth %>% 
  filter(Region == 'Cache') %>% 
  dplyr::select(-Region) %>% 
  dplyr::rename_with(function(x) paste0("ca_", x), -Year)

ri_hlth <- hlth %>% 
  filter(Region == 'Rich') %>% 
  dplyr::select(-Region) %>% 
  dplyr::rename_with(function(x) paste0("ri_", x), -Year)
#merged br with be and so on
hlth_wide <- merge(br_hlth, be_hlth, by = 'Year')
hlth_wide <- merge(hlth_wide, ca_hlth, by = 'Year')
hlth_wide <- merge(hlth_wide, ri_hlth, by = 'Year')

####################
# Risk Data #
####################
#treated it as a numeric rather than an integer for year this works for risk( check it)
#risk$Year <- as.integer(risk$Year)

#**from column 3 to the end make into a percentage. Figure out how to make the percentages into percents and the rates leave as is** Only multiply certain col
risk[6:ncol(risk)] <- risk[6:ncol(risk)] %>% 
  mutate_if(is.numeric, ~ . * 100)

risk_pov[6:ncol(risk_pov)] <- risk_pov[6:ncol(risk_pov)] %>% 
  mutate_if(is.numeric, ~ . * 100)

risk_race[6:ncol(risk_race)] <- risk_race[6:ncol(risk_race)] %>% 
  mutate_if(is.numeric, ~ . * 100)

#Pivot wider...kind of- Create new data set that is the data just for BRHD and take of region variable and rename each variable br_hlth
br_risk <- risk %>% 
  filter(Region == 'BRHD') %>% 
  dplyr::select(-Region) %>% 
  dplyr::rename_with(function(x) paste0("br_", x), -Year)

be_risk <- risk %>% 
  filter(Region == 'Box Elder') %>% 
  dplyr::select(-Region) %>% 
  dplyr::rename_with(function(x) paste0("be_", x), -Year)

ca_risk <- risk %>% 
  filter(Region == 'Cache') %>% 
  dplyr::select(-Region) %>% 
  dplyr::rename_with(function(x) paste0("ca_", x), -Year)

#ri_risk <- risk %>% 
#filter(Region == 'Rich') %>% 
#dplyr::select(-Region) %>% 
#dplyr::rename_with(function(x) paste0("ri_", x), -Year)

#merged br with be and so on
risk_wide <- merge(br_risk, be_risk, by = 'Year')
risk_wide <- merge(risk_wide, ca_risk, by = 'Year')
#risk_wide <- merge(risk_wide, ri_risk, by = 'Year')

####################
# Outcomes Data #
####################
#treated it as a numeric rather than an integer for year this works for risk( check it)
#outcomes$Year <- as.integer(outcomes$Year)

#**from column 3 to the end make into a percentage. Figure out how to make the percentages into percents and the rates leave as is** Only multiply certain col
outcomes[96:ncol(outcomes)] <- outcomes[96:ncol(outcomes)] %>% 
  mutate_if(is.numeric, ~ . * 100)

outcomes_pov[96:ncol(outcomes_pov)] <- outcomes_pov[96:ncol(outcomes_pov)] %>% 
  mutate_if(is.numeric, ~ . * 100)

outcomes_race[96:ncol(outcomes_race)] <- outcomes_race[96:ncol(outcomes_race)] %>% 
  mutate_if(is.numeric, ~ . * 100)

#Pivot wider...kind of- Create new data set that is the data just for BRHD and take of region variable and rename each variable br_hlth
br_outcomes <- outcomes %>% 
  filter(Region == 'BRHD') %>% 
  dplyr::select(-Region) %>% 
  dplyr::rename_with(function(x) paste0("br_", x), -Year)

be_outcomes <- outcomes %>% 
  filter(Region == 'Box Elder') %>% 
  dplyr::select(-Region) %>% 
  dplyr::rename_with(function(x) paste0("be_", x), -Year)

ca_outcomes <- outcomes %>% 
  filter(Region == 'Cache') %>% 
  dplyr::select(-Region) %>% 
  dplyr::rename_with(function(x) paste0("ca_", x), -Year)

#ri_outcomes <- outcomes %>% 
#filter(Region == 'Rich') %>% 
#dplyr::select(-Region) %>% 
#dplyr::rename_with(function(x) paste0("ri_", x), -Year)

#merged br with be and so on
outcomes_wide <- merge(br_outcomes, be_outcomes, by = 'Year')
outcomes_wide <- merge(outcomes_wide, ca_outcomes, by = 'Year')
#outcomes_wide <- merge(outcomes_wide, ri_outcomes, by = 'Year')


#########
# Plots #
#########

###Demographic###

#Cache
cr_dr <- plot_ly(dem_wide, x=~Year, y=~ca_death_rate_cr, type = 'scatter', mode = "lines", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
cr_dr <- cr_dr %>% add_trace(y = ~ca_death_rate_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
cr_dr <- cr_dr %>% add_trace(y = ~ca_death_rate_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
cr_dr <- cr_dr %>% add_trace(y = ~be_death_rate_cr, type = 'scatter', mode = "lines", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
cr_dr <- cr_dr %>% add_trace(y = ~be_death_rate_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
cr_dr <- cr_dr %>% add_trace(y = ~be_death_rate_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
cr_dr <- cr_dr %>% add_trace(y = ~br_death_rate_cr, type = 'scatter', mode = "lines", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
cr_dr <- cr_dr %>% add_trace(y = ~br_death_rate_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
cr_dr <- cr_dr %>% add_trace(y = ~br_death_rate_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
cr_dr <- cr_dr %>% layout(title = 'Death Rates by County',
                          xaxis = list(title = 'Year'),
                          yaxis = list(title = 'Death Rate (per 100,000'))



### AA Death Rate
#Cache
aa_dr <- plot_ly(dem_wide, x=~Year, y=~ca_death_rate_aar, type = 'scatter', mode = "lines", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
aa_dr <- aa_dr %>% add_trace(y = ~ca_death_rate_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
aa_dr <- aa_dr %>% add_trace(y = ~ca_death_rate_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
aa_dr <- aa_dr %>% add_trace(y = ~be_death_rate_aar, type = 'scatter', mode = "lines", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
aa_dr <- aa_dr %>% add_trace(y = ~be_death_rate_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
aa_dr <- aa_dr %>% add_trace(y = ~be_death_rate_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
aa_dr <- aa_dr %>% add_trace(y = ~br_death_rate_aar, type = 'scatter', mode = "lines", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
aa_dr <- aa_dr %>% add_trace(y = ~br_death_rate_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
aa_dr <- aa_dr %>% add_trace(y = ~br_death_rate_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
aa_dr <- aa_dr %>% layout(title = 'Age-Adjusted Death Rates by County',
                          xaxis = list(title = 'Year'),
                          yaxis = list(title = 'Death Rate (per 100,000)'))


### Birth Rate
#Cache
birth_rate <- plot_ly(dem_wide, x = ~Year, y = ~ca_birth_rate, type = 'scatter', mode = "lines", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
birth_rate <- birth_rate %>% add_trace(y = ~ca_birth_rate_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
birth_rate <- birth_rate %>% add_trace(y = ~ca_birth_rate_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
birth_rate <- birth_rate %>% add_trace(y = ~be_birth_rate, type = 'scatter', mode = "lines", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
birth_rate <- birth_rate %>% add_trace(y = ~be_birth_rate_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
birth_rate <- birth_rate %>% add_trace(y = ~be_birth_rate_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
birth_rate <- birth_rate %>% add_trace(y = ~br_birth_rate, type = 'scatter', mode = "lines", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
birth_rate <- birth_rate %>% add_trace(y = ~br_birth_rate_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
birth_rate <- birth_rate %>% add_trace(y = ~br_birth_rate_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
birth_rate <- birth_rate %>% layout(title = 'Birth Rates by County',
                                    xaxis = list(title = 'Year'),
                                    yaxis = list(title = 'Birth Rate (per 1,000)'))


#Population Chart
pop <- plot_ly(dem_wide, x = ~Year, y = ~ca_tot_pop, type = 'scatter', mode = "lines", name = 'Cache', line = list(color = BRHD_cols[[1]]))
pop <- pop %>% add_trace(y = ~be_tot_pop, name = 'Box Elder', line = list(color = BRHD_cols[[3]]))
pop <- pop %>% add_trace(y = ~br_tot_pop, name = 'BRHD', line = list(color = BRHD_cols[[4]]))
pop <- pop %>% add_trace(y = ~ri_tot_pop, name = 'Rich', line = list(color = BRHD_cols[[6]]))
pop <- pop %>% layout(title = 'Total Population by County',
                      xaxis = list(title = "Year"),
                      yaxis = list (title = "Population"))


#Ethnic pop percent BRHD
ethn <- plot_ly(dem_new, x = ~Region, y = ~ethn_non_hisp_perc, type = "bar", name = 'Non-Hispanic', marker = list(color = BRHD_cols[[1]]))
ethn <- ethn %>% add_trace(y = ~ethn_hisp_perc, name = 'Hispanic', marker = list(color = BRHD_cols[[3]]))
ethn <- ethn %>% layout(title = 'Proportions of Counties by Ethnicity, 2020',
                        xaxis = list(title = "Region"),
                        yaxis = list (title = "Percent of Population"),
                        barmode = 'stack')

#Race pop percent 
race <- plot_ly(dem_new, x = ~Region, y = ~race_wh_perc, type = 'bar', name = 'White', marker = list(color = BRHD_cols[[1]]))
race <- race %>% add_trace(y = ~race_afr_amer_perc, name = 'Black/African-American', marker = list(color = BRHD_cols[[2]]))
race <- race %>% add_trace(y = ~race_ai_an_perc, name = 'American Indian/Alaskan Native', marker = list(color = BRHD_cols[[3]]))
race <- race %>% add_trace(y = ~race_asi_perc, name = 'Asian', marker = list(color = BRHD_cols[[4]]))
race <- race %>% add_trace(y = ~race_nhw_pi_perc, name = 'Native Hawaiian/Pacific Islander', marker = list(color = BRHD_cols[[5]]))
race <- race %>% add_trace(y = ~race_2ormore_perc, name = 'Two or More Races', marker = list(color = BRHD_cols[[6]]))
race <- race %>% layout(title = 'Proportions of Counties by Race, 2020',
                        xaxis = list(title = "Region"),
                        yaxis = list (title = "Percent of Population"),
                        barmode = 'stack')

###Healthcare###

###Insured
#Cache
ins <- plot_ly(hlth_wide, x=~Year, y=~ca_hlcov_cr, type = 'scatter', mode = "lines", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
ins <- ins %>% add_trace(y = ~ca_hlcov_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
ins <- ins %>% add_trace(y = ~ca_hlcov_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
ins <- ins %>% add_trace(y = ~be_hlcov_cr, type = 'scatter', mode = "lines", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
ins <- ins %>% add_trace(y = ~be_hlcov_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
ins <- ins %>% add_trace(y = ~be_hlcov_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
ins <- ins %>% add_trace(y = ~br_hlcov_cr, type = 'scatter', mode = "lines", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
ins <- ins %>% add_trace(y = ~br_hlcov_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
ins <- ins %>% add_trace(y = ~br_hlcov_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout: Adds titles and labels
ins <- ins %>% layout(title = 'Percent of Adults with Health Insurance',
                      xaxis = list(title = 'Year'),
                      yaxis = list(title = 'Percent'))

###AA Insured
#Cache
aa_ins <- plot_ly(hlth_wide, x=~Year, y=~ca_hlcov_aar, type = 'scatter', mode = "lines", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
aa_ins <- aa_ins %>% add_trace(y = ~ca_hlcov_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
aa_ins <- aa_ins %>% add_trace(y = ~ca_hlcov_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
aa_ins <- aa_ins %>% add_trace(y = ~be_hlcov_aar, type = 'scatter', mode = "lines", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
aa_ins <- aa_ins %>% add_trace(y = ~be_hlcov_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
aa_ins <- aa_ins %>% add_trace(y = ~be_hlcov_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
aa_ins <- aa_ins %>% add_trace(y = ~br_hlcov_aar, type = 'scatter', mode = "lines", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
aa_ins <- aa_ins %>% add_trace(y = ~br_hlcov_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
aa_ins <- aa_ins %>% add_trace(y = ~br_hlcov_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
aa_ins <- aa_ins %>% layout(title = 'Age-Adjusted Percent of Adults with Health Insurance',
                            xaxis = list(title = 'Year'),
                            yaxis = list(title = 'Percent'))

#Poverty
ins_pov <- ggplot(hlth_pov, aes(Poverty_Status,hlcov_cr, fill = Poverty_Status)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = hlcov_cr_lci, ymax = hlcov_cr_uci), width = 0.2) +
  labs(title = "Percent of Adults with Health Insurance by Poverty", x="Poverty Status", y="Percent") +
  theme_classic() +
  scale_fill_manual(values = c(BRHD_cols[[1]], BRHD_cols[[3]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))
#Race
ins_race <- ggplot(hlth_race, aes(Race_Ethnicity,hlcov_cr, fill= Race_Ethnicity)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = hlcov_cr_lci, ymax = hlcov_cr_uci), width=0.3, colour=BRHD_cols[[9]], alpha=0.9, size=1.3) +
  labs(title = "Percent of Adults with Health Insurance by Race/Ethinicity", x="Race/Ethnicity", y="Percent") +
  theme_classic() +
  scale_fill_manual(labels = c("Black or African American only, non-Hispanic","American Indian or Alaska Native only, non-Hispanic","Asian only, non-Hispanic","Hispanic/Latino","Native Hawaiian or Other Pacific Islander only, non-Hispanic","Two or More Races only, non-Hispanic","Unknown","White only, non-Hispanic"), values = c(BRHD_cols[[5]],BRHD_cols[[6]],BRHD_cols[[1]],BRHD_cols[[7]],BRHD_cols[[3]],BRHD_cols[[2]],BRHD_cols[[4]],BRHD_cols[[8]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold")) 



###Percent unable to afford needed care
#Cache
no_care <- plot_ly(hlth_wide, x=~Year, y=~ca_no_care_cr, type = 'scatter', mode = "lines", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
no_care <- no_care %>% add_trace(y = ~ca_no_care_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
no_care <- no_care %>% add_trace(y = ~ca_no_care_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
no_care <- no_care %>% add_trace(y = ~be_no_care_cr, type = 'scatter', mode = "lines", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
no_care <- no_care %>% add_trace(y = ~be_no_care_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
no_care <- no_care %>% add_trace(y = ~be_no_care_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
no_care <- no_care %>% add_trace(y = ~br_no_care_cr, type = 'scatter', mode = "lines", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
no_care <- no_care %>% add_trace(y = ~br_no_care_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
no_care <- no_care %>% add_trace(y = ~br_no_care_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
no_care <- no_care %>% layout(title = 'Percent of Adults Unable to Afford Needed Care',
                              xaxis = list(title = 'Year'),
                              yaxis = list(title = 'Percent'))

###AA Percent unable to afford needed care
#Cache
aa_no_care <- plot_ly(hlth_wide, x=~Year, y=~ca_no_care_aar, type = 'scatter', mode = "lines", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
aa_no_care <- aa_no_care %>% add_trace(y = ~ca_no_care_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
aa_no_care <- aa_no_care %>% add_trace(y = ~ca_no_care_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
aa_no_care <- aa_no_care %>% add_trace(y = ~be_no_care_aar, type = 'scatter', mode = "lines", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
aa_no_care <- aa_no_care %>% add_trace(y = ~be_no_care_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
aa_no_care <- aa_no_care %>% add_trace(y = ~be_no_care_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
aa_no_care <- aa_no_care %>% add_trace(y = ~br_no_care_aar, type = 'scatter', mode = "lines", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
aa_no_care <- aa_no_care %>% add_trace(y = ~br_no_care_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
aa_no_care <- aa_no_care %>% add_trace(y = ~br_no_care_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
aa_no_care <- aa_no_care %>% layout(title = 'Age-Adjusted Percent of Adults Unable to Afford Needed Care',
                                    xaxis = list(title = 'Year'),
                                    yaxis = list(title = 'Percent'))
#Poverty
no_care_pov <- ggplot(hlth_pov, aes(Poverty_Status,no_care_cr, fill = Poverty_Status)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = no_care_cr_lci, ymax = no_care_cr_uci), width = 0.2) +
  labs(title = "Percent of Adults Unable to Afford Needed Care by Poverty", x="Poverty Status", y="Percent") +
  theme_classic() +
  scale_fill_manual(values = c(BRHD_cols[[1]], BRHD_cols[[3]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))
#Race
no_care_race <- ggplot(hlth_race, aes(Race_Ethnicity,no_care_cr, fill= Race_Ethnicity)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = no_care_cr_lci, ymax = no_care_cr_uci), width=0.3, colour=BRHD_cols[[9]], alpha=0.9, size=1.3) +
  labs(title = "Percent of Adults Unable to Afford Needed Care by Race/Ethinicity", x="Race/Ethnicity", y="Percent") +
  theme_classic() +
  scale_fill_manual(labels = c("Black or African American only, non-Hispanic","American Indian or Alaska Native only, non-Hispanic","Asian only, non-Hispanic","Hispanic/Latino","Native Hawaiian or Other Pacific Islander only, non-Hispanic","Two or More Races only, non-Hispanic","Unknown","White only, non-Hispanic"), values = c(BRHD_cols[[5]],BRHD_cols[[6]],BRHD_cols[[1]],BRHD_cols[[7]],BRHD_cols[[3]],BRHD_cols[[2]],BRHD_cols[[4]],BRHD_cols[[8]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))  


###Percent have health provider
#Cache
hlth_pro <- plot_ly(hlth_wide, x=~Year, y=~ca_hlth_pro_cr, type = 'scatter', mode = "lines", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
hlth_pro <- hlth_pro %>% add_trace(y = ~ca_hlth_pro_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
hlth_pro <- hlth_pro %>% add_trace(y = ~ca_hlth_pro_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
hlth_pro <- hlth_pro %>% add_trace(y = ~be_hlth_pro_cr, type = 'scatter', mode = "lines", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
hlth_pro <- hlth_pro %>% add_trace(y = ~be_hlth_pro_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
hlth_pro <- hlth_pro %>% add_trace(y = ~be_hlth_pro_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
hlth_pro <- hlth_pro %>% add_trace(y = ~br_hlth_pro_cr, type = 'scatter', mode = "lines", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
hlth_pro <- hlth_pro %>% add_trace(y = ~br_hlth_pro_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
hlth_pro <- hlth_pro %>% add_trace(y = ~br_hlth_pro_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
hlth_pro <- hlth_pro %>% layout(title = 'Percent of Adults with a Usual Primary Care Provider',
                                xaxis = list(title = 'Year'),
                                yaxis = list(title = 'Percent'))


###AA Percent have health provider
#Cache
aa_hlth_pro <- plot_ly(hlth_wide, x=~Year, y=~ca_hlth_pro_aar, type = 'scatter', mode = "lines", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
aa_hlth_pro <- aa_hlth_pro %>% add_trace(y = ~ca_hlth_pro_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
aa_hlth_pro <- aa_hlth_pro %>% add_trace(y = ~ca_hlth_pro_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
aa_hlth_pro <- aa_hlth_pro %>% add_trace(y = ~be_hlth_pro_aar, type = 'scatter', mode = "lines", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
aa_hlth_pro <- aa_hlth_pro %>% add_trace(y = ~be_hlth_pro_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
aa_hlth_pro <- aa_hlth_pro %>% add_trace(y = ~be_hlth_pro_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
aa_hlth_pro <- aa_hlth_pro %>% add_trace(y = ~br_hlth_pro_aar, type = 'scatter', mode = "lines", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
aa_hlth_pro <- aa_hlth_pro %>% add_trace(y = ~br_hlth_pro_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
aa_hlth_pro <- aa_hlth_pro %>% add_trace(y = ~br_hlth_pro_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
aa_hlth_pro <- aa_hlth_pro %>% layout(title = 'Age-Adjusted Percent of Adults with a Usual Primary Care Provider',
                                      xaxis = list(title = 'Year'),
                                      yaxis = list(title = 'Percent'))

#Poverty
hlth_pro_pov <- ggplot(hlth_pov, aes(Poverty_Status,hlth_pro_cr, fill = Poverty_Status)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = hlth_pro_cr_lci, ymax = hlth_pro_cr_uci), width = 0.2) +
  labs(title = "Percent of Adults with a Usual Primary Care Provider by Poverty", x="Poverty Status", y="Percent") +
  theme_classic() +
  scale_fill_manual(values = c(BRHD_cols[[1]], BRHD_cols[[3]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))
#Race
hlth_pro_race <- ggplot(hlth_race, aes(Race_Ethnicity,hlth_pro_cr, fill= Race_Ethnicity)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = hlth_pro_cr_lci, ymax = hlth_pro_cr_uci), width=0.3, colour=BRHD_cols[[9]], alpha=0.9, size=1.3) +
  labs(title = "Percent of Adults with a Usual Primary Care Provider by Race/Ethinicity", x="Race/Ethnicity", y="Percent") +
  theme_classic() +
  scale_fill_manual(labels = c("Black or African American only, non-Hispanic","American Indian or Alaska Native only, non-Hispanic","Asian only, non-Hispanic","Hispanic/Latino","Native Hawaiian or Other Pacific Islander only, non-Hispanic","Two or More Races only, non-Hispanic","Unknown","White only, non-Hispanic"), values = c(BRHD_cols[[5]],BRHD_cols[[6]],BRHD_cols[[1]],BRHD_cols[[7]],BRHD_cols[[3]],BRHD_cols[[2]],BRHD_cols[[4]],BRHD_cols[[8]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold")) 


###Percent with a routine medical checkup in last 12 months
#Cache
med_check <- plot_ly(hlth_wide, x=~Year, y=~ca_med_check_cr, type = 'scatter', mode = "lines", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
med_check <- med_check %>% add_trace(y = ~ca_med_check_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
med_check <- med_check %>% add_trace(y = ~ca_med_check_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
med_check <- med_check %>% add_trace(y = ~be_med_check_cr, type = 'scatter', mode = "lines", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
med_check <- med_check %>% add_trace(y = ~be_med_check_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
med_check <- med_check %>% add_trace(y = ~be_med_check_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
med_check <- med_check %>% add_trace(y = ~br_med_check_cr, type = 'scatter', mode = "lines", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
med_check <- med_check %>% add_trace(y = ~br_med_check_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
med_check <- med_check %>% add_trace(y = ~br_med_check_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
med_check <- med_check %>% layout(title = 'Percent of Adults who Received a Routine Medical Check-Up in the Last 12 months',
                                  xaxis = list(title = 'Year'),
                                  yaxis = list(title = 'Percent'))


###AA Percent unable to afford needed care
#Cache
aa_med_check <- plot_ly(hlth_wide, x=~Year, y=~ca_med_check_aar, type = 'scatter', mode = "lines", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
aa_med_check <- aa_med_check %>% add_trace(y = ~ca_med_check_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
aa_med_check <- aa_med_check %>% add_trace(y = ~ca_med_check_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
aa_med_check <- aa_med_check %>% add_trace(y = ~be_med_check_aar, type = 'scatter', mode = "lines", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
aa_med_check <- aa_med_check %>% add_trace(y = ~be_med_check_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
aa_med_check <- aa_med_check %>% add_trace(y = ~be_med_check_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
aa_med_check <- aa_med_check %>% add_trace(y = ~br_med_check_aar, type = 'scatter', mode = "lines", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
aa_med_check <- aa_med_check %>% add_trace(y = ~br_med_check_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
aa_med_check <- aa_med_check %>% add_trace(y = ~br_med_check_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
aa_med_check <- aa_med_check %>% layout(title = 'Age-Adjusted Percent of Adults who Received a Routine Medical Check-Up in the Last 12 months',
                                        xaxis = list(title = 'Year'),
                                        yaxis = list(title = 'Percent'))
#Poverty
med_check_pov <- ggplot(hlth_pov, aes(Poverty_Status,med_check_cr, fill = Poverty_Status)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = med_check_cr_lci, ymax = med_check_cr_uci), width = 0.2) +
  labs(title = "Percent of Adults who Received a Routine Medical Check-Up in the Last 12 months by Poverty", x="Poverty Status", y="Percent") +
  theme_classic() +
  scale_fill_manual(values = c(BRHD_cols[[1]], BRHD_cols[[3]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))
#Race
med_check_race <- ggplot(hlth_race, aes(Race_Ethnicity,med_check_cr, fill= Race_Ethnicity)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = med_check_cr_lci, ymax = med_check_cr_uci), width=0.3, colour=BRHD_cols[[9]], alpha=0.9, size=1.3) +
  labs(title = "Percent of Adults who Received a Routine Medical Check-Up in the Last 12 months by Race/Ethinicity", x="Race/Ethnicity", y="Percent") +
  theme_classic() +
  scale_fill_manual(labels = c("Black or African American only, non-Hispanic","American Indian or Alaska Native only, non-Hispanic","Asian only, non-Hispanic","Hispanic/Latino","Native Hawaiian or Other Pacific Islander only, non-Hispanic","Two or More Races only, non-Hispanic","Unknown","White only, non-Hispanic"), values = c(BRHD_cols[[5]],BRHD_cols[[6]],BRHD_cols[[1]],BRHD_cols[[7]],BRHD_cols[[3]],BRHD_cols[[2]],BRHD_cols[[4]],BRHD_cols[[8]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))  


###Percent with a routine dental checkup in last 12 months
#Cache
dent_check <- plot_ly(hlth_wide, x=~Year, y=~ca_dent_check_cr, type = 'scatter', mode = "marker", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
dent_check <- dent_check %>% add_trace(y = ~ca_dent_check_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
dent_check <- dent_check %>% add_trace(y = ~ca_dent_check_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
dent_check <- dent_check %>% add_trace(y = ~be_dent_check_cr, type = 'scatter', mode = "marker", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
dent_check <- dent_check %>% add_trace(y = ~be_dent_check_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
dent_check <- dent_check %>% add_trace(y = ~be_dent_check_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
dent_check <- dent_check %>% add_trace(y = ~br_dent_check_cr, type = 'scatter', mode = "marker", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
dent_check <- dent_check %>% add_trace(y = ~br_dent_check_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
dent_check <- dent_check %>% add_trace(y = ~br_dent_check_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
dent_check <- dent_check %>% layout(title = 'Percent of Adults who Received a Routine Dental Check-Up in the Last 12 months',
                                    xaxis = list(title = 'Year'),
                                    yaxis = list(title = 'Percent'))

###AA Percent with a routine dental checkup in last 12 months
#Cache
aa_dent_check <- plot_ly(hlth_wide, x=~Year, y=~ca_dent_check_aar, type = 'scatter', mode = "marker", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
aa_dent_check <- aa_dent_check %>% add_trace(y = ~ca_dent_check_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
aa_dent_check <- aa_dent_check %>% add_trace(y = ~ca_dent_check_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
aa_dent_check <- aa_dent_check %>% add_trace(y = ~be_dent_check_aar, type = 'scatter', mode = "marker", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
aa_dent_check <- aa_dent_check %>% add_trace(y = ~be_dent_check_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
aa_dent_check <- aa_dent_check %>% add_trace(y = ~be_dent_check_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
aa_dent_check <- aa_dent_check %>% add_trace(y = ~br_dent_check_aar, type = 'scatter', mode = "marker", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
aa_dent_check <- aa_dent_check %>% add_trace(y = ~br_dent_check_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
aa_dent_check <- aa_dent_check %>% add_trace(y = ~br_dent_check_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
aa_dent_check <- aa_dent_check %>% layout(title = 'Age-Adjusted Percent of Adults who Received a Routine Dental Check-Up in the Last 12 months',
                                          xaxis = list(title = 'Year'),
                                          yaxis = list(title = 'Percent'))
#Poverty
dent_check_pov <- ggplot(hlth_pov, aes(Poverty_Status,dent_check_cr, fill = Poverty_Status)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = dent_check_cr_lci, ymax = dent_check_cr_uci), width = 0.2) +
  labs(title = "Percent of Adults who Received a Routine Dental Check-Up in the Last 12 months by Poverty", x="Poverty Status", y="Percent") +
  theme_classic() +
  scale_fill_manual(values = c(BRHD_cols[[1]], BRHD_cols[[3]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))
#Race
dent_check_race <- ggplot(hlth_race, aes(Race_Ethnicity,dent_check_cr, fill= Race_Ethnicity)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = dent_check_cr_lci, ymax = dent_check_cr_uci), width=0.3, colour=BRHD_cols[[9]], alpha=0.9, size=1.3) +
  labs(title = "Percent of Adults who Received a Routine Dental Check-Up in the Last 12 months by Race/Ethinicity", x="Race/Ethnicity", y="Percent") +
  theme_classic() +
  scale_fill_manual(labels = c("Black or African American only, non-Hispanic","American Indian or Alaska Native only, non-Hispanic","Asian only, non-Hispanic","Hispanic/Latino","Native Hawaiian or Other Pacific Islander only, non-Hispanic","Two or More Races only, non-Hispanic","Unknown","White only, non-Hispanic"), values = c(BRHD_cols[[5]],BRHD_cols[[6]],BRHD_cols[[1]],BRHD_cols[[7]],BRHD_cols[[3]],BRHD_cols[[2]],BRHD_cols[[4]],BRHD_cols[[8]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold")) 


###Percent of Women 40+ Who Had a Mammogram in the Previous Two Years
#Cache
mammo <- plot_ly(hlth_wide, x=~Year, y=~ca_mammo_cr, type = 'scatter', mode = "marker", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
mammo <- mammo %>% add_trace(y = ~ca_mammo_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
mammo <- mammo %>% add_trace(y = ~ca_mammo_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
mammo <- mammo %>% add_trace(y = ~be_mammo_cr, type = 'scatter', mode = "marker", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
mammo <- mammo %>% add_trace(y = ~be_mammo_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
mammo <- mammo %>% add_trace(y = ~be_mammo_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
mammo <- mammo %>% add_trace(y = ~br_mammo_cr, type = 'scatter', mode = "marker", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
mammo <- mammo %>% add_trace(y = ~br_mammo_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
mammo <- mammo %>% add_trace(y = ~br_mammo_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
mammo <- mammo %>% layout(title = 'Percent of Women 40+ Who Had a Mammogram in the Previous Two Years',
                          xaxis = list(title = 'Year'),
                          yaxis = list(title = 'Percent'))


###AA Percent unable to afford needed care
#Cache
aa_mammo <- plot_ly(hlth_wide, x=~Year, y=~ca_mammo_aar, type = 'scatter', mode = "marker", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
aa_mammo <- aa_mammo %>% add_trace(y = ~ca_mammo_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
aa_mammo <- aa_mammo %>% add_trace(y = ~ca_mammo_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
aa_mammo <- aa_mammo %>% add_trace(y = ~be_mammo_aar, type = 'scatter', mode = "marker", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
aa_mammo <- aa_mammo %>% add_trace(y = ~be_mammo_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
aa_mammo <- aa_mammo %>% add_trace(y = ~be_mammo_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
aa_mammo <- aa_mammo %>% add_trace(y = ~br_mammo_aar, type = 'scatter', mode = "marker", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
aa_mammo <- aa_mammo %>% add_trace(y = ~br_mammo_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
aa_mammo <- aa_mammo %>% add_trace(y = ~br_mammo_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
aa_mammo <- aa_mammo %>% layout(title = 'Age-Adjusted Percent of Women 40+ Who Had a Mammogram in the Previous Two Years',
                                xaxis = list(title = 'Year'),
                                yaxis = list(title = 'Percent'))

#Poverty
mammo_pov <- ggplot(hlth_pov, aes(Poverty_Status,mammo_cr, fill = Poverty_Status)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mammo_cr_lci, ymax = mammo_cr_uci), width = 0.2) +
  labs(title = "Percent of Women 40+ Who Had a Mammogram in the Previous Two Years by Poverty", x="Poverty Status", y="Percent") +
  theme_classic() +
  scale_fill_manual(values = c(BRHD_cols[[1]], BRHD_cols[[3]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))
#Race
mammo_race <- ggplot(hlth_race, aes(Race_Ethnicity,mammo_cr, fill= Race_Ethnicity)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mammo_cr_lci, ymax = mammo_cr_uci), width=0.3, colour=BRHD_cols[[9]], alpha=0.9, size=1.3) +
  labs(title = "Percent of Women 40+ Who Had a Mammogram in the Previous Two Years by Race/Ethinicity", x="Race/Ethnicity", y="Percent") +
  theme_classic() +
  scale_fill_manual(labels = c("Black or African American only, non-Hispanic","American Indian or Alaska Native only, non-Hispanic","Asian only, non-Hispanic","Hispanic/Latino","Native Hawaiian or Other Pacific Islander only, non-Hispanic","Two or More Races only, non-Hispanic","Unknown","White only, non-Hispanic"), values = c(BRHD_cols[[5]],BRHD_cols[[6]],BRHD_cols[[1]],BRHD_cols[[7]],BRHD_cols[[3]],BRHD_cols[[2]],BRHD_cols[[4]],BRHD_cols[[8]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold")) 


###Percent of Adults 50+ who were up-to-date with Colorectal Cancer Screening
#Cache
col_scree <- plot_ly(hlth_wide, x=~Year, y=~ca_col_scree_cr, type = 'scatter', mode = "marker", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
col_scree <- col_scree %>% add_trace(y = ~ca_col_scree_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
col_scree <- col_scree %>% add_trace(y = ~ca_col_scree_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
col_scree <- col_scree %>% add_trace(y = ~be_col_scree_cr, type = 'scatter', mode = "marker", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
col_scree <- col_scree %>% add_trace(y = ~be_col_scree_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
col_scree <- col_scree %>% add_trace(y = ~be_col_scree_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
col_scree <- col_scree %>% add_trace(y = ~br_col_scree_cr, type = 'scatter', mode = "marker", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
col_scree <- col_scree %>% add_trace(y = ~br_col_scree_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
col_scree <- col_scree %>% add_trace(y = ~br_col_scree_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
col_scree <- col_scree %>% layout(title = 'Percent of Adults 50+ who were up-to-date with Colorectal Cancer Screening',
                                  xaxis = list(title = 'Year'),
                                  yaxis = list(title = 'Percent'))


###AA Percent of Adults 50+ who were up-to-date with Colorectal Cancer Screening

#Cache
aa_col_scree <- plot_ly(hlth_wide, x=~Year, y=~ca_col_scree_aar, type = 'scatter', mode = "marker", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
aa_col_scree <- aa_col_scree %>% add_trace(y = ~ca_col_scree_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
aa_col_scree <- aa_col_scree %>% add_trace(y = ~ca_col_scree_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
aa_col_scree <- aa_col_scree %>% add_trace(y = ~be_col_scree_aar, type = 'scatter', mode = "marker", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
aa_col_scree <- aa_col_scree %>% add_trace(y = ~be_col_scree_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
aa_col_scree <- aa_col_scree %>% add_trace(y = ~be_col_scree_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
aa_col_scree <- aa_col_scree %>% add_trace(y = ~br_col_scree_aar, type = 'scatter', mode = "marker", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
aa_col_scree <- aa_col_scree %>% add_trace(y = ~br_col_scree_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
aa_col_scree <- aa_col_scree %>% add_trace(y = ~br_col_scree_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
aa_col_scree <- aa_col_scree %>% layout(title = 'Age-Adjusted Percent of Adults 50+ who were up-to-date with Colorectal Cancer Screening',
                                        xaxis = list(title = 'Year'),
                                        yaxis = list(title = 'Percent'))
#Poverty
ins_pov <- ggplot(hlth_pov, aes(Poverty_Status,hlcov_cr, fill = Poverty_Status)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = hlcov_cr_lci, ymax = hlcov_cr_uci), width = 0.2) +
  labs(title = "Percent of Adults with Health Insurance by Poverty", x="Poverty Status", y="Percent") +
  theme_classic() +
  scale_fill_manual(values = c(BRHD_cols[[1]], BRHD_cols[[3]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))
#Race
ins_race <- ggplot(hlth_race, aes(Race_Ethnicity,hlcov_cr, fill= Race_Ethnicity)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = hlcov_cr_lci, ymax = hlcov_cr_uci), width=0.3, colour=BRHD_cols[[9]], alpha=0.9, size=1.3) +
  labs(title = "Percent of Adults with Health Insurance by Race/Ethinicity", x="Race/Ethnicity", y="Percent") +
  theme_classic() +
  scale_fill_manual(labels = c("Black or African American only, non-Hispanic","American Indian or Alaska Native only, non-Hispanic","Asian only, non-Hispanic","Hispanic/Latino","Native Hawaiian or Other Pacific Islander only, non-Hispanic","Two or More Races only, non-Hispanic","Unknown","White only, non-Hispanic"), values = c(BRHD_cols[[5]],BRHD_cols[[6]],BRHD_cols[[1]],BRHD_cols[[7]],BRHD_cols[[3]],BRHD_cols[[2]],BRHD_cols[[4]],BRHD_cols[[8]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold")) 


###Percent of Births Where the Mother Received First Trimester Prenatal Care
#Cache
pn_hlth_wide <- hlth_wide %>% 
  filter(Year >= '2009')

pn_perc <- plot_ly(pn_hlth_wide, x=~Year, y=~ca_pn_perc, type = 'scatter', mode = "lines", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
pn_perc <- pn_perc %>% add_trace(y = ~ca_pn_perc_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
pn_perc <- pn_perc %>% add_trace(y = ~ca_pn_perc_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
pn_perc <- pn_perc %>% add_trace(y = ~be_pn_perc, type = 'scatter', mode = "lines", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
pn_perc <- pn_perc %>% add_trace(y = ~be_pn_perc_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
pn_perc <- pn_perc %>% add_trace(y = ~be_pn_perc_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
pn_perc <- pn_perc %>% add_trace(y = ~br_pn_perc, type = 'scatter', mode = "lines", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
pn_perc <- pn_perc %>% add_trace(y = ~br_pn_perc_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
pn_perc <- pn_perc %>% add_trace(y = ~br_pn_perc_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
pn_perc <- pn_perc %>% layout(title = 'Percent of Births Where the Mother Received First Trimester Prenatal Care',
                              xaxis = list(title = 'Year'),
                              yaxis = list(title = 'Percent'))
#Poverty
ins_pov <- ggplot(hlth_pov, aes(Poverty_Status,hlcov_cr, fill = Poverty_Status)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = hlcov_cr_lci, ymax = hlcov_cr_uci), width = 0.2) +
  labs(title = "Percent of Adults with Health Insurance by Poverty", x="Poverty Status", y="Percent") +
  theme_classic() +
  scale_fill_manual(values = c(BRHD_cols[[1]], BRHD_cols[[3]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))
#Race
ins_race <- ggplot(hlth_race, aes(Race_Ethnicity,hlcov_cr, fill= Race_Ethnicity)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = hlcov_cr_lci, ymax = hlcov_cr_uci), width=0.3, colour=BRHD_cols[[9]], alpha=0.9, size=1.3) +
  labs(title = "Percent of Adults with Health Insurance by Race/Ethinicity", x="Race/Ethnicity", y="Percent") +
  theme_classic() +
  scale_fill_manual(labels = c("Black or African American only, non-Hispanic","American Indian or Alaska Native only, non-Hispanic","Asian only, non-Hispanic","Hispanic/Latino","Native Hawaiian or Other Pacific Islander only, non-Hispanic","Two or More Races only, non-Hispanic","Unknown","White only, non-Hispanic"), values = c(BRHD_cols[[5]],BRHD_cols[[6]],BRHD_cols[[1]],BRHD_cols[[7]],BRHD_cols[[3]],BRHD_cols[[2]],BRHD_cols[[4]],BRHD_cols[[8]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold")) 

###Percent of Adults Receiving Flu Vaccination in Last 12 Months
fluvac <- plot_ly(hlth_wide, x=~Year, y=~ca_fluvac_cr, type = 'scatter', mode = "lines", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
fluvac <- fluvac %>% add_trace(y = ~ca_fluvac_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
fluvac <- fluvac %>% add_trace(y = ~ca_fluvac_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
fluvac <- fluvac %>% add_trace(y = ~be_fluvac_cr, type = 'scatter', mode = "lines", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
fluvac <- fluvac %>% add_trace(y = ~be_fluvac_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
fluvac <- fluvac %>% add_trace(y = ~be_fluvac_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
fluvac <- fluvac %>% add_trace(y = ~br_fluvac_cr, type = 'scatter', mode = "lines", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
fluvac <- fluvac %>% add_trace(y = ~br_fluvac_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
fluvac <- fluvac %>% add_trace(y = ~br_fluvac_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
fluvac <- fluvac %>% layout(title = 'Percent of Adults Receiving Flu Vaccination in Last 12 Months',
                            xaxis = list(title = 'Year'),
                            yaxis = list(title = 'Percent'))

###AA Percent of Adults Receiving Flu Vaccination in Last 12 Months
#Cache
aa_fluvac <- plot_ly(hlth_wide, x=~Year, y=~ca_fluvac_aar, type = 'scatter', mode = "lines", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
aa_fluvac <- aa_fluvac %>% add_trace(y = ~ca_fluvac_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
aa_fluvac <- aa_fluvac %>% add_trace(y = ~ca_fluvac_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
aa_fluvac <- aa_fluvac %>% add_trace(y = ~be_fluvac_aar, type = 'scatter', mode = "lines", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
aa_fluvac <- aa_fluvac %>% add_trace(y = ~be_fluvac_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
aa_fluvac <- aa_fluvac %>% add_trace(y = ~be_fluvac_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
aa_fluvac <- aa_fluvac %>% add_trace(y = ~br_fluvac_aar, type = 'scatter', mode = "lines", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
aa_fluvac <- aa_fluvac %>% add_trace(y = ~br_fluvac_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
aa_fluvac <- aa_fluvac %>% add_trace(y = ~br_fluvac_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
aa_fluvac <- aa_fluvac %>% layout(title = 'Age-Adjusted Percent of Adults Receiving Flu Vaccination in Last 12 Months',
                                  xaxis = list(title = 'Year'),
                                  yaxis = list(title = 'Percent'))
#Poverty
ins_pov <- ggplot(hlth_pov, aes(Poverty_Status,hlcov_cr, fill = Poverty_Status)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = hlcov_cr_lci, ymax = hlcov_cr_uci), width = 0.2) +
  labs(title = "Percent of Adults with Health Insurance by Poverty", x="Poverty Status", y="Percent") +
  theme_classic() +
  scale_fill_manual(values = c(BRHD_cols[[1]], BRHD_cols[[3]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))
#Race
ins_race <- ggplot(hlth_race, aes(Race_Ethnicity,hlcov_cr, fill= Race_Ethnicity)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = hlcov_cr_lci, ymax = hlcov_cr_uci), width=0.3, colour=BRHD_cols[[9]], alpha=0.9, size=1.3) +
  labs(title = "Percent of Adults with Health Insurance by Race/Ethinicity", x="Race/Ethnicity", y="Percent") +
  theme_classic() +
  scale_fill_manual(labels = c("Black or African American only, non-Hispanic","American Indian or Alaska Native only, non-Hispanic","Asian only, non-Hispanic","Hispanic/Latino","Native Hawaiian or Other Pacific Islander only, non-Hispanic","Two or More Races only, non-Hispanic","Unknown","White only, non-Hispanic"), values = c(BRHD_cols[[5]],BRHD_cols[[6]],BRHD_cols[[1]],BRHD_cols[[7]],BRHD_cols[[3]],BRHD_cols[[2]],BRHD_cols[[4]],BRHD_cols[[8]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold")) 

###Risk###

### Adolescent Birth Rate
#Cache
adol_brths <- plot_ly(risk_wide, x=~Year, y=~ca_adol_brths, type = 'scatter', mode = "marker", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
adol_brths <- adol_brths %>% add_trace(y = ~ca_adol_brths_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
adol_brths <- adol_brths %>% add_trace(y = ~ca_adol_brths_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
adol_brths <- adol_brths %>% add_trace(y = ~be_adol_brths, type = 'scatter', mode = "lines", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
adol_brths <- adol_brths %>% add_trace(y = ~be_adol_brths_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
adol_brths <- adol_brths %>% add_trace(y = ~be_adol_brths_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
adol_brths <- adol_brths %>% add_trace(y = ~br_adol_brths, type = 'scatter', mode = "lines", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
adol_brths <- adol_brths %>% add_trace(y = ~br_adol_brths_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
adol_brths <- adol_brths %>% add_trace(y = ~br_adol_brths_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
adol_brths <- adol_brths %>% layout(title = 'Rate of Births among Adolescents',
                                    xaxis = list(title = 'Year'),
                                    yaxis = list(title = 'Birth Rate (per 1,000)'))
#Poverty
adol_brths_pov <- ggplot(risk_pov, aes(Poverty_Status,adol_brths, fill = Poverty_Status)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = adol_brthsr_lci, ymax = adol_brths_uci), width = 0.2) +
  labs(title = "Rate of Births among Adolescents by Poverty", x="Poverty Status", y="Birth Rate (per 1,000)") +
  theme_classic() +
  scale_fill_manual(values = c(BRHD_cols[[1]], BRHD_cols[[3]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))
#Race
adol_brths_race <- ggplot(risk_race, aes(Race_Ethnicity,adol_brths, fill= Race_Ethnicity)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = adol_brths_lci, ymax = adol_brths_uci), width=0.3, colour=BRHD_cols[[9]], alpha=0.9, size=1.3) +
  labs(title = "Rate of Births among Adolescents by Race/Ethinicity", x="Race/Ethnicity", y="Birth Rate (per 1,000)") +
  theme_classic() +
  scale_fill_manual(labels = c("Black or African American only, non-Hispanic","American Indian or Alaska Native only, non-Hispanic","Asian only, non-Hispanic","Hispanic/Latino","Native Hawaiian or Other Pacific Islander only, non-Hispanic","Two or More Races only, non-Hispanic","Unknown","White only, non-Hispanic"), values = c(BRHD_cols[[5]],BRHD_cols[[6]],BRHD_cols[[1]],BRHD_cols[[7]],BRHD_cols[[3]],BRHD_cols[[2]],BRHD_cols[[4]],BRHD_cols[[8]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold")) 

### Fruit Rate
#Cache
fruit_cr <- plot_ly(risk_wide, x=~Year, y=~ca_fruit_cr, type = 'scatter', mode = "marker", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
fruit_cr <- fruit_cr %>% add_trace(y = ~ca_fruit_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
fruit_cr <- fruit_cr %>% add_trace(y = ~ca_fruit_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
fruit_cr <- fruit_cr %>% add_trace(y = ~be_fruit_cr, type = 'scatter', mode = "marker", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
fruit_cr <- fruit_cr %>% add_trace(y = ~be_fruit_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
fruit_cr <- fruit_cr %>% add_trace(y = ~be_fruit_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
fruit_cr <- fruit_cr %>% add_trace(y = ~br_fruit_cr, type = 'scatter', mode = "marker", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
fruit_cr <- fruit_cr %>% add_trace(y = ~br_fruit_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
fruit_cr <- fruit_cr %>% add_trace(y = ~br_fruit_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
fruit_cr <- fruit_cr %>% layout(title = 'Percent of Adults Consuming the Recommended Amount of Fruit',
                                xaxis = list(title = 'Year'),
                                yaxis = list(title = 'Percent'))


aa_fruit <- plot_ly(risk_wide, x=~Year, y=~ca_fruit_aar, type = 'scatter', mode = "marker", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
aa_fruit <- aa_fruit %>% add_trace(y = ~ca_fruit_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
aa_fruit <- aa_fruit %>% add_trace(y = ~ca_fruit_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
aa_fruit <- aa_fruit %>% add_trace(y = ~be_fruit_aar, type = 'scatter', mode = "marker", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
aa_fruit <- aa_fruit %>% add_trace(y = ~be_fruit_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
aa_fruit <- aa_fruit %>% add_trace(y = ~be_fruit_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
aa_fruit <- aa_fruit %>% add_trace(y = ~br_fruit_aar, type = 'scatter', mode = "marker", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
aa_fruit <- aa_fruit %>% add_trace(y = ~br_fruit_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
aa_fruit <- aa_fruit %>% add_trace(y = ~br_fruit_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
aa_fruit <- aa_fruit %>% layout(title = 'Age-Adjusted Percent of Adults Consuming the Recommended Amount of Fruit',
                                xaxis = list(title = 'Year'),
                                yaxis = list(title = 'Percent'))
#Poverty
ins_pov <- ggplot(risk_pov, aes(Poverty_Status,hlcov_cr, fill = Poverty_Status)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = hlcov_cr_lci, ymax = hlcov_cr_uci), width = 0.2) +
  labs(title = "Percent of Adults with Health Insurance by Poverty", x="Poverty Status", y="Percent") +
  theme_classic() +
  scale_fill_manual(values = c(BRHD_cols[[1]], BRHD_cols[[3]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))
#Race
ins_race <- ggplot(risk_race, aes(Race_Ethnicity,hlcov_cr, fill= Race_Ethnicity)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = hlcov_cr_lci, ymax = hlcov_cr_uci), width=0.3, colour=BRHD_cols[[9]], alpha=0.9, size=1.3) +
  labs(title = "Percent of Adults with Health Insurance by Race/Ethinicity", x="Race/Ethnicity", y="Percent") +
  theme_classic() +
  scale_fill_manual(labels = c("Black or African American only, non-Hispanic","American Indian or Alaska Native only, non-Hispanic","Asian only, non-Hispanic","Hispanic/Latino","Native Hawaiian or Other Pacific Islander only, non-Hispanic","Two or More Races only, non-Hispanic","Unknown","White only, non-Hispanic"), values = c(BRHD_cols[[5]],BRHD_cols[[6]],BRHD_cols[[1]],BRHD_cols[[7]],BRHD_cols[[3]],BRHD_cols[[2]],BRHD_cols[[4]],BRHD_cols[[8]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))

### Veg Rate
#Cache
veg_cr <- plot_ly(risk_wide, x=~Year, y=~ca_veg_cr, type = 'scatter', mode = "marker", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
veg_cr <- veg_cr %>% add_trace(y = ~ca_veg_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
veg_cr <- veg_cr %>% add_trace(y = ~ca_veg_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
veg_cr <- veg_cr %>% add_trace(y = ~be_veg_cr, type = 'scatter', mode = "marker", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
veg_cr <- veg_cr %>% add_trace(y = ~be_veg_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
veg_cr <- veg_cr %>% add_trace(y = ~be_veg_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
veg_cr <- veg_cr %>% add_trace(y = ~br_veg_cr, type = 'scatter', mode = "marker", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
veg_cr <- veg_cr %>% add_trace(y = ~br_veg_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
veg_cr <- veg_cr %>% add_trace(y = ~br_veg_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
veg_cr <- veg_cr %>% layout(title = 'Percent of Adults Consuming the Recommended Amount of Vegetables',
                            xaxis = list(title = 'Year'),
                            yaxis = list(title = 'Percent'))



aa_veg <- plot_ly(risk_wide, x=~Year, y=~ca_veg_aar, type = 'scatter', mode = "marker", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
aa_veg <- aa_veg %>% add_trace(y = ~ca_veg_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
aa_veg <- aa_veg %>% add_trace(y = ~ca_veg_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
aa_veg <- aa_veg %>% add_trace(y = ~be_veg_aar, type = 'scatter', mode = "marker", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
aa_veg <- aa_veg %>% add_trace(y = ~be_veg_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
aa_veg <- aa_veg %>% add_trace(y = ~be_veg_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
aa_veg <- aa_veg %>% add_trace(y = ~br_veg_aar, type = 'scatter', mode = "marker", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
aa_veg <- aa_veg %>% add_trace(y = ~br_veg_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
aa_veg <- aa_veg %>% add_trace(y = ~br_veg_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
aa_veg <- aa_veg %>% layout(title = 'Age-Adjusted Percent of Adults Consuming the Recommended Amount of Vegetables',
                            xaxis = list(title = 'Year'),
                            yaxis = list(title = 'Percent'))
#Poverty
ins_pov <- ggplot(risk_pov, aes(Poverty_Status,hlcov_cr, fill = Poverty_Status)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = hlcov_cr_lci, ymax = hlcov_cr_uci), width = 0.2) +
  labs(title = "Percent of Adults with Health Insurance by Poverty", x="Poverty Status", y="Percent") +
  theme_classic() +
  scale_fill_manual(values = c(BRHD_cols[[1]], BRHD_cols[[3]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))
#Race
ins_race <- ggplot(risk_race, aes(Race_Ethnicity,hlcov_cr, fill= Race_Ethnicity)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = hlcov_cr_lci, ymax = hlcov_cr_uci), width=0.3, colour=BRHD_cols[[9]], alpha=0.9, size=1.3) +
  labs(title = "Percent of Adults with Health Insurance by Race/Ethinicity", x="Race/Ethnicity", y="Percent") +
  theme_classic() +
  scale_fill_manual(labels = c("Black or African American only, non-Hispanic","American Indian or Alaska Native only, non-Hispanic","Asian only, non-Hispanic","Hispanic/Latino","Native Hawaiian or Other Pacific Islander only, non-Hispanic","Two or More Races only, non-Hispanic","Unknown","White only, non-Hispanic"), values = c(BRHD_cols[[5]],BRHD_cols[[6]],BRHD_cols[[1]],BRHD_cols[[7]],BRHD_cols[[3]],BRHD_cols[[2]],BRHD_cols[[4]],BRHD_cols[[8]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))

##Physical Activity

phy_act_cr <- plot_ly(risk_wide, x=~Year, y=~ca_phy_act_cr, type = 'scatter', mode = "marker", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
phy_act_cr <- phy_act_cr %>% add_trace(y = ~ca_phy_act_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
phy_act_cr <- phy_act_cr %>% add_trace(y = ~ca_phy_act_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
phy_act_cr <- phy_act_cr %>% add_trace(y = ~be_phy_act_cr, type = 'scatter', mode = "marker", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
phy_act_cr <- phy_act_cr %>% add_trace(y = ~be_phy_act_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
phy_act_cr <- phy_act_cr %>% add_trace(y = ~be_phy_act_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
phy_act_cr <- phy_act_cr %>% add_trace(y = ~br_phy_act_cr, type = 'scatter', mode = "marker", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
phy_act_cr <- phy_act_cr %>% add_trace(y = ~br_phy_act_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
phy_act_cr <- phy_act_cr %>% add_trace(y = ~br_phy_act_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
phy_act_cr <- phy_act_cr %>% layout(title = 'Percent of Adults Getting the Recommended Amount of Aerobic Physical Activity and Muscle Strengthening',
                                    xaxis = list(title = 'Year'),
                                    yaxis = list(title = 'Percent'))


aa_phy_act <- plot_ly(risk_wide, x=~Year, y=~ca_phy_act_aar, type = 'scatter', mode = "marker", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
aa_phy_act <- aa_phy_act %>% add_trace(y = ~ca_phy_act_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
aa_phy_act <- aa_phy_act %>% add_trace(y = ~ca_phy_act_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
aa_phy_act <- aa_phy_act %>% add_trace(y = ~be_phy_act_aar, type = 'scatter', mode = "marker", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
aa_phy_act <- aa_phy_act %>% add_trace(y = ~be_phy_act_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
aa_phy_act <- aa_phy_act %>% add_trace(y = ~be_phy_act_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
aa_phy_act <- aa_phy_act %>% add_trace(y = ~br_phy_act_aar, type = 'scatter', mode = "marker", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
aa_phy_act <- aa_phy_act %>% add_trace(y = ~br_phy_act_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
aa_phy_act <- aa_phy_act %>% add_trace(y = ~br_phy_act_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
aa_phy_act <- aa_phy_act %>% layout(title = 'Age-Adjusted Percent of Adults Getting the Recommended Amount of Aerobic Physical Activity and Muscle Strengthening',
                                    xaxis = list(title = 'Year'),
                                    yaxis = list(title = 'Percent'))

#Poverty
ins_pov <- ggplot(risk_pov, aes(Poverty_Status,hlcov_cr, fill = Poverty_Status)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = hlcov_cr_lci, ymax = hlcov_cr_uci), width = 0.2) +
  labs(title = "Percent of Adults with Health Insurance by Poverty", x="Poverty Status", y="Percent") +
  theme_classic() +
  scale_fill_manual(values = c(BRHD_cols[[1]], BRHD_cols[[3]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))
#Race
ins_race <- ggplot(risk_race, aes(Race_Ethnicity,hlcov_cr, fill= Race_Ethnicity)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = hlcov_cr_lci, ymax = hlcov_cr_uci), width=0.3, colour=BRHD_cols[[9]], alpha=0.9, size=1.3) +
  labs(title = "Percent of Adults with Health Insurance by Race/Ethinicity", x="Race/Ethnicity", y="Percent") +
  theme_classic() +
  scale_fill_manual(labels = c("Black or African American only, non-Hispanic","American Indian or Alaska Native only, non-Hispanic","Asian only, non-Hispanic","Hispanic/Latino","Native Hawaiian or Other Pacific Islander only, non-Hispanic","Two or More Races only, non-Hispanic","Unknown","White only, non-Hispanic"), values = c(BRHD_cols[[5]],BRHD_cols[[6]],BRHD_cols[[1]],BRHD_cols[[7]],BRHD_cols[[3]],BRHD_cols[[2]],BRHD_cols[[4]],BRHD_cols[[8]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))


##Smoking cig_smkng

cig_smkng_cr <- plot_ly(risk_wide, x=~Year, y=~ca_cig_smkng_cr, type = 'scatter', mode = "marker", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
cig_smkng_cr <- cig_smkng_cr %>% add_trace(y = ~ca_cig_smkng_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
cig_smkng_cr <- cig_smkng_cr %>% add_trace(y = ~ca_cig_smkng_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
cig_smkng_cr <- cig_smkng_cr %>% add_trace(y = ~be_cig_smkng_cr, type = 'scatter', mode = "marker", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
cig_smkng_cr <- cig_smkng_cr %>% add_trace(y = ~be_cig_smkng_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
cig_smkng_cr <- cig_smkng_cr %>% add_trace(y = ~be_cig_smkng_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
cig_smkng_cr <- cig_smkng_cr %>% add_trace(y = ~br_cig_smkng_cr, type = 'scatter', mode = "marker", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
cig_smkng_cr <- cig_smkng_cr %>% add_trace(y = ~br_cig_smkng_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
cig_smkng_cr <- cig_smkng_cr %>% add_trace(y = ~br_cig_smkng_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
cig_smkng_cr <- cig_smkng_cr %>% layout(title = 'Percent of Adults Currently Smoking',
                                        xaxis = list(title = 'Year'),
                                        yaxis = list(title = 'Percent'))


aa_cig_smkng <- plot_ly(risk_wide, x=~Year, y=~ca_cig_smkng_aar, type = 'scatter', mode = "marker", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
aa_cig_smkng <- aa_cig_smkng %>% add_trace(y = ~ca_cig_smkng_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
aa_cig_smkng <- aa_cig_smkng %>% add_trace(y = ~ca_cig_smkng_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
aa_cig_smkng <- aa_cig_smkng %>% add_trace(y = ~be_cig_smkng_aar, type = 'scatter', mode = "marker", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
aa_cig_smkng <- aa_cig_smkng %>% add_trace(y = ~be_cig_smkng_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
aa_cig_smkng <- aa_cig_smkng %>% add_trace(y = ~be_cig_smkng_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
aa_cig_smkng <- aa_cig_smkng %>% add_trace(y = ~br_cig_smkng_aar, type = 'scatter', mode = "marker", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
aa_cig_smkng <- aa_cig_smkng %>% add_trace(y = ~br_cig_smkng_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
aa_cig_smkng <- aa_cig_smkng %>% add_trace(y = ~br_cig_smkng_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
aa_cig_smkng <- aa_cig_smkng %>% layout(title = 'Age-Adjusted Percent of Adults Currently Smoking',
                                        xaxis = list(title = 'Year'),
                                        yaxis = list(title = 'Percent'))
#Poverty
ins_pov <- ggplot(risk_pov, aes(Poverty_Status,hlcov_cr, fill = Poverty_Status)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = hlcov_cr_lci, ymax = hlcov_cr_uci), width = 0.2) +
  labs(title = "Percent of Adults with Health Insurance by Poverty", x="Poverty Status", y="Percent") +
  theme_classic() +
  scale_fill_manual(values = c(BRHD_cols[[1]], BRHD_cols[[3]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))
#Race
ins_race <- ggplot(risk_race, aes(Race_Ethnicity,hlcov_cr, fill= Race_Ethnicity)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = hlcov_cr_lci, ymax = hlcov_cr_uci), width=0.3, colour=BRHD_cols[[9]], alpha=0.9, size=1.3) +
  labs(title = "Percent of Adults with Health Insurance by Race/Ethinicity", x="Race/Ethnicity", y="Percent") +
  theme_classic() +
  scale_fill_manual(labels = c("Black or African American only, non-Hispanic","American Indian or Alaska Native only, non-Hispanic","Asian only, non-Hispanic","Hispanic/Latino","Native Hawaiian or Other Pacific Islander only, non-Hispanic","Two or More Races only, non-Hispanic","Unknown","White only, non-Hispanic"), values = c(BRHD_cols[[5]],BRHD_cols[[6]],BRHD_cols[[1]],BRHD_cols[[7]],BRHD_cols[[3]],BRHD_cols[[2]],BRHD_cols[[4]],BRHD_cols[[8]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))


#BRHD
ecig_cr <- plot_ly(risk_wide, x=~Year, y=~br_ecig_cr, type = 'scatter', mode = "marker", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
ecig_cr <- ecig_cr %>% add_trace(y = ~br_ecig_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
ecig_cr <- ecig_cr %>% add_trace(y = ~br_ecig_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
ecig_cr <- ecig_cr %>% layout(title = 'Percent of Adults Currently Using E-cigarettes',
                              xaxis = list(title = 'Year'),
                              yaxis = list(title = 'Percent'))
#BRHD
aa_ecig <- plot_ly(risk_wide, x=~Year, y=~br_ecig_aar, type = 'scatter', mode = "marker", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
aa_ecig <- aa_ecig %>% add_trace(y = ~br_ecig_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
aa_ecig <- aa_ecig %>% add_trace(y = ~br_ecig_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
aa_ecig <- aa_ecig %>% layout(title = 'Age-Adjusted Percent of Adults Currently Using E-cigarettes',
                              xaxis = list(title = 'Year'),
                              yaxis = list(title = 'Percent'))
#Poverty
ins_pov <- ggplot(risk_pov, aes(Poverty_Status,hlcov_cr, fill = Poverty_Status)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = hlcov_cr_lci, ymax = hlcov_cr_uci), width = 0.2) +
  labs(title = "Percent of Adults with Health Insurance by Poverty", x="Poverty Status", y="Percent") +
  theme_classic() +
  scale_fill_manual(values = c(BRHD_cols[[1]], BRHD_cols[[3]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))
#Race
ins_race <- ggplot(risk_race, aes(Race_Ethnicity,hlcov_cr, fill= Race_Ethnicity)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = hlcov_cr_lci, ymax = hlcov_cr_uci), width=0.3, colour=BRHD_cols[[9]], alpha=0.9, size=1.3) +
  labs(title = "Percent of Adults with Health Insurance by Race/Ethinicity", x="Race/Ethnicity", y="Percent") +
  theme_classic() +
  scale_fill_manual(labels = c("Black or African American only, non-Hispanic","American Indian or Alaska Native only, non-Hispanic","Asian only, non-Hispanic","Hispanic/Latino","Native Hawaiian or Other Pacific Islander only, non-Hispanic","Two or More Races only, non-Hispanic","Unknown","White only, non-Hispanic"), values = c(BRHD_cols[[5]],BRHD_cols[[6]],BRHD_cols[[1]],BRHD_cols[[7]],BRHD_cols[[3]],BRHD_cols[[2]],BRHD_cols[[4]],BRHD_cols[[8]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))

##Binge Drinking (At risk (5+ drinks for men, 4+ drinks for women, 1 or more times)) 
#Crude Rate Line Plot
#Cache
bnge_drnkng_cr <- plot_ly(risk_wide, x=~Year, y=~ca_bnge_drnkng_cr, type = 'scatter', mode = "marker", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
bnge_drnkng_cr <- bnge_drnkng_cr %>% add_trace(y = ~ca_bnge_drnkng_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
bnge_drnkng_cr <- bnge_drnkng_cr %>% add_trace(y = ~ca_bnge_drnkng_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
bnge_drnkng_cr <- bnge_drnkng_cr %>% add_trace(y = ~be_bnge_drnkng_cr, type = 'scatter', mode = "marker", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
bnge_drnkng_cr <- bnge_drnkng_cr %>% add_trace(y = ~be_bnge_drnkng_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
bnge_drnkng_cr <- bnge_drnkng_cr %>% add_trace(y = ~be_bnge_drnkng_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
bnge_drnkng_cr <- bnge_drnkng_cr %>% add_trace(y = ~br_bnge_drnkng_cr, type = 'scatter', mode = "marker", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
bnge_drnkng_cr <- bnge_drnkng_cr %>% add_trace(y = ~br_bnge_drnkng_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
bnge_drnkng_cr <- bnge_drnkng_cr %>% add_trace(y = ~br_bnge_drnkng_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
bnge_drnkng_cr <- bnge_drnkng_cr %>% layout(title = 'Percent of Adults Currently at Risk for Binge Drinking',
                                            xaxis = list(title = 'Year'),
                                            yaxis = list(title = 'Percent'))
#Age Adjusted Line plot
#Cache
aa_bnge_drnkng <- plot_ly(risk_wide, x=~Year, y=~ca_bnge_drnkng_aar, type = 'scatter', mode = "marker", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
aa_bnge_drnkng <- aa_bnge_drnkng %>% add_trace(y = ~ca_bnge_drnkng_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
aa_bnge_drnkng <- aa_bnge_drnkng %>% add_trace(y = ~ca_bnge_drnkng_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
aa_bnge_drnkng <- aa_bnge_drnkng %>% add_trace(y = ~be_bnge_drnkng_aar, type = 'scatter', mode = "marker", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
aa_bnge_drnkng <- aa_bnge_drnkng %>% add_trace(y = ~be_bnge_drnkng_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
aa_bnge_drnkng <- aa_bnge_drnkng %>% add_trace(y = ~be_bnge_drnkng_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
aa_bnge_drnkng <- aa_bnge_drnkng %>% add_trace(y = ~br_bnge_drnkng_aar, type = 'scatter', mode = "marker", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
aa_bnge_drnkng <- aa_bnge_drnkng %>% add_trace(y = ~br_bnge_drnkng_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
aa_bnge_drnkng <- aa_bnge_drnkng %>% add_trace(y = ~br_bnge_drnkng_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
aa_bnge_drnkng <- aa_bnge_drnkng %>% layout(title = 'Age-Adjusted Percent of Adults Currently at Risk for Binge Drinking',
                                            xaxis = list(title = 'Year'),
                                            yaxis = list(title = 'Percent'))
#Poverty bar graph
ins_pov <- ggplot(risk_pov, aes(Poverty_Status,hlcov_cr, fill = Poverty_Status)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = hlcov_cr_lci, ymax = hlcov_cr_uci), width = 0.2) +
  labs(title = "Percent of Adults with Health Insurance by Poverty", x="Poverty Status", y="Percent") +
  theme_classic() +
  scale_fill_manual(values = c(BRHD_cols[[1]], BRHD_cols[[3]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))
#Race bar graph
ins_race <- ggplot(risk_race, aes(Race_Ethnicity,hlcov_cr, fill= Race_Ethnicity)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = hlcov_cr_lci, ymax = hlcov_cr_uci), width=0.3, colour=BRHD_cols[[9]], alpha=0.9, size=1.3) +
  labs(title = "Percent of Adults with Health Insurance by Race/Ethinicity", x="Race/Ethnicity", y="Percent") +
  theme_classic() +
  scale_fill_manual(labels = c("Black or African American only, non-Hispanic","American Indian or Alaska Native only, non-Hispanic","Asian only, non-Hispanic","Hispanic/Latino","Native Hawaiian or Other Pacific Islander only, non-Hispanic","Two or More Races only, non-Hispanic","Unknown","White only, non-Hispanic"), values = c(BRHD_cols[[5]],BRHD_cols[[6]],BRHD_cols[[1]],BRHD_cols[[7]],BRHD_cols[[3]],BRHD_cols[[2]],BRHD_cols[[4]],BRHD_cols[[8]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))

#Seat Belt Use

st_blt_cr <- plot_ly(risk_wide, x=~Year, y=~ca_st_blt_cr, type = 'scatter', mode = "marker", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
st_blt_cr <- st_blt_cr %>% add_trace(y = ~ca_st_blt_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
st_blt_cr <- st_blt_cr %>% add_trace(y = ~ca_st_blt_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
st_blt_cr <- st_blt_cr %>% add_trace(y = ~be_st_blt_cr, type = 'scatter', mode = "marker", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
st_blt_cr <- st_blt_cr %>% add_trace(y = ~be_st_blt_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
st_blt_cr <- st_blt_cr %>% add_trace(y = ~be_st_blt_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
st_blt_cr <- st_blt_cr %>% add_trace(y = ~br_st_blt_cr, type = 'scatter', mode = "marker", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
st_blt_cr <- st_blt_cr %>% add_trace(y = ~br_st_blt_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
st_blt_cr <- st_blt_cr %>% add_trace(y = ~br_st_blt_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
st_blt_cr <- st_blt_cr %>% layout(title = 'Percent of Adults Who Always or Nearly Always Wear a Seat Belt',
                                  xaxis = list(title = 'Year'),
                                  yaxis = list(title = 'Percent'))


aa_st_blt <- plot_ly(risk_wide, x=~Year, y=~ca_st_blt_aar, type = 'scatter', mode = "marker", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
aa_st_blt <- aa_st_blt %>% add_trace(y = ~ca_st_blt_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
aa_st_blt <- aa_st_blt %>% add_trace(y = ~ca_st_blt_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
aa_st_blt <- aa_st_blt %>% add_trace(y = ~be_st_blt_aar, type = 'scatter', mode = "marker", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
aa_st_blt <- aa_st_blt %>% add_trace(y = ~be_st_blt_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
aa_st_blt <- aa_st_blt %>% add_trace(y = ~be_st_blt_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
aa_st_blt <- aa_st_blt %>% add_trace(y = ~br_st_blt_aar, type = 'scatter', mode = "marker", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
aa_st_blt <- aa_st_blt %>% add_trace(y = ~br_st_blt_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
aa_st_blt <- aa_st_blt %>% add_trace(y = ~br_st_blt_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
aa_st_blt <- aa_st_blt %>% layout(title = 'Age-Adjusted Percent of Adults Who Always or Nearly Always Wear a Seat Belt',
                                  xaxis = list(title = 'Year'),
                                  yaxis = list(title = 'Percent'))

#Poverty
ins_pov <- ggplot(risk_pov, aes(Poverty_Status,hlcov_cr, fill = Poverty_Status)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = hlcov_cr_lci, ymax = hlcov_cr_uci), width = 0.2) +
  labs(title = "Percent of Adults with Health Insurance by Poverty", x="Poverty Status", y="Percent") +
  theme_classic() +
  scale_fill_manual(values = c(BRHD_cols[[1]], BRHD_cols[[3]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))
#Race
ins_race <- ggplot(risk_race, aes(Race_Ethnicity,hlcov_cr, fill= Race_Ethnicity)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = hlcov_cr_lci, ymax = hlcov_cr_uci), width=0.3, colour=BRHD_cols[[9]], alpha=0.9, size=1.3) +
  labs(title = "Percent of Adults with Health Insurance by Race/Ethinicity", x="Race/Ethnicity", y="Percent") +
  theme_classic() +
  scale_fill_manual(labels = c("Black or African American only, non-Hispanic","American Indian or Alaska Native only, non-Hispanic","Asian only, non-Hispanic","Hispanic/Latino","Native Hawaiian or Other Pacific Islander only, non-Hispanic","Two or More Races only, non-Hispanic","Unknown","White only, non-Hispanic"), values = c(BRHD_cols[[5]],BRHD_cols[[6]],BRHD_cols[[1]],BRHD_cols[[7]],BRHD_cols[[3]],BRHD_cols[[2]],BRHD_cols[[4]],BRHD_cols[[8]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))

##############
###Outcomes###
##############


#Cache
inf_mort_rate <- plot_ly(outcomes_wide, x=~Year, y=~ca_inf_mort_rate, type = 'scatter', mode = "marker", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
inf_mort_rate <- inf_mort_rate %>% add_trace(y = ~ca_inf_mort_rate_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
inf_mort_rate <- inf_mort_rate %>% add_trace(y = ~ca_inf_mort_rate_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
inf_mort_rate <- inf_mort_rate %>% add_trace(y = ~be_inf_mort_rate, type = 'scatter', mode = "marker", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
inf_mort_rate <- inf_mort_rate %>% add_trace(y = ~be_inf_mort_rate_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
inf_mort_rate <- inf_mort_rate %>% add_trace(y = ~be_inf_mort_rate_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
inf_mort_rate <- inf_mort_rate %>% add_trace(y = ~br_inf_mort_rate, type = 'scatter', mode = "marker", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
inf_mort_rate <- inf_mort_rate %>% add_trace(y = ~br_inf_mort_rate_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
inf_mort_rate <- inf_mort_rate %>% add_trace(y = ~br_inf_mort_rate_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout: Adds titles and labels
inf_mort_rate <- inf_mort_rate %>% layout(title = 'Infant Mortality Rates by County',
                                          xaxis = list(title = 'Year'),
                                          yaxis = list(title = 'Mortality Rate (per 1,000)'))
#Poverty
inf_mort_rate_pov <- ggplot(outcomes_pov, aes(Poverty_Status,inf_mort_rate, fill = Poverty_Status)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = inf_mort_rate_lci, ymax = inf_mort_rate_uci), width = 0.2) +
  labs(title = "Infant Mortality Rate per 1,000 births by Poverty", x="Poverty Status", y="Mortality Rate (per 1,000)") +
  theme_classic() +
  scale_fill_manual(values = c(BRHD_cols[[1]], BRHD_cols[[3]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))
#Race
inf_mort_rate_race <- ggplot(outcomes_race, aes(Race_Ethnicity,inf_mort_rate, fill= Race_Ethnicity)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = inf_mort_rate_lci, ymax = inf_mort_rate_uci), width=0.3, colour=BRHD_cols[[9]], alpha=0.9, size=1.3) +
  labs(title = "Infant Mortality Rate per 1,000 births by Race/Ethinicity", x="Race/Ethnicity", y="Mortality Rate (per 1,000)") +
  theme_classic() +
  scale_fill_manual(labels = c("Black or African American only, non-Hispanic","American Indian or Alaska Native only, non-Hispanic","Asian only, non-Hispanic","Hispanic/Latino","Native Hawaiian or Other Pacific Islander only, non-Hispanic","Two or More Races only, non-Hispanic","Unknown","White only, non-Hispanic"), values = c(BRHD_cols[[5]],BRHD_cols[[6]],BRHD_cols[[1]],BRHD_cols[[7]],BRHD_cols[[3]],BRHD_cols[[2]],BRHD_cols[[4]],BRHD_cols[[8]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))

#Unintentional Injury
unint_inj_cr <- plot_ly(outcomes_wide, x=~Year, y=~ca_unint_inj_cr, type = 'scatter', mode = "lines", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
unint_inj_cr <- unint_inj_cr %>% add_trace(y = ~ca_unint_inj_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
unint_inj_cr <- unint_inj_cr %>% add_trace(y = ~ca_unint_inj_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
unint_inj_cr <- unint_inj_cr %>% add_trace(y = ~be_unint_inj_cr, type = 'scatter', mode = "lines", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
unint_inj_cr <- unint_inj_cr %>% add_trace(y = ~be_unint_inj_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
unint_inj_cr <- unint_inj_cr %>% add_trace(y = ~be_unint_inj_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
unint_inj_cr <- unint_inj_cr %>% add_trace(y = ~br_unint_inj_cr, type = 'scatter', mode = "lines", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
unint_inj_cr <- unint_inj_cr %>% add_trace(y = ~br_unint_inj_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
unint_inj_cr <- unint_inj_cr %>% add_trace(y = ~br_unint_inj_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
unint_inj_cr <- unint_inj_cr %>% layout(title = 'Unintentional Injury Mortality Rates by County',
                                        xaxis = list(title = 'Year'),
                                        yaxis = list(title = 'Mortality Rate (per 100,000)'))


aa_unint_inj <- plot_ly(outcomes_wide, x=~Year, y=~ca_unint_inj_aar, type = 'scatter', mode = "lines", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
aa_unint_inj <- aa_unint_inj %>% add_trace(y = ~ca_unint_inj_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
aa_unint_inj <- aa_unint_inj %>% add_trace(y = ~ca_unint_inj_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
aa_unint_inj <- aa_unint_inj %>% add_trace(y = ~be_unint_inj_aar, type = 'scatter', mode = "lines", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
aa_unint_inj <- aa_unint_inj %>% add_trace(y = ~be_unint_inj_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
aa_unint_inj <- aa_unint_inj %>% add_trace(y = ~be_unint_inj_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
aa_unint_inj <- aa_unint_inj %>% add_trace(y = ~br_unint_inj_aar, type = 'scatter', mode = "lines", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
aa_unint_inj <- aa_unint_inj %>% add_trace(y = ~br_unint_inj_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
aa_unint_inj <- aa_unint_inj %>% add_trace(y = ~br_unint_inj_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
aa_unint_inj <- aa_unint_inj %>% layout(title = 'Age-Adjusted Unintentional Injury Mortality Rates by County',
                                        xaxis = list(title = 'Year'),
                                        yaxis = list(title = 'Mortality Rate (per 100,000)'))
#Poverty
unint_inj_pov <- ggplot(outcomes_pov, aes(Poverty_Status,unint_inj, fill = Poverty_Status)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = unint_inj_lci, ymax = unint_inj_uci), width = 0.2) +
  labs(title = "Unintentional Injury Mortality Rates by County by Poverty", x="Poverty Status", y="Mortality Rate (per 100,000)") +
  theme_classic() +
  scale_fill_manual(values = c(BRHD_cols[[1]], BRHD_cols[[3]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))
#Race
unint_inj_race <- ggplot(outcomes_race, aes(Race_Ethnicity,unint_inj, fill= Race_Ethnicity)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = unint_inj_lci, ymax = unint_inj_uci), width=0.3, colour=BRHD_cols[[9]], alpha=0.9, size=1.3) +
  labs(title = "Unintentional Injury Mortality Rates by County by Race/Ethinicity", x="Race/Ethnicity", y="Mortality Rate (per 100,000)") +
  theme_classic() +
  scale_fill_manual(labels = c("Black or African American only, non-Hispanic","American Indian or Alaska Native only, non-Hispanic","Asian only, non-Hispanic","Hispanic/Latino","Native Hawaiian or Other Pacific Islander only, non-Hispanic","Two or More Races only, non-Hispanic","Unknown","White only, non-Hispanic"), values = c(BRHD_cols[[5]],BRHD_cols[[6]],BRHD_cols[[1]],BRHD_cols[[7]],BRHD_cols[[3]],BRHD_cols[[2]],BRHD_cols[[4]],BRHD_cols[[8]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))

#Motor vehicle 
#Cache
mvc_cr <- plot_ly(outcomes_wide, x=~Year, y=~ca_mvc_cr, type = 'scatter', mode = "marker", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
mvc_cr <- mvc_cr %>% add_trace(y = ~ca_mvc_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
mvc_cr <- mvc_cr %>% add_trace(y = ~ca_mvc_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
mvc_cr <- mvc_cr %>% add_trace(y = ~be_mvc_cr, type = 'scatter', mode = "marker", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
mvc_cr <- mvc_cr %>% add_trace(y = ~be_mvc_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
mvc_cr <- mvc_cr %>% add_trace(y = ~be_mvc_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
mvc_cr <- mvc_cr %>% add_trace(y = ~br_mvc_cr, type = 'scatter', mode = "marker", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
mvc_cr <- mvc_cr %>% add_trace(y = ~br_mvc_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
mvc_cr <- mvc_cr %>% add_trace(y = ~br_mvc_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
mvc_cr <- mvc_cr %>% layout(title = 'Motor Vehicle Mortality Rates by County',
                            xaxis = list(title = 'Year'),
                            yaxis = list(title = 'Mortality Rate (per 100,000)'))


aa_mvc <- plot_ly(outcomes_wide, x=~Year, y=~ca_mvc_aar, type = 'scatter', mode = "marker", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
aa_mvc <- aa_mvc %>% add_trace(y = ~ca_mvc_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
aa_mvc <- aa_mvc %>% add_trace(y = ~ca_mvc_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
aa_mvc <- aa_mvc %>% add_trace(y = ~be_mvc_aar, type = 'scatter', mode = "marker", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
aa_mvc <- aa_mvc %>% add_trace(y = ~be_mvc_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
aa_mvc <- aa_mvc %>% add_trace(y = ~be_mvc_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
aa_mvc <- aa_mvc %>% add_trace(y = ~br_mvc_aar, type = 'scatter', mode = "marker", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
aa_mvc <- aa_mvc %>% add_trace(y = ~br_mvc_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
aa_mvc <- aa_mvc %>% add_trace(y = ~br_mvc_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
aa_mvc <- aa_mvc %>% layout(title = 'Age-Adjusted Motor Vehicle Mortality Rates by County',
                            xaxis = list(title = 'Year'),
                            yaxis = list(title = 'Mortality Rate (per 100,000)'))
#Poverty
mvc_pov <- ggplot(outcomes_pov, aes(Poverty_Status,mvc_cr, fill = Poverty_Status)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mvc_cr_lci, ymax = mvc_cr_uci), width = 0.2) +
  labs(title = "Motor Vehicle Mortality Rates by Poverty", x="Poverty Status", y="Mortality Rate (per 100,000)") +
  theme_classic() +
  scale_fill_manual(values = c(BRHD_cols[[1]], BRHD_cols[[3]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))
#Race
mvc_race <- ggplot(outcomes_race, aes(Race_Ethnicity,mvc_cr, fill= Race_Ethnicity)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mvc_cr_lci, ymax = mvc_cr_uci), width=0.3, colour=BRHD_cols[[9]], alpha=0.9, size=1.3) +
  labs(title = "Motor Vehicle Mortality Rates by Race/Ethinicity", x="Race/Ethnicity", y="Mortality Rate (per 100,000)") +
  theme_classic() +
  scale_fill_manual(labels = c("Black or African American only, non-Hispanic","American Indian or Alaska Native only, non-Hispanic","Asian only, non-Hispanic","Hispanic/Latino","Native Hawaiian or Other Pacific Islander only, non-Hispanic","Two or More Races only, non-Hispanic","Unknown","White only, non-Hispanic"), values = c(BRHD_cols[[5]],BRHD_cols[[6]],BRHD_cols[[1]],BRHD_cols[[7]],BRHD_cols[[3]],BRHD_cols[[2]],BRHD_cols[[4]],BRHD_cols[[8]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))

#Drug Poisoning
#Cache
drug_poi_cr <- plot_ly(outcomes_wide, x=~Year, y=~ca_drug_poi_cr, type = 'scatter', mode = "marker", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
drug_poi_cr <- drug_poi_cr %>% add_trace(y = ~ca_drug_poi_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
drug_poi_cr <- drug_poi_cr %>% add_trace(y = ~ca_drug_poi_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
drug_poi_cr <- drug_poi_cr %>% add_trace(y = ~be_drug_poi_cr, type = 'scatter', mode = "marker", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
drug_poi_cr <- drug_poi_cr %>% add_trace(y = ~be_drug_poi_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
drug_poi_cr <- drug_poi_cr %>% add_trace(y = ~be_drug_poi_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
drug_poi_cr <- drug_poi_cr %>% add_trace(y = ~br_drug_poi_cr, type = 'scatter', mode = "marker", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
drug_poi_cr <- drug_poi_cr %>% add_trace(y = ~br_drug_poi_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
drug_poi_cr <- drug_poi_cr %>% add_trace(y = ~br_drug_poi_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
drug_poi_cr <- drug_poi_cr %>% layout(title = 'Drug Poisoning Mortality Rates by County',
                                      xaxis = list(title = 'Year'),
                                      yaxis = list(title = 'Mortality Rate (per 100,000)'))


aa_drug_poi <- plot_ly(outcomes_wide, x=~Year, y=~ca_drug_poi_aar, type = 'scatter', mode = "marker", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
aa_drug_poi <- aa_drug_poi %>% add_trace(y = ~ca_drug_poi_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
aa_drug_poi <- aa_drug_poi %>% add_trace(y = ~ca_drug_poi_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
aa_drug_poi <- aa_drug_poi %>% add_trace(y = ~be_drug_poi_aar, type = 'scatter', mode = "marker", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
aa_drug_poi <- aa_drug_poi %>% add_trace(y = ~be_drug_poi_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
aa_drug_poi <- aa_drug_poi %>% add_trace(y = ~be_drug_poi_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
aa_drug_poi <- aa_drug_poi %>% add_trace(y = ~br_drug_poi_aar, type = 'scatter', mode = "marker", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
aa_drug_poi <- aa_drug_poi %>% add_trace(y = ~br_drug_poi_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
aa_drug_poi <- aa_drug_poi %>% add_trace(y = ~br_drug_poi_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
aa_drug_poi <- aa_drug_poi %>% layout(title = 'Age-Adjusted Drug Poisoning Mortality Rates by County',
                                      xaxis = list(title = 'Year'),
                                      yaxis = list(title = 'Mortality Rate (per 100,000)'))
#Poverty
ins_pov <- ggplot(outcomes_pov, aes(Poverty_Status,hlcov_cr, fill = Poverty_Status)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = hlcov_cr_lci, ymax = hlcov_cr_uci), width = 0.2) +
  labs(title = "Percent of Adults with Health Insurance by Poverty", x="Poverty Status", y="Mortality Rate (per 100,000)") +
  theme_classic() +
  scale_fill_manual(values = c(BRHD_cols[[1]], BRHD_cols[[3]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))
#Race
ins_race <- ggplot(outcomes_race, aes(Race_Ethnicity,hlcov_cr, fill= Race_Ethnicity)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = hlcov_cr_lci, ymax = hlcov_cr_uci), width=0.3, colour=BRHD_cols[[9]], alpha=0.9, size=1.3) +
  labs(title = "Percent of Adults with Health Insurance by Race/Ethinicity", x="Race/Ethnicity", y="Mortality Rate (per 100,000)") +
  theme_classic() +
  scale_fill_manual(labels = c("Black or African American only, non-Hispanic","American Indian or Alaska Native only, non-Hispanic","Asian only, non-Hispanic","Hispanic/Latino","Native Hawaiian or Other Pacific Islander only, non-Hispanic","Two or More Races only, non-Hispanic","Unknown","White only, non-Hispanic"), values = c(BRHD_cols[[5]],BRHD_cols[[6]],BRHD_cols[[1]],BRHD_cols[[7]],BRHD_cols[[3]],BRHD_cols[[2]],BRHD_cols[[4]],BRHD_cols[[8]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))

#Falls
#Cache
falls_cr <- plot_ly(outcomes_wide, x=~Year, y=~ca_falls_cr, type = 'scatter', mode = "marker", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
falls_cr <- falls_cr %>% add_trace(y = ~ca_falls_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
falls_cr <- falls_cr %>% add_trace(y = ~ca_falls_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
falls_cr <- falls_cr %>% add_trace(y = ~be_falls_cr, type = 'scatter', mode = "marker", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
falls_cr <- falls_cr %>% add_trace(y = ~be_falls_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
falls_cr <- falls_cr %>% add_trace(y = ~be_falls_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
falls_cr <- falls_cr %>% add_trace(y = ~br_falls_cr, type = 'scatter', mode = "marker", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
falls_cr <- falls_cr %>% add_trace(y = ~br_falls_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
falls_cr <- falls_cr %>% add_trace(y = ~br_falls_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
falls_cr <- falls_cr %>% layout(title = 'Falls Mortality Rates by County',
                                xaxis = list(title = 'Year'),
                                yaxis = list(title = 'Mortality Rate (per 100,000)'))


aa_falls <- plot_ly(outcomes_wide, x=~Year, y=~ca_falls_aar, type = 'scatter', mode = "marker", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
aa_falls <- aa_falls %>% add_trace(y = ~ca_falls_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
aa_falls <- aa_falls %>% add_trace(y = ~ca_falls_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
aa_falls <- aa_falls %>% add_trace(y = ~be_falls_aar, type = 'scatter', mode = "marker", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
aa_falls <- aa_falls %>% add_trace(y = ~be_falls_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
aa_falls <- aa_falls %>% add_trace(y = ~be_falls_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
aa_falls <- aa_falls %>% add_trace(y = ~br_falls_aar, type = 'scatter', mode = "marker", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
aa_falls <- aa_falls %>% add_trace(y = ~br_falls_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
aa_falls <- aa_falls %>% add_trace(y = ~br_falls_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
aa_falls <- aa_falls %>% layout(title = 'Age-Adjusted Falls Mortality Rates by County',
                                xaxis = list(title = 'Year'),
                                yaxis = list(title = 'Mortality Rate (per 100,000)'))
#Poverty
ins_pov <- ggplot(outcomes_pov, aes(Poverty_Status,hlcov_cr, fill = Poverty_Status)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = hlcov_cr_lci, ymax = hlcov_cr_uci), width = 0.2) +
  labs(title = "Percent of Adults with Health Insurance by Poverty", x="Poverty Status", y="Mortality Rate (per 100,000)") +
  theme_classic() +
  scale_fill_manual(values = c(BRHD_cols[[1]], BRHD_cols[[3]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))
#Race
ins_race <- ggplot(outcomes_race, aes(Race_Ethnicity,hlcov_cr, fill= Race_Ethnicity)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = hlcov_cr_lci, ymax = hlcov_cr_uci), width=0.3, colour=BRHD_cols[[9]], alpha=0.9, size=1.3) +
  labs(title = "Percent of Adults with Health Insurance by Race/Ethinicity", x="Race/Ethnicity", y="Mortality Rate (per 100,000)") +
  theme_classic() +
  scale_fill_manual(labels = c("Black or African American only, non-Hispanic","American Indian or Alaska Native only, non-Hispanic","Asian only, non-Hispanic","Hispanic/Latino","Native Hawaiian or Other Pacific Islander only, non-Hispanic","Two or More Races only, non-Hispanic","Unknown","White only, non-Hispanic"), values = c(BRHD_cols[[5]],BRHD_cols[[6]],BRHD_cols[[1]],BRHD_cols[[7]],BRHD_cols[[3]],BRHD_cols[[2]],BRHD_cols[[4]],BRHD_cols[[8]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))

#Suicide
#Cache
sui_cr <- plot_ly(outcomes_wide, x=~Year, y=~ca_sui_cr, type = 'scatter', mode = "marker", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
sui_cr <- sui_cr %>% add_trace(y = ~ca_sui_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
sui_cr <- sui_cr %>% add_trace(y = ~ca_sui_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
sui_cr <- sui_cr %>% add_trace(y = ~be_sui_cr, type = 'scatter', mode = "marker", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
sui_cr <- sui_cr %>% add_trace(y = ~be_sui_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
sui_cr <- sui_cr %>% add_trace(y = ~be_sui_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
sui_cr <- sui_cr %>% add_trace(y = ~br_sui_cr, type = 'scatter', mode = "marker", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
sui_cr <- sui_cr %>% add_trace(y = ~br_sui_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
sui_cr <- sui_cr %>% add_trace(y = ~br_sui_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
sui_cr <- sui_cr %>% layout(title = 'Suicide Mortality Rates by County',
                            xaxis = list(title = 'Year'),
                            yaxis = list(title = 'Mortality Rate (per 100,000)'))


aa_sui <- plot_ly(outcomes_wide, x=~Year, y=~ca_sui_aar, type = 'scatter', mode = "marker", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
aa_sui <- aa_sui %>% add_trace(y = ~ca_sui_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
aa_sui <- aa_sui %>% add_trace(y = ~ca_sui_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
aa_sui <- aa_sui %>% add_trace(y = ~be_sui_aar, type = 'scatter', mode = "marker", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
aa_sui <- aa_sui %>% add_trace(y = ~be_sui_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
aa_sui <- aa_sui %>% add_trace(y = ~be_sui_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
aa_sui <- aa_sui %>% add_trace(y = ~br_sui_aar, type = 'scatter', mode = "marker", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
aa_sui <- aa_sui %>% add_trace(y = ~br_sui_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
aa_sui <- aa_sui %>% add_trace(y = ~br_sui_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
aa_sui <- aa_sui %>% layout(title = 'Age-Adjusted Suicide Mortality Rates by County',
                            xaxis = list(title = 'Year'),
                            yaxis = list(title = 'Mortality Rate (per 100,000)'))
#Poverty
ins_pov <- ggplot(outcomes_pov, aes(Poverty_Status,hlcov_cr, fill = Poverty_Status)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = hlcov_cr_lci, ymax = hlcov_cr_uci), width = 0.2) +
  labs(title = "Percent of Adults with Health Insurance by Poverty", x="Poverty Status", y="Mortality Rate (per 100,000)") +
  theme_classic() +
  scale_fill_manual(values = c(BRHD_cols[[1]], BRHD_cols[[3]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))
#Race
ins_race <- ggplot(outcomes_race, aes(Race_Ethnicity,hlcov_cr, fill= Race_Ethnicity)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = hlcov_cr_lci, ymax = hlcov_cr_uci), width=0.3, colour=BRHD_cols[[9]], alpha=0.9, size=1.3) +
  labs(title = "Percent of Adults with Health Insurance by Race/Ethinicity", x="Race/Ethnicity", y="Mortality Rate (per 100,000)") +
  theme_classic() +
  scale_fill_manual(labels = c("Black or African American only, non-Hispanic","American Indian or Alaska Native only, non-Hispanic","Asian only, non-Hispanic","Hispanic/Latino","Native Hawaiian or Other Pacific Islander only, non-Hispanic","Two or More Races only, non-Hispanic","Unknown","White only, non-Hispanic"), values = c(BRHD_cols[[5]],BRHD_cols[[6]],BRHD_cols[[1]],BRHD_cols[[7]],BRHD_cols[[3]],BRHD_cols[[2]],BRHD_cols[[4]],BRHD_cols[[8]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))

#Mort Diabetes
#Cache
mort_diab_cr <- plot_ly(outcomes_wide, x=~Year, y=~ca_mort_diab_cr, type = 'scatter', mode = "lines", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
mort_diab_cr <- mort_diab_cr %>% add_trace(y = ~ca_mort_diab_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
mort_diab_cr <- mort_diab_cr %>% add_trace(y = ~ca_mort_diab_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
mort_diab_cr <- mort_diab_cr %>% add_trace(y = ~be_mort_diab_cr, type = 'scatter', mode = "lines", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
mort_diab_cr <- mort_diab_cr %>% add_trace(y = ~be_mort_diab_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
mort_diab_cr <- mort_diab_cr %>% add_trace(y = ~be_mort_diab_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
mort_diab_cr <- mort_diab_cr %>% add_trace(y = ~br_mort_diab_cr, type = 'scatter', mode = "lines", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
mort_diab_cr <- mort_diab_cr %>% add_trace(y = ~br_mort_diab_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
mort_diab_cr <- mort_diab_cr %>% add_trace(y = ~br_mort_diab_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
mort_diab_cr <- mort_diab_cr %>% layout(title = 'Diabetes Mortality Rates by County',
                                        xaxis = list(title = 'Year'),
                                        yaxis = list(title = 'Mortality Rate (per 100,000)'))


aa_mort_diab <- plot_ly(outcomes_wide, x=~Year, y=~ca_mort_diab_aar, type = 'scatter', mode = "lines", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
aa_mort_diab <- aa_mort_diab %>% add_trace(y = ~ca_mort_diab_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
aa_mort_diab <- aa_mort_diab %>% add_trace(y = ~ca_mort_diab_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
aa_mort_diab <- aa_mort_diab %>% add_trace(y = ~be_mort_diab_aar, type = 'scatter', mode = "lines", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
aa_mort_diab <- aa_mort_diab %>% add_trace(y = ~be_mort_diab_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
aa_mort_diab <- aa_mort_diab %>% add_trace(y = ~be_mort_diab_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
aa_mort_diab <- aa_mort_diab %>% add_trace(y = ~br_mort_diab_aar, type = 'scatter', mode = "lines", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
aa_mort_diab <- aa_mort_diab %>% add_trace(y = ~br_mort_diab_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
aa_mort_diab <- aa_mort_diab %>% add_trace(y = ~br_mort_diab_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
aa_mort_diab <- aa_mort_diab %>% layout(title = 'Age-Adjusted Diabetes Mortality Rates by County',
                                        xaxis = list(title = 'Year'),
                                        yaxis = list(title = 'Mortality Rate (per 100,000)'))
#Poverty
ins_pov <- ggplot(outcomes_pov, aes(Poverty_Status,hlcov_cr, fill = Poverty_Status)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = hlcov_cr_lci, ymax = hlcov_cr_uci), width = 0.2) +
  labs(title = "Percent of Adults with Health Insurance by Poverty", x="Poverty Status", y="Mortality Rate (per 100,000)") +
  theme_classic() +
  scale_fill_manual(values = c(BRHD_cols[[1]], BRHD_cols[[3]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))
#Race
ins_race <- ggplot(outcomes_race, aes(Race_Ethnicity,hlcov_cr, fill= Race_Ethnicity)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = hlcov_cr_lci, ymax = hlcov_cr_uci), width=0.3, colour=BRHD_cols[[9]], alpha=0.9, size=1.3) +
  labs(title = "Percent of Adults with Health Insurance by Race/Ethinicity", x="Race/Ethnicity", y="Mortality Rate (per 100,000)") +
  theme_classic() +
  scale_fill_manual(labels = c("Black or African American only, non-Hispanic","American Indian or Alaska Native only, non-Hispanic","Asian only, non-Hispanic","Hispanic/Latino","Native Hawaiian or Other Pacific Islander only, non-Hispanic","Two or More Races only, non-Hispanic","Unknown","White only, non-Hispanic"), values = c(BRHD_cols[[5]],BRHD_cols[[6]],BRHD_cols[[1]],BRHD_cols[[7]],BRHD_cols[[3]],BRHD_cols[[2]],BRHD_cols[[4]],BRHD_cols[[8]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))

#Mort Cardiovascular
#Cache
mort_cvs_cr <- plot_ly(outcomes_wide, x=~Year, y=~ca_mort_cvs_cr, type = 'scatter', mode = "lines", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
mort_cvs_cr <- mort_cvs_cr %>% add_trace(y = ~ca_mort_cvs_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
mort_cvs_cr <- mort_cvs_cr %>% add_trace(y = ~ca_mort_cvs_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
mort_cvs_cr <- mort_cvs_cr %>% add_trace(y = ~be_mort_cvs_cr, type = 'scatter', mode = "lines", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
mort_cvs_cr <- mort_cvs_cr %>% add_trace(y = ~be_mort_cvs_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
mort_cvs_cr <- mort_cvs_cr %>% add_trace(y = ~be_mort_cvs_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
mort_cvs_cr <- mort_cvs_cr %>% add_trace(y = ~br_mort_cvs_cr, type = 'scatter', mode = "lines", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
mort_cvs_cr <- mort_cvs_cr %>% add_trace(y = ~br_mort_cvs_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
mort_cvs_cr <- mort_cvs_cr %>% add_trace(y = ~br_mort_cvs_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
mort_cvs_cr <- mort_cvs_cr %>% layout(title = 'Cerebrovascular Mortality Rates by County',
                                      xaxis = list(title = 'Year'),
                                      yaxis = list(title = 'Mortality Rate (per 100,000)'))


aa_mort_cvs <- plot_ly(outcomes_wide, x=~Year, y=~ca_mort_cvs_aar, type = 'scatter', mode = "lines", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
aa_mort_cvs <- aa_mort_cvs %>% add_trace(y = ~ca_mort_cvs_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
aa_mort_cvs <- aa_mort_cvs %>% add_trace(y = ~ca_mort_cvs_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
aa_mort_cvs <- aa_mort_cvs %>% add_trace(y = ~be_mort_cvs_aar, type = 'scatter', mode = "lines", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
aa_mort_cvs <- aa_mort_cvs %>% add_trace(y = ~be_mort_cvs_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
aa_mort_cvs <- aa_mort_cvs %>% add_trace(y = ~be_mort_cvs_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
aa_mort_cvs <- aa_mort_cvs %>% add_trace(y = ~br_mort_cvs_aar, type = 'scatter', mode = "lines", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
aa_mort_cvs <- aa_mort_cvs %>% add_trace(y = ~br_mort_cvs_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
aa_mort_cvs <- aa_mort_cvs %>% add_trace(y = ~br_mort_cvs_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
aa_mort_cvs <- aa_mort_cvs %>% layout(title = 'Age-Adjusted Cerebrovascular Mortality Rates by County',
                                      xaxis = list(title = 'Year'),
                                      yaxis = list(title = 'Mortality Rate (per 100,000)'))
#Poverty
ins_pov <- ggplot(outcomes_pov, aes(Poverty_Status,hlcov_cr, fill = Poverty_Status)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = hlcov_cr_lci, ymax = hlcov_cr_uci), width = 0.2) +
  labs(title = "Percent of Adults with Health Insurance by Poverty", x="Poverty Status", y="Mortality Rate (per 100,000)") +
  theme_classic() +
  scale_fill_manual(values = c(BRHD_cols[[1]], BRHD_cols[[3]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))
#Race
ins_race <- ggplot(outcomes_race, aes(Race_Ethnicity,hlcov_cr, fill= Race_Ethnicity)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = hlcov_cr_lci, ymax = hlcov_cr_uci), width=0.3, colour=BRHD_cols[[9]], alpha=0.9, size=1.3) +
  labs(title = "Percent of Adults with Health Insurance by Race/Ethinicity", x="Race/Ethnicity", y="Mortality Rate (per 100,000)") +
  theme_classic() +
  scale_fill_manual(labels = c("Black or African American only, non-Hispanic","American Indian or Alaska Native only, non-Hispanic","Asian only, non-Hispanic","Hispanic/Latino","Native Hawaiian or Other Pacific Islander only, non-Hispanic","Two or More Races only, non-Hispanic","Unknown","White only, non-Hispanic"), values = c(BRHD_cols[[5]],BRHD_cols[[6]],BRHD_cols[[1]],BRHD_cols[[7]],BRHD_cols[[3]],BRHD_cols[[2]],BRHD_cols[[4]],BRHD_cols[[8]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))

#Mort Cancer
#Cache
mort_can_cr <- plot_ly(outcomes_wide, x=~Year, y=~ca_mort_can_cr, type = 'scatter', mode = "marker", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
mort_can_cr <- mort_can_cr %>% add_trace(y = ~ca_mort_can_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
mort_can_cr <- mort_can_cr %>% add_trace(y = ~ca_mort_can_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
mort_can_cr <- mort_can_cr %>% add_trace(y = ~be_mort_can_cr, type = 'scatter', mode = "lines", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
mort_can_cr <- mort_can_cr %>% add_trace(y = ~be_mort_can_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
mort_can_cr <- mort_can_cr %>% add_trace(y = ~be_mort_can_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
mort_can_cr <- mort_can_cr %>% add_trace(y = ~br_mort_can_cr, type = 'scatter', mode = "lines", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
mort_can_cr <- mort_can_cr %>% add_trace(y = ~br_mort_can_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
mort_can_cr <- mort_can_cr %>% add_trace(y = ~br_mort_can_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
mort_can_cr <- mort_can_cr %>% layout(title = 'Cancer Mortality Rates by County',
                                      xaxis = list(title = 'Year'),
                                      yaxis = list(title = 'Mortality Rate (per 100,000)'))


aa_mort_can <- plot_ly(outcomes_wide, x=~Year, y=~ca_mort_can_aar, type = 'scatter', mode = "lines", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
aa_mort_can <- aa_mort_can %>% add_trace(y = ~ca_mort_can_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
aa_mort_can <- aa_mort_can %>% add_trace(y = ~ca_mort_can_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
aa_mort_can <- aa_mort_can %>% add_trace(y = ~be_mort_can_aar, type = 'scatter', mode = "lines", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
aa_mort_can <- aa_mort_can %>% add_trace(y = ~be_mort_can_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
aa_mort_can <- aa_mort_can %>% add_trace(y = ~be_mort_can_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
aa_mort_can <- aa_mort_can %>% add_trace(y = ~br_mort_can_aar, type = 'scatter', mode = "lines", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
aa_mort_can <- aa_mort_can %>% add_trace(y = ~br_mort_can_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
aa_mort_can <- aa_mort_can %>% add_trace(y = ~br_mort_can_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
aa_mort_can <- aa_mort_can %>% layout(title = 'Age-Adjusted Cancer Mortality Rates by County',
                                      xaxis = list(title = 'Year'),
                                      yaxis = list(title = 'Mortality Rate (per 100,000)'))
#Poverty
ins_pov <- ggplot(outcomes_pov, aes(Poverty_Status,hlcov_cr, fill = Poverty_Status)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = hlcov_cr_lci, ymax = hlcov_cr_uci), width = 0.2) +
  labs(title = "Percent of Adults with Health Insurance by Poverty", x="Poverty Status", y="Mortality Rate (per 100,000)") +
  theme_classic() +
  scale_fill_manual(values = c(BRHD_cols[[1]], BRHD_cols[[3]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))
#Race
ins_race <- ggplot(outcomes_race, aes(Race_Ethnicity,hlcov_cr, fill= Race_Ethnicity)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = hlcov_cr_lci, ymax = hlcov_cr_uci), width=0.3, colour=BRHD_cols[[9]], alpha=0.9, size=1.3) +
  labs(title = "Percent of Adults with Health Insurance by Race/Ethinicity", x="Race/Ethnicity", y="Mortality Rate (per 100,000)") +
  theme_classic() +
  scale_fill_manual(labels = c("Black or African American only, non-Hispanic","American Indian or Alaska Native only, non-Hispanic","Asian only, non-Hispanic","Hispanic/Latino","Native Hawaiian or Other Pacific Islander only, non-Hispanic","Two or More Races only, non-Hispanic","Unknown","White only, non-Hispanic"), values = c(BRHD_cols[[5]],BRHD_cols[[6]],BRHD_cols[[1]],BRHD_cols[[7]],BRHD_cols[[3]],BRHD_cols[[2]],BRHD_cols[[4]],BRHD_cols[[8]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))

#Mort Lung Cancer
#Cache
mort_lung_can <- plot_ly(outcomes_wide, x=~Year, y=~ca_mort_lung_can_cr, type = 'scatter', mode = "marker", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
mort_lung_can <- mort_lung_can %>% add_trace(y = ~ca_mort_lung_can_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
mort_lung_can <- mort_lung_can %>% add_trace(y = ~ca_mort_lung_can_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
mort_lung_can <- mort_lung_can %>% add_trace(y = ~be_mort_lung_can_cr, type = 'scatter', mode = "marker", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
mort_lung_can <- mort_lung_can %>% add_trace(y = ~be_mort_lung_can_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
mort_lung_can <- mort_lung_can %>% add_trace(y = ~be_mort_lung_can_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
mort_lung_can <- mort_lung_can %>% add_trace(y = ~br_mort_lung_can_cr, type = 'scatter', mode = "marker", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
mort_lung_can <- mort_lung_can %>% add_trace(y = ~br_mort_lung_can_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
mort_lung_can <- mort_lung_can %>% add_trace(y = ~br_mort_lung_can_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
mort_lung_can <- mort_lung_can %>% layout(title = 'Lung Cancer Mortality Rates by County',
                                          xaxis = list(title = 'Year'),
                                          yaxis = list(title = 'Mortality Rate (per 100,000)'))


aa_mort_lung_can <- plot_ly(outcomes_wide, x=~Year, y=~ca_mort_lung_can_aar, type = 'scatter', mode = "marker", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
aa_mort_lung_can <- aa_mort_lung_can %>% add_trace(y = ~ca_mort_lung_can_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
aa_mort_lung_can <- aa_mort_lung_can %>% add_trace(y = ~ca_mort_lung_can_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
aa_mort_lung_can <- aa_mort_lung_can %>% add_trace(y = ~be_mort_lung_can_aar, type = 'scatter', mode = "marker", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
aa_mort_lung_can <- aa_mort_lung_can %>% add_trace(y = ~be_mort_lung_can_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
aa_mort_lung_can <- aa_mort_lung_can %>% add_trace(y = ~be_mort_lung_can_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
aa_mort_lung_can <- aa_mort_lung_can %>% add_trace(y = ~br_mort_lung_can_aar, type = 'scatter', mode = "marker", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
aa_mort_lung_can <- aa_mort_lung_can %>% add_trace(y = ~br_mort_lung_can_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
aa_mort_lung_can <- aa_mort_lung_can %>% add_trace(y = ~br_mort_lung_can_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
aa_mort_lung_can <- aa_mort_lung_can %>% layout(title = 'Age-Adjusted Lung Cancer Mortality Rates by County',
                                                xaxis = list(title = 'Year'),
                                                yaxis = list(title = 'Mortality Rate (per 100,000)'))
#Poverty
ins_pov <- ggplot(outcomes_pov, aes(Poverty_Status,hlcov_cr, fill = Poverty_Status)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = hlcov_cr_lci, ymax = hlcov_cr_uci), width = 0.2) +
  labs(title = "Percent of Adults with Health Insurance by Poverty", x="Poverty Status", y="Mortality Rate (per 100,000)") +
  theme_classic() +
  scale_fill_manual(values = c(BRHD_cols[[1]], BRHD_cols[[3]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))
#Race
ins_race <- ggplot(outcomes_race, aes(Race_Ethnicity,hlcov_cr, fill= Race_Ethnicity)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = hlcov_cr_lci, ymax = hlcov_cr_uci), width=0.3, colour=BRHD_cols[[9]], alpha=0.9, size=1.3) +
  labs(title = "Percent of Adults with Health Insurance by Race/Ethinicity", x="Race/Ethnicity", y="Mortality Rate (per 100,000)") +
  theme_classic() +
  scale_fill_manual(labels = c("Black or African American only, non-Hispanic","American Indian or Alaska Native only, non-Hispanic","Asian only, non-Hispanic","Hispanic/Latino","Native Hawaiian or Other Pacific Islander only, non-Hispanic","Two or More Races only, non-Hispanic","Unknown","White only, non-Hispanic"), values = c(BRHD_cols[[5]],BRHD_cols[[6]],BRHD_cols[[1]],BRHD_cols[[7]],BRHD_cols[[3]],BRHD_cols[[2]],BRHD_cols[[4]],BRHD_cols[[8]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))

#Mort Breast Cancer
#Cache
mort_brst_can_cr <- plot_ly(outcomes_wide, x=~Year, y=~ca_mort_brst_can_cr, type = 'scatter', mode = "marker", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
mort_brst_can_cr <- mort_brst_can_cr %>% add_trace(y = ~ca_mort_brst_can_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
mort_brst_can_cr <- mort_brst_can_cr %>% add_trace(y = ~ca_mort_brst_can_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
mort_brst_can_cr <- mort_brst_can_cr %>% add_trace(y = ~be_mort_brst_can_cr, type = 'scatter', mode = "marker", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
mort_brst_can_cr <- mort_brst_can_cr %>% add_trace(y = ~be_mort_brst_can_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
mort_brst_can_cr <- mort_brst_can_cr %>% add_trace(y = ~be_mort_brst_can_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
mort_brst_can_cr <- mort_brst_can_cr %>% add_trace(y = ~br_mort_brst_can_cr, type = 'scatter', mode = "marker", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
mort_brst_can_cr <- mort_brst_can_cr %>% add_trace(y = ~br_mort_brst_can_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
mort_brst_can_cr <- mort_brst_can_cr %>% add_trace(y = ~br_mort_brst_can_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
mort_brst_can_cr <- mort_brst_can_cr %>% layout(title = 'Breast Cancer Mortality Rates by County',
                                                xaxis = list(title = 'Year'),
                                                yaxis = list(title = 'Mortality Rate (per 100,000)'))


aa_mort_brst_can <- plot_ly(outcomes_wide, x=~Year, y=~ca_mort_brst_can_aar, type = 'scatter', mode = "marker", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
aa_mort_brst_can <- aa_mort_brst_can %>% add_trace(y = ~ca_mort_brst_can_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
aa_mort_brst_can <- aa_mort_brst_can %>% add_trace(y = ~ca_mort_brst_can_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
aa_mort_brst_can <- aa_mort_brst_can %>% add_trace(y = ~be_mort_brst_can_aar, type = 'scatter', mode = "marker", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
aa_mort_brst_can <- aa_mort_brst_can %>% add_trace(y = ~be_mort_brst_can_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
aa_mort_brst_can <- aa_mort_brst_can %>% add_trace(y = ~be_mort_brst_can_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
aa_mort_brst_can <- aa_mort_brst_can %>% add_trace(y = ~br_mort_brst_can_aar, type = 'scatter', mode = "marker", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
aa_mort_brst_can <- aa_mort_brst_can %>% add_trace(y = ~br_mort_brst_can_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
aa_mort_brst_can <- aa_mort_brst_can %>% add_trace(y = ~br_mort_brst_can_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
aa_mort_brst_can <- aa_mort_brst_can %>% layout(title = 'Age-Adjusted Breast Cancer Mortality Rates by County',
                                                xaxis = list(title = 'Year'),
                                                yaxis = list(title = 'Mortality Rate (per 100,000)'))
#Poverty
ins_pov <- ggplot(outcomes_pov, aes(Poverty_Status,hlcov_cr, fill = Poverty_Status)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = hlcov_cr_lci, ymax = hlcov_cr_uci), width = 0.2) +
  labs(title = "Percent of Adults with Health Insurance by Poverty", x="Poverty Status", y="Mortality Rate (per 100,000)") +
  theme_classic() +
  scale_fill_manual(values = c(BRHD_cols[[1]], BRHD_cols[[3]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))
#Race
ins_race <- ggplot(outcomes_race, aes(Race_Ethnicity,hlcov_cr, fill= Race_Ethnicity)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = hlcov_cr_lci, ymax = hlcov_cr_uci), width=0.3, colour=BRHD_cols[[9]], alpha=0.9, size=1.3) +
  labs(title = "Percent of Adults with Health Insurance by Race/Ethinicity", x="Race/Ethnicity", y="Mortality Rate (per 100,000)") +
  theme_classic() +
  scale_fill_manual(labels = c("Black or African American only, non-Hispanic","American Indian or Alaska Native only, non-Hispanic","Asian only, non-Hispanic","Hispanic/Latino","Native Hawaiian or Other Pacific Islander only, non-Hispanic","Two or More Races only, non-Hispanic","Unknown","White only, non-Hispanic"), values = c(BRHD_cols[[5]],BRHD_cols[[6]],BRHD_cols[[1]],BRHD_cols[[7]],BRHD_cols[[3]],BRHD_cols[[2]],BRHD_cols[[4]],BRHD_cols[[8]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))

#Mort Colorectal Cancer
#Cache
mort_colrect_can_cr <- plot_ly(outcomes_wide, x=~Year, y=~ca_mort_colrect_can_cr, type = 'scatter', mode = "marker", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
mort_colrect_can_cr <- mort_colrect_can_cr %>% add_trace(y = ~ca_mort_colrect_can_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
mort_colrect_can_cr <- mort_colrect_can_cr %>% add_trace(y = ~ca_mort_colrect_can_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
mort_colrect_can_cr <- mort_colrect_can_cr %>% add_trace(y = ~be_mort_colrect_can_cr, type = 'scatter', mode = "marker", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
mort_colrect_can_cr <- mort_colrect_can_cr %>% add_trace(y = ~be_mort_colrect_can_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
mort_colrect_can_cr <- mort_colrect_can_cr %>% add_trace(y = ~be_mort_colrect_can_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
mort_colrect_can_cr <- mort_colrect_can_cr %>% add_trace(y = ~br_mort_colrect_can_cr, type = 'scatter', mode = "marker", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
mort_colrect_can_cr <- mort_colrect_can_cr %>% add_trace(y = ~br_mort_colrect_can_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
mort_colrect_can_cr <- mort_colrect_can_cr %>% add_trace(y = ~br_mort_colrect_can_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
mort_colrect_can_cr <- mort_colrect_can_cr %>% layout(title = 'Colorectal Cancer Mortality Rates by County',
                                                      xaxis = list(title = 'Year'),
                                                      yaxis = list(title = 'Mortality Rate (per 100,000)'))

aa_mort_colrect_can <- plot_ly(outcomes_wide, x=~Year, y=~ca_mort_colrect_can_aar, type = 'scatter', mode = "marker", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
aa_mort_colrect_can <- aa_mort_colrect_can %>% add_trace(y = ~ca_mort_colrect_can_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
aa_mort_colrect_can <- aa_mort_colrect_can %>% add_trace(y = ~ca_mort_colrect_can_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
aa_mort_colrect_can <- aa_mort_colrect_can %>% add_trace(y = ~be_mort_colrect_can_aar, type = 'scatter', mode = "marker", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
aa_mort_colrect_can <- aa_mort_colrect_can %>% add_trace(y = ~be_mort_colrect_can_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
aa_mort_colrect_can <- aa_mort_colrect_can %>% add_trace(y = ~be_mort_colrect_can_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
aa_mort_colrect_can <- aa_mort_colrect_can %>% add_trace(y = ~br_mort_colrect_can_aar, type = 'scatter', mode = "marker", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
aa_mort_colrect_can <- aa_mort_colrect_can %>% add_trace(y = ~br_mort_colrect_can_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
aa_mort_colrect_can <- aa_mort_colrect_can %>% add_trace(y = ~br_mort_colrect_can_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
aa_mort_colrect_can <- aa_mort_colrect_can %>% layout(title = 'Age-Adjusted Colorectal Cancer Mortality Rates by County',
                                                      xaxis = list(title = 'Year'),
                                                      yaxis = list(title = 'Mortality Rate (per 100,000)'))
#Poverty
ins_pov <- ggplot(outcomes_pov, aes(Poverty_Status,hlcov_cr, fill = Poverty_Status)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = hlcov_cr_lci, ymax = hlcov_cr_uci), width = 0.2) +
  labs(title = "Percent of Adults with Health Insurance by Poverty", x="Poverty Status", y="Mortality Rate (per 100,000)") +
  theme_classic() +
  scale_fill_manual(values = c(BRHD_cols[[1]], BRHD_cols[[3]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))
#Race
ins_race <- ggplot(outcomes_race, aes(Race_Ethnicity,hlcov_cr, fill= Race_Ethnicity)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = hlcov_cr_lci, ymax = hlcov_cr_uci), width=0.3, colour=BRHD_cols[[9]], alpha=0.9, size=1.3) +
  labs(title = "Percent of Adults with Health Insurance by Race/Ethinicity", x="Race/Ethnicity", y="Mortality Rate (per 100,000)") +
  theme_classic() +
  scale_fill_manual(labels = c("Black or African American only, non-Hispanic","American Indian or Alaska Native only, non-Hispanic","Asian only, non-Hispanic","Hispanic/Latino","Native Hawaiian or Other Pacific Islander only, non-Hispanic","Two or More Races only, non-Hispanic","Unknown","White only, non-Hispanic"), values = c(BRHD_cols[[5]],BRHD_cols[[6]],BRHD_cols[[1]],BRHD_cols[[7]],BRHD_cols[[3]],BRHD_cols[[2]],BRHD_cols[[4]],BRHD_cols[[8]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))

#Mort Prostate Cancer
#Cache
mort_pros_can_cr <- plot_ly(outcomes_wide, x=~Year, y=~ca_mort_pros_can_cr, type = 'scatter', mode = "marker", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
mort_pros_can_cr <- mort_pros_can_cr %>% add_trace(y = ~ca_mort_pros_can_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
mort_pros_can_cr <- mort_pros_can_cr %>% add_trace(y = ~ca_mort_pros_can_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
mort_pros_can_cr <- mort_pros_can_cr %>% add_trace(y = ~be_mort_pros_can_cr, type = 'scatter', mode = "marker", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
mort_pros_can_cr <- mort_pros_can_cr %>% add_trace(y = ~be_mort_pros_can_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
mort_pros_can_cr <- mort_pros_can_cr %>% add_trace(y = ~be_mort_pros_can_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
mort_pros_can_cr <- mort_pros_can_cr %>% add_trace(y = ~br_mort_pros_can_cr, type = 'scatter', mode = "marker", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
mort_pros_can_cr <- mort_pros_can_cr %>% add_trace(y = ~br_mort_pros_can_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
mort_pros_can_cr <- mort_pros_can_cr %>% add_trace(y = ~br_mort_pros_can_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
mort_pros_can_cr <- mort_pros_can_cr %>% layout(title = 'Prostate Cancer Mortality Rates by County',
                                                xaxis = list(title = 'Year'),
                                                yaxis = list(title = 'Mortality Rate (per 100,000)'))


aa_mort_pros_can <- plot_ly(outcomes_wide, x=~Year, y=~ca_mort_pros_can_aar, type = 'scatter', mode = "marker", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
aa_mort_pros_can <- aa_mort_pros_can %>% add_trace(y = ~ca_mort_pros_can_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
aa_mort_pros_can <- aa_mort_pros_can %>% add_trace(y = ~ca_mort_pros_can_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
aa_mort_pros_can <- aa_mort_pros_can %>% add_trace(y = ~be_mort_pros_can_aar, type = 'scatter', mode = "marker", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
aa_mort_pros_can <- aa_mort_pros_can %>% add_trace(y = ~be_mort_pros_can_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
aa_mort_pros_can <- aa_mort_pros_can %>% add_trace(y = ~be_mort_pros_can_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
aa_mort_pros_can <- aa_mort_pros_can %>% add_trace(y = ~br_mort_pros_can_aar, type = 'scatter', mode = "marker", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
aa_mort_pros_can <- aa_mort_pros_can %>% add_trace(y = ~br_mort_pros_can_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
aa_mort_pros_can <- aa_mort_pros_can %>% add_trace(y = ~br_mort_pros_can_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
aa_mort_pros_can <- aa_mort_pros_can %>% layout(title = 'Age-Adjusted Prostate Cancer Mortality Rates by County',
                                                xaxis = list(title = 'Year'),
                                                yaxis = list(title = 'Mortality Rate (per 100,000)'))
#Poverty
ins_pov <- ggplot(outcomes_pov, aes(Poverty_Status,hlcov_cr, fill = Poverty_Status)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = hlcov_cr_lci, ymax = hlcov_cr_uci), width = 0.2) +
  labs(title = "Percent of Adults with Health Insurance by Poverty", x="Poverty Status", y="Mortality Rate (per 100,000)") +
  theme_classic() +
  scale_fill_manual(values = c(BRHD_cols[[1]], BRHD_cols[[3]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))
#Race
ins_race <- ggplot(outcomes_race, aes(Race_Ethnicity,hlcov_cr, fill= Race_Ethnicity)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = hlcov_cr_lci, ymax = hlcov_cr_uci), width=0.3, colour=BRHD_cols[[9]], alpha=0.9, size=1.3) +
  labs(title = "Percent of Adults with Health Insurance by Race/Ethinicity", x="Race/Ethnicity", y="Mortality Rate (per 100,000)") +
  theme_classic() +
  scale_fill_manual(labels = c("Black or African American only, non-Hispanic","American Indian or Alaska Native only, non-Hispanic","Asian only, non-Hispanic","Hispanic/Latino","Native Hawaiian or Other Pacific Islander only, non-Hispanic","Two or More Races only, non-Hispanic","Unknown","White only, non-Hispanic"), values = c(BRHD_cols[[5]],BRHD_cols[[6]],BRHD_cols[[1]],BRHD_cols[[7]],BRHD_cols[[3]],BRHD_cols[[2]],BRHD_cols[[4]],BRHD_cols[[8]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))

#Mort Skin Cancer
#Cache
mort_skin_can_cr <- plot_ly(outcomes_wide, x=~Year, y=~ca_mort_skin_can_cr, type = 'scatter', mode = "marker", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
mort_skin_can_cr <- mort_skin_can_cr %>% add_trace(y = ~ca_mort_skin_can_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
mort_skin_can_cr <- mort_skin_can_cr %>% add_trace(y = ~ca_mort_skin_can_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
mort_skin_can_cr <- mort_skin_can_cr %>% add_trace(y = ~be_mort_skin_can_cr, type = 'scatter', mode = "marker", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
mort_skin_can_cr <- mort_skin_can_cr %>% add_trace(y = ~be_mort_skin_can_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
mort_skin_can_cr <- mort_skin_can_cr %>% add_trace(y = ~be_mort_skin_can_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
mort_skin_can_cr <- mort_skin_can_cr %>% add_trace(y = ~br_mort_skin_can_cr, type = 'scatter', mode = "marker", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
mort_skin_can_cr <- mort_skin_can_cr %>% add_trace(y = ~br_mort_skin_can_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
mort_skin_can_cr <- mort_skin_can_cr %>% add_trace(y = ~br_mort_skin_can_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
mort_skin_can_cr <- mort_skin_can_cr %>% layout(title = 'Skin Cancer Mortality Rates by County',
                                                xaxis = list(title = 'Year'),
                                                yaxis = list(title = 'Mortality Rate (per 100,000)'))


aa_mort_skin_can <- plot_ly(outcomes_wide, x=~Year, y=~ca_mort_skin_can_aar, type = 'scatter', mode = "marker", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
aa_mort_skin_can <- aa_mort_skin_can %>% add_trace(y = ~ca_mort_skin_can_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
aa_mort_skin_can <- aa_mort_skin_can %>% add_trace(y = ~ca_mort_skin_can_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
aa_mort_skin_can <- aa_mort_skin_can %>% add_trace(y = ~be_mort_skin_can_aar, type = 'scatter', mode = "marker", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
aa_mort_skin_can <- aa_mort_skin_can %>% add_trace(y = ~be_mort_skin_can_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
aa_mort_skin_can <- aa_mort_skin_can %>% add_trace(y = ~be_mort_skin_can_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
aa_mort_skin_can <- aa_mort_skin_can %>% add_trace(y = ~br_mort_skin_can_aar, type = 'scatter', mode = "marker", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
aa_mort_skin_can <- aa_mort_skin_can %>% add_trace(y = ~br_mort_skin_can_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
aa_mort_skin_can <- aa_mort_skin_can %>% add_trace(y = ~br_mort_skin_can_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
aa_mort_skin_can <- aa_mort_skin_can %>% layout(title = 'Age-Adjusted Skin Cancer Mortality Rates by County',
                                                xaxis = list(title = 'Year'),
                                                yaxis = list(title = 'Mortality Rate (per 100,000)'))
#Poverty
ins_pov <- ggplot(outcomes_pov, aes(Poverty_Status,hlcov_cr, fill = Poverty_Status)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = hlcov_cr_lci, ymax = hlcov_cr_uci), width = 0.2) +
  labs(title = "Percent of Adults with Health Insurance by Poverty", x="Poverty Status", y="Mortality Rate (per 100,000)") +
  theme_classic() +
  scale_fill_manual(values = c(BRHD_cols[[1]], BRHD_cols[[3]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))
#Race
ins_race <- ggplot(outcomes_race, aes(Race_Ethnicity,hlcov_cr, fill= Race_Ethnicity)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = hlcov_cr_lci, ymax = hlcov_cr_uci), width=0.3, colour=BRHD_cols[[9]], alpha=0.9, size=1.3) +
  labs(title = "Percent of Adults with Health Insurance by Race/Ethinicity", x="Race/Ethnicity", y="Mortality Rate (per 100,000)") +
  theme_classic() +
  scale_fill_manual(labels = c("Black or African American only, non-Hispanic","American Indian or Alaska Native only, non-Hispanic","Asian only, non-Hispanic","Hispanic/Latino","Native Hawaiian or Other Pacific Islander only, non-Hispanic","Two or More Races only, non-Hispanic","Unknown","White only, non-Hispanic"), values = c(BRHD_cols[[5]],BRHD_cols[[6]],BRHD_cols[[1]],BRHD_cols[[7]],BRHD_cols[[3]],BRHD_cols[[2]],BRHD_cols[[4]],BRHD_cols[[8]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))

#Low Birth Weight
#Cache
lw_brth_inf <- plot_ly(outcomes_wide, x=~Year, y=~ca_lw_brth_inf, type = 'scatter', mode = "lines", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
lw_brth_inf <- lw_brth_inf %>% add_trace(y = ~ca_lw_brth_inf_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
lw_brth_inf <- lw_brth_inf %>% add_trace(y = ~ca_lw_brth_inf_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
lw_brth_inf <- lw_brth_inf %>% add_trace(y = ~be_lw_brth_inf, type = 'scatter', mode = "lines", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
lw_brth_inf <- lw_brth_inf %>% add_trace(y = ~be_lw_brth_inf_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
lw_brth_inf <- lw_brth_inf %>% add_trace(y = ~be_lw_brth_inf_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
lw_brth_inf <- lw_brth_inf %>% add_trace(y = ~br_lw_brth_inf, type = 'scatter', mode = "lines", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
lw_brth_inf <- lw_brth_inf %>% add_trace(y = ~br_lw_brth_inf_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
lw_brth_inf <- lw_brth_inf %>% add_trace(y = ~br_lw_brth_inf_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout: Adds titles and labels
lw_brth_inf <- lw_brth_inf %>% layout(title = 'Percent of Infants with Low Birth Weight',
                                      xaxis = list(title = 'Year'),
                                      yaxis = list(title = 'Percent'))
#Poverty
lw_brth_inf_pov <- ggplot(outcomes_pov, aes(Poverty_Status,lw_brth_inf, fill = Poverty_Status)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = lw_brth_inf_lci, ymax = lw_brth_inf_uci), width = 0.2) +
  labs(title = "Percent of Adults with Health Insurance by Poverty", x="Poverty Status", y="Percent") +
  theme_classic() +
  scale_fill_manual(values = c(BRHD_cols[[1]], BRHD_cols[[3]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))
#Race
lw_brth_inf_race <- ggplot(outcomes_race, aes(Race_Ethnicity,lw_brth_inf, fill= Race_Ethnicity)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = lw_brth_inf_lci, ymax = lw_brth_inf_uci), width=0.3, colour=BRHD_cols[[9]], alpha=0.9, size=1.3) +
  labs(title = "Percent of Adults with Health Insurance by Race/Ethinicity", x="Race/Ethnicity", y="Percent") +
  theme_classic() +
  scale_fill_manual(labels = c("Black or African American only, non-Hispanic","American Indian or Alaska Native only, non-Hispanic","Asian only, non-Hispanic","Hispanic/Latino","Native Hawaiian or Other Pacific Islander only, non-Hispanic","Two or More Races only, non-Hispanic","Unknown","White only, non-Hispanic"), values = c(BRHD_cols[[5]],BRHD_cols[[6]],BRHD_cols[[1]],BRHD_cols[[7]],BRHD_cols[[3]],BRHD_cols[[2]],BRHD_cols[[4]],BRHD_cols[[8]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))

#Preterm Births
#Cache
preterm_births <- plot_ly(outcomes_wide, x=~Year, y=~ca_preterm_births, type = 'scatter', mode = "lines", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
preterm_births <- preterm_births %>% add_trace(y = ~ca_preterm_births_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
preterm_births <- preterm_births %>% add_trace(y = ~ca_preterm_births_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
preterm_births <- preterm_births %>% add_trace(y = ~be_preterm_births, type = 'scatter', mode = "lines", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
preterm_births <- preterm_births %>% add_trace(y = ~be_preterm_births_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
preterm_births <- preterm_births %>% add_trace(y = ~be_preterm_births_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
preterm_births <- preterm_births %>% add_trace(y = ~br_preterm_births, type = 'scatter', mode = "lines", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
preterm_births <- preterm_births %>% add_trace(y = ~br_preterm_births_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
preterm_births <- preterm_births %>% add_trace(y = ~br_preterm_births_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout: Adds titles and labels
preterm_births <- preterm_births %>% layout(title = 'Percent of Infants Born Preterm',
                                            xaxis = list(title = 'Year'),
                                            yaxis = list(title = 'Percent'))
#Poverty
preterm_births_pov <- ggplot(outcomes_pov, aes(Poverty_Status,preterm_births, fill = Poverty_Status)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = preterm_births_lci, ymax = preterm_births_uci), width = 0.2) +
  labs(title = "Percent of Adults with Health Insurance by Poverty", x="Poverty Status", y="Percent") +
  theme_classic() +
  scale_fill_manual(values = c(BRHD_cols[[1]], BRHD_cols[[3]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))
#Race
preterm_births_race <- ggplot(outcomes_race, aes(Race_Ethnicity,preterm_births, fill= Race_Ethnicity)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = preterm_births_lci, ymax = preterm_births_uci), width=0.3, colour=BRHD_cols[[9]], alpha=0.9, size=1.3) +
  labs(title = "Percent of Adults with Health Insurance by Race/Ethinicity", x="Race/Ethnicity", y="Percent") +
  theme_classic() +
  scale_fill_manual(labels = c("Black or African American only, non-Hispanic","American Indian or Alaska Native only, non-Hispanic","Asian only, non-Hispanic","Hispanic/Latino","Native Hawaiian or Other Pacific Islander only, non-Hispanic","Two or More Races only, non-Hispanic","Unknown","White only, non-Hispanic"), values = c(BRHD_cols[[5]],BRHD_cols[[6]],BRHD_cols[[1]],BRHD_cols[[7]],BRHD_cols[[3]],BRHD_cols[[2]],BRHD_cols[[4]],BRHD_cols[[8]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))

#Rape
#Cache
rape_cr <- plot_ly(outcomes_wide, x=~Year, y=~ca_rape_cr, type = 'scatter', mode = "marker", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
rape_cr <- rape_cr %>% add_trace(y = ~ca_rape_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
rape_cr <- rape_cr %>% add_trace(y = ~ca_rape_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
rape_cr <- rape_cr %>% add_trace(y = ~be_rape_cr, type = 'scatter', mode = "marker", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
rape_cr <- rape_cr %>% add_trace(y = ~be_rape_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
rape_cr <- rape_cr %>% add_trace(y = ~be_rape_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
rape_cr <- rape_cr %>% add_trace(y = ~br_rape_cr, type = 'scatter', mode = "marker", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
rape_cr <- rape_cr %>% add_trace(y = ~br_rape_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
rape_cr <- rape_cr %>% add_trace(y = ~br_rape_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
rape_cr <- rape_cr %>% layout(title = 'Percent of Adults Who have Experienced Rape/Attempted Rape',
                              xaxis = list(title = 'Year'),
                              yaxis = list(title = 'Percent'))


aa_rape <- plot_ly(outcomes_wide, x=~Year, y=~ca_rape_aar, type = 'scatter', mode = "marker", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
aa_rape <- aa_rape %>% add_trace(y = ~ca_rape_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
aa_rape <- aa_rape %>% add_trace(y = ~ca_rape_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
aa_rape <- aa_rape %>% add_trace(y = ~be_rape_aar, type = 'scatter', mode = "marker", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
aa_rape <- aa_rape %>% add_trace(y = ~be_rape_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
aa_rape <- aa_rape %>% add_trace(y = ~be_rape_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
aa_rape <- aa_rape %>% add_trace(y = ~br_rape_aar, type = 'scatter', mode = "marker", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
aa_rape <- aa_rape %>% add_trace(y = ~br_rape_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
aa_rape <- aa_rape %>% add_trace(y = ~br_rape_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
aa_rape <- aa_rape %>% layout(title = 'Age-Adjusted Percent of Adults Who have Experienced Rape or Attempted Rape',
                              xaxis = list(title = 'Year'),
                              yaxis = list(title = 'Percent'))
#Poverty
ins_pov <- ggplot(outcomes_pov, aes(Poverty_Status,hlcov_cr, fill = Poverty_Status)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = hlcov_cr_lci, ymax = hlcov_cr_uci), width = 0.2) +
  labs(title = "Percent of Adults with Health Insurance by Poverty", x="Poverty Status", y="Percent") +
  theme_classic() +
  scale_fill_manual(values = c(BRHD_cols[[1]], BRHD_cols[[3]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))
#Race
ins_race <- ggplot(outcomes_race, aes(Race_Ethnicity,hlcov_cr, fill= Race_Ethnicity)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = hlcov_cr_lci, ymax = hlcov_cr_uci), width=0.3, colour=BRHD_cols[[9]], alpha=0.9, size=1.3) +
  labs(title = "Percent of Adults with Health Insurance by Race/Ethinicity", x="Race/Ethnicity", y="Percent") +
  theme_classic() +
  scale_fill_manual(labels = c("Black or African American only, non-Hispanic","American Indian or Alaska Native only, non-Hispanic","Asian only, non-Hispanic","Hispanic/Latino","Native Hawaiian or Other Pacific Islander only, non-Hispanic","Two or More Races only, non-Hispanic","Unknown","White only, non-Hispanic"), values = c(BRHD_cols[[5]],BRHD_cols[[6]],BRHD_cols[[1]],BRHD_cols[[7]],BRHD_cols[[3]],BRHD_cols[[2]],BRHD_cols[[4]],BRHD_cols[[8]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))

#Physical Health
#Cache
phy_hlth_cr <- plot_ly(outcomes_wide, x=~Year, y=~ca_phy_hlth_cr, type = 'scatter', mode = "lines", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
phy_hlth_cr <- phy_hlth_cr %>% add_trace(y = ~ca_phy_hlth_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
phy_hlth_cr <- phy_hlth_cr %>% add_trace(y = ~ca_phy_hlth_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
phy_hlth_cr <- phy_hlth_cr %>% add_trace(y = ~be_phy_hlth_cr, type = 'scatter', mode = "lines", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
phy_hlth_cr <- phy_hlth_cr %>% add_trace(y = ~be_phy_hlth_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
phy_hlth_cr <- phy_hlth_cr %>% add_trace(y = ~be_phy_hlth_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
phy_hlth_cr <- phy_hlth_cr %>% add_trace(y = ~br_phy_hlth_cr, type = 'scatter', mode = "lines", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
phy_hlth_cr <- phy_hlth_cr %>% add_trace(y = ~br_phy_hlth_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
phy_hlth_cr <- phy_hlth_cr %>% add_trace(y = ~br_phy_hlth_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
phy_hlth_cr <- phy_hlth_cr %>% layout(title = 'Percent of Adults Who had <7 days of Poor Physical Health in the Last 30 Days',
                                      xaxis = list(title = 'Year'),
                                      yaxis = list(title = 'Percent'))


aa_phy_hlth <- plot_ly(outcomes_wide, x=~Year, y=~ca_phy_hlth_aar, type = 'scatter', mode = "lines", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
aa_phy_hlth <- aa_phy_hlth %>% add_trace(y = ~ca_phy_hlth_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
aa_phy_hlth <- aa_phy_hlth %>% add_trace(y = ~ca_phy_hlth_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
aa_phy_hlth <- aa_phy_hlth %>% add_trace(y = ~be_phy_hlth_aar, type = 'scatter', mode = "lines", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
aa_phy_hlth <- aa_phy_hlth %>% add_trace(y = ~be_phy_hlth_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
aa_phy_hlth <- aa_phy_hlth %>% add_trace(y = ~be_phy_hlth_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
aa_phy_hlth <- aa_phy_hlth %>% add_trace(y = ~br_phy_hlth_aar, type = 'scatter', mode = "lines", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
aa_phy_hlth <- aa_phy_hlth %>% add_trace(y = ~br_phy_hlth_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
aa_phy_hlth <- aa_phy_hlth %>% add_trace(y = ~br_phy_hlth_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
aa_phy_hlth <- aa_phy_hlth %>% layout(title = 'Age-Adjusted Percent of Adults Who had <7 days of Poor Physical Health in the Last 30 Days',
                                      xaxis = list(title = 'Year'),
                                      yaxis = list(title = 'Percent'))
#Poverty
ins_pov <- ggplot(outcomes_pov, aes(Poverty_Status,hlcov_cr, fill = Poverty_Status)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = hlcov_cr_lci, ymax = hlcov_cr_uci), width = 0.2) +
  labs(title = "Percent of Adults with Health Insurance by Poverty", x="Poverty Status", y="Percent") +
  theme_classic() +
  scale_fill_manual(values = c(BRHD_cols[[1]], BRHD_cols[[3]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))
#Race
ins_race <- ggplot(outcomes_race, aes(Race_Ethnicity,hlcov_cr, fill= Race_Ethnicity)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = hlcov_cr_lci, ymax = hlcov_cr_uci), width=0.3, colour=BRHD_cols[[9]], alpha=0.9, size=1.3) +
  labs(title = "Percent of Adults with Health Insurance by Race/Ethinicity", x="Race/Ethnicity", y="Percent") +
  theme_classic() +
  scale_fill_manual(labels = c("Black or African American only, non-Hispanic","American Indian or Alaska Native only, non-Hispanic","Asian only, non-Hispanic","Hispanic/Latino","Native Hawaiian or Other Pacific Islander only, non-Hispanic","Two or More Races only, non-Hispanic","Unknown","White only, non-Hispanic"), values = c(BRHD_cols[[5]],BRHD_cols[[6]],BRHD_cols[[1]],BRHD_cols[[7]],BRHD_cols[[3]],BRHD_cols[[2]],BRHD_cols[[4]],BRHD_cols[[8]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))

#Mental Health
#Cache
ment_hlth_cr <- plot_ly(outcomes_wide, x=~Year, y=~ca_ment_hlth_cr, type = 'scatter', mode = "lines", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
ment_hlth_cr <- ment_hlth_cr %>% add_trace(y = ~ca_ment_hlth_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
ment_hlth_cr <- ment_hlth_cr %>% add_trace(y = ~ca_ment_hlth_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
ment_hlth_cr <- ment_hlth_cr %>% add_trace(y = ~be_ment_hlth_cr, type = 'scatter', mode = "lines", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
ment_hlth_cr <- ment_hlth_cr %>% add_trace(y = ~be_ment_hlth_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
ment_hlth_cr <- ment_hlth_cr %>% add_trace(y = ~be_ment_hlth_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
ment_hlth_cr <- ment_hlth_cr %>% add_trace(y = ~br_ment_hlth_cr, type = 'scatter', mode = "lines", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
ment_hlth_cr <- ment_hlth_cr %>% add_trace(y = ~br_ment_hlth_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
ment_hlth_cr <- ment_hlth_cr %>% add_trace(y = ~br_ment_hlth_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
ment_hlth_cr <- ment_hlth_cr %>% layout(title = 'Percent of Adults Who had <7 days of Poor Mental Health in the Last 30 Days',
                                        xaxis = list(title = 'Year'),
                                        yaxis = list(title = 'Percent'))


aa_ment_hlth <- plot_ly(outcomes_wide, x=~Year, y=~ca_ment_hlth_aar, type = 'scatter', mode = "lines", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
aa_ment_hlth <- aa_ment_hlth %>% add_trace(y = ~ca_ment_hlth_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
aa_ment_hlth <- aa_ment_hlth %>% add_trace(y = ~ca_ment_hlth_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
aa_ment_hlth <- aa_ment_hlth %>% add_trace(y = ~be_ment_hlth_aar, type = 'scatter', mode = "lines", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
aa_ment_hlth <- aa_ment_hlth %>% add_trace(y = ~be_ment_hlth_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
aa_ment_hlth <- aa_ment_hlth %>% add_trace(y = ~be_ment_hlth_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
aa_ment_hlth <- aa_ment_hlth %>% add_trace(y = ~br_ment_hlth_aar, type = 'scatter', mode = "lines", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
aa_ment_hlth <- aa_ment_hlth %>% add_trace(y = ~br_ment_hlth_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
aa_ment_hlth <- aa_ment_hlth %>% add_trace(y = ~br_ment_hlth_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
aa_ment_hlth <- aa_ment_hlth %>% layout(title = 'Age-Adjusted Percent of Adults Who had <7 days of Poor Mental Health in the Last 30 Days',
                                        xaxis = list(title = 'Year'),
                                        yaxis = list(title = 'Percent'))
#Poverty
ins_pov <- ggplot(outcomes_pov, aes(Poverty_Status,hlcov_cr, fill = Poverty_Status)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = hlcov_cr_lci, ymax = hlcov_cr_uci), width = 0.2) +
  labs(title = "Percent of Adults with Health Insurance by Poverty", x="Poverty Status", y="Percent") +
  theme_classic() +
  scale_fill_manual(values = c(BRHD_cols[[1]], BRHD_cols[[3]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))
#Race
ins_race <- ggplot(outcomes_race, aes(Race_Ethnicity,hlcov_cr, fill= Race_Ethnicity)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = hlcov_cr_lci, ymax = hlcov_cr_uci), width=0.3, colour=BRHD_cols[[9]], alpha=0.9, size=1.3) +
  labs(title = "Percent of Adults with Health Insurance by Race/Ethinicity", x="Race/Ethnicity", y="Percent") +
  theme_classic() +
  scale_fill_manual(labels = c("Black or African American only, non-Hispanic","American Indian or Alaska Native only, non-Hispanic","Asian only, non-Hispanic","Hispanic/Latino","Native Hawaiian or Other Pacific Islander only, non-Hispanic","Two or More Races only, non-Hispanic","Unknown","White only, non-Hispanic"), values = c(BRHD_cols[[5]],BRHD_cols[[6]],BRHD_cols[[1]],BRHD_cols[[7]],BRHD_cols[[3]],BRHD_cols[[2]],BRHD_cols[[4]],BRHD_cols[[8]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))

#Arthritis
#Cache
arthrits_cr <- plot_ly(outcomes_wide, x=~Year, y=~ca_arthrits_cr, type = 'scatter', mode = "marker", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
arthrits_cr <- arthrits_cr %>% add_trace(y = ~ca_arthrits_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
arthrits_cr <- arthrits_cr %>% add_trace(y = ~ca_arthrits_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
arthrits_cr <- arthrits_cr %>% add_trace(y = ~be_arthrits_cr, type = 'scatter', mode = "marker", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
arthrits_cr <- arthrits_cr %>% add_trace(y = ~be_arthrits_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
arthrits_cr <- arthrits_cr %>% add_trace(y = ~be_arthrits_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
arthrits_cr <- arthrits_cr %>% add_trace(y = ~br_arthrits_cr, type = 'scatter', mode = "marker", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
arthrits_cr <- arthrits_cr %>% add_trace(y = ~br_arthrits_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
arthrits_cr <- arthrits_cr %>% add_trace(y = ~br_arthrits_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
arthrits_cr <- arthrits_cr %>% layout(title = 'Percent of Adults Who have Diagnosed Arthritis',
                                      xaxis = list(title = 'Year'),
                                      yaxis = list(title = 'Percent'))


aa_arthrits <- plot_ly(outcomes_wide, x=~Year, y=~ca_arthrits_aar, type = 'scatter', mode = "marker", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
aa_arthrits <- aa_arthrits %>% add_trace(y = ~ca_arthrits_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
aa_arthrits <- aa_arthrits %>% add_trace(y = ~ca_arthrits_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
aa_arthrits <- aa_arthrits %>% add_trace(y = ~be_arthrits_aar, type = 'scatter', mode = "marker", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
aa_arthrits <- aa_arthrits %>% add_trace(y = ~be_arthrits_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
aa_arthrits <- aa_arthrits %>% add_trace(y = ~be_arthrits_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
aa_arthrits <- aa_arthrits %>% add_trace(y = ~br_arthrits_aar, type = 'scatter', mode = "marker", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
aa_arthrits <- aa_arthrits %>% add_trace(y = ~br_arthrits_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
aa_arthrits <- aa_arthrits %>% add_trace(y = ~br_arthrits_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
aa_arthrits <- aa_arthrits %>% layout(title = 'Age-Adjusted Percent of Adults Who have Diagnosed Arthritis',
                                      xaxis = list(title = 'Year'),
                                      yaxis = list(title = 'Percent'))
#Poverty
ins_pov <- ggplot(outcomes_pov, aes(Poverty_Status,hlcov_cr, fill = Poverty_Status)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = hlcov_cr_lci, ymax = hlcov_cr_uci), width = 0.2) +
  labs(title = "Percent of Adults with Health Insurance by Poverty", x="Poverty Status", y="Percent") +
  theme_classic() +
  scale_fill_manual(values = c(BRHD_cols[[1]], BRHD_cols[[3]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))
#Race
ins_race <- ggplot(outcomes_race, aes(Race_Ethnicity,hlcov_cr, fill= Race_Ethnicity)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = hlcov_cr_lci, ymax = hlcov_cr_uci), width=0.3, colour=BRHD_cols[[9]], alpha=0.9, size=1.3) +
  labs(title = "Percent of Adults with Health Insurance by Race/Ethinicity", x="Race/Ethnicity", y="Percent") +
  theme_classic() +
  scale_fill_manual(labels = c("Black or African American only, non-Hispanic","American Indian or Alaska Native only, non-Hispanic","Asian only, non-Hispanic","Hispanic/Latino","Native Hawaiian or Other Pacific Islander only, non-Hispanic","Two or More Races only, non-Hispanic","Unknown","White only, non-Hispanic"), values = c(BRHD_cols[[5]],BRHD_cols[[6]],BRHD_cols[[1]],BRHD_cols[[7]],BRHD_cols[[3]],BRHD_cols[[2]],BRHD_cols[[4]],BRHD_cols[[8]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))

#Asthma
#Cache
astma_cr <- plot_ly(outcomes_wide, x=~Year, y=~ca_astma_cr, type = 'scatter', mode = "lines", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
astma_cr <- astma_cr %>% add_trace(y = ~ca_astma_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
astma_cr <- astma_cr %>% add_trace(y = ~ca_astma_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
astma_cr <- astma_cr %>% add_trace(y = ~be_astma_cr, type = 'scatter', mode = "lines", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
astma_cr <- astma_cr %>% add_trace(y = ~be_astma_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
astma_cr <- astma_cr %>% add_trace(y = ~be_astma_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
astma_cr <- astma_cr %>% add_trace(y = ~br_astma_cr, type = 'scatter', mode = "lines", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
astma_cr <- astma_cr %>% add_trace(y = ~br_astma_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
astma_cr <- astma_cr %>% add_trace(y = ~br_astma_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
astma_cr <- astma_cr %>% layout(title = 'Percent of Adults Who Currently have Diagnosed Asthma',
                                xaxis = list(title = 'Year'),
                                yaxis = list(title = 'Percent'))

aa_astma <- plot_ly(outcomes_wide, x=~Year, y=~ca_astma_aar, type = 'scatter', mode = "lines", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
aa_astma <- aa_astma %>% add_trace(y = ~ca_astma_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
aa_astma <- aa_astma %>% add_trace(y = ~ca_astma_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
aa_astma <- aa_astma %>% add_trace(y = ~be_astma_aar, type = 'scatter', mode = "lines", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
aa_astma <- aa_astma %>% add_trace(y = ~be_astma_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
aa_astma <- aa_astma %>% add_trace(y = ~be_astma_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
aa_astma <- aa_astma %>% add_trace(y = ~br_astma_aar, type = 'scatter', mode = "lines", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
aa_astma <- aa_astma %>% add_trace(y = ~br_astma_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
aa_astma <- aa_astma %>% add_trace(y = ~br_astma_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
aa_astma <- aa_astma %>% layout(title = 'Age-Adjusted Percent of Adults Who Currently have Diagnosed Asthma',
                                xaxis = list(title = 'Year'),
                                yaxis = list(title = 'Percent'))
#Poverty
ins_pov <- ggplot(outcomes_pov, aes(Poverty_Status,hlcov_cr, fill = Poverty_Status)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = hlcov_cr_lci, ymax = hlcov_cr_uci), width = 0.2) +
  labs(title = "Percent of Adults with Health Insurance by Poverty", x="Poverty Status", y="Percent") +
  theme_classic() +
  scale_fill_manual(values = c(BRHD_cols[[1]], BRHD_cols[[3]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))
#Race
ins_race <- ggplot(outcomes_race, aes(Race_Ethnicity,hlcov_cr, fill= Race_Ethnicity)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = hlcov_cr_lci, ymax = hlcov_cr_uci), width=0.3, colour=BRHD_cols[[9]], alpha=0.9, size=1.3) +
  labs(title = "Percent of Adults with Health Insurance by Race/Ethinicity", x="Race/Ethnicity", y="Percent") +
  theme_classic() +
  scale_fill_manual(labels = c("Black or African American only, non-Hispanic","American Indian or Alaska Native only, non-Hispanic","Asian only, non-Hispanic","Hispanic/Latino","Native Hawaiian or Other Pacific Islander only, non-Hispanic","Two or More Races only, non-Hispanic","Unknown","White only, non-Hispanic"), values = c(BRHD_cols[[5]],BRHD_cols[[6]],BRHD_cols[[1]],BRHD_cols[[7]],BRHD_cols[[3]],BRHD_cols[[2]],BRHD_cols[[4]],BRHD_cols[[8]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))

#Diabetes
#Cache
diab_cr <- plot_ly(outcomes_wide, x=~Year, y=~ca_diab_cr, type = 'scatter', mode = "lines", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
diab_cr <- diab_cr %>% add_trace(y = ~ca_diab_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
diab_cr <- diab_cr %>% add_trace(y = ~ca_diab_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
diab_cr <- diab_cr %>% add_trace(y = ~be_diab_cr, type = 'scatter', mode = "lines", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
diab_cr <- diab_cr %>% add_trace(y = ~be_diab_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
diab_cr <- diab_cr %>% add_trace(y = ~be_diab_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
diab_cr <- diab_cr %>% add_trace(y = ~br_diab_cr, type = 'scatter', mode = "lines", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
diab_cr <- diab_cr %>% add_trace(y = ~br_diab_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
diab_cr <- diab_cr %>% add_trace(y = ~br_diab_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
diab_cr <- diab_cr %>% layout(title = 'Percent of Adults Who have been Told they have Diabetes',
                              xaxis = list(title = 'Year'),
                              yaxis = list(title = 'Percent'))


aa_diab <- plot_ly(outcomes_wide, x=~Year, y=~ca_diab_aar, type = 'scatter', mode = "lines", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
aa_diab <- aa_diab %>% add_trace(y = ~ca_diab_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
aa_diab <- aa_diab %>% add_trace(y = ~ca_diab_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
aa_diab <- aa_diab %>% add_trace(y = ~be_diab_aar, type = 'scatter', mode = "lines", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
aa_diab <- aa_diab %>% add_trace(y = ~be_diab_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
aa_diab <- aa_diab %>% add_trace(y = ~be_diab_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
aa_diab <- aa_diab %>% add_trace(y = ~br_diab_aar, type = 'scatter', mode = "lines", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
aa_diab <- aa_diab %>% add_trace(y = ~br_diab_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
aa_diab <- aa_diab %>% add_trace(y = ~br_diab_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
aa_diab <- aa_diab %>% layout(title = 'Age-Adjusted Percent of Adults Who have been Told they have Diabetes',
                              xaxis = list(title = 'Year'),
                              yaxis = list(title = 'Percent'))
#Poverty
ins_pov <- ggplot(outcomes_pov, aes(Poverty_Status,hlcov_cr, fill = Poverty_Status)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = hlcov_cr_lci, ymax = hlcov_cr_uci), width = 0.2) +
  labs(title = "Percent of Adults with Health Insurance by Poverty", x="Poverty Status", y="Percent") +
  theme_classic() +
  scale_fill_manual(values = c(BRHD_cols[[1]], BRHD_cols[[3]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))
#Race
ins_race <- ggplot(outcomes_race, aes(Race_Ethnicity,hlcov_cr, fill= Race_Ethnicity)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = hlcov_cr_lci, ymax = hlcov_cr_uci), width=0.3, colour=BRHD_cols[[9]], alpha=0.9, size=1.3) +
  labs(title = "Percent of Adults with Health Insurance by Race/Ethinicity", x="Race/Ethnicity", y="Percent") +
  theme_classic() +
  scale_fill_manual(labels = c("Black or African American only, non-Hispanic","American Indian or Alaska Native only, non-Hispanic","Asian only, non-Hispanic","Hispanic/Latino","Native Hawaiian or Other Pacific Islander only, non-Hispanic","Two or More Races only, non-Hispanic","Unknown","White only, non-Hispanic"), values = c(BRHD_cols[[5]],BRHD_cols[[6]],BRHD_cols[[1]],BRHD_cols[[7]],BRHD_cols[[3]],BRHD_cols[[2]],BRHD_cols[[4]],BRHD_cols[[8]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))

#Prediabetes
#Cache
prediabets_cr <- plot_ly(outcomes_wide, x=~Year, y=~ca_prediabets_cr, type = 'scatter', mode = "marker", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
prediabets_cr <- prediabets_cr %>% add_trace(y = ~ca_prediabets_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
prediabets_cr <- prediabets_cr %>% add_trace(y = ~ca_prediabets_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
prediabets_cr <- prediabets_cr %>% add_trace(y = ~be_prediabets_cr, type = 'scatter', mode = "marker", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
prediabets_cr <- prediabets_cr %>% add_trace(y = ~be_prediabets_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
prediabets_cr <- prediabets_cr %>% add_trace(y = ~be_prediabets_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
prediabets_cr <- prediabets_cr %>% add_trace(y = ~br_prediabets_cr, type = 'scatter', mode = "marker", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
prediabets_cr <- prediabets_cr %>% add_trace(y = ~br_prediabets_cr_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
prediabets_cr <- prediabets_cr %>% add_trace(y = ~br_prediabets_cr_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
prediabets_cr <- prediabets_cr %>% layout(title = 'Percent of Adults Who have been Told they have Pre-diabetes',
                                          xaxis = list(title = 'Year'),
                                          yaxis = list(title = 'Percent'))


aa_prediabets <- plot_ly(outcomes_wide, x=~Year, y=~ca_prediabets_aar, type = 'scatter', mode = "marker", name = "Cache", line = list(color = BRHD_cols[[1]]), legendgroup = 'group1')
aa_prediabets <- aa_prediabets %>% add_trace(y = ~ca_prediabets_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
aa_prediabets <- aa_prediabets %>% add_trace(y = ~ca_prediabets_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[1]], 0.2), line = list(color = 'transparent'), legendgroup = 'group1', showlegend = FALSE)
#Box Elder
aa_prediabets <- aa_prediabets %>% add_trace(y = ~be_prediabets_aar, type = 'scatter', mode = "marker", name = "Box Elder", line = list(color = BRHD_cols[[3]]), legendgroup = 'group2')
aa_prediabets <- aa_prediabets %>% add_trace(y = ~be_prediabets_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
aa_prediabets <- aa_prediabets %>% add_trace(y = ~be_prediabets_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[3]], 0.2), line = list(color = 'transparent'), legendgroup = 'group2', showlegend = FALSE)
#BRHD
aa_prediabets <- aa_prediabets %>% add_trace(y = ~br_prediabets_aar, type = 'scatter', mode = "marker", name = "BRHD", line = list(color = BRHD_cols[[4]]), legendgroup = 'group3')
aa_prediabets <- aa_prediabets %>% add_trace(y = ~br_prediabets_aar_uci, name = "Upper CI", line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
aa_prediabets <- aa_prediabets %>% add_trace(y = ~br_prediabets_aar_lci, name = "Lower CI", fill = 'tonextx', fillcolor = alpha(BRHD_cols[[4]], 0.2), line = list(color = 'transparent'), legendgroup = 'group3', showlegend = FALSE)
#Layout
aa_prediabets <- aa_prediabets %>% layout(title = 'Age-Adjusted Percent of Adults Who have been Told they have Pre-diabetes',
                                          xaxis = list(title = 'Year'),
                                          yaxis = list(title = 'Percent'))
#Poverty
ins_pov <- ggplot(outcomes_pov, aes(Poverty_Status,hlcov_cr, fill = Poverty_Status)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = hlcov_cr_lci, ymax = hlcov_cr_uci), width = 0.2) +
  labs(title = "Percent of Adults with Health Insurance by Poverty", x="Poverty Status", y="Percent") +
  theme_classic() +
  scale_fill_manual(values = c(BRHD_cols[[1]], BRHD_cols[[3]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))
#Race
ins_race <- ggplot(outcomes_race, aes(Race_Ethnicity,hlcov_cr, fill= Race_Ethnicity)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = hlcov_cr_lci, ymax = hlcov_cr_uci), width=0.3, colour=BRHD_cols[[9]], alpha=0.9, size=1.3) +
  labs(title = "Percent of Adults with Health Insurance by Race/Ethinicity", x="Race/Ethnicity", y="Percent") +
  theme_classic() +
  scale_fill_manual(labels = c("Black or African American only, non-Hispanic","American Indian or Alaska Native only, non-Hispanic","Asian only, non-Hispanic","Hispanic/Latino","Native Hawaiian or Other Pacific Islander only, non-Hispanic","Two or More Races only, non-Hispanic","Unknown","White only, non-Hispanic"), values = c(BRHD_cols[[5]],BRHD_cols[[6]],BRHD_cols[[1]],BRHD_cols[[7]],BRHD_cols[[3]],BRHD_cols[[2]],BRHD_cols[[4]],BRHD_cols[[8]])) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12)) +
  theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))


##############
# Reactables #
##############


#Dem Reactable
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

dem_react <- reactable(dem_table, resizable = TRUE, showPageSizeOptions = TRUE,
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

#Hlth Reactable-Creates a table
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
                      
#Creates the drop down menu that shows the graph
hlth_react <- reactable(hlth_table, resizable = TRUE, showPageSizeOptions = TRUE,
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
                                                             'Percent of Births Where the Mother Received First Trimester Prenatal Care by Poverty',
                                                             'Percent of Adults Receiving Flu Vaccination in Last 12 Months by Poverty'),
                        `Below` = c(paste(first(hlth_pov$hlcov_cr),'%', sep = ''),
                                         paste(first(hlth_pov$no_care_cr),'%', sep = ''),
                                         paste(first(hlth_pov$hlth_pro_cr),'%', sep = ''),
                                         paste(first(hlth_pov$med_check_cr),'%', sep = ''),
                                         paste(first(hlth_pov$dent_check_cr),'%', sep = ''),
                                         paste(first(hlth_pov$mammo_cr),'%', sep = ''),
                                         paste(first(hlth_pov$col_scree_cr),'%', sep = ''),
                                         paste(first(hlth_pov$pn_perc),'%', sep = ''),
                                         paste(first(hlth_pov$fluvac_cr),'%', sep = '')),
                        `Above` = c(paste(last(hlth_pov$hlcov_cr),'%', sep = ''),
                                        paste(last(hlth_pov$no_care_cr),'%', sep = ''),
                                        paste(last(hlth_pov$hlth_pro_cr),'%', sep = ''),
                                        paste(last(hlth_pov$med_check_cr),'%', sep = ''),
                                        paste(last(hlth_pov$dent_check_cr),'%', sep = ''),
                                        paste(last(hlth_pov$mammo_cr),'%', sep = ''),
                                        paste(last(hlth_pov$col_scree_cr),'%', sep = ''),
                                        paste(last(hlth_pov$pn_perc),'%', sep = ''),
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
                            htmltools::div(style = "padding: 25px", ggplotly(pn_perc_pov))
                          } else if (index == 9) {
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
                                                             'Percent of Births Where the Mother Received First Trimester Prenatal Care by Race/Ethnicity',
                                                             'Percent of Adults Receiving Flu Vaccination in Last 12 Months by Race/Ethnicity'),
                             `American Indian` = c(paste(hlth_race[1, 6],'%', sep = ''),
                                                   paste(hlth_race[1, 12],'%', sep = ''),
                                                   paste(hlth_race[1, 18],'%', sep = ''),
                                                   paste(hlth_race[1, 24],'%', sep = ''),
                                                   paste(hlth_race[1, 30],'%', sep = ''),
                                                   paste(hlth_race[1, 36],'%', sep = ''),
                                                   paste(hlth_race[1, 42],'%', sep = ''),
                                                   paste(hlth_race[1, 45],'%', sep = ''),
                                                   paste(hlth_race[1, 51],'%', sep = '')),
                             `Asian` = c(paste(hlth_race[2, 6],'%', sep = ''),
                                                   paste(hlth_race[2, 12],'%', sep = ''),
                                                   paste(hlth_race[2, 18],'%', sep = ''),
                                                   paste(hlth_race[2, 24],'%', sep = ''),
                                                   paste(hlth_race[2, 30],'%', sep = ''),
                                                   paste(hlth_race[2, 36],'%', sep = ''),
                                                   paste(hlth_race[2, 42],'%', sep = ''),
                                                   paste(hlth_race[2, 45],'%', sep = ''),
                                                   paste(hlth_race[2, 51],'%', sep = '')),
                             `Black` = c(paste(hlth_race[3, 6],'%', sep = ''),
                                                   paste(hlth_race[3, 12],'%', sep = ''),
                                                   paste(hlth_race[3, 18],'%', sep = ''),
                                                   paste(hlth_race[3, 24],'%', sep = ''),
                                                   paste(hlth_race[3, 30],'%', sep = ''),
                                                   paste(hlth_race[3, 36],'%', sep = ''),
                                                   paste(hlth_race[3, 42],'%', sep = ''),
                                                   paste(hlth_race[3, 45],'%', sep = ''),
                                                   paste(hlth_race[3, 51],'%', sep = '')),
                             `Hispanic` = c(paste(hlth_race[4, 6],'%', sep = ''),
                                                   paste(hlth_race[4, 12],'%', sep = ''),
                                                   paste(hlth_race[4, 18],'%', sep = ''),
                                                   paste(hlth_race[4, 24],'%', sep = ''),
                                                   paste(hlth_race[4, 30],'%', sep = ''),
                                                   paste(hlth_race[4, 36],'%', sep = ''),
                                                   paste(hlth_race[4, 42],'%', sep = ''),
                                                   paste(hlth_race[4, 45],'%', sep = ''),
                                                   paste(hlth_race[4, 51],'%', sep = '')),
                             `Pacific Islander` = c(paste(hlth_race[5, 6],'%', sep = ''),
                                                   paste(hlth_race[5, 12],'%', sep = ''),
                                                   paste(hlth_race[5, 18],'%', sep = ''),
                                                   paste(hlth_race[5, 24],'%', sep = ''),
                                                   paste(hlth_race[5, 30],'%', sep = ''),
                                                   paste(hlth_race[5, 36],'%', sep = ''),
                                                   paste(hlth_race[5, 42],'%', sep = ''),
                                                   paste(hlth_race[5, 45],'%', sep = ''),
                                                   paste(hlth_race[5, 51],'%', sep = '')),
                             `Two or More` = c(paste(hlth_race[6, 6],'%', sep = ''),
                                                   paste(hlth_race[6, 12],'%', sep = ''),
                                                   paste(hlth_race[6, 18],'%', sep = ''),
                                                   paste(hlth_race[6, 24],'%', sep = ''),
                                                   paste(hlth_race[6, 30],'%', sep = ''),
                                                   paste(hlth_race[6, 36],'%', sep = ''),
                                                   paste(hlth_race[6, 42],'%', sep = ''),
                                                   paste(hlth_race[6, 45],'%', sep = ''),
                                                   paste(hlth_race[6, 51],'%', sep = '')),
                             `White` = c(paste(hlth_race[7, 6],'%', sep = ''),
                                                   paste(hlth_race[7, 12],'%', sep = ''),
                                                   paste(hlth_race[7, 18],'%', sep = ''),
                                                   paste(hlth_race[7, 24],'%', sep = ''),
                                                   paste(hlth_race[7, 30],'%', sep = ''),
                                                   paste(hlth_race[7, 36],'%', sep = ''),
                                                   paste(hlth_race[7, 42],'%', sep = ''),
                                                   paste(hlth_race[7, 45],'%', sep = ''),
                                                   paste(hlth_race[7, 51],'%', sep = '')),
                             `Unknown` = c(paste(hlth_race[8, 6],'%', sep = ''),
                                                   paste(hlth_race[8, 12],'%', sep = ''),
                                                   paste(hlth_race[8, 18],'%', sep = ''),
                                                   paste(hlth_race[8, 24],'%', sep = ''),
                                                   paste(hlth_race[8, 30],'%', sep = ''),
                                                   paste(hlth_race[8, 36],'%', sep = ''),
                                                   paste(hlth_race[8, 42],'%', sep = ''),
                                                   paste(hlth_race[8, 45],'%', sep = ''),
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
                                htmltools::div(style = "padding: 25px", ggplotly(pn_perc_race))
                              } else if (index == 9) {
                                htmltools::div(style = "padding: 25px", ggplotly(fluvac_race))
                              }
                            })
                     


###Risk Reactable-Creates a table##
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


#Creates the drop down menu that shows the graph
risk_react <- reactable(risk_table, resizable = TRUE, showPageSizeOptions = TRUE,
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



risk_react_aa <- reactable(risk_table_aa, resizable = TRUE, showPageSizeOptions = TRUE,
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
risk_table_pov <- bind_cols(`Access to Care Indicators by Poverty` = c('Rate of Births among Adolescents',
                                                                       'Percent of Adults Consuming the Recommended Amount of Fruit',
                                                                       'Percent of Adults Consuming the Recommended Amount of Vegetables',
                                                                       'Percent of Adults Getting the Recommended Amount of Aerobic Physical Activity and Muscle Strengthening',
                                                                       'Percent of Adults Currently Smoking',
                                                                       'Percent of Percent of Adults Currently Using E-cigarettes',
                                                                       'Percent of Adults Currently at Risk for Binge Drinking',
                                                                       'Percent of Adults Who Always or Nearly Always Wear a Seat Belt'),
                            `Below` = c(paste(first(risk_pov$adol_brths),'%', sep = ''),
                                        paste(first(risk_pov$fruit_cr),'%', sep = ''),
                                        paste(first(risk_pov$veg_cr),'%', sep = ''),
                                        paste(first(risk_pov$phy_act_cr),'%', sep = ''),
                                        paste(first(risk_pov$cig_smkng_cr),'%', sep = ''),
                                        paste(first(risk_pov$ecig_cr),'%', sep = ''),
                                        paste(first(risk_pov$bnge_drnkng_cr),'%', sep = ''),
                                        paste(first(risk_pov$st_blt_cr),'%', sep = ''),
                                        paste(first(risk_pov$fluvac_cr),'%', sep = '')),
                            `Above` = c(paste(last(risk_pov$adol_birth_cr),'%', sep = ''),
                                        paste(last(risk_pov$fruit_cr),'%', sep = ''),
                                        paste(last(risk_pov$veg_pro_cr),'%', sep = ''),
                                        paste(last(risk_pov$phy_act_cr),'%', sep = ''),
                                        paste(last(risk_pov$cig_smkng_cr),'%', sep = ''),
                                        paste(last(risk_pov$ecig_cr),'%', sep = ''),
                                        paste(last(risk_pov$bnge_drnkng_cr),'%', sep = ''),
                                        paste(last(risk_pov$st_blt_cr),'%', sep = ''),
                                        paste(last(risk_pov$fluvac_cr),'%', sep = '')))

#Creates the drop down menu that shows the graph
risk_react_pov <- reactable(risk_table, resizable = TRUE, showPageSizeOptions = TRUE,
                        onClick = 'expand', highlight = TRUE, rowStyle = list(cursor = 'pointer'),
                        details = function(index) {
                          if (index == 1) {
                            htmltools::div(style = "padding: 25px", ggplotly(adol_brths_pov))
                          } else if (index == 2) {
                            htmltools::div(style = "padding: 25px", ggplotly(fruit_cr_pov))
                          } else if (index == 3) {
                            htmltools::div(style = "padding: 25px", ggplotly(veg_cr_pov))
                          } else if (index == 4) {
                            htmltools::div(style = "padding: 25px", ggplotly(phy_act_cr_pov))
                          } else if (index == 5) {
                            htmltools::div(style = "padding: 25px", ggplotly(cig_smkng_cr_pov))
                          } else if (index == 6) {
                            htmltools::div(style = "padding: 25px", ggplotly(ecig_cr_pov))
                          } else if (index == 7) {
                            htmltools::div(style = "padding: 25px", ggplotly(bnge_drnkng_cr_pov))
                          } else if (index == 8) {
                            htmltools::div(style = "padding: 25px", ggplotly(st_blt_cr_pov))
                          }  
                        })
#Race
risk_table_race <- bind_cols(`Access to Care Indicators by Race/Ethnicity` = c('Rate of Births among Adolescents',
                                                                               'Percent of Adults Consuming the Recommended Amount of Fruit',
                                                                               'Percent of Adults Consuming the Recommended Amount of Vegetables',
                                                                               'Percent of Adults Getting the Recommended Amount of Aerobic Physical Activity and Muscle Strengthening',
                                                                               'Percent of Adults Currently Smoking',
                                                                               'Percent of Percent of Adults Currently Using E-cigarettes',
                                                                               'Percent of Adults Currently at Risk for Binge Drinking',
                                                                               'Percent of Adults Who Always or Nearly Always Wear a Seat Belt'),
                             `American Indian` = c(paste(risk_race[1, 6],'%', sep = ''),
                                                   paste(risk_race[1, 12],'%', sep = ''),
                                                   paste(risk_race[1, 18],'%', sep = ''),
                                                   paste(risk_race[1, 24],'%', sep = ''),
                                                   paste(risk_race[1, 30],'%', sep = ''),
                                                   paste(risk_race[1, 36],'%', sep = ''),
                                                   paste(risk_race[1, 42],'%', sep = ''),
                                                   paste(risk_race[1, 45],'%', sep = ''),
                                                   paste(risk_race[1, 51],'%', sep = '')),
                             `Asian` = c(paste(risk_race[2, 6],'%', sep = ''),
                                         paste(risk_race[2, 12],'%', sep = ''),
                                         paste(risk_race[2, 18],'%', sep = ''),
                                         paste(risk_race[2, 24],'%', sep = ''),
                                         paste(risk_race[2, 30],'%', sep = ''),
                                         paste(risk_race[2, 36],'%', sep = ''),
                                         paste(risk_race[2, 42],'%', sep = ''),
                                         paste(risk_race[2, 45],'%', sep = ''),
                                         paste(risk_race[2, 51],'%', sep = '')),
                             `Black` = c(paste(risk_race[3, 6],'%', sep = ''),
                                         paste(risk_race[3, 12],'%', sep = ''),
                                         paste(risk_race[3, 18],'%', sep = ''),
                                         paste(risk_race[3, 24],'%', sep = ''),
                                         paste(risk_race[3, 30],'%', sep = ''),
                                         paste(risk_race[3, 36],'%', sep = ''),
                                         paste(risk_race[3, 42],'%', sep = ''),
                                         paste(risk_race[3, 45],'%', sep = ''),
                                         paste(risk_race[3, 51],'%', sep = '')),
                             `Hispanic` = c(paste(risk_race[4, 6],'%', sep = ''),
                                            paste(risk_race[4, 12],'%', sep = ''),
                                            paste(risk_race[4, 18],'%', sep = ''),
                                            paste(risk_race[4, 24],'%', sep = ''),
                                            paste(risk_race[4, 30],'%', sep = ''),
                                            paste(risk_race[4, 36],'%', sep = ''),
                                            paste(risk_race[4, 42],'%', sep = ''),
                                            paste(risk_race[4, 45],'%', sep = ''),
                                            paste(risk_race[4, 51],'%', sep = '')),
                             `Pacific Islander` = c(paste(risk_race[5, 6],'%', sep = ''),
                                                    paste(risk_race[5, 12],'%', sep = ''),
                                                    paste(risk_race[5, 18],'%', sep = ''),
                                                    paste(risk_race[5, 24],'%', sep = ''),
                                                    paste(risk_race[5, 30],'%', sep = ''),
                                                    paste(risk_race[5, 36],'%', sep = ''),
                                                    paste(risk_race[5, 42],'%', sep = ''),
                                                    paste(risk_race[5, 45],'%', sep = ''),
                                                    paste(risk_race[5, 51],'%', sep = '')),
                             `Two or More` = c(paste(risk_race[6, 6],'%', sep = ''),
                                               paste(risk_race[6, 12],'%', sep = ''),
                                               paste(risk_race[6, 18],'%', sep = ''),
                                               paste(risk_race[6, 24],'%', sep = ''),
                                               paste(risk_race[6, 30],'%', sep = ''),
                                               paste(risk_race[6, 36],'%', sep = ''),
                                               paste(risk_race[6, 42],'%', sep = ''),
                                               paste(risk_race[6, 45],'%', sep = ''),
                                               paste(risk_race[6, 51],'%', sep = '')),
                             `White` = c(paste(risk_race[7, 6],'%', sep = ''),
                                         paste(risk_race[7, 12],'%', sep = ''),
                                         paste(risk_race[7, 18],'%', sep = ''),
                                         paste(risk_race[7, 24],'%', sep = ''),
                                         paste(risk_race[7, 30],'%', sep = ''),
                                         paste(risk_race[7, 36],'%', sep = ''),
                                         paste(risk_race[7, 42],'%', sep = ''),
                                         paste(risk_race[7, 45],'%', sep = ''),
                                         paste(risk_race[7, 51],'%', sep = '')),
                             `Unknown` = c(paste(risk_race[8, 6],'%', sep = ''),
                                           paste(risk_race[8, 12],'%', sep = ''),
                                           paste(risk_race[8, 18],'%', sep = ''),
                                           paste(risk_race[8, 24],'%', sep = ''),
                                           paste(risk_race[8, 30],'%', sep = ''),
                                           paste(risk_race[8, 36],'%', sep = ''),
                                           paste(risk_race[8, 42],'%', sep = ''),
                                           paste(risk_race[8, 45],'%', sep = ''),
                                           paste(risk_race[8, 51],'%', sep = '')))

risk_react_race <- reactable(risk_table, resizable = TRUE, showPageSizeOptions = TRUE,
                            onClick = 'expand', highlight = TRUE, rowStyle = list(cursor = 'pointer'),
                            details = function(index) {
                              if (index == 1) {
                                htmltools::div(style = "padding: 25px", ggplotly(adol_brths_race))
                              } else if (index == 2) {
                                htmltools::div(style = "padding: 25px", ggplotly(fruit_cr_race))
                              } else if (index == 3) {
                                htmltools::div(style = "padding: 25px", ggplotly(veg_cr_race))
                              } else if (index == 4) {
                                htmltools::div(style = "padding: 25px", ggplotly(phy_act_cr_race))
                              } else if (index == 5) {
                                htmltools::div(style = "padding: 25px", ggplotly(cig_smkng_cr_race))
                              } else if (index == 6) {
                                htmltools::div(style = "padding: 25px", ggplotly(ecig_cr_race))
                              } else if (index == 7) {
                                htmltools::div(style = "padding: 25px", ggplotly(bnge_drnkng_cr_race))
                              } else if (index == 8) {
                                htmltools::div(style = "padding: 25px", ggplotly(st_blt_cr_race))
                              }  
                            })

#########
#Outcomes
#########

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
                                                'Percent of Adults Who have Experienced Rape/Attempted Rape',
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
#Creates the drop down menu that shows the graph
outcomes_react <- reactable(outcomes_table, resizable = TRUE, showPageSizeOptions = TRUE,
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

#AA Outcomes
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
outcomes_react_aa <- reactable(outcomes_table_aa, resizable = TRUE, showPageSizeOptions = TRUE,
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
outcomes_table_pov <- bind_cols(`Access to Care Indicators by Poverty` = c('Infant Mortality Rate per 1,000 births',
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
                                                                           'Percent of Adults Who have Experienced Rape/Attempted Rape',
                                                                           'Percent of Adults Who had <7 days of Poor Physical Health in the Last 30 Days',
                                                                           'Percent of Adults Who had <7 days of Poor Mental Health in the Last 30 Days',
                                                                           'Percent of Adults Who have Diagnosed Arthritis',
                                                                           'Percent of Adults Who Currently have Diagnosed Asthma',
                                                                           'Percent of Adults Who have been Told they have Diabetes',
                                                                           'Percent of Adults Who have been Told they have Pre-diabetes'),
                            `Below` = c(paste(first(outcomes_pov$inf_mort_rate),'%', sep = ''),
                                        paste(first(outcomes_pov$unint_inj_cr),'%', sep = ''),
                                        paste(first(outcomes_pov$mvc_cr),'%', sep = ''),
                                        paste(first(outcomes_pov$drug_poi_cr),'%', sep = ''),
                                        paste(first(outcomes_pov$falls_cr),'%', sep = ''),
                                        paste(first(outcomes_pov$sui_cr),'%', sep = ''),
                                        paste(first(outcomes_pov$mort_diab_cr),'%', sep = ''),
                                        paste(first(outcomes_pov$mort_cvs_cr),'%', sep = ''),
                                        paste(first(outcomes_pov$mort_can_cr),'%', sep = ''),
                                        paste(first(outcomes_pov$mort_lung_can),'%', sep = ''),
                                        paste(first(outcomes_pov$mort_brst_can_cr),'%', sep = ''),
                                        paste(first(outcomes_pov$mort_colrect_can_cr),'%', sep = ''),
                                        paste(first(outcomes_pov$mort_pros_can_cr),'%', sep = ''),
                                        paste(first(outcomes_pov$mort_skin_can_cr),'%', sep = ''),
                                        paste(first(outcomes_pov$lw_brth_inf),'%', sep = ''),
                                        paste(first(outcomes_pov$preterm_births),'%', sep = ''),
                                        paste(first(outcomes_pov$rape_cr),'%', sep = ''),
                                        paste(first(outcomes_pov$phy_hlth_cr),'%', sep = ''),
                                        paste(first(outcomes_pov$ment_hlth_cr),'%', sep = ''),
                                        paste(first(outcomes_pov$arthrits_cr),'%', sep = ''),
                                        paste(first(outcomes_pov$astma_cr),'%', sep = ''),
                                        paste(first(outcomes_pov$diab_cr),'%', sep = ''),
                                        paste(first(outcomes_pov$prediabets_cr),'%', sep = '')),
                            `Above` = c(paste(last(outcomes_pov$inf_mort_rate),'%', sep = ''),
                                        paste(last(outcomes_pov$unint_inj_cr),'%', sep = ''),
                                        paste(last(outcomes_pov$mvc_cr),'%', sep = ''),
                                        paste(last(outcomes_pov$drug_poi_cr),'%', sep = ''),
                                        paste(last(outcomes_pov$falls_cr),'%', sep = ''),
                                        paste(last(outcomes_pov$sui_cr),'%', sep = ''),
                                        paste(last(outcomes_pov$mort_diab_cr),'%', sep = ''),
                                        paste(last(outcomes_pov$mort_cvs_cr),'%', sep = ''),
                                        paste(last(outcomes_pov$mort_can_cr),'%', sep = ''),
                                        paste(last(outcomes_pov$mort_lung_can),'%', sep = ''),
                                        paste(last(outcomes_pov$mort_brst_can_cr),'%', sep = ''),
                                        paste(last(outcomes_pov$mort_colrect_can_cr),'%', sep = ''),
                                        paste(last(outcomes_pov$mort_pros_can_cr),'%', sep = ''),
                                        paste(last(outcomes_pov$mort_skin_can_cr),'%', sep = ''),
                                        paste(last(outcomes_pov$lw_brth_inf),'%', sep = ''),
                                        paste(last(outcomes_pov$preterm_births),'%', sep = ''),
                                        paste(last(outcomes_pov$rape_cr),'%', sep = ''),
                                        paste(last(outcomes_pov$phy_hlth_cr),'%', sep = ''),
                                        paste(last(outcomes_pov$ment_hlth_cr),'%', sep = ''),
                                        paste(last(outcomes_pov$arthrits_cr),'%', sep = ''),
                                        paste(last(outcomes_pov$astma_cr),'%', sep = ''),
                                        paste(last(outcomes_pov$diab_cr),'%', sep = ''),
                                        paste(last(outcomes_pov$prediabets_cr),'%', sep = '')))

outcomes_react_pov <- reactable(outcomes_table, resizable = TRUE, showPageSizeOptions = TRUE,
                            onClick = 'expand', highlight = TRUE, rowStyle = list(cursor = 'pointer'),
                            details = function(index) {
                              if (index == 1) {
                                htmltools::div(style = "padding: 25px", ggplotly(inf_mort_rate_pov))
                              } else if (index == 2) {
                                htmltools::div(style = "padding: 25px", ggplotly(unint_inj_cr_pov))
                              } else if (index == 3) {
                                htmltools::div(style = "padding: 25px", ggplotly(mvc_cr_pov))
                              } else if (index == 4) {
                                htmltools::div(style = "padding: 25px", ggplotly(drug_poi_cr_pov))
                              } else if (index == 5) {
                                htmltools::div(style = "padding: 25px", ggplotly(falls_cr_pov))
                              } else if (index == 6) {
                                htmltools::div(style = "padding: 25px", ggplotly(sui_cr_pov))
                              } else if (index == 7) {
                                htmltools::div(style = "padding: 25px", ggplotly(mort_diab_cr_pov))
                              } else if (index == 8) {
                                htmltools::div(style = "padding: 25px", ggplotly(mort_cvs_cr_pov))
                              } else if (index == 9) {
                                htmltools::div(style = "padding: 25px", ggplotly(mort_can_cr_pov))
                              } else if (index == 10) {
                                htmltools::div(style = "padding: 25px", ggplotly(mort_lung_can_pov))
                              } else if (index == 11) {
                                htmltools::div(style = "padding: 25px", ggplotly(mort_brst_can_cr_pov))
                              } else if (index == 12) {
                                htmltools::div(style = "padding: 25px", ggplotly(mort_colrect_can_cr_pov))
                              } else if (index == 13) {
                                htmltools::div(style = "padding: 25px", ggplotly(mort_pros_can_cr_pov))
                              } else if (index == 14) {
                                htmltools::div(style = "padding: 25px", ggplotly(mort_skin_can_cr_pov))
                              } else if (index == 15) {
                                htmltools::div(style = "padding: 25px", ggplotly(lw_brth_inf_pov))
                              } else if (index == 16) {
                                htmltools::div(style = "padding: 25px", ggplotly(preterm_births_pov))
                              } else if (index == 17) {
                                htmltools::div(style = "padding: 25px", ggplotly(rape_cr_pov))
                              } else if (index == 18) {
                                htmltools::div(style = "padding: 25px", ggplotly(phy_hlth_cr_pov))
                              } else if (index == 19) {
                                htmltools::div(style = "padding: 25px", ggplotly(ment_hlth_cr_pov))
                              } else if (index == 20) {
                                htmltools::div(style = "padding: 25px", ggplotly(arthrits_cr_pov))
                              } else if (index == 21) {
                                htmltools::div(style = "padding: 25px", ggplotly(astma_cr_pov))
                              } else if (index == 22) {
                                htmltools::div(style = "padding: 25px", ggplotly(diab_cr_pov))
                              } else if (index == 23) {
                                htmltools::div(style = "padding: 25px", ggplotly(prediabets_cr_pov))
                              }
                            })

#Race
outcomes_table_race <- bind_cols(`Access to Care Indicators by Race/Ethnicity` = c('Infant Mortality Rate per 1,000 births',
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
                                                                                   'Percent of Adults Who have Experienced Rape/Attempted Rape',
                                                                                   'Percent of Adults Who had <7 days of Poor Physical Health in the Last 30 Days',
                                                                                   'Percent of Adults Who had <7 days of Poor Mental Health in the Last 30 Days',
                                                                                   'Percent of Adults Who have Diagnosed Arthritis',
                                                                                   'Percent of Adults Who Currently have Diagnosed Asthma',
                                                                                   'Percent of Adults Who have been Told they have Diabetes',
                                                                                   'Percent of Adults Who have been Told they have Pre-diabetes'),
                             `American Indian` = c(paste(outcomes_race[1, 3],'%', sep = ''),
                                                   paste(outcomes_race[1, 9],'%', sep = ''),
                                                   paste(outcomes_race[1, 15],'%', sep = ''),
                                                   paste(outcomes_race[1, 22],'%', sep = ''),
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
                                                   paste(outcomes_race[1, ],'%', sep = ''),
                                                   paste(outcomes_race[1, ],'%', sep = ''),
                                                   paste(outcomes_race[1, ],'%', sep = ''),
                                                   paste(outcomes_race[1, ],'%', sep = ''),
                                                   paste(outcomes_race[1, ],'%', sep = ''),
                                                   paste(outcomes_race[1, ],'%', sep = ''),
                                                   paste(outcomes_race[1, ],'%', sep = ''),
                                                   paste(outcomes_race[1, ],'%', sep = ''),
                                                   paste(outcomes_race[1, 51],'%', sep = '')),
                             `Asian` = c(paste(outcomes_race[2, 6],'%', sep = ''),
                                         paste(outcomes_race[2, 12],'%', sep = ''),
                                         paste(outcomes_race[2, 18],'%', sep = ''),
                                         paste(outcomes_race[2, 24],'%', sep = ''),
                                         paste(outcomes_race[2, 30],'%', sep = ''),
                                         paste(outcomes_race[2, 36],'%', sep = ''),
                                         paste(outcomes_race[2, 42],'%', sep = ''),
                                         paste(outcomes_race[2, 45],'%', sep = ''),
                                         paste(outcomes_race[2, 51],'%', sep = '')),
                             `Black` = c(paste(outcomes_race[3, 6],'%', sep = ''),
                                         paste(outcomes_race[3, 12],'%', sep = ''),
                                         paste(outcomes_race[3, 18],'%', sep = ''),
                                         paste(outcomes_race[3, 24],'%', sep = ''),
                                         paste(outcomes_race[3, 30],'%', sep = ''),
                                         paste(outcomes_race[3, 36],'%', sep = ''),
                                         paste(outcomes_race[3, 42],'%', sep = ''),
                                         paste(outcomes_race[3, 45],'%', sep = ''),
                                         paste(outcomes_race[3, 51],'%', sep = '')),
                             `Hispanic` = c(paste(outcomes_race[4, 6],'%', sep = ''),
                                            paste(outcomes_race[4, 12],'%', sep = ''),
                                            paste(outcomes_race[4, 18],'%', sep = ''),
                                            paste(outcomes_race[4, 24],'%', sep = ''),
                                            paste(outcomes_race[4, 30],'%', sep = ''),
                                            paste(outcomes_race[4, 36],'%', sep = ''),
                                            paste(outcomes_race[4, 42],'%', sep = ''),
                                            paste(outcomes_race[4, 45],'%', sep = ''),
                                            paste(outcomes_race[4, 51],'%', sep = '')),
                             `Pacific Islander` = c(paste(outcomes_race[5, 6],'%', sep = ''),
                                                    paste(outcomes_race[5, 12],'%', sep = ''),
                                                    paste(outcomes_race[5, 18],'%', sep = ''),
                                                    paste(outcomes_race[5, 24],'%', sep = ''),
                                                    paste(outcomes_race[5, 30],'%', sep = ''),
                                                    paste(outcomes_race[5, 36],'%', sep = ''),
                                                    paste(outcomes_race[5, 42],'%', sep = ''),
                                                    paste(outcomes_race[5, 45],'%', sep = ''),
                                                    paste(outcomes_race[5, 51],'%', sep = '')),
                             `Two or More` = c(paste(outcomes_race[6, 6],'%', sep = ''),
                                               paste(outcomes_race[6, 12],'%', sep = ''),
                                               paste(outcomes_race[6, 18],'%', sep = ''),
                                               paste(outcomes_race[6, 24],'%', sep = ''),
                                               paste(outcomes_race[6, 30],'%', sep = ''),
                                               paste(outcomes_race[6, 36],'%', sep = ''),
                                               paste(outcomes_race[6, 42],'%', sep = ''),
                                               paste(outcomes_race[6, 45],'%', sep = ''),
                                               paste(outcomes_race[6, 51],'%', sep = '')),
                             `White` = c(paste(outcomes_race[7, 6],'%', sep = ''),
                                         paste(outcomes_race[7, 12],'%', sep = ''),
                                         paste(outcomes_race[7, 18],'%', sep = ''),
                                         paste(outcomes_race[7, 24],'%', sep = ''),
                                         paste(outcomes_race[7, 30],'%', sep = ''),
                                         paste(outcomes_race[7, 36],'%', sep = ''),
                                         paste(outcomes_race[7, 42],'%', sep = ''),
                                         paste(outcomes_race[7, 45],'%', sep = ''),
                                         paste(outcomes_race[7, 51],'%', sep = '')),
                             `Unknown` = c(paste(outcomes_race[8, 6],'%', sep = ''),
                                           paste(outcomes_race[8, 12],'%', sep = ''),
                                           paste(outcomes_race[8, 18],'%', sep = ''),
                                           paste(outcomes_race[8, 24],'%', sep = ''),
                                           paste(outcomes_race[8, 30],'%', sep = ''),
                                           paste(outcomes_race[8, 36],'%', sep = ''),
                                           paste(outcomes_race[8, 42],'%', sep = ''),
                                           paste(outcomes_race[8, 45],'%', sep = ''),
                                           paste(outcomes_race[8, 51],'%', sep = '')))




outcomes_react_race <- reactable(outcomes_table, resizable = TRUE, showPageSizeOptions = TRUE,
                            onClick = 'expand', highlight = TRUE, rowStyle = list(cursor = 'pointer'),
                            details = function(index) {
                              if (index == 1) {
                                htmltools::div(style = "padding: 25px", ggplotly(inf_mort_rate_race))
                              } else if (index == 2) {
                                htmltools::div(style = "padding: 25px", ggplotly(unint_inj_cr_race))
                              } else if (index == 3) {
                                htmltools::div(style = "padding: 25px", ggplotly(mvc_cr_race))
                              } else if (index == 4) {
                                htmltools::div(style = "padding: 25px", ggplotly(drug_poi_cr_race))
                              } else if (index == 5) {
                                htmltools::div(style = "padding: 25px", ggplotly(falls_cr_race))
                              } else if (index == 6) {
                                htmltools::div(style = "padding: 25px", ggplotly(sui_cr_race))
                              } else if (index == 7) {
                                htmltools::div(style = "padding: 25px", ggplotly(mort_diab_cr_race))
                              } else if (index == 8) {
                                htmltools::div(style = "padding: 25px", ggplotly(mort_cvs_cr_race))
                              } else if (index == 9) {
                                htmltools::div(style = "padding: 25px", ggplotly(mort_can_cr_race))
                              } else if (index == 10) {
                                htmltools::div(style = "padding: 25px", ggplotly(mort_lung_can_race))
                              } else if (index == 11) {
                                htmltools::div(style = "padding: 25px", ggplotly(mort_brst_can_cr_race))
                              } else if (index == 12) {
                                htmltools::div(style = "padding: 25px", ggplotly(mort_colrect_can_cr_race))
                              } else if (index == 13) {
                                htmltools::div(style = "padding: 25px", ggplotly(mort_pros_can_cr_race))
                              } else if (index == 14) {
                                htmltools::div(style = "padding: 25px", ggplotly(mort_skin_can_cr_race))
                              } else if (index == 15) {
                                htmltools::div(style = "padding: 25px", ggplotly(lw_brth_inf_race))
                              } else if (index == 16) {
                                htmltools::div(style = "padding: 25px", ggplotly(preterm_births_race))
                              } else if (index == 17) {
                                htmltools::div(style = "padding: 25px", ggplotly(rape_cr_race))
                              } else if (index == 18) {
                                htmltools::div(style = "padding: 25px", ggplotly(phy_hlth_cr_race))
                              } else if (index == 19) {
                                htmltools::div(style = "padding: 25px", ggplotly(ment_hlth_cr_race))
                              } else if (index == 20) {
                                htmltools::div(style = "padding: 25px", ggplotly(arthrits_cr_race))
                              } else if (index == 21) {
                                htmltools::div(style = "padding: 25px", ggplotly(astma_cr_race))
                              } else if (index == 22) {
                                htmltools::div(style = "padding: 25px", ggplotly(diab_cr_race))
                              } else if (index == 23) {
                                htmltools::div(style = "padding: 25px", ggplotly(prediabets_cr_race))
                              }
                            })

#Save Items
tmp.env <- new.env()

tmp.env$dem_react <- dem_react
tmp.env$pop <- pop
tmp.env$ethn <- ethn
tmp.env$race <- race
tmp.env$birth_rate <- birth_rate
tmp.env$cr_dr <- cr_dr
tmp.env$aa_dr <- aa_dr
tmp.env$dem_wide <- dem_wide

tmp.env$hlth_react <- hlth_react
tmp.env$hlth_wide <- hlth_wide
tmp.env$ins <- ins
tmp.env$no_care <- no_care
tmp.env$hlth_pro <- hlth_pro
tmp.env$med_check <- med_check
tmp.env$dent_check <- dent_check
tmp.env$mammo <- mammo
tmp.env$col_scree <- col_scree
tmp.env$pn_perc <- pn_perc
tmp.env$fluvac <- fluvac

tmp.env$hlth_pov_table <- hlth_pov_table
tmp.env$hlth_pov_react <- hlth_pov_react
tmp.env$hlth_pov <- hlth_pov
tmp.env$ins_pov <- ins_pov
tmp.env$no_care_pov <- no_care_pov
tmp.env$hlth_pro_pov <- hlth_pro_pov
tmp.env$med_check_pov <- med_check_pov
tmp.env$dent_check_pov <- dent_check_pov
tmp.env$mammo_pov <- mammo_pov
tmp.env$col_scree_pov <- col_scree_pov
tmp.env$pn_perc_pov <- pn_perc_pov
tmp.env$fluvac_pov <- fluvac_pov

tmp.env$hlth_table_race <- hlth_table_race
tmp.env$hlth_react_race <- hlth_react_race
tmp.env$hlth_race <- hlth_race
tmp.env$ins_race <- ins_race
tmp.env$no_care_race <- no_care_race
tmp.env$hlth_pro_race <- hlth_pro_race
tmp.env$med_check_race <- med_check_race
tmp.env$dent_check_race <- dent_check_race
tmp.env$mammo_race <- mammo_race
tmp.env$col_scree_race <- col_scree_race
tmp.env$pn_perc_race <- pn_perc_race
tmp.env$fluvac_race <- fluvac_race

tmp.env$risk_react <- risk_react
tmp.env$risk_wide <- risk_wide
tmp.env$adol_brths <- adol_brths
tmp.env$fruit_cr <- fruit_cr
tmp.env$veg_cr <- veg_cr
tmp.env$phy_act_cr <- phy_act_cr
tmp.env$cig_smkng_cr <- cig_smkng_cr
tmp.env$ecig_cr <- ecig_cr
tmp.env$bnge_drnkng_cr <- bnge_drnkng_cr
tmp.env$st_blt_cr <- st_blt_cr


tmp.env$risk_react_aa <- risk_react_aa
tmp.env$aa_fruit <- aa_fruit
tmp.env$aa_veg <- aa_veg
tmp.env$aa_phy_act <- aa_phy_act
tmp.env$aa_cig_smkng <- aa_cig_smkng
tmp.env$aa_ecig <- aa_ecig
tmp.env$aa_bnge_drnkng <- aa_bnge_drnkng
tmp.env$aa_st_blt <- aa_st_blt

tmp.env$risk_table_pov <- risk_table_pov
tmp.env$risk_react_pov <- risk_react_pov
tmp.env$risk_wide_pov <- risk_wide_pov
tmp.env$adol_brths_pov <- adol_brths_pov
tmp.env$fruit_cr_pov <- fruit_cr_pov
tmp.env$veg_cr_pov <- veg_cr_pov
tmp.env$phy_act_cr_pov <- phy_act_cr_pov
tmp.env$cig_smkng_cr_pov <- cig_smkng_cr_pov
tmp.env$ecig_cr_pov <- ecig_cr_pov
tmp.env$bnge_drnkng_cr_pov <- bnge_drnkng_cr_pov
tmp.env$st_blt_cr_pov <- st_blt_cr_pov

tmp.env$risk_table_race <- risk_table_race
tmp.env$risk_react_race <- risk_react_race
tmp.env$risk_wide_race <- risk_wide_race
tmp.env$adol_brths_race <- adol_brths_race
tmp.env$fruit_cr_race <- fruit_cr_race
tmp.env$veg_cr_race <- veg_cr_race
tmp.env$phy_act_cr_race <- phy_act_cr_race
tmp.env$cig_smkng_cr_race <- cig_smkng_cr_race
tmp.env$ecig_cr_race <- ecig_cr_race
tmp.env$bnge_drnkng_cr_race <- bnge_drnkng_cr_race
tmp.env$st_blt_cr_race <- st_blt_cr_race

tmp.env$outcomes_react <- outcomes_react
tmp.env$outcomes_wide <- outcomes_wide
tmp.env$outcomes_table <- outcomes_table
tmp.env$inf_mort_rate <- inf_mort_rate
tmp.env$unint_inj_cr <- unint_inj_cr
tmp.env$mvc_cr <- mvc_cr
tmp.env$drug_poi_cr <- drug_poi_cr
tmp.env$falls_cr <- falls_cr
tmp.env$sui_cr <- sui_cr
tmp.env$mort_diab_cr <- mort_diab_cr
tmp.env$mort_cvs_cr <- mort_cvs_cr
tmp.env$mort_can_cr <- mort_can_cr
tmp.env$mort_lung_can <- mort_lung_can
tmp.env$mort_brst_can_cr <- mort_brst_can_cr
tmp.env$mort_colrect_can_cr <- mort_colrect_can_cr
tmp.env$mort_pros_can_cr <- mort_pros_can_cr
tmp.env$mort_skin_can_cr <- mort_skin_can_cr
tmp.env$lw_brth_inf <- lw_brth_inf
tmp.env$preterm_births <- preterm_births
tmp.env$rape_cr <- rape_cr
tmp.env$phy_hlth_cr <- phy_hlth_cr
tmp.env$ment_hlth_cr <- ment_hlth_cr
tmp.env$arthrits_cr <- arthrits_cr
tmp.env$astma_cr <- astma_cr 
tmp.env$diab_cr <- diab_cr
tmp.env$prediabets_cr <- prediabets_cr

tmp.env$outcomes_react_aa <- outcomes_react_aa
tmp.env$aa_unint_inj <- aa_unint_inj
tmp.env$aa_mvc <- aa_mvc
tmp.env$aa_drug_poi <- aa_drug_poi
tmp.env$aa_falls <- aa_falls
tmp.env$aa_sui <- aa_sui
tmp.env$aa_mort_diab <- aa_mort_diab
tmp.env$aa_mort_cvs <- aa_mort_cvs
tmp.env$aa_mort_can <- aa_mort_can
tmp.env$aa_mort_lung_can <- aa_mort_lung_can
tmp.env$aa_mort_brst_can <- aa_mort_brst_can
tmp.env$aa_mort_colrect_can <- aa_mort_colrect_can
tmp.env$aa_mort_pros_can <- aa_mort_pros_can
tmp.env$aa_mort_skin_can <- aa_mort_skin_can
tmp.env$aa_rape <- aa_rape
tmp.env$aa_phy_hlth <- aa_phy_hlth
tmp.env$aa_ment_hlth <- aa_ment_hlth
tmp.env$aa_arthrits <- aa_arthrits
tmp.env$aa_astma <- aa_astma 
tmp.env$aa_diab <- aa_diab
tmp.env$aa_prediabets <- aa_prediabets

tmp.env$outcomes_react_pov <- outcomes_react_pov
tmp.env$outcomes_wide_pov <- outcomes_wide_pov
tmp.env$outcomes_table_pov <- outcomes_table_pov
tmp.env$inf_mort_rate_pov <- inf_mort_rate_pov
tmp.env$unint_inj_cr_pov <- unint_inj_cr_pov
tmp.env$mvc_cr_pov <- mvc_cr_pov
tmp.env$drug_poi_cr_pov <- drug_poi_cr_pov
tmp.env$falls_cr_pov <- falls_cr_pov
tmp.env$sui_cr_pov <- sui_cr_pov
tmp.env$mort_diab_cr_pov <- mort_diab_cr_pov
tmp.env$mort_cvs_cr_pov <- mort_cvs_cr_pov
tmp.env$mort_can_cr_pov <- mort_can_cr_pov
tmp.env$mort_lung_can_pov <- mort_lung_can_pov
tmp.env$mort_brst_can_cr_pov <- mort_brst_can_cr_pov
tmp.env$mort_colrect_can_cr_pov <- mort_colrect_can_cr_pov
tmp.env$mort_pros_can_cr_pov <- mort_pros_can_cr_pov
tmp.env$mort_skin_can_cr_pov <- mort_skin_can_cr_pov
tmp.env$lw_brth_inf_pov <- lw_brth_inf_pov
tmp.env$preterm_births_pov <- preterm_births_pov
tmp.env$rape_cr_pov <- rape_cr_pov
tmp.env$phy_hlth_cr_pov <- phy_hlth_cr_pov
tmp.env$ment_hlth_cr_pov <- ment_hlth_cr_pov
tmp.env$arthrits_cr_pov <- arthrits_cr_pov
tmp.env$astma_cr_pov <- astma_cr_pov 
tmp.env$diab_cr_pov <- diab_cr_pov
tmp.env$prediabets_cr_pov <- prediabets_cr_pov

tmp.env$outcomes_react_race <- outcomes_react_race
tmp.env$outcomes_wide_race <- outcomes_wide_race
tmp.env$outcomes_table_race <- outcomes_table_race
tmp.env$inf_mort_rate_race <- inf_mort_rate_race
tmp.env$unint_inj_cr_race <- unint_inj_cr_race
tmp.env$mvc_cr_race <- mvc_cr_race
tmp.env$drug_poi_cr_race <- drug_poi_cr_race
tmp.env$falls_cr_race <- falls_cr_race
tmp.env$sui_cr_race <- sui_cr_race
tmp.env$mort_diab_cr_race <- mort_diab_cr_race
tmp.env$mort_cvs_cr_race <- mort_cvs_cr_race
tmp.env$mort_can_cr_race <- mort_can_cr_race
tmp.env$mort_lung_can_race <- mort_lung_can_race
tmp.env$mort_brst_can_cr_race <- mort_brst_can_cr_race
tmp.env$mort_colrect_can_cr_race <- mort_colrect_can_cr_race
tmp.env$mort_pros_can_cr_race <- mort_pros_can_cr_race
tmp.env$mort_skin_can_cr_race <- mort_skin_can_cr_race
tmp.env$lw_brth_inf_race <- lw_brth_inf_race
tmp.env$preterm_births_race <- preterm_births_race
tmp.env$rape_cr_race <- rape_cr_race
tmp.env$phy_hlth_cr_race <- phy_hlth_cr_race
tmp.env$ment_hlth_cr_race <- ment_hlth_cr_race
tmp.env$arthrits_cr_race <- arthrits_cr_race
tmp.env$astma_cr_race <- astma_cr_race 
tmp.env$diab_cr_race <- diab_cr_race
tmp.env$prediabets_cr_race <- prediabets_cr_race

save(list = ls(all.names = TRUE, pos = tmp.env), envir = tmp.env,
     file = paste0("dashdata",
                   Sys.Date(), ".Rdata"))

save(list = ls(all.names = TRUE, pos = tmp.env), envir = tmp.env,
     file = "C:/Users/tstoker/Desktop/RWD/CHA_Dash/BRHD_CHA/dashdata.Rdata")

rm(tmp.env)
