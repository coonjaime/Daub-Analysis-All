#DAUB ANALYSIS - ALL YEARS OMG #
#.....................................................................----
#1. METADATA                                                          ####
#.....................................................................----

#Authors of this code: Jaime Coon.

#Abbreviations defined:
##235 - Kellerton 235
##BSH - Besh Offset
##DUN - Dunn Ranch
##GIL - Gilleland
##KLN - Kellerton North
##KLL - Kell (Prairie Chicken Area)
##KLT - Kellerton Tauke
##LTR - Lee Trail Road
##RC2 - Richardson 2 (Richardson Hay)
##RCH2007 - Richardson from 2007-2013
##RCH2014 - Richardson from 2014-Present
##RNR - Ringgold North Richardson
##RIS - Ringgold South
##RIN - Ringgold North
##RIE - Ringgold East
##PAW - Pawnee Prairie
##PYN - Pyland North
##PYS - Pyland South
##PYW - Pyland West

#.....................................................................----
#2. SETUP                                                          ####
#.....................................................................----

#ggplot themes
theme_bar_noleg <- function () { 
  theme(text=element_text(size=12),
        axis.title=element_text(face="bold", size=12),
        # axis.title.y=element_blank(),
        axis.text=element_text(size=12,color="black"),
        axis.line=element_line(color="black",size=1),
        panel.background=element_rect("white"),
        panel.grid=element_blank(),
        legend.position="none",
        legend.text=element_text(size=12, color="black"))}

theme_bar_yaxisnoleg <- function () { 
  theme(text=element_text(size=12),
        axis.title=element_text(face="bold", size=12),
        axis.text=element_text(size=12,color="black"),
        axis.line=element_line(color="black",size=1),
        panel.background=element_rect("snow2"),
        panel.grid=element_blank(),
        legend.position="none",
        legend.text=element_text(size=12, color="black"))}
theme_bar_leg <- function () { 
  theme(text=element_text(size=12),
        axis.title=element_text(face="bold", size=12),
        axis.text=element_text(size=12,color="black"),
        axis.line=element_line(color="black",size=1),
        panel.background=element_rect("snow2"),
        panel.grid=element_blank(),
        legend.position="right",
        legend.title=element_text(face="bold", size=12),
        legend.text=element_text(size=12, color="black"))}


#.....................................................................----
#3. IMPORTING AND CLEANING DATA                                       ####
#.....................................................................----
setwd("~/Dropbox/_Manuscripts/DaubCode/Daub-Analysis-All")

#install.packages('easypackages')
library('easypackages')
packages('tidyverse','janitor','TMB','glmmTMB','TMB','ggeffects','AICcmodavg','readxl','writexl','patchwork')


VegData<-read_csv("DaubData_AllYears_2007-2018,2021-2023_JJC_1.26.2024.csv")%>%
  mutate(avg_robel=(Robel.N+Robel.E+Robel.S+Robel.W)/4)%>%
  rowwise() %>% 
  mutate(sd_robel     = sd(c_across(Robel.N:Robel.E)))%>%
  mutate(CSG_FEAR     = if_else(Year < 2022, NA_character_, CSG_FEAR))%>%
  mutate(CHEAT     = if_else(Year < 2022, NA_character_, CSG_FEAR))%>%
  mutate(Litter_dep   = if_else(Litter_dep > 80, NA_character_, Litter_dep))%>%
  
  mutate(Sedges       = fct_recode(Sedges, NULL = "."),
         Sedges       = as.numeric(as.character(Sedges)),
         
         CSG_FEAR     = fct_recode(CSG_FEAR, NULL = "."),
         CSG_FEAR     = as.numeric(as.character(CSG_FEAR)),
         
         CHEAT        = fct_recode(CHEAT, NULL = "."),
         CHEAT        = as.numeric(as.character(CHEAT)),
         
         Violets      = fct_recode(Violets, NULL = "."),
         Violets      = as.numeric(as.character(Violets)),
         
         Panic        = fct_recode(Panic, NULL = "."),
         Panic        = as.numeric(as.character(Panic)),
         
         Seed_mix_forbs     = fct_recode(Seed_mix_forbs, NULL = "."),
         Seed_mix_forbs     = as.numeric(as.character(Seed_mix_forbs)),
         
         Litter_dep = fct_recode(Litter_dep, NULL = "."),
         Litter_dep = as.numeric(as.character(Litter_dep)))%>%
  #filter(Year>2007)%>%
  clean_names()%>%
  mutate(litter               = ifelse(year <  2012,NA_real_,litter))%>% #this replaces zeros with NAs for years we did not collect data
  mutate(bare                 = ifelse(year <  2012,NA_real_,bare))%>% #this replaces zeros with NAs for years we did not collect data
  mutate(litter_dep           = ifelse(year <  2012,NA_real_,litter_dep))%>%
  mutate(sedges               = ifelse(year >  2016,NA_real_,sedges))%>%
  mutate(panic                = ifelse(year <  2014,NA_real_,panic))%>%
  mutate(seed_cover_total     = ifelse(year <  2017,NA_real_,seed_cover_total))%>%
  mutate(seed_mix_forbs       = ifelse(year <  2014,NA_real_,seed_mix_forbs))%>%
  mutate(sdv                  = ifelse(year <  2014,NA_real_,sdv))%>%
  mutate(sdv                  = ifelse(year == 2017,NA_real_,sdv))%>%
  mutate(sdv                  = ifelse(year == 2018,NA_real_,sdv))%>%
  mutate(sdv                  = ifelse(year == 2021,NA_real_,sdv))

write_csv(VegData,"Veg_Raw_Data.csv")

str(VegData)
#.....................................................................----
#3. SAVING  DATA                                       ####
#.....................................................................----

Veg_Avg_Data_Year = VegData %>%
  group_by(year)%>%
  summarise_at(vars(wsg:sd_robel),list(mean="mean",sd="sd"), na.rm=TRUE)
Veg_Avg_Data_Year

write_csv(Veg_Avg_Data_Year,"Veg_Avg_Data.csv")

Veg_Avg_Data_Pasture = VegData %>%
  group_by(year,pasture)%>%
  summarise_at(vars(wsg:sd_robel),list(mean="mean",sd="sd"), na.rm=TRUE)
Veg_Avg_Data_Pasture
write_csv(Veg_Avg_Data_Pasture,"Veg_Avg_Data_Pasture.csv")

Veg_Avg_Data_Pasture_Patch = VegData %>%
  group_by(year,pasture, pasture_patch,pasture_patch_year)%>%
  summarise_at(vars(wsg:sd_robel),list(mean="mean",sd="sd"), na.rm=TRUE)
Veg_Avg_Data_Pasture_Patch

write_csv(Veg_Avg_Data_Pasture_Patch,"Veg_Avg_Data_PasturePasture.csv")


