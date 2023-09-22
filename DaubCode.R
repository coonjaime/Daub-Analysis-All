#DAUB ANALYSIS - ALL YEARS OMG #
#.....................................................................----
#1. METADATA                                                          ####
#.....................................................................----

#Authors of this code: Jaime Coon.

#Abbreviations defined:
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
setwd("~/Library/CloudStorage/Dropbox/_Manuscripts/BHCO Project/Prelim-BHCO-Analysis")

#install.packages('easypackages')
library('easypackages')
packages('tidyverse','janitor','glmmTMB','ggeffects','AICcmodavg','readxl','writexl','patchwork')

NestData_Species<-read_excel("Nests_By_Species.xlsx")

VegData<-read_excel("2023_Daub_Data.xlsx")

NestData_Treatment<-read_excel("Nests_By_Treatment_Site.xlsx")%>%
  mutate(Nests_per_ha=Total_Nests/Area)%>%
  mutate(Percent_Parasitized=Parasitized_Nests/Total_Nests)%>%
  mutate(Percent_Successful=Successful_Nests/Complete_Nests)


#.....................................................................----
#3. GRAPHING  DATA                                       ####
#.....................................................................----
#__3a.By Site####
Veg_Avg_Data = VegData %>%
  mutate(Avg_Robel=(Robel.N+Robel.E+Robel.S+Robel.W)/4)%>%
  group_by(Pasture)%>%
  summarise_at(vars(WSG,FEAR,CHEAT,Total_CSG,CSG_noF,Forbs,Legumes,Woody,Litter,SdMixForbs,Panic,Avg_Robel,Litter_dep),mean)%>%
  mutate_if(is.numeric,round,digits=3)

write_xlsx(Veg_Avg_Data,"Veg_Avg_Data.xlsx")