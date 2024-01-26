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

Veg_Pasture       = read_csv("Veg_Avg_Data_Pasture.csv")
Veg_Pasture_Patch = read_csv("Veg_Avg_Data_PasturePasture.csv")
Veg_Raw           = read_csv("Veg_Raw_Data.csv")

#.....................................................................----
#3. GRAPHING  DATA                                       ####
#.....................................................................----
#__3a.By Site####

Veg_Pasture_Filtered=Veg_Pasture%>%
  filter(!pasture=="KELL")%>%
  filter(!pasture=="NA")%>%
  filter(!pasture=="RIE")%>%
  filter(!pasture=="RNR")%>%
  #filter(pasture %in% c("KLN","LTR","PYN"))%>%
  mutate(pasture=recode(pasture,
                        "RCH2007"="RCH",
                        "RCH2014"="RCH"))
  
  
#Bar graph
fearPerYear_Plot=ggplot(data=Veg_Pasture_Filtered) +  
  #geom_line(aes(x=AcademicYear, y=percent_of_total_count_of_id, group=major, color=major),size=1.5)+
  #geom_point(aes(x=AcademicYear, y=percent_of_total_count_of_id, group=major, color=major),size=3)+
  #scale_color_manual(values=c("#6fc5b4","#61a382","#446f6b","#6d8aa0","#6acee0","black"))+
  geom_bar(aes(x=as.factor(year),y=fear_mean,fill=pasture),stat="identity")+
  theme_bar_noleg()+ylab("% Cover Tall fear")+xlab("Year")+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  geom_errorbar(aes(x=as.factor(year),y=fear_mean,ymin=fear_mean-fear_sd,ymax=fear_mean+fear_sd))+
  #scale_x_discrete(expand = c(0.05, 0.05, .25, 0.05)) +
  #geom_label(aes(x=year,y=fear,label = fear), nudge_y=0,nudge_x = .55, size = 3)+
  facet_wrap(~pasture)
fearPerYear_Plot
#ggsave(BiologyPerYear_Plot,filename="BiologyPerYear_Plot.jpg",dpi=300,units="in",h=5,w=8)

#Boxplot
#fearPerYear_Boxplot=ggplot(data=Veg_Raw) +  
#  geom_boxplot(aes(x=as.factor(year),y=fear))+
#  theme_bar_leg()+ylab("% Cover Tall fear")+xlab("Year")+ 
#  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
#  facet_wrap(~pasture)
#fearPerYear_Boxplot


WSGPerYear_Plot=ggplot(data=Veg_Pasture_Filtered) +  
  #geom_line(aes(x=AcademicYear, y=percent_of_total_count_of_id, group=major, color=major),size=1.5)+
  #geom_point(aes(x=AcademicYear, y=percent_of_total_count_of_id, group=major, color=major),size=3)+
  #scale_color_manual(values=c("#6fc5b4","#61a382","#446f6b","#6d8aa0","#6acee0","black"))+
  geom_bar(aes(x=as.factor(year),y=wsg_mean,fill=pasture),stat="identity")+
  theme_bar_noleg()+ylab("% Cover WSG")+xlab("Year")+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  geom_errorbar(aes(x=as.factor(year),y=wsg_mean,ymin=wsg_mean-wsg_sd,ymax=wsg_mean+wsg_sd))+
  #scale_x_discrete(expand = c(0.05, 0.05, .25, 0.05)) +
  #geom_label(aes(x=year,y=fear,label = fear), nudge_y=0,nudge_x = .55, size = 3)+
  facet_wrap(~pasture)
WSGPerYear_Plot
