library(Rcpp)
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
library(gtable)
library(GGally)
library(dplyr)
library(plyr)
library(reshape2)
library(stats)
library(tidyr)
library(shiny)
library(shinyapps)
library(openxlsx)
library(googlesheets) 
source("helper.R")

clinic_list <- c("Clinic 1"="C1", "Clinic 2"= "C2", "Clinic 3"="C3", "Clinic 4"="C4","Clinic 5"="C5")

#load the googlesheet
#gskey2 <- c("1dN9rj--OEghw7DdOO0f0y1dcObm2GQwvpbEPWcvAZUU")
gskey2 <- c("1m6oVBbHRKb3UuDyImH1Tvl2nUXkQTwLi3Le2kwsciC4")
gsobj <- gs_key(x=gskey2)



#retrieve google sheet data, force as data.frame else the object is a tbl that seems to confuse other
#functions expecting a dataframe

df_master1 <- as.data.frame(gs_read(ss=gsobj,ws="Summary_Data"))
#sort df_master1 to have clinics in alpha order (contiguous records), and date ordered within clinic
#in case data have been appended in earlier sessions
df_master1 <- df_master1[order(df_master1$ClinicName,df_master1$RepMonth),] 

#now melt the df for manipulation
df_melt <- melt(df_master1[,-2],id.vars=c("ClinicName","MeasMonth"),variable.name="Measure")