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
library(zoo)
source("helper.R")

clinic_list <- c("C1"="C1", "C2"= "C2", "C3"="C3", "C4"="C4","C5"="C5")
#test change in folder

#load the googlesheet
#gskey2 <- c("1dN9rj--OEghw7DdOO0f0y1dcObm2GQwvpbEPWcvAZUU")
#gskey2 <- c("1m6oVBbHRKb3UuDyImH1Tvl2nUXkQTwLi3Le2kwsciC4")
gskey2 <- c("1tOYZfT6ZGRw06UabraijdgXtPu_J4a6oifvlWwRfsXg")
gsobj <- gs_key(x=gskey2)



#retrieve google sheet data, force as data.frame else the object is a tbl that seems to confuse other
#functions expecting a dataframe

df_master1 <- as.data.frame(gs_read(ss=gsobj,ws="Summary_Data"))
#assumes the Summary Data has already been cleaned--starts in a clean state.
#df_master1 <- clean_up_df1(df_master1)
#sort df_master1 to have clinics in alpha order (contiguous records), and date ordered within clinic
#in case data have been appended in earlier sessions
df_master1 <- df_master1[order(df_master1$ClinicName,df_master1$RepMonth),] 

#now melt the df for manipulation, omitting the Reporting Month, column 2 and setting up for plotting
df_melt <- melt(df_master1[,-2],id.vars=c("ClinicName","MeasMonth"),variable.name="Measure")

df_melt$ClinicName <- as.factor(df_melt$ClinicName)
#append measure type 
df_melt$MeasType <- measure_type_maker(df_melt)
