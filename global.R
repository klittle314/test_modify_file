#shiny app allows users to load data from an Excel template to update a master Google sheet
# Kevin Little, Ph.D.  Informing Ecological Design, LLC  June-August 2016

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
library(shinyBS)
source("helper.R")

#will need to have the org file in the directory when converting to shinyapps.io
path1 <- "Applications and selections  07-29-16.xlsx"
clinic_table <- read.xlsx(path1, sheet="update 28 July 2016")
clinic_table <- clinic_table[1:20,]
clinic_names <- clinic_table$Clinic.Name

#load the googlesheet
#gskey2 <- c("1dN9rj--OEghw7DdOO0f0y1dcObm2GQwvpbEPWcvAZUU")
#gskey2 <- c("1m6oVBbHRKb3UuDyImH1Tvl2nUXkQTwLi3Le2kwsciC4")
#working sheet July 2016
gskey2 <- c("1tOYZfT6ZGRw06UabraijdgXtPu_J4a6oifvlWwRfsXg")
#master sheet with 20 clinics
#gskey2 <- c("12XUHTunbyWQDG7eXusKPk3zJBAsjIBcj4ipCq_h4wHA")
gsobj <- gs_key(x=gskey2)



#retrieve google sheet data, force as data.frame else the object is a tbl that seems to confuse other
#functions expecting a dataframe

df_master1 <- as.data.frame(gs_read(ss=gsobj,ws="Summary_Data"))
#assumes the Summary Data has already been cleaned--starts in a clean state.
df_master1 <- clean_up_df1(df_master1)
#sort df_master1 to have clinics in alpha order (contiguous records), and date ordered within clinic
#in case data have been appended in earlier sessions
#25 July 2016:  do not change the ordering of the values in the master data table
#df_master1 <- df_master1[order(df_master1$ClinicName,df_master1$RepMonth),] 

#now melt the df for manipulation, omitting the Reporting Month, column 2 and setting up for plotting
df_melt <- melt(df_master1[,-2],id.vars=c("ClinicName","MeasMonth"),variable.name="Measure")

df_melt$ClinicName <- as.factor(df_melt$ClinicName)
#append measure type 
df_melt$MeasType <- measure_type_maker(df_melt)
