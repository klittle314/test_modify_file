#shiny app allows users to load data from an Excel template to update a master Google sheet
# Kevin Little, Ph.D.  Informing Ecological Design, LLC  June-August 2016

library(Rcpp)
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
library(gtable)
library(GGally)
# library(plyr)
# library(dplyr)
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
#gskey2 <- c("1tOYZfT6ZGRw06UabraijdgXtPu_J4a6oifvlWwRfsXg")
#master sheet with 20 clinics updated 5 Aug 2016
#gskey2 <- c("12XUHTunbyWQDG7eXusKPk3zJBAsjIBcj4ipCq_h4wHA")
gskey2 <- c("1ia4R53Q1P8EGy5Yg4CKq0iCFk9CNOSV1Z73jhZ677IA")
gsobj <- gs_key(x=gskey2)

#retrieve google sheet data, force as data.frame else the object is a tbl that seems to confuse other
#functions expecting a dataframe

df_master1 <- as.data.frame(gs_read(ss=gsobj,ws="Summary_Data"))
#assumes the Summary Data has already been cleaned--starts in a clean state.
df_master1 <- clean_up_df1(df_master1)

#now melt the df for manipulation, omitting the Reporting Month, column 2 and setting up for plotting
df_melt <- melt(df_master1[,-2],id.vars=c("ClinicName","MeasMonth"),variable.name="Measure")

df_melt$ClinicName <- as.factor(df_melt$ClinicName)


#append measure type  
df_melt$MeasType <- measure_type_maker(df_melt)

#make a copy of the df before stripping off the goals
df_melt1 <- df_melt

#strip off goals and associate goals with the measures
df_melt <- goal_melt_df(df_melt)

#read clinic names and short names, append short_names to df_melt
df_clinic_names <- read.xlsx("Applications and selections  07-29-16.xlsx",sheet="short_names", rows=c(1:21))

#now trim trailing white spaces
df_clinic_names$Short.Name <- trim.trailing(df_clinic_names$Short.Name)

df_melt$ShortName <- mapvalues(df_melt$ClinicName,from=df_clinic_names$Clinic.Name,df_clinic_names$Short.Name)
df_melt1$ShortName <- mapvalues(df_melt1$ClinicName,from=df_clinic_names$Clinic.Name,df_clinic_names$Short.Name)
#subset of measures for individual clinic display
# meas_subset <- levels(df_melt$Measure)[1:12]

#create a factor vector with measure names to match wide format in ui Data Table: 1872 records
MNames <- levels(df_melt1$Measure)[seq(3,39, by=3)]
#str(MNames)
#now create a vector of measure names
MeasName1 <- as.data.frame(sapply(MNames,rep, 108,simplify=TRUE), stringsAsFactors=FALSE)
MeasName1.1<- stack(MeasName1)
MeasName1.2 <- MeasName1.1$values
MeasName2 <- as.data.frame(sapply(MNames,rep, 36,simplify=TRUE), stringsAsFactors=FALSE)
MeasName2.1<- stack(MeasName2)
MeasName2.2 <- MeasName2.1$values
MeasName <- c(MeasName1.2,MeasName2.2)
MeasName <- factor(MeasName, levels=c("OM1","PM1","PM2","PM3",
                                      "PM4","PM5","PM6","PM7","OPM1",
                                      "OPM2","OPM3","OPM4","OPM5"))
