#shiny app allows users to load data from an Excel template to update a master Google sheet
# Kevin Little, Ph.D.  Informing Ecological Design, LLC  June-Nov 2016

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
library(DT)
library(shinyapps)
library(openxlsx)
library(googlesheets) 
library(zoo)
library(shinyBS)
source("helper.R")

#will need to have the org file in the directory when converting to shinyapps.io
path1 <- "Applications and selections  07-29-16.xlsx"
clinic_table <- read.xlsx(path1, sheet="update 28 July 2016")
clinic_table <- clinic_table[1:26,]
clinic_names <- clinic_table$Clinic.Name

path2 <- "abbreviated measure names.xlsx"
measname_table <- read.xlsx(path2,sheet="lookup")

#load the googlesheet
#test sheet to avoid contaminating the master data table: TEST_NNOHA Dental Dashboard Tracking Tool 30 Aug 2016
#gskey2 <- c("1moCxJ0bw5D0B3OqZ-iqJhN8MUMyhiwJx9U5_aWPQYlo")

#working sheet NNOHA Dental Dashboard Tracking Tool 30 Aug 2016:  take the coded from the URL of the sheet
gskey2 <- c("1_iIVDAS1gzqr6KYcYZiNicE3o7anPJNXQJTWcrLIfW4")
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
df_melt <- droplevels(goal_melt_df(df_melt))

#read clinic names and short names, append short_names to df_melt
df_clinic_names <- read.xlsx("Applications and selections  07-29-16.xlsx",sheet="short_names", rows=c(1:27))

#now trim trailing white spaces
df_clinic_names$Short.Name <- trim.trailing(df_clinic_names$Short.Name)

df_melt$ShortName <- plyr::mapvalues(df_melt$ClinicName,from=df_clinic_names$Clinic.Name,df_clinic_names$Short.Name)
df_melt1$ShortName <- plyr::mapvalues(df_melt1$ClinicName,from=df_clinic_names$Clinic.Name,df_clinic_names$Short.Name)

df_melt$MeasName <- plyr::mapvalues(df_melt$Measure,from=measname_table$Code,measname_table$Abbreviation)
df_melt1$MeasName <- plyr::mapvalues(df_melt1$Measure,from=measname_table$Code,measname_table$Abbreviation)

#subset of measures for individual clinic display
# meas_subset <- levels(df_melt$Measure)[1:12]

#create a factor vector with measure names to match wide format in ui Data Table: 2016 records
#this variable is used to create the table ui output and to determine the correct axis labels for plots
MNames <- levels(df_melt1$Measure)[seq(3,42, by=3)]
#str(MNames)
#now create a vector of measure names
MeasName1 <- as.data.frame(sapply(MNames,rep, 108,simplify=TRUE), stringsAsFactors=FALSE)
MeasName1.1<- stack(MeasName1)
MeasName1.2 <- MeasName1.1$values
MeasName2 <- as.data.frame(sapply(MNames,rep, 36,simplify=TRUE), stringsAsFactors=FALSE)
MeasName2.1<- stack(MeasName2)
MeasName2.2 <- MeasName2.1$values
MeasName <- c(MeasName1.2,MeasName2.2)
code_meas <- c("OM1","PM1","PM2","PM3",
               "PM4","PM5","PM6","PM7","OPM1",
               "OPM2","OPM3_d","OPM3_h","OPM4","OPM5")
MeasName <- factor(MeasName, levels= code_meas)
name_meas <- c("Caries at Recall",
                "Caries Risk Assess",
                "Sealants 6-9 yrs",
                "Self-Mgmt Goal Rev",
                "Trt Plan Completion",
                "Risk-based Recall",
                "Sealants 10-14 yrs",
                "Topical Fluoride",
                "No Shows",
                "Gross Chrgs/Enctr",
                "Dentist Enctrs/Hr",
                "Hygienist Enctrs/Hr",
                "Direct Costs/Visit",
                "Recommendation")
#Now create the variable that substitutes the abbreviated names for the codes, to use in the output table
MeasNameChar <- plyr::mapvalues(MeasName,code_meas,name_meas)

#Now reorder the clinics to match the patient volume given by the CRA denominator, PM1_D largest to smallest
df_melt <- reorder_df(df_melt)
df_melt1 <- reorder_df(df_melt1)
