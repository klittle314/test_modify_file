#read in the spreadsheet
library(googlesheets) 
library(openxlsx)
source("helper.R")

#load the googlesheet
#test sheet to avoid contaminating the master data table: TEST2_NNOHA Dental Dashboard Tracking Tool 30 Aug 2016
#gskey2 <- c("1uw8PgwqG6fx44RJKnInGGhCDXTswWxz0sDbIv467bA4")
#test sheet 21 May 2017, copy of the production sheet as of 12:52 pm CT on 21 May
#Test 21 May NNOHA Dental Dashboard Tracking Tool 30 Aug 2016
gskey2 <- c("10VK0Qz4Ottdg9O2rZc_wucRx3DultK3OfRk2r3NbGnk")

#working sheet NNOHA Dental Dashboard Tracking Tool 30 Aug 2016:  take the coded from the URL of the sheet
#gskey2 <- c("1_iIVDAS1gzqr6KYcYZiNicE3o7anPJNXQJTWcrLIfW4")
gsobj <- gs_key(x=gskey2)

#retrieve google sheet data, force as data.frame else the object is a tbl that seems to confuse other
#functions expecting a dataframe

df_master1 <- as.data.frame(gs_read(ss=gsobj,ws="Summary_Data"))
#assumes the Summary Data has already been cleaned--starts in a clean state.
df_master1 <- clean_up_df1(df_master1)

#load the Excel sheet with problematic goals

path1 <- "C:/Users/Kevin/Documents/NNOHA_2016_Dental_Collab/test_modify_file/issues May 2017"
filename <- "/NEW Kokua Kalihi Valley Comprehensive Family Services with data.xlsx"
datapath1 <- paste0(path1,filename)
df_clinicA <- read.xlsx(datapath1, sheet="Data Table", startRow=4,cols=c(1:59),detectDates=TRUE)
df_clinicA <- clean_up_df1(df_clinicA)
names(df_clinicA) <- names(df_master1)

#write the 2016 records, mostly to take care of the goals.

clinic_name <- df_clinicA$ClinicName[1]
#delete clinic records from the master file 
# this next assignment appears a dead end 17 Mar 2017
#df_all_but_clinic <- df_master1[df_master1$ClinicName!=clinic_name,]

#need to add 1 to index because the first row of the googlesheet is a header row, not data
idx_start_old <- match(clinic_name,df_master1$ClinicName)

# This will always be 36, and is used for checking correct number of rows
nrec_clinic_old <- length(df_master1$ClinicName[df_master1$ClinicName==clinic_name])

# we use the 2016 records
nrec_to_use <- 12

idx_end_old <- idx_start_old + nrec_to_use - 1  #idx_start_old + nrec_clinic_old-1

#get the index of records for clinic in df_clinic
nrec_clinic_new <- nrow(df_clinicA)
#diagnostic print
cat(file=stderr(),"nrec clinic old",nrec_clinic_old,"nrec clinic new",nrec_clinic_new,"\n")
cat(file=stderr(), "check identical function",isTRUE(all.equal(nrec_clinic_old,nrec_clinic_new)),"\n")

#reinitialize gs object [do I need this?]

gsobj <- gs_key(x=gskey2)
#define the cell in the first row of the clinic's records in df_master1 and the google sheet
anchor1 <- paste0("A",as.character(idx_start_old+1))

df_clinicA1 <-df_clinicA[1:nrec_to_use,]

if(isTRUE(base::all.equal(nrec_clinic_old,nrec_clinic_new))) {
  #since the new record set has same number of rows as old record set, simply replace old with new df
  gs_edit_cells(ss=gsobj,ws="Summary_Data",input=df_clinicA1,col_names=FALSE,anchor=anchor1)
  df_master1[idx_start_old:idx_end_old,] <- df_clinicA1
} else cat(file=stderr(),"records of uploaded file do not match master file")

df_updated<- df_master1

#check 
df_master2 <- as.data.frame(gs_read(ss=gsobj,ws="Summary_Data"))
#assumes the Summary Data has already been cleaned--starts in a clean state.
df_master2 <- clean_up_df1(df_master1)

check_success <- identical(df_master2,df_updated)

check_success
#########################################################################################
#now use the 2018 records
nrec_to_use <- 12
# idx_start_old <- match(clinic_name,df_master1$ClinicName)
# clinic_name <- df_clinicA$ClinicName[1]
idx_end_old <- idx_start_old + 24 + nrec_to_use - 1  #idx_start_old + nrec_clinic_old-1

#get the index of records for clinic in df_clinic
nrec_clinic_new <- nrow(df_clinicA)
#diagnostic print
cat(file=stderr(),"nrec clinic old",nrec_clinic_old,"nrec clinic new",nrec_clinic_new,"\n")
cat(file=stderr(), "check identical function",isTRUE(all.equal(nrec_clinic_old,nrec_clinic_new)),"\n")

#reinitialize gs object [do I need this?]

gsobj <- gs_key(x=gskey2)
#define the cell in the first row of the clinic's records in df_master1 and the google sheet
anchor1 <- paste0("A",as.character(idx_start_old+24+1))

df_clinicA2 <-df_clinicA[25:36,]

if(isTRUE(base::all.equal(nrec_clinic_old,nrec_clinic_new))) {
  #since the new record set has same number of rows as old record set, simply replace old with new df
  gs_edit_cells(ss=gsobj,ws="Summary_Data",input=df_clinicA2,col_names=FALSE,anchor=anchor1)
  df_master1[(idx_start_old+24):idx_end_old,] <- df_clinicA2
} else cat(file=stderr(),"records of uploaded file do not match master file")

df_updated<- df_master1

#check 
df_master2 <- as.data.frame(gs_read(ss=gsobj,ws="Summary_Data"))
#assumes the Summary Data has already been cleaned--starts in a clean state.
df_master2 <- clean_up_df1(df_master1)

check_success <- identical(df_master2,df_updated)

check_success
