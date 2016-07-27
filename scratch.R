df_clinic <- read.xlsx("C2data.xlsx", sheet=4, startRow=4,detectDates=TRUE)

#clean clinic data to our standards
df_clinic <- clean_up_df1(df_clinic)
names(df_clinic) <- names(df_master1)

#clinic_name <- input$choose_clinic
clinic_name <- "C2"
#two cases:  the googlesheet already has data from the clinic or we just add the clinic data to end of sheet
#to speed processing, we need to set up the master data sheet in Google with dummy values for each clinic
#and we need to have the format locked down of the spreadsheets so we are in the simplest case of just
#replacing a rectangle of 35 x 55 cells on update.
# Case 1


#get the index of records (start position and count) for clinic in df_master1 to use to revise the google sheet

if(clinic_name %in% unique(df_master1$ClinicName)) {
  #delete clinic records from the master file
  #df_all_but_clinic <- df_master1[df_master1$ClinicName!=clinic_name,]
  #need to add 1 to index because the first row of the googlesheet is a header row, not data
  idx_start_old <- match(clinic_name,df_master1$ClinicName)
  nrec_clinic_old <- length(df_master1$ClinicName[df_master1$ClinicName==clinic_name])
  idx_end_old <- idx_start_old + nrec_clinic_old-1L  #
  
  #get the index of records for clinic in df_clinic
  nrec_clinic_new <- nrow(df_clinic)
  #diagnostic print
  cat(file=stderr(),"nrec clinic old",nrec_clinic_old,"nrec clinic new",nrec_clinic_new,"\n")
  cat(file=stderr(), "check identical function",isTRUE(all.equal(nrec_clinic_old,nrec_clinic_new)),"\n")
  
  #reinitialize gs object [do I need this?]
  
  gsobj <- gs_key(x=gskey2)
  #define the cell in the first row of the clinic's records in df_master1 and the google sheet
  anchor1 <- paste0("A",as.character(idx_start_old+1L))
  
  if(isTRUE(base::all.equal(nrec_clinic_old,nrec_clinic_new))) {
    #since the new record set has same number of rows as old record set, simply replace old with new df
    gs_edit_cells(ss=gsobj,ws="Summary_Data",input=df_clinic,col_names=FALSE,anchor=anchor1)
    df_master1[idx_start_old:idx_end_old,] <- df_clinic
  } else cat(file=stderr(),"records of uploaded file do not match master file")
  #now create the revised dataframe to use in plotting and summaries by the Shiny app         
  df_new <- df_master1
  
  
  #Case 2:  the clinic has no records in the google sheet and we append at bottom of table
# } else {
#   gsobj <- gs_key(x=gskey2)
#   idx_start <- nrow(df_master1)+2
#   anchor1 <- paste0("A",as.character(idx_start))
#   gs_edit_cells(ss=gsobj,ws="Summary_Data",input=df_clinic,col_names=FALSE,anchor=anchor1)
#   #now create the revised dataframe to use in plotting and summaries 
#   df_new <- rbind(df_master1,df_clinic)
# }

#now create a melted, reduced version of the data frame for manipulation (remove RepMonth variable)
df_new1 <- melt(df_new[,-2],id.vars=c("ClinicName","MeasMonth"),variable.name="Measure")
df_new1$ClinicName <- as.factor(df_new1$ClinicName)
#append measure type column
df_new1$MeasType <- measure_type_maker((df_new1))

#order the clinics by PM1_D values, largest to smallest
df_new1 <- reorder_df(df_new1)
values$df_data <- df_new1   



#function to handle missing patient counts in ordering the clinic factor by size of median patient counts.

median_or_NA <- function(x) {
  if(all(is.na(x))){
    x1 <- NA
  } else x1 <- median(x,na.rm=TRUE)
  return(x1)
}

df <- df_new1
pt_count <- unlist(by(df3A$value,df3A$ClinicName,FUN=median_or_NA,simplify=FALSE))

#26 July 2016 revise this function to put the clinic team just uploaded as the first entry in drop down
team_choice <- reactive({
  data <- values$df_data
  if(!is.null(data)) {
    levs <- sort(as.character(levels(as.factor(data$ClinicName))))
    levs1 <- levs[levs != values$clinic_name]
    levs <- c(values$clinic_name,levs1)
    return(levs)
  }
})

df_melt
clinic_name <- "C2"
test1 <- levels(df_melt$ClinicName)
test2 <- as.factor(df_melt$ClinicName)
test3 <- test1[test1 != clinic_name]
test4 <- c(clinic_name,test3)
