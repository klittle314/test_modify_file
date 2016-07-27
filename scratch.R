<<<<<<< HEAD
clinic_name <- "C1"
df_clinic <- read.xlsx("C1data.xlsx",sheet=4, startRow=4,detectDates=TRUE)

meastype <- c(rep("M",nrow(df)))
meastype[grep("OPM1", df$Measure)] <- "OPM"
meastype[grep("OPM2", df$Measure)] <- "OPM"
meastype[grep("OPM3", df$Measure)] <- "OPM"
meastype[grep("OPM4", df$Measure)] <- "OPM"
meastype[grep("_N",df$Measure)] <- "N"
meastype[grep("_D",df$Measure)] <- "D"
meastype[grep("Goal",df$Measure)] <- "Goal"
meastype[grep("Goal_OPM",df$Measure)] <- "Goal_OPM"
df$MeasType <- meastype

df_melt$ClinicName <- as.factor(df_melt$ClinicName)
df <- df_melt
df$ClinicName <- as.factor(df$ClinicName)
MName <- "OM1"
levels(df$Measure)
p_nrow <- 1

#df is melted df, y-goal will need to be extracted from the file, p_nrow is the number of rows in the facet plot 
p_by_measure <- function(df,MName,p_nrow){
  
  dfB <- droplevels(df[df$Measure==MName,])
  
#Set up axis label and goals for Measure variables of type M or N and D REVISE THIS LOGIC, ugly.
  if(dfB$MeasType[1]=="M" | dfB$Measure[1]=="OPM1"){
      y_axis_lab <- "per cent"
      y_goal_label <- paste0("Goal_",dfB$Measure[1])
      dfB$goal <- df$value[grep(y_goal_label,df$Measure)]
  } else if(dfB$MeasType[1]=="N" | dfB$MeasType[1]=="D"){
    y_axis_lab <- "Count"
  } else if(MeasName=="OPM2") {
    y_axis_lab <- "$/Hr"
    y_goal_label <- paste0("Goal_",dfB$Measure[1])
    dfB$goal <- df$value[grep(y_goal_label,df$Measure)]
    } else if(MeasName=="OPM3") {
    y_axis_lab <- "Encounters/Hr"
    y_goal_label <- paste0("Goal_",dfB$Measure[1])
    dfB$goal <- df$value[grep(y_goal_label,df$Measure)]
    } else if(MeasName=="OPM4") {
    y_axis_lab <- "$/Visit"
    y_goal_label <- paste0("Goal_",dfB$Measure[1])
    dfB$goal <- df$value[grep(y_goal_label,df$Measure)]
  }
  
  #create medians
  med_B <- as.vector(tapply(dfB$value,dfB$ClinicName,median,na.rm=TRUE))
  ClinicName <- levels(dfB$ClinicName)
  df.hlines <- data.frame(ClinicName,med_B)
  
  
  #create facet plot
  p2 <- ggplot(dfB,aes(x=MeasMonth,y=value))+
    theme_bw()+
    facet_wrap(~ClinicName,nrow=p_nrow)+
    geom_point(size=2.5)+
    geom_line()+
    ylab(y_axis_lab)+
    xlab("Date") +
    xlim(as.Date("2016-01-01"), as.Date("2017-7-01"))+
    theme(axis.text.x=element_text(angle=30,hjust=1,vjust=1))
  
  
  p21 <- p2 + geom_hline(aes(yintercept=med_B),data=df.hlines,lty=2)
  if(dfB$MeasType[1]=="M"){
    p31 <- p21 + geom_line(aes(x=MeasMonth,y=goal), 
                            lty=1,colour="green")+
      ggtitle(paste0(MName," by Clinic
                     Series median: dashed line; Goal: solid line."))
    
  } else {
    p31 <- p21 + ggtitle(paste0(MName," by Clinic 
                                Series median dashed line."))
  } 
  return(p31)
  
  
}



reorder_df <- function(df) {
  
  #order the levels of clinics by volume:  There may be a simpler way to do this than by brute force
  df3 <- df[,c("Measure","ClinicName","value")]
  df3A <- droplevels(df3[df3$Measure=="PM1_D",])
  #now get a function of the PM1_D values by Clinic, here we use medians.
  pt_count <- unlist(by(df3A$value,df3A$ClinicName,FUN=median, na.rm=TRUE,simplify=FALSE))
  #now create a dataframe with the Clinic levels, the vector of medians, and an integer sequence
  df4 <- data.frame(levels(df3A$ClinicName),pt_count,c(1:length(pt_count)))
  names(df4) <- c("ClinicName","med_pt_count","orig_order")
  #reorder the dataframe by descending order of the medians
  df4 <- df4[order(-df4[,2]),]
  #now reorder the levels of the ClinicName factor in the df2 dataframe, using the descending median order index from
  #dataframe df4
  df$ClinicName <- factor(df$ClinicName,levels(df$ClinicName)[df4$orig_order])
  #     df$Measure_Name <- df$Measure
  #     levels(df$Measure_Name) <- levels(as.factor(df_mnames$Measure_Name))
  return(df)
}

test33 <- reorder_df(df_melt)
test33$MeasType <- measure_type_maker(test33)
unique(test33$MeasType)

test4444 <- df$value[df$Measure %in% test55]

df <- test33

test44 <- p_by_team(df=test33,Clinic_Name="Test Organization",meas_type="M", x_axis_lab=TRUE)   
#WORKING FOR M MEASURES.
test55 <- sapply(levels(dfA$Measure),function(x)paste0("Goal_",x))
df0 <- df[df$ClinicName==Clinic_Name,]

test111 <- p_by_team1(df=test33,Clinic_Name="Test Organization",meas_type="N", x_axis_lab=TRUE)

 #function only set up to work with Measures types M, N, D.   will not work with OPM type
p_by_team1 <- function(df,Clinic_Name,meas_type,x_axis_lab){
  df0 <- droplevels(df[df$ClinicName==Clinic_Name,])
  dfA <- droplevels(df[df$ClinicName==Clinic_Name & 
                         df$MeasType==meas_type,])
  goal_labs <- sapply(levels(dfA$Measure),function(x)paste0("Goal_",x))
  #Set up axis label and goals for Measure variables of type M or N and D
  if(dfA$MeasType[1]=="M"){
    y_axis_lab <- "per cent"
    y_goal_label <- paste0("Goal_",dfA$Measure[1])
    dfA$goal <- df0$value[df0$Measure %in% goal_labs]
  } else if(dfA$MeasType[1]=="N" | dfA$MeasType[1]=="D"){
    y_axis_lab <- "Count"
    dfA$goal <- NULL
  } 
  
  #create medians
  med_A <- as.vector(tapply(dfA$value,dfA$Measure,median,na.rm=TRUE))
  Measure <- levels(dfA$Measure)
  df.hlines <- data.frame(Measure,med_A)
  
  
  #can vary the axis labels by measure type
  p1 <- ggplot(dfA,aes(x=MeasMonth,y=value)) +
    theme_bw() +
    facet_wrap(~Measure,nrow=2)+
    geom_point(size=2.5)+
    geom_line() +
    ylab(y_axis_lab)+
    xlab("Date") +
    theme(axis.text.x=element_text(angle=30,hjust=1,vjust=1))
  
  if(!x_axis_lab){
    p1 <- p1+ theme(axis.title.x=element_blank())
  }
  
  p11 <- p1 + geom_hline(aes(yintercept=med_A),data=df.hlines,lty=2)
   if(!is.null(dfA$goal)){
    p12 <- p11 + geom_line(aes(x=MeasMonth,y=goal), lty=1,colour="green")
   } else p12 <- p11
  return(p12)
}    


#to plot the operational measures   Need to check if there are valid OPMs
p_by_team2 <- function(df,Clinic_Name,meas_type="OPM",x_axis_lab){
  df0 <- droplevels(df[df$ClinicName==Clinic_Name,])
  dfA <- droplevels(df[df$ClinicName==Clinic_Name & 
                         df$MeasType==meas_type,])
  goal_labs <- sapply(levels(dfA$Measure),function(x)paste0("Goal_",x))
  dfA$goal <- df0$value[df0$Measure %in% goal_labs]
  #Set up axis label and goals for Measure variables of type M or N and D REVISE THIS LOGIC, ugly.
  if(MeasName=="OPM1") {
    y_axis_lab == "per cent"
  }
  else if(MeasName=="OPM2") {
    y_axis_lab <- "$/Hr"
    
  } else if(MeasName=="OPM3") {
    y_axis_lab <- "Encounters/Hr"
    y_goal_label <- paste0("Goal_",dfA$Measure[1])
    dfA$goal <- df$value[grep(y_goal_label,df$Measure)]
  } else if(MeasName=="OPM4") {
    y_axis_lab <- "$/Visit"
    y_goal_label <- paste0("Goal_",dfA$Measure[1])
    dfA$goal <- df$value[grep(y_goal_label,df$Measure)]
  
  if(dfA$MeasType[1]=="M"){
    y_axis_lab <- "per cent"
      } else if(dfA$MeasType[1]=="N" | dfA$MeasType[1]=="D"){
    y_axis_lab <- "Count"
  } 
  
  #create medians
  med_A <- as.vector(tapply(dfA$value,dfA$Measure,median,na.rm=TRUE))
  Measure <- levels(dfA$Measure)
  df.hlines <- data.frame(Measure,med_A)
  
  
  #can vary the axis labels by measure type
  p1 <- ggplot(dfA,aes(x=MeasMonth,y=value)) +
    theme_bw() +
    facet_wrap(~Measure,nrow=2)+
    geom_point(size=2.5)+
    geom_line() +
    ylab(y_axis_lab)+
    xlab("Date") +
    theme(axis.text.x=element_text(angle=30,hjust=1,vjust=1))
  
  if(!x_axis_lab){
    p1 <- p1+ theme(axis.title.x=element_blank())
  }
  
  p11 <- p1 + geom_hline(aes(yintercept=med_A),data=df.hlines,lty=2)
  #   if(!is.na(y_goal)){
  p12 <- p11 + geom_line(aes(x=MeasMonth,y=goal), lty=1,colour="green")
  
  return(p12)
}    

=======
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
>>>>>>> kl_test
