#Functions for analysis

#read all values as character using the readr package (called by googlesheets)
#http://stackoverflow.com/questions/31734469/r-readr-single-col-types 12 June 2016
read_table_asis = function(...) {
  n_cols = ncol(read_table(..., n_max = 1))
  read_table(..., col_types = paste(rep("c", n_cols), collapse = ""))
}


#substitute Excel expression #N/A with actual NA; substitute NA string with actual NA
#Google sheet written from a dataframe with NA entries will show NA string in cell.
make_NA <- function(x) {
  x <- gsub("#N/A",NA,x)
  x <- gsub("NA", NA, x)
}

#remove $ and extraneous characters from a vector that is supposed to be numeric, possibly including decimal
clean_chars <- function(col1){
  col_out<- as.numeric(gsub('[^0-9.]','',col1))
}

#function to restrict nominal per cents to numbers between 0 and 100, else replacing by NA
restrict_percents <- function(x) {
  if(!is.na(x) && is.numeric(x) && x >=0 && x<=100) {
    x <- x
  } else x <- NA
  return(x)
}

#function to check columns that are nominally per cents between 0 and 100 and replace by NA
check_percents <- function(col1) {
  col_out <- sapply(col1,restrict_percents)
}

# functionto reorder factor by mean:  reverse order function:  may be useful for ordering levels for facet plots
reverse_order <- function(x){
  rev_order <- -1*mean(x)
}

# function to return a dataframe where rows with all NA in a set of columns are eliminated, the column set defined by cindex 
remove_NA_rows <- function(df,cindex) {
  dfa <- df[,cindex]
  names_use <- names(dfa)
  dfb <- as.data.frame(t(dfa))
  rm_idx <- sapply(dfb,function(x) all(is.na(x)))
  dfc <- dfb[,!rm_idx]
  dfd <- as.data.frame(t(dfc))
  #names(df_out) <- names_use
  row_ID <- as.integer(gsub("V","",row.names(dfd)))
  df_out <- df[row_ID,]
}

#function to clean up the dataframe loaded from the spreadsheet  THIS CAN BE GENERALIZED to handle arbitrary column assignment
# at very least to pass index values of columns that are per cents for checking.
# on 30 June modified to remove rows that have all NA in the measure columns
# assumes that the input file will always have 36 data rows and four headers.
clean_up_df1 <- function(df) {
  df2 <- as.data.frame(lapply(df,make_NA), stringsAsFactors = FALSE)
  names(df2) <- gsub("-","_",names(df2))
  df2[,4:ncol(df2)] <- sapply(df2[,4:ncol(df2)],clean_chars)
  #rename the goal columns
  RepMonth <- as.Date(as.yearmon("2016-02-01")+ 0:35/12)
  MeasMonth <- as.Date(as.yearmon("2016-01-01")+ 0:35/12)
  #define the names of the goal columns properly
  names(df2)[43:55] <- c(paste0("Goal_",names(df2)[seq(6,42,3)]))
  df2$RepMonth <- RepMonth
  df2$MeasMonth <- MeasMonth
  #put any per cents greater than 100 as NA
  df2[,c(seq(6,30,3),42)] <- sapply(df2[,c(seq(6,30,3),42)],check_percents)
  #remove all rows with all cells NA for the measure columns 4 to 42
  #commented out 25 July 2016 to match use of blocks of 36 rows per clinic
  #df2 <- remove_NA_rows(df2,c(4:42))
  return(df2)
}

#function to handle missing patient counts in ordering the clinic factor by size of median patient counts.

median_or_NA <- function(x) {
  if(all(is.na(x))){
    x1 <- NA
  } else x1 <- median(x,na.rm=TRUE)
  return(x1)
}

#function to reorder ClinicName factor using the median patient count in month (0-20 yrs) for plotting facets in population order
reorder_df <- function(df) {
  
  #order the levels of clinics by volume:  There may be a simpler way to do this than by brute force
  df3 <- df[,c("Measure","ClinicName","value")]
  df3A <- droplevels(df3[df3$Measure=="PM1_D",])
  #now get a function of the PM1_D values by Clinic, here we use medians.
  pt_count <- unlist(by(df3A$value,df3A$ClinicName,FUN=median_or_NA,simplify=FALSE))
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


#function to append a measure type column to the melted data frame
measure_type_maker <- function(df){
  meastype <- c(rep("M",nrow(df)))
  meastype[grep("OPM1", df$Measure)] <- "OPM"
  meastype[grep("OPM2", df$Measure)] <- "OPM"
  meastype[grep("OPM3", df$Measure)] <- "OPM"
  meastype[grep("OPM4", df$Measure)] <- "OPM"
  meastype[grep("_N",df$Measure)] <- "N"
  meastype[grep("_D",df$Measure)] <- "D"
  meastype[grep("Goal",df$Measure)] <- "Goal"
  meastype[grep("Goal_OPM",df$Measure)] <- "Goal_OPM"
  return(meastype)
}



# function to plot teams by measure....issue may be the number of team series?  allow value of nrows in facet plot to be variable
# to generalize.  Make it an input on the user interface for more general use.
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
    xlim(as.Date("2016-01-01"),as.Date("2017-7-01"))+
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





#function to create a data table with ratio values greater than 1 or duplicated records for output and download
df_prblm_records <- function(df) {
  #records with ratio values greater than 1
  value_idx <- as.integer(row.names(df[df$Measure=="M" & df$value > 100,]))
  df1x <- df[value_idx,]
  df1x$issue <- rep("ratio error",times=nrow(df1x))
  #records that are truly duplicated
  #http://stackoverflow.com/questions/11369961/subset-with-unique-cases-based-on-multiple-columns 
  df2x <- df[duplicated(df[c(4,6,7,8)]) | duplicated(df[c(4,6,7,8)], fromLast=TRUE),]
  df2x$issue <- rep("site_time_measure duplication",times=nrow(df2x))
  df_all <- rbind.data.frame(df1x,df2x)
  return(df_all)
}



#function to plot measures for a given team, which will be assembled into a display with grid.arrange
#     
#     take main data frame and subset by type of measure (D, M, N); for a stack of plots, the 
#     x_axis_lab is a logical variable where FALSE indicates suppression of the x axis label for each plan
p_by_team <- function(df,Clinic_Name,meas_type,x_axis_lab){
  
  dfA <- droplevels(df[df$ClinicName==Clinic_Name & 
                         df$MeasType==meas_type,])
  
  #Set up axis label and goals for Measure variables of type M or N and D REVISE THIS LOGIC, ugly.
  if(dfA$MeasType[1]=="M" | dfA$Measure[1]=="OPM1"){
    y_axis_lab <- "per cent"
    y_goal_label <- paste0("Goal_",dfA$Measure[1])
    dfA$goal <- df$value[grep(y_goal_label,df$Measure)]
  } else if(dfA$MeasType[1]=="N" | dfA$MeasType[1]=="D"){
    y_axis_lab <- "Count"
  } else if(MeasName=="OPM2") {
    y_axis_lab <- "$/Hr"
    y_goal_label <- paste0("Goal_",dfA$Measure[1])
    dfA$goal <- df$value[grep(y_goal_label,df$Measure)]
  } else if(MeasName=="OPM3") {
    y_axis_lab <- "Encounters/Hr"
    y_goal_label <- paste0("Goal_",dfA$Measure[1])
    dfA$goal <- df$value[grep(y_goal_label,df$Measure)]
  } else if(MeasName=="OPM4") {
    y_axis_lab <- "$/Visit"
    y_goal_label <- paste0("Goal_",dfA$Measure[1])
    dfA$goal <- df$value[grep(y_goal_label,df$Measure)]
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

