#Functions for analysis

#read all values as character using the readr package (called by googlesheets)
#http://stackoverflow.com/questions/31734469/r-readr-single-col-types 12 June 2016
read_table_asis = function(...) {
  n_cols = ncol(read_table(..., n_max = 1))
  read_table(..., col_types = paste(rep("c", n_cols), collapse = ""))
}

# function returns string w/o trailing whitespace
trim.trailing <- function (x) sub("\\s+$", "", x)

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

# function to reorder factor by mean:  reverse order function:  may be useful for ordering levels for facet plots
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
  #why is - (dash) being rendered as "."?
  names(df2) <- gsub("\\.","_",names(df2))
  df2[,4:ncol(df2)] <- sapply(df2[,4:ncol(df2)],clean_chars)
  #rename the goal columns
  RepMonth <- as.Date(as.yearmon("2016-02-01")+ 0:35/12)
  MeasMonth <- as.Date(as.yearmon("2016-01-01")+ 0:35/12)
  #define the names of the goal columns properly
  names(df2)[46:59] <- c(paste0("Goal_",names(df2)[seq(6,45,3)]))
  df2$RepMonth <- RepMonth
  df2$MeasMonth <- MeasMonth
  #put any per cents greater than 100 as NA
  df2[,c(seq(6,30,3),45)] <- sapply(df2[,c(seq(6,30,3),45)],check_percents)
  #remove all rows with all cells NA for the measure columns 4 to 45
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
  df3 <- df[,c("Measure","ShortName","value")]
  df3A <- droplevels(df3[df3$Measure=="PM1_D",])
  #now get a function of the PM1_D values by Clinic, here we use medians.
  pt_count <- unlist(by(df3A$value,df3A$ShortName,FUN=median_or_NA,simplify=FALSE))
  #now create a dataframe with the Clinic levels, the vector of medians, and an integer sequence
  df4 <- data.frame(levels(df3A$ShortName),pt_count,c(1:length(pt_count)))
  names(df4) <- c("ShortName","med_pt_count","orig_order")
  #reorder the dataframe by descending order of the medians
  df4 <- df4[order(-df4[,2]),]
  #now reorder the levels of the ShortName factor in the df2 dataframe, using the descending median order index from
  #dataframe df4
  df$ShortName <- factor(df$ShortName,levels(df$ShortName)[df4$orig_order])
  #     df$Measure_Name <- df$Measure
  #     levels(df$Measure_Name) <- levels(as.factor(df_mnames$Measure_Name))
  return(df)
}


#function to append a measure type column to the melted data frame
measure_type_maker <- function(df){
  meastype <- c(rep("M",nrow(df)))
  meastype[grep("OPM1", df$Measure)] <- "OPM"
  meastype[grep("OPM2", df$Measure)] <- "OPM"
  meastype[grep("OPM3_d", df$Measure)] <- "OPM"
  meastype[grep("OPM3_h", df$Measure)] <- "OPM"
  meastype[grep("OPM4", df$Measure)] <- "OPM"
  meastype[grep("OPM5", df$Measure)] <- "OPM"
  meastype[grep("_N",df$Measure)] <- "N"
  meastype[grep("_D",df$Measure)] <- "D"
  meastype[grep("Goal",df$Measure)] <- "Goal"
  meastype[grep("Goal_OPM",df$Measure)] <- "Goal_OPM"
  return(meastype)
}

#function to restructure the melted data set with goals as a separate column, given preliminary melted data set
#input df must have five columns:  ClinicName, MeasMonth, Measure (factor), value, MeasType, Goal, ShortName
goal_melt_df <- function(df1) {
  measures_groupA <- c("OM1","PM1","PM2","PM3","PM4","PM5","PM6", "PM7")
  goals_groupA <- paste0("Goal_",measures_groupA)
  measures_groupB <- c("OPM1","OPM2","OPM3_d","OPM3_h","OPM4","OPM5")
  goals_groupB <- paste0("Goal_",measures_groupB)
  dfA <- df1[df1$MeasType=='N',]
  dfA$Goal <- NA
  dfB <- df1[df1$MeasType=='D',]
  dfB$Goal <- NA
  dfC <- df1[df1$Measure %in% measures_groupA,]
  dfC$Goal <- df1$value[df1$Measure %in% goals_groupA] 
  dfD <- df1[df1$Measure %in% measures_groupB,]
  dfD$Goal <- df1$value[df1$Measure %in% goals_groupB] 
  df_out<- rbind.data.frame(dfA,dfB,dfC,dfD)
} 

# function to plot teams by measure....issue may be the number of team series?  allow value of nrows in facet plot to be variable
# to generalize.  Make it an input on the user interface for more general use.
#df is melted df, y-goal will need to be extracted from the file, p_nrow is the number of rows in the facet plot 
p_by_measure <- function(df,MName,p_nrow){
  
  dfB <- droplevels(df[df$MeasName==MName,])
  
  #Set up axis label and goals for Measure variables of type M or N and D REVISE THIS LOGIC, ugly.
  if(dfB$MeasType[1]=="M" | dfB$Measure[1]=="OPM1" | dfB$Measure[1] == "OPM5"){
    y_axis_lab <- "per cent"
    y_goal_label <- paste0("Goal_",dfB$Measure[1])
    #dfB$goal <- df$value[grep(y_goal_label,df$Measure)]
  } else if(dfB$MeasType[1]=="N" | dfB$MeasType[1]=="D"){
    y_axis_lab <- "Count"
  } else if(MeasName=="OPM2") {
    y_axis_lab <- "$/Hr"
    y_goal_label <- paste0("Goal_",dfB$Measure[1])
    #dfB$goal <- df$value[grep(y_goal_label,df$Measure)]
  } else if(MeasName=="OPM3_d" | MeasName=="OPM3_h") {
    y_axis_lab <- "Encounters/Hr"
    y_goal_label <- paste0("Goal_",dfB$Measure[1])
    #dfB$goal <- df$value[grep(y_goal_label,df$Measure)]
  } else if(MeasName=="OPM4") {
    y_axis_lab <- "$/Visit"
    y_goal_label <- paste0("Goal_",dfB$Measure[1])
    #dfB$goal <- df$value[grep(y_goal_label,df$Measure)]
  }
  
  #create medians
  med_B <- as.vector(tapply(dfB$value,dfB$ShortName,median,na.rm=TRUE))
  ShortName <- levels(dfB$ShortName)
  df.hlines <- data.frame(ShortName,med_B)
  
  
  #create facet plot
  p2 <- ggplot(dfB,aes(x=MeasMonth,y=value))+
    theme_bw()+
    facet_wrap(~ShortName,nrow=p_nrow)+
    geom_point(size=2.5)+
    geom_line()+
    ylab(y_axis_lab)+
    xlab("Date") +
    xlim(as.Date("2016-01-01"),as.Date("2017-7-01"))+
    theme(axis.text.x=element_text(angle=30,hjust=1,vjust=1))
  
  
  p21 <- p2 + geom_hline(aes(yintercept=med_B),data=df.hlines,lty=2)
  if(dfB$MeasType[1]=="M" | dfB$MeasType[1]=="OPM"){
    p31 <- p21 + geom_line(aes(x=MeasMonth,y=Goal), 
                           lty=1,colour="green")+
      ggtitle(paste0(MName," by Health Center, ordered by CRA patient volume, largest to smallest
                     Series median: dashed line; Goal: solid line."))
    
  } else {
    p31 <- p21 + ggtitle(paste0(MName," by Health Center ordered by CRA patient volume; Series median: dashed line."))
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
#     we can plot the OM1 and PM measures together as type "M" because they are all % variables
#     Need a separate function to handle the OPM measures, they have a variety of units and hence y axis labels.
p_by_team1 <- function(df,Clinic_Name,meas_type,x_axis_lab,nrow_plot=2){
  
  dfA <- droplevels(df[df$ShortName==Clinic_Name & 
                         df$MeasType==meas_type,])
  
  #Set up axis label and goals for Measure variables of type M or N and D REVISE THIS LOGIC, ugly.
  if(dfA$Measure[1]=="M"){
    y_axis_lab <- "per cent"
    y_goal_label <- paste0("Goal_",dfA$Measure[1])
  } else if(dfA$MeasType[1]=="N" | dfA$MeasType[1]=="D"){
    y_axis_lab <- "Count"
    y_goal_label <- NULL
  }
  
  #create medians
  med_A <- as.vector(tapply(dfA$value,dfA$Measure,median,na.rm=TRUE))
  Measure <- levels(dfA$Measure)
  df.hlines <- data.frame(Measure,med_A)
  
  
  #can vary the axis labels by measure type
  p1 <- ggplot(dfA,aes(x=MeasMonth,y=value)) +
    theme_bw() +
    facet_wrap(~Measure,nrow=nrow_plot)+
    geom_point(size=2.5)+
    geom_line() +
    ylab(y_axis_lab)+
    xlab("Date") +
    theme(axis.text.x=element_text(angle=30,hjust=1,vjust=1))
  
  if(!x_axis_lab){
    p1 <- p1+ theme(axis.title.x=element_blank())
  }
  
  p11 <- p1 + geom_hline(aes(yintercept=med_A),data=df.hlines,lty=2)
     if(!is.null(y_goal_label)){
        p12 <- p11 + geom_line(data=dfA,aes(x=MeasMonth,y=Goal), lty=1,colour="green")
     } else  {
        p12 <- p11
     }
  return(p12)
}    

#this function takes the individual measures and creates a graphical object that can then be gridded
#parameter trbl1 is a vector of plot.margin values (top, right, bottom, left) to pad space 
p_by_team2 <- function(df,Clinic_Name,meas_name,x_axis_lab, asp_ratio=.625, trbl1=c(0,.05,0,.05)) {
  dfA <- droplevels(df[df$ShortName==Clinic_Name & 
                         df$Measure==meas_name,])
  y_axis_lab <- "per cent"
  y_goal_label <- paste0("Goal_",meas_name)
  if(meas_name=="OPM2") {
    y_axis_lab <- "$/Hr"
  } else if(meas_name=="OPM3") {
    y_axis_lab <- "Encounters/Hr"
  } else if(meas_name=="OPM4") {
    y_axis_lab <- "$/Visit"
  }
  
  p1 <- ggplot(dfA,aes(x=MeasMonth,y=value)) +
    theme_bw() +
    geom_point(size=3.5)+
    geom_line() +
    ylab(y_axis_lab)+
    theme(axis.title.y=element_text(size=rel(1.25)))+
    theme(axis.text.y=element_text(size=rel(1.25)))+
    theme(axis.text.x=element_text(size=rel(1.25)))+
    theme(plot.margin=unit(trbl1,"cm"))+
    theme(aspect.ratio=asp_ratio)+
    xlab(" ")+
    #ggtitle(paste0(meas_name))
    ggtitle(dfA$MeasName[1])
 
  if(!x_axis_lab){
    p1 <- p1+ theme(axis.title.x=element_blank())
   }
  med_y <- median(dfA$value, na.rm=TRUE)
  p11 <- p1 + geom_hline(aes(yintercept=med_y), lty=2)
  if(!is.null(y_goal_label)){
    p12 <- p11 + geom_line(data=dfA,aes(x=MeasMonth,y=Goal), lty=1,colour="green")
  } else  {
    p12 <- p11
  }
  return(p12) 
}

report_check <- function(month_check){
  df00 <- df_master1[df_master1$MeasMonth==month_check,c(1,3,(seq(6,45,3)))]
  df0 <- df00[,c(3:16)]
  month_year <- as.character(format(as.Date(month_check),"%B %Y"))
  #get index of rows with all na
  df10 <- df00[rowSums(is.na(df00[,c(3:16)])) !=ncol(df0),]
 #data_id1 <- rep(paste("at least one value reported for", month_year),nrow(df10))
  data_id1 <- paste("at least one value reported for ", month_year)
  df11 <- df00[rowSums(is.na(df00[,c(3:16)])) ==ncol(df0),]
# data_id2 <- rep(paste("no measures reported for",month_year),nrow(df11))
  data_id2 <- paste("no measures reported for",month_year)
  out_list<- list(df10,data_id1,df11,data_id2)
}

#function to find the nearest month for overplot limit

date_limit_overplot <- function(){
  
  today <- Sys.Date()
  day1 <- as.numeric(format(today,"%d"))
  #monthuse <- ifelse(day1 <=15,today - 65,today-35)
  if(day1 <= 15) {
    monthuse <- today - 65
  } else {
    monthuse <- today - 35
  }
  return(monthuse)
}
  


# function to create median overlay plot;
########################################
# Parameter definitions/explanations--what will be passed to this function
# df_data            default data frame to pass from server.R  values$df_data1, the updated version of df_melt1
# measure_use        from drop down box input#choose_Meas
# goal_use           NA except for Caries Risk Assessment and Sealants (6-9 and 10-14)
# date_end           default:   provided by function date_limit_overplot
median_overlay_plot <- function(df_data,
                                measure_use,
                                goal_use, 
                                date_end,size_median_dot = 4, 
                                clinic_dot_size=3,
                                clinic_dot_colour= "gray75",
                                jitter_width=4,
                                jitter_height=1){
  
  
  
  if(measure_use %in% name_meas_pct) {
    y_axis_lab <- "Per Cent"
  } else if(measure_use %in% name_meas_enctrs) {
    y_axis_lab <- "Encounters/Hr"
  } else if(measure_use=="Direct Costs/Visit") {
    y_axis_lab <- "$/Visit"
  } else if(measure_use=="Gross Chrgs/Enctr") {
    y_axis_lab <- "$/Encounter"
  } else y_axis_lab <- "Count"
  df1 <- droplevels(df_data[df_data$MeasName==measure_use & df_data$MeasMonth <= date_end ,])
  MeasMonth2 <- unique(df1$MeasMonth)
  monthly_medians <- as.vector(by(df1$value,df1$MeasMonth,median,na.rm=TRUE))
  df_medians <- cbind.data.frame(MeasMonth2,monthly_medians)
  if(is.na(goal_use)) {
    title_string <- paste0(df1$MeasName[1],"\nEach gray dot is one health center's monthly data; black dots are monthly medians.")
    p0 <- ggplot(data=df1, aes(x=MeasMonth,y=value))+
      theme_bw()+
      xlab("Month")+
      ylab(y_axis_lab)+
      theme(axis.text=element_text(size=rel(1.5)))+
      theme(axis.title=element_text(size=rel(1.75)))+
      geom_jitter(size=clinic_dot_size, 
                  colour=clinic_dot_colour,
                  width=jitter_width,
                  height=jitter_height)+
      ggtitle(title_string)+
      theme(plot.title=element_text(size=rel(2.0)))
  } else {
    title_string <- paste0(df1$MeasName[1],"; Dashed line is goal (",goal_use,"%)\nEach gray dot is one health center's monthly data; black dots are monthly medians.")
    
    p0 <- ggplot(data=df1, aes(x=MeasMonth,y=value))+
      theme_bw()+
      xlab("Month")+
      ylab(y_axis_lab)+
      theme(axis.text=element_text(size=rel(1.5)))+
      theme(axis.title=element_text(size=rel(1.75)))+
      geom_jitter(size=clinic_dot_size, 
                  colour=clinic_dot_colour,
                  width=jitter_width,
                  height=jitter_height)+
      geom_hline(yintercept = goal_use, linetype="dashed")+
      ggtitle(title_string)+
      theme(plot.title=element_text(size=rel(2.0)))
  }
  
  p01 <- p0 + geom_point(data=df_medians,aes(MeasMonth2,monthly_medians),size=size_median_dot)+
    geom_line(data=df_medians,aes(MeasMonth2,monthly_medians))
  return(p01)
}

#median overlay plot function with counts of organizations overlaid
median_overlay_plot1 <- function(df_data,
                                 measure_use,
                                 goal_use, 
                                 date_end,size_median_dot = 4, 
                                 clinic_dot_size=3,
                                 clinic_dot_colour= "gray75",
                                 jitter_width=4,
                                 jitter_height=1){
  
  
  
  if(measure_use %in% name_meas_pct) {
    y_axis_lab <- "Per Cent"
  } else if(measure_use %in% name_meas_enctrs) {
    y_axis_lab <- "Encounters/Hr"
  } else if(measure_use=="Direct Costs/Visit") {
    y_axis_lab <- "$/Visit"
  } else if(measure_use=="Gross Chrgs/Enctr") {
    y_axis_lab <- "$/Encounter"
  } else y_axis_lab <- "Count"
  #restrict the data frame to the measure, date range and non missing values 
  df1 <- droplevels(df_data[df_data$MeasName==measure_use & 
                              df_data$MeasMonth <= date_end &
                              !is.na(df_melt1$value),])
  MeasMonth2 <- sort(unique(df1$MeasMonth))
  monthly_medians <- as.vector(by(df1$value,df1$MeasMonth,median,na.rm=TRUE))
  #call dplyr::count to get the number of organizations reporting each month
  count_orgs <- count(df1,MeasMonth)
  df_medians <- cbind.data.frame(MeasMonth2,monthly_medians,count_orgs$n)
  names(df_medians)[3] <- "count"
  if(is.na(goal_use)) {
    title_string <- df1$MeasName[1]
    subtitle_string <- "Each gray dot is one health center's monthly data; \nBlack dots are monthly medians with monthly counts below."
    p0 <- ggplot(data=df1, aes(x=MeasMonth,y=value))+
      theme_bw()+
      xlab("Month")+
      ylab(y_axis_lab)+
      theme(axis.text=element_text(size=rel(1.5)))+
      theme(axis.title=element_text(size=rel(1.75)))+
      geom_jitter(size=clinic_dot_size, 
                  colour=clinic_dot_colour,
                  width=jitter_width,
                  height=jitter_height)+
      ggtitle(title_string, subtitle=subtitle_string)+
      theme(plot.title=element_text(size=rel(2.0)))+
      theme(plot.subtitle=element_text(size=rel(1.5)))
  } else {
    title_string <- paste0(df1$MeasName[1],"; Dashed line is goal (",goal_use,"%)")
    subtitle_string <- "Each gray dot is one health center's monthly data; \nBlack dots are monthly medians with monthly counts below."
    
    p0 <- ggplot(data=df1, aes(x=MeasMonth,y=value))+
      theme_bw()+
      xlab("Month")+
      ylab(y_axis_lab)+
      theme(axis.text=element_text(size=rel(1.5)))+
      theme(axis.title=element_text(size=rel(1.75)))+
      geom_jitter(size=clinic_dot_size, 
                  colour=clinic_dot_colour,
                  width=jitter_width,
                  height=jitter_height)+
      geom_hline(yintercept = goal_use, linetype="dashed")+
      ggtitle(title_string,subtitle=subtitle_string)+
      theme(plot.title=element_text(size=rel(2.0)))+
      theme(plot.subtitle=element_text(size=rel(1.5)))
  }
  
  p01 <- p0 + geom_point(data=df_medians,aes(MeasMonth2,monthly_medians),size=size_median_dot)+
    geom_line(data=df_medians,aes(MeasMonth2,monthly_medians)) 
  
  if(measure_use %in% name_meas_pct) {
    p02 <- p01 + geom_text(data=df_medians,aes(MeasMonth2,monthly_medians- 5,
                                               label=as.character(count)))
  } else {
    
    p02 <- p01 + geom_text(data=df_medians,aes(MeasMonth2,0.9*monthly_medians,
                                               label=as.character(count)))
  }
  
  return(p02)
}
