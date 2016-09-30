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
<<<<<<< HEAD


debugonce(p_by_team)
p0 <- p_by_team(df=df_melt,Clinic_Name="Zufall",meas_type="OPM",x_axis_lab=TRUE)
p0

if(!is.null(y_goal_label)){
  p12 <- p11 + geom_line(data=dfA,aes(x=MeasMonth,y=Goal), lty=1,colour="green")
} else  {
  p12 <- p11
}


# returns string w/o trailing whitespace
trim.trailing <- function (x) sub("\\s+$", "", x)
df

#function to restructure the melted data set with goals as a separate column, given preliminary melted data set
goal_melt_df <- function(df1) {
  measures_groupA <- c("OM1","PM1","PM2","PM3","PM4","PM5","PM6", "PM7", "PM8")
  goals_groupA <- paste0("Goal_",measures_groupA)
  measures_groupB <- c("OPM1","OPM2","OPM3","OPM4")
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

debugonce(p_by_measure)
p

debugonce(reorder_df)
test11 <- reorder_df(df_melt)

clinic_name <- levels(df_melt$ClinicName)[20]

debugonce(p_by_team2)
p00 <- p_by_team2(df=df_melt,Clinic_Name="Zufall",meas_name="OPM4",x_axis_lab=TRUE)
p01 <- p_by_team2(df=df_melt,Clinic_Name="Zufall",meas_name="PM1",x_axis_lab=TRUE)


#plot the measures by team and type of measure  
p_m_M <- reactive({
  team <- input$choose_Team
  data <- values$df_data
  if(!is.null(data) && !is.null(team)) {
    p1 <- p_by_team1(df=data,Clinic_Name=team,meas_type="M",x_axis_lab=FALSE)
  }
})

p_m_N <- reactive({
  team <- input$choose_Team
  data <- values$df_data
  if(!is.null(data) && !is.null(team)) {
    p1 <- p_by_team1(df=data,Clinic_Name=team,meas_type="N",x_axis_lab=FALSE)
  }
})

p_m_D <- reactive({
  team <- input$choose_Team
  data <- values$df_data
  if(!is.null(data) && !is.null(team)) {
    p1 <- p_by_team1(df=data,Clinic_Name=team,meas_type="D",x_axis_lab=TRUE)
  }
})

output$team_plot <- renderPlot({
  
  team_name <- input$choose_Team
  p_M <- p_m_M()
  p_N <- p_m_N()
  p_D <- p_m_D()
  p_m1 <- grid.arrange(p_M,p_N,p_D,
                       top=paste0("Measures for ",team_name,"\n Series median: dashed line; Measure goal: solid line."),
                       ncol=1)
  print(p_m1)
})

#https://cran.r-project.org/web/packages/gridExtra/vignettes/arrangeGrob.html
p_by_team3 <- function(df,Clinic_Name,meas_name,x_axis_lab, asp_ratio=.625, trbl1) {
  dfA <- droplevels(df[df$ShortName==Clinic_Name & 
                         df$Measure==meas_name,])
  y_axis_lab <- "per cent"
  y_goal_label <- paste0("Goal_",meas_name)
  if(meas_name=="OPM1"){
    y_axis_lab <- "per cent"
  } else if(meas_name=="OPM2") {
    y_axis_lab <- "$/Hr"
  } else if(meas_name=="OPM3") {
    y_axis_lab <- "Encounters/Hr"
  } else if(meas_name=="OPM4") {
    y_axis_lab <- "$/Visit"
  }
  
  a <- trbl1
  
  p1 <- ggplot(dfA,aes(x=MeasMonth,y=value)) +
    theme_bw() +
    geom_point(size=3.5)+
    geom_line() +
    ylab(y_axis_lab)+
    theme(axis.title.y=element_text(size=rel(1.5)))+
    theme(axis.text.y=element_text(size=rel(1.5)))+
    theme(axis.text.x=element_text(size=rel(1.25)))+
    theme(plot.margin=unit(c(a[1],a[2],a[3],a[4]), "cm"))+
    xlab(" ") +
    ggtitle(paste0(meas_name)) +
    theme(aspect.ratio=asp_ratio)
  
  #,  
  #Series median: dashed line; Goal: solid line."))
  # theme(axis.text.x=element_text(angle=30,hjust=1,vjust=1))
  
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



debugonce(p_by_team3)
test333 <- p_by_team2(df=df_melt,Clinic_Name="Zufall",meas_name="OM1",x_axis_lab=TRUE)
meas_subset1 <- c("OM1","PM1","PM2","OPM1")
test_list <- lapply(meas_subset1,p_by_team2,df=df_melt, Clinic_Name="Zufall",x_axis_lab=TRUE, trbl1=c(0,0.5,0,.5))
grid.arrange(grobs=test_list, ncol=2, top="Zufall", bottom="Series median: dashed line; Goal: solid line.")


debugonce(clean_up_df1)
test22 <- clean_up_df1(df_master1)
sub("\\.","_","test.1")



p = vector("list", 3) #List for arranging grid

for(ii in 1:3){
  p[[ii]] = ggplot(mtcars, aes(x = wt, y = mpg))+
    geom_point()+
    theme(plot.background = element_rect(colour = 'black', size = 1))
}
do.call("grid.arrange", c(p, ncol=1))


Layout <- grid.layout(nrow = 5, ncol = 1, 
                      heights=c(1, .1, 1, .1, 1) )
# could have written code to alternate heights or widths with gaps
#grid.show.layout(Layout)
vplayout <- function(...) {  # sets up new page with Layout
  grid.newpage()
  pushViewport(viewport(layout = Layout))
}

subplot <- function(x, y) viewport(layout.pos.row = x,
                                   layout.pos.col = y)

mmplot <- function(p1=p) { # could make more general
  vplayout()
  print(p1[[1]], vp = subplot(1, 1))
  print(p1[[2]], vp = subplot(3, 1))
  print(p1[[3]], vp = subplot(5, 1))
}

p111 <- mmplot()

debugonce(mmplot)

team <- "Choptank"
df_out <- droplevels(df_melt1[df_melt1$ShortName==team,c(2:4,6)])
df_out1 <- droplevels(df_melt1[df_melt1$ShortName==team,])
df_out1$MeasType <- as.factor(df_out1$MeasType)
df_out1$MeasType[df_out1$MeasType=="Goal_OPM"] <- "Goal"
df_out1$MeasType[df_out1$MeasType=="OPM"] <- "M"
#need to reorder the columns before melting so that all the measures are grouped.

#recover the raw data
#test222 <- dcast(data=df_out,MeasMonth ~ Measure)
#test233 <- dcast(data=df_out, MeasMonth ~ Goal)

#test444 <- df_master1[,]
#Create a vector of Measure names corresponding to the stack of 1872 records:  one clinic by 13 measures x 4 elements x 36 months.
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
#Code the measure types
#Mblock1 <- c(rep("N",36),rep("D",36),rep("R",36))
#Mtype1 <- rep(Mblock1,13)
#Mtype2 <- rep("Goal",13*36)
#Mtype <- c(Mtype1,Mtype2)

df_out2 <- cbind.data.frame(df_out1,MeasName)
#Now order the levels of the Mstar3 variable
#test444$Mstar3 <- factor(test444$Mstar3, levels=c("OM1","PM1","PM2","PM3",
 #                                                 "PM4","PM5","PM6","PM7","OPM1",
#                                                "OPM2","OPM3","OPM4","OPM5"))
#now order the data frame by levels of Mstar3 variable

df_out3<- dcast(data=df_out2,MeasMonth + MeasName ~ MeasType)


df_out4 <- df_out3[order(df_out3$MeasName),c(1,2,6,3,5,4)]
=======
>>>>>>> kl_test
>>>>>>> origin
