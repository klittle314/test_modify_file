
source("global.R")

shinyServer(function(input, output) {
  values <- reactiveValues(df_data = df_melt)
  
  # df_to_update <- eventReactive(input$Update1,{
  #   withProgress(message ='Exchanging Data with Google Drive', 
  #                detail='This communication may take 60 seconds, please wait for screen to refresh.', value=0
  
  observeEvent(input$file1, {
    withProgress(message ='Exchanging Data with Google Drive', 
                 detail='This communication may take 60 seconds, please wait for screen to refresh.', value=1, {
                   clinic_name <- input$choose_clinic
                   #retrieve new clinic data 
                   df_clinic <- read.xlsx(input$file1$datapath, sheet=4, startRow=4,detectDates=TRUE)
                   
                   #clean clinic data to our standards
                   df_clinic <- clean_up_df1(df_clinic)
                   names(df_clinic) <- names(df_master1)
                   
                   #two cases:  the googlesheet already has data from the clinic or we just add the clinic data to end of sheet
                   #to speed processing, we need to set up the master data sheet in Google with dummy values for each clinic
                   #and we need to have the format locked down of the spreadsheets so we are in the simplest case of just
                   #replacing a rectangle of 35 x 55 cells on update.
                   # Case 1
                   
                   
                   #get the index of records (start position and count) for clinic in df_master1 to use to revise the google sheet
                   
                   if(clinic_name %in% unique(df_master1$ClinicName)) {
                     #delete clinic records from the master file
                     df_all_but_clinic <- df_master1[df_master1$ClinicName!=clinic_name,]
                     
                     idx_start <- match(clinic_name,df_master1$ClinicName)
                     nrec_clinic <- length(df_master1$ClinicName[df_master1$ClinicName==clinic_name])
                     idx_end <- idx_start + nrec_clinic
                     
                     #get the index of records for clinic in df_clinic
                     nrec_clinic_new <- nrow(df_clinic)
                     #diagnostic print
                     cat(file=stderr(),"nrec clinic",nrec_clinic,"nrec_clinic_new",nrec_clinic_new,"\n")
                     cat(file=stderr(), "check identical function",isTRUE(all.equal(nrec_clinic,nrec_clinic_new)),"\n")
                     
                     #reinitialize gs object [do I need this?]
                     
                     gsobj <- gs_key(x=gskey2)
                     #define the cell in the first row of the clinic's records in df_master1 and the google sheet
                     anchor1 <- paste0("A",as.character(idx_start))
                     
                     if(isTRUE(all.equal(nrec_clinic,nrec_clinic_new))) {
                       #since the new record set has same number of rows as old record set, simply replace old with new df
                       gs_edit_cells(ss=gsobj,ws="Summary_Data",input=df_clinic,col_names=FALSE,anchor=anchor1)
                     } else if(nrec_clinic > nrec_clinic_new) {
                       #number of old records is greater than the number of new records
                       #get number of columns in df_master1, which by assumption is then same number as in df_clinic
                       ncol_clinic <- ncol(df_master1)
                       #create a matrix of NA to overwrite the existing records
                       tbl_NA <- cbind(rep(clinic_name,nrec_clinic),matrix(data=NA,nrow=nrec_clinic,ncol=ncol_clinic-1))
                       gs_edit_cells(ss=gsobj,ws="Summary_Data",input=tbl_NA,col_names=FALSE,anchor=anchor1)
                       #call the object again?
                       gsobj <- gs_key(x=gskey2)
                       gs_edit_cells(ss=gsobj,ws="Summary_Data",input=df_clinic,col_names=FALSE,anchor=anchor1)
                     } else {
                       #final case the number of old rows is less than the number of new rows, so split up the new set
                       #write the first nrec_clinic rows into the position of the old clinic records
                       gs_edit_cells(ss=gsobj,ws="Summary_Data",input=df_clinic[1:nrec_clinic,],col_names=FALSE,anchor=anchor1)
                       #call the object again?
                       gsobj <- gs_key(x=gskey2)
                       anchor2 <- paste0("A", as.character(nrow(df_master1)+1))
                       gs_edit_cells(ss=gsobj,ws="Summary_Data",input=df_clinic[(nrec_clinic+1):nrec_clinic_new,],col_names=FALSE,anchor=anchor2)
                     }
                     #now create the revised dataframe to use in plotting and summaries by the Shiny app         
                     df_new <- rbind(df_all_but_clinic,df_clinic)   
                     
                     
                     #Case 2:  the clinic has no records in the google sheet and we append at bottom of table
                   } else {
                     gsobj <- gs_key(x=gskey2)
                     idx_start <- nrow(df_master1)+2
                     anchor1 <- paste0("A",as.character(idx_start))
                     gs_edit_cells(ss=gsobj,ws="Summary_Data",input=df_clinic,col_names=FALSE,anchor=anchor1)
                     #now create the revised dataframe to use in plotting and summaries 
                     df_new <- rbind(df_master1,df_clinic)
                   }
                   
                   #now create a melted, reduced version of the data frame for manipulation (remove RepMonth variable)
                   df_new1 <- melt(df_new[,-2],id.vars=c("ClinicName","MeasMonth"),variable.name="Measure")
                   df_new1$ClinicName <- as.factor(df_new1$ClinicName)
                   #append measure type column
                   df_new1$MeasType <- measure_type_maker((df_new1))
                   values$df_data <- df_new1               
                 })  
  })
  
  measure_choice <- reactive({
    data <- values$df_data
    if(!is.null(data)) {
      levs <- levels(as.factor(data$Measure))
      return(levs)
    }
  })
  #
  #
  output$selectMeasures <- renderUI({
    measure_choice1 <- measure_choice()
    if(!is.null(measure_choice1)) {
      selectInput("choose_Meas",label=h4("Choose measure"),choices=measure_choice1,width="100%")
    }
  })
  
  
  team_choice <- reactive({
    data <- values$df_data
    if(!is.null(data)) {
      levs <- sort(as.character(levels(as.factor(data$ClinicName))))
      return(levs)
    }
  })
  
  output$selectTeam <- renderUI({
    team_choice1 <- team_choice()
    if(!is.null(team_choice1)) {
      selectInput("choose_Team",label=h4("Choose team"),choices=team_choice1,width="100%")
    }
  })  
  
  output$measure_plot <- renderPlot({
    measure <- input$choose_Meas
    data <-  values$df_data
    if(!is.null(data) && !is.null(measure)) {
      p_m2 <- p_by_measure(df=data,MName=measure,p_nrow=1)
      print(p_m2)
    }
  })
  
  output$df_data_out <- renderDataTable(values$df_data)
})