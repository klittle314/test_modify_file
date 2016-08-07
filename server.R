
source("global.R")

shinyServer(function(input, output, session) {
  values <- reactiveValues()
  values$df_data = df_melt
  values$clinic_name = as.character(df_melt$ShortName[1])
  
  #check the excel file for conformance to our structure 31 July 2016:  can add detail to the checks.
  excel_confirmation <- eventReactive(input$file1, {
                    df_clinic <- read.xlsx(input$file1$datapath, sheet=4, startRow=4,detectDates=TRUE)
                    clinic_name <- df_clinic$ClinicName[1]
                  
                    if(!(clinic_name %in% unique(df_master1$ClinicName))) {
                      out_message <- "Clinic name does not match our collaborative list."
                    } else if(nrow(df_clinic) != 36) {
                      out_message <- paste0("Data Table worksheet has ",nrow(df_clinic),"  rows; we require 36 data rows.")
                    } else if(ncol(df_clinic) != 55) {
                      out_message <- paste0("Data Table worksheet has ",ncol(df_clinic)," columns; we require 55 columns.")
                    } else {
                      out_message <- "Spreadsheet passes basic checks."
                    }
      })               
                    

# excel_message <- reactive({
#   excel_message1 <- excel_confirmation()      
#   if(identical(excel_message1,"Spreadsheet passes basic checks.")){
#     excel_message1 <- " "
#   } 
#   return(excel_message1)
# })
                    
output$excel_confirmation <- renderText(excel_confirmation())

                 

df_clinic <- reactive({
      if(identical(excel_confirmation(),"Spreadsheet passes basic checks.")){
        df_clinicA <- read.xlsx(input$file1$datapath, sheet=4, startRow=4,detectDates=TRUE)
      #clean clinic data to our standards
    
       df_clinicA <- clean_up_df1(df_clinicA)
       names(df_clinicA) <- names(df_master1)
    
      #clinic_name <- input$choose_clinic
      #clinic_name <- as.character(df_clinicA$ClinicName[1])
        return(df_clinicA)
    } else {
      df_clinicA <- NULL
  }
})
  


#conditional show of the upload button, show only if excel check is OK
output$uploadbutton <- renderUI({
  excel_message1 <- excel_confirmation()      
  if(identical(excel_message1,"Spreadsheet passes basic checks.")) {
    tagList(
      HTML("Click the Update! button to merge clinic data with the Google Sheet master data file."),
      br(),
      br(),
      HTML("The update may take up to 60 seconds, please wait for confirmation message in pop up window."),
      br(),
      br(),
      actionButton("Update1", label = "Update!", class = NULL)
    )
  } else NULL
  
})


#get the index of records (start position and count) for clinic in df_master1 to use to revise the google sheet
observeEvent(input$Update1,{                 
                    toggleModal(
                            session = session,
                            modalId = 'gs_data_exchange_modal',
                            toggle = 'close')  
                     df_clinicA <- df_clinic()
                     clinic_name <- df_clinicA$ClinicName[1]
                     #delete clinic records from the master file
                     df_all_but_clinic <- df_master1[df_master1$ClinicName!=clinic_name,]
                     #need to add 1 to index because the first row of the googlesheet is a header row, not data
                     idx_start_old <- match(clinic_name,df_master1$ClinicName)
                     nrec_clinic_old <- length(df_master1$ClinicName[df_master1$ClinicName==clinic_name])
                     idx_end_old <- idx_start_old + nrec_clinic_old-1  #
                     
                     #get the index of records for clinic in df_clinic
                     nrec_clinic_new <- nrow(df_clinicA)
                     #diagnostic print
                     cat(file=stderr(),"nrec clinic old",nrec_clinic_old,"nrec clinic new",nrec_clinic_new,"\n")
                     cat(file=stderr(), "check identical function",isTRUE(all.equal(nrec_clinic_old,nrec_clinic_new)),"\n")
                     
                     #reinitialize gs object [do I need this?]
                     
                     gsobj <- gs_key(x=gskey2)
                     #define the cell in the first row of the clinic's records in df_master1 and the google sheet
                     anchor1 <- paste0("A",as.character(idx_start_old+1))
                     
            
                     if(isTRUE(base::all.equal(nrec_clinic_old,nrec_clinic_new))) {
                       #since the new record set has same number of rows as old record set, simply replace old with new df
                       gs_edit_cells(ss=gsobj,ws="Summary_Data",input=df_clinicA,col_names=FALSE,anchor=anchor1)
                       df_master1[idx_start_old:idx_end_old,] <- df_clinicA
                     } else cat(file=stderr(),"records of uploaded file do not match master file")
                     #now create the revised dataframe to use in plotting and summaries by the Shiny app         
                     df_new <- df_master1
                     
                     
                   #now create a melted, reduced version of the data frame for manipulation (remove RepMonth variable)
                   df_new1 <- melt(df_new[,-2],id.vars=c("ClinicName","MeasMonth"),variable.name="Measure")
                   df_new1$ClinicName <- as.factor(df_new1$ClinicName)
                   #append measure type column
                   df_new1$MeasType <- measure_type_maker(df_new1)
                   
                   #append goals column
                   df_new1 <- goal_melt_df(df_new1)
                   
                   #append shortnames column
                   df_new1$ShortName <- mapvalues(df_new1$ClinicName,from=df_clinic_names$Clinic.Name,df_clinic_names$Short.Name)

                   #order the clinics by PM1_D values, largest to smallest
                   df_new1 <- reorder_df(df_new1)
                   values$df_data <- df_new1  
                   
                   #update the clinic name, using the short name
                   values$clinic_name <- df_clinic_names$Short.Name[grep(clinic_name,df_clinic_names$Clinic.Name)]
                  

                                  
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
      selectInput("choose_Meas",label=h4("Choose measure to plot by health center"),choices=measure_choice1,width="100%")
    }
  })
  
  #26 July 2016 revised this function to put the clinic team just uploaded as the first entry in drop down
  team_choice <- reactive({
    data <- values$df_data
    if(!is.null(data)) {
      levs <- sort(as.character(levels(as.factor(data$ShortName))))
      levs1 <- levs[levs != values$clinic_name]
      levs <- c(values$clinic_name,levs1)
      return(levs)
    }
  })
  
  output$selectTeam <- renderUI({
    team_choice1 <- team_choice()
    if(!is.null(team_choice1)) {
      selectInput("choose_Team",label=h4("Choose health center"),choices=team_choice1,width="100%")
    }
  })  
  
  output$measure_plot <- renderPlot({
    measure <- input$choose_Meas
    data <-  values$df_data
    if(!is.null(data) && !is.null(measure)) {
      p_m2 <- p_by_measure(df=data,MName=measure,p_nrow=5)
      print(p_m2)
    }
  })
  
  #https://cran.r-project.org/web/packages/gridExtra/vignettes/arrangeGrob.html describes how to arrange grobs
 
  
  team_plot <- reactive({
    #for testing only, make measure subset dynamic or at least by reference
    meas_subset1 <- c("OM1","PM1","PM2","OPM1")
    team <- input$choose_Team
    data <- values$df_data
    # if(!is.null(data) && !is.null(team)) {
    
    p_list<- lapply(meas_subset1,p_by_team2,df=data,Clinic_Name=team,x_axis_lab=TRUE,asp_ratio=5/8)
    p_out <- grid.arrange(grobs=p_list, 
                          ncol=2, 
                          top=textGrob(team, gp=gpar(fontsize=20)),
                          bottom=textGrob("Series median: dashed line; Goal: solid line.",gp=gpar(fontsize=20)))
  })
   
  #idea from https://groups.google.com/forum/#!msg/shiny-discuss/u7gwXc8_vyY/IZK_o7b7I8gJ tp create non-reactive fcn
  team_plot0 <- function(){
    #need to revise subset of measures during 2016-17 as we add reporting requirement
    meas_subset1 <- c("OM1","PM1","PM2","OPM1")
    team <- input$choose_Team
    data <- values$df_data
    # if(!is.null(data) && !is.null(team)) {
    
    p_list<- lapply(meas_subset1,p_by_team2,df=data,Clinic_Name=team,x_axis_lab=TRUE,asp_ratio=5/8,trbl1=c(0,.5,0,.5))
    p_out <- grid.arrange(grobs=p_list, 
                          ncol=2, 
                          top=textGrob(team, gp=gpar(fontsize=20)),
                          bottom=textGrob("Series median: dashed line; Goal: solid line.",gp=gpar(fontsize=14)))
  }
  
  output$team_plot2 <- renderPlot({
    print(team_plot())
  })
  
  output$downloadFile <- downloadHandler(
    filename = function() { 
      paste0(input$choose_Team, "_", Sys.Date(),'.png') 
    },
    content = function(file) {
      png(file)
      print(team_plot0())
      dev.off()
    }
  )
  
  output$df_data_out <- renderDataTable(values$df_data[,c(1,2,3,4,6)])
})