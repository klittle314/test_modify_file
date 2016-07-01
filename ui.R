

  shinyUI(fluidPage(
    titlePanel("NNOHA Collaborative Data Page"),
    
    sidebarLayout(
      sidebarPanel(
        selectInput("choose_clinic", label = h3("Select clinic"), 
                    choices = clinic_list, 
                    selected = 1),
        
        br(),
        
        fileInput('file1', 'Upload Clinic Data Template',
                  accept=c('.xlsx','.xls')), 
      
        #drop down to select the Measure
        htmlOutput("selectMeasures"),
        
        #drop down to select the Team
        htmlOutput("selectTeam")
      #actionButton("Update1", "Update the data file"),
      #actionButton("Update2", "Update display")
      ),
    mainPanel(
      tabsetPanel(type="tabs",
                  
        tabPanel("table",dataTableOutput("df_data_out")),
        tabPanel("Measure by Team",
                 plotOutput("measure_plot",height="800px"))
      )
    )
  )))
  