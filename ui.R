
#references for conditional view of update action button are 
#http://shiny.rstudio.com/articles/dynamic-ui.html and http://shiny.rstudio.com/articles/dynamic-ui.html


 shinyUI(fluidPage(
   img(src='logo.png', align = "top"),
   titlePanel("NNOHA Collaborative Data Web Application"),
    sidebarLayout(
      sidebarPanel(
        # selectInput("choose_clinic", label = h3("Select clinic"), 
        #             choices = clinic_list, 
        #             selected = 1),
        # 
        # br(),
        
        fileInput('file1', 'Upload Clinic Excel Data File',
                  accept=c('.xlsx','.xls')),
        
        textOutput("excel_confirmation"),
        
        br(),
        #conditional UI
        uiOutput("uploadbutton"),
        
        # verbatimTextOutput("check"),
        # 
        # conditionalPanel(condition= "output.check == 'Click the update button to refresh data tables and displays.'",
        #   actionButton("update1", "Update")
        # ),
        
        br(),
        #drop down to select the Measure
        htmlOutput("selectMeasures"),
        
        #drop down to select the Team
        htmlOutput("selectTeam"),
        br(),
        br(),
        # author info
        shiny::hr(),
        em(
          span("Created by "),
          a("Kevin Little, Ph.D. NNOHA Improvement Advisor", href = "mailto:klittle@iecodesign.com"),
          span(", 28 July 2016"),
          br(), br()
        )
      #actionButton("Update1", "Update the data file"),
      #actionButton("Update2", "Update display")
      ),
    mainPanel(
      bsModal(
        id = 'gs_data_exchange_modal',
        title = 'Data exchange with Google Drive Successful.',
        h4('Close this window to continue.'),
        trigger = 'Update1'),
      tabsetPanel(type="tabs",
                  
        tabPanel("table",dataTableOutput("df_data_out")),
        tabPanel("Measure by Team",
                 plotOutput("measure_plot",height="800px"))
      )
    )
  )))
  