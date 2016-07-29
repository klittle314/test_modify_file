
#references for conditional view of update action button are 
#http://shiny.rstudio.com/articles/dynamic-ui.html and http://shiny.rstudio.com/articles/dynamic-ui.html


 shinyUI(fluidPage(
    titlePanel("NNOHA Collaborative Data Page"),
    img(src='logo.png', align = "top"),
    sidebarLayout(
      sidebarPanel(
        # selectInput("choose_clinic", label = h3("Select clinic"), 
        #             choices = clinic_list, 
        #             selected = 1),
        # 
        # br(),
        
        fileInput('file1', 'Upload Clinic Data Template',
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
        title = 'Exchanging Data with Google Drive',
        h4('This communication may take up to 60 seconds, please wait for screen to refresh.'),
        trigger = 'update1'),
      tabsetPanel(type="tabs",
                  
        tabPanel("table",dataTableOutput("df_data_out")),
        tabPanel("Measure by Team",
                 plotOutput("measure_plot",height="800px"))
      )
    )
  )))
  