#references for conditional view of update action button are 
#http://shiny.rstudio.com/articles/dynamic-ui.html and http://shiny.rstudio.com/articles/dynamic-ui.html


shinyUI(navbarPage("NNOHA Collaborative Data Web Application",
  
  tabPanel("Overview",
    img(src='logo.png', align = "top"),
    h3("Web App: Update NNOHA Collaborative Master Data and Display Measures"),
    wellPanel(
      tags$style(type="text/css", '#leftPanel { width:200px; float:left;}'),
      helpText("Click on the Update/Display tab to:"),
      helpText("(1) Upload data using the Excel file for your health center"),
      helpText("(2) View measures by health center and health centers by measures"),
               
      br(),
      helpText("Questions? Contact Kevin Little, Ph.D., NNOHA Collaborative Improvement Advisor"),
      
      # author info
      shiny::hr(),
      em(
        span("Created by "),
        a("Kevin Little", href = "mailto:klittle@iecodesign.com"),
        span("1 August 2016"),
        br(), br()
      )
    )
  ), 
    
    
  tabPanel("Update/Display",
    sidebarLayout(
      sidebarPanel(
      
      
      fileInput('file1', "Upload Your Health Center's Excel Data File",
                accept=c('.xlsx','.xls')),
      
      textOutput("excel_confirmation"),
      
      br(),
      #conditional UI
      uiOutput("uploadbutton"),
      
      br(),
      #drop down to select the Measure
      htmlOutput("selectMeasures"),
      
      #drop down to select the Health Center
      htmlOutput("selectTeam"),
      br()
    ),
    mainPanel(
      bsModal(
        id = 'gs_data_exchange_modal',
        title = 'Data exchange with Google Drive Successful.',
        h4('Close this window to continue.'),
        trigger = 'Update1'),
      tabsetPanel(type="tabs",
                  
                  tabPanel("Measure by Health Center",
                           plotOutput("measure_plot",height="800px")),
                  #to uncomment when I have the function working
                  tabPanel("Health Center Measures",
                           plotOutput("team_plot", height="800px")),
                  tabPanel("Data Table",dataTableOutput("df_data_out"))
      )
     )
    )
  )
 )
)
