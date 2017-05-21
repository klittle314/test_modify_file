#references for conditional view of update action button are 
#http://shiny.rstudio.com/articles/dynamic-ui.html and http://shiny.rstudio.com/articles/dynamic-ui.html


shinyUI(navbarPage("NNOHA Collaborative Data Web Application",
  
  tabPanel("Overview",
    img(src='logo.png', align = "top"),
    h3("Web App: Update NNOHA Collaborative Master Data and Display Measures"),
    wellPanel(
      tags$style(type="text/css", '#leftPanel { width:200px; float:left;}'),
      helpText("(1) Click on the Update tab to upload data using the Excel file for your health center"),
      helpText("(2) Click on the Display tab to view measures by health center and health centers by measures"),
               
      br(),
      helpText("Questions? Contact Kevin Little, Ph.D., NNOHA Collaborative Improvement Advisor"),
      
      # author info
      shiny::hr(),
      em(
        span("Created by "),
        a("Kevin Little", href = "mailto:klittle@iecodesign.com"),
        span("updated 21 May 2017"),
        br(), br()
      )
    )
  ), 
    
    
  tabPanel("Update",
    sidebarLayout(
      sidebarPanel(
      
      
     # fileInput('file1', label=h3("Upload Your Health Center's Excel Data File"),
     #            accept=c('.xlsx','.xls')),
      
    #Mason suggests 16 Feb 2017 to remove the accept restrictions and somehow catch errors of file type otherwise
        fileInput('file1', label=h3("Upload Your Health Center's Excel Data File")), 
        
      textOutput("excel_confirmation"),
      
      br(),
      #conditional UI
      uiOutput("uploadbutton"),
      
      br(),
      br()
      
      
    ),
    mainPanel(
      bsModal(
        id = 'gs_data_exchange_modal',
        title = 'Data exchange with Google Drive successful.',
        h4('Please close this window to continue.'),
        trigger = 'Update1')
      )
    )
  ),
     
  tabPanel("Display",
           sidebarLayout(
             sidebarPanel( 
      h3("View Collaborative Data by selecting a Measure or a Health Center"),
      #drop down to select the Measure
      htmlOutput("selectMeasures"),
      
      #drop down to select the Health Center
      htmlOutput("selectTeam"),
      br()
    ),
    mainPanel(
        tabsetPanel(type="tabs",
                  
                  tabPanel("Measure by Health Center",
                           plotOutput("measure_plot2",height="750px"),
                           br(),
                           h4("Click to download a .png picture of this display"),
                           br(),
                           downloadButton('downloadMPlot', 'Download')),
                  tabPanel("Median Overplots",
                           plotOutput("over_plot",height="600px"),
                           br(),
                           h4("Click to download a .png picture of this display"),
                           br(),
                           downloadButton('downloadOverPlot', 'Download')),
                  tabPanel("Health Center Measures",
                           plotOutput("team_plot2", height="600px"),
                           br(),
                           h4("Click to download a .png picture of this display"),
                           br(),
                           downloadButton('downloadHCPlot', 'Download')),
                          
                  tabPanel("Health Center Data Table",
                           textOutput("clinic_name"),
                           h4("Records with numerator and denominator values"),
                           tags$head(tags$style("#clinic_name{color: black;
                                 font-size: 20px;
                                                font-style: normal;
                                                }"
                              )
                            ),
                           br(),
                           DT::dataTableOutput("df_data_out"))
                  
      )
     )
    )
  ),
  tabPanel("Admin",
           sidebarLayout(
             sidebarPanel(
               selectInput("select_month", label=h3("Select month"),
                           choices=list("Sept-2016"="2016-09-01","Oct-2016"="2016-10-01",
                                        "Nov-2016"="2016-11-01",
                                        "Dec-2016"="2016-12-01",
                                        "Jan-2017"="2017-01-01",
                                        "Feb-2017"="2017-02-01",
                                        "Mar-2017"="2017-03-01",
                                        "Apr-2017"="2017-04-01",
                                        "May-2017"="2017-05-01",
                                        "Jun-2017"="2017-06-01"
                                        ),
                           selected = "2016-09-01")
             ),
             
             mainPanel(
               textOutput("report0"),
               br(),
               textOutput("report1"),
               br(),
               tableOutput("report2"),
               br(),
               textOutput("report3"),
               br(),
               tableOutput("report4")
             )
             
           )
           
           
  )
 )
)
