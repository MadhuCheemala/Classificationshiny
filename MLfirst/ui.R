library(shiny)

shinyUI(fluidPage(
  
 HTML('<style type="text/css">
         .content-wrapper,
                   .right-side {
                   background-color:#f2f2f2;font-family: Myriad Pro;
                   }
                   
                   </style>'),
 
              HTML('<style type="text/css">
                   .well { background-color: white;}
                   .nav-tabs>li>a:hover{background-color: #a9a9a9;}
                   </style>'),
 
 
 HTML('<style type="text/css">
       .well { background-color: white;
      }
      .nav-tabs>li>a:hover{background-color: #a9a9a9;}
      
      table, td, th {
      border: 1px solid #003366;
      }
      
      th {
      background-color: #003366;
      color: white;
      }
      
      .col-sm-4 {
    width: 20.3333%;
      }
       .col-sm-8 {
    width: 79.6666%;
      }
      </style>'),
 
              
tags$style("body {background-color: #f0f0f0;font-family: Myriad Pro;}"),
tags$style(type='text/css', '#dim {background-color:white  ; color: black;}'), 
tags$style(type='text/css', '#structure {background-color:white  ; color: black;}'), 
tags$style(type='text/css', '#summary {background-color:white  ; color: black;}'), 
tags$style(type='text/css', '#rawDataView {background-color:white  ; color: black;}'), 
tags$style(type='text/css', '#corr {background-color:white  ; color: black;}'), 
tags$style(type='text/css', '#describe {background-color:white  ; color: black;}'), 
 
 
 
tags$style(type='text/css', '#traindim {background-color:white  ; color: black;}'), 
tags$style(type='text/css', '#trainstructure {background-color:white  ; color: black;}'), 
tags$style(type='text/css', '#trainsummary {background-color:white  ; color: black;}'), 
tags$style(type='text/css', '#traindataview {background-color:white  ; color: black;}'), 
tags$style(type='text/css', '#traincorr {background-color:white  ; color: black;}'), 
tags$style(type='text/css', '#traindescribe {background-color:white  ; color: black;}'), 
 
 
tags$style(type='text/css', '#testdim {background-color:white  ; color: black;}'), 
tags$style(type='text/css', '#teststructure {background-color:white  ; color: black;}'), 
tags$style(type='text/css', '#testsummary {background-color:white  ; color: black;}'), 
tags$style(type='text/css', '#testdataview {background-color:white  ; color: black;}'), 
tags$style(type='text/css', '#testcorr {background-color:white  ; color: black;}'), 
tags$style(type='text/css', '#testdescribe {background-color:white  ; color: black;}'), 
tags$style(type='text/css', '#trainResultsUI1 {background-color:white  ; color: black;}'), 
tags$style(type='text/css', '#pr {background-color:white  ; color: black;}'), 

tags$style(type='text/css', '#trainResultsUI {background-color:white  ; color: black;}'), 
tags$style(type='text/css', '#bestResultsUI {background-color:white  ; color: black;}'), 
tags$style(type='text/css', '#ts {background-color:white  ; color: black;}'), 
tags$style(type='text/css', '#perf {background-color:white  ; color: black;}'), 
tags$style(type='text/css', '#ac {background-color:white  ; color: black;}'),  
tags$style(type='text/css', '#esr {background-color:white  ; color: red;}'),  


 
# img(src="img1.png", height = 400, width = 400),
h1("Classification Algorithms",align = "center",style = "color:#003366;font-family: Myriad Pro"),
  sidebarLayout(position = "right",
    sidebarPanel(
      fileInput('file1',label = "Upload CSV File",
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
      tags$hr(),
      uiOutput("dependent"),
      uiOutput("independents"),
      tags$hr(),

sliderInput("slidertrainsplit",
            "Training Obseravation",
            min = 0, max = 1, value = 0.7, step = 0.05)
      
      # uiOutput('ui.action') 
    ),


    mainPanel( 
      
      tabsetPanel(
        
        tabPanel("DataTable",tabsetPanel(tabPanel("DataView", DT::dataTableOutput("rawDataView"),
                   h4(verbatimTextOutput("dim"))),
                   
                   tabPanel( "Structure",verbatimTextOutput("structure")),
                   
                   tabPanel( "Describe",DT::dataTableOutput("describe"),h4("Skewness and Kurtosis dataPlot"),plotOutput("plotsk")),
                             # h4("Kurtosis of dataPlot"),plotOutput("plotku")),
                   
                   tabPanel( "Summary",DT::dataTableOutput("summary"),h4("SummaryPlot"),plotOutput("sumplot")),
                   
                   tabPanel( "Correlation",DT::dataTableOutput("corr"),h4("CorrelationPlot"),plotOutput("corplot"))
                   
                )),
        
        
        tabPanel("Training and Testing",    tabsetPanel(
            tabPanel("Training"   ,tabsetPanel(
            tabPanel("Trainview",DT::dataTableOutput("traindataview"),
            h4(verbatimTextOutput("traindim"))),
            tabPanel( "Structure",verbatimTextOutput("trainstructure")),
            tabPanel( "Describe",DT::dataTableOutput("traindescribe"),h4("Skewness and Kurtosis dataPlot"),plotOutput("trplotsk")),
            tabPanel( "Summary",DT::dataTableOutput("trainsummary"),h4("TrainingdataSummaryPlot"),plotOutput("trainsumplot")),
            tabPanel( "Correlation",DT::dataTableOutput("traincorr"),h4("TrainingCorPlot"),plotOutput("trcorplot"))
          )),
          
          
          
          tabPanel( "Testing",tabsetPanel(
            tabPanel("Testingview",DT::dataTableOutput("testdataview"),
            h4(verbatimTextOutput("testdim"))),
            tabPanel( "Structure",verbatimTextOutput("teststructure")),
            tabPanel( "Describe",DT::dataTableOutput("testdescribe"),h4("Skewness and Kurtosis dataPlot"),plotOutput("ttplotsk")),
            tabPanel( "Summary",DT::dataTableOutput("testsummary"),h4("TestingdataSummaryPlot"),plotOutput("testsumplot")),
            tabPanel( "Correlation",DT::dataTableOutput("testcorr"),h4("TestingCorPlot"),plotOutput("ttcorplot"))
          ))
        )),
        
        
        
        tabPanel("Model Selection", tabsetPanel(
          
          tabPanel("Evaluation ",
                   
                   radioButtons("crossFoldTypeUI","Cross Validation Type",c("K-Fold CV"='cv',"Repeated KFold CV"="repeatedcv"),"K-Fold CV"),
                   
                   numericInput("foldsUI","Number of Folds(k)",5),
                   
                   conditionalPanel(condition="input.crossFoldTypeUI == repeatedcv",numericInput("repeatUI","Number of Repeats",5)),
                   
                   uiOutput("CVTypeUI"),
                   
                   radioButtons("preprocessingUI","Pre-processing Type",c('No Preprocessing'="",'PCA'="pca"),'No Preprocessing'),
                   
                   uiOutput("ppUI"),
                   
                   
                   selectInput("modelSelectionUI","Select Model",
                               c('Elastic Net'="en",'Neural Network'="nn",
                                 'Random Forest'="rf"),"Elastic Net"),
                   
                   
                   
                   uiOutput("modelParametersUI"),
                   
                   tags$hr(),
                   
                   actionButton("runAnalysisUI","Run Analysis"))

                      ,id="mainTabUI")),
        
        tabPanel("Analysis Results",    tabsetPanel(
          tabPanel("Full Model Output", h4("TrainingStructure"),tableOutput("trainResultsUI1"),
                   h4("Full Model Output"),tableOutput("trainResultsUI"),h4("Best Fit Model"),tableOutput("bestResultsUI")),
          tabPanel("AccuracyPlot",plotOutput("finalPlotUI")),
          tabPanel("Quick Analysis",
                   h4("Confusion Matrix"),tableOutput("pr"),
                   h4("Accuracy"),tableOutput("ac"),
                   h4("Performance"),tableOutput("perf"),
                   h4("Estimated out-of-sample error"),verbatimTextOutput("esr"),
                   h4("Testcases"),verbatimTextOutput("ts"),
                   h4("Confusionmatrixplot"),plotOutput("confplot")
                   )
           )),


        tabPanel("Plots",    tabsetPanel( tabPanel("CaretPlot",plotOutput("caretPlotUI"))
         
        ))
        
        ) 
        
  )
  
))

)




