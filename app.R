#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(gapminder)
library(agricolae)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Anova"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        fileInput(
          'file1', 'Choose file to upload', 
          accept = c( 
            'text/csv', 
            'text/comma-separated-values', 
            'text/tab-separated-values', 
            'text/plain', 
            '.csv', 
            '.tsv' 
          )
        ), 
        tags$hr(), 
        checkboxInput('header', 'Header', TRUE), 
        radioButtons(
          'sep', 'Separator'
          , c(Comma=',', Semicolon=';', Tab='\t'), ','
        ), 
        radioButtons(
          'quote', 'Quote'
          , c(None='', 'Double Quote'='"', 'Single Quote'="'"), '"'
        ), 
        tags$hr(),
        p('If you want a sample .csv or .tsv file to upload,', 
          'you can first download the sample', 
          a(href = 'mtcars.csv', 'mtcars.csv'), 'or', 
          a(href = 'pressure.tsv', 'pressure.tsv'), 
          'files, and then try uploading them.' 
        )
        
      ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            tabPanel("DT",DT::dataTableOutput('contents')),
            tabPanel("ANOVA",
                     uiOutput("varNames"),
                     uiOutput("varNames_y"),
                     verbatimTextOutput("anovaTable")
            ),
            tabPanel("Tukey Test"
                     , verbatimTextOutput("TukeyTable")
                     , verbatimTextOutput("TukeyGroup")
                     , plotOutput("TukeyPlot"))
          )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {


  output$contents <- DT::renderDataTable({
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    
    k <- read.csv(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote) 
  })

  output$varNames <- renderUI({
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    
    k <- read.csv(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote) 
    selectizeInput("varNames2",
                   "Variables: ",
                   choices = names(k[,-(1:4)])
    )
  })

  output$varNames_y <- renderUI({
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    
    k <- read.csv(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote) 
    selectizeInput("varNames3",
                   "Variables Y: ",
                   choices = names(dplyr::select_if(k, is.character))
    )
  })

  output$anovaTable <- renderPrint({     
    inFile <- input$file1 
    
    if (is.null(inFile)) return(NULL) 
    k<-read.csv(
      inFile$datapath
      , header = input$header
      , sep = input$sep
      , quote = input$quote
    )
    aov.model <- aov(k[[input$varNames2]] ~ k[[input$varNames3]], data = k)
    print(aov.model)
    print(summary(aov.model))
  })
  
  
  output$TukeyTable <- renderPrint({     
    inFile <- input$file1
    if (is.null(inFile)) return(NULL) 
    k<-read.csv(
      inFile$datapath
      , header = input$header
      , sep = input$sep
      , quote = input$quote
    )
    model <- aov( k[[input$varNames2]] ~ factor(k[[input$varNames3]]), data = k )
    out <- HSD.test(model, 'factor(k[[input$varNames3]])', group = F)
    print(out$comparison)
  })

  output$TukeyGroup <- renderPrint({     
    inFile <- input$file1
    if (is.null(inFile)) return(NULL) 
    k<-read.csv(
      inFile$datapath
      , header = input$header
      , sep = input$sep
      , quote = input$quote
    )
    model <- aov( k[[input$varNames2]] ~ factor(k[[input$varNames3]]), data = k )
    out <- HSD.test(model, 'factor(k[[input$varNames3]])', group = T)
    print(out$groups)
  })

  output$TukeyPlot <- renderPlot({     
    inFile <- input$file1 
    
    if (is.null(inFile)) return(NULL) 
    k <- read.csv(
      inFile$datapath
      , header = input$header
      , sep = input$sep
      , quote = input$quote
    )
    model=lm( k[[input$varNames2]] ~ factor(k[[input$varNames3]]), data = k )
    ANOVA=aov(model)
    
    # Tukey test to study each pair of treatment :
    TUKEY <- TukeyHSD(x=ANOVA, 'factor(k[[input$varNames3]])', conf.level=0.95)
    
    # Tuckey test representation :
    plot(TUKEY , las=1 , col="steelblue")
  })

}


shinyApp(ui, server)
