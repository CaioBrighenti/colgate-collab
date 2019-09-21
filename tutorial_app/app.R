library(shiny)
library(ggplot2)
library(tools)
library(tidyverse)

ui <- fluidPage(
  
  # App title ----
  titlePanel("Violin Plots"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: File browse to load data
      fileInput(inputId = "dat_file",
                label = "File input"
      ),
      
      # Input: Drop down to select y var
      selectInput(inputId = "y_var",
                  label = "Continuous variable",
                  choices = c("y1","y2")),
      
      # Input: Drop down to select x var
      selectInput(inputId = "x_var",
                  label = "Categorical variable",
                  choices = c("x1","x2"))
      
      
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "violinPlot"),
      fluidRow(column(4, verbatimTextOutput("value")))
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$violinPlot <- renderPlot({
    req(v$dat)
    x<-v$dat[,which(colnames(v$dat) == input$x_var)]
    y<-v$dat[,which(colnames(v$dat) == input$y_var)]
    
  })
  
  v <- reactiveValues()
  
  output$value <- renderPrint({
    str(v$dat)
  })
  
  observeEvent(input$dat_file,{
    url = input$dat_file$datapath
    tryCatch(
      {input_data <- switch(file_ext(url),
                            "csv"=read.csv(url),
                            "dta"=read.dta(url),
                            "tsv"=read_tsv(url),
                            "xls"=read_excel(url),
                            "xlsx"=read_excel(url)
      )}, error = function(e){
        showNotification("Invalid data",type="error",duration=3.5)
        return()
      }
    )
    req(input_data)
    v$dat <- as_tibble(input_data)
    
    updateSelectInput(session, "y_var",
                      label = paste("Select input label", length(colnames(v$dat))),
                      choices = colnames(v$dat),
                      selected = tail(colnames(v$dat), 1)
    )
    updateSelectInput(session, "x_var",
                      label = paste("Select input label", length(colnames(v$dat))),
                      choices = colnames(v$dat),
                      selected = tail(colnames(v$dat), 1)
    )
  })  
  
}
shinyApp(ui, server)
