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
      h3('Data Upload'),
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
      #fluidRow(column(4, verbatimTextOutput("value"))),
      hr()
    ),
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  
  ## creates a reactive violin plot from user uploaded data
  output$violinPlot <- renderPlot({
    ## ensure data loaded, else stop
    req(v$dat)
    ## pull out x and y 
    x<-v$dat[,which(colnames(v$dat) == input$x_var)]
    y<-v$dat[,which(colnames(v$dat) == input$y_var)]
    ## create data frame for plotting
    dat<-data.frame(x,y)
    ## plot data
    ggplot(dat, aes(x=x,y=y))+
      geom_violin(aes(fill = dat$x), color='black', alpha=0.5)+
      geom_boxplot(width=0.1) +
      theme_minimal()
    
  })
  
  ## init reactive values
  v <- reactiveValues()
  
  ## test output
  output$value <- renderPrint({
    #str(v$dat)
  })
  
  
  ## loads data into reactive input and updates necessary panels
  ## runs when user uploads a new file
  observeEvent(input$dat_file,{
    ## get file path
    url = input$dat_file$datapath
    ## load using appropriate method
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
    ## ensure data load
    req(input_data)
    
    ## update drop downs to use variable names
    factor_idx <- which(sapply(input_data, class) %in% c('factor', 'logical'))
    updateSelectInput(session, "y_var",
                      choices = colnames(input_data[-factor_idx]),
                      selected = tail(colnames(input_data[-factor_idx]), 1)
    )
    updateSelectInput(session, "x_var",
                      choices = colnames(input_data[factor_idx]),
                      selected = tail(colnames(input_data[factor_idx]), 1)
    )
    ## put data into reactive value
    v$dat <- input_data
  })  
  
}
shinyApp(ui, server)
