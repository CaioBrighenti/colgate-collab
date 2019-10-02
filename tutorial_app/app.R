library(shiny)
library(ggplot2)
library(tools)
library(tidyverse)

ui <- fluidPage(
  
  # App title ----
  titlePanel("Violin Plots"),
  
  # Main panel for displaying outputs ----
  mainPanel(
    # Output: Tabset w/ plot, summary, and table ----
    tabsetPanel(type = "tabs",
                id = "tab_panel",
                tabPanel("Data Upload",
                   h3('Data Upload'),
                   # Input: File browse to load data
                   fileInput(inputId = "dat_file",
                             label = "File input"
                   ),
                   conditionalPanel(condition = "output.fileUploaded == true",
                                    selectInput(inputId = "continuous_vars",
                                           label = "Continuous variables to remap as categorical",
                                           multiple = TRUE,
                                           choices = NULL),
                                    selectInput(inputId = "categorical_vars",
                                                label = "Categorical variables to remap as continuous",
                                                multiple = TRUE,
                                                choices = NULL),
                                    actionButton("plot_button", "Plot my data")
                                    )
                ),
                tabPanel("Violin Plot",
                  hr(),
                  sidebarLayout(
                    sidebarPanel(
                      # Input: Drop down to select y var
                      selectInput(inputId = "y_var",
                                  label = "Continuous variable",
                                  choices = c("y1","y2")),
                      # Input: Drop down to select x var
                      selectInput(inputId = "x_var",
                                  label = "Categorical variable",
                                  choices = c("x1","x2"))
                    ),
                    mainPanel(
                      # Output: Histogram ----
                      plotOutput(outputId = "violinPlot")
                      )
                  )
                )
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {

  ## hide tabs until needed
  hideTab(inputId = "tab_panel", target = "Violin Plot")
  
  ## creates a reactive violin plot from user uploaded data
  output$violinPlot <- renderPlot(width=500,height=500,{
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
    str(v$dat)
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
    ## update input options
    factor_idx <- which(sapply(input_data, class) %in% c('factor', 'logical'))
    updateSelectInput(session, "continuous_vars",
                      choices = colnames(input_data[-factor_idx]),
                      selected = NULL
    )
    updateSelectInput(session, "categorical_vars",
                      choices = colnames(input_data[factor_idx]),
                      selected = NULL
    )
    ## put data into reactive value
    v$dat <- input_data
  })  
  
  ## keeps track of when file has been succesfuly uploaded
  output$fileUploaded <- reactive({
    return(!is.null(v$dat))
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  
  ## button to active plot panel
  observeEvent(input$plot_button, {
    req(v$dat)
    ## update categorical/numeric
    cat_idx <- which(colnames(v$dat) == input$categorical_vars)
    v$dat[,cat_idx] = as.numeric(v$dat[,cat_idx])
    cont_idx <- which(colnames(v$dat) == input$continuous_vars)
    v$dat[,cont_idx] = as.factor(v$dat[,cont_idx])
    ## update drop downs to use variable names
    factor_idx <- which(sapply(v$dat, class) %in% c('factor', 'logical'))
    updateSelectInput(session, "y_var",
                      choices = colnames(v$dat[-factor_idx]),
                      selected = tail(colnames(v$dat[-factor_idx]), 1)
    )
    updateSelectInput(session, "x_var",
                      choices = colnames(v$dat[factor_idx]),
                      selected = tail(colnames(v$dat[factor_idx]), 1)
    )
    ## make tab visible
    showTab(inputId = "tab_panel", target = "Violin Plot")
    updateTabsetPanel(session,"tab_panel","Violin Plot")
  })
  
}
shinyApp(ui, server)
