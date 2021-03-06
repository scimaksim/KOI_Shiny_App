library(shiny)
library(shinydashboard)
library(DT)
library(htmltools)
library(latex2exp)
library(tidyverse)
library(ggrepel)
library(grid)
library(caret)
library(lares)
library(data.table)

# Custom Shiny input binding for selecting model predictors, sourced from
# https://github.com/rstudio/shiny-examples/tree/main/036-custom-input-control
source("chooser.R")

ui <- dashboardPage(skin="blue",
                    
                    #add title
                    dashboardHeader(title="NASA - Kepler Objects of Interest",titleWidth=1000),
                    
                    #define sidebar items
                    dashboardSidebar(sidebarMenu(
                      menuItem("About", tabName = "about", icon = icon("info")),
                      menuItem("Data", tabName = "data", icon = icon("table")),
                      menuItem("Data Exploration", tabName = "exploration", icon = icon("chart-line")),
                      menuItem("Modeling", tabName = "modeling", icon = icon("laptop-code")),
                      menuItem("References", tabName = "references", icon = icon("book"))
                    )),
                    
                    #define the body of the app
                    dashboardBody(
                      tabItems(
#------------------------------------------------------------------------------------------------------------                        
#-------------------------------------------About------------------------------------------------------------ 
#------------------------------------------------------------------------------------------------------------                          
                        # First tab content
                        tabItem(tabName = "about",
                                fluidRow(
                                  #add in latex functionality if needed
                                  withMathJax(),
                                  
                                  #two columns for each of the two items
                                  column(6,
                                         #Description of App
                                         h1("What does this app do?"),
                                         #box to contain description
                                         box(background="red",width=12,
                                             h4("This application shows the relationship between the prior distribution and the posterior distribution for a simple Bayesian model."),
                                             h4("The prior distribution is assumed to be a Beta distribution and the likelihood is a Binomial distribution with 30 trials (of which you can change the number of successes).  This yields a Beta distribution as the posterior. Note: As the prior distribution is in the same family as the posterior, we say the prior is conjugate for the likelihood."),
                                             h4("This application corresponds to an example in ",span("Mathematical Statistics and Data Analysis",style = "font-style:italic"), "section 3.5, example E, by John Rice."),
                                             h4("The goal of the example is to update our belief about the parameter \\(\\Theta\\) = the probability of obtaining a head when a particular coin is flipped.  The experiment is to flip the coin 30 times and observe the number of heads. The likelihood is then a binomial distribution. The prior is assumed to be a Beta distribution.")
                                         )
                                  ),
                                  
                                  column(6,
                                         #How to use the app
                                         h1("How to use the app?"),
                                         #box to contain description
                                         box(background="red",width=12,
                                             h4("The controls for the app are located to the left and the visualizations are available on the right."),
                                             h4("To change the number of successes observed (for example the number of coins landing head side up), the slider on the top left can be used."),
                                             h4("To change the prior distribution, the hyperparameters can be set using the input boxes on the left.  The changes in this distribution can be seen on the first graph."),
                                             h4("The resulting changes to the posterior distribution can be seen on the second graph.")
                                         )
                                  )
                                )
                        ),
                        
#------------------------------------------------------------------------------------------------------------                        
#-------------------------------Data page-------------------------------------------------------------------- 
#------------------------------------------------------------------------------------------------------------        

                        # Data page - scroll through data, subset the data, and export the data as a file
                        tabItem(tabName = "data", 
                                h2(strong("Kepler Objects of Interest (KOI)")),
                                fluidRow(infoBox(nrow(dataKOI), "Observations", icon = icon("eye"), width = 3),
                                         infoBox(nrow(filter(dataKOI, koi_disposition == "CONFIRMED")), "Confirmed", icon = icon("check"), color = "green", width = 3),
                                         infoBox(nrow(filter(dataKOI, koi_disposition == "CANDIDATE")), "Candidate(s)", icon = icon("question"), color = "yellow", width = 3),
                                         infoBox(nrow(filter(dataKOI, koi_disposition == "FALSE POSITIVE")), "False Positive", icon = icon("times-circle"), color = "red", width = 3)
                                ),
                                selectInput("select", "Select columns to display", names(dataKOI), multiple = TRUE),
                                DTOutput("tableKOI")
                                ),

#------------------------------------------------------------------------------------------------------------                        
#--------------------------------Data exploration------------------------------------------------------------ 
#------------------------------------------------------------------------------------------------------------  
                        # Data exploration - create numerical and graphical summaries, change the type of plot and type of summary reported,
                        # change the variables and filter the rows to change the data in the plots/summaries
                        tabItem(tabName = "exploration",
                                tabsetPanel(id = "explorationTabSet",
                                            tabPanel("Numerical summaries",
                                                     # Layout inspired by Radiant - https://github.com/radiant-rstats
                                                     column(width = 4,
                                                            box(width = 12,
                                                                verbatimTextOutput('out3'),
                                                                # Subset data set to include only numeric variables
                                                                selectInput("numColInput", "Numeric variable(s)", colnames(defaultValKOI), multiple=TRUE, selectize=FALSE, selected = c("koi_period", "koi_duration", "koi_depth", "koi_prad", "koi_teq")),
                                                                selectInput("applyFuncInput", "Apply function(s)", c("mean", "median", "min", "max", "sd", "var", "sum"), multiple=TRUE, selectize=TRUE, selected = c("mean", "min", "max", "sd")),
                                                                numericInput("roundDigitsInput", label = "Decimals", value = 1, max = 6)
                                                                )),
                                                     column(width = 8,
                                                            box(width = 12, DTOutput("summaryTable")))
                                                     
                                            ),
                                            tabPanel("Graphical summaries",
                                fluidRow(column(width = 4,
                                                box(width = 12,
                                                    selectInput("selectPlotInput", label = "Plot type", 
                                                                choices = c("Distribution", "Density", "Scatter"), 
                                                                selected = "Distribution"),
                                                    conditionalPanel(condition = "input.selectPlotInput == 'Distribution'",
                                                                     selectInput("distributionXInput", "x variable", colnames(select_if(defaultValKOI, is.numeric)), multiple=TRUE, selectize=FALSE, selected = "koi_disposition"),
                                                                     sliderInput("numBinsInput", "Number of bins",
                                                                                 min = 5, max = 50, value = 10, step = 5)))
                                                
                                                ),
                                         column(width = 8,
                                                box(width = 12, plotOutput("summaryPlot")))
                                         ),                     

                                fluidRow(
                                  column(width = 4, tabBox(id = "plotTabs", width = 12,
                                                           tabPanel("Scatter",
                                                                    h4(strong("Scatter plot options")),
                                                                    br(),
                                                                    # Checkbox to label excessively large planets
                                                                    checkboxInput("radialOutliers", h4("Label radial outliers", style = "color:black;")),
                                                                    
                                                                    # Only show if "Label radial outliers" is selected
                                                                    conditionalPanel(condition = "input.radialOutliers",
                                                                                     checkboxInput("candidatesOnly", h4("Label only 'CANDIDATE' objects?",
                                                                                                                        style = "color:black;")))
                                                           ),
                                                           tabPanel("Histogram",
                                                                    h4(strong("Histogram options")),
                                                                    br(),
                                                                    h5(strong("koi_teq:"), "Equilibrium Temperature (Kelvin)"),
                                                                    h5(strong("koi_sma:"), "Orbit Semi-Major Axis (Astronomical Unit (au))"),
                                                                    br(),
                                                                    # Dropdown ("variables to summarize")
                                                                    selectInput(
                                                                      inputId = "summaryVariable",
                                                                      label = "Variables to Summarize",
                                                                      choices = c("koi_teq",
                                                                                  "koi_sma"),
                                                                      selected = "koi_score"
                                                                    ),
                                                                    checkboxInput("logAxis", h4("Use a logarithmic axis (log10)?", style = "color:black;")),
                                                                    sliderInput("binNumber", "Number of bins",
                                                                                min = 10, max = 75, value = 30, step = 5)
                                                                    
                                                           ),
                                                           tabPanel("Correlations",
                                                                    selectInput(inputId = "corrType", label = "Select a correlation plot",
                                                                                choices = c("Ranked cross-correlations",
                                                                                            "Correlation between variable and dataframe")),
                                                                    plotOutput("corrPlot")))),
                                  
                                  # Show outputs
                                  column(width = 8, 
                                         fluidRow(
                                           box(width = 12, plotOutput("finalPlot"))
                                         )
                                  ))))),
#------------------------------------------------------------------------------------------------------------                        
#-----------------------------------Data modeling------------------------------------------------------------ 
#------------------------------------------------------------------------------------------------------------                          
                        # Modeling page
                        tabItem(tabName = "modeling", 
                                tabsetPanel(id = "modelingTabSet",
                                  tabPanel("Information", 
                                           fluidRow(
                                             # Add in LaTeX functionality 
                                             withMathJax(),
                                             
                                             # Three columns for each of the two items
                                             column(4,
                                                    #Description of App
                                                    h1("Multiple linear regression"),
                                                    #box to contain description
                                                    box(background="light-blue",width=12,
                                                        h4("This application shows the relationship between the prior distribution and the posterior distribution for a simple Bayesian model."),
                                                        h4("The prior distribution is assumed to be a Beta distribution and the likelihood is a Binomial distribution with 30 trials (of which you can change the number of successes).  This yields a Beta distribution as the posterior. Note: As the prior distribution is in the same family as the posterior, we say the prior is conjugate for the likelihood."),
                                                        h4("This application corresponds to an example in ",span("Mathematical Statistics and Data Analysis",style = "font-style:italic"), "section 3.5, example E, by John Rice."),
                                                        h4("The goal of the example is to update our belief about the parameter \\(\\Theta\\) = the probability of obtaining a head when a particular coin is flipped.  The experiment is to flip the coin 30 times and observe the number of heads. The likelihood is then a binomial distribution. The prior is assumed to be a Beta distribution.")
                                                    )
                                             ),
                                             column(4,
                                                    #Description of App
                                                    h1("Classification tree"),
                                                    #box to contain description
                                                    box(background="blue",width=12,
                                                        h4("This application shows the relationship between the prior distribution and the posterior distribution for a simple Bayesian model."),
                                                        h4("The prior distribution is assumed to be a Beta distribution and the likelihood is a Binomial distribution with 30 trials (of which you can change the number of successes).  This yields a Beta distribution as the posterior. Note: As the prior distribution is in the same family as the posterior, we say the prior is conjugate for the likelihood."),
                                                        h4("This application corresponds to an example in ",span("Mathematical Statistics and Data Analysis",style = "font-style:italic"), "section 3.5, example E, by John Rice."),
                                                        h4("The goal of the example is to update our belief about the parameter \\(\\Theta\\) = the probability of obtaining a head when a particular coin is flipped.  The experiment is to flip the coin 30 times and observe the number of heads. The likelihood is then a binomial distribution. The prior is assumed to be a Beta distribution.")
                                                    )
                                             ),
                                             column(4,
                                                    #Description of App
                                                    h1("Random forest model"),
                                                    #box to contain description
                                                    box(background="aqua",width=12,
                                                        h4("This application shows the relationship between the prior distribution and the posterior distribution for a simple Bayesian model."),
                                                        h4("The prior distribution is assumed to be a Beta distribution and the likelihood is a Binomial distribution with 30 trials (of which you can change the number of successes).  This yields a Beta distribution as the posterior. Note: As the prior distribution is in the same family as the posterior, we say the prior is conjugate for the likelihood."),
                                                        h4("This application corresponds to an example in ",span("Mathematical Statistics and Data Analysis",style = "font-style:italic"), "section 3.5, example E, by John Rice."),
                                                        h4("The goal of the example is to update our belief about the parameter \\(\\Theta\\) = the probability of obtaining a head when a particular coin is flipped.  The experiment is to flip the coin 30 times and observe the number of heads. The likelihood is then a binomial distribution. The prior is assumed to be a Beta distribution.")
                                                    )
                                             ))),
                                  tabPanel("Fitting", verbatimTextOutput("summary"),
                                           # First column
                                           fluidRow(box(width = 4,
                                                        h3("Train/test data split"),
                                                        br(),
                                                        numericInput("percentInput", label = h4("Specify the percentage (%) of data to designate for training"), value = 80)
                                                        ),
                                                    box(width = 4,
                                                        h3("Select predictors"),
                                                        chooserInput("mychooser", "Available frobs", "Selected frobs",
                                                                     colnames(filteredKOI), c(), size = 10, multiple = TRUE
                                                        ),
                                                        verbatimTextOutput("selection")
                                                    ),
                                                    box(width = 4,
                                                        
                                                        )
                                                    ),
                                           fluidRow(
                                             column(width = 4, 
                                                           box(width = NULL, 
                                                               h3("Classification tree parameters"),
                                                               br()
                                                               ),
                                                    
                                             
                                                    tabBox(id = "classTabs", width = NULL,
                                                           tabPanel("Summary", verbatimTextOutput("classTree")),
                                                           tabPanel("Plot", plotOutput("rpartPlot")),
                                                           tabPanel("Accuracy (test data)", verbatimTextOutput("rfTestPredict")))),
                                             
                                           # Second column
                                             column(width = 4, 
                                                    box(width = NULL, 
                                                        h3("Random forest parameters"),
                                                        br()
                                                    ),
                                            
                                    
                                                    tabBox(id = "rfTabs", width = NULL,
                                                           tabPanel("Summary", verbatimTextOutput("rfSummary")),
                                                           tabPanel("Plot", plotOutput("rfPlot")),
                                                           tabPanel("Variable Importance", plotOutput("rfVarImp")),
                                                           tabPanel("Accuracy (test data)", verbatimTextOutput("rpartTestPredict")))),
                                           
                                           # Third Column
                                          
                                             column(width = 4, 
                                                    box(width = NULL, 
                                                        h3("Generalized linear regression parameters"),
                                                        br()
                                                    ),
                                             
                                             
                                                    tabBox(id = "rfTabs", width = NULL,
                                                           tabPanel("Summary", verbatimTextOutput("glmSummary")),
                                                           tabPanel("Plot", plotOutput("glmPlot")),
                                                           tabPanel("Accuracy (test data)", verbatimTextOutput("glmTestPredict"))))
                                           ),
                                           fluidRow(
                                             column(width = 4,
                                                    
                                                        # Cause all the inputs on the page to not send updates to the server until the button is pressed.
                                                        actionButton("fit", "Fit models")
                                                    )
                                             
                                           )
                                           ),
                                           
                                  tabPanel("Prediction", tableOutput("table"))
                                )), 
                        
                        # First tab content
                        tabItem(tabName = "references",
                                fluidRow(
                                  #add in latex functionality if needed
                                  withMathJax(),
                                  
                                  box(background="red",width=12,
                                      h4("Lissauer, J., Dawson, R. & Tremaine, S. Advances in exoplanet science from Kepler.", em("Nature"), "513, 336–344 (2014). https://doi-org.prox.lib.ncsu.edu/10.1038/nature13781")
                                  )
                                )
                                
                                
                        )
                      )
                    )
                    
)
#---------------------------------------------------------------------------------------------------------------------------------------
#
# Server logic
#
#---------------------------------------------------------------------------------------------------------------------------------------

# Define server logic required to draw the plots
server <- shinyServer(function(input, output) {
  
  #Create prior plot output
  output$priorPlot<-renderPlot({
    
    #Plotting sequence
    x <- seq(from=0,to=1,by=0.01)
    
    #get alpha and beta values from input
    alphaval<-input$alpha
    betaval<-input$beta
    
    #set defaults if not supplied
    if (is.na(alphaval)){alphaval<-1}
    if (is.na(betaval)){betaval<-1}
    
    #draw the prior distribution plot
    plot(x=x,y=dbeta(x=x,shape1=alphaval,shape2=betaval),main="Prior Density for Theta",xlab="theta's", ylab="f(theta)",type="l")
    
  })
  
  # Data subsetting
  getData <- reactive({
    
    columns = names(dataKOI)
    
    # Stores names of user-selected columns for subsetting in output$tableKOI
    if (!is.null(input$select)) {
      columns = input$select
    }
    dataKOI[,columns,drop=FALSE]
  })
  
  # Create table of observations    
  output$tableKOI <- DT::renderDT(server = FALSE, {
    # Include horizontal and vertical scrolling, render only visible portion of data,
    # include buttons for downloading in CSV and XLSX,
    # render DT in the client to allow all data or filtered to be downloaded.
    dtable <- datatable(getData(), extensions = c('Buttons', 'Scroller', 'Select', 'SearchBuilder'), selection = 'none',
                        options = list(scrollX = TRUE, 
                                       deferRender = TRUE,
                                       scrollY = 400,
                                       scroller = TRUE,
                                       dom = 'QlBfrtip',
                                       buttons = list('copy', list(extend = "collection",
                                                                   buttons = c("csv", "excel"), text = "Download")),
                                       searchBuilder = list(
                                         columns = 1:ncol(getData()) # Include all columns in custom seatch builder
                                         
                                       )))
    # Subset the data set using SearchBuilder implementation with CSS and JS files
    # https://www.datatables.net/extensions/searchbuilder/
    # https://stackoverflow.com/questions/64773579/how-to-implement-datatables-option-in-shiny-r-syntax
    dep <- htmlDependency(
      name = "searchBuilder",
      version = "1.0.0", 
      src = path_to_searchBuilder,
      script = "dataTables.searchBuilder.min.js",
      stylesheet = "searchBuilder.dataTables.min.css",
      all_files = FALSE
    )
    
    dtable$dependencies <- c(dtable$dependencies, list(dep))
    dtable
    
  })
  
  getDropdownChoice <- reactive({
    selectedVar <- input$summaryVariable
  })
  
  # Create orbital period/radius scatter plot 
  output$finalPlot <- renderPlot({

    
    if(input$plotTabs == "Scatter"){
      
      # Use LaTeX to denote the standard astronomical symbol for the Earth
      periodRadScatter <- ggplot(defaultValKOI, aes(x = koi_period, y = koi_prad)) +
        geom_point(aes(color = koi_disposition), 
                   alpha = 0.6, position = "jitter") +
        labs(x = "Orbital period (days)", y = TeX(r'(Planet mass $(M_{E})$)'),
             title = "Orbital period versus planetary radius", col = "Disposition") +
        scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                      labels = scales::trans_format("log10", scales::math_format(10^.x)), limits = c(10^0, 10^3)) +
        scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                      labels = scales::trans_format("log10", scales::math_format(10^.x)), limits = c(10^0, 10^4)) 
      
      # Label excessively large KOIs, eliminate overlapping labels
      # using geom_text_repel from the ggrepel package.
      # Examples provided at https://ggrepel.slowkow.com/articles/examples.html 
      if(input$radialOutliers){
        # Label only "CANDIDATE" objects exceeding 500 Earth radii
        if(input$candidatesOnly){
          periodRadScatter + geom_text_repel(aes(label = ifelse(koi_prad >= 500 & koi_disposition == "CANDIDATE", kepoi_name,'')), point.padding = 0.2,    
                                             nudge_x = .15,
                                             nudge_y = .5,
                                             arrow = arrow(length = unit(0.02, "npc")),
                                             segment.curvature = -1e-20,
                                             segment.linetype = 6)
          
        } else {
          # Label all objects exceeding R = 2000
          periodRadScatter + geom_text_repel(aes(label = ifelse(koi_prad >= 2000, kepoi_name,'')), point.padding = 0.2,    
                                             nudge_x = .15,
                                             nudge_y = .5,
                                             arrow = arrow(length = unit(0.02, "npc")),
                                             segment.curvature = -1e-20,
                                             segment.linetype = 6)
        }
      } else {
        periodRadScatter
      }
      
    } else if(input$plotTabs == "Histogram"){
      
      # Get user var selection for histogram
      selectedVar <- getDropdownChoice()
      
      # Plot histogram
      histo <- ggplot(dataKOI, aes_string(x = selectedVar)) +
        geom_histogram(aes(y = ..density..), color = "#e9ecef", fill = "#69b3a2", bins = input$binNumber) +
        labs(x = selectedVar) +
        geom_density(adjust = 0.5, alpha = 0.5)
      
      # If checkbox is selected, use a log10 scle for x-axis
      if(input$logAxis){
        histo + scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                              labels = scales::trans_format("log10", scales::math_format(10^.x)))
      } else {
        histo
      }

    
    }
    
  })
  
  getCorrChoice <- reactive({
    corrDropdownChoice <- input$corrType
    
  })
  
  output$corrPlot <- renderPlot({
    if(getCorrChoice() == "Ranked cross-correlations") {
    
    corr_cross(defaultValKOI, # name of dataset
               max_pvalue = 0.05, # display only significant correlations (at 5% level)
               top = 10 # display top 10 couples of variables (by correlation coefficient)
    )
    } else {
      
      corr_var(defaultValKOI, # name of dataset
               koi_disposition_binary,
               top = 10 # display top 10 couples of variables (by correlation coefficient)
      )
               
    }
  })
  
  # Columns for numeric summaries
  chosenCol <- reactive({
    colsChosen <- input$numColInput
    colsChosen
  })

  # Summaries for numeric summaries
  chosenSummaries <- reactive({
    summaries <- input$applyFuncInput
    summaries
  })
  
  numDecimals <- reactive({
    decimals <- input$roundDigitsInput
    decimals
  })
  
  # Table for numeric summaries   
  output$summaryTable <- DT::renderDT({
    
    koiDT <- data.table()
    
    for (i in chosenSummaries()) {
      koiDT <- rbind(koiDT, filteredKOI[ , lapply(.SD, i), .SDcols = chosenCol()])
    }
    
    # Transpose in order to append new vars vertically and apply rounding
    koiDT <- t(round(koiDT, numDecimals()))
    
    # Output final table, ensure column names atch summary type
    summTable <- datatable(koiDT, colnames = chosenSummaries())
    summTable
  })

  distributionVars <- reactive({
    distVars <- input$distributionXInput
    distVars
  })
  
  distributionBins <- reactive({
    distBins <- input$numBinsInput
    distBins
  })
  
  # Create summary plots
  output$summaryPlot <- renderPlot({
    
    if(input$selectPlotInput == "Distribution"){
      g <- ggplot(filteredKOI, aes(x = distributionVars())) +
        geom_histogram(bins = distributionBins())
    }
    
    g
    
  })

#------------------------------------------------------------------------------------------
# --------------------------------Modeling-------------------------------------------------
#------------------------------------------------------------------------------------------

  # Do not fit models until actionButton is clicked
  splitVals <- observeEvent(input$fit, {
    
    # Render caret formula
    output$selection <- renderPrint({
      predictorList <- as.formula(paste0("koi_disposition_binary ~ ", paste0(input$mychooser$right, collapse="+")))
      predictorList
    })
    
  # Formula for models
  predictorList <- as.formula(paste0("koi_disposition_binary ~ ", paste0(input$mychooser$right, collapse="+")))

  
  # Use user input to split filtered data into training and test sets
  dataSplit <- reactive({
    set.seed(100)
    
    dataIndex <- createDataPartition(filteredKOI$koi_disposition_binary, p = (input$percentInput/100), list = FALSE)
    
  })
  
  dataTrain <- reactive({
    dataTrain <- filteredKOI[dataSplit(),]
  })
  
  dataTest <- reactive({
    dataTest <- filteredKOI[-dataSplit(),]
  })
  


  
#--------------------------------------------------------------------------
# Generalized linear regression
#--------------------------------------------------------------------------    
  # Create generalized linear regression model
  glmTrain <- reactive({
    glmFit <- train(predictorList, 
                      data = dataTrain(),
                      method = "glmnet",
                      preProcess = c("center", "scale"),
                      trControl = trainControl(method = "cv", number = 5))
    glmFit
  })
  
  # Output generalized linear regression summary
  output$glmSummary <- renderPrint({
    summary(glmTrain())
    #confusionMatrix(predict(rpartTrain(), dataTest()$koi_disposition_binary), dataTest()$koi_disposition_binary$y)$overall["Accuracy"]
  })
  
  # Output generalized linear regression plot
  output$glmPlot <- renderPlot({
    plot(glmTrain())
  })  
  
  # The models should be compared on the test set and appropriate fit statistics reported.
  output$glmTestPredict <- renderPrint({
    predictGLM <- predict(glmTrain(), dataTest())
    glmRMSE <- postResample(predictGLM, obs = dataTest()$koi_disposition_binary)
    glmRMSE
  })
  
#--------------------------------------------------------------------------
# Classification tree
#--------------------------------------------------------------------------
  # Create classification model
  rpartTrain <- reactive({
    rpartFit <- train(predictorList,
                   data = dataTrain(),
                   method = "rpart",
                   preProcess = c("center", "scale"),
                   trControl = trainControl(method = "cv", number = 5))
    rpartFit
  })
  
  # Output classification tree summary
  output$classTree <- renderPrint({
    rpartTrain()
    #confusionMatrix(predict(rpartTrain(), dataTest()$koi_disposition_binary), dataTest()$koi_disposition_binary$y)$overall["Accuracy"]
  })
  
  # Output classification tree plot
  output$rpartPlot <- renderPlot({
    plot(rpartTrain())
  })
  
  # The models should be compared on the test set and appropriate fit statistics reported.
  output$rpartTestPredict <- renderPrint({
    predictRPART <- predict(rpartTrain(), dataTest())
    rpartRMSE <- postResample(predictRPART, obs = dataTest()$koi_disposition_binary)
    rpartRMSE
  })

#--------------------------------------------------------------------------
# Random forest
#--------------------------------------------------------------------------  

  # Create random forest model
  rfTrain <- reactive({
    rfFit <- train(predictorList,
                      data = dataTrain(),
                      method = "rf",
                      preProcess = c("center", "scale"),
                      trControl = trainControl(method = "cv", number = 5))
    rfFit
  })
  

  
  # Output random forest summary
  output$rfSummary <- renderPrint({
    rfTrain()
  })
  
  # Output random forest plot
  output$rfPlot <- renderPlot({
    plot(rfTrain())
  })
  
  # A plot showing the variable importance from the random forest model
  output$rfVarImp <- renderPlot({
    rfImp <- varImp(rfTrain(), scale = FALSE)
    plot(rfImp, top = 20)
  })

  # The models should be compared on the test set and appropriate fit statistics reported.
  output$rfTestPredict <- renderPrint({
    predictRF <- predict(rfTrain(), dataTest())
    rfRMSE <- postResample(predictRF, obs = dataTest()$koi_disposition_binary)
    rfRMSE
  })
  
  })
  
})

shinyApp(ui = ui, server = server)
