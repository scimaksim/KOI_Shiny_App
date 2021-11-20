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
                                            # -------------Numerical summaries--------------
                                            #-----------------------------------------------
                                            
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
                                            
                                            # -------------Graphical summaries--------------
                                            #-----------------------------------------------
                                            
                                            tabPanel("Graphical summaries",
                                                     fluidRow(column(width = 4,
                                                                     box(width = 12,
                                                                         selectInput("selectPlotInput", label = "Plot type", 
                                                                                     choices = c("Distribution", "Density", "Scatter"), 
                                                                                     selected = "Distribution"),
                                                                         # Keep same variable list for distribution and density plots, but alternate between number of bins/smooth options
                                                                         conditionalPanel(condition = "input.selectPlotInput == 'Distribution' || input.selectPlotInput == 'Density'",
                                                                                          selectInput("distributionXInput", "x variable", colnames(select_if(defaultValKOI, is.numeric)), multiple=TRUE, selectize=FALSE, selected = "koi_period"),
                                                                                          conditionalPanel(condition = "input.selectPlotInput == 'Distribution'",
                                                                                                           sliderInput("numBinsInput", "Number of bins",
                                                                                                                       min = 5, max = 50, value = 10, step = 5)),
                                                                                          conditionalPanel(condition = "input.selectPlotInput == 'Density'",
                                                                                                           sliderInput("widthBinsInput", "Smooth", 
                                                                                                                       min = 0.1, max = 3, value = 1, step = 0.1))
                                                                                          ),
                                                                         conditionalPanel(condition = "input.selectPlotInput == 'Scatter'",
                                                                                          selectInput("distributionXInput", "x variable", colnames(select_if(defaultValKOI, is.numeric)), multiple=TRUE, selectize=FALSE, selected = "koi_period"),
                                                                                          selectInput("distributionYInput", "y variable", colnames(select_if(defaultValKOI, is.numeric)), multiple=TRUE, selectize=FALSE, selected = "koi_prad"),
                                                                                          checkboxGroupInput("scatterCheckGroup", label = "Plot options", 
                                                                                                             choices = list("Log X" = 1, "Log Y" = 2),
                                                                                                             selected = c(1,2)),
                                                                                          selectInput("scatterColorVar", "Color", colnames(defaultValKOI), multiple=FALSE, selectize=FALSE, selected = "koi_disposition"),
                                                                                          )
                                                     )),
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

