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

# install.packages("shiny", "shinydashboard", "DT", "htmltools", "latex2exp", "tidyverse", 
# "ggrepel", "grid", "caret", "lares", "data.table", "glmnet", "rpart", "randomForest")

# Custom Shiny input binding for selecting model predictors, sourced from
# https://github.com/rstudio/shiny-examples/tree/main/036-custom-input-control
source("chooser.R")

ui <- dashboardPage(skin="blue",
                    
                    #add title
                    dashboardHeader(title=strong("NASA - Kepler Objects of Interest"),titleWidth=1000),
                    
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
                                         box(background="blue",width=12,
                                             h4("This application explores the ", span("cumulative", style = "font-style:italic"), "database of Kepler Objects of Interest (KOI) in the NASA Exoplanet Archive. 'The intent of the cumulative table is to provide the most accurate dispositions and stellar and planetary information for all KOIs in one place.'"),
                                             h4("Kepler's was the 'first space mission to search for Earth-sized and smaller planets in the habitable zone of other stars in our neighborhood of the galaxy'. KOIs are 'well vetted, periodic, transit-like events in the Kepler data'."),
                                             h4("'The cumulative table is created algorithmically, following simple rules. The information for each KOI is pulled from the preferred activity table based on two priority lists. One priority list (Disposition Priority) indicates the activity table from which the disposition (e.g., CANDIDATE or FALSE POSITIVE) has been pulled. If the object is not dispositioned in the highest priority activity table for a specific KOI, then it is pulled from the next highest priority activity table, and so on. In this way the cumulative table contains the most current disposition for each KOI. The second priority list (Transit-Fit Priority) indicates where the remaining information for each KOI (e.g., the transit fits, stellar properties and vetting statistics) was obtained. The activity table with reliable transit fits to the longest data set is given priority for the cumulative table. This will not necessarily provide the best fit for every individual KOI, but gives the most reliable fits overall. The current Disposition Priority order is: Q1-Q17 DR 25 Supplemental, Q1-Q17 DR 25, Q1-Q17 DR 24, Q1-Q16, Q1-Q12, Q1-Q8, Q1-Q6. The current Transit-Fit Priority order is: Q1-Q17 DR 25, Q1-17 DR 24, Q1-Q16, Q1-Q12, Q1-Q8, Q1-Q6, and Q1-Q17 DR 25 Supplemental.

 Additional information is available '", tags$a(href="https://exoplanetarchive.ipac.caltech.edu/docs/PurposeOfKOITable.html#cumulative", "here.")),
                                             h4("A description for each data column is available ", tags$a(href="https://exoplanetarchive.ipac.caltech.edu/docs/API_kepcandidate_columns.html", "here.")),
                                             img(src='logos.png', height="75%", width="75%")
                                         )
                                  ),
                                  
                                  column(6,
                                         #How to use the app
                                         h1("How to use the app?"),
                                         #box to contain description
                                         box(background="blue",width=12,
                                             h4("The controls for this application are located on the left."),
                                             h4("The ", strong("Data"), " page allows users to scroll through the data set, subset the data, and save the (possibly subsetted) data as a CSV file."),
                                             h4("The ", strong("Data Exploration"), " page allows users to create numerical and graphical summaries, change the type of plot and type of summary reported, and change the variables and filter the rows to change the data in the plots/summaries."),
                                             h4("The ", strong("Modeling"), " page allows users to fit three supervised learning models. On this page, the ", strong("Modeling Info"), " tab explains the three modeling approaches.", "The ", strong("Model Fitting"), " tab allows users to split the data into training and test sets, to choose model settings, and to view corresponding model statistics.", "The ", strong("Prediction"), " tab gives users a way to use one of the models for prediction.")
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
                                                                                                             choices = list("Log X" = 1, "Log Y" = 2)),
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
                                                              #Description of generalized linear models
                                                              h1("Generalized linear regression"),
                                                              box(background="olive",width=12,
                                                                  h4("Generalized linear models are useful for accommodating non-numeric responses (e.g. nominal, ordinal) and responses that may stem from non-normal distributions. Any distribution which falls into the exponential family of densities has a generalized linear model that may apply."),
                                                                  h4("Generally, we can look at a link function ", span("g",style = "font-style:italic"), " for different types of data such that $$g(u)=\\beta_{0}+\\beta_{1}x_{1}+\\beta_{2}x_{2}+...+\\beta_{p}x_{p}$$ where ", span("u",style = "font-style:italic"), "is the mean for the set of ", span("x",style = "font-style:italic"), " values used. In other words, the link function is converting the expected value of our response to a linear predictor scale."),
                                                                  h4("Each distribution in a generalized linear model has an associated link function. A handful of such functions are listed ", tags$a(href="https://en.wikipedia.org/wiki/Generalized_linear_model#Link_function", "here.", style = "color: red;"))
                                                              )
                                                       ),
                                                       column(4,
                                                              #Description of classification trees
                                                              h1("Classification tree"),
                                                              #box to contain description
                                                              box(background="blue",width=12,
                                                                  h4("With classification trees, we are predicting group membership (e.g. 'CONFIRMED' or 'FALSE POSITIVE' planets ) rather than the value of a continuous variable."),
                                                                  h4("We scour our predictor space to find the optimal split, but we must first establish our definition of 'optimal'. Residual sums of squares, as measures of performance in a given region, are not ideal for classification."),
                                                                  h4("Instead, for a binary response, we use the Gini index, $$2p(1-p)$$ or deviance, $$-2plog(p)-2(1-p)log(1-p)$$ where ", span("p",style = "font-style:italic"), " is the probability of correct classification."),
                                                                  h4("Both measures are minimized when ", span("p",style = "font-style:italic"), " is near 0 or 1 and should be weighed based on the number of observations in a given node.")
                                                              )
                                                       ),
                                                       column(4,
                                                              #Description of random forest model
                                                              h1("Random forest model"),
                                                              #box to contain description
                                                              box(background="purple",width=12,
                                                                  h4("Random forests are an extension of bagging but, instead of including every predictor in each tree, we include only a random subset."),
                                                                  h4("The method still requires us to fit a tree for each of our bootstrap samples and average the results. If there is a strong predictor in our data set, every tree will likely use that predictor for its first split, making them highly correlated with one another. This is not ideal for reducing variation through averaging, since high reduction in variation requires that everything be independent of one another."),
                                                                  h4("Often, we can improve our test error rate by averaging our trees only after disaggregating them."),
                                                                  h4("How many predictors should we include when doing a particular tree fit? For classification trees, the rule of thumb is to set $$m = \\sqrt{p}$$ as our number of included predictors. For a regression model, use $$m = \\frac{p}{3}$$"),
                                                                  h4("In practice, we can fit trees for many different values of ", span("m",style = "font-style:italic"), " and select the best one based on the out-of-bag (OOB) error rate.")
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
                                                                  br(),
                                                                  splitLayout(
                                                                    numericInput("complexityInput", label = "Complexity:", value = 0.001)
                                                                  )
                                                              ),
                                                              
                                                              
                                                              tabBox(id = "classTabs", width = NULL,
                                                                     tabPanel("Summary", verbatimTextOutput("classTree")),
                                                                     tabPanel("Plot", plotOutput("rpartPlot")),
                                                                     tabPanel("Accuracy (test data)", verbatimTextOutput("rfTestPredict")))),
                                                       
                                                       # Second column
                                                       column(width = 4, 
                                                              box(width = NULL, 
                                                                  h3("Random forest parameters"),
                                                                  br(),
                                                                  splitLayout(
                                                                    numericInput("mtryInput", label = "mtry:", value = 1),
                                                                    numericInput("nTreeInput", label = "# of trees:", value = 100)
                                                                  )
                                                                  
                                                                    
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
                                            
                                            tabPanel("Prediction", 
                                                     fluidRow(
                                                       column(width = 4,
                                                              box(width = 12,
                                                                  selectInput("predictionModel", label = "Prediction model", 
                                                                              choices = c("Generalized linear regression" = 1, "Classification tree" = 2, "Random forest" = 3), 
                                                                              selected = 1)
                                                                  )
                                                              ),
                                                       column(width = 8,
                                                              tabsetPanel(id = "predictionTabSet",
                                                                          tabPanel("Raw predictions",
                                                                                   box(width = 12,
                                                                                       DTOutput("glmCandidatePredict")
                                                                                       )
                                                                                   ),
                                                                          tabPanel("Data table", DTOutput("confirmedTable"))
                                                                          )
                                                     ))
                                            
                                ))), 
                        
                        # References
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

