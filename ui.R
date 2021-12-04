# KOI Exploration
# Authored by Maksim Nikiforov
# NCSU ST 558 - Fall, 2021

# Requisite libraries
library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(DT)
library(htmltools)
library(latex2exp)
library(tidyverse)
library(ggrepel)
library(grid)
library(caret)
library(lares)
library(data.table)
library(rpart.plot)
library(randomForest)
library(glmnet)
library(ggplot2)
library(Cairo)
library(ggcorrplot)


# Custom Shiny input binding for selecting model predictors, sourced from
# https://github.com/rstudio/shiny-examples/tree/main/036-custom-input-control
source("chooser.R")

ui <- dashboardPage(#skin="blue",
  
  # Title
  dashboardHeader(title = strong("NASA - Kepler Objects of Interest"), titleWidth=1000),
  
  # Sidebar items
  dashboardSidebar(width = 170, sidebarMenu(
    menuItem("About", tabName = "about", icon = icon("info")),
    menuItem("Data", tabName = "data", icon = icon("table")),
    menuItem("Data Exploration", tabName = "exploration", icon = icon("chart-line")),
    menuItem("Modeling", tabName = "modeling", icon = icon("laptop-code")),
    menuItem("References", tabName = "references", icon = icon("book"))
  )),
  
  # Body of the app
  dashboardBody(
    
    # Theme sourced from https://github.com/nik01010/dashboardthemes
    shinyDashboardThemes(
      theme = "poor_mans_flatly"
    ),
    
    tabItems(
      #------------------------------------------------------------------------------------------------------------                        
      #-------------------------------------------"About" page-----------------------------------------------------
      #------------------------------------------------------------------------------------------------------------                          
      # First tab content
      tabItem(tabName = "about",
              fluidRow(
                # Add LaTeX functionality
                withMathJax(),
                
                # First of two columns on page
                column(6,
                       # App description
                       h1("What does this app do?"),
                       box(#background="blue",
                         width=12,
                         h4("NASA's Kepler space telescope launched in 2009 'to search for Earth-sized and smaller planets in the habitable zone of other stars in our neighborhood of the galaxy'. This application explores some of its vetted findings - the so-called 'Kepler Objects of Interest' (KOIs)."),
                         h4("The source of the data is the ", span("cumulative",style = "font-style:italic"), " Kepler Objects of Interest Table in the ", tags$a(href="https://exoplanetarchive.ipac.caltech.edu/index.html", "NASA Exoplanet Archive."), "Its intent is to 'provide the most accurate dispositions and stellar and planetary information for all KOIs in one place'. Additional information is available ", tags$a(href="https://exoplanetarchive.ipac.caltech.edu/docs/PurposeOfKOITable.html#cumulative", "here.")),
                         h4("A description for each data column is ", tags$a(href="https://exoplanetarchive.ipac.caltech.edu/docs/API_kepcandidate_columns.html", "also available."), "Only default columns, designated with '†', are used in this application."),
                         h4("This research has made use of the NASA Exoplanet Archive, which is operated by the California Institute of Technology, under contract with the National Aeronautics and Space Administration under the Exoplanet Exploration Program."),
                         img(src='logos.png', height="75%", width="75%")
                       )
                ),
                
                column(6,
                       # How to use the app
                       h1("How to use the app?"),
                       box(#background="blue",
                         width=12,
                         h4("The controls for this application are located on the left."),
                         h4("The ", strong("Data"), " page allows users to scroll through the data set, subset the data, and save the (possibly subsetted) data as a CSV file."),
                         h4("The ", strong("Data Exploration"), " page allows users to create numerical and graphical summaries, change the type of plot and type of summary reported, and change the variables and filter the rows to change the data in the plots/summaries."),
                         h4("The ", strong("Modeling"), " page allows users to fit three supervised learning models. On this page, the ", strong("Modeling Info"), " tab explains the three modeling approaches.", "The ", strong("Model Fitting"), " tab allows users to split the data into training and test sets, to choose model settings, and to view corresponding model statistics.", "The ", strong("Prediction"), " tab gives users a way to use one of the models for prediction.")
                       )
                )
              )
      ),
      
      #------------------------------------------------------------------------------------------------------------                        
      #-------------------------------"Data" page------------------------------------------------------------------
      #------------------------------------------------------------------------------------------------------------        
      
      # Scroll through data, subset the data, and export the data as a file
      tabItem(tabName = "data", 
              h2(strong("Kepler Objects of Interest (KOI)")),
              # Colorful infoboxes with aggregate number of KOI in the data, number of "CONFIRMED/CANDIDATE/FALSE POSITIVE" objects
              fluidRow(infoBox(nrow(defaultValKOI), "Observations", icon = icon("eye"), width = 3),
                       infoBox(nrow(filter(defaultValKOI, koi_disposition == "CONFIRMED")), "Confirmed", icon = icon("check"), color = "green", width = 3),
                       infoBox(nrow(filter(defaultValKOI, koi_disposition == "CANDIDATE")), "Candidate(s)", icon = icon("question"), color = "yellow", width = 3),
                       infoBox(nrow(filter(defaultValKOI, koi_disposition == "FALSE POSITIVE")), "False Positive",icon = icon("times-circle"), color = "red", width = 3)     
              ),
              
              fluidRow(
                # Information boxes.
                # Color palette for background is available at https://www.color-hex.com/color-names.html
                column(width = 6,
                       p("This page presents set of default columns in the ", tags$a(href="https://exoplanetarchive.ipac.caltech.edu/docs/API_kepcandidate_columns.html", "Kepler Objects of Interest Table."), br(), br(),
                         "Subsequent tabs in this program reference the 'disposition' of observations in the data, which 'designates the most probable physical explanation of the KOI'. 
                                                                                                Note that 'the value of this flag may change over time as the evaluation of KOIs proceeds to deeper levels of analysis using Kepler time-series pixel and light curve data, or follow-up observations'.", br(), br(),
                         "A disposition score between 0 and 1, designated as ", strong("koi_score"), " in the data, 'indicates the confidence in the KOI disposition. For CANDIDATEs, a higher value indicates more confidence in its disposition, while for FALSE POSITIVEs, a higher value indicates less confidence in that disposition'.",
                         style="text-align:justify;color:black;background-color:OldLace;padding:15px;border-radius:10px")
                ),
                column(width = 6,
                       p("'A planetary candidate has passed all prior tests conducted to identify false positives, although this does not a priori mean that all possible tests have been conducted. 
                                           A future test may confirm this KOI as a false positive'.", style="text-align:justify;color:black;background-color:OldLace;padding:15px;border-radius:10px"),
                       p("A false positive has failed at least one of the tests described in ", tags$a(href="https://ui.adsabs.harvard.edu/abs/2013ApJS..204...24B/abstract", "Batalha et al. (2012)."), 
                         "False positives can occur when: 1) the KOI is in reality an eclipsing binary star, 2) the Kepler light curve is contaminated by a background eclipsing binary, 
                                           3) stellar variability is confused for coherent planetary transits, or 4) instrumental artifacts are confused for coherent planetary transits.' ",
                         style="text-align:justify;color:black;background-color:OldLace;padding:15px;border-radius:10px")
                )),
              br(),
              # Subset columns in data set
              selectInput("select", "Select columns to display", names(defaultValKOI), multiple = TRUE),
              # Output table of data
              DTOutput("tableKOI"),
              downloadButton("downloadFiltered", "Download CSV")
      ),
      
      #------------------------------------------------------------------------------------------------------------                        
      #--------------------------------"Data exploration" page-----------------------------------------------------
      #------------------------------------------------------------------------------------------------------------  
      # Create numerical and graphical summaries, change the type of plot and type of summary reported,
      # change the variables and filter the rows to change the data in the plots/summaries
      tabItem(tabName = "exploration",
              
              # Graphical summaries
              # Settings
              fluidRow(column(width = 3,
                              h3(strong("Graphical summaries"))
              )
              ),
              fluidRow(column(width = 3,
                              box(width = 12,
                                  selectInput("selectPlotInput", label = "Plot type", 
                                              choices = c("Histogram", "Density", "Scatter", "Correlation"), 
                                              selected = "Distribution"),
                                  # Keep same variable list for distribution and density plots, but alternate between number of bins/smooth options
                                  conditionalPanel(condition = "input.selectPlotInput == 'Histogram' || input.selectPlotInput == 'Density'",
                                                   selectInput("distributionXInput", "x variable", colnames(select_if(defaultValKOI, is.numeric)), multiple=TRUE, selectize=FALSE, selected = "koi_period")
                                  ),
                                  conditionalPanel(condition = "input.selectPlotInput == 'Histogram'",
                                                   sliderInput("numBinsInput", "Number of bins",
                                                               min = 5, max = 50, value = 10, step = 5),
                                                   checkboxGroupInput("distributionLogCheck", label = "Plot options", 
                                                                      choices = list("Logarithmic scale" = 1),
                                                                      selected = 1)
                                  ),
                                  conditionalPanel(condition = "input.selectPlotInput == 'Density'",
                                                   sliderInput("widthBinsInput", "Smooth", 
                                                               min = 0.1, max = 3, value = 1, step = 0.1),
                                                   checkboxGroupInput("densityLogCheck", label = "Plot options", 
                                                                      choices = list("Logarithmic scale" = 1),
                                                                      selected = 1)
                                  ),
                                  
                                  conditionalPanel(condition = "input.selectPlotInput == 'Scatter'",
                                                   selectInput("distributionXInput", "x variable", colnames(select_if(defaultValKOI, is.numeric)), multiple=TRUE, selectize=FALSE, selected = "koi_period"),
                                                   selectInput("distributionYInput", "y variable", colnames(select_if(defaultValKOI, is.numeric)), multiple=TRUE, selectize=FALSE, selected = "koi_prad"),
                                                   checkboxGroupInput("scatterCheckGroup", label = "Plot options", 
                                                                      choices = list("Log X" = 1, "Log Y" = 2), selected = c(1, 2)),
                                                   checkboxInput("colorScatter", label = "Color by...", value = TRUE),
                                                   conditionalPanel(condition = "input.colorScatter == 1",
                                                                    selectInput("scatterColorVar", "Color", colnames(defaultValKOI), multiple=FALSE, selectize=FALSE, selected = "koi_disposition")
                                                   )
                                  ),
                                  conditionalPanel(condition = "input.selectPlotInput == 'Correlation'",
                                                   radioButtons("corrTypeRadio", label = "Correlation plot",
                                                                choices = list("Single variable" = 1, "Ranked cross-correlations" = 2, "Corrolelogram" = 3), 
                                                                selected = 3),
                                                   numericInput("pValMax", label = "Significance level", value = 0.05, max = 0.1, step = 0.01),
                                                   conditionalPanel(condition = "input.corrTypeRadio == 1",
                                                                    selectInput('corrSingleVar', 'Variable', colnames(defaultValKOI), multiple=FALSE, selectize=FALSE, selected = "koi_prad")
                                                   ),
                                                   conditionalPanel(condition = "input.corrTypeRadio == 2",
                                                                    numericInput("corrVarCouples", label = "# of variable couples", value = 10, min = 2, max = 15)
                                                   ),
                                                   conditionalPanel(condition = "input.corrTypeRadio == 3",
                                                                    chooserInput("ggCorrVars", "Available frobs", "Selected frobs",
                                                                                 colnames(defaultValKOI), size = 9, multiple = TRUE, rightChoices = c("koi_disposition", "koi_period", "koi_duration", "koi_prad", "koi_teq", "koi_score", "koi_srad"))
                                                   )
                                                   
                                  )
                              )),
                       # Plot graphics
                       column(width = 9,
                              box(width = 12,
                                  conditionalPanel(condition = "input.selectPlotInput == 'Scatter'",
                                                   plotOutput("scatterPlot"),
                                                   br(),
                                                   verbatimTextOutput("scatterClickInfo")
                                  ),
                                  conditionalPanel(condition = "input.selectPlotInput == 'Density'",
                                                   plotOutput("densityPlot"),
                                                   br()
                                  ),
                                  conditionalPanel(condition = "input.selectPlotInput == 'Histogram'",
                                                   plotOutput("histogramPlot"),
                                                   br()
                                  ),
                                  conditionalPanel(condition = "input.selectPlotInput == 'Correlation'",
                                                   plotOutput("corrCrossPlot"),
                                                   br()
                                  )
                              )
                       )
              ),
              
              
              # Numerical summaries
              # Settings
              fluidRow(column(width = 3,
                              h3(strong("Numerical summaries"))
              )
              ),
              fluidRow(column(width = 3,
                              box(width = 12,
                                  verbatimTextOutput('out3'),
                                  # Subset data set to include only numeric variables
                                  h4("Select variables to summarize"),
                                  chooserInput("dataVarChooser", "Available frobs", "Selected frobs",
                                               colnames(filteredKOI), size = 15, multiple = TRUE, rightChoices = c("koi_period", "koi_duration", "koi_depth", "koi_prad", "koi_teq", "koi_steff", "koi_srad", "koi_kepmag")
                                  ),
                                  br(),
                                  selectInput("applyFuncInput", "Apply function(s)", c("mean", "median", "min", "max", "sd", "var", "sum"), multiple=TRUE, selectize=TRUE, selected = c("mean", "min", "max", "sd")),
                                  numericInput("roundDigitsInput", label = "Decimals", value = 1, max = 6)
                              )
              ),
              column(width = 9,
                     box(width = 12, DTOutput("summaryTable"))
              )
              )
              
              
      ),
      #------------------------------------------------------------------------------------------------------------                        
      #-----------------------------------Data modeling------------------------------------------------------------ 
      #------------------------------------------------------------------------------------------------------------                          
      # Modeling page
      tabItem(tabName = "modeling", 
              tabsetPanel(id = "modelingTabSet",
                          
                          # Descriptions of models
                          tabPanel("Information", 
                                   fluidRow(
                                     # Add in LaTeX functionality 
                                     withMathJax(),
                                     
                                     # Three columns for each of the two items
                                     column(4,
                                            #Description of generalized linear models
                                            h1("Generalized linear regression"),
                                            box(#background="olive",
                                              width=12,
                                              h4("Generalized linear models are useful for accommodating non-numeric responses (e.g. nominal, ordinal) and responses that may stem from non-normal distributions. Any distribution which falls into the exponential family of densities has a generalized linear model that may apply."),
                                              h4("Generally, we can look at a link function ", span("g",style = "font-style:italic"), " for different types of data such that $$g(u)=\\beta_{0}+\\beta_{1}x_{1}+\\beta_{2}x_{2}+...+\\beta_{p}x_{p}$$ where ", span("u",style = "font-style:italic"), "is the mean for the set of ", span("x",style = "font-style:italic"), " values used. In other words, the link function is converting the expected value of our response to a linear predictor scale."),
                                              h4("Each distribution in a generalized linear model has an associated link function. A handful of such functions are listed ", tags$a(href="https://en.wikipedia.org/wiki/Generalized_linear_model#Link_function", "here.", style = "color: red;")),
                                              h4("The downside of GLMs is that they may be difficult to interpret due to factors such as confounding.")
                                            )
                                     ),
                                     column(4,
                                            #Description of classification trees
                                            h1("Classification tree"),
                                            #box to contain description
                                            box(#background="blue",
                                              width=12,
                                              h4("With classification trees, we are predicting group membership (e.g. 'CONFIRMED' or 'FALSE POSITIVE' planets) rather than the value of a continuous variable."),
                                              h4("We scour our predictor space to find the optimal split, but we must first establish our definition of 'optimal'. Residual sums of squares, as measures of performance in a given region, are not ideal for classification."),
                                              h4("Instead, for a binary response, we use the Gini index, $$2p(1-p)$$ or deviance, $$-2plog(p)-2(1-p)log(1-p)$$ where ", span("p",style = "font-style:italic"), " is the probability of correct classification."),
                                              h4("Both measures are minimized when ", span("p",style = "font-style:italic"), " is near 0 or 1 and should be weighed based on the number of observations in a given node."),
                                              h4("Classification trees are simple to understand and their output is easy to interpret. Their predictors do not need to be scaled, and we do not need to specify interaction terms. On the other hand, small changes in data can vastly change a tree (high variance) and some form of pruning is usually necessary.")
                                            )
                                     ),
                                     column(4,
                                            #Description of random forest model
                                            h1("Random forest model"),
                                            #box to contain description
                                            box(#background="purple",
                                              width=12,
                                              h4("Random forests are an extension of bagging but, instead of including every predictor in each tree, we include only a random subset."),
                                              h4("The method still requires us to fit a tree for each of our bootstrap samples and average the results. If there is a strong predictor in our data set, every tree will likely use that predictor for its first split, making them highly correlated with one another. This is not ideal for reducing variation through averaging, since high reduction in variation requires that everything be independent of one another."),
                                              h4("Often, we can improve our test error rate by averaging our trees only after disaggregating them."),
                                              h4("How many predictors should we include when doing a particular tree fit? For classification trees, the rule of thumb is to set $$m = \\sqrt{p}$$ as our number of included predictors. For a regression model, use $$m = \\frac{p}{3}$$"),
                                              h4("In practice, we can fit trees for many different values of ", span("m",style = "font-style:italic"), " and select the best one based on the out-of-bag (OOB) error rate."),
                                              h4("Random forest models are advantageous in that, by using many decision trees, they tend to reduce overfitting and variance, thereby gaining in prediction accuracy. However, can no longer pinpoint which variable we are splitting upon, thereby losing interpretability.")
                                            )
                                     ))),
                          
                          # Model fitting tab
                          tabPanel("Fitting", verbatimTextOutput("summary"),
                                   # First column
                                   fluidRow(
                                     column(width = 3,
                                            
                                            box(width = 12,
                                                h3("Model settings"),
                                                numericInput("percentInput", label = h4("Specify the percentage (%) of data to designate for training"), value = 80),
                                                numericInput("foldInput", label = h4("k-fold cross validation (specify ", span("k",style = "font-style:italic"), ")"), value = 5),
                                                numericInput("seedInput", label = h4("Set a seed for reproducibility"), value = 1234),
                                            ),
                                            
                                            box(width = 12,
                                                h4("Classification tree parameters"),
                                                radioButtons("classTune", label = h4("Hyperparameter tuning"),
                                                             choices = list("Auto (tuneLength)" = 1, "Manual (tuneGrid)" = 2), 
                                                             selected = 1, inline = TRUE),
                                                conditionalPanel(condition = "input.classTune == 1",
                                                                 splitLayout(
                                                                   numericInput("classTuneLengthInput", label = "# of unique values to consider:", value = 3)
                                                                 )),
                                                conditionalPanel(condition = "input.classTune == 2",
                                                                 splitLayout(
                                                                   numericInput("complexityInput", label = "Complexity:", value = 0.05))
                                                ),
                                                h4("Classification tree predictors"),
                                                chooserInput("classPredictors", "Available frobs", "Selected frobs",
                                                             colnames(filteredKOI), size = 5, multiple = TRUE, rightChoices = c("koi_period", "koi_duration", "koi_prad", "koi_teq")
                                                ),
                                            ),
                                            
                                            box(width = 12,
                                                
                                                h4("Random forest parameters"),
                                                radioButtons("rfTune", label = h4("Hyperparameter tuning"),
                                                             choices = list("Auto (tuneLength)" = 1, "Manual (tuneGrid)" = 2), 
                                                             selected = 1, inline = TRUE),
                                                conditionalPanel(condition = "input.rfTune == 1",
                                                                 splitLayout(
                                                                   numericInput("rfTuneLengthInput", label = "# of unique values to consider:", value = 3)
                                                                 )),
                                                conditionalPanel(condition = "input.rfTune == 2",
                                                                 splitLayout(
                                                                   numericInput("mtryInput", label = "mtry:", value = 1),
                                                                   numericInput("nTreeInput", label = "# of trees:", value = 100),
                                                                 )),
                                                h4("Random forest predictors"),
                                                chooserInput("rfPredictors", "Available frobs", "Selected frobs",
                                                             colnames(filteredKOI), size = 5, multiple = TRUE, rightChoices = c("koi_period", "koi_duration", "koi_prad", "koi_teq")
                                                ),
                                            ),
                                            
                                            box(width = 12,
                                                
                                                h4("Generalized linear regression parameters"),
                                                h4("GLM predictors"),
                                                chooserInput("glmPredictors", "Available frobs", "Selected frobs",
                                                             leftChoices = colnames(filteredKOI), size = 5, multiple = TRUE, rightChoices = c("koi_period", "koi_duration", "koi_prad", "koi_teq")
                                                )
                                            ),
                                            
                                            # Cause all the inputs on the page to not send updates to the server until the button is pressed.
                                            actionButton("fit", "Fit models")
                                     ),
                                     
                                     
                                     column(width = 9, 
                                            
                                            box(width = NULL,
                                                h3("Classification tree model"),
                                                tabBox(id = "classTabs", width = NULL,
                                                       tabPanel("Summary (training data)", verbatimTextOutput("classTree")),
                                                       tabPanel("Plot", plotOutput("rpartPlot")),
                                                       tabPanel("Decision tree", plotOutput("rpartDecisionTree")),
                                                       tabPanel("Accuracy (test data)", verbatimTextOutput("rfTestPredict"))
                                                )),
                                            
                                            
                                            box(width = NULL,
                                                h3("Random forest model"),
                                                tabBox(id = "rfTabs", width = NULL,
                                                       tabPanel("Summary (training data)", verbatimTextOutput("rfSummary")),
                                                       tabPanel("Plot", plotOutput("rfPlot")),
                                                       tabPanel("Variable Importance", plotOutput("rfVarImp")),
                                                       tabPanel("Accuracy (test data)", verbatimTextOutput("rpartTestPredict")))),
                                            
                                            box(width = NULL, 
                                                h3("Generalized linear model"),
                                                tabBox(id = "rfTabs", width = NULL,
                                                       tabPanel("Summary (training data)", verbatimTextOutput("glmSummary")),
                                                       tabPanel("Plot", plotOutput("glmPlot")),
                                                       tabPanel("Accuracy (test data)", verbatimTextOutput("glmTestPredict"))))
                                     )
                                   )),
                          
                          
                          tabPanel("Prediction", 
                                   fluidRow(
                                     column(width = 2,
                                            box(width = 12,
                                                selectInput("predictionModel", label = "Prediction model", 
                                                            choices = c("Generalized linear regression" = 1, "Classification tree" = 2, "Random forest" = 3), 
                                                            selected = 1),
                                                conditionalPanel(condition = "input.predictionModel == 1",
                                                                 uiOutput("glmPredictorInput"),
                                                                 br(),
                                                                 h4(strong("KOI disposition")),
                                                                 h4("Predicted probabilities"),
                                                                 h5("0 = 'FALSE POSITIVE'"),
                                                                 h5("1 = 'CONFIRMED'"),
                                                                 verbatimTextOutput("glmPrediction"),
                                                ),
                                                conditionalPanel(condition = "input.predictionModel == 2",
                                                                 uiOutput("classPredictorInput"),
                                                                 br(),
                                                                 h4(strong("KOI disposition")),
                                                                 h4("Predicted probabilities"),
                                                                 h5("0 = 'FALSE POSITIVE'"),
                                                                 h5("1 = 'CONFIRMED'"),
                                                                 verbatimTextOutput("classPrediction"),
                                                ),
                                                conditionalPanel(condition = "input.predictionModel == 3",
                                                                 uiOutput("rfPredictorInput"),
                                                                 br(),
                                                                 h4(strong("KOI disposition")),
                                                                 h4("Predicted probabilities"),
                                                                 h5("0 = 'FALSE POSITIVE'"),
                                                                 h5("1 = 'CONFIRMED'"),
                                                                 verbatimTextOutput("rfPrediction"),
                                                ),
                                                
                                                actionButton("predictButton", "Obtain prediction")
                                                
                                            )
                                            
                                            
                                     ),
                                     column(width = 10,
                                            h4(strong("CANDIDATE KOI observations")),
                                            h4("Predicted probabilities"),
                                            conditionalPanel(condition = "input.predictionModel == 1",
                                                             DTOutput("glmCandidatePredict")),
                                            conditionalPanel(condition = "input.predictionModel == 2",
                                                             DTOutput("classCandidatePredict")),
                                            conditionalPanel(condition = "input.predictionModel == 3",
                                                             DTOutput("rfCandidatePredict"))
                                     ),
                                   ))
                          
              )), 
      
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

