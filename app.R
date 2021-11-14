library(shiny)
library(shinydashboard)
library(DT)
library(htmltools)

ui <- dashboardPage(skin="blue",
                    
                    #add title
                    dashboardHeader(title="NASA - Kepler Objects of Interest",titleWidth=1000),
                    
                    #define sidebar items
                    dashboardSidebar(sidebarMenu(
                      menuItem("About", tabName = "about", icon = icon("info")),
                      menuItem("Data", tabName = "data", icon = icon("table")),
                      menuItem("Data Exploration", tabName = "exploration", icon = icon("chart-line")),
                      menuItem("Modeling", tabName = "modeling", icon = icon("laptop-code")),
                      menuItem("Application", tabName = "app", icon = icon("laptop"))
                    )),
                    
                    #define the body of the app
                    dashboardBody(
                      tabItems(
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
                        
                        #actual app layout      
                        tabItem(tabName = "app",
                                fluidRow(
                                  column(width=3,
                                         box(width=12,background="red",sliderInput("yvalue","Y=Number of Successes",min = 0,max = 30,value = 15)
                                         ),
                                         box(width=12,
                                             title="Hyperparameters of the prior distribution for \\(\\Theta\\)",
                                             background="red",
                                             solidHeader=TRUE,
                                             p("\\(\\frac{\\Gamma(\\alpha+\\beta)}{\\Gamma(\\alpha)\\Gamma(\\beta)}\\theta^{\\alpha-1}(1-\\theta)^{\\beta-1}\\)"),
                                             h5("(Set to 1 if blank.)"),
                                             numericInput("alpha",label=h5("\\(\\alpha\\) Value (> 0)"),value=1,min=0,step=0.1),
                                             numericInput("beta",label=h5("\\(\\beta\\) Value (> 0)"),value=1,min=0,step=0.1)
                                         )
                                  ),
                                  column(width=9,
                                         fluidRow(
                                           box(width=6,
                                               plotOutput("priorPlot"),
                                               br(),
                                               h4("Prior distribution for the probability of success parameter \\(\\Theta\\).")
                                           ),
                                           box(width=6,
                                               plotOutput("distPlot"),
                                               br(),
                                               h4("Posterior distribution for the probability of success \\(\\Theta\\).")
                                           )
                                         )
                                  )
                                )
                        ),
                        
                        tabItem(tabName = "data", 
                                h2(strong("Kepler Objects of Interest (KOI)")),
                                selectInput("select", "Select columns to display", names(dataKOI), multiple = TRUE),
                                dataTableOutput("tableKOI"))
                      )
                    )
)

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

  #create posterior plot  
  output$distPlot <- renderPlot({

    #Plotting sequence
    x    <- seq(from=0,to=1,by=0.01)
    
    #number of success from input slider
    numsuccess <- input$yvalue
    
    #get alpha and beta values from input
    alphaval<-input$alpha
    betaval<-input$beta
    
    #sample size
    n<-30

    #set defaults if not supplied
    if (is.na(alphaval)){alphaval<-1}
    if (is.na(betaval)){betaval<-1}
        
    # draw the posterior
    plot(x=x,y=dbeta(x=x,shape1=numsuccess+alphaval,shape2=n-numsuccess+betaval),main=paste("Posterior Density for Theta|Y=",numsuccess,sep=""),xlab="theta's", ylab="f(theta|y)",type="l")
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
  output$tableKOI <- renderDataTable(server = FALSE, {
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

})

shinyApp(ui = ui, server = server)
