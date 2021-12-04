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
    
    columns = names(filteredKOI)
    
    # Stores names of user-selected columns for subsetting in output$tableKOI
    if (!is.null(input$select)) {
      columns = input$select
    }
    filteredKOI[,columns,drop=FALSE]
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
                                       buttons = list(list(extend = "collection",
                                                                   buttons = c("csv", "excel"), text = "Download")),
                                       searchBuilder = list(
                                         columns = 1:ncol(getData()) # Include all columns in custom search builder
                                         
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
    colsChosen <- input$dataVarChooser$right
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
    
    setDT(filteredKOI)
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
  
  scatterYVars <- reactive({
    yVars <- input$distributionYInput
    yVars
  })
  
  scatterColorVar <- reactive({
    colorVar <- input$scatterColorVar
    colorVar
  })
  
  scatterLogCheck <- reactive({
    logChoice <- input$scatterCheckGroup
    logChoice
  })
  
  distributionBins <- reactive({
    distBins <- input$numBinsInput
    distBins
  })
  
  densitySmooth <- reactive({
    densSmooth <- input$widthBinsInput
    densSmooth
  })
  
  # Create summary plots
  output$summaryPlot <- renderPlot({
    
    if (input$selectPlotInput == "Distribution") {
      
      g <- ggplot(defaultValKOI, aes_string(x = distributionVars())) +
        geom_histogram(bins = distributionBins())
      g
    } else if (input$selectPlotInput == "Density") {
      
      g <- ggplot(defaultValKOI, aes_string(x = distributionVars())) +
        geom_density(adjust = densitySmooth(), fill = "blue", alpha = 0.5)  
      g
    } else if (input$selectPlotInput == "Scatter") {
      
      g <- ggplot(defaultValKOI, aes_string(x = distributionVars(), y = scatterYVars())) +
        geom_point(aes_string(color = scatterColorVar()), 
                   alpha = 0.6, position = "jitter")
      
      # If "Log X" is selected  
      if (1 %in% scatterLogCheck()) {
        g <- g + scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                               labels = scales::trans_format("log10", scales::math_format(10^.x)), limits = c(10^0, 10^3))
      }
      
      # If "Log Y" is selected
      if (2 %in% scatterLogCheck()) {
        g <- g + scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                               labels = scales::trans_format("log10", scales::math_format(10^.x)), limits = c(10^0, 10^4))
      }
      
    }
    
    g
    
    
    
  })
  
  
  
  
  #-------------------------------------------------------------------------------------------------------------------------------------------  
  #-------------------------------------------------------------------------------------------------------------------------------------------  
  #-------------------------------------------------------------------------------------------------------------------------------------------  
  #-------------------------------------------------------------------------------------------------------------------------------------------  
  
  
  
  
  
  
  #------------------------------------------------------------------------------------------
  # --------------------------------Modeling-------------------------------------------------
  #------------------------------------------------------------------------------------------
  
  
  
  # Do not fit models until actionButton is clicked
  splitVals <- observeEvent(input$fit, {
    
    # Seed
    seedNum <- reactive({
      seedVal <- input$seedInput
      seedVal
    })
    
    # Number of folds in cross validation
    kFolds <- reactive({
      folds <- input$foldInput
      folds
    })
    
    set.seed(seedNum())
    
    # Render caret formula
    output$selection <- renderPrint({
      predictorList <- as.formula(paste0("koi_disposition_binary ~ ", paste0(input$classPredictors$right, collapse="+")))
      predictorList
    })
    
    # Formula for models
    classTreePredList <- as.formula(paste0("koi_disposition_binary ~ ", paste0(input$classPredictors$right, collapse="+")))
    rfPredList <- as.formula(paste0("koi_disposition_binary ~ ", paste0(input$rfPredictors$right, collapse="+")))
    glmPredList <- as.formula(paste0("koi_disposition_binary ~ ", paste0(input$glmPredictors$right, collapse="+")))
    
    
    # Use user input to split filtered data into training and test sets
    dataSplit <- reactive({
      
      
      dataIndex <- createDataPartition(filteredKOI$koi_disposition_binary, p = (input$percentInput/100), list = FALSE)
      
    })
    
    dataTrain <- reactive({
      dataTrain <- filteredKOI[dataSplit(),]
    })
    
    dataTest <- reactive({
      dataTest <- filteredKOI[-dataSplit(),]
    })
    
    # Prediction tab - react to model choice
    predictionModelSelect <- reactive({
      predModel <- input$predictionModel 
      predModel
    })
    
    
    
    
    #--------------------------------------------------------------------------
    # Generalized linear regression
    #--------------------------------------------------------------------------    
    
    
    # Create generalized linear regression model
    glmTrain <- reactive({
      glmFit <- train(glmPredList, 
                      data = dataTrain(),
                      method = "glmnet",
                      family = "binomial",
                      preProcess = c("center", "scale"),
                      trControl = trainControl(method = "cv", number = kFolds()))
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
    
    #------ Render data tables with predicted probabilities for 'CANDIDATE' objects-----------
    #-----------------------------------------------------------------------------------------
    
    # GLM
    output$glmCandidatePredict <- renderDT({
      df <- filteredCandidateKOI[, input$glmPredictors$right]
      predictCandidateGLM <- predict(glmTrain(), df, type = "prob")
      df <- data.frame(predictCandidateGLM, filteredCandidateKOI$kepid, filteredCandidateKOI$kepoi_name, df)
      df <- rename(df, c("FALSE_POS_prob" = "X0", "CANDIDATE_prob" = "X1"))
      datatable(df)
    })
    
    # Classification tree
    output$classCandidatePredict <- renderDT({
      df <- filteredCandidateKOI[, input$rfPredictors$right]
      predictCandidateGLM <- predict(rpartTrain(), df, type = "prob")
      df <- data.frame(predictCandidateGLM, filteredCandidateKOI$kepid, filteredCandidateKOI$kepoi_name, df)
      df <- rename(df, c("FALSE_POS_prob" = "X0", "CANDIDATE_prob" = "X1"))
      datatable(df)
    })
    
    # Random forest
    output$rfCandidatePredict <- renderDT({
      df <- filteredCandidateKOI[, input$rfPredictors$right]
      predictCandidateGLM <- predict(rfTrain(), df, type = "prob")
      df <- data.frame(predictCandidateGLM, filteredCandidateKOI$kepid, filteredCandidateKOI$kepoi_name, df)
      df <- rename(df, c("FALSE_POS_prob" = "X0", "CANDIDATE_prob" = "X1"))
      datatable(df)
    })
    
    
    
    
    
    #--------------------------------------------------------------------------
    # Classification tree
    #--------------------------------------------------------------------------
    
    complexityReactive <- reactive({
      cp <- expand.grid(cp = input$complexityInput)
      cp
    })
    
    # tuneLength - auto hyperparameter tuning
    classTuneLengthReactive <- reactive({
      classTuneLength <- input$classTuneLengthInput
      classTuneLength
    })
    
    
    if(input$classTune == 2) {
      # Create classification model
      rpartTrain <- reactive({
        rpartFit <- train(classTreePredList,
                          data = dataTrain(),
                          method = "rpart",
                          preProcess = c("center", "scale"),
                          tuneGrid = complexityReactive(),
                          trControl = trainControl(method = "cv", number = kFolds()))
        rpartFit
        
      })
    }
    
    else {
      # Create classification model
      rpartTrain <- reactive({
        rpartFit <- train(classTreePredList,
                          data = dataTrain(),
                          method = "rpart",
                          preProcess = c("center", "scale"),
                          tuneLength = classTuneLengthReactive(),
                          trControl = trainControl(method = "cv", number = kFolds()))
        rpartFit
        
        
        
      })
      
    }
    
    
    # Output decision tree plot
    output$rpartDecisionTree <- renderPlot({
      prp(rpartTrain()$finalModel, box.palette = "Reds", tweak = 1.2)
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
    # mtry - manual hyperparameter tuning (tuneGrid)
    mtryReactive <- reactive({
      mtry <- expand.grid(.mtry = input$mtryInput)
      mtry
    })
    
    # Number of trees
    ntreeReactive <- reactive({
      ntrees <- input$nTreeInput
      ntrees
    })
    
    # tuneLength - auto hyperparameter tuning
    rfTuneLengthReactive <- reactive({
      rfTuneLength <- input$rfTuneLengthInput
      rfTuneLength
    })
    
    
    # Manual hyperparameter tuning (tuneGrid)
    if(input$rfTune == 2) {
      
      # Create random forest model with tuneGrid
      rfTrain <- reactive({
        rfFit <- train(rfPredList,
                       data = dataTrain(),
                       method = "rf",
                       preProcess = c("center", "scale"),
                       tuneGrid = mtryReactive(),
                       trControl = trainControl(method = "cv", number = kFolds()),
                       ntree = ntreeReactive())
        rfFit
      }) }
    
    # Auto hyperparameter tuning (tuneLength)
    else {
      # Create random forest model with tuneLength
      rfTrain <- reactive({
        rfFit <- train(rfPredList,
                       data = dataTrain(),
                       method = "rf",
                       preProcess = c("center", "scale"),
                       tuneLength = rfTuneLengthReactive(),
                       trControl = trainControl(method = "cv", number = kFolds()),
                       ntree = ntreeReactive())
        rfFit
        
        
        
      })
    }
    
    
    
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
    
    
    #------------Prediction tab------------------------
    
    # References:
    # https://stackoverflow.com/questions/39135877/in-rshiny-ui-how-to-dynamic-show-several-numericinput-based-on-what-you-choose
    # https://stackoverflow.com/questions/50795355/how-to-extract-the-values-of-dynamically-generated-inputs-in-shiny
    
    # Customizable predictors based on GLM selections
    output$glmPredictorInput <- renderUI(
      
      lapply(1:length(input$glmPredictors$right),function(i){
        
        # Retrieve names of selected GLM predictors
        varName <- input$glmPredictors$right[i]
        
        # Retrieve sample values from first row of "CANDIDATE" observations
        varVal <- eval(parse(text = paste0("filteredCandidateKOI$", varName, "[1]"))) 
        
        # Create numericInput fields with sample values
        numericInput(inputId = paste0(input$glmPredictors$right[i], "_weight"), label = input$glmPredictors$right[i], value = varVal)
        
      })
    )
    
    # Customizable predictors based on selection in classification tree
    output$classPredictorInput <- renderUI(
      
      lapply(1:length(input$classPredictors$right),function(i){
        
        # Retrieve names of selected classification tree predictors
        varName <- input$classPredictors$right[i]
        
        # Retrieve sample values from first row of "CANDIDATE" observations
        varVal <- eval(parse(text = paste0("filteredCandidateKOI$", varName, "[1]"))) 
        
        # Create numericInput fields with sample values
        numericInput(inputId = paste0(input$classPredictors$right[i], "_weight"), label = input$classPredictors$right[i], value = varVal)
        
      })
    )
    
    # Customizable predictors based on random forest selection
    output$rfPredictorInput <- renderUI(
      
      lapply(1:length(input$rfPredictors$right),function(i){
        
        # Retrieve names of selected random forest predictors
        varName <- input$rfPredictors$right[i]
        
        # Retrieve sample values from first row of "CANDIDATE" observations
        varVal <- eval(parse(text = paste0("filteredCandidateKOI$", varName, "[1]"))) 
        
        # Create numericInput fields with sample values
        numericInput(inputId = paste0(input$rfPredictors$right[i], "_weight"), label = input$rfPredictors$right[i], value = varVal)
        
      })
    )
    
    modelPredictions <- observeEvent(input$predictButton, {
      
      # Predict using GLM
      output$glmPrediction <- renderPrint({
        # Obtain user's predictor values
        values = sapply(1:length(input$glmPredictors$right), function(i) {
          input[[ paste0(input$glmPredictors$right[i], "_weight") ]]
        })
        
        # Create a data frame using user's predictor values
        
        # Transpose values and names and turn into data frames
        valNum <- transpose(as.data.frame(unlist(values)))
        
        # Rename columns using predictor names
        valNum <- valNum %>% rename_at(vars(names(valNum)), ~input$glmPredictors$right)
        
        # Predict whether KOI should be marked as "CONFIRMED" (1) or "FALSE POSITIVE" (0)
        # and output result
        predictGLM <- predict(glmTrain(), valNum, type = "prob")
        predictGLM
      })
      
      # Predict using classification tree
      output$classPrediction <- renderPrint({
        # Obtain user's predictor values
        values = sapply(1:length(input$classPredictors$right), function(i) {
          input[[ paste0(input$classPredictors$right[i], "_weight") ]]
        })
        
        # Create a data frame using user's predictor values
        valDF <- transpose(as.data.frame(values))
        valDF <- setNames(valDF, input$classPredictors$right)
        
        # Predict whether KOI should be marked as "CONFIRMED"Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred (1) or "FALSE POSITIVE" (0)
        # and output result
        predictClass <- predict(rpartTrain(), valDF, type = "prob")
        predictClass
      })
      
      # Predict using random forest
      output$rfPrediction <- renderPrint({
        # Obtain user's predictor values
        values = sapply(1:length(input$rfPredictors$right), function(i) {
          input[[ paste0(input$rfPredictors$right[i], "_weight") ]]
        })
        
        # Create a data frame using user's predictor values
        valDF <- transpose(as.data.frame(values))
        valDF <- setNames(valDF, input$rfPredictors$right)
        
        # Predict whether KOI should be marked as "CONFIRMED" (1) or "FALSE POSITIVE" (0)
        # and output result
        predictRF <- predict(rfTrain(), valDF, type = "prob")
        predictRF
      })
      
    })
    
  })
  
  
  
  
  
})


