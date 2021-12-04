#---------------------------------------------------------------------------------------------------------------------------------------
#
# Server logic
#
#---------------------------------------------------------------------------------------------------------------------------------------


# Define server logic required to draw the plots
server <- shinyServer(function(input, output) {
  
  # Get column names for filtering data
  getData <- reactive({

    # Stores names of user-selected columns for subsetting in output$tableKOI
    if (!is.null(input$select)) {
      columnNames <- input$select
    } else {
      columnNames <- names(defaultValKOI)
    }
    #defaultValKOI[ , columnNames, drop=FALSE]
    columnNames
  })
  
  # Render a datatable of KOIs, consider column input from getData() above    
  output$tableKOI <- DT::renderDT(#server = FALSE, 
                                  {
    # Include horizontal and vertical scrolling, render only visible portion of data,
    # include buttons for downloading in CSV and XLSX,
    # render DT in the client to allow all data or filtered to be downloaded.
    datatable(defaultValKOI[, getData(), drop=FALSE], filter = 'top', extensions = c('Scroller'), 
                        options = list(
                                       scrollX = TRUE, 
                                       deferRender = TRUE,
                                       scrollY = 400,
                                       scroller = TRUE,
                                       dom = 'Qlfrtip')
              )
    # Subset the data set using SearchBuilder implementation with CSS and JS files
    # https://www.datatables.net/extensions/searchbuilder/
    # https://stackoverflow.com/questions/64773579/how-to-implement-datatables-option-in-shiny-r-syntax
    # dep <- htmlDependency(
    #   name = "searchBuilder",
    #   version =https://stackoverflow.com/questions/41597062/r-download-filtered-datatable "1.0.0", 
    #   src = path_to_searchBuilder,
    #   script = "dataTables.searchBuilder.min.js",
    #   stylesheet = "searchBuilder.dataTables.min.css",
    #   all_files = FALSE
    # )
    
    #dtable$dependencies <- c(dtable$dependencies, list(dep))

  })
  
  # Download filtered data set
  # Based on response from https://stackoverflow.com/questions/53499066/downloadhandler-with-filtered-data-in-shiny
  output$downloadFiltered <- downloadHandler(
    
    filename = function() {
      # File name
      paste('Filtered_KOI-', Sys.Date(), '.csv', sep = '')  
    },
    content = function(file) {
      # Filtered table
      write.csv(defaultValKOI[input[["tableKOI_rows_all"]], getData()],
                file= file,
                row.names=F)
    }
  )
  
  
  #-------------------- Create scatter plot and histogram--------------
  #--------------------------------------------------------------------
  
  # Create scatter plot
  output$scatterPlot <- renderPlot({
    scatterPlot <- ggplot(defaultValKOI, aes_string(x = distributionVars(), y = scatterYVars())) +
      geom_point(aes_string(color = scatterColorVar()), 
                 alpha = 0.6, position = "jitter")
    
    # If "Log X" is selected  
    if (1 %in% scatterLogCheck()) {
      scatterPlot <- scatterPlot + scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                             labels = scales::trans_format("log10", scales::math_format(10^.x)), limits = c(10^0, 10^3))
    }
    
    # If "Log Y" is selected
    if (2 %in% scatterLogCheck()) {
      scatterPlot <- scatterPlot + scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                             labels = scales::trans_format("log10", scales::math_format(10^.x)), limits = c(10^0, 10^4))
    }
    
    scatterPlot
    
  })
  
  # Find information about selected point in the scatter plot
  output$scatterClickInfo <- renderPrint({
    # Because it's a ggplot2, we don't need to supply xvar or yvar; if this
    # were a base graphics plot, we'd need those.
    nearPoints(defaultValKOI, input$scatterPlot_click)
  })
  
  
  # Identify which variable is selected for density plot
  densityVarChoice <- reactive({
    selectedVar <- input$summaryVariable
    selectedVar
  })
  
  # Distribution plot  
  output$distributionPlot <- renderPlot({
      
      # Get user var selection for histogram
      selectedVar <- densityVarChoice()
      
      # Plot histogram
      histo <- ggplot(defaultValKOI, aes_string(x = distributionVars())) +
        geom_histogram(bins = distributionBins())
      
      # If checkbox is selected, use a log10 scale for x-axis
      if(1 %in% input$distributionLogCheck){
        histo <- histo + scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                              labels = scales::trans_format("log10", scales::math_format(10^.x)))
        
        histo
        
      } else {
        
        histo
      }

    })
  
  # Density plot  
  output$densityPlot <- renderPlot({
      densityPlot <- ggplot(defaultValKOI, aes_string(x = distributionVars())) +
        geom_density(adjust = densitySmooth(), fill = "blue", alpha = 0.5)  
      
      if(1 %in% input$densityLogCheck){
        densityPlot <- densityPlot + scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                                                   labels = scales::trans_format("log10", scales::math_format(10^.x)))
        densityPlot
      } else {
        
      densityPlot
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


