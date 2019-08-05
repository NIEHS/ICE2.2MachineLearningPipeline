####functions
# Name:  MLCreate()
# Author: ILS (comptox@ils-inc.com)
# Date:19 June, 2019
# Version: 1.0
# License: MIT
# Summary: Provide information about data processing.Takes the input dataset and applies a list of machine learning models specified in MLmethods to evaulate the performance of the models
#                and writes data to disk. This is the combined function of DataSummaryWriteToDisk and MLCreate  -- JAA 6/18/2019
# called by: main program
# calls:     ModelFit()

# INPUTS
# data:                  original dataframe from ICE server. input data, must contain a column called "toPredict" that contains the values to be predicted
#
# dataAfterMunging       dataframe after removing 1) rows and columns with all NA, 2) columns with highly-correlated variables,
#                           3) columns with near-zero variance.  This dataframe has imputed data and is the data used
#                           for machine learning
# colRemoved             character string with names of columns (assays) removed
# User_Chem_list         This is the chemical list that the user choose in ICE interface
# Percent_chemical       character string showing the percentage of the chemicals from user's list that was retained
# chems_retained         character string with the casrn of the chemicals from the user's list that were retained
# DataMungingSummaryOut  argument for file name
#
# methods:               a list of machine learning methods
# type:                  "classification" or"regression", the type of ML to be performed
# trControlParams:       control parameters for model training
# metric:                the metric by which to evaluate the models. Can support list. see caret::train for more options
# PerformanceSummary:    true/false, do we want a summary of performance to be saved to working directory
# trainingData:          dataframe for ML training
# testingData:           dataframe for ML testing
# outfile:               file name for writing to disk.  Specified by  PerformanceWithTestDataOut <- args[8]


# Library required:



# FOR DEBUGGING
# data               <- modelData
# methods            <- MLmethods
# type               <- 'Regression'
# trControlParams    <- trn_ctl
# metric             <- 'RMSE'
# PerformanceSummary <- TRUE
# trainingData       <- trainingData
# testingData        <- testingData
# outfile            <- 'PerformanceWithTestDataOut'


MLCreate <- function (data= indata, User_Chem_list=Chemicals_in, ML_data=dataIn, dataAfterMunging=dataAfterMunging, methods = MLmethods, type="Classification", trControlParams = trn_ctl,
                      metric="ROC", PerformanceSummary=FALSE,
                      trainingData = trainingData, testingData = testingData, outfile=NULL,...){

  modelsFittedList <- lapply(1 : length(methods), function(i) {
    temp         <- ModelFit(methods[i], trainingData, trControlParams, metric = metric)
    # print(paste0(methods[i], " Fit created"), quote=FALSE)
    # print("-------------------------------------------")
    return(temp)
  }
  )

  # prints out performance of models to file
  if(PerformanceSummary == TRUE){
    predictionTestDataList <- lapply(1 : length(modelsFittedList), function(i) {
      temp               <- predict(modelsFittedList[[i]] , newdata = testingData)
      return(temp)
    }
    )

    if(is.null(outfile)){
      outfile <- "SummaryReport.txt"
    }

    # add the data summary
    capture.output(

      if (nrow(ML_data) == nrow(dataAfterMunging ) && ncol(ML_data) == ncol(dataAfterMunging ) ) {
        cat('No rows or columns removed due to insufficient data.',
            'Data for model construction has ', nrow(dataAfterMunging ), ' chemicals and ', ncol(dataAfterMunging) - 2, ' assays'
        )

      } else {

        dfInfoStr1 <- paste0('Data before processing had ', nrow(ML_data), ' chemicals and ', ncol(ML_data) - 2, ' assays. From those, '
                             , nrow(User_Chem_list), ' chemicals are provided by the user and the rest are from the seed list')
        dfInfoStr2 <- paste0('Data after processing for model construction has ', nrow(dataAfterMunging), ' chemicals and ', ncol(dataAfterMunging) - 2, ' assays')

        dfInfoStr3 <- paste0(ncol(ML_data) - ncol(dataAfterMunging),  ' assays removed with insufficient data leaving ',
                             ncol(dataAfterMunging) - 2, ' assays for model building')

        dfInfoStr4 <- paste0(nrow(dataIn) - nrow(dataAfterMunging ),  ' chemicals removed including ' ,nrow(User_Chem_list)-length(chems_retained),
                             ' chemicals from the user provided chemical list removed with insufficient data leaving '
                             ,nrow(dataAfterMunging ), ' chemicals for model building')

        dfInfoStr5a <- '\nAssays retained:'
        dfInfoStr5b <- names(dataAfterMunging)[3 : ncol(dataAfterMunging)]
        dfInfoStr5c <- '\nAssays removed:'
        dfInfoStr5d <- colRemoved

        dfInfoStr6a <- '\nPercentage of user provided chemicals available for the model: '
        dfInfoStr6b <- paste0(round(nrow(subset(dataAfterMunging, dataAfterMunging$CASRN %in% User_Chem_list$CASRN))/length(User_Chem_list$CASRN)*100, 2),'%')


        cat(dfInfoStr1, dfInfoStr2, dfInfoStr3, dfInfoStr4, dfInfoStr5a, dfInfoStr5b, dfInfoStr5c, dfInfoStr5d,
            dfInfoStr6a, dfInfoStr6b,  sep = '\n')

      },
      file=outfile
    )
    # Need 2 or more models for the code immediately below to run
    #print(methods)
    #print(modelsFittedList)
    capture.output(
      if (length(methods) >= 2){
        compareModels <- caret::resamples(  modelsFittedList[1 : length(methods)], modelNames = methods, decreasing=TRUE)

        diffModels <- NULL
        try(diffModels <- diff(compareModels), silent = TRUE)

        if(!is.null(diffModels)){
          print(summary(diffModels))
        }
      },
      file=outfile, append = TRUE)

    # performance summary for classification: confusion matrix
    capture.output(
      if(type == "classification"){
        confusionMatrixnTestDataList <- lapply(1 : length(modelsFittedList), function(i) {
          temp                     <- confusionMatrix(predictionTestDataList[[i]] , testingData$toPredict)
          return(temp)
        }
        )

        for(i in 1 : length(methods)) {
          print(paste("--------------------------  ",  methods[i], "  ---------------------------------"), quote=FALSE)
          print(paste("Machine Learning Method:", methods[i]), quote=FALSE)
          print(confusionMatrixnTestDataList[[i]])
          print(paste("=====  SUMMARY:", methods[i], "  ====="), quote=F)
          print(confusionMatrixnTestDataList[[i]]$overall)
        }

      },
      file=outfile, append = TRUE)

    # performance summary for regression: MAE, RMSE and Rquared
    capture.output(
      if(type == "regression"){
        # Eval performance of testing set
        predictionTestDataList <- lapply(1 : length(methods), function(i) {
          temp <- predict(modelsFittedList[[i]] , newdata = testingData)
          return(temp)
        }
        )
        cat('Data in the', oriName, ' assay has a range of', min(data$toPredict), 'to', max(data$toPredict), '\n')
        cat('The range is important in interpreting the RMSE, which is in the same units as', oriName, '\n')
        cat('The smaller the RMSE, the better the model \n\n')


        # dataframe MUST have colnames of 'obs' and 'pred'
        for (i in 1 : length(MLmethods)) {
          obs     <- testingData$toPredict
          pred    <- predictionTestDataList[[i]]
          obsPred <- as.data.frame(cbind(obs, pred))
          cat('\n Performance metrics for the model:', MLmethods[i], '\n')
          print(defaultSummary(obsPred))
        }
      },
      file=outfile, append = TRUE)
    # print(paste("output of performance saved to ", outfile))
  }
  return(modelsFittedList)
}  # end of 'MLCreate()'


# function:    ModelFit()
# purpose:    Takes the input data and develops a list of models based on the ML methods specified.  methods are accessible using list notation
# called by:  MLCreate()
# calls:      no function calls

# inputs:
#           MLtype: single or list of classification or dual use ML methods (see http://topepo.github.io/caret/modelList.html)
#           dataIn: Data which is to be predicted. data frame with a column labeled "toPredict" of type factor (if classification) or numeric (if regression) **if data has NA, verify that the ML approach choosen can support
#           trControlParams: the training control parameters, specified in trn_ctl
#           metric: the metric by which to evaluate the models. Can support list. see caret::train for more options.  The only metrics we use here are ROC or RMSE

# output:  caret train object

ModelFit <- function(MLtype, dataIn, trControlParams, metric='ROC', ...) {
  if (is.factor(dataIn$toPredict)) {
    # cat('Using classification model')

    fitData <- caret::train(toPredict ~.                           ,
                            data            = dataIn               ,
                            method          = MLtype               ,
                            metric          = metric               ,
                            preProcess      = c('center', 'scale') , # this centers, scales data
                            # importance      = TRUE   Comment this out because it was causing trouble and is not needed
                            trControl       = trControlParams
    )

  } else {
    # cat('Using regression model')

    fitData <- caret::train(toPredict ~.                           ,
                            data            = dataIn               ,
                            method          = MLtype               ,
                            metric          = metric               ,
                            preProcess      = c('center', 'scale') , # this centers, scales data
                            # importance      = TRUE                 ,
                            trControl       = trControlParams
    )
  }
  return(fitData)
}
