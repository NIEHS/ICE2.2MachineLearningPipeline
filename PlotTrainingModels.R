####functions
# Name:  PlotTrainingModels()
# Author: ILS (comptox@ils-inc.com)
# Date:19 June, 2019
# Version: 1.0
# License: MIT
# Summary: plots the performance of the training models
# called by:  main code
# calls:      no function calls

# Inputs:
#        MLmethods:             a string with machine learning methods selected by user
#        mlList:                R list object with model performance using training data
#        ModelPerormanceOutPdf: file name for PDF

# Output:  PDF file that plots training performance for different models (need at least two) 

# Library required: caret


PlotTrainingModels <- function(MLmethods = MLmethods, mlList = mlList, ModelPerformancePdf = ModelPerformancePdf ) {
  
  # resamples() is a caret function, see documentation
  # Need 2 or more models for this function to work.  
  
  if(length(MLmethods) > 1) {
    
    pdf(ModelPerformancePdf, width = 11, height = 8, onefile = TRUE)
    
    compareModels <- caret::resamples( mlList[1 : length(MLmethods)], modelNames = MLmethods ,
                                       decreasing=TRUE)
    
    # Box and whiskers plot.  bwplot() is a lattice function
    # print statement needed to plot
    print(bwplot(compareModels, main='Training Data Performance'))
    
    # Statistical differences between models.  diff() is a base R function.
    
    diffsModels <- diff(compareModels) 
    summary(diffsModels)
    
    dev.off()
  }
} # end PlotTrainingModels
