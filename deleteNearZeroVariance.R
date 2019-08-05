####functions
# Name:  deleteNearZeroVariance()
# Author: ILS (comptox@ils-inc.com)
# Date:19 June, 2019
# Version: 1.0
# License: MIT
# Summary: remove variables with near zero variance using caret::nearZeroVar
# called by: main program
# calls:    no function calls

# Input: a complete dataframe.  Data, with the exception of what is being predicted, needs to be ALL NUMERIC for this function, otherwise the funcion will fail.
# This condition is checked in code prior to calling this function so failure should not be a option.
# The first 2 cols should be CASRN and toPredict

# Output: Either (1) if no near-zero variables are found, then the orginal dataframe is returned, otherwise (2) a dataframe
#            with near-zero variance variables removed
# Library required: caret


deleteNearZeroVariance <- function(dataIn = dataIn) {
  
  # What is being predicted, either 'classLabel' for classification or 'toPredict' for regression needs to
  #     be removed before low variance features are removed.  The removed columns are then added back after
  #     near-zero variance variables are removed
  
  tempColsRemoved    <- dataIn[c( 1, 2)] 
  predictedToAddBack <- dataIn[c(-1,-2)]
  
  low_variance_cols <- caret::nearZeroVar(predictedToAddBack)
  
  # O means nothing to be removed, just return input dataframe
  if(length(low_variance_cols) == 0) {
    return(dataIn)
  }
  
  predictedToAddBackTemp <- predictedToAddBack[, -low_variance_cols]
  
  # add back
  toReturn <- cbind(tempColsRemoved, predictedToAddBackTemp)

  
  return(toReturn)
  
} # end  function deleteNearZeroVariance

