####functions
# Name:  deleteHighlyCorrelated()
# Author: ILS (comptox@ils-inc.com)
# Date:19 June, 2019
# Version: 1.0
# License: MIT
# Summary: examine highly correlated variables for possible exclusion using caret::findCorrelation
# called by: main program
# calls:    no function calls
#
# Input: A complete dataframe (no NAs allowed).   If this conditions is not met, 
#             the function with fail.  The condition is checked before the function is called so this really should not happen.
#         Data should have first 2 cols as CASRN, toPredict.  These cols are removed prior to
#            calculations, then added back after calculations are done.
# Output:    a complete dataframe with highly correlated variables removed
# Library required: caret
#
# See the caret::findCorrelation() documentation for more details

deleteHighlyCorrelated <- function(dataIn = dataIn) {
  
  toAddBack   <- dataIn[c( 1, 2)] 
  forAnalysis <- dataIn[c(-1,-2)]
  
  highlyCorrelated <- findCorrelation(cor(forAnalysis), cutoff=0.95, exact=FALSE)
  
  # if no highly correlated variables found, return input df
  if (length(highlyCorrelated) == 0) {
    # print('#############  In function deleteHighlyCorrelated().  Nothing to remove')
    toReturn <- cbind(toAddBack, forAnalysis)
    return(toReturn)
  }
  
  # print(paste("Highly correlated variables being deleted:", names(forAnalysis)[highlyCorrelated]) )
  
  highlyCorrelated <- as.integer(highlyCorrelated)
  # print(paste0("########### Removed ", length(highlyCorrelated), " highly correlated columns"))
  
  tempDataRemoved <- forAnalysis[, -highlyCorrelated]
  toReturn <- cbind(toAddBack, tempDataRemoved)
  return(toReturn)
  
} # end function deleteHighlyCorrelated
