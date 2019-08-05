####functions
# Name: DataInitialCleanup()
# Author: ILS (comptox@ils-inc.com)
# Date:19 June, 2019
# Version: 1.0
# License: MIT
# Summary: data cleanup for data coming from ICE server
# called by: main program
# calls:     no functions called
#
# Input: initial dataframe from ICE
#        first col is CASRN
#        second col is what we are trying to predcit
#        all other cols are data
# Output: datafrome with the following removed:
#            rows where CASRN or toPredict are NA
#            rows and cols that are all NA
# Library required: 
#



DataInitialCleanup <- function(dataIn = dataFrameIn) {
  
  # Col 1 and 2 are CASRN and toPredict so DATA starts in col 3 and goes to the end of the row
  # We are counting the number of DATA NAs in each row, that is, the number of NAs from col 3 to the end of the row.  
  # If the number of DATA NAs is ncol(dataframe) - 2, then all the DATA is NA and that row should be deleted
  
  keepRow         <- rowSums(is.na(dataIn[ , 3 : ncol(dataIn)])) != ncol(dataIn) - 2
  dataRowsDeleted <- dataIn[keepRow , ]
  
  # delete any col that is all NA
  dataRowsAndColsDeleted <- dataRowsDeleted[, colSums(is.na(dataRowsDeleted)) != nrow(dataRowsDeleted) ]
  
  # Remove cols where either CASRN or variable to predict is NA (cols 1 and 2)
  dataRemoveCol1Na <- subset(dataRowsAndColsDeleted, !is.na(dataRowsAndColsDeleted[, 1] ))
  dataRemoveCol2Na <- subset(dataRemoveCol1Na,       !is.na(dataRemoveCol1Na      [, 2] ))
  
  return(dataRemoveCol2Na)
}  # end   DataInitialCleanup 



