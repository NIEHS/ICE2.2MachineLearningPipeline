####functions
# Name:  MissingValueImpute()
# Author: ILS (comptox@ils-inc.com)
# Date:19 June, 2019
# Version: 1.0
# License: MIT
# Summary: impute missing values (NAs)
# called by: main program
# calls:   NaDeleteRowsAndCols() which removes rows and cols if have too many NAs and calls DataSufficientToModel() 
#
# Arguments:  data for imputation and upper threshholds for deleting cols and rows
# Outputs:    1) dataframe with rows and cols removed above a threshhold and NAs inputed or
#             2)  -1 if there is not enough data for modeling

# Order of removing rows and cols is important, rows are removed first.
# We want to retain as many cols as possible.  By removing rows first, this keeps as many cols as possible.

# Library required: DMwwR , imputeTS

# For imputations, DMwR::knnImputation(rowsAndColsRemoved, meth = "weighAvg") is called first and if this
#   generates a warning (knnImputation failed), then imputeTS::na.mean(dataInImputation, option = 'median') is called.

MissingValueImpute <- function(dataInImputation = dataIn, percentDeleteThreshold = DELETE_THRESHOLD_FOR_NA)  {

  # If no NAs, just return input dataframe
  if (anyNA(dataInImputation) == FALSE) {
    return(dataInImputation)
  }

  # Remove rows and cols that have more NAs than allowed in DELETE_THRESHOLD_FOR_NA
  rowsAndColsRemoved <- NaDeleteRowsAndCols(dataInImputation, percentDeleteThreshold)
  
  # There may be no NAs after previous step, return df in this case
  if(anyNA(rowsAndColsRemoved) == FALSE) {
    return(rowsAndColsRemoved)
  }
  
  #  NaDeleteRowsAndCols() will return -1 if the dataframe is too small to model after removing the rows and cols
  #  NaDeleteRowsAndCols() has already written to ErrorOut text file in this case
  #  Otherwise a dataframe is returned
  
  # if rowsAndColsRemoved is a dataframe, then we are OK to impute, otherwise rowsAndColsRemoved should be -1
  if (is.data.frame(rowsAndColsRemoved)) {
  } else {
    if (rowsAndColsRemoved == -1) {
      return(-1)
    }
  }
  
  
  # knn gets messed up if leave CASRNs in df
  removedCASRN               <- subset(rowsAndColsRemoved, select =  c(CASRN))
  rowsAndColsRemovedNoCASRN  <- subset(rowsAndColsRemoved, select = -c(CASRN))
  
  tryCatch({
  
    impKnn <- DMwR::knnImputation(rowsAndColsRemovedNoCASRN, meth = "weighAvg")
    
  }, warning = function(cond) {
    message('Warning in knn imputation')
    message(cond)
    
  }, error = function(cond) {
    message('Too many missing values for k Nearest Neighbor imputation, using column median instead')
    impKnn <- imputeTS::na.mean(rowsAndColsRemovedNoCASRN, option = 'median')
    message(cond)
    
  }, finally = {
    # nothing
  } )
  
  # Add back CASRN
  finalDf <- cbind(removedCASRN, impKnn)
  return(finalDf)
  
}

# Name:  NaDeleteRowsAndCols()
# Summary: deteermine if enough data for modeling
# called by: MissingValueImpute()
# calls:   DataSufficientToModel()  
# Input:     dataframe
# Output:    
# Library required: 
#
# See which order of deletion leaves the most data: delete rows first then cols or vice versa

NaDeleteRowsAndCols <- function(df = dataIn, percentDeleteThreshold = thresholdIn) {
  
  # If there are no NAs, just return df
  if (anyNA(df) == FALSE) {return(df)}
  
  # get fraction of NAs in each row and multiply by 100 to get percent
  # First col is CASRN, so do not include that when tallying NAs in each row,
  #    that is, ncol(df) - 1  is the number of informative data columns
  
  naPercentEachRow <- 100 * (as.numeric(rowSums(is.na(df)) / (ncol(df) - 1)))
  rowsForDeletion  <- which(naPercentEachRow > percentDeleteThreshold)
  
  # can have situtation where all rows are to be deleted
  if(length(rowsForDeletion) == nrow(df)) {
    return(-1)
  }
  
  if(length(rowsForDeletion) == 0)  {  # no rows will be deleted
    rowsDeletedDf <- df
  } else {                         # delete the rows
    rowsDeletedDf <- df[-c(rowsForDeletion) , ]
  }
  
  # Enough data to continue after removing rows ?
  EnoughDataToProceed <- DataSufficientToModel(rowsDeletedDf)
  if (EnoughDataToProceed == FALSE) {
    return(-1)
  }
  
  # Now the columns
  
  naPercentEachCol <- 100 * (as.numeric(colSums(is.na(rowsDeletedDf)) / nrow(rowsDeletedDf)))
  colsForDeletion  <- which(naPercentEachCol > percentDeleteThreshold)
  
  # can have situtation where all cols are to be deleted.  There are ncol(dataframe) -1 data columns
  if(length(colsForDeletion) == ncol(df) -1) {
    return(-1)
  }
  
  
  if(length(colsForDeletion) == 0)  {       # no cols will be deleted
    return(rowsDeletedDf)
  } else {                                  # delete the rows
    DataFinal <- rowsDeletedDf[, -c(colsForDeletion) ]
  }
  
  # Enough data to continue after removing cols ?
  EnoughDataToProceed <- DataSufficientToModel(DataFinal)
  if (EnoughDataToProceed == FALSE) {
    return(-1)
  }  else {
    return(DataFinal)
  }  
  
}  # end  NaDeleteRowsAndCols()


# Summary: deteermine if enough data for modeling
# called by: NaDeleteRowsAndCols()  - twice
# calls:     no functions called
# Input:     dataframe
# Output:    TRUE if enough data for modeling or FALSE is not enough data for modeling

DataSufficientToModel <- function(dataIn = dataIn) {
  if(nrow(dataIn) < 10 || ncol(dataIn) < 4) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}



# We need at least 10 chemicals and 2 assays
# Note that the first two cols are CASRN and toPredict, so if < 4 cols, then there are not two assays with data
