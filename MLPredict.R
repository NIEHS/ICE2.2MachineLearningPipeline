####functions
# Name:   MLPredict()
# Author: ILS (comptox@ils-inc.com)
# Date:19 June, 2019
# Version: 1.0
# License: MIT
# Summary: takes the user-specified ML models and predicts the variable specified by the user.
# called by: main program
# calls:    no function calls

# Inputs:
#           1)  data: dataframe for modeling
#           2)  modelList:  string with ML methods for testing created by MLmethods 
#               for example, (MLmethods <- c('rpart', 'knn', 'svmRadial', 'pls', 'cforest') 

# Output:   returns a data frame with the first n columns the predictions of all n models in modelList. 
#           If the correct class/value to be predicted is included it will be retained in the output data

# Library required: 




MLPredict <- function(data= indata, modelList = ModelsFittedList, ...){
  predictionList <- lapply(1 : length(modelList), function(i) {
    temp <- predict(modelList[[i]] , newdata = data)
    return(temp)
  }
  )
  predData           <- as.data.frame(predictionList)
  colnames(predData) <- unlist(lapply(seq_along(modelList), function(i){ mlm=modelList[[i]]$method; return(mlm) }))
  predData2          <- cbind(predData,data)
  return(predData2)
}  # end MLPredict
