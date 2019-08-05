####functions
# Name:  PlotVariableImportance()
# Author: ILS (comptox@ils-inc.com)
# Date:19 June, 2019
# Version: 1.0
# License: MIT
# Summary: plots variable importance
# called by: main program
# calls:    no function calls

# Inputs:
#        mlList:                      R list object with model performance using training data
#        VariableImportanceOutPdf:    file name for PDF
#        VariableImportancePlotFail:  file name if this function fails

# Output:  Either
#          (1) PDF file that plots variable importance for the models
#            or
#         (2) text file indicating that this plot could not be created

# Library required:



PlotVariableImportance <- function(mlList = mlList, VariableImportancePdf = VariableImportancePdf,
                                   VariableImportancePlotFail = VariableImportancePlotFail) {

  tryCatch({

    pdf(VariableImportancePdf,  onefile = TRUE)

    for(i in 1 : length(mlList) ) {

      varImportanceTemp <- caret::varImp(mlList[[i]])
      # print(paste0(MLmethods[i], ' is the model'))

      # plot top 10
      print(plot(varImportanceTemp, top = 10, main = paste0('Variable Importance for ', MLmethods[i])))

    }
    dev.off()

  }, error = function(cond) {
    message('plot of variable importance has failed')
    message(cond)
    cat('Variable Importance Plot has failed.
            \nSee file Data_Processing_Summary.txt for the data used.  Machine learning methods used are: \n', MLmethods,
        file = VariableImportancePlotFail)
    dev.off()
    file.remove(VariableImportancePdf)

  }, finally = {
    # nothing
  } )

}
