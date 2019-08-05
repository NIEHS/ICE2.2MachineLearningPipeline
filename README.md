This R program allows rapid assessment of a variety of machine learning algorithms for classification and regression predictions

# Input files
	MachineLearning_input.txt: example data file of the values for the workflow. Note that the first column needs to be the chemical identifier (CASRN) and the second column the value to predict(can have NAs but must have some data to train on) 
	userChemicals.txt: example data file that containes a list of CASRN that the user wants predictions for

# Code files
	DataInitialCleanup.R: contains functions for the initial steps in data cleanup 
	deleteHighlyCorrelated.R: functions for the identification and removal of highly correlated variables 
	deleteNearZeroVariance.R: identifies and deletes variables with near zero variance 
	MissingValueImpute.R: Imputes missing values 
	MLCreate.R: function for creating the model objects 
	MLPredict.R: function that generates the predictions using the models generated in MLCreate 
	PlotTrainingModels.R: Plots the training performance of the models 
	PlotVariableImportance.R: Plots the variable imporance for each model 
	ICE_MLScript.Rmd: the R notebook with the workflow code 
	ICE_MLScript.nd.html: the R notebook as HTML with the workflow code
