
library(e1071)

# Load the data from the csv file
dataDirectory <- "H:/Ing 3 - Heriot Watt/test_Axa/LFB incident data 1 Jan 2009 to 31 Aug 2015/"

# read the data
data <- read.csv(paste(dataDirectory, 'MyData.csv', sep=""), header = TRUE, sep=";")

# generate the unique vector without duplicates containing all the Borough names 
#BoroughName <- unique(tolower(data$IncGeo_BoroughName))

# generate the unique vector without duplicates containing all the Property category
PropertyCat <- unique(tolower(data$PropertyCategory))

# Loop on the borough names
# for(i in BoroughName) {

# Loop on the property type
for(i in PropertyCat) {
  
  # generate a subset of the data by boroughs
  #borough_data <- subset(data, tolower(data$IncGeo_BoroughName)==toString(i))
  
  # generate a subset of the data by properties
  property_data <- subset(data, tolower(data$PropertyCategory)==toString(i))
  
  # generate matrix of data picking up specific fields of the data
  # 3,4 and 5 (day, month and year) 7 (the hour of call) 11 (number of the property category) 13 (number of the property type)
  #X <- as.matrix(borough_data[,c(3,4,5,7,11,13)])
  # 3,4 and 5 (day, month and year) 7 (the hour of call) 13 (number of the property type) 15 (number of borough)
  X <- as.matrix(property_data[,c(3,4,5,7,13,15)])
  
  # generate the category vector for each subset of data (if its a fire = 1, if not = 0)
  #Y <- borough_data$Fire
  Y <- property_data$Fire
  
  # generate the training sets years 2012, 2013 and 2014
  train <- X[,"Year"] == 2012 | X[,"Year"] == 2013 | X[,"Year"] == 2014
  # remove the year field for the training set (-3)
  xtrain <- X[train,-3] 
  ytrain <- Y[train]
  
  #generate the test set year 2015
  # remove the year field for the training set (-3)
  xtest <- X[X[,"Year"] == 2015, -3]
  ytest <- Y[X[,"Year"] == 2015]
  
  # create the svm model
  model <- svm(xtrain,ytrain, type= 'C-classification', scale= FALSE ,kernel = 'linear')#,cost= drop(obj$best.parameters))
  
  # make predictions on the test test using svm model
  pred <- predict(model, xtest)

  # generate the confusion matrix
  #cTr <- table(pred, ytest)
  #print(cTr)
  
  # print the prediction level of fire risk by borough or property
  #count the sum of true predictions and compare it to the actual values of the test set
  cat("Prediction level of fire risk for ",i," is ", 100*sum(pred == ytest)/length(ytest), "% \n") 
  
}
