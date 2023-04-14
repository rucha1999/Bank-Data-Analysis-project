# Normalizing the pre-processed data

# Deleting all past variables from our R environment
rm(list=ls())

# Loading the pre-processed dataframe onto R script
filename <- file.choose()

# Reading the dataframe 
data <- read.csv(filename) 

# Viewing the dataframe loaded
View(data)

# Summarizing the dataframe
summary(data)

# Checking the datatypes of all variables of the dataframe
str(data)

# Converting integer datatype columns to numeric datatype columns
data[1:2] = lapply(data[1:2], FUN = function(y){as.numeric(y)})
data[12:15] = lapply(data[12:15], FUN = function(y){as.numeric(y)})

# Creating a function that performs min-max normalization
minmax_normalizing <- function(x, na.rm = TRUE) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Creating a function that performs z-score normalization
z_normalizing <- function(x, na.rm = TRUE) {
  return ((x - mean(x)) / sd(x))
}

# Normalizing our dataframe using the min-max normalization function described above
minmax_normalized_data = as.data.frame(lapply(data[c('age', 'duration', 'campaign', 'pdays', 'previous', 'emp_var_rate', 'cons_price_idx', 'cons_conf_idx', 'euribor3m', 'nr_employed')], minmax_normalizing))

# Creating min-max normalized dataframe 
minmax_normalized_data["job"] = data$job
minmax_normalized_data["marital"] = data$marital
minmax_normalized_data["education"] = data$education
minmax_normalized_data["default"] = data$default
minmax_normalized_data["housing"] = data$housing
minmax_normalized_data["loan"] = data$loan
minmax_normalized_data["contact"] = data$contact
minmax_normalized_data["month"] = data$month
minmax_normalized_data["day_of_week"] = data$day_of_week
minmax_normalized_data["cons.conf.idx"] = data$cons.conf.idx
minmax_normalized_data["poutcome"] = data$poutcome
minmax_normalized_data["y"] = data$y

# Viewing the min-max normalized dataframe 
View(minmax_normalized_data)

# Normalizing our dataframe using the z-score normalization function described above
z_normalized_data = as.data.frame(lapply(data[c('age', 'duration', 'campaign', 'pdays', 'previous', 'emp_var_rate', 'cons_price_idx', 'cons_conf_idx', 'euribor3m', 'nr_employed')], z_normalizing))

# Creating z-score normalized dataframe 
z_normalized_data["job"] = data$job
z_normalized_data["marital"] = data$marital
z_normalized_data["education"] = data$education
z_normalized_data["default"] = data$default
z_normalized_data["housing"] = data$housing
z_normalized_data["loan"] = data$loan
z_normalized_data["contact"] = data$contact
z_normalized_data["month"] = data$month
z_normalized_data["day_of_week"] = data$day_of_week
z_normalized_data["cons.conf.idx"] = data$cons.conf.idx
z_normalized_data["poutcome"] = data$poutcome
z_normalized_data["y"] = data$y

# Viewing the z-score normalized dataframe 
View(z_normalized_data)

#Exporting the min-max normalized data as a csv file
write.csv(minmax_normalized_data, "C:\\Users\\tanya\\Desktop\\1st_Sem\\KDDM\\Project\\Datasets_Used\\MinMax_Normalized_Data.csv", row.names = FALSE)

#Exporting the z-score normalized data as a csv file
write.csv(z_normalized_data, "C:\\Users\\tanya\\Desktop\\1st_Sem\\KDDM\\Project\\Datasets_Used\\Z_Normalized_data.csv", row.names = FALSE)

# ----------------------------------------------------------------------------------------------

# Decision Trees

# Deleting all past variables from our R environment
rm(list=ls())

# Loading the rpart and rpart.plot library
library(rpart)
library(rpart.plot)

# Implementing CART on min-max normalized dataframe

# Loading the pre-processed min-max normalized dataframe onto R script
filename <- file.choose()

# Reading the dataset with 'y' column as 'factor'
minmax_normalized_data <- read.csv(filename, colClasses = c("y" = "factor")) 

# Summarizing the dataframe
summary(minmax_normalized_data)

# Checking the datatypes of variables of dataframe
str(minmax_normalized_data)

# Initializing the plotting window
dev.off

# Viewing the dataframe loaded
View(minmax_normalized_data)

# Setting the seed value at '555'
set.seed(555)

# Generating a list of 70% random indexes of the dataframe
index <- sort(sample(nrow(minmax_normalized_data), round(.70*nrow(minmax_normalized_data))))

# Creating a training dataframe with 70% data from dataframe using the above generated 'indexes' list
training <- minmax_normalized_data[index,]

# Creating a test dataframe with 30% data from dataframe using the above generated 'indexes' list
test <- minmax_normalized_data[-index,]

# Training our model on our training dataframe
CART_y <- rpart(y~., data = training)

# Plotting our model that has been trained
rpart.plot(CART_y)

# Closing the plotting window
dev.off()

# Predict results on our test dataframe
CART_predict <- predict(CART_y, test, type="class") 

# Creating a confusion matrix of our results and storing them in variable named 'result'
result <- table(Actual=test[,21], CART = CART_predict)

# Printing the confusion matrix
print("Confusion matrix of our result is: ")
print(result)

# Calculating the incorrect values, that is, values that are predicted wrong
incorrect <- (test[,21] != CART_predict)

# Calculating the 'error rate' of our model
errror_rate <- sum(incorrect)/length(incorrect)
# Printing the error rate of our model
print(paste("Error rate of our model (with min-max normalization) is: ", errror_rate))

# Calculating the 'accuracy' of our model
accuracy <- (1 - errror_rate) * 100
# Printing the accuracy of our model
print(paste("Accuracy of our model (with min-max normalization) is: ", accuracy))

# Calculating the 'precision score' of our model
precision_score <- result[2,2] / ifelse(test = sum(result[2,2], result[1,2]) == 0, yes = 1, no = sum(result[2,2], result[1,2]))
# Printing the precision score of our model
print(paste("Precision score of our model (with min-max normalization) is: ", precision_score))

# Calculating the 'recall score' of our model
recall_score <- result[2,2] / ifelse(test = sum(result[2,2], result[2,1]) == 0, yes = 1, no = sum(result[2,2], result[2,1]))
# Printing the recall score of our model
print(paste("Recall score of our model (with min-max normalization) is: ", recall_score))

# Calculating the 'F1-measure' of our model
f1_measure <- 2 * precision_score * recall_score / ifelse(test = precision_score + recall_score == 0, yes = 1, no = precision_score + recall_score)
# Printing the F1-measure of our model
print(paste("F1-measure of our model (with min-max normalization) is: ", f1_measure))

# --------------------------------------------------------------------------

# Implementing CART on z-score normalized dataframe

# Deleting all past variables from our R environment
rm(list=ls())

# Loading the pre-processed min-max normalized dataframe onto R script
filename <- file.choose()

# Reading the dataset with 'y' column as 'factor'
zscore_normalized_data <- read.csv(filename, colClasses = c("y" = "factor")) 

# Summarizing the dataframe
summary(zscore_normalized_data)

# Checking the datatypes of variables of dataframe
str(zscore_normalized_data)

# Initializing the plotting window
dev.off

# Viewing the dataframe loaded
View(zscore_normalized_data)

# Setting the seed value at '555'
set.seed(555)

# Generating a list of 70% random indexes of the dataframe
index <- sort(sample(nrow(zscore_normalized_data), round(.70*nrow(zscore_normalized_data))))

# Creating a training dataframe with 70% data from dataframe using the above generated 'indexes' list
training <- zscore_normalized_data[index,]

# Creating a test dataframe with 30% data from dataframe using the above generated 'indexes' list
test <- zscore_normalized_data[-index,]

# Training our model on our training dataframe
CART_y <- rpart(y~., data = training)

# Plotting our model that has been trained
rpart.plot(CART_y)

# Closing the plotting window
dev.off()

# Predict results on our test dataframe
CART_predict <- predict(CART_y, test, type="class") 

# Creating a confusion matrix of our results and storing them in variable named 'result'
result <- table(Actual=test[,21], CART = CART_predict)

# Printing the confusion matrix
print("Confusion matrix of our result is: ")
print(result)

# Calculating the incorrect values, that is, values that are predicted wrong
incorrect <- (test[,21] != CART_predict)

# Calculating the 'error rate' of our model
errror_rate <- sum(incorrect)/length(incorrect)
# Printing the error rate of our model
print(paste("Error rate of our model (with z-score normalization) is: ", errror_rate))

# Calculating the 'accuracy' of our model
accuracy <- (1 - errror_rate) * 100
# Printing the accuracy of our model
print(paste("Accuracy of our model (with z-score normalization) is: ", accuracy))

# Calculating the 'precision score' of our model
precision_score <- result[2,2] / ifelse(test = sum(result[2,2], result[1,2]) == 0, yes = 1, no = sum(result[2,2], result[1,2]))
# Printing the precision score of our model
print(paste("Precision score of our model (with z-score normalization) is: ", precision_score))

# Calculating the 'recall score' of our model
recall_score <- result[2,2] / ifelse(test = sum(result[2,2], result[2,1]) == 0, yes = 1, no = sum(result[2,2], result[2,1]))
# Printing the recall score of our model
print(paste("Recall score of our model (with z-score normalization) is: ", recall_score))

# Calculating the 'F1-measure' of our model
f1_measure <- 2 * precision_score * recall_score / ifelse(test = precision_score + recall_score == 0, yes = 1, no = precision_score + recall_score)
# Printing the F1-measure of our model
print(paste("F1-measure of our model (with z-score normalization) is: ", f1_measure))


# ----------------------------------------------------------------------------------------------

# Random Forest

# Deleting all past variables from our R environment
rm(list=ls())

# Loading the Random Forest library
library(randomForest)

# Implementing Random Forest on min-max normalized dataframe

# Loading the pre-processed min-max normalized dataframe onto R script
filename <- file.choose()

# Reading the dataset with 'y' column as 'factor'
minmax_normalized_data <- read.csv(filename, colClasses = c("y" = "factor")) 

# Summarizing the dataframe
summary(minmax_normalized_data)

# Checking the datatypes of variables of dataframe
str(minmax_normalized_data)

# Initializing the plotting window
dev.off

# Viewing the dataframe loaded
View(minmax_normalized_data)

# Setting the seed value at '555'
set.seed(555)

# Generating a list of 70% random indexes of the dataframe
index <- sort(sample(nrow(minmax_normalized_data), round(.70*nrow(minmax_normalized_data))))

# Creating a training dataframe with 70% data from dataframe using the above generated 'indexes' list
training <- minmax_normalized_data[index,]

# Creating a test dataframe with 30% data from dataframe using the above generated 'indexes' list
test <- minmax_normalized_data[-index,]

# Fitting our model on our training dataframe
fit <- randomForest(factor(y)~., data = training, importance=TRUE, ntree=1000)

# Calculating the importance of each feature of our dataframe, MeanDecreaseAccuracy and MeanDecreaseGini
importance(fit)

# Plotting the importance of each feature of our dataframe, MeanDecreaseAccuracy and MeanDecreaseGini
varImpPlot(fit)

# Closing the plotting window
dev.off()

# Predicting results on our test dataframe
Prediction <- predict(fit, test)

# Creating a confusion matrix of our results and storing them in variable named 'result'
result <- table(Actual=test[,21], Prediction)

# Printing the confusion matrix
print("Confusion matrix of our result is: ")
print(result)

# Calculating the incorrect values, that is, values that are predicted wrong
incorrect <- (test[,21] != Prediction)

# Calculating the 'error rate' of our model
errror_rate <- sum(incorrect)/length(incorrect)
# Printing the error rate of our model
print(paste("Error rate of our model (with min-max normalization) is: ", errror_rate))

# Calculating the 'accuracy' of our model
accuracy <- (1 - errror_rate) * 100
# Printing the accuracy of our model
print(paste("Accuracy of our model (with min-max normalization) is: ", accuracy))

# Calculating the 'precision score' of our model
precision_score <- result[2,2] / ifelse(test = sum(result[2,2], result[1,2]) == 0, yes = 1, no = sum(result[2,2], result[1,2]))
# Printing the precision score of our model
print(paste("Precision score of our model (with min-max normalization) is: ", precision_score))

# Calculating the 'recall score' of our model
recall_score <- result[2,2] / ifelse(test = sum(result[2,2], result[2,1]) == 0, yes = 1, no = sum(result[2,2], result[2,1]))
# Printing the recall score of our model
print(paste("Recall score of our model (with min-max normalization) is: ", recall_score))

# Calculating the 'F1-measure' of our model
f1_measure <- 2 * precision_score * recall_score / ifelse(test = precision_score + recall_score == 0, yes = 1, no = precision_score + recall_score)
# Printing the F1-measure of our model
print(paste("F1-measure of our model (with min-max normalization) is: ", f1_measure))

# --------------------------------------------------------------------------

# Implementing Random Forest on z-score normalized dataframe

# Deleting all past variables from our R environment
rm(list=ls())

# Loading the pre-processed z-score normalized dataframe onto R script
filename <- file.choose()

# Reading the dataset with 'y' column as 'factor'
zscore_normalized_data <- read.csv(filename, colClasses = c("y" = "factor")) 

# Summarizing the dataframe
summary(zscore_normalized_data)

# Checking the datatypes of variables of dataframe
str(zscore_normalized_data)

# Initializing the plotting window
dev.off

# Viewing the dataframe loaded
View(zscore_normalized_data)

# Setting the seed value at '555'
set.seed(555)

# Generating a list of 70% random indexes of the dataframe
index <- sort(sample(nrow(zscore_normalized_data), round(.70*nrow(zscore_normalized_data))))

# Creating a training dataframe with 70% data from dataframe using the above generated 'indexes' list
training <- zscore_normalized_data[index,]

# Creating a test dataframe with 30% data from dataframe using the above generated 'indexes' list
test <- zscore_normalized_data[-index,]

# Fitting our model on our training dataframe
fit <- randomForest(factor(y)~., data = training, importance=TRUE, ntree=1000)

# Calculating the importance of each feature of our dataframe, MeanDecreaseAccuracy and MeanDecreaseGini
importance(fit)

# Plotting the importance of each feature of our dataframe, MeanDecreaseAccuracy and MeanDecreaseGini
varImpPlot(fit)

# Closing the plotting window
dev.off()

# Predicting results on our test dataframe
Prediction <- predict(fit, test)

# Creating a confusion matrix of our results and storing them in variable named 'result'
result <- table(Actual=test[,21], Prediction)

# Printing the confusion matrix
print("Confusion matrix of our result is: ")
print(result)

# Calculating the incorrect values, that is, values that are predicted wrong
incorrect <- (test[,21] != Prediction)

# Calculating the 'error rate' of our model
errror_rate <- sum(incorrect)/length(incorrect)
# Printing the error rate of our model
print(paste("Error rate of our model (with z-score normalization) is: ", errror_rate))

# Calculating the 'accuracy' of our model
accuracy <- (1 - errror_rate) * 100
# Printing the accuracy of our model
print(paste("Accuracy of our model (with z-score normalization) is: ", accuracy))

# Calculating the 'precision score' of our model
precision_score <- result[2,2] / ifelse(test = sum(result[2,2], result[1,2]) == 0, yes = 1, no = sum(result[2,2], result[1,2]))
# Printing the precision score of our model
print(paste("Precision score of our model (with z-score normalization) is: ", precision_score))

# Calculating the 'recall score' of our model
recall_score <- result[2,2] / ifelse(test = sum(result[2,2], result[2,1]) == 0, yes = 1, no = sum(result[2,2], result[2,1]))
# Printing the recall score of our model
print(paste("Recall score of our model (with z-score normalization) is: ", recall_score))

# Calculating the 'F1-measure' of our model
f1_measure <- 2 * precision_score * recall_score / ifelse(test = precision_score + recall_score == 0, yes = 1, no = precision_score + recall_score)
# Printing the F1-measure of our model
print(paste("F1-measure of our model (with z-score normalization) is: ", f1_measure))

# ----------------------------------------------------------------------------------------------

# Naive Bayes

# Deleting all past variables from our R environment
rm(list=ls())

# Loading the library
library(e1071)
library(class)

# Implementing Naive bayes on min-max normalized dataframe

# Loading the pre-processed min-max normalized dataframe onto R script
filename <- file.choose()

# Reading the dataset with 'y' column as 'factor'
minmax_normalized_data<- read.csv(filename, colClasses = c("y" = "factor")) 

# Summarizing the dataframe
summary(minmax_normalized_data)

# Checking the datatypes of variables of dataframe
str(minmax_normalized_data)

# Viewing the dataframe loaded
View(minmax_normalized_data)

# Setting the seed value at '555'
set.seed(555)

# Generating a list of 70% random indexes of the dataframe
index <- sort(sample(nrow(minmax_normalized_data), round(.70*nrow(minmax_normalized_data))))

# Creating a training dataframe with 70% data from dataframe using the above generated 'indexes' list
training <- minmax_normalized_data[index,]

# Creating a test dataframe with 30% data from dataframe using the above generated 'indexes' list
test <- minmax_normalized_data[-index,]

#Implementing NaiveBayes technique
fit <- naiveBayes(factor(y)~., data = training, na.action=na.pass)

# Predicting results on our test dataframe
Prediction <- predict(fit, test)

# Creating a confusion matrix of our results and storing them in variable named 'result'
result <- table(Actual=test[,21], Prediction)

# Printing the confusion matrix
print("Confusion matrix of our result is: ")
print(result)

# Calculating the incorrect values, that is, values that are predicted wrong
incorrect <- (test[,21] != Prediction)

# Calculating the 'error rate' of our model
errror_rate <- sum(incorrect)/length(incorrect)
# Printing the error rate of our model
print(paste("Error rate of our model (with min-max normalization) is: ", errror_rate))

# Calculating the 'accuracy' of our model
accuracy <- (1 - errror_rate) * 100
# Printing the accuracy of our model
print(paste("Accuracy of our model (with min-max normalization) is: ", accuracy))

# Calculating the 'precision score' of our model
precision_score <- result[2,2] / ifelse(test = sum(result[2,2], result[1,2]) == 0, yes = 1, no = sum(result[2,2], result[1,2]))
# Printing the precision score of our model
print(paste("Precision score of our model (with min-max normalization) is: ", precision_score))

# Calculating the 'recall score' of our model
recall_score <- result[2,2] / ifelse(test = sum(result[2,2], result[2,1]) == 0, yes = 1, no = sum(result[2,2], result[2,1]))
# Printing the recall score of our model
print(paste("Recall score of our model (with min-max normalization) is: ", recall_score))

# Calculating the 'F1-measure' of our model
f1_measure <- 2 * precision_score * recall_score / ifelse(test = precision_score + recall_score == 0, yes = 1, no = precision_score + recall_score)
# Printing the F1-measure of our model
print(paste("F1-measure of our model (with min-max normalization) is: ", f1_measure))

# --------------------------------------------------------------------------

# Implementing Naive Bayes on z-score normalized dataframe

# Deleting all past variables from our R environment
rm(list=ls())

# Loading the pre-processed z-score normalized dataframe onto R script
filename <- file.choose()

# Reading the dataset with 'y' column as 'factor'
zscore_normalized_data <- read.csv(filename, colClasses = c("y" = "factor")) 

# Summarizing the dataframe
summary(zscore_normalized_data)

# Checking the datatypes of variables of dataframe
str(zscore_normalized_data)

# Viewing the dataframe loaded
View(zscore_normalized_data)

# Setting the seed value at '555'
set.seed(555)

# Generating a list of 70% random indexes of the dataframe
index <- sort(sample(nrow(zscore_normalized_data), round(.70*nrow(zscore_normalized_data))))

# Creating a training dataframe with 70% data from dataframe using the above generated 'indexes' list
training <- zscore_normalized_data[index,]

# Creating a test dataframe with 30% data from dataframe using the above generated 'indexes' list
test <- zscore_normalized_data[-index,]

# Fitting our model on our training dataframe
fit <- naiveBayes(factor(y)~., data = training, na.action=na.pass)

# Predicting results on our test dataframe
Prediction <- predict(fit, test)

# Creating a confusion matrix of our results and storing them in variable named 'result'
result <- table(Actual=test[,21], Prediction)

# Printing the confusion matrix
print("Confusion matrix of our result is: ")
print(result)

# Calculating the incorrect values, that is, values that are predicted wrong
incorrect <- (test[,21] != Prediction)

# Calculating the 'error rate' of our model
errror_rate <- sum(incorrect)/length(incorrect)
# Printing the error rate of our model
print(paste("Error rate of our model (with z-score normalization) is: ", errror_rate))

# Calculating the 'accuracy' of our model
accuracy <- (1 - errror_rate) * 100
# Printing the accuracy of our model
print(paste("Accuracy of our model (with z-score normalization) is: ", accuracy))

# Calculating the 'precision score' of our model
precision_score <- result[2,2] / ifelse(test = sum(result[2,2], result[1,2]) == 0, yes = 1, no = sum(result[2,2], result[1,2]))
# Printing the precision score of our model
print(paste("Precision score of our model (with z-score normalization) is: ", precision_score))

# Calculating the 'recall score' of our model
recall_score <- result[2,2] / ifelse(test = sum(result[2,2], result[2,1]) == 0, yes = 1, no = sum(result[2,2], result[2,1]))
# Printing the recall score of our model
print(paste("Recall score of our model (with z-score normalization) is: ", recall_score))

# Calculating the 'F1-measure' of our model
f1_measure <- 2 * precision_score * recall_score / ifelse(test = precision_score + recall_score == 0, yes = 1, no = precision_score + recall_score)
# Printing the F1-measure of our model
print(paste("F1-measure of our model (with z-score normalization) is: ", f1_measure))

# ----------------------------------------------------------------------------------------------

# K-Nearest Neighbors

# Deleting all past variables from our R environment
rm(list=ls())

# Loading the KNN library
library(kknn)

# Implementing KNN on min-max normalized dataframe

# Loading the pre-processed min-max normalized dataframe onto R script
name <- file.choose()

# Reading the dataset with 'y' column as 'factor'
minmax_normalized_data <- read.csv(name, colClasses = c("y" = "factor"))

# View the dataframe loaded
View(minmax_normalized_data)

# Generating a list of 70% random indexes of the dataframe
indexes <- sort(sample(nrow(minmax_normalized_data),as.integer(.70*nrow(minmax_normalized_data))))

# Creating a training dataframe with 70% data from dataframe using the above generated 'indexes' list
training <- minmax_normalized_data[indexes,]

# Creating a test dataframe with 30% data from dataframe using the above generated 'indexes' list
test <- minmax_normalized_data[-indexes,]

# Performing KNN on our dataframe
predict_k <- kknn(formula = factor(y)~., training, test, k = 3, kernel ="rectangular")

# Fitting our predicted 'Y' values for the 'test' dataframe
fit <- fitted(predict_k)

# Creating a confusion matrix of our results and storing them in variable named 'result'
result <- table(test$y,fit)

# Printing the confusion matrix
print("Confusion matrix of our result is: ")
print(result)

# Calculating the incorrect values, that is, values that are predicted wrong
incorrect <- (test[,21] != fit)

# Calculating the 'error rate' of our model
errror_rate <- sum(incorrect)/length(incorrect)
# Printing the error rate of our model
print(paste("Error rate of our model (with min-max normalization) is: ", errror_rate))

# Calculating the 'accuracy' of our model
accuracy <- (1 - errror_rate) * 100
# Printing the accuracy of our model
print(paste("Accuracy of our model (with min-max normalization) is: ", accuracy))

# Calculating the 'precision score' of our model
precision_score <- result[2,2] / ifelse(test = sum(result[2,2], result[1,2]) == 0, yes = 1, no = sum(result[2,2], result[1,2]))
# Printing the precision score of our model
print(paste("Precision score of our model (with min-max normalization) is: ", precision_score))

# Calculating the 'recall score' of our model
recall_score <- result[2,2] / ifelse(test = sum(result[2,2], result[2,1]) == 0, yes = 1, no = sum(result[2,2], result[2,1]))
# Printing the recall score of our model
print(paste("Recall score of our model (with min-max normalization) is: ", recall_score))

# Calculating the 'F1-measure' of our model
f1_measure <- 2 * precision_score * recall_score / ifelse(test = precision_score + recall_score == 0, yes = 1, no = precision_score + recall_score)
# Printing the F1-measure of our model
print(paste("F1-measure of our model (with min-max normalization) is: ", f1_measure))

# --------------------------------------------------------------------------

# Implementing KNN on z-score normalized dataframe

# Deleting all past variables from our R environment
rm(list=ls())

# Loading the KNN library
library(kknn)

# Implementing KNN on z-score normalized dataframe

# Loading the pre-processed z-score normalized dataframe onto R script
name <- file.choose()

# Reading the dataset with 'y' column as 'factor'
zscore_normalized_data <- read.csv(name, colClasses = c("y" = "factor"))

# View the dataframe loaded
View(zscore_normalized_data)

# Generating a list of 70% random indexes of the dataframe
indexes <- sort(sample(nrow(zscore_normalized_data),as.integer(.70*nrow(zscore_normalized_data))))

# Creating a training dataframe with 70% data from dataframe using the above generated 'indexes' list
training <- zscore_normalized_data[indexes,]

# Creating a test dataframe with 30% data from dataframe using the above generated 'indexes' list
test <- zscore_normalized_data[-indexes,]

# Performing KNN on our dataframe
predict_k <- kknn(formula = factor(y)~., training, test, k = 3, kernel ="rectangular")

# Fitting our predicted 'Y' values for the 'test' dataframe
fit <- fitted(predict_k)

# Creating a confusion matrix of our results and storing them in variable named 'result'
result <- table(test$y,fit)

# Printing the confusion matrix
print("Confusion matrix of our result is: ")
print(result)

# Calculating the incorrect values, that is, values that are predicted wrong
incorrect <- (test[,21] != fit)

# Calculating the 'error rate' of our model
errror_rate <- sum(incorrect)/length(incorrect)
# Printing the error rate of our model
print(paste("Error rate of our model (with z-score normalization) is: ", errror_rate))

# Calculating the 'accuracy' of our model
accuracy <- (1 - errror_rate) * 100
# Printing the accuracy of our model
print(paste("Accuracy of our model (with z-score normalization) is: ", accuracy))

# Calculating the 'precision score' of our model
precision_score <- result[2,2] / ifelse(test = sum(result[2,2], result[1,2]) == 0, yes = 1, no = sum(result[2,2], result[1,2]))
# Printing the precision score of our model
print(paste("Precision score of our model (with z-score normalization) is: ", precision_score))

# Calculating the 'recall score' of our model
recall_score <- result[2,2] / ifelse(test = sum(result[2,2], result[2,1]) == 0, yes = 1, no = sum(result[2,2], result[2,1]))
# Printing the recall score of our model
print(paste("Recall score of our model (with z-score normalization) is: ", recall_score))

# Calculating the 'F1-measure' of our model
f1_measure <- 2 * precision_score * recall_score / ifelse(test = precision_score + recall_score == 0, yes = 1, no = precision_score + recall_score)
# Printing the F1-measure of our model
print(paste("F1-measure of our model (with z-score normalization) is: ", f1_measure))
