# Deleting all past variables from our R environment
rm(list=ls())

# Loading the KNN library
library(kknn)

# Implementing KNN on min-max normalized dataframe

# Loading the pre-processed min-max normalized dataframe onto R script
name <- file.choose()

# Reading the dataset with 'y' column as 'factor'
knn_data <- read.csv(name, colClasses = c("y" = "factor"))

# View the dataframe loaded
View(knn_data)

# Generating a list of 70% random indexes of the dataframe
indexes <- sort(sample(nrow(knn_data),as.integer(.70*nrow(knn_data))))

# Creating a training dataframe with 70% data from dataframe using the above generated 'indexes' list
training <- knn_data[indexes,]

# Creating a test dataframe with 30% data from dataframe using the above generated 'indexes' list
test <- knn_data[-indexes,]

# Performing KNN on our dataframe
predict_k <- kknn(formula = factor(y)~., training, test, k = 3, kernel ="rectangular")
  
# Fitting our predicted 'Y' values for the 'test' dataframe
fit <- fitted(predict_k)
result<-table(test$y,fit)

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
knn_data <- read.csv(name, colClasses = c("y" = "factor"))

# View the dataframe loaded
View(knn_data)

# Generating a list of 70% random indexes of the dataframe
indexes <- sort(sample(nrow(knn_data),as.integer(.70*nrow(knn_data))))

# Creating a training dataframe with 70% data from dataframe using the above generated 'indexes' list
training <- knn_data[indexes,]

# Creating a test dataframe with 30% data from dataframe using the above generated 'indexes' list
test <- knn_data[-indexes,]

# Performing KNN on our dataframe
predict_k <- kknn(formula = factor(y)~., training, test, k = 3, kernel ="rectangular")

# Fitting our predicted 'Y' values for the 'test' dataframe
fit <- fitted(predict_k)
result<-table(test$y,fit)

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

