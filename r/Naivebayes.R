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
