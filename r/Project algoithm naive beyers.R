
rm(list=ls())
DS <- read.csv("~/Desktop/CS - 513 KDD/breast-cancer-wisconsin.csv",
               na.string = "?")
#Summarizing each column 
summary(DS)
View(DS) 
str(DS)
is.data.frame(DS) 
library(e1071)
library(class)
#Converting the type of column F6 from character to numeric
#DS$F6<-as.integer(DS$F6)
#Converted the Class into type factor
DS$Class<- factor(DS$Class , levels = c("2","4") , labels
                  = c("Benign","Malignant"))
print(DS)
is.factor(DS$Class)

UpData<-DS[2:11]

#70% of the sample 
sample_size <- floor(0.70 * nrow(UpData))
#Set the seed 
set.seed(123)
train_data <- sample(seq_len(nrow(UpData)), size = sample_size)
#Loading 70% Breast cancer record in training dataset
training <- UpData[train_data, ]
#Loading 30% Breast cancer in test dataset
test <- UpData[-train_data, ]

#Implementing NaiveBayes technique
model_naiveData<- naiveBayes(Class ~ ., data = training)
#Predicting target class for the Validation set in confusion matrix
predict_naive <- predict(model_naiveData, test)

confusion_matrix <- table(predict_nb=predict_naive,class=test$Class)
print(confusion_matrix)
#Output of Naive Bayes Classifier in confusion matrix
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(confusion_matrix)

