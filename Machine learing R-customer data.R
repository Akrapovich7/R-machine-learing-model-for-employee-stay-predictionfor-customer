install.packages("caret")
install.packages("dplyr")
install.packages("randomForest")
install.packages("e1071")

# Load the libraries
library(caret)
library(dplyr)
library(randomForest)
library(e1071)
#------making a dataframe-------------
set.seed(123)
data <- data.frame(
  Age = sample(20:60, 200, replace = TRUE),
  Salary = sample(30000:120000, 200, replace = TRUE),
  YearsAtCompany = sample(1:20, 200, replace = TRUE),
  JobSatisfaction = sample(1:5, 200, replace = TRUE),
  Stayed = sample(c(1, 0), 200, replace = TRUE)
  
#----------------train and test data------------------
  set.seed(123)
  trainIndex <- createDataPartition(data$Stayed, p = 0.8, list = FALSE)
  trainData <- data[trainIndex, ]
  testData <- data[-trainIndex, ]
  
#---training my model----------------------
  set.seed(123)
  model_rf <- randomForest(Stayed ~ ., data = trainData, importance = TRUE)
  print(model_rf)
  
#----------evaluating my model-----------------
  predictions <- predict(model_rf, testData)
  conf_matrix <- confusionMatrix(predictions, testData$Stayed)
  print(conf_matrix)
  
#-------------tuning my model-----------
  tuneGrid <- expand.grid(.mtry = c(1:5))
  tune_rf <- train(Stayed ~ ., data = trainData, method = "rf", tuneGrid = tuneGrid)
  print(tune_rf)