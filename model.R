library(dplyr)
library(ggplot2)
library(caret)
master_data <- read.csv('data/HDdata.csv')

# Remove missing values
master_data <- master_data %>%
  filter(!is.na(age))

# Split out the dependent variable
heart_data <- master_data[, 1:10]
y <- master_data[,11]

# Split out training and test data
set.seed(12)
training_indexes <- createDataPartition(y, p=.70, list = F)
X_train <- heart_data[training_indexes, 1:10]
y_train <- y[training_indexes]
X_test <- heart_data[-training_indexes, 1:10]
y_test <- y[-training_indexes]

# Preprocessing
# Scale data
X_train <- scale(X_train, center = F)
X_test <- scale(X_test, center= F, scale=attr(X_train, "scaled:scale"))
X_train <- as.data.frame(X_train)
X_test <- as.data.frame(X_test)


fit <- glm(y_train~age+sex+cp+trestbps+chol+fbs+restecg+thalach+exang+oldpeak,data=X_train,family=binomial())

threshold <- 0.5

# Train Predictions
train_predictions <- predict(fit, X_train, type = 'response')
train_predictions <- as.numeric(train_predictions > threshold)
train_cm <- confusionMatrix(as.factor(train_predictions), as.factor(y_train))

# Test Predictions
predictions <- predict(fit, X_test, type = 'response')
predictions <- as.numeric(predictions > threshold)
cm <- confusionMatrix(as.factor(predictions), as.factor(y_test))

accuracy <- cm$overall['Accuracy']
sensitivity <- cm$byClass['Sensitivity']
specificity <- cm$byClass['Specificity']
ppv <- cm$byClass['Pos Pred Value']
npv <- cm$byClass['Neg Pred Value']


