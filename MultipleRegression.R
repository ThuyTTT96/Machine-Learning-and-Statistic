### Re-reproduce mouse size prediction from statquest with Josh Starmer
#simple regression
mouse.data <- data.frame(weight = c(0.9, 1.8, 2.4, 3.5, 3.9, 4.4, 5.1, 5.6, 6.3), 
                         size =c(1.4, 2.6, 1.0, 3.7, 5.5, 3.2, 3.0, 4.9, 6.3),
                         tail = c(0.7, 1.3, 0.7, 2.0, 3.6, 3.0, 2.9, 3.9, 4.0))

plot(mouse.data$weight, mouse.data$size)
simple_regression <- lm(size~weight, data = mouse.data)
summary(simple_regression)
abline(simple_regression, col = 'blue', lwd = 3)

#Multiple regression

plot(mouse.data)
multi_regression <-lm(size~weight + tail, data = mouse.data)
summary(multi_regression)

### Multiple regression in Fish market dataset from Kaggle
## This dataset is a record of 3 common different activities affected in heart desease rate. 
##With this dataset, a predictive model can be performed using machine friendly data
##and estimate the heart desease.

# 1. Load library
library(readr)
library(tidyverse)
library(tidyr)
library(broom)
library(MLmetrics)
# 2. load dataset
heart_data <- read_csv("heart.data.csv")
View(heart_data)
plot(heart_data)

# 3. Split the data to training and testing
trainingRowIndex <- sample(1:nrow(heart_data ), 0.8*nrow(heart_data ))
trainingData <- heart_data[trainingRowIndex,]
testingData <- heart_data[-trainingRowIndex,]

#4. Fit biking and heart.desease to linear regression
plot(heart_data$biking, heart_data$heart.disease)
biking_regression <-lm(heart.disease~biking, data = heart_data)
summary(biking_regression)
tidy(biking_regression)
abline(biking_regression, col='red', lwd = 3)

disease_Pred <- predict(biking_regression, testingData)
actuals_preds <- data.frame(cbind(actuals=testData$heart.disease, predicteds=disease_Pred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  
head(actuals_preds)
mape <-MAPE(actuals_preds$actuals,actuals_preds$predciteds ) # mean absolute percentage error

#5. Fit multiple regression

multiple_regression = lm(heart.disease ~ biking + smoking, data = heart_data)
summary(multiple_regression)
