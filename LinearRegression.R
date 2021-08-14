###Use least-squares to fit a line to the data
###Calculate R^2
###Calculate p-value for R^2

#Create a dummy data
mouse.data <- data.frame(weight = c(0.9, 1.8, 2.4, 3.5, 3.9, 4.4, 5.1, 5.6, 6.3), 
                         size =c(1.4, 2.6, 1.0, 3.7, 5.5, 3.2, 3.0, 4.9, 6.3))

plot(mouse.data$weight, mouse.data$size)
mouse.regression<-lm(size~weight, data = mouse.data)
modelSummary <- summary(mouse.regression)
abline(mouse.regression,  xlab = 'Mouse weight',
       ylab = 'Mouse size', col = '#377eb8', lwd = 4)

### Testing mode
##Create Training and Testing data
set.seed(420)# setting seed to reproduce results of random sampling

#Indicate the row for training data
trainingRowIndex <- sample(1:nrow(mouse.data ), 0.8*nrow(mouse.data ))
trainingData <- mouse.data[trainingRowIndex,]
testingData <- mouse.data[-trainingRowIndex,]

lmMod <- lm(size~weight, data  = trainingData)
sizePre <- predict(lmMod, testingData)
summary(lmMod)

#Calculate prediction accuracy and error rates

actuals_preds <- data.frame(cbind(actuals = testingData$size, predciteds = sizePre))
correlation_accuracy <- cor(actuals_preds)
head(actuals_preds)


###Calculate the Min Max Accuracy and the Mean Absolute Percentage Error

library(MLmetrics)
min_max_acc <- mean(apply(actuals_preds,1,min)/apply(actuals_preds,1,max))

#mape <- mean(abs(actuals_preds$predciteds - actuals_preds$actuals)/actuals_preds$actuals)
mape <-MAPE(actuals_preds$actuals,actuals_preds$predciteds )
