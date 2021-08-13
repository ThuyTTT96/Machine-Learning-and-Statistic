#import the libraries
library(pROC)
library(randomForest)

#generate 100 values from normal distribution with mean = 172, standard deviation = 29
set.seed(420)
num.samples <- 100
weight <- sort(rnorm(n=num.samples, mean = 172, sd = 29))

#for each ramdom [0-1] sample, compare with the rank(weight)/100, if it is > than the rank return obese, if not return not obese
obese <- ifelse((test = (runif(n = num.samples) < rank(weight)/100)), 
yes=1, no=0)

#print 100 return for obese and plot it in the graph
obese
plot(x = weight, y = obese)

glm.fit = glm(obese ~ weight, family = binomial) 
line(weight, glm.fit$fitted.values)

#draw AUC and pROC

roc(obese, glm.fit$fitted.values, plot = TRUE)
 
# remove the extra space in the chart

par(pty = 's')
roc(obese, glm.fit$fitted.values, plot = TRUE)

# Change the x axis to False positive rate (1 - specificity)

roc(obese, glm.fit$fitted.values, plot = TRUE, legacy.axes = TRUE)

# Change the color of the ROC curve and make it wider and label 2 axises
roc(obese, glm.fit$fitted.values, plot = TRUE, legacy.axes = TRUE, percent = TRUE, xlab = 'False Positive Percentage',
    ylab = 'True positive percentage', col = '#377eb8', lwd = 4)

#Make it variable as an dataframe to see the threshold
roc.info <-roc(obese, glm.fit$fitted.values, legacy.axes = TRUE)
str(roc.info)

#take sensitivities, specificitives and threshold as an dataframe
roc.df<-data.frame(tpp = roc.info$sensitivities*100,
                   fpp = (1 - roc.info$specificities)*100,
                   threshold = roc.info$thresholds)

#print first 6 rows and last 6 rows in this dataframe
head(roc.df)
tail(roc.df)

# print the information of thersholds when tpp between 60 and 80%
roc.df[roc.df$tpp > 60 & roc.df$tpp < 80,]

# Calcute the area under the curve AUC by call print.auc = TRUE in the graph
roc(obese, glm.fit$fitted.values, plot = TRUE, legacy.axes = TRUE, percent = TRUE, xlab = 'False Positive Percentage',
    ylab = 'True positive percentage', col = '#377eb8', lwd = 4, print.auc = TRUE)
# and its partial
roc(obese, glm.fit$fitted.values, plot = TRUE, legacy.axes = TRUE, percent = TRUE, xlab = 'False Positive Percentage',
    ylab = 'True positive percentage', col = '#377eb8', lwd = 4, print.auc = TRUE, print.auc.x = 45, partial.auc = c(100,90),
    auc.polygon = TRUE, auc.polygon.col ='#377eb822' )

#fit other model to see 2 ROC
rf.model <- randomForest(factor(obese) ~ weight)
#layer the new model in graph
roc(obese, glm.fit$fitted.values, plot = TRUE, legacy.axes = TRUE, percent = TRUE, xlab = 'False Positive Percentage',
    ylab = 'True positive percentage', col = '#377eb8', lwd = 4, print.auc = TRUE)
plot.roc(obese, rf.model$votes[,1], percent = TRUE, col ="#4daf4a", lwd = 4, print.auc = TRUE, add = TRUE, print.auc.y  =40)
legend('bottomright', legend = c('Logistic Regression', 'Random Forest'), col = c("#377eb8", "#4daf4a"), lwd = 4)
par(pty = 'm')
