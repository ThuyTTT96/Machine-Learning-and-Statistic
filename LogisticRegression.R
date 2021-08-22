library(tidyverse)
library(ggplot2)
library(cowplot)

# Load and read the data to dataframe
url = "http://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"

heart_disease_df <-read.csv(url,header = FALSE)

# View data and replace the name of the columns based on the database's website
View(heart_disease_df)
colnames(heart_disease_df)<- c("age", #age in years
                               "sex", #(1 = male; 0 = female)
                               "cp", # cp: chest pain type
                                     #-- Value 1: typical angina
                                     #-- Value 2: atypical angina
                                     #-- Value 3: non-anginal pain
                                     #-- Value 4: asymptomatic
                               "trestbps", # resting blood pressure (in mm Hg on admission to the hospital)
                               "chol", #serum cholestoral in mg/dl
                               "fbs", #(fasting blood sugar > 120 mg/dl) (1 = true; 0 = false)
                               "restecg", 
                               #(fasting  restecg: resting electrocardiographic results
                               # -- Value 0: normal
                               #-- Value 1: having ST-T wave abnormality (T wave inversions and/or ST elevation or depression of > 0.05 mV)
                               #-- Value 2: showing probable or definite left ventricular hypertrophy by Estes' criteriablood sugar > 120 mg/dl) (1 = true; 0 = false)
                               "thalach", # maximum heart rate achieved
                               "exang", # exercise induced angina (1 = yes; 0 = no)
                               "oldpeak",# ST depression induced by exercise relative to rest
                               "slope", # the slope of the peak exercise ST segment
                              #-- Value 1: upsloping
                               #-- Value 2: flat
                               #-- Value 3: downsloping
                               "ca", #  number of major vessels (0-3) colored by flourosopy
                               "thal", # 3 = normal; 6 = fixed defect; 7 = reversable defect
                               "num" 
                               ) 

# Check the Data type of each varaiable in DF
str(heart_disease_df)

#Change the value of some variables to more readable type
### sex (1-> Male, 0 -> Female)
### fasting blood sugar-fbs (1-> True, 0 -> False)
### exercise included angina - exang (1-> yes, 0 -> no)

heart_disease_df$sex = ifelse(heart_disease_df$sex=='1', "M","F" )
heart_disease_df$fbs = ifelse(heart_disease_df$fbs=="1", "T","F")
heart_disease_df$exang = ifelse(heart_disease_df$exang=="1", "Y", "N")
heart_disease_df$num <- ifelse(heart_disease_df$num=="0", "Healthy", "Unhealthy")
str(heart_disease_df)


### Correct the data type of factor data
heart_disease_df$sex <- as.factor(heart_disease_df$sex)
heart_disease_df$cp  <- as.factor(heart_disease_df$cp)
heart_disease_df$fbs <- as.factor(heart_disease_df$fbs)
heart_disease_df$restecg <- as.factor(heart_disease_df$restecg)
heart_disease_df$slope <- as.factor(heart_disease_df$slope)
heart_disease_df$ca <- as.factor(heart_disease_df$ca)
heart_disease_df$thal <- as.factor(heart_disease_df$thal)
heart_disease_df[heart_disease_df=="?"]<-NA
heart_disease_df$num <- as.factor(heart_disease_df$num)
str(heart_disease_df)


###check missing values
apply(is.na(heart_disease_df), 2, which)
heart_disease_df[is.na(heart_disease_df$ca) | is.na(heart_disease_df$thal),]
### Remove them
heart_disease_df <- heart_disease_df[!(is.na(heart_disease_df$ca) | is.na(heart_disease_df$thal)),]
nrow(heart_disease_df)

### Verify that the data is not imbalanced
xtabs(~num + sex, data = heart_disease_df)
xtabs(~num + cp, data = heart_disease_df)
xtabs(~num + fbs, data = heart_disease_df)
xtabs(~num + restecg, data = heart_disease_df)
xtabs(~num + slope, data = heart_disease_df)
xtabs(~num + ca, data = heart_disease_df)
xtabs(~num + thal, data = heart_disease_df)

View(heart_disease_df)
##Predict heart disease by logistic regression
logistic <- glm(num~sex,  data = heart_disease_df,family = binomial())
summary(logistic)

logistic_mul_var <- glm(num ~ ., data = heart_disease_df,  family =  binomial() )
summary(logistic_mul_var)

logistic_mul_var.df<- data.frame(logistic_mul_var$fitted.values )
View(logistic_mul_var.df)
### Calculate R^2
## Null devaince = 2*(0 - LogLikelihood(null model))
##               = -2*LogLikihood(null model)
## Residual deviacne = 2*(0 - LogLikelihood(proposed model))
##                   = -2*LogLikelihood(proposed model)
ll.null <-logistic_mul_var$null.deviance/-2
ll.proposed <- logistic_mul_var$deviance/-2
## McFadden's Pseudo R^2 = [ LL(Null) - LL(Proposed) ] / LL(Null)
(ll.null -ll.proposed)/ll.null

1 - pchisq(2*(ll.proposed-ll.null), df = (length(logistic_mul_var$coefficients)-1))

### Draw the logistic regression graph
predicted_data <-data.frame(probability.of.hd = logistic$fitted.values, sex = heart_disease_df$sex )
View(predicted_data)

ggplot(data = predicted_data, aes(x = sex, y = probability.of.hd)) + geom_point(aes(color = sex), size = 5) +
         xlab('Sex') +
         ylab("Predicted probability of getting heart disease")
xtabs(~probability.of.hd + sex, data = predicted_data)

### Predict for all the variables
predicted_data <-data.frame(probability.of.hd = logistic_mul_var$fitted.values, hd = heart_disease_df$num )
View(predicted_data)

predicted_data <- predicted_data[order(predicted_data$probability.of.hd, decreasing = FALSE),] 
predicted_data$rank <- 1:nrow(predicted_data)


ggplot(data=predicted_data, aes(x=rank, y=probability.of.hd)) +
  geom_point(aes(color=hd), alpha=1, shape=4, stroke=2) +
  xlab("Index") +
  ylab("Predicted probability of getting heart disease")
