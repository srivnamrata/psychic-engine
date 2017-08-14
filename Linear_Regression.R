setwd("D:/AP/linear")

insurance = read.csv("insurance.csv")
head(insurance)

### pre checks 

### target variable should follow normal distribution 

hist(insurance$charges)

### target variable is right skewed 
## apply transformation 

insurance$log_charges = log( insurance$charges)

hist(insurance$log_charges)

### linear relationship b/w input and target 

plot(insurance$age, insurance$log_charges)
plot(insurance$age , insurance$charges)

### multicollinearity ( input variables are correlated with each other)

cor( insurance$age, insurance$log_charges) ## Correlation with taarget variabel 
 cor( insurance$age, insurance$bmi) ### cor with input variable ( not desirable)
 
 cor( insurance$bmi, insurance$log_charges) ## cor with target variable 
 
 
ggplot( insurance, aes( smoker, log_charges)) + geom_boxplot()


### MOdel bulding  

insurance$charges = NULL 

## train and test set split 

set.seed(675)

ids = sample( nrow(insurance), nrow(insurance)*0.8)

train = insurance[ids,]
test = insurance[-ids,]

## model 

lin_model = lm( log_charges ~ . , data=train )

## Test the model 

test$pred = predict(lin_model, newdata=test )

summary(lin_model)

### RMSE 

test$error = test$log_charges - test$pred

test$error_sq = test$error ** 2

rmse = sqrt(mean(test$error_sq))

0.4/9
