library(dplyr)
library(MASS)
library(caret)
raw_train <- read.csv('P12-Churn-Modelling.csv')
train <- raw_train %>% select(-.data$Surname,-.data$RowNumber) %>% 
      mutate(Geography = factor(Geography,levels=c("France","Spain","Germany"))) %>%
      mutate(Gender = factor(Gender,levels=c("Male","Female")))
   
# Creating dummy variables
Female <- model.matrix(~train$Gender,contrasts.arg = 'Male')[,2]
Spain <- model.matrix(~train$Geography, contrasts.arg='France')[,2]
Germany <- model.matrix(~train$Geography, contrasts.arg='France')[,3]

train <- cbind(train, Female,Spain,Germany)

confMat <- function(mod) {
      p_hat <- factor(ifelse(mod$fitted.values > 0.5, 1,0))
      confusionMatrix(factor(train$Exited),p_hat)
}

mcf_r <- function(mod) {
      1-(mod$deviance / mod$null.deviance)
}


# Full model
model1 <- glm(Exited ~ CreditScore + Age + Tenure + Balance + NumOfProducts +
                    HasCrCard + IsActiveMember + EstimatedSalary + 
                    Female + Spain + Germany, family="binomial", data = train)

summary(model1)

# Confustion Matrix for Model 1
confMat(model1)

# McFadden R_Squared for Model 1
mcf_r(model1)

# Removing Spain from the model
model2 <- glm(Exited ~ CreditScore + Age + Tenure + Balance + NumOfProducts +
                    HasCrCard + IsActiveMember + EstimatedSalary + 
                    Female + Germany, family="binomial", data = train)

summary(model2)

# Confustion Matrix for Model 2
confMat(model2)

# McFadden R_Squared for Model 2
mcf_r(model2)

# Removing HasCrCard from the model
model3 <- glm(Exited ~ CreditScore + Age + Tenure + Balance + NumOfProducts +
                    IsActiveMember + EstimatedSalary + 
                    Female + Germany, family="binomial", data = train)

summary(model3)

# Confustion Matrix for Model 3
confMat(model3)

# McFadden R_Squared for Model 3
mcf_r(model3)

# Removing EstSal from the model
model4 <- glm(Exited ~ CreditScore + Age + Tenure + Balance + NumOfProducts +
                    IsActiveMember + Female + Germany, family="binomial", data = train)

summary(model4)

# Confustion Matrix for Model 4
confMat(model4)

# McFadden R_Squared for Model 4
mcf_r(model4)

# Removing Tenure from the model
model5 <- glm(Exited ~ CreditScore + Age + Balance + NumOfProducts +
                    IsActiveMember + Female + Germany, family="binomial", data = train)

summary(model5)

# Confustion Matrix for Model 5
confMat(model5)

# McFadden R_Squared for Model 4
mcf_r(model5)

# Keeping Model 4 - after comparing R_Squared for models 4 and 5, decided to keep
# Tenure in

#------------------------------------------------------------------------------#

summary(model4)
confMat(model4)
mcf_r(model4)

# transformation of Balance (log 10 + 1)
train <- train %>% mutate(LogBalance = log10(Balance + 1))

model4a <- glm(Exited ~ CreditScore + Age + Tenure + LogBalance + NumOfProducts +
                              Germany, family="binomial", data = train)

# model not better but not much worse.  Logically better to use Log

#------------------------------------------------------------------------------#

# checking for interactions
train <- train %>% mutate(WealthAccum = Balance/Age)

model4b <- glm(Exited ~ CreditScore + Age + Tenure + LogBalance + NumOfProducts +
                     IsActiveMember + Female + 
                     Germany + WealthAccum, family="binomial", data = train)


# running model without LogBalance to check for signs of multicollinearity
model4c <- glm(Exited ~ CreditScore + Age + Tenure + NumOfProducts +
                     IsActiveMember + Female + 
                     Germany + WealthAccum, family="binomial", data = train)

train <- train %>% mutate(LogWealthAccum = log10(Balance/Age + 1))

model4d <- glm(Exited ~ CreditScore + Age + Tenure + LogBalance + NumOfProducts +
                     IsActiveMember + Female + 
                     Germany + LogWealthAccum, family="binomial", data = train)
# checking for multicollinearity
vif(model4d)

# found multicollinearity 
model4e <- glm(Exited ~ CreditScore + Age + Tenure + NumOfProducts +
                     IsActiveMember + Female + 
                     Germany + LogWealthAccum, family="binomial", data = train)

model4f <- glm(Exited ~ CreditScore + Age + Tenure + LogBalance + NumOfProducts +
                     IsActiveMember + Female + 
                     Germany, family="binomial", data = train)
#best mcf_r
mcf_r(model4f)

#------------------------------------------------------------------------------#
res <- cor(train[,c(2,4,5,7,9)])
round(res, 2)