library(dplyr)
library(ggplot2)

email <- read.csv('P12-Email-Offer.csv')
str(email)

ggplot(email, aes(x = Age, y = TookAction)) + 
      geom_point() +
      geom_smooth(method = "glm", method.args = list(family = "binomial"))

# Logistic Regression
model1 <- glm(TookAction ~ Age, family = "binomial", data = email)
model2 <- glm(TookAction ~ Age + Gender, family = "binomial", data = email)
summary(model1)
summary(model2)
ggplot(email, aes(x=Age, y=model2$fitted.values)) +
      geom_point()

cbind( 'Age' = email$Age, 'Gender'=email$Gender,'TookAction' = email$TookAction,"Forecast" = model2$fitted.values)

plot(email$Age,email$TookAction,xlab="Age",ylab="Probability of Taking Action") # plot with body size on x-axis and survival (0 or 1) on y-axis
g=glm(TookAction~Age,family="binomial",data=email) # run a logistic regression model (in this case, generalized linear model with logit link). see ?glm

curve(predict(g,data.frame(Age=x),type="resp"),add=TRUE) # draws a curve based on prediction from logistic regression model

points(email$Age,fitted(g),pch=20) # optional: you could skip this draws an invisible set of points of body size survival based on a 'fit' to glm model. pch= changes type of dots.


