#Loading the dataset from the given URL
student_scores=read.csv("http://bit.ly/w-data")


#Importing the libraries
library(dplyr)
library(ggplot2)


#Plotting the distribution of Scores with respect to hours
ggplot(student_scores, aes(x=Hours, y=Scores))+geom_point()
##We can see a positive linear relationship between Percentage of Scores and the no. of Hours studied


#Data preparation
glimpse(student_scores)
##Both Scores and Hours are numeric


#Splitting the data into train and test data
set.seed(2)
s=sample(1:nrow(student_scores),0.70*nrow(student_scores))
train=student_scores[s,]
test=student_scores[-s,]


#Training the algorithm
fit=lm(Scores~.,data=train)
summary(fit)

formula(fit)
fit=lm(Scores ~ Hours,data=train)


#Making predictions on the test data
test_pred=predict(fit, newdata = test)


#Actual Vs Predicted
train_res=cbind.data.frame(Actual=test$Scores, Predicted=test_pred)


#Evaluating the model performance of test data using Root Mean Square Errors (RMSE)
errors=test$Scores-train_res$Predicted
rmse=sqrt(mean(errors)^2)
rmse
## The RMSE of the model is 1.33 


#Building the model on the entire training data set
fit_final=lm(Scores~.,data=student_scores)
summary(fit_final)


#Predicting the score if a student studies 9.25 hours/day
predicted_score = predict(fit_final, newdata = data.frame(Hours = 9.25))
predicted_score
## The predicted score is 92.91