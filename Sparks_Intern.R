#We have been provided with the raw data having two variables, 'Hours' & 'Scores', out of which we have to produce a lm for predicting the scores related to no. of hours invested in preparation!
#Make a dataframe out of the data given.
Scores <- data.frame(Hours = c(2.5, 5.1, 3.2, 8.5, 3.5, 1.5, 9.2, 5.5, 8.3, 2.7, 7.7, 5.9, 4.5, 3.3, 1.1, 8.9, 2.5, 1.9, 6.1, 7.4, 2.7, 4.8, 3.8, 6.9, 7.8), Score = c(21, 47, 27, 75, 30, 20, 88, 60, 81, 25, 85, 62, 41, 42, 17, 95, 30, 24, 67, 69, 30, 54, 35, 76, 86))
View(Scores)
#We see that both the variables are of numeric class, hence we'll perform a regression type of SVM as we'll be predicting the scores, which is again a numerical data.
library(dplyr)
library(ggplot2)
#Lets just see how our data looks!
summary(Scores)
ggplot(Scores, aes(x=Score, y=Hours)) + geom_point()
#We clearly see that, more the study hours, more will be the marks scored.
#From the scatter plot, we see that a linear relation already exists between the variables.


#Let us fit a linear model!
model <- lm(Score ~ Hours, data = Scores)
predict(model)
#Let us compare our predictions with the original outcomes!
Scores$Scores_predict <- predict(model)
ggplot(Scores, aes(x=Scores_predict, y= Score)) + geom_point() + geom_abline(color = "darkblue") + ggtitle("Prediction plot")
#We can have a residual plot if we want for evaluatiing the model but we'll have follow accurate methods(RMSE & Rsquared) for evaluating our model, which we will do after few steps.
#let us now predict the model on new data. For this, lets consider the data (9.25 hours) given to us in this project task.
new_data <- data.frame(Hours = 9.25)
predict(model, newdata = new_data)
#We found that, with 9.25 hours of study time, one will score 92.90985 ~ 93


#Let us now evaluate our model for its validation.
#1)RMSE
error <- Scores$Score - Scores$Scores_pred
mean_error <- mean(error^2)
RMSE <- sqrt(mean_error)
RMSE
#We see that our RMSE is 5.374266.
sd(Scores$Score)
#sd is 25.28689.
#We can clearly say that our model's predictions are way better than just taking the average of the actual prediction as RMSE < sd.
#2)Rsquared
#Rss- Residual sum of squared errors, Tss- Total sum of squared errors.
RSS <- sum((Scores$Score - Scores$Scores_predict)^2)
TSS <- sum((Scores$Score - mean(Scores$Score))^2)
Rsquared <- 1 - RSS/TSS
Rsquared
#Rsquared valuefor our model is 0.9529482, which shows that our model fits or explains the data very well.
