library(caret)
library(glmnet)
library(leaps)
library(stringr)
library(lubridate)

#TRAINING OF OUR MODEL
#loading the data 
train <- read.csv('New Data/train.csv')
predictions <- read.csv('Original Data/predictions.csv')

#MOETEN WE HIER REFERENCE CATEGORIES WEGDOEN? 
train = subset(train, select = -c(pid,total_returned,total_sold,net_sales_amount,date_id,sku_size_1,season_Winter,category_Add.Ons,brand_.brand...CanvasCraft,subsubcategory_Waist.Wrap,subcategory_Sweaters.Petite,day_of_the_week_Wednesday,month_05,day_of_the_week_Monday,day_of_the_week_Tuesday,day_of_the_week_Thursday,day_of_the_week_Friday,day_of_the_week_Saturday,day_of_the_week_Sunday))
train$price <- scale(train$price)
##We'll perform K-cross validation manually by changing the seed value and changing our training 
##and test data in each fold
##We'll choose the model with the lowest mean squared error 
##Deze methode skippen vanaf nu 
#set.seed(1) #IS HET NIET BETER HIER EEN VASTE SEED TE ZETTEN? KRIJG JE ONDERAAN BIJ MEAN(RR) SLECHTERE WAARDE
#mse <- rep(0,10)
#for(i in 1:10){
  #set.seed(i)
 # test.indices <- rbind(sample(1:nrow(train), nrow(train)/10))
  #lm.fit <- lm(return_rate ~ price + channel, data = train[-test.indices, ])
  #preds <- predict(lm.fit, train[test.indices, ], )
  #mse[i] <- mean((preds - train$return_rate[test.indices])^2)
#}

###We'll inspect our mean squared errors to check if no errors happened
#print(mse)
###Select the lowest mean squared error
#i <- which.min(mse)
### Run the model again, this is our best model 
#set.seed(i)
#test.indices <- rbind(sample(1:nrow(train), nrow(train)/10))
#best.lm.fit <- lm(return_rate ~ price + channel, data = train[-test.indices, ])

#CV VOLGENS INTERNET 
set.seed(1)
train_control <- trainControl(method = "cv",
                              number = 10)

# training the model by assigning sales column
# as target variable and rest other column
# as independent variable
regfit.full <- regsubsets(return_rate ~ ., data = train, nvmax = 10,really.big = T)
reg.summary <- summary(regfit.full)
par(mfrow = c(2, 1))
plot(reg.summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")

best.lm.fit <- train(return_rate ~ price + channel + category_Trousers, data = train, 
               method = "lm",
               trControl = train_control)


#PREDICTING THE RETURN RATES
##We'll load the test set  
test <- read.csv('New Data/test.csv')
test = subset(test, select = -c(pid,total_returned,total_sold))
test$price <- scale(test$price)

### Use this data set and our best linear regression model to perform the predictions
preds <- predict(best.lm.fit, test)
predictions$return_rate <- preds
###Remove the row names in order to get the correct file type for our submission in the competition
rownames(predictions) <- NULL
predictions 
mean(predictions$return_rate)
### Write out our predictions to 
write.csv(predictions, 'New Data/submission.csv', row.names = FALSE)
