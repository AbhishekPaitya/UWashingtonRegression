library(tidyverse)



# import datasets
house_data <- read.csv("file:///C:/Users/Abhishek.Paitya/Desktop/coursera/UWashington - Regression/week 1/datasets/kc_house_data.csv")
house_test_data <- read.csv("file:///C:/Users/Abhishek.Paitya/Desktop/coursera/UWashington - Regression/week 1/datasets/kc_house_test_data.csv")
house_train_data <- read.csv("file:///C:/Users/Abhishek.Paitya/Desktop/coursera/UWashington - Regression/week 1/datasets/kc_house_train_data.csv")

house_train <- house_train_data %>% select(price, sqft_living)
linear.model.fit <- lm(data = house_train, price ~ sqft_living)
summary(linear.model.fit)

model.bedroom <- lm(data = house_train_data, price ~ bedrooms)

model <- data.frame(t(linear.model.fit$coefficients))
model2 <- data.frame(t(model.bedroom$coefficients))
house_train$residual <- linear.model.fit$residuals
house_train$fitted.values <- linear.model.fit$fitted.values

predict.price <- function(sqft){
  price <- model$X.Intercept. + (model$sqft_living * sqft)
  return(price)
}

inverse.predict <- function(price){
  sq_ft <- (price - model$X.Intercept.)/model$sqft_living
  return(sq_ft)
}

house_train$ape <- abs(house_train$residual*100)/house_train$price
house_train$mape <- mean(house_train$ape)
house_train$residual_sq <- (house_train$residual)^2


house_test <- house_test_data %>% select(price,sqft_living)
house_test$intercept <- model$X.Intercept.
house_test$intercept2 <- model2$X.Intercept.
house_test$sqft_living_Coeff <- model$sqft_living
house_test$bedroom_coeff <- model2$bedrooms
house_test$pred_price <- house_test$intercept + house_test$sqft_living_Coeff * house_test$sqft_living
house_test$residual <- house_test$pred_price - house_test$price
house_test$ape <- abs(house_test$residual*100)/house_test$price
house_test$mape <- mean(house_test$ape)
house_test$residual_sq <- (house_test$residual)^2
house_test$bedroom <- house_test_data$bedrooms
house_test$bedroom_pred_price <- house_test$intercept2 + house_test$bedroom_coeff * house_test$bedroom
house_test$bedroom_resid_sq <- (house_test$bedroom_pred_price - house_test$price)^2
