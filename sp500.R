# R Script for Forecasting and Volatility Modeling
# of S&P 500 Index Closing Value Log Returns

#Loading the MSGARCH package
library(MSGARCH)
#Loading the tseries package
library(tseries)

#Function to calculate the root mean squared error
rmse <- function(error)
{
  sqrt(mean(error^2))
}

#Importing the S&P 500 index closing value log return time series dataset
data("sp500")

#Plotting the 'sp500' time series
plot.ts(sp500)

#Plotting the Auto Correlation Function for sp500
acf(sp500)

#Plotting the Partial Auto Correlation Function for sp500
pacf(sp500)

#Performing the Ljung-Box and Box-Pierce test for GARCH effects
Box.test(sp500,lag=4,type=c("Box-Pierce","Ljung-Box"),fitdf = 0)

#Finding the 70-30 split marker for the data
marker = ceiling(0.7*length(sp500))

#Now we split the data into training and testing sets

#The training data consists of 70% of the original sp500 dataset
train = sp500[2:marker]
marker = marker + 1

#The testing data consists of 30% of the original sp500 dataset
test = sp500[marker:4528]

#Plotting the training data time series
plot.ts(train)

#Plotting the testing data time series
plot.ts(test)

#Creating a MSGARCH model specification with 2 regimes having normal probability distribution
#which have no skew and are regime independent
spec = create.spec(model = c("sGARCH","sGARCH"), distribution = c("norm","norm"),do.skew = c(FALSE,FALSE), do.mix = FALSE, do.shape.ind = FALSE)

#Method to perform Maximum Likelihood Estimation (MLE) of a MSGARCH Specification
#on the training dataset
fit = fit.mle(spec = spec, y = train, ctr = list(do.init = FALSE))

#Summary of the model
summary(fit)

#Displays the transition matrix of the model
transmat(fit)

#Forecasting on test dataset using pred method
prediction = pred(object = fit, x=test, log = FALSE, do.its = TRUE)  

dev = prediction$pred

#Extracting the actual values of test dataset
actual = prediction$x


#Extracting only the predicted values from the forecast : pred$x+pred$pred
predicted <- mapply(function(actual,dev) actual[[1]]+dev[[1]], actual,dev)


#Plotting the actual test dataset
plot(ts(actual))

#Plotting the forecasted values
plot(ts(predicted))

#Calculating the error
error = actual - predicted
error = error[2:1340]

#Calculating the root mean squared error of the forecast 
rmse(error)

#Plotting the residuals
plot.ts(error)

#Method returning the unconditional volatility of the process in each state.
unc.vol = MSGARCH::unc.vol(object = fit)
View(unc.vol)
