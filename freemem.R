#Loading the MSGARCH package
library(MSGARCH)
#Loading the tseries package
library(tseries)

#Function to calculate the root mean squared error
rmse <- function(error)
{
  sqrt(mean(error^2))
}

#Reading 'freemem.csv'
freemem = read.csv('/home/samyak/5th Sem Local/Time Series Analysis/Project/freemem.csv',header=F)

#Converting freemem to a time series
freemem = ts(freemem)

#Plotting the 'freemem' time series
plot.ts(freemem)

#Plotting the Auto Correlation Function for freemem
acf(freemem)

#Plotting the Partial Auto Correlation Function for freemem
pacf(freemem)

#Performing the Ljung-Box and Box-Pierce test for GARCH effects
Box.test(freemem,lag=4,type=c("Box-Pierce","Ljung-Box"),fitdf = 0)

#Finding the 70-30 split marker for the data
marker = 450

#Now we split the data into training and testing sets

#The training data consists of 70% of the original freemem dataset
train = freemem[2:marker]
marker = marker + 1

#The testing data consists of 30% of the original freemem dataset
test = freemem[marker:500]

#Plotting the training data time series
plot.ts(train)

#Plotting the testing data time series
plot.ts(test)

#Creating a MSGARCH model specification with 2 regimes having normal probability distribution
#which have no skew and are regime independent
spec = create.spec(model = c("sGARCH","sGARCH"), distribution = c("norm","norm"),
                   do.skew = c(FALSE,FALSE), do.mix = FALSE, do.shape.ind = FALSE)

#Method to perform Maximum Likelihood Estimation (MLE) of a MSGARCH Specification
#on the training dataset
fit = fit.mle(spec = spec, y = freemem, ctr = list(do.init = FALSE))

#Summary of the model
summary(fit)

#Displays the transition matrix of the model
transmat(fit)

#Forecasting on test dataset using pred method
prediction = pred(object = fit, x=test, log = FALSE, do.its = FALSE)  

dev = prediction$pred

#Extracting the actual values of test dataset
actual = prediction$x

#Extracting only the predicted values from the forecast
predicted <- mapply(function(actual, dev) actual[[1]]+dev[[1]], actual, dev)

#Plotting the actual test dataset
plot(ts(actual))

#Plotting the forecasted values
plot(ts(predicted))

#Calculating the error
error = actual - predicted

#Calculating the root mean squared error of the forecast 
rmse(error)

#Plotting the residuals
plot.ts(error)

#Method returning the unconditional volatility of the process in each state.
unc_vol = unc.vol(object = fit)

View(unc_vol)
