
### LOAD PACKAGES & FUNCTIONS
install.packages(c("smooth", "Mcomp", "TTR")) # download packages

library(smooth)	
library(Mcomp)
library(TTR)

errors = function(actual, predicted){
  d = actual-predicted
  mse = mean((d)^2)
  mae = mean(abs(d))
  mape = mean(abs(d/actual))*100
  rmse = sqrt(mse)
  
  return(cat(" MAE:", mae, "\n", "MSE:", mse, "\n", 
             "MAPE:", mape, "%", "\n", "RMSE:", rmse, "\n"))
}



##################################################
### dat - loaded data set 

dat = AirPassengers

# Create time series of your data
#tsdat = ts(dat) 	# only if necessary
tsdat = dat

# Plot data
plot.ts(tsdat, xlab="Year", ylab="Passenger count")

# Plot data with x axis as years
#plot.ts(dat)

# Boxplot
boxplot(tsdat ~ cycle(tsdat), xlab="Month", ylab="Passenger count")

### Autocorrelation coefficient

acfRes = acf(tsdat) # autocorrelation
pacfRes = pacf(tsdat)  # partial autocorrelation

### MOVING AVERAGE

plot.ts(tsdat, xlab="Year", ylab="Passenger count")

tsdatSMA3 = SMA(tsdat,n=3)	# lag 3
tsdatSMA5 = SMA(tsdat,n=5)	# lag 5
tsdatSMA10 = SMA(tsdat,n=10)	# lag 10

lines(tsdatSMA3, col="red")
lines(tsdatSMA5, col="green")
lines(tsdatSMA10, col="blue")

legend(1, 1, legend=c("Lag 3", "Lag 5", "Lag 10"), col=c("red", "green", "blue"),
                       lty=1:2, cex=0.8)

### SIMPLE MOVING AVERAGE (built-in functions)
N = length(as.numeric(tsdat))

# SMA (last 10 observations)
sm1 = sma(tsdat, h=10, holdout = TRUE, silent=FALSE,interval="l")
forecast(sm1)
fc1 = forecast(sm1)$forecast

# Actual value
as.numeric(tsdat)[(N-9):N]

errors(as.numeric(tsdat)[(N-9):N], fc1) # analysis of SMA model


# SMA (the last observation)
sm2 = sma(tsdat, h=1, holdout = TRUE, silent=FALSE,interval="l")
forecast(sm2)
fc2 = forecast(sm2)$forecast
forecast(sm2)$lower[1]
forecast(sm2)$upper[1]

# Actual value
as.numeric(tsdat)[N]

errors(as.numeric(tsdat)[N], fc2) # analysis of SMA model



### AUTOREGRESSION: AR(1)

vdat = as.numeric(tsdat)[-N]
vN = length(vdat)

vdat_level = vdat[-1]
vdat_lags = vdat[-vN]

armod = lm(vdat_level ~ vdat_lags)
summary(armod)

# Forecast
new = data.frame(vdat_lags = as.numeric(tsdat)[N])
frct = predict(armod, newdata =new, interval="confidence", level=0.95)

# Actual value
as.numeric(tsdat)[N]

errors(as.numeric(tsdat)[N], frct[1]) # analysis of SMA model



