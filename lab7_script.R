

dat = read.csv("V01.csv", sep=";")

y = dat$Y
xs = dat[-ncol(dat)]

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

# Create time series of your data
#tsdat = ts(dat) 	# only if necessary

# Plot data
plot.ts(tsdat)

# Boxplot
boxplot(tsdat ~ cycle(tsdat))

### Autocorrelation coefficient

acfRes = acf(tsdat) # autocorrelation
pacfRes = pacf(tsdat)  # partial autocorrelation

### MOVING AVERAGE

plot.ts(tsdat)

tsdatSMA3 = SMA(tsdat,n=3)	# lag 3
tsdatSMA5 = SMA(tsdat,n=5)	# lag 5
tsdatSMA10 = SMA(tsdat,n=10)	# lag 10

lines(tsdatSMA3, col="red")
lines(tsdatSMA5, col="green")
lines(tsdatSMA10, col="blue")

### SIMPLE MOVING AVERAGE (built-in functions)
N = length(as.numeric(tsdat))

# SMA (last 10 observations)
sm = sma(tsdat, h=10, holdout = TRUE, silent=FALSE,interval="l")
forecast(sm)
fc = forecast(sm)$forecast

errors(as.numeric(tsdat)[(N-9):N], fc) # analysis of SMA model


# SMA (the last observation)
sm = sma(tsdat, h=1, holdout = TRUE, silent=FALSE,interval="l")
forecast(sm)
fc = forecast(sm)$forecast
forecast(sm)$lower[1]
forecast(sm)$upper[1]

errors(as.numeric(tsdat)[N], fc) # analysis of SMA model



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

errors(as.numeric(tsdat)[N], frct[1]) # analysis of SMA model



