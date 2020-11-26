#setwd("C:\Users\st70983\Downloads")
mydata = read.csv("mydata2_21var.csv", header=F)
#save.image("C:\\Users\\st70983\\Downloads\\workspace")

mydata = mydata[, 1]
mydata

plot(mydata, type="l")

xfit = seq(min(mydata ),max(mydata ),length=100) 
yfit = dnorm(xfit,mean=mean(mydata ),sd=sd(mydata )) 
yfit = yfit*diff(h$mids[1:2])*length(yfit)
h = hist(mydata, ylim=c(0,max(yfit)))
lines(xfit, yfit, col="red", lwd=2)

# cumulative
plot(ecdf(mydata))
plot(ecdf(mydata), verticals = TRUE, do.points = FALSE)
yfit = rnorm(length(mydata ),mean=mean(mydata),sd=sd(mydata))
plot(ecdf(yfit), verticals = TRUE, do.points = FALSE, add=TRUE, col="red")


############ EXAMPLE

rm(list = ls())	# clean all objects from the workspace
			# command ?ls()? shows you names of all used variables

########## LOAD DATA

sunspot.year = read.csv("sunspot_year.csv")   # read data from CSV file

# Since it is an example, the data set is taken from the basic package ?datasets?,  
# so you can work with the set just using it name (without reading from the CSV)

sunspot.year

########## PLOTS

# scatter plot
#plot(sunspot.year)

# histogram
#hist(sunspot.year)

# add normal curve
#h = hist(sunspot.year)
#xfit = seq(min(sunspot.year),max(sunspot.year),length=100) 
#yfit = dnorm(xfit,mean=mean(sunspot.year),sd=sd(sunspot.year)) 
#yfit = yfit*diff(h$mids[1:2])*length(yfit )
#lines(xfit, yfit, col="red", lwd=2)


# cumulative distribution function plot
#plot(ecdf(sunspot.year))
#plot(ecdf(sunspot.year ), verticals = TRUE, do.points = FALSE)
#yfit = rnorm(length(sunspot.year),mean=mean(sunspot.year),sd=sd(sunspot.year))
#plot(ecdf(yfit), verticals = TRUE, do.points = FALSE, add=TRUE, col="red")


########## DESCRIPTIVE STATISTICS

#summary(sunspot.year)
summary(mydata)

sd(mydata)
var(mydata)

# to calculate skewness and kurtosis, you should install and load package ?fBasics? 
# (if you want you can use any other package, to get the right result the main thing )

install.packages("fBasics")	# install
library("fBasics")		# load			

skewness(mydata)
kurtosis(mydata)

# Variance for skewness and kurtosis
N = length(mydata)

v_skew = sqrt(6*N*(N-1) / ((N-2)*(N+1)*(N+3))) # must be nearly equal to others
v_kur = sqrt(4*(N^2-1)*v_skew^2 / ((N-3)*(N+5))) # must be less than one

# Confidence interval for mean
t.test(mydata, conf.level=0.9)$conf.int
t.test(mydata, conf.level=0.95)$conf.int
t.test(mydata, conf.level=0.99)$conf.int

########## QUANTILES

# get quantile value 
quantile(mydata, 0.9)
quantile(mydata, 0.95) 
quantile(mydata, 0.5)

normaldata = seq(min(mydata ),max(mydata ),length=100) 
normaldata = dnorm(xfit,mean=mean(mydata ),sd=sd(mydata ))
quantile(normaldata, 0.9)
quantile(normaldata, 0.95)

# get quantile number knowing quantile value
#ecdf(sunspot.year)( 1.5 )
ecdf(mydata)(1.5)
ecdf(mydata)(2)

ecdf(mydata)(8.2)
1 - ecdf(mydata)(10.5)
