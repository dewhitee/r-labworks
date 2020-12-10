# x - 1st set
# y - 2nd set
# z - 3rd set

mydata = read.csv("Data_21var.csv")

x = mydata[["Spec_Length"]]
y = mydata[["Energy"]]
z = mydata[["Passengers"]]

##### Draw scatter plots

plot(x, y, xlab="x", ylab="y", main="Correlation between X and Y") # scatter plot

# Add trend line to the plot	
lm.out = lm(y ~ x)
abline(lm.out, col="red")

# Add 95% confidence intervals for the trend line
newx_xy = seq(min(x),max(x),by = 0.05)
conf_interval = predict(lm.out, newdata=data.frame(x=newx_xy), interval="confidence",
                        level = 0.95)
#cat("Condidence interval for x-y", conf_interval)
lines(newx_xy, conf_interval[,2], col="blue", lty=2)
lines(newx_xy, conf_interval[,3], col="blue", lty=2)


plot(x, z, xlab="x", ylab="z", main="Correlation between X and Z")
lm.out = lm(z ~ x)
abline(lm.out, col="red")
newx_xz = seq(min(x),max(x),by=0.05)
conf_interval = predict(lm.out, newdata=data.frame(x=newx_xz), interval="confidence",
                        level = 0.95)
#cat("Condidence interval for x-z", conf_interval)
lines(newx_xz, conf_interval[,2],col="blue", lty=2)
lines(newx_xz, conf_interval[,3],col="blue", lty=2)


plot(y, z, xlab="y", ylab="z", main="Correlation between Y and Z")
lm.out = lm(z ~ y)
abline(lm.out, col="red")
newy_yz = seq(min(y), max(y), by=0.05)
conf_interval = predict(lm.out, newdata=data.frame(y=newy_yz), interval="confidence",
                        level = 0.95)
#cat("Condidence interval for y-z", conf_interval)
lines(newy_yz, conf_interval[,2],col="blue", lty=2)
lines(newy_yz, conf_interval[,3],col="blue", lty=2)

#plot(z, y, xlab="z", ylab="y", main="Correlation between Z and Y")
#lm.out = lm(y ~ z)
#abline(lm.out, col="red")
#newy_zy = seq(min(z), max(z), by=0.05)
#conf_interval = predict(lm.out, newdata=data.frame(z=newy_zy), interval="confidence",
#                        level = 0.95)
#cat("Condidence interval for y-z", conf_interval)
#lines(newy_zy, conf_interval[,2],col="blue", lty=2)
#lines(newy_zy, conf_interval[,3],col="blue", lty=2)

##### Calcucate correlation coefficients

cor_xy = cor.test(x, y) 
cor_xy
cor_xz = cor.test(x, z)
cor_xz
cor_yz = cor.test(y, z)
cor_yz

# For non-rejection region calculation use function 'qt(p, df)' to know the 
# quantiles of the t distribution. Don't forget to set right p and df parameters 

alpha = 0.05
critical_value_l = qt(alpha/2, cor_xy$parameter) # = -2.039513
critical_value_r = qt(1-alpha/2, cor_xy$parameter) # = 2.039513

##### Partial correlation coefficients
source("F:/dev/R/Labworks/pcor.R")
pcor.test(x, y, z)
pcor.test(x, z, y)
#pcor.test(y, x, z)
pcor.test(y, z, x)

# Multiple correlation coefficient
# r=sqrt((rxy^2+rxz^2-2*rxy*rxz*ryz)/(1-ryz^2));
rxyz = sqrt((cor_xy$estimate^2 + cor_xz$estimate^2 - 2*cor_xy$estimate * cor_xz$estimate * cor_yz$estimate) /
              (1 - cor_yz$estimate^2))
rxyz



