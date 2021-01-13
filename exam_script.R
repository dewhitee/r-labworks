
##### loading the data set
#dat = read.csv("myvariant.csv")

##### (optional) get xs and y
#y = dat$Y

# If Y column in dat is the last column in the table
#dat = dat[-ncol(dat)]

# If Y column in dat is the first column in the table
#dat = dat[-1,]

#####
#dat

# Variance for skewness and kurtosis
#N = length(mydata)

#v_skew = sqrt(6*N*(N-1) / ((N-2)*(N+1)*(N+3))) # must be nearly equal to others
#v_kur = sqrt(4*(N^2-1)*v_skew^2 / ((N-3)*(N+5))) # must be less than one


# LAB 1 - Знакомство с программой R. Нахождение описательной статистики.
# LAB 2 - Проверка статистических гипотез. Проверка гипотезы о виде распределения генеральной совокупности.
# LAB 3 - Проверка статистичекских гипотез. Проверка гипотез об однородности.
# LAB 4 - Исследование статистических зависимостей. Корреляционный анализ.
# LAB 5 - Исследование статистических зависимостей. Однофакторный регрессионный анализ.
# LAB 6 - Исследование статистических зависимостей. Многофакторный регрессонный анализ.
# LAB 7 - Анализ временный рядов.

#############
# - For histograms, ecdf plots, quantiles, skewness and kurtosis calculation - use 1st lab
# - To check distribution of the data - use code from the 2nd lab
# - To check the homogeinity of the two pieces from one data population (x[1:25] and y[26:50] from mydata) - use code from 3rd lab
# - To check how y and x are correlated - use code from 4th lab
# - For one-factor regression - use 5-th lab
# - For multi-factor regression - use 6-th lab
# - For autocorrelation and partial autocorrelation - use 7th lab

# (for example)
# 1) hist (initial conclusion)
# 2) summary
# 3) ks.test for (norm\exp\unif etc.) (conclusion about general population from the given sample data (mydata))
# 4) founding confidence interval
# 5) explaining everything in ZOOM


###### ...
# Initials
alpha = 0.05

# Reading data
dat = read.csv("mydata2_21var.csv", sep=";", header=F)

# Adjust data if necessary
dat = dat[, 1]
dat

N = length(dat)

# Useful functions
source("F:/dev/R/Labworks/exam_helpers_script.R")

# Plotting
plot(dat, xlab="x", ylab="y", main="Title of the plot")

# Building histogram
hist(dat)

# add more lines on the histogram (for example, to show the normal distribution red line)
#xfit = seq(min(mydata), max(mydata), length=100) 
#yfit = dnorm(xfit, mean=mean(mydata), sd=sd(mydata)) 
#yfit = yfit * diff(h$mids[1:2]) * length(yfit)
#h = hist(mydata, ylim=c(0, max(yfit)))
#lines(xfit, yfit, col="red", lwd=2)

# Summarizing
summary(dat)
sd(dat)
var(dat)

# Run my variant test function

ks_test_for_normal(dat, N)
ks_test_for_gamma(dat, N)
ks_test_for_exponential(dat, N)
ks_test_for_chisquare(dat, N)

# Confidence interval
acceptance_region = ks_acceptance_region()


###### ...


