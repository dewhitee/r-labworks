
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
dat = read.csv("2_1.csv")

# Adjust data if necessary
#dat = dat[, 1]
dat

N = length(dat)

v1 = dat$V1
v1N = length(v1)

v2 = dat$V2
v2N = length(v2)


# Useful functions
source("F:/dev/R/Labworks/exam_helpers_script.R")

# Building histogram
#hist(dat[, 1])
hist(v1)
hist(v2)

# Summarizing
summary(dat)
summary(v1)
summary(v2)

#sd(dat)
#var(dat)

# T-Student
st = t.test(v1, v2, alternative = "two.sided", var.equal = FALSE)

# Student Confidence interval
st_critical_value_l = qt(alpha/2, st$parameter) # -1.981872
st_critical_value_r = qt(1-alpha/2, st$parameter) # 1.981872


# Wilcoxon-Mann-Whitney test for homogeneity
wt = wilcox.test(v1, v2, alternative = "two.sided", correct = FALSE)
attributes(wt)	# get names of all test parameters

wt$statistic # 3987

wmw_mean = (v1N*v2N)/2 # 5000
wmw_sd = sqrt((1/12)*v1N*v2N*(v1N+v2N+1)) # 409.2676
wmw_standardized_zvalue = (wt$statistic - wmw_mean) / wmw_sd # -2.475153
wmw_acceptance_region = qnorm(1-alpha/2) # (-1.959964; +1.959964)

# Kolmogorov
kt = ks.test(v1, v2, alternative = "two.side")

kt$statistic # 0.31
kt$p.value # 0.000134

ks_tval = kt$statistic * sqrt((v1N * v2N)/(v1N + v2N)) # 2.192031
ks_acceptance_reg = sqrt(-(1/2)*log(alpha/2)) # 1.358102

### Boxplot
boxplot(v1, v2, names=c("v1", "v2"))


