# x - 1st set
# y - 2nd set
# mysample_index = sample(seq_len(nrow(mydata), size = nrow(mydata)))
mydata = read.csv("mydata2_21var.csv", header=F)

mydata = mydata[, 1]

x = mydata[1:25]
y = mydata[26:50]

### t-Student's test for homogeneity
ttestresult = t.test(x, y, alternative = "two.sided", var.equal = FALSE)

alpha = 0.05

critical_value_l = qt(alpha/2, ttestresult$parameter) # = -2.010635
critical_value_r = qt(1-alpha/2, ttestresult$parameter) # = 2.010635

### Wilcoxon-Mann-Whitney test for homogeneity
wt = wilcox.test(x, y, alternative = "two.sided", correct = FALSE)
attributes(wt)	# get names of all test parameters

wt$statistic	# get WMW test statistic value = 361.5

N1 = length(x) # = 25
N2 = length(y) # = 25

mwm_mean = (N1*N2)/2 # = 312.5
mwm_sd = sqrt((1/12)*N1*N2*(N1+N2+1)) # = 51.53882
mwm_zvalue = qnorm(1-alpha/2) # = 1.959964
mwm_standardized_zvalue = (wt$statistic - mwm_mean) / mwm_sd # = 0.9507397
mwm_acceptance_region = qnorm(1-alpha/2) # = 1.959964

### Kolmogorov-Smirnov test for homogeneity
kt = ks.test(x, y, alternative = "two.side")

kt$statistic # D = 0.24
kt$p.value # pvalue = 0.4676
  
ks_tvalue = kt$statistic * sqrt((N1 * N2)/(N1 + N2)) #  = 0.8485281 
ks_acceptance_region = sqrt(-log(alpha/2)/2) # = 1.358102

### Boxplot
boxplot(x, y, names=c("x", "y"))
