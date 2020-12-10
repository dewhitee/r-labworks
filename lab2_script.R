rm(list = ls())	# clean all objects from the workspace
# command ‘ls()’ shows you names of all used variables

mydata = read.csv("mydata2_21var.csv", header=F)

mydata = mydata[, 1]
mydata

########## KOLMOGOROV-SMIRNOV TEST

# Null hypothesis is that mydata distribution is NOT SO FAR from (normal/exp/gamma/unif/chi)
# Alternative hypothesis is that mydata distribution is FAR from (normal/exp/gamma/unif/chi)

alpha = 0.05
N = length(mydata) # = 50

acceptance_region = sqrt(-(1/2)*log(alpha/2)) # = 1.358102
adjusted_acceptance_region = acceptance_region / sqrt(N) # = 0.1920646

skew_standard_error = sqrt(6*N*(N-1) / ((N-2)*(N+1)*(N+3))) # must be nearly equal to others
kur_standard_error = sqrt(4*(N^2-1)*skew_standard_error^2 / ((N-3)*(N+5))) # must be less than one

ks_val <- function(ksresult) {
  return(ksresult$statistic * sqrt(50))
}

ks_val_print <- function(ksresult, distname) {
  #val = ks_val(ksresult)
  val = ksresult$statistic
  aregion = adjusted_acceptance_region
  if (val < aregion) {
    cat("K.S (test value) for", distname, "is", val, "and is in the acceptance range (", aregion,")\n")
  } else {
    cat("K.S (test value) for", distname, "is", val, "and is NOT in the acceptance range (", aregion,")\n")
  }
  
  if (ksresult$p.value < aregion) {
    cat("Pvalue for", distname, "is", ksresult$p.value, "and is in the acceptance range (", aregion,")\n")
  } else {
    cat("Pvalue for", distname, "is", ksresult$p.value, "and is NOT in the acceptance range (", aregion,")\n")
  }
  ksresult
}

# Normal distribution
normksresult = ks.test(mydata, "pnorm", mean=mean(mydata), sd=sd(mydata))
ks_val_print(normksresult, "normal dist") # D = test value = 0.068085

# Exponential distribution
expksresult = ks.test(mydata, "pexp", rate=1/mean(mydata))
ks_val_print(expksresult, "exponential dist") # D = test value = 0.52891

# Uniform distribution
uniformksresult = ks.test(mydata, "punif", min=min(mydata), max=max(mydata))
ks_val_print(uniformksresult, "uniform dist") # D = test value = 0.1936

# Gamma distribution
library(e1071) # package is used to calculate skewness and kurtosis
library("fBasics")		# load		

sk = skewness(mydata) # = 0.1534346
kurt = kurtosis(mydata) # = -0.1856019

k = sqrt(2)/sk
l = k/mean(mydata)
gammaksresult = ks.test(mydata, "pgamma", shape=k, rate=l)
ks_val_print(gammaksresult, "gamma dist") # D = test value = 0.28359

# Chi-square distribution
chiksresult = ks.test(mydata, "pchisq", df=mean(mydata))
ks_val_print(chiksresult, "chisq dist") # D = test value = 0.35492

######## Approximate method

#for quanite calculation use command 'qnorm(p)'

coefficient_skew = sqrt((6*(N-2))/((N+1)*(N+3))) # = 0.3264173
coefficient_kur = sqrt(((24*N)*(N-2)*(N-3))/((N+1)^2*(N+3)*(N+5))) # = 0.5975451
adjusted_kur = kurtosis(mydata) + (6/(N+1)) # = -0.0679548

skew_with_se = skewness(mydata) / v_skew # = 0.4558355
kur_with_se = kurtosis(mydata) / v_kur # = -0.2804042

standard_normal_dist_quantile = qnorm(1-alpha/2) # = 1.959964

acceptance_region_skew = qnorm(1-alpha/2)*coefficient_skew # = 0.6397662
acceptance_region_kur = qnorm(1-alpha/2)*coefficient_kur # = 1.171167
