alpha = 0.05

#library(e1071) # package is used to calculate skewness and kurtosis
library("fBasics")		# load
library(smooth)	
library(Mcomp)
library(TTR)

# Defining some useful variables and functions
v_skew <- function(N) { return(sqrt(6*N*(N-1) / ((N-2)*(N+1)*(N+3)))) } # must be nearly equal to others
v_kur <- function(N) { return(sqrt(4*(N^2-1)*v_skew(N)^2 / ((N-3)*(N+5)))) } # must be less than one
coefficient_skew <- function(N) { return(sqrt((6*(N-2))/((N+1)*(N+3)))) }
coefficient_kur <- function(N) { return(sqrt(((24*N)*(N-2)*(N-3))/((N+1)^2*(N+3)*(N+5)))) }
adjusted_kur <- function(dat, N) { return(kurtosis(dat) + (6/(N+1))) }

standard_normal_dist_quantile <- function(N) { return(qnorm(1-alpha/2)) }

acceptance_region_skew <- function(N) { return(qnorm(1-alpha/2)*coefficient_skew(N)) }
acceptance_region_kur <- function(N) { return(qnorm(1-alpha/2)*coefficient_kur(N)) }

ks_acceptance_region <- function() { return(sqrt(-(1/2)*log(alpha/2))) }
ks_tvalue <- function(kstestresult, N1, N2) { return(kstestresult$statistic * sqrt((N1 * N2)/(N1 + N2))) }
ks_acceptance_region_2 <- function() { return(sqrt(-log(alpha/2)/2)) }
ks_adjusted_acceptance_region <- function(N) { return(ks_acceptance_region() / sqrt(N)) }
ks_val_print <- function(ksresult, distname, N) {
  val = ksresult$statistic
  aregion = ks_adjusted_acceptance_region(N)
  if (val < aregion) {
    cat("K.S (test value) for", distname, "is", val, "and is in the acceptance range (", aregion, ")\n")
  } else {
    cat("K.S (test value) for", distname, "is", val, "and is NOT in the acceptance range (", aregion, ")\n")
  }
  
  if (ksresult$p.value > alpha) {
    cat("Pvalue for", distname, "is", ksresult$p.value, "and is more than alpha (", alpha, ")\n")
  } else {
    cat("Pvalue for", distname, "is", ksresult$p.value, "and is less than alpha (", alpha, ")\n")
  }
  ksresult
}
ks_test_for_normal <- function(mydata, N) {
  # Normal distribution
  normksresult = ks.test(mydata, "pnorm", mean=mean(mydata), sd=sd(mydata))
  ks_val_print(normksresult, "normal dist", N) # D = test value = 0.068085
  return(normksresult)
}
ks_test_for_exponential <- function(mydata, N) {
  # Exponential distribution
  expksresult = ks.test(mydata, "pexp", rate=1/mean(mydata))
  ks_val_print(expksresult, "exponential dist", N) # D = test value = 0.52891
  return(expksresult)
}
ks_test_for_uniform <- function(mydata, N) {
  # Uniform distribution
  uniformksresult = ks.test(mydata, "punif", min=min(mydata), max=max(mydata))
  ks_val_print(uniformksresult, "uniform dist", N) # D = test value = 0.1936
  return(uniformksresult)
}
ks_test_for_gamma <- function(mydata, N) {
  # Gamma distribution
  sk = skewness(mydata) # = 0.1534346
  k = sqrt(2)/sk
  l = k/mean(mydata)
  gammaksresult = ks.test(mydata, "pgamma", shape=k, rate=l)
  ks_val_print(gammaksresult, "gamma dist", N) # D = test value = 0.28359
  return(gammaksresult)
}
ks_test_for_chisquare <- function(mydata, N) {
  # Chi-square distribution
  chiksresult = ks.test(mydata, "pchisq", df=mean(mydata))
  ks_val_print(chiksresult, "chisq dist", N) # D = test value = 0.35492
  return(chiksresult)
}

student_critical_value_l <- function(ttestresult) { return(qt(alpha/2, ttestresult$parameter)) }
student_critical_value_r <- function(ttestresult) { return(qt(1-alpha/2, ttestresult$parameter)) }

mwm_mean <- function(N1, N2) { return((N1*N2)/2) }
mwm_sd <- function(N1, N2) { return(sqrt((1/12)*N1*N2*(N1+N2+1))) }
mwm_zvalue <- function() { return(qnorm(1-alpha/2)) }
mwm_standardized_zvalue <- function(wilcoxtestresult, N1, N2) { return((wilcoxtestresult$statistic - mwm_mean(N1, N2)) / mwm_sd(N1, N2)) }
mwm_acceptance_region <- function() { return(qnorm(1-alpha/2)) }

f_acceptance_region <- function(p, anovaresult) { return(qf(p, anovaresult$Df[1], anovaresult$Df[2])) }

# Find outliers
get_without_outliers <- function(data, resids) {
  return(subset(data, resids > mean(resids)-2*sd(resids) & resids < mean(resids)+2*sd(resids)))
}

get_last_outlier_index <- function(data, resids) {
  last_outlier_index = which.max(abs(resids))
  print(paste("Index of max resid =", last_outlier_index, "(removing)"))
  return(last_outlier_index)
}

plot_new_resids <- function(newreg, i) {
  res_without_outliers = resid(newreg)
  plot(res_without_outliers, main=paste("Table", i),
       ylim=c(mean(res_without_outliers)-3*sd(res_without_outliers), 
              mean(res_without_outliers)+3*sd(res_without_outliers)))
  abline(h=mean(res_without_outliers), col="red", lwd=2)
  abline(h=mean(res_without_outliers)-sd(res_without_outliers), col="darkgreen")
  abline(h=mean(res_without_outliers)+sd(res_without_outliers), col="darkgreen")
  
  abline(h=mean(res_without_outliers)-2*sd(res_without_outliers), col="green")
  abline(h=mean(res_without_outliers)+2*sd(res_without_outliers), col="green")
  
  abline(h=mean(res_without_outliers)-3*sd(res_without_outliers), col="blue")
  abline(h=mean(res_without_outliers)+3*sd(res_without_outliers), col="blue")
  return(res_without_outliers)
}

reggress_model <- function(prev_dat, prev_y, prev_res, index, exclude_outliers=TRUE) {
  print(paste("Index", index))
  new_dat = get_without_outliers(prev_dat, prev_res)
  new_y = get_without_outliers(prev_y, prev_res)
  
  if (exclude_outliers == FALSE) {
    new_dat = prev_dat
    new_y = prev_y
  } else {
    if (length(prev_y) - length(new_y) != 0) {
      print(paste("Outliers found! (", length(prev_y) - length(new_y), ")"))
      last_outlier_index = get_last_outlier_index(prev_dat, prev_res)
      new_dat = prev_dat[-c(last_outlier_index),]
      new_y = prev_y[-c(last_outlier_index)]
    } else {
      print("No outliers found.")
    }
  }
  
  #print(paste("Removed", length(prev_y) - length(new_y), "observations."))
  new_reg = lm(new_y ~ ., data=new_dat)
  print(summary(new_reg))
  anova_result = anova(new_reg)
  print(anova_result)
  print(paste("Residuals sum sq =", tail(anova_result$`Sum Sq`, 1)))
  print(paste("Residuals df =", tail(anova_result$Df, 1)))
  print(paste("SEE =", sqrt(tail(anova_result$`Sum Sq`,1)/tail(anova_result$Df, 1))))
  new_res = plot_new_resids(new_reg, index)
  return(list("new_dat" = new_dat, "new_y" = new_y, "new_res" = new_res, "anova_res" = anova_result))
}

