#
### READ DATA
dat = read.csv("V01.csv", sep=";")

y = dat$Y
x1 = dat$X1
x2 = dat$X2

dat = dat[-ncol(dat)]

#### CONSTRUCT REGRESSION
# [-ncol(dat)]
reg = lm(y ~ ., data=dat)
summary(reg)

# Anova
reg_anova = anova(reg)

#### RESIDUALS ANALYSIS: GAUSS-MARKOV THEOREM

res = resid(reg)
plot(res, ylim=c(mean(res)-3*sd(res), mean(res)+3*sd(res)))
abline(h=mean(res), col="red", lwd=2)
abline(h=mean(res)-sd(res), col="darkgreen")
abline(h=mean(res)+sd(res), col="darkgreen")

abline(h=mean(res)-2*sd(res), col="green")
abline(h=mean(res)+2*sd(res), col="green")

abline(h=mean(res)-3*sd(res), col="blue")
abline(h=mean(res)+3*sd(res), col="blue")

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

# removing 52nd elements in all columns of dat
model1 = reggress_model(dat, y, res, 1)

# removing another outliers
model2 = reggress_model(model1$new_dat, model1$new_y, model1$new_res, 2)
model3 = reggress_model(model2$new_dat, model2$new_y, model2$new_res, 3)
model4 = reggress_model(model3$new_dat, model3$new_y, model3$new_res, 4)
#model5 = reggress_model(model4$new_dat, model4$new_y, model4$new_res, 5)

model4_dat_no_x2 = model4$new_dat[,c("X1","X3","X4","X5","X6","X7","X8")]
#model5_dat_no_x2 = model5$new_dat[,c("X1","X3","X4","X5","X6","X7","X8")]
model6 = reggress_model(model4_dat_no_x2, model4$new_y, model4$new_res, 6, FALSE)

model4_dat_no_x2x5 = model4$new_dat[,c("X1","X3","X4","X6","X7","X8")]
#model5_dat_no_x2x5 = model5$new_dat[,c("X1","X3","X4","X6","X7","X8")]
model7 = reggress_model(model4_dat_no_x2x5, model4$new_y, model4$new_res, 7, FALSE)

model4_dat_no_x2x5x7 = model4$new_dat[,c("X1","X3","X4","X6","X8")]
#model5_dat_no_x2x5x7 = model5$new_dat[,c("X1","X3","X4","X6","X8")]
model8 = reggress_model(model4_dat_no_x2x5x7, model4$new_y, model4$new_res, 8, FALSE)

model4_dat_no_x2x5x7x8 = model4$new_dat[,c("X1","X3","X4","X6")]
#model5_dat_no_x2x5x7x8 = model5$new_dat[,c("X1","X3","X4","X6")]
model9 = reggress_model(model4_dat_no_x2x5x7x8, model4$new_y, model4$new_res, 9, FALSE)

################################################
############ NON-LINEAR MODEL

nlreg = lm(y ~ I(x1^2) + I(x2^2), data=dat)
summary(nlreg)
nlreg_anova = anova(nlreg)

nlreg_updated = lm(model9$new_y ~ I(model9$new_dat$X3^2) + I(model9$new_dat$X4^2), data=model9$new_dat)
summary(nlreg_updated)
nlreg_updated_anova = anova(nlreg_updated)
nlreg_updated_ess = sum(nlreg_updated_anova$`Sum Sq`[1:2])

logreg = lm(model9$new_y ~ I(log10(model9$new_dat$X3)) + I(log10(model9$new_dat$X4)), data=model9$new_dat)
summary(logreg)
logreg_anova = anova(logreg)
logreg_ess = sum(logreg_anova$`Sum Sq`[1:2])

################################################
############ STEPWISE

#### FORWARD

min_mod = lm(y ~ 1, data=dat)
max_mod = formula(lm(y ~ ., data=dat))
regF = step(min_mod, direction="forward", scope=max_mod)
summary(regF)

#### BACKWARD

regB = step(lm(y ~ ., data=dat), direction = "backward")
summary(regB)

