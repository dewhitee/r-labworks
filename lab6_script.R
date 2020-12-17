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

reggress_model <- function(prev_dat, prev_y, prev_res, index) {
  new_dat = get_without_outliers(prev_dat, prev_res)
  new_y = get_without_outliers(prev_y, prev_res)
  new_reg = lm(new_y ~ ., data=new_dat)
  summary(new_reg)
  anova(new_reg)
  new_res = plot_new_resids(new_reg, index)
  return(list("new_dat" = new_dat, "new_y" = new_y, "new_res" = new_res))
}

# removing 52nd elements in all columns of dat
model1 = reggress_model(dat, y, res, 1)

# removing another outliers
model2 = reggress_model(model1$new_dat, model1$new_y, model1$new_res, 2)
model3 = reggress_model(model2$new_dat, model2$new_y, model2$new_res, 3)
model4 = reggress_model(model3$new_dat, model3$new_y, model3$new_res, 4)
model5 = reggress_model(model4$new_dat, model4$new_y, model4$new_res, 5)

################################################
############ NON-LINEAR MODEL

nlreg = lm(y ~ I(x1^2) + I(x2^2), data=dat)
summary(nlreg)

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

