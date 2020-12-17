### READ DATA
dat = read.csv("V10.csv", sep=";")


#### CONSTRUCT REGRESSIN

reg = lm(y ~ ., data=dat)
summary(reg)


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


################################################
############ NON-LINEAR MODEL

nlreg = lm(y ~ I(x1^2) + I(x2^2), data=dat)
summary(nlreg)

################################################
############ STEPWISE

#### FORWARD

min_mod = lm(y ~ 1, data=dat)
max_mod = formula(lm(y ~ ., data=dat))
regF =  step(min_mod, direction="forward", scope=max_mod)
summary(regF)

#### BACKWARD

regB = step(lm(y ~ ., data=dat), direction = "backward")
summary(regB)

