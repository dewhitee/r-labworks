### READ DATA
dat = read.csv("Data_LW_5.csv")

alpha = 0.05

### TAKE YOUR VARIANT DATA
y = dat$DOC
x1 = dat$DPI
x2 = dat$TIME

# Correlation coefficient
source("F:/dev/R/Labworks/pcor.R")
corr_between_y_and_x1 = cor.test(x1, y)
corr_between_y_and_x2 = cor.test(x2, y)

#### CONSTRUCT REGRESSION

reg = lm(y ~ x1)
summary(reg)

reg_y_x2 = lm(y ~ x2)
summary(reg_y_x2)

#### ANOVA

anova_result = anova(reg)
anova_result_y_x2 = anova(reg_y_x2)

# To calculate acceptance region for Fisher's test
# use Fisher distribution quantile

p = 1 - alpha
df1 = anova_result$Df[1]
df2 = anova_result$Df[2]

qf(p, df1, df2)

df1_y_x2 = anova_result_y_x2$Df[1]
df2_y_x2 = anova_result_y_x2$Df[2]

qf(p, df1_y_x2, df2_y_x2)

# p - probability
# df1 - degrees of freedom of ESS
# df2 - degrees of freedom of RSS


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

res_y_x2 = resid(reg_y_x2)
plot(res_y_x2, ylim=c(mean(res_y_x2)-3*sd(res_y_x2), mean(res_y_x2)+3*sd(res_y_x2)))
abline(h=mean(res_y_x2), col="red", lwd=2)
abline(h=mean(res_y_x2)-sd(res_y_x2), col="darkgreen")
abline(h=mean(res_y_x2)+sd(res_y_x2), col="darkgreen")

abline(h=mean(res_y_x2)-2*sd(res_y_x2), col="green")
abline(h=mean(res_y_x2)+2*sd(res_y_x2), col="green")

abline(h=mean(res_y_x2)-3*sd(res_y_x2), col="blue")
abline(h=mean(res_y_x2)+3*sd(res_y_x2), col="blue")

#### PREDICTION
# Make a prediction for 3 values - a, b and c. 
# Values choose by yourself
a = 3 
b = 8
c = 17
d = 19
e = 20
f = 21
g = 22

new = data.frame(x1 = c(a, b, c, d, e, f, g))
predict(reg,  newdata = new, 
        se.fit=TRUE, interval="confidence", level=0.95)

predict(reg_y_x2, newdata = data.frame(x2 = c(a, b, c, d, e, f, g)), 
        se.fit=TRUE, interval="confidence", level=0.95)


