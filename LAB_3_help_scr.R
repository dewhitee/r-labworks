# x - 1st set
# y - 2nd set

### t-Student's test for homogeneity
t.test(x, y, alternative = "two.sided", var.equal = FALSE)

### Wilcoxon-Mann-Whitney test for homogeneity
wt = wilcox.test(x, y, alternative = "two.sided", correct = FALSE)
attributes(wt)	# get names of all test parameters

wt$statistic	# get WMW test statistic value


### Kolmogorov-Smirnov test for homogeneity
ks.test(x, y, alternative = "two.side")

### Boxplot
boxplot(x, y, names=c("x", "y"))
