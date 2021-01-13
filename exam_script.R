
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