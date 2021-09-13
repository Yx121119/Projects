
source("functions.r")
library("dslabs")

data("gapminder")

# Basic Information for data set
data<-na.omit(gapminder)

# Check the density plots for original data & simulated data
allPlot(data)



# Estimate the parameters for each country

# Under the assumption of normal distribution

normEstimates<-paramEstimate(data)

# Without the assumption of normal distribution
nonEstimates<-paramEstimate(data,Assumption = FALSE)


# Simulated data Non-normal distributed
set.seed(1234)
s1<-newData(data,nonEstimates,conType = rep("Asia",5),Assumption = FALSE,
            samplesize = rep(1400,5))

allPlot(s1)
# Simulated data for Normal distributed
set.seed(1233)
s11<-newData(data,normEstimates,conType = rep("Asia",5),Assumption = TRUE,
             samplesize = rep(1400,5))

allPlot(s11)

# Simulated data for power testing: non-normal distributed
set.seed(1232)
pow_non<-newData(data,nonEstimates,samplesize = rep(1400,5),Assumption = FALSE,
             conType = c("Americas", "Asia", "Asia","Asia","Asia"))

allPlot(pow_non)

# Testing on scenarios 
#=====scenario1 The response of each group does not follow a normal distribution

# From the basic density plot of 5 continent, assume the response of each group
# follows a Gaussian mixture distribution
# To test size, given the null hypothesis is true, simulate 5 group of numbers
# based on the distribution of 'Asia' as it's density plot shows a clear pattern 
# of non-normal distribution
# With significant level alpha = 0.05

# Test for size (parametric: with Anova)
set.seed(12)
testOn(data,nonEstimates,conType = rep("Asia",5),Assumption = FALSE,
       samplesize = rep(1400,5),alpha= 0.05)


# The size for parametric method = 0.061. For non-parametric = 0.055


# When all groups follow a normal distribution

set.seed(22)
testOn(data,normEstimates,samplesize = rep(1400,5),Assumption = TRUE,
       conType = rep("Asia",5),alpha = 0.05)
# size for parametric = 0.047. For non-parametric = 0.047


# ==================Scenario2 Imbalanced sample size================#

# Under this scenario, firstly set the normal assumption to be true.
# To test size, given the null hypothesis is true, simulate 5 group of numbers
# based on the distribution of 'Asia'.
# Calculate the size for data set from small sample size to large sample size

#--------------with normal assumption---------------#
#2.1 sample size = 200
set.seed(203)
testOn(data,normEstimates,samplesize = rep(40,5),Assumption = TRUE,
       conType = rep("Asia",5),alpha = 0.05)

# Size for parametric: 0.051. Size for non-parametric: 0.043

#2.2 sample size = 500

set.seed(2001)
testOn(data,normEstimates,samplesize = rep(100,5),Assumption = TRUE,
       conType = rep("Asia",5),alpha = 0.05)

# Size for parametric: 0.05. Size for non-parametric: 0.053

#2.3 sample size = 1000
set.seed(2004)
testOn(data,normEstimates,samplesize = rep(200,5),Assumption = TRUE,
       conType = rep("Asia",5),alpha = 0.05)

# Size for parametric: 0.055. Size for non-parametric: 0.051

#2.4 sample size = 5000
set.seed(2099)
testOn(data,normEstimates,samplesize = rep(1000,5),Assumption = TRUE,
       conType = rep("Asia",5),alpha = 0.05)

# Size for parametric: 0.055. Size for non-parametric: 0.051


#2.5 large sample size vs small sample size

set.seed(20010)
testOn(data,normEstimates,samplesize = c(20,50,1000,1000,1000),
       Assumption = TRUE,conType = rep("Asia",5),alpha = 0.05)

# Size for parametric: 0.041. Size for non-parametric: 0.032

set.seed(20016)
testOn(data,normEstimates,samplesize = c(20,20,50,100,100),
       Assumption = TRUE,conType = rep("Asia",5),alpha = 0.05)
# Size for parametric: 0.043. Size for non-parametric: 0.051


#---------imbalanced sample size without normal assumption-------------#

# 3.1 sample size = 200
set.seed(36)
testOn(data,nonEstimates,samplesize = rep(40,5),Assumption = FALSE,
       conType = rep("Asia",5),alpha = 0.05)
# Size for parametric: 0.063. Size for non-parametric: 0.061

# 3.2 sample size = 500
set.seed(37)
testOn(data,nonEstimates,samplesize = rep(100,5),Assumption = FALSE,
       conType = rep("Asia",5),alpha = 0.05)
# Size for parametric: 0.046. Size for non-parametric: 0.042


# 3.3 sample size = 1000
set.seed(42)
testOn(data,nonEstimates,samplesize = rep(200,5),Assumption = FALSE,
       conType = rep("Asia",5),alpha = 0.05)
# Size for parametric: 0.048. Size for non-parametric: 0.05

# 3.4 sample size = 5000
set.seed(52)
testOn(data,nonEstimates,samplesize = rep(1000,5),Assumption = FALSE,
       conType = rep("Asia",5),alpha = 0.05)
# Size for parametric: 0.05. Size for non-parametric: 0.042


#3.5 large sample size vs small sample size

set.seed(30013)
testOn(data,nonEstimates,samplesize = c(20,50,1000,1000,1000),
       Assumption = FALSE,conType = rep("Asia",5),alpha = 0.05)

# Size for parametric: 0.037, Size for non-parametric: 0.039

set.seed(30016)
testOn(data,nonEstimates,samplesize = c(20,20,50,100,100),
       Assumption = FALSE,conType = rep("Asia",5),alpha = 0.05)
# Size for parametric: 0.041. Size for non-parametric: 0.044



#================ Test on Power ========================#
# To calculate power, given that h1 is true. The testing data set is generated 
# based on the simulation results from group Asia and Americas. The reason for 
# this is that these two groups have similar distribution and due to the 
# obvious differences in the distribution properties of these five groups it is
# quite easy for power to get 1 as groups. Thus, by generating 
# data from Asia and Americas we have a lower h1 value but it does not 
# equal to 0.

#=========Scenario1: without normal distribution=================#

# Generate simulated data from two groups are quite similar: Asia & Americas
set.seed(204)
testOn(data,n = 1000,nonEstimates,samplesize = rep(1400,5),Assumption = FALSE,
       conType = c("Americas", "Asia", "Asia","Asia","Asia"),
       stats = "power",alpha = 0.05)
# Power for parametric: 0.392. Power for non-param:0.097


# Under the assumption of normality
set.seed(407)
testOn(data,n = 1000,normEstimates,samplesize = rep(1400,5),Assumption = TRUE,
       conType = c("Americas", "Asia", "Asia","Asia","Asia"),
       stats = "power",alpha = 0.05)
# Power for parametric: 0.43. Power for non-param:0.44


#===============Scenario2: imbalanced sample size==================#

#-----------With normality assumption----------------#

# 4.1 sample size = 200
set.seed(501)
testOn(data,n = 1000,normEstimates,samplesize = rep(40,5),Assumption = TRUE,
       conType = c("Americas", "Asia", "Asia","Asia","Asia"),
       stats = "power",alpha = 0.05)
# Power for parametric: 0.052. Power for non-param:0.048

# 4.2 sample size = 500
set.seed(502)
testOn(data,n = 1000,normEstimates,samplesize = rep(100,5),Assumption = TRUE,
       conType = c("Americas", "Asia", "Asia","Asia","Asia"),
       stats = "power",alpha = 0.05)
# Power for parametric: 0.068. Power for non-param:0.071

# 4.3 sample size = 1000
set.seed(504)
testOn(data,n = 1000,normEstimates,samplesize = rep(200,5),Assumption = TRUE,
       conType = c("Americas", "Asia", "Asia","Asia","Asia"),
       stats = "power",alpha = 0.05)
# Power for parametric: 0.087. Power for non-param:0.088

# 4.4 sample size = 5000
set.seed(507)
testOn(data,n = 1000,normEstimates,samplesize = rep(1000,5),Assumption = TRUE,
       conType = c("Americas", "Asia", "Asia","Asia","Asia"),
       stats = "power",alpha = 0.05)
# Power for parametric: 0.29. Power for non-param:0.303

# 4.5 large sample size vs small sample size

set.seed(40013)
testOn(data,nonEstimates,samplesize = c(20,50,1000,1000,1000),
       Assumption = TRUE,
       conType = c("Americas", "Asia", "Asia","Asia","Asia"),
       stats = "power",alpha = 0.05)

# power for parametric: 0.037, power for non-parametric: 0.039

set.seed(40016)
testOn(data,nonEstimates,samplesize = c(20,20,50,100,100),
       Assumption = TRUE,
       conType = c("Americas", "Asia", "Asia","Asia","Asia"),
       stats = "power",alpha = 0.05)
# power for parametric: 0.041. power for non-parametric: 0.044




#-------------Imbalanced sample size Without the normality assumption-------#


# 5.1 sample size = 100
set.seed(517)
testOn(data,n = 1000,nonEstimates,samplesize = rep(40,5),Assumption = FALSE,
       conType = c("Americas", "Asia", "Asia","Asia","Asia"),
       stats = "power",alpha = 0.05)
# Power for parametric: 0.054. Power for non-param:0.056


# 5.2 sample size = 500
set.seed(509)
testOn(data,n = 1000,nonEstimates,samplesize = rep(100,5),Assumption = FALSE,
       conType = c("Americas", "Asia", "Asia","Asia","Asia"),
       stats = "power",alpha = 0.05)
# Power for parametric: 0.069. Power for non-param:0.065


# 5.3 sample size = 1000
set.seed(514)
testOn(data,n = 1000,nonEstimates,samplesize = rep(200,5),Assumption = FALSE,
       conType = c("Americas", "Asia", "Asia","Asia","Asia"),
       stats = "power",alpha = 0.05)
# Power for parametric: 0.092. Power for non-param:0.062



# 5.4 sample size = 5000
set.seed(513)
testOn(data,n = 1000,nonEstimates,samplesize = rep(1000,5),Assumption = FALSE,
       conType = c("Americas", "Asia", "Asia","Asia","Asia"),
       stats = "power",alpha = 0.05)
# Power for parametric: 0.3. Power for non-param:0.079

# 5.5 Large sample size Vs small sample size

set.seed(50013)
testOn(data,nonEstimates,samplesize = c(20,50,1000,1000,1000),
       Assumption = FALSE,
       conType = c("Americas", "Asia", "Asia","Asia","Asia"),
       stats = "power",alpha = 0.05)

# power for parametric: 0.032, Size for non-parametric: 0.032

set.seed(50016)
testOn(data,nonEstimates,samplesize = c(20,20,50,100,100),
       Assumption = FALSE,
       conType = c("Americas", "Asia", "Asia","Asia","Asia"),
       stats = "power",alpha = 0.05)
# power for parametric: 0.043. Size for non-parametric: 0.044

