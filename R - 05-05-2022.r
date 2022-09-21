library("ISwR")


?cystfibr

cystfibr

summary(cystfibr)


## sex is stored as a metric variable

cystfibr$sex <- as.factor(cystfibr$sex)
summary(cystfibr)
levels(cystfibr$sex) <- c("m","f")
summary(cystfibr)


## histogram of age

attach(cystfibr)
hist(age)

boxplot(age)


## Q-Q plot for normal distribution
qqnorm(age)
# looks pretty much on a straight line, but only few data points

detach(cystfibr)

?secher
secher

hist(secher$bwt)  ## looks normal - a bit right skewed

boxplot(secher$bwt)

# Q-Q plot for normal distirbution
qqnorm(secher$bwt) ## looks normally distributed


?qqplot
length(secher$bwt) ## sample size

mean(secher$bwt)  # this values seems plausible 
# we check for fit of an exponential distribution Exp(1/2739)-- it is clearly not exponentially distributed!
# theoretical quantiles  
x <- qexp((1:107)/107,rate=1/2739)

qqplot(x,secher$bwt)  ## clearly not exponentially distributed
## if it was exponentially distributed it shoud lie on the diagonal!


## for exponetial distribution, we do not need to specify the rate! Chose lambda =1 -> then values do not need to lie on digonal but on a stright line starting at the origin


x <- qexp((1:107)/107,rate=1)

qqplot(x,secher$bwt)  ## clearly not exponentially distributed
## if it was exponentially distributed it shoud lie on the diagonal!




### simulated data
### simulate data size 1000 from Y~Exp(2.5)

y <- rexp(1000,rate=2.5)
x # theoretical quantiles
x <- qexp((1:1000)/1000,rate=2.5)


qqplot(x,y) ## perfect fit to the exponential distribution with rate 2.5

### If we do not know the rate, we can compare it with Exp(1) distr


y <- rexp(1000,rate=2.5)
x # theoretical quantiles
x <- qexp((1:1000)/1000,rate=1)


qqplot(x,y) ## perfect straight line starting at the origin, slope is < 1 -> true rate is larger than 1 


### compare with normal ditribution

qqnorm(y) ## clearly not normally distributed 




