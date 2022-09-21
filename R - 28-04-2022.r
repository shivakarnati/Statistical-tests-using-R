#### strong law of large numbers
## sample mean converges to the true mean

## We assume X1,...,XN are a sample of an exponential distribution wiht parameter lambda

## exponential distribution
## plot density

dexp(-10:10,0.1)

## plot density of Exp(0.2) -> mean value =5
x <- seq(-10,30,0.1)
plot(x,dexp(x,0.2),type='l')
## density has formula f(x)= 0 for x<0 
## and f(x)= 0.2 * exp(-0.2*x)

## plot distribution function 

x <- seq(-10,30,0.1)
plot(x,pexp(x,0.2),type='l')
pexp(10,0.2)

## random sample of size N=35 of X~Exp(0.2)

rexp(35,0.2)

# A sample of size N=35, lambda=0.2
N <- 35
lambda <- 0.2   ## true mean 1/lambda=5
mean(rexp(N,lambda))

## repeat taking samples of size N, K=1000 times
# and store them into out

K<- 1000
out <- array(,K)
for(k in 1:K){
    out[k] <- mean(rexp(N,lambda))
}
## What is the averag and the variance
mean(out)  ## this is close to the tru parameter
var(out)  ### there is quiote some variance

## Strong law of large numbers says (X1+..+XN)/N converges to true mean 1/lambda
## with prob 1 for N-> infinity, hence if N is large X1+..+XN)/N is closer to the true mean

N <- 500
K<- 1000
out <- array(,K)
for(k in 1:K){
    out[k] <- mean(rexp(N,lambda))
}
## What is the averag and the variance
mean(out)  ## this is close to the tru parameter
var(out)  ### the variance is small, sample means of size N=500 are close to the true mean



N <- 5000
K<- 1000
out <- array(,K)
for(k in 1:K){
    out[k] <- mean(rexp(N,lambda))
}
## What is the averag and the variance
mean(out)  ## this is close to the tru parameter
var(out)  ### the variance is small, sample means of size N=500 are close to the true mean

mean(rexp(5000,lambda))
mean(rexp(50000,lambda))


#### bias for second centralized moment
### interactiv exercise get estimates for the 2nd centralized moment and sample variance
### what is th ebias
### true variance of the exponential distribution is 1/lambda^2


library("moments")

K<- 100000   ## the larger K the bette rthe estimate for bias
N <- 10
out <- array(,K)
out1 <- array(,K)
for(k in 1:K){
    data <- rexp(N,lambda)  #random sample
    out[k] <- moment(data,order=2,central = TRUE) ## 2nd empirical centralized moment # nolint
    out1[k] <- var(data) # sample variance
}
## What is the average and the variance
mean(out) - 1/lambda^2  ## estimated bias of 2nd empirical centralized moment
mean(out1) - 1/lambda^2  ## estimated bias of the sample variance

# the 2nd centralized moment is biased, the sample variance has hardly a bias
# remember sample variance is an unbiasd estimator
# the 2nd empirical moment is an asymptotically unbiased estimator -> bias vanishes for N-> infty # nolint

## MSE

(mean(out) - 1/lambda^2)^2 + var(out) 
(mean(out1) - 1/lambda^2)^2 + var(out1) 



### large N =100

K<- 100000   ## the larger K the better the estimate for bias
N <- 100
out <- array(,K)
out1 <- array(,K)
for(k in 1:K){
    data <- rexp(N,lambda)  #random sample
    out[k] <- moment(data,order=2,central = TRUE) ## 2nd empirical centralized moment # nolint
    out1[k] <- var(data) # sample variance
}
## What is the average and the variance
mean(out) - 1/lambda^2  ## estimated bias of 2nd empirical centralized moment
mean(out1) - 1/lambda^2  ## estimated bias of the sample variance

### bias of the 2nd centralized moment decreased!
### sample variance has better properties, less bias
## MSE

(mean(out) - 1/lambda^2)^2 + var(out) 
(mean(out1) - 1/lambda^2)^2 + var(out1) 


### even large N =1000

K<- 100000   ## the larger K the better the estimate for bias
N <- 1000
out <- array(,K)
out1 <- array(,K)
for(k in 1:K){
    data <- rexp(N,lambda)  #random sample
    out[k] <- moment(data,order=2,central = TRUE) ## 2nd empirical centralized moment # nolint
    out1[k] <- var(data) # sample variance
}
## What is the average and the variance
mean(out) - 1/lambda^2  ## estimated bias of 2nd empirical centralized moment
mean(out1) - 1/lambda^2  ## estimated bias of the sample variance

var(out)  # both estimators have similar variance
var(out1)

### sample variance has better properties, less bias
## MSE

(mean(out) - 1/lambda^2)^2 + var(out) 
(mean(out1) - 1/lambda^2)^2 + var(out1) 

### The second centralized moment is better in terms of mean squared error -> see lecture notes # nolint



#######
## example data set


library('ISwR')
## https://en.wikipedia.org/wiki/Cystic_fibrosis
?cystfibr

cystfibr

summary(cystfibr)


## sex is stored as a metric variable

cystfibr$sex <- as.factor(cystfibr$sex)
summary(cystfibr)
levels(cystfibr$sex) <- c("m","f")
summary(cystfibr)

barplot(table(cystfibr$sex))

pie(table(cystfibr$sex))

#### boxplots and histograms of the metric variables
attach(cystfibr)
boxplot(age)  ##age looks symetrically distirbuted median 14

?boxplot

boxplot(height)  ## height ditribution is leftskewed -> not surprising 50% of patients  # nolint
# younger than 14, soo body size varies a lot in this age group

boxplot(weight)


boxplot(bmp)
?cystfibr

hist(age)  ### gives the absolute counts and equally spaced bins

?hist

hist(age,freq = TRUE)  ## absolute frequencies

hist(age,freq = FALSE) ## relative frequencies (area of the bins resembles density) # nolint


### the number of bins was maybe too large for this small sample size

hist(age, breaks=4)  ## just have 4 equally spaced bins
hist(age, breaks=4,freq = FALSE)  ## just have 4 equally spaced bins

#### unequal bins


hist(age, breaks=c(0,10,15,100))  ## just have 3 unqually equally spaced bins
## will be scaled by default


hist(age, breaks=c(0,10,15,100),freq=TRUE)  ## just have 3 unqually equally spaced bins 
## pplot the absolute counts -> we obtain a warning that this histogram does not remble the density



hist(age, breaks=c(0,10,15,100),freq=FALSE) 

hist(age, breaks=c(0,10,15,50),freq=FALSE) 

hist(age, breaks=4,freq = FALSE)  ## just have 4 equally spaced bins
