
### Mean squared error MSQ for sample variance S and 2nd centralized moment M_2^c
### For exponential distribution



library("moments")

K<- 100000   ## number of Samples of size N we take (K repeats) - the larger K the better the estimate for bias
N <- 10   ## sample of size N
out <- array(,K)  ## here we store the sample variance M_2^c  for each of the K simulated data sets
out1 <- array(,K) ## here we store the sample variance Sfor each of the K simulated data sets

lambda <- 0.2 ## This is the choice of the true parameter
1/lambda^2  ## 25 is the true variance
## In practice we never know the true parameter
# In simulation we do - we choose the true parameter that allows us to evaluate the perfromance of statistical methods


for(k in 1:K){
    data <- rexp(N,lambda)  #random sample from an exponential distribution with parameter lambda
    out[k] <- moment(data,order=2,central = TRUE) ## 2nd empirical centralized moment # nolint
    out1[k] <- var(data) # sample variance
}
hist(out) ## distibution of M_2^c
hist(out1) ## distibution of S

boxplot(out,out1)



### True Bias of S is 0 - S is unbiased - estimate of bias is reasonable clone to 0
### True bias if M_2^c is -1/N *  1/lambda^2

# the 2nd centralized moment is biased, the sample variance has hardly a bias
# remember sample variance is an unbiasd estimator
# the 2nd empirical moment is an asymptotically unbiased estimator -> bias vanishes for N-> infty # nolint
-1/N * 1/lambda^2  ## -2.5 


## What is the Bias of the estimator? We estimate it as follows
mean(out) - 1/lambda^2  ## estimated bias of 2nd empirical centralized moment 
## sufficiently close to the true value

mean(out1) - 1/lambda^2  ## estimated bias of the sample variance
## sufficiently close to 0

## MSE  - lecture notes
# MSE for S:
1/lambda^4 * (8/N + 2/ (N*(N-1)))  ##513.8889


## MSE  - lecture notes
# MSE for M_2^c:
1/lambda^4 * (8/N + 2/ (N*(N-1))-15/N^2 + 8/N^3 - (4*N-2)/(N^3*(N-1)))  ##422.5

### MSE of M_2^c is much smaller than that of S!

## estimate MSE for S

(mean(out1) - 1/lambda^2)^2 + var(out1)  # close to the true value

## estimate MSE for M_2^c

(mean(out) - 1/lambda^2)^2 + var(out) # close to the true value


## This little exercise shows that we can assess the performance of an estimator by numerical simulations
# Most of the time it is impossible to get explicit formulas for the MSE - there is no alternative to numerical investigations

###################################
# larger sample size -> each data set contains more information, so we get better estimations for the variance of the distribution
N <- 100   ## sample of size N
out <- array(,K)  ## here we store the sample variance M_2^c  for each of the K simulated data sets
out1 <- array(,K) ## here we store the sample variance Sfor each of the K simulated data sets

lambda <- 0.2 ## This is the choice of the true parameter
1/lambda^2  ## 25 is the true variance
## In practice we never know the true parameter
# In simulation we do - we choose the true parameter that allows us to evaluate the perfromance of statistical methods


for(k in 1:K){
    data <- rexp(N,lambda)  #random sample from an exponential distribution with parameter lambda
    out[k] <- moment(data,order=2,central = TRUE) ## 2nd empirical centralized moment # nolint
    out1[k] <- var(data) # sample variance
}
hist(out) ## distibution of M_2^c
hist(out1) ## distibution of S

boxplot(out,out1)



### True Bias of S is 0 - S is unbiased - estimate of bias is reasonable clone to 0
### True bias if M_2^c is -1/N *  1/lambda^2

# the 2nd centralized moment is biased, the sample variance has hardly a bias
# remember sample variance is an unbiasd estimator
# the 2nd empirical moment is an asymptotically unbiased estimator -> bias vanishes for N-> infty # nolint
-1/N * 1/lambda^2  ## -0.25 


## What is the Bias of the estimator? We estimate it as follows
mean(out) - 1/lambda^2  ## estimated bias of 2nd empirical centralized moment 
## sufficiently close to the true value

mean(out1) - 1/lambda^2  ## estimated bias of the sample variance
## sufficiently close to 0

## MSE  - lecture notes
# MSE for S:
1/lambda^4 * (8/N + 2/ (N*(N-1)))  ## 50.12626


## MSE  - lecture notes
# MSE for M_2^c:
1/lambda^4 * (8/N + 2/ (N*(N-1))-15/N^2 + 8/N^3 - (4*N-2)/(N^3*(N-1)))  ##  49.19125

### MSE of M_2^c is smaller but not much smaller than that of S!



## estimate MSE for S 
(mean(out1) - 1/lambda^2)^2 + var(out1)  # close to the true value

## estimate MSE for M_2^c

(mean(out) - 1/lambda^2)^2 + var(out) # close to the true value

