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
K<- 1000
out <- array(,K)
for(k in 1:K){
    out[k] <- mean(rexp(N,lambda))
}
## What is the averag and the variance
mean(out)  ## this is close to the tru parameter
var(out)  ### there is quiote some variance

