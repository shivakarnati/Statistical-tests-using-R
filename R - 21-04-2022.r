
#nolint 
library('ISwR')

library("moments")




library("xlsx")

### load excel file
data <- read.xlsx("/Users/kristanschneider/Meine Ablage (Kristan.schneider@hs-mittweida.de)/Lehre/SoSe2022/Sel Topic Comp Stat/ExampleData.xlsx",1)

data
summary(data)  ## all variables are read in as numerical variables -> change to categorical where appropriate
## summary statistics: min, q1/4, q1/2, mean x bar, q3/4, max
## makes sense only for bidy weight and height
## we need data frame
Data <- as.data.frame(data) 
Data$S <- as.factor(Data$S)
levels(Data$S) <- c("female","male")

Data$eye <- as.factor(Data$eye)
levels(Data$eye) <- c("blue","green","brown")

Data$hair <- as.factor(Data$hair)
levels(Data$hair) <- c("blonde","brown","black")


Data$school <- as.factor(Data$school)
levels(Data$school) <- c("no degree","highschool","university")
summary(Data) 


#### moments
?moment
moment(data$BS)

moment(data$BS,order=1)  ## order=1 ist the deafault # nolint
### the optional arguments have to be set only if you want to change the default value # nolint

moment(data$BS,order=2)  ## 2nd uncentralized moment # nolint
moment(data$BS,order=3)  ## 3rd uncentralized moment # nolint
moment(data$BS,order=4)  ## 3rd uncentralized moment # nolint

### centralzed moments
moment(data$BS,central= TRUE)  #### should be 0 # nolint
### is approximately 0

moment(data$BS,order=2,central= TRUE) ## 2nd centralized moment # nolint
var(data$BS)  ## sample variance

length(data$BS)  # sample size n =6
moment(data$BS,order=2,central= TRUE) * 6/(6-1)  ## equals sample variance # nolint
var(data$BS)  ## sample variance

## order of optionalarguments does not matter!

moment(BS,central=TRUE,order=3) 
# is the same as
moment(BS,order=3,central=TRUE)
# is the same as
moment(order=3,BS,central=TRUE)
# is the same as
moment(central=TRUE,order=3,BS)


attach(data)  ## we don't want to write dta$BS all the time

skewness(BS)

moment(BS,central=TRUE,order=3) / (moment(BS,central=TRUE,order=2)^(3/2)) # nolint

kurtosis(BS)  ## this is our definition +3


moment(BS,central=TRUE,order=4) / (moment(BS,central=TRUE,order=2)^2) 
## this =3 for the normal distribution


moment(BS,central=TRUE,order=4) / (moment(BS,central=TRUE,order=2)^2) -3 ## this was our definition
kurtosis(BS) -3   ## this is our definition
#this is positive, more centralized than the normal distribution

## We can also implement these quantities directly

mean <- sum(BS)/length(BS)  # sample mean
(sum((BS- mean)^4)/length(BS))/(sum((BS- mean)^2)/length(BS))^2 -3 


#### illustrate th estrong law of large numbers:

# take a sample of size n=50 from e.g. a Poisson distribution with parameter lambda=4
# mean = 4


?rpois

x <- rpois(50,4)
mean(x)

x <- rpois(50,4)
mean(x)

x <- rpois(50,4)
mean(x)

## increase sample size

x <- rpois(500,4)
mean(x)

x <- rpois(500,4)
mean(x)

### rather than evaluating this manually, we can repeat it in a loop
## for loop

out <- array(,100)  ## empty array of size 100
out

for(k in 1:100){
    out[k] <- mean(rpois(50,4))
}
out

for(k in 1:100){
    out[k] <- mean(rpois(500,4))
}

out   ### the values seem to be closer to the true mean for large sample size

for(k in 1:100){
    out[k] <- mean(rpois(5000,4))
}

out ### the values seem to be closer to the true mean for large sample size


for(k in 1:100){
    out[k] <- mean(rpois(50000,4))
}

out ### the values seem to be closer to the true mean for large sample size

### Estimate the bias! Take the sample mean of the simulated means (i.e. mean(out)) and substract the tru parameter

mean(out) - 4  ## very small - this is unbiased!

# the bias depend on sample size n?

# n=10
out10 <- array(,100)
for(k in 1:100){
    out10[k] <- mean(rpois(10,4))
}

# n=5000
out5K <- array(,100)
for(k in 1:100){
    out5K[k] <- mean(rpois(5000,4))
}
mean(out10)-4  # estimate of bias with sample size 10
mean(out5K)-4  # estimate of  bias with sample size 5K


### estimates are not very accurate, because we just averaged over 100 repeats
### 10000 repeats



# n=10
out10 <- array(,10000)
for(k in 1:10000){
    out10[k] <- mean(rpois(10,4))
}

# n=5000
out5K <- array(,10000)
for(k in 1:10000){
    out5K[k] <- mean(rpois(5000,4))
}
mean(out10)-4  # estimate of bias with sample size 10
mean(out5K)-4  # estimate of  bias with sample size 5K
### both can be considered unbiased

#### PErformance = bias^2 + Variance of esimateor!
#### How good is the precision?

## Estimate for precision: sample variance of out
## out10  10000 estimates of mean from samples of size 10
## out5K  10000 estimates of mean from samples of size 5000

var(out10)
var(out5K)

### Variance of the esimator is much smaller for larger sample size!

### MSE = bias ^2 + var

(mean(out10)-4)^2 + var(out10)  ## for sample size n=10
(mean(out5K)-4)^2 + var(out5K)  ## for sample size n=5000

## if sample size is larger = more information, the precision of the estimator improves # nolint


### Home exercise

## Repeat the above, but use exponential distribution "rexp(n,1/mean)"
## e.g. rexp(n,0.25) mean is 4

mean(rexp(1000,0.25))


#### illustrate the unbiasedness and asymptotic unbiasedness



