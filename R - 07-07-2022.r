
library("ISwR")

?intake
intake

### is the premenstrual intake larger than the postmenstrual intake

boxplot(intake$pre,intake$post)

## we can do a t-test!
## first F-test on variance homogeneity
var.test(intake$pre,intake$post)
## p-val: 0.8451 >0.05 -> variances are equal

##############################
#### WARNING WHAT  NOT  TO DO!
##############################
## one-sided two sample t-test with equal variances

t.test(intake$pre,intake$post,alternative="greater",var.equal=TRUE)

### p-value of 0.01625 -> groups differ!

### Problem: Assumption: the two groups are independent is violated!
### We have 2 measures from each patient -> we do not make use of this information (suboptimal)

####  Solution -> Paired-two sample t-test


#######
#  Proper way for group comparison here - two sample t-test!
#######

### two sided t-test for paired comparisons 
?t.test
t.test(intake$pre,intake$post,alternative="two.sided",paired=TRUE)


### this should be the same as 1-sample t-test on the differences

t.test(intake$pre - intake$post,alternative="two.sided")

#### check whether the lecture notes are correct
n <- length(intake$pre)
Dj <- intake$pre-intake$post


T <- (mean(Dj)-0)/sd(Dj) * sqrt(n)  ## this is in fact the test statistic
mean(Dj)
## p -value

2*(1-pt(abs(T),n-1))

2*pt(-abs(T),n-1)

####
install.packages("EnvStats")
library("EnvStats")
?varTest

## We want to test (HA) sigma_d^2 > 8000  -> sigma_d^2 <= 8000 
## 1 sided 1 sample chi-squ test 
var(Dj)
varTest(Dj,alternative="greater",sigma.squared=8000)
## p-value ~0 -> reject 0 accept HA: sigma_d^2 > 8000

### test stat

chi<- var(Dj)*(n-1)/(8000)
chi   ## in fact the same as in the output of th etest
var(Dj)

1- pchisq(chi,n-1)

#### Confidence interval:alternative

### 95% symmetric CI:alternative
## data is Dj

# qt() quantile function of t distribution
?qt
alpha <- 0.05
mean(Dj) - sd(Dj)/sqrt(n)*qt(1-alpha/2,n-1)  ### lower confidence point
mean(Dj) - sd(Dj)/sqrt(n)*qt(alpha/2,n-1)    ### uper confidence point
t.test(Dj,alternative="two.sided")


alpha <- 0.01  ## 99% CI will be wider than 95% CI
mean(Dj) - sd(Dj)/sqrt(n)*qt(1-alpha/2,n-1)  ### lower confidence point
mean(Dj) - sd(Dj)/sqrt(n)*qt(alpha/2,n-1)    ### uper confidence point
t.test(Dj,alternative="two.sided",conf.level = 0.99)



alpha <- 0.1  ## 90% CI will be narower than 95% CI
mean(Dj) - sd(Dj)/sqrt(n)*qt(1-alpha/2,n-1)  ### lower confidence point
mean(Dj) - sd(Dj)/sqrt(n)*qt(alpha/2,n-1)    ### uper confidence point
t.test(Dj,alternative="two.sided",conf.level = 0.90)

### One sided




alpha <- 0.1  ## 90% CI will be narower than 95% CI
mean(Dj) - sd(Dj)/sqrt(n)*qt(1-alpha,n-1)  ### lower confidence point

t.test(Dj,alternative="greater",conf.level = 0.90)


### two sample t-test



summary(juul)

### all variables categorical, ordinal, metric are stored as metric variables
## transorm scale levels as last time


# age ... metric
# mencharche ... nominal - can also be interpreted as ordinal - only for women
# sex... nominal - cannot be interpreted as ordinal
# igf1 ... metric
# tanner ... ordinal
# testvol ... metric only for males

summary(juul) # menarch, sex tanner are coded as metric variables -> change

juulNew <- juul

juulNew $menarche <- as.factor(juulNew $menarche)

juulNew $sex <- as.factor(juulNew$sex)

juulNew$tanner <- as.factor(juulNew$tanner)

levels(juulNew$menarche) <- c("No","Yes")
levels(juulNew$sex) <- c("M","F")
levels(juulNew$tanner) <- c("I","II","III","IV","V")

summary(juulNew)
attach(juulNew)
juulNew2 <- juulNew[!is.na(tanner),]

boxplot(igf1~tanner)
mean(igf1,na.rm=TRUE)


## Next question
## Are there differences in the igf1 values accross groups?
## compare all groups with each other:igf1 - with two-sided, two sample t-test (with equal variances)

x1 <- juulNew2$igf1[juulNew2$tanner== "I"]
x2 <- juulNew2$igf1[juulNew2$tanner== "II"]
x3 <- juulNew2$igf1[juulNew2$tanner== "III"]
x4 <- juulNew2$igf1[juulNew2$tanner== "IV"]
x5 <- juulNew2$igf1[juulNew2$tanner== "V"]


t.test(x2,x3,var.equal=TRUE)   # 2 sample t test with equal variances

## CI
n1 <- sum(!is.na(x2))
n2 <- sum(!is.na(x3))
sp <- sqrt((n1-1)/(n1+n2-2)*var(x2,na.rm=TRUE)+(n2-1)/(n1+n2-2)*var(x3,na.rm=TRUE))

alpha <- 0.05

mean(x2,na.rm=TRUE) - mean(x3,na.rm=TRUE) - sp*sqrt(1/n1+1/n2) * qt(1-alpha/2,n1+n2-2)
mean(x2,na.rm=TRUE) - mean(x3,na.rm=TRUE) - sp*sqrt(1/n1+1/n2) * qt(alpha/2,n1+n2-2)

t.test(x2,x3,var.equal=TRUE)   # 2 sample t test with equal variances


### 99% CI


alpha <- 0.01

mean(x2,na.rm=TRUE) - mean(x3,na.rm=TRUE) - sp*sqrt(1/n1+1/n2) * qt(1-alpha/2,n1+n2-2)
mean(x2,na.rm=TRUE) - mean(x3,na.rm=TRUE) - sp*sqrt(1/n1+1/n2) * qt(alpha/2,n1+n2-2)

t.test(x2,x3,var.equal=TRUE,conf.level =0.99)   # 2 sample t test with equal variances


## F test CI


alpha <- 0.05

var.test(x2,x3)

var(x2,na.rm=TRUE)/var(x3,na.rm=TRUE)  ## ratio of variances

s1 <- var(x2,na.rm=TRUE)
s2 <- var(x3,na.rm=TRUE) 

s2/s1*qf(alpha/2,n1-1,n2-1) ### lower confidence points for sigma2/sigma1
s2/s1*qf(1- alpha/2,n1-1,n2-1) ### upper confidence point for sigma2/sigma1

1/(s2/s1*qf(1- alpha/2,n1-1,n2-1)) ### upper confidence point for sigma1/sigma2
1/(s2/s1*qf(alpha/2,n1-1,n2-1)) ### lower confidence points for sigma1/sigma2

### alternatively
s1/s2*qf(alpha/2,n2-1,n1-1) ### upper confidence point for sigma1/sigma2
s1/s2*qf(1-alpha/2,n2-1,n1-1) ### lower confidence points for sigma1/sigma2



var.test(x2,x3)

