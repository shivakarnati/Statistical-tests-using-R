
#install.packages("ISwR")
library("ISwR")


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


### boxplot of igf vs. tnanner score

boxplot(juulNew$igf1 ~juulNew$tanner)

### We want to test whther tanner II had systematicaly lower igf1 concentration than tanner III
### -> one-sided two-sample t-test
###  H0: mu(tanner II) => mu(tanner III)  vs. HA: mu(tanner II) < mu(tanner III)
### Assume equal variances! Can we do that? -> See later



## select only igf1 in samples with tanner =III

igftannerIII <- juulNew[juulNew$tanner =="III" , "igf1"]  ## select all samples with Tanner=I
## this does not exclude NAs


## select only igf1 in samples with tanner =I

igftannerII <- juulNew[juulNew$tanner =="II" , "igf1"]  ## select all samples with Tanner=I
## this does not exclude NAs

## check assumptions

hist(igftannerIII)   ## looks normal enough

qqnorm(igftannerIII) ## looks normal enough


hist(igftannerII)   ## looks normal enough

qqnorm(igftannerII) ## looks normal enough

### equal variances

var(igftannerII,na.rm=TRUE)
var(igftannerIII,na.rm=TRUE)

### variances might not be equal -> we need a test to decide this


## Assume for now equal variances
?t.test
t.test(igftannerII,igftannerIII,alternative="less",var.equal=TRUE)

## does the test of the lecture notes give the same results:


### d = 0
### test stat

n <- sum(!is.na(igftannerII))

m <- sum(!is.na(igftannerIII))
sp <- sqrt((n-1)/(n+m-2)*var(igftannerII,na.rm=TRUE)+(m-1)/(n+m-2)*var(igftannerIII,na.rm=TRUE))

X <- mean(igftannerII,na.rm=TRUE) 
X
Y <- mean(igftannerIII,na.rm=TRUE)
Y

## Test statistic 
T <-(X-Y-0)/(sp*sqrt(1/n+1/m))

## p-value:
pt(T,m+n-2)

### Now we want to test whether igf1 are equal between tanner III and tanner V
##  H0: mu(tanner III) = mu(tanner V)  vs. HA: mu(tanner III) != mu(tanner V)
## one sided two-sample unpaired t-test


igftannerV <- juulNew[juulNew$tanner =="V" , "igf1"]  ## select all samples with Tanner=I
## this does not exclude NAs

## check assumptions

hist(igftannerV)   ## looks normal enough

qqnorm(igftannerV) ## looks normal enough

### assume equal variances!

?t.test
t.test(igftannerIII,igftannerV,alternative="two.sided",var.equal=TRUE)
## p-val > 0.05 -> We keep H0 the igf levels in the two groups are equal

### Are groups tanner III and tanner IV equal?
### Same test

igftannerIV <- juulNew[juulNew$tanner =="IV" , "igf1"]  ## select all samples with Tanner=I
## this does not exclude NAs

## check assumptions

hist(igftannerIV)   ## looks normal enough

qqnorm(igftannerIV) ## looks normal enough

t.test(igftannerIII,igftannerIV,alternative="two.sided",var.equal=TRUE)


### p-val > 0.05 -> we keep H0.


### Note we performed 3 tests on the same data (tannerIII)  -> multiple testing
