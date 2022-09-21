
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
attach(juulNew)
juulNew2 <- juulNew[!is.na(tanner),]

boxplot(igf1~tanner)
mean(igf1,na.rm=TRUE)


### Suppose we expect an igf1 level of 350 mg/L
### We test at which Tanner stage the igf levels differ from 350 mg/L
### 5 1-sample t-tests

t.test(juulNew2$igf1[juulNew2$tanner== "I"],mu=350)    ## H1: p-value: < 2.2e-16
t.test(juulNew2$igf1[juulNew2$tanner== "II"],mu=350)   ## H2: p-value: 0.8559
t.test(juulNew2$igf1[juulNew2$tanner== "III"],mu=350)  ## H3: p-value: 5.266e-07 
t.test(juulNew2$igf1[juulNew2$tanner== "IV"],mu=350)   ## H4: p-value: 8.017e-15
t.test(juulNew2$igf1[juulNew2$tanner== "V"],mu=350)    ## H5: p-value: < 2.2e-16 

# Holm correction FWER: alpha =0.05
# order p -values: 

od <- rank(c(2.2e-16,0.8559, 5.266e-07,8.017e-15 ,2.2e-16),ties.method = "first")
od
# alpha = 0.05 
cond <- 0.05/(5+1-od) - c(2.2e-16,0.8559, 5.266e-07,8.017e-15 ,2.2e-16) >0
cond 
min(od[!cond])
## 
## Here Holm's prceedure rejects H(1)=H1, H(2)=H5, H(3)=H4, H(4)=H3 an accepts H(5)=H2.

## Hochberg 
max(od[cond])  # 4 first orderd 4 null-hypotheses are rejected
## Here Holm's prceedure rejects H(1)=H1, H(2)=H5, H(3)=H4, H(4)=H3 an accepts H(5)=H2.

### Holm and Hochberg yield the same results

### Bonferone would also yoield the same 
c(2.2e-16,0.8559, 5.266e-07,8.017e-15 ,2.2e-16) < 0.05/5
## H(1)=H1, H(2)=H5, H(3)=H4, H(4)=H3 an accepts H(5)=H2.

## Although all corrections yield the same the Hochber and holm are preferentiable

## Next question
## Are there differences in the igf1 values accross groups?
## compare all groups with each other:igf1 - with two-sided, two sample t-test (with equal variances)

x1 <- juulNew2$igf1[juulNew2$tanner== "I"]
x2 <- juulNew2$igf1[juulNew2$tanner== "II"]
x3 <- juulNew2$igf1[juulNew2$tanner== "III"]
x4 <- juulNew2$igf1[juulNew2$tanner== "IV"]
x5 <- juulNew2$igf1[juulNew2$tanner== "V"]


t.test(x1,x2,var.equal = TRUE )    ## H1: p-value: < 2.2e-16  - Tanner group I with tanner group II 
t.test(x1,x3,var.equal = TRUE )    ## H2: p-value: < 2.2e-16  - Tanner group I with tanner group III
t.test(x1,x4,var.equal = TRUE )    ## H3: p-value: < 2.2e-16  - Tanner group I with tanner group IV
t.test(x1,x5,var.equal = TRUE )    ## H4: p-value: < 2.2e-16  - Tanner group I with tanner group V 

t.test(x2,x3,var.equal = TRUE )    ## H5: p-value: 1.613e-06  - Tanner group II with tanner group III
t.test(x2,x4,var.equal = TRUE )    ## H6: p-value: 1.223e-11  - Tanner group II with tanner group IV
t.test(x2,x5,var.equal = TRUE )    ## H7: p-value: 3.877e-10  - Tanner group II with tanner group V 

t.test(x3,x4,var.equal = TRUE )    ## H8: p-value: 0.2677     - Tanner group III with tanner group IV
t.test(x3,x5,var.equal = TRUE )    ## H9: p-value: 0.4131     - Tanner group III with tanner group V 

t.test(x4,x5,var.equal = TRUE )    ## H10: p-value: 0.01213     - Tanner group IV with tanner group V 


## order of p-values
pvals <- c(2.2e-16,2.2e-16,2.2e-16,2.2e-16,1.613e-06, 1.223e-11, 3.877e-10,0.2677, 0.4131,0.01213)
od <- rank(pvals,ties.method = "first") 
od
cond <- 0.05/(10+1-od) - pvals <0
min(od[cond])   ### reject H(1), ... , H(7), H(8), and accept H(9)=H8, H(10)=H9

Holm <- array(od,c(10,3))
Holm[,2] <- 1:10
Holm[,3] <- Holm[,1] < min(od[cond])

colnames(Holm) <- c("H(k)","Hk","reject")
Holm

## Hochberg


cond <- 0.05/(10+1-od) - pvals > 0
od[cond]
max(od[cond])   ### reject H(1), ... , H(7), and accept H(8)=H10, H(9)=H8, H(10)=H9


Hoch <- array(od,c(10,3))
Hoch[,2] <- 1:10
Hoch[,3] <- Hoch[,1] <= max(od[cond])

colnames(Hoch) <- c("H(k)","Hk","reject")

### Hochberg rejects on H(1), ...., H(8) and accepts H(9)=H8 and  H(10)=H9
### we reject the same null hypothesis than with Holm.

### The 10 hypotheses are not independent!  E.g. if H1 and H2 are true, then H5 must also be true. 
### There is a dependency structure -> Holm correction more appropriate!

#### comparisions with simle syntax!

?pairwise.t.test
pairwise.t.test(igf1,tanner,pool.sd=FALSE,var.equal=TRUE,p.adjust.method="no") ### don't do multiple correction

### We reject everything except: III vs. IV (H8), III vs. V (H8) -> gives the same as Holm or Hochberg in this case

### Holm correction
pairwise.t.test(igf1,tanner,pool.sd=FALSE,var.equal=TRUE,p.adjust.method="holm") ### Holm multiple correction
## this returns adjusted p-values and they have to be compared with the signiciacne level for the FWER
### We reject everything except: III vs. IV (H8), III vs. V (H8) -> gives the same as Holm or Hochberg in this case


## or 
pairwise.t.test(igf1,tanner,pool.sd=FALSE,var.equal=TRUE) ### Holm multiple correction

## Hochberg 
pairwise.t.test(igf1,tanner,pool.sd=FALSE,var.equal=TRUE,p.adjust.method="hochberg") ### Holm multiple correction


### Here the p-values are transformed - > compare each value with 0.05
## Default is Holm:igf1
## Gr

?pairwise.t.test


### F-test for equal variance

var.test(x1,x2)

length(x1[!is.na(x1)])  ## n =311

length(x2[!is.na(x2)]) ## m =70 

var(x1,na.rm=TRUE)/var(x2,na.rm=TRUE)  # test stet

### tests stat f = 0.5422207 < 1 
2*pf(var(x1,na.rm=TRUE)/var(x2,na.rm=TRUE),310,69)  ### pavalues
###

### alpha =0.05
### no multiple correction!
var.test(x1,x2)  # p-val  0.0004802  < alpha -> variances are NOT equal 
var.test(x1,x3)  # p-val  1.511e-07 < aplha  -> variances are NOT equal
var.test(x1,x4)  # p-val  0.00335 < aplha  -> variances are NOT equal
var.test(x1,x5)  # p-val  5.111e-12 < aplha  -> variances are NOT equal

var.test(x2,x3)  # p-val  0.1047  > aplha  -> variances are equal
var.test(x2,x4)  # p-val  0.8263 > aplha  -> variances are equal
var.test(x2,x5)  # p-val  0.3591 > aplha  -> variances are equal

var.test(x3,x4)  # p-val  0.08087 > aplha  -> variances are equal
var.test(x3,x5)  # p-val  0.2364 > aplha  -> variances are equal

var.test(x4,x5)  # p-val  0.2691 > aplha  -> variances are equal


### T-tests: 


t.test(x1,x2,var.equal = FALSE )    ## H1: p-value:  8.784e-15  - Tanner group I with tanner group II 
## T-test with Welch correction df= 86.583
## n=311
## m=70
s1 <- var(x1,na.rm=TRUE)
s2 <- var(x2,na.rm=TRUE)
(s1/311+s2/70)^2/(1/310*(s1/311)^2+1/69*(s2/70)^2)  ## v... deg of freedom
(mean(x1,na.rm=TRUE)-mean(x2,na.rm=TRUE))/sqrt(s1/311+s2/70) ## test stat

t.test(x1,x3,var.equal = FALSE )    ## H2: p-value: 6.153e-16  - Tanner group I with tanner group III
t.test(x1,x4,var.equal = FALSE )    ## H3: p-value: < 2.2e-16  - Tanner group I with tanner group IV
t.test(x1,x5,var.equal = FALSE )    ## H4: p-value: < 2.2e-16  - Tanner group I with tanner group V 

t.test(x2,x3,var.equal = TRUE )    ## H5: p-value: 1.613e-06  - Tanner group II with tanner group III
t.test(x2,x4,var.equal = TRUE )    ## H6: p-value: 1.223e-11  - Tanner group II with tanner group IV
t.test(x2,x5,var.equal = TRUE )    ## H7: p-value: 3.877e-10  - Tanner group II with tanner group V 

t.test(x3,x4,var.equal = TRUE )    ## H8: p-value: 0.2677     - Tanner group III with tanner group IV
t.test(x3,x5,var.equal = TRUE )    ## H9: p-value: 0.4131     - Tanner group III with tanner group V 

t.test(x4,x5,var.equal = TRUE )    ## H10: p-value: 0.01213     - Tanner group IV with tanner group V 

pvals <- c(8.784e-15,6.153e-16, 2.2e-16, 2.2e-16, 1.613e-06, 1.223e-11, 3.877e-10,0.2677, 0.4131,0.01213)
od <- rank(pvals,ties.method = "first") 


## rest as above -> same outcome in this case

## igf1 values are different across groups! What is if we only want to test whether the
## groups are all equal, rather than testing all 2 by 2 comparisions 
## -> One-Way ANOVA

boxplot(igf1 ~tanner)

lm(igf1 ~ tanner)  ## linear model for igf1 as dpendent variable and tanner as independent
## variable (co-vriate) (see later)
## output is an R object
summary(lm(igf1 ~ tanner))

sum(!is.na(igf1) & !is.na(tanner))  ## sample size n=792
## k=5 goups
## F statisic with k-1=4, n-k=787
## p value < 2.2e-16 -> igf1 is different across groups!

## an important assumption was that the variances are euqal! 
## if not -> we need another approach

### Bartlett-Test:igf1
bartlett.test(igf1~tanner)
### p-value = 2.362e-11 < 0.05 -> variances ar enot equal!


## For Levene and Brown-Forsythe tests we need package "lawstat"
#   install.packages("lawstat")
library(lawstat)

juul3 <- juul[!is.na(igf1)& !is.na(tanner), ]
?levene.test
##Brown Forsythe test
levene.test(juul3$igf1,juul3$tanner)
# pvalue: 8.922e-07 < 0.05 -> variances are unequal

## Levene test
levene.test(juul3$igf1,juul3$tanner,location="mean")
# p-value = 4.315e-07 < 0.05 -> variances are unequal

pairwise.t.test(igf1,tanner,pool.sd=TRUE) ### Holm multiple correction
### here pooled sd (variance) does not make too much sense, since the variances are unequal.


##### New data set
?energy

boxplot(energy$expend ~energy$stature)

## Exercise: is the energy intake between lean and obese women different?
## Check assumptions, is data in both groups normally distributed -> QQ plots & histogram
## Variance homogeneity
## t - test (one sided - HA: obese women have higher energy expenditure)


#### New data set

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