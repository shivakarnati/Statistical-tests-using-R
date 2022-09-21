
#######
#  One sample chi^2 test on varinces
#######

 
library("ISwR")



#### check whether the lecture notes are correct
n <- length(intake$pre)
Dj <- intake$pre-intake$post


####  package for chi^2 test
#install.packages("EnvStats")
library("EnvStats")
?varTest

## We want to test (HA) sigma_d^2 = 8000  -> sigma_d^2 != 8000 
## 1 sided 1 sample chi-squ test 
var(Dj)
varTest(Dj,alternative="two.sided",sigma.squared=8000)
## p-value ~0 -> reject 0 accept HA: sigma_d^2 > 8000

### test stat

chi<- var(Dj)*(n-1)/(8000)
chi   ## in fact the same as in the output of the test
var(Dj)

1- pchisq(chi,n-1)

#### Confidence interval:alternative

### 95% symmetric CI:alternative
## data is Dj

# qt() quantile function of t distribution
?qchisq  ## quantile function of chi^2 distribution
alpha <- 0.05
var(Dj)*(n-1)/qchisq(1-alpha/2,n-1)  ### lower confidence point
var(Dj)*(n-1)/qchisq(alpha/2,n-1)    ### upper confidence point

#### 99% CI 
alpha <- 0.01
var(Dj)*(n-1)/qchisq(1-alpha/2,n-1)  ### lower confidence point
var(Dj)*(n-1)/qchisq(alpha/2,n-1)    ### upper confidence point

?varTest
varTest(Dj,alternative="two.sided",conf.level = 0.99,sigma.squared=8000)

#### Wilcoxon signed rank test

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

hist(juulNew2$igf1)

x1 <- juulNew2$igf1[juulNew2$tanner== "I"]
x2 <- juulNew2$igf1[juulNew2$tanner== "II"]
x3 <- juulNew2$igf1[juulNew2$tanner== "III"]
x4 <- juulNew2$igf1[juulNew2$tanner== "IV"]
x5 <- juulNew2$igf1[juulNew2$tanner== "V"]

hist(x3)   ### group with tanner score 3 - seems symmetric

## We want to test whether igf1 levels are 400 mug/L

qqnorm(x3)  ## data does not look normally distributed enough for us
## We use a Wilcoxon signed rank test

?wilcox.test

wilcox.test(x3,mu=400)
# reject H0 and decide for HA


### Another look at data intake

intake

## data set is small
qqnorm(intake$pre)
qqnorm(intake$post)
## we might not be willing to accept that data is normally distributed. 
## we an alternative for the paired t-test

wilcox.test(intake$pre,intake$post,paired = TRUE)
## reject H0 and accept HA, that the intake pre and postmenstruation are different
## ties in data -> no exact p-values

boxplot(intake$pre,intake$post)
## From the boxplot it looks like post menstruation intake is smaller -> 1-sided test!
## HA pre menstruation intake is higher
wilcox.test(intake$pre,intake$post,paired = TRUE, alternative = "greater")
## Reject H0 and accept HA: pre menstruation intake is higher

## Alternative syntax:
wilcox.test(intake$pre - intake$post, alternative = "greater")

## Wrong syntax
wilcox.test(intake$pre , intake$post, alternative = "greater") ### this is for unpaired data!




#### Back to juul
### want to compare igf1 amoing Tanner I and Tanner II
boxplot(igf1~tanner)
qqnorm (x1)
qqnorm (x3)
# Not willing to accept that igf1 is normally distributed
# Mann Whitney U test - Wilcoxon rank sum test



wilcox.test(x1,x3)

## MWU testw with continuity correction
## reject H0 and accept HA

wilcox.test(x1,x3,exact = TRUE)
## exact p-values are not possible due to ties. 
?wilcox.test
##


### Kruscal-Wallis test to compare group means

?kruskal.test
kruskal.test(juulNew2$igf1 ~ juulNew2$tanner)
# m =5 -> 4 DF
# p-val < 0.05 -> accept the alternative the groups are systematically different



### NEW DATA set heart.rate

?heart.rate
heart.rate
### plot of the heart rate of the 9 patients
evalq(interaction.plot(time,subj,hr), heart.rate)

attach(heart.rate)
boxplot(hr ~time )

#### What we should NOT DO - 1-Way ANOVA or Kruskal Wallis Test
#### Why not -> because the groups are NOT independent - several measurements of each patient!
####  and we neglect this information

summary(lm(hr ~time))

## just the ANOVA table
anova(lm(hr ~time))
### 1 -Way ANOVA would suggest that there are no difference!
### However applying the 1-Way ANOVA is totally incorrect!

### Also wrong Kruskal-Wallis test

kruskal.test(hr ~time)
### KW test would suggest that there are no difference!
### However applying the KW is totally incorrect!


### Correct Way - 2-Way ANOVA

hr
time

subj
anova(lm(hr ~ time + subj))

## the time of measurment has a significant effect -> measurment are not identical acorss time! p-val: 0.01802
## the patients themself have a significant effect -> the patients are different as we see from plot

evalq(interaction.plot(time,subj,hr), heart.rate)

## WARNING: the 2-Way ANOVA is correct and clealy shows the effect of the drug on hear rate after administration
## the incorrect 1-Way ANOVA did not identify this

### Friedman test
?friedman.test
friedman.test(hr~subj|time)   ## we don't need to speciy the data set, because it was attached to the work space
## here there is a signifcalt difference between the patients

friedman.test(hr~time |subj) 
### there ther are systematic differences between time points of measurments -> drug has an effect on heart rate
