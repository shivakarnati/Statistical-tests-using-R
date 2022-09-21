

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

### we want to test whether igf1 =380 for individuals with Tanner score III


## select only igf1 in samples with tanner =III

igftannerIII <- juulNew[juulNew$tanner =="III" , "igf1"]  ## select all samples with Tanner=III
## this does not exclude NAs

igftannerIII <- igftannerIII[!is.na(igftannerIII)] # this excludes NAs

hist(igftannerIII)   ## looks normal enough

qqnorm(igftannerIII) ## looks normal enough

## now 1-sample 2-sided t-test for Ho: mu = 380
## default significance level alpha=0.05
?t.test
t.test(igftannerIII,mu=380)

## p-value 4.236e-05 < 0.05  -> reject H0 and accept HA

### is this test realy what we defined in the course?

# Test statistic

t <- sqrt(45)*(mean(igftannerIII)-380)/(sd(igftannerIII))
t

## pvalue

?pt
2* pt(-abs(t),44)  # correct (twice the left tail)
2* (1-pt(abs(t),44))  # is the same as twice the right tail


# aceptance region
## lower bound
qt(0.05/2,44) 
## upper bound
qt(1-0.05/2,44)
-qt(0.05/2,44)
### Acceptance region is [-2.015368, 2.015368]
## test statistic = 4.546923 is not in the acceptance region -> pval < 0.05

## different H0: mu=460

t.test(igftannerIII,mu=460)
## p-value > 0.05 -> accept H0
### value of test statistic t = 1.0229 lies in acceptance region 


### Exercise perform the t-tests for tanner = I, II, IV and V with H0: mu=483 (mean of tanner = III)