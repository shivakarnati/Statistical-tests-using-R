

data <- array(,c(4,2))

data[,1] <- c(175,182,183,190)  # height in cm

data[,2] <- c(65,75,80,85)  # weight in kg


data

cov(data[,1],data[,2])  
# the covariance between weight and height
# 50.83333

### Take the same data and change the units! 

data1 <- array(,c(4,2))

data1[,1] <- c(175,182,183,190)/100  # height in m

data1[,2] <- c(65,75,80,85)  # weight in kg

data1


cov(data1[,1],data1[,2])  
# 0.5083333  Covaraince changed although the data is essentially the same

### correlation coefficient

# first scatter plot

plot(data[,1],data[,2])

cor(data[,1],data[,2])  ## Pearson' s corr coff =0.9699662
##stron linear relation

# now for the rescaled data:
cor(data1[,1],data1[,2])  ## Pearson' s corr coff =0.9699662
##stron linear relation

### while the co-variances dependet on the scaleing, the corelation coefficient doen't


### another example
library("ISwR")

?juul
tail(juul)  #https://en.wikipedia.org/wiki/Tanner_scale

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

boxplot(juulNew$age)
hist(juulNew$age)
plot(juul$age)
qqnorm(juul$age)  # age not normally distributed in the data, 
# Also we don't have a sample that resembles the age distribution in th epopulatoion, we have maily younger people enrolled


boxplot(juulNew$igf1)
hist(juulNew$igf1)

qqnorm(juulNew$igf1)


plot(juulNew[,c("age","igf1")])


cor(juulNew$age,juulNew$igf1) # the NAs in the data leed to no result


cor(juulNew$age,juulNew$igf1,use = "complete")

## the corelation coefficient is close to 0! 
## is the data really uncorrelated?
## look just at age < 20

juulNew$age < 20  ## returns logical vactir
sel <- juulNew$age < 20  
X <- juulNew$age[sel]  # selects only the rows where age <20
Y <- juulNew$igf1[sel]  # selects only the rows where age <20

cor(X,Y,use = "complete") ## 0.6432274
## for participants < 20 years of age there is a clear pos. correlation between age and igf1


juulNew$age > 20  ## returns logical vector
sel <- juulNew$age > 20  
X <- juulNew$age[sel]  # selects only the rows where age >20
Y <- juulNew$igf1[sel]  # selects only the rows where age >20

cor(X,Y,use = "complete") ## -0.6878961
## for participants older than 20 years of age there is a clear neg. correlation between age and igf1

### example for non-linear monotone depence


### example for non-linear monotone depence

X <- rnorm(200)
Y <- X^3  ## Y is a function of X
plot(X,Y)
cor(X,Y) ## although Y completely depends monotonically on X, the correlation coefficient is < 1
cor(X,Y,method="spearman")


### different examples - the problem with outliers

X <- rnorm(400)
Y <- rnorm(400)
plot(X,Y)
cor(X,Y) ## the data are independent of each other
cor(X,Y,method="spearman")

### data looks uncorrelated

#Lets add one outlier (100,100)

X1 <- c(X,100)
Y1 <- c(Y,100)
plot(X1,Y1)
cor(X1,Y1) ## very high correlation!
cor(X1,Y1,method="spearman") ## still very low correlation
