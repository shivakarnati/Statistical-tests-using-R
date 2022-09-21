
x <- c(175,165,175,162,180,177,165)
y <-  c(75,72,70,90, NA,72,72)

dat <- cbind(x,y)


cor(dat, use = "complete", method="kendall") ### outputs a correlation matrix

## specify x and y explicitly


cor(dat[,1],dat[,2], use = "complete", method="kendall")


cor(x,y, use = "complete", method="kendall")

#### more realistic example


library("ISwR")


plot(juul$age,juul$igf)  ## not independent


cor(juul$igf,juul$age,method="kendall") ## na because of missing values
cor(juul$igf,juul$age,method="kendall", use = "complete")
cor(juul$igf,juul$age,method="spearman", use = "complete")
cor(juul$igf,juul$age,method="pearson", use = "complete") ## inappropriate here

summary(juul)

### all variables categorical, ordinal, metric are stored as metric variables
##transorm scale levels as last time


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

### tanner score is ordinal! This is like an interval scaled age variable

#### does tanner scare corrlate wiht age?
### Pearsons coefficient not applicable! Metric & ordinal data
### Spearman and kendall -> expect kendal to be more reliable because tanner score is like interval scaled age

cor(juulNew$age,juulNew$tanner,method="kendall",use="complete")
### does not work because variables - even ifordinal have to be entered metrically!
### as it was done in the original data


cor(juul$age,juul$tanner,method="kendall",use="complete")   ## preferable in this case because interval scale
cor(juul$age,juul$tanner,method="spearman",use="complete")  ### larger value than kendall

### age and tanner score are highly correlated as expected

### has the tanner score some correlation wiht igf?
### Why does this make sense?
### Age and igf are problematic, becaut initially it is increasing and igf levels drop for grown ups. 
### Tanner score would say how correlated the developmental stage is with igf

cor(juul$igf,juul$tanner,method="kendall",use="complete")   ## preferable in this case because interval scale
cor(juul$igf,juul$tanner,method="spearman",use="complete")  ### larger value than kendall

## There is a pos. correlation if igf1 and tanner score. So igf levels are higher in individuals with higher tanner score. 