#shivakarnati


library('ISwR')
## load data packages 


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


#### Turn variabls hair, school into categorical variables and give appropriate labels (levels)

t <- table(Data$eye)
t
prop.table(t)


### contingency tables

table(Data$eye, Data$hair)

# sometimes combersome to always write "Data$"

table(eye,hair)  ## error objects eye and hair not defined

# attach Data to workspace 

attach(Data)

table(eye,hair)  ## no error now! 

# detach Data from workspace

detach(Data) 

table(eye,hair)  ## again error! objects eye and hair not defined again

attach(Data)


t <-table(eye,hair)  

prop.table(t)  ## entries in tbale sum up to 1

prop.table(t,margin=c(1))  ## each row sums up to 1
prop.table(t,margin=c(2))  ## each colum sums up to 1

bp <- margin.table(t,2)
barplot(bp)


### stacked bar chart
barplot(t)

### grouped bar chart:


barplot(t,beside=T)

#### summary statistics for metrix variables

summary(BS)  ## summary of body size
var(BS) ## variance
sd(BS)  ## standard deviation of body size
sqrt(var(BS)) ## gives the sma
mean(BS)
sum(BS)/length(BS)  ## calculating mean from formula
sum((BS - mean(BS))^2)/(length(BS)-1)

quantile(BS,0.1)
?quantile
### body size is continusou, chose e.g. type=4

median(BS,type=4)


#install.packages("moments")

library("moments")
?moment

moment(BS, order=1)  # gives the mean
moment(BS, order=2,central =TRUE)  # gives the mean (n-1)/n var
var(BS)
var(BS)*5/6  

skewness(BS)   ## negatively skewed left-skewed mean less than median


mean(BS)
 
median(BS)

kurtosis(BS)
