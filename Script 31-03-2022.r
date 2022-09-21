library('xlsx')

data <- read.xlsx("/Users/kristanschneider/Meine Ablage (Kristan.schneider@hs-mittweida.de)/Lehre/SoSe2022/Sel Topic Comp Stat/ExampleData.xlsx",1)

data
summary(data)  ## all variables are read in as numerical variables -> change to categorical where appropriate

Data <- as.data.frame(data) 
Data
Data$S <- as.factor(Data$S)
summary(Data)  ## now Variable S (Sex) is a categorical variable

levels(Data$S) <- c("female","male")
Data
summary(Data)  ## now Variable S (Sex) has labels male and female rather than showing the coding 1 and 0

Data$eye <- as.factor(Data$eye)
levels(Data$eye) <- c("blue","green","brown")
summary(Data) 

#### Turn variabls hair, school into categorical variables and give appropriate labels (levels)


### Frequency table of eye color

t<- table(Data$eye)  ## counts
t
ft <- prop.table(table(Data$eye))
ft
barplot(t)
barplot(ft)
barplot(ft*100)


pie(t)
pie(ft)

