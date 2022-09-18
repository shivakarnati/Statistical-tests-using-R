library('xlsx') # To import the base data


#Reading dataframe as Dataset
Dataset <- read.xlsx("/home/shivakrishnakarnati/Documents/computational statistics/Project/S2Dataset.xlsx",sheet=1)
Dataset
#Duplicating the Dataset as Data for further calculations
Data <- as.data.frame(Dataset)

# Task 3

# To partition categorical variables into 3 groups
Data$group <- as.factor(Data$group) 
# Changing categorical variable as factor, we have 3 levels here
levels(Data$group) <- c("Fitbit-only", "Fitbit-Message", "Fitbit-Message+goal") 
summary(Data)

# Task 4
# Define a new variable “obesity” as follows. Obesity is “underweight”, “normal”, “overweight”, “obese”,
# “extremely obese” by bmi_v1 < 18.5, 18,5<= bmi_v1 < 25, 25<=bmi_v1<30, 30<=bmi_v1<35,
# 35<=bmi_v1.
  
# To create a new variable and assign to "underweight", "normal", "overweight", "obese", "extremely obese"
Data$obesity <- NA
Dt <- Data$bmi_v1

for(i in 1:length(Dt)){
  if(Dt[i] <18.5){
    Data$obesity[i] <- "underweight"
  } else if (Dt[i] >18 & Dt[i] <25){
    Data$obesity[i] <- "normal"
  } else if (Dt[i] >25 & Dt[i] <30){
    Data$obesity[i] <- "overweight"
  }else if(Dt[i] >30 & Dt[i] <35){
    Data$obesity[i] <- "obese"
  }else if(Dt[i] >35){
    Data$obesity[i] <- "extremely obese"
  }
}


# 5. Provide a frequency table (absolute and relative counts + marginals) of the variable “obesity” vs. “male”
#    categorical variables alongside with bar charts.

# Absolute frequency
# f1+f2+..+fn = N
# fi - frequency of each value
# N - total no. of data values
# We use table() function for absolute frequency
af <- table(Data$obesity,Data$male)
af
# Relative frequency (ni) = fi/length(fi)
rf <- af/length(af)
rf

# Marginal table
margin.table(af,1) # no. of overweight, obese, extremely obese are 96
margin.table(af,2) # females are 96 and males are 31
# Barplot
barplot(af)
barplot(rf)

# 6. Provide summary statistics for the variables” weight_v1”, “sbp_v1”, “dbp_v1”, “glucose_v1”,
#    “insulin_v1”, boxplots, and histograms

# Summary statistics of the variables "weight_v1", "sbp_v1", "dbp_v1", "glucose_v1", "insulin_v1"
# It's only possible for metric variables

attach(Data)

# box plots
boxplot(weight_v1,sbp_v1,dbp_v1,glucose_v1,insulin_v1)
# histograms
hist(weight_v1)
hist(sbp_v1)
hist(dbp_v1)
hist(glucose_v1)
hist(insulin_v1)

hist(weight_v1,sbp_v1,dbp_v1,glucose_v1,insulin_v1) # Error:'probability' is an alias for '!freq', however they differ


# 7. Investigate any if there is a correlation between “weight_v1” and “glucose_v1”. Provide a scatter plot.
#    Which correlation coefficient do you prefer in this case? Can you come up with a hypothesis? Perform
#    a test (based on your hypothesis) concerning the correlation coefficient

cor(Data$weight_v1,Data$glucose_v1,use = "complete", method = ("pearson")) #0.06784542
cor(Data$weight_v1,Data$glucose_v1,use = "complete", method = ("spearman")) #0.04475156
cor(Data$weight_v1,Data$glucose_v1,use = "complete", method = ("kendall")) # 0.04016933
# The correlation coefficient is close to 0!
# Here there is no specifically difference between pearson, spearman, kendall coefficients, 
# But we can use "person" correlation coefficients, because there are no much outliers in data

# Scatter plot
plot(Data$weight_v1,Data$glucose_v1)

# Test for correlation
#              Null Hypothesis H0 := Weight_v1 and glucose_v1 are uncorrelated
#       Alternative Hypothesis H1 := weight_v1 and glucose_v1 are correlated 

t.test(Data$weight_v1,Data$glucose_v1)
# Based on p-value(>0.05) we are failed to reject Null hypothesis,
# Which means weight_v1 and glucose_v1 are uncorrelated.
# Type II error 

# 8. To see whether the distributions of “glucose_v1” is comparable among males and females, perform a
#    two-sided test (t-test and a non-parametric alternative). Check whether you can safely assume equal
#    variances (test for this too). If the variances are equal, would you use the Welch-correction (yes or no
#    and why)?

# Assigning the glucose_v1 with female and male respectively 
x1<- Data$glucose_v1[Data$male=="0"]
x2 <- Data$glucose_v1[Data$male=="1"]

# Performing two sample two-sided test
t.test(x1,x2,alternative="two.sided") 

# Non-parametric alternative (Unpaired two-samples Wilcoxon test)
#  is a non-parametric alternative to the unpaired two-samples t-test, 
#  which can be used to compare two independent groups of samples.
#  It’s used when your data are not normally distributed.
wilcox.test(x1,x2,alternative = "two.sided")

# Test for equality of variance

#             Null Hypothesis: sample variances are Equal
#      Alternative Hypothesis: sample variances are not Equal

# F-Test formula :       Larger sample variance
#                  F = -------------------------
#                        Smaller sample variance

# Alpha = 0.05

var.test(x1,x2)

# We can assure that variances are not equal
# To check manually
var(Data$glucose_v1,Data$male=="0",na.rm = TRUE)
var(Data$glucose_v1,Data$male=="1",na.rm = TRUE)
# Here we can perform Welch correction because the variances are not equal

#9.Compare the distribution of “glucose_v1” across the three different groups “Fitbit-Only”, “Fitbit-
#Message”, “Fitbit-Message + goal”. Perform a 1-Way ANOVA and a nonparametric alternative. Which
#would you prefer in this case? (Argue based on whether the assumptions for the respective tests hold –
#this might require even more testing.) If age cannot be assumed to be identical across the groups,
#perform pairwise t-tests (with a common estimate for variance) and identify which groups are different.
#Apply a multiple test-correction. Which test correction would you prefer?

?lm

anova(lm(glucose_v1~group))

# Kruskal-Wallis test by rank is a non-parametric alternative to one-way ANOVA test,
# which extends the two-samples Wilcoxon test in the situation where there are more than two groups. 
# It’s recommended when the assumptions of one-way ANOVA test are not met. 

# significance level (alpha) = 0.05

kruskal.test(glucose_v1 ~group)

# As the p-value is less than the significance level 0.05,
# we can conclude that there are significant differences between the glucose and  fitbit groups.


#The paired samples t-test is used to compare the means between two related groups of samples
pairwise.t.test(glucose_v1,group,pool.sd=FALSE,var.equal=TRUE,p.adjust.method="no",alternative = "two.sided") 

#10. Perform a t-test (and a non-parametric alternative) to check whether “glucose_v1” and “glucose_v3”
#are significantly different in the 3 groups (1 test per group). Provide 90% confidence intervals for the
#true difference in glucose at the first and third visit.

t.test(Data$glucose_v1,Data$glucose_v3)

#11. Perform a 2-Way-ANOVA separately for the groups “Fitbit-Only”, “Fitbit-Message”, “Fitbit-Message +
#  goal” to compare the measurements “glucose_v1”, “glucose_v2“, “glucose_v3”. Can they be assumed
#to be identical? Also perform a non-parametric alternative to the 2-Way-ANOVA.





