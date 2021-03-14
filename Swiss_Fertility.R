# Packages
#install.packages("datasets.load")
library(datasets.load)

# Setting Working Directory 
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/GitHub/Data-Analytics-Project")

# Dataset: Swiss 
help(swiss)
View(swiss)
mydata <- swiss 

### Descriptive Statistics 

# Relative Frequencies 
Rel.Freq_Fertility = mydata[,1]/length(mydata)
Rel.Freq_Agriculture = mydata[,2]/length(mydata)
Rel.Freq_Examination = mydata[,3]/length(mydata)
Rel.Freq_Education = mydata[,4]/length(mydata)
Rel.Freq_Catholic = mydata[,5]/length(mydata)
Rel.Freq_InfantMortality = mydata[,6]/length(mydata)

#' Summary 
summary(mydata)
 
#' Boxplot
boxplot(mydata)

#' Histogram 
hist(mydata$Fertility,main="Fertility",xlab="Fertility") # Fertility rates are mostly between 60 and 90% 


#'  Correlations 
cor(mydata) # correlations with response variable lower than .8. No multicollinearity. 
pairs(mydata) # assumption: linear relationship between education and examination/examination and agriculture 

 
