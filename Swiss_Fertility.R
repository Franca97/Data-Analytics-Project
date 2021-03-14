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

# Boxplots 
boxplot(mydata)

# Correlations 
cor(mydata)

### Simple Linear Regression 
