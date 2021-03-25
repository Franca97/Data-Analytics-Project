# Packages
#install.packages("datasets.load")
library(datasets.load)
library(tidyverse)
library(ggplot2)
library(dplyr)

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

#' Histogram: Distribution 
hist(mydata$Fertility,main="Fertility",xlab="Fertility") # Fertility rates are mostly between 60 and 90% 

#' Density Plot of response variable 
plot(density(mydata$Fertility), main = "Fertility") # a little bit right skewed 

#'  Correlations 
cor(mydata) # correlations with response variable lower than .8. 
pairs(mydata) # assumption: linear relationship between education and examination/examination and agriculture 


## Exploring Data

#' Top 10 Provinces with high fertility 
mydata %>% 
  select(Fertility) %>% 
  arrange(desc(Fertility)) %>% 
  head(10)

#' Top 10 Provinces with low fertility 
mydata %>% 
  select(Fertility) %>% 
  arrange(desc(Fertility)) %>% 
  tail(10) # Cities: Geneve, Lausanne, Nyone 

#' Relationship Agriculture Fertility
mydata %>%  
  ggplot() +
  geom_point(mapping = aes(x = Agriculture, y = Fertility)) +
  geom_smooth(mapping = aes(x = Agriculture, y = Fertility),
              method = "lm") 

#' Relationship Examination Fertility
mydata %>%  
  ggplot() +
  geom_point(mapping = aes(x = Examination, y = Fertility)) +
  geom_smooth(mapping = aes(x = Examination, y = Fertility),
              method = "lm") 

mydata %>%
  ggplot() +
  xlab("Examination") +
  ylab("Fertility") +
  geom_point(mapping = aes(
    x = Examination,
    y = Fertility,
    color = Examination,
    size = Fertility,
    alpha = 0.5)) +
  geom_smooth(mapping = aes(
    x = Examination, 
    y = Fertility),
    method = "lm")

#' Relationship Education Fertility
mydata %>%  
  ggplot() +
  geom_point(mapping = aes(x = Education, y = Fertility)) +
  geom_smooth(mapping = aes(x = Education, y = Fertility),
              method = "lm") 

#' Relationship Catholic Fertility
mydata %>%  
  ggplot() +
  geom_point(mapping = aes(x = Catholic, y = Fertility)) +
  geom_smooth(mapping = aes(x = Catholic, y = Fertility),
              method = "lm") # Two regions as either high in catholic or low 

#' Relationship Infant Mortality Fertility
mydata %>%  
  ggplot() +
  geom_point(mapping = aes(x = Infant.Mortality, y = Fertility)) +
  geom_smooth(mapping = aes(x = Infant.Mortality, y = Fertility),
              method = "lm")


### Empirical Analysis

## Full Model 
Reg_full <- lm(Fertility ~  Agriculture + Education + Examination + Catholic + Infant.Mortality, data = mydata)
summary(Reg_full) # Examination not significant 

Reg_fullwoexam <- lm(Fertility ~  Agriculture + Education + Catholic + Infant.Mortality, data = mydata)
summary(Reg_fullwoexam)


