###################################################################################
############################# Swiss Fertility #####################################
###################################################################################

#### Loading all the relevant packages and data ####
install.packages("datasets.load")
library(datasets.load)
library(tidyverse)
library(ggplot2)
library(dplyr)


#### Getting an overview of the Swiss Fertility dataset #### 
help(swiss)
View(swiss)
mydata <- swiss 


#### Re-naming the relevant variables for easier access ####
Fertility <- mydata$Fertility
Agriculture <- mydata$Agriculture
Examination <- mydata$Examination
Education <- mydata$Education
Catholic <- mydata$Catholic
InfantMortality <- mydata$Infant.Mortality


################################
#### Descriptive Statistics ####
################################

#### Getting a general overview of the data #### 
summary(mydata)


#### Drawing a boxplot for first inspection ####
# We have to watch out with "Catholic" as it is more or less a "binary" variable
boxplot(mydata)


#### Calculating the relative frequencies #### <- evtl. löschen, da beinahe keine Werte mehrmals. Evtl mit Intervallen arbeiten? 
# Müssten wir nicht die Variablen durch die Anzahl der Beobachtungen teilen?
# length(mydata)=6. D.h. wir müssten wenn dann z.B. Fertility/length(mydata$fertility) berechnen?
Rel.Freq_Fertility = Fertility / length(mydata$Fertility)
Rel.Freq_Agriculture = Agriculture / length(mydata)
Rel.Freq_Examination = Examination / length(mydata)
Rel.Freq_Education = Education / length(mydata)
Rel.Freq_Catholic = Catholic / length(mydata)
Rel.Freq_InfantMortality = InfantMortality / length(mydata)

## Creating frequency brackets for analysis ##
# Warum diese Ober- und Untergrenze?
lower_bound <- 60  ## defining the lower bound as 60 ##
upper_bound <- 90  ## defining the upper bound as 90 ##
Fertility_low <- c(Fertility <= lower_bound)  ## defining low fertility as values below the lower bound ##
Fertility_average <- between(Fertility, lower_bound, upper_bound)  ## defining average fertility as values between the lower and the upper bound ##
Fertility_high <- c(Fertility >= upper_bound)  ## defining high fertility as values above the upper bound ##

Rel.Freq_Fertility_low <- sum(Fertility_low) / length(Fertility)  ## calculating the relative frequency of low fertility ##
Rel.Freq_Fertility_average <- sum(Fertility_average) / length(Fertility)  ## calculating the relative frequency of average fertility ##
Rel.Freq_Fertility_high <- sum(Fertility_high) / length(Fertility)  ## calculating the relative frequency of high fertility ##

print(Rel.Freq_Fertility_low)
print(Rel.Freq_Fertility_average)
print(Rel.Freq_Fertility_high)


#### Histogram: Distribution ####
hist(Fertility, main = "Fertility", xlab = "Fertility") # Fertility rates are mostly between 60 and 90% 


#### Density Plot of response variable #### 
plot(density(Fertility), main = "Fertility") # a little bit right skewed 
#<> Isn't it skewed to the left?


#### Inspecting and plotting the covariance and correlation #### 
cov(mydata)
cor(mydata) # correlations with response variable lower than .8. 

## Plotting the correlation matrix ##
require(lattice)
levelplot(cor(mydata), xlab = "", ylab = "")

## General plot of all variables ##
pairs(mydata) # assumption: linear relationship between education and examination/examination and agriculture 


################################
###### Exploring the Data ######
################################

#### Ranking of provinces with regards to fertility rates ####
## Top 10 provinces with high fertility ## 
mydata %>% 
  select(Fertility) %>% 
  arrange(desc(Fertility)) %>% 
  head(10)

## Top 10 provinces with low fertility ## 
mydata %>% 
  select(Fertility) %>% 
  arrange(desc(Fertility)) %>% 
  tail(10) # Cities: Geneve, Lausanne, Nyone 


#### Mapping relationships between different variables (with simple regression model) ####
## Relationship Agriculture Fertility ##
mydata %>%  
  ggplot() +
  geom_point(mapping = aes(x = Agriculture, y = Fertility)) +
  geom_smooth(mapping = aes(x = Agriculture, y = Fertility),
              method = "lm") 

## Relationship Examination Fertility ##
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

## Relationship Education Fertility ##
# Advanced model #
mydata %>%  
  ggplot() +
  geom_point(mapping = aes(x = Education, y = Fertility)) +
  geom_smooth(mapping = aes(x = Education, y = Fertility), 
              method = "lm") + 
  ylim(0, 100)
# Simple model #
# I have changed the axes
plot(y = Fertility, ylab = "Fertility", ylim = c(0,100), x = Education, xlab = "Education", 
     main = "Swiss Fertility and Education Indicators", pch = 19, col="black")
simple.regression_FertilityEdcuation <- lm(Fertility ~ Education, data = swiss)
abline(simple.regression_FertilityEdcuation, col = "red")
# Axis inverted for visual inspection #
# Why two plots? Isn't it easier to just change the first plot?
plot(x = Education, xlab = "Education", y = Fertility, ylab = "Fertility", ylim = c(0,100), 
     main = "Swiss Fertility and Education Indicators", pch = 19, col="black")
abline(simple.regression_FertilityEdcuation, col = "red")

## Relationship Catholic Fertility ##
mydata %>%  
  ggplot() +
  geom_point(mapping = aes(x = Catholic, y = Fertility)) +
  geom_smooth(mapping = aes(x = Catholic, y = Fertility), 
              method = "lm") # Two regions as either high in catholic or low 

## Relationship Infant Mortality Fertility ##
mydata %>%  
  ggplot() +
  geom_point(mapping = aes(x = Infant.Mortality, y = Fertility)) +
  geom_smooth(mapping = aes(x = Infant.Mortality, y = Fertility), 
              method = "lm")


################################
######## Model analysis ########
################################

## Full Model 

Reg_full <- lm(Fertility ~  Agriculture + Education + Examination + Catholic + Infant.Mortality, data = mydata)
summary(Reg_full) # Examination not significant 

# Why exclude examination? (=> potential OVB )
Reg_fullwoexam <- lm(Fertility ~  Agriculture + Education + Catholic + Infant.Mortality, data = mydata)
summary(Reg_fullwoexam)
