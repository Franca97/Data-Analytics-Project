###################################################################################
############################# Swiss Fertility #####################################
###################################################################################
#test
#### Loading all the relevant packages and data ####
install.packages("datasets.load")
install.packages("moments")
install.packages("stargazer")
install.packages("leaps")

library(datasets.load)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(lattice)
library(plotly)
library(knitr)
library(MASS)
library(faraway)
library(moments)
library(stargazer)
library(leaps)
#### Getting an overview of the Swiss Fertility dataset #### 
help(swiss)
View(swiss)
mydata <- swiss 


#### Re-naming the relevant variables for easier access ####
attach(mydata) # Kommentar Franca: attach(mydata) wir könnten auch nur diesen Code verwenden  anstatt sechs variablen machen
# <- mydata$Fertility
#Agriculture <- mydata$Agriculture
#Examination <- mydata$Examination
#Education <- mydata$Education
#Catholic <- mydata$Catholic
#InfantMortality <- mydata$Infant.Mortality


################################
#### Descriptive Statistics ####
################################

#### Getting a general overview of the data #### 
summary(mydata) # Franca Kommentar: Catholic: Median and Mean are completely different, also high sd (41)

stargazer(mydata, type="html",nobs=FALSE,style = "aer", iqr=FALSE , title="Table 1 - Swiss Fertility Summary Statistics", digits=2, out="Summary Statistics")


#### Drawing a boxplot for first inspection ####
# We have to watch out with "Catholic" as it is more or less a "binary" variable
boxplot(mydata)


#### Calculating the relative frequencies #### <- evtl. l?schen, da beinahe keine Werte mehrmals. Evtl mit Intervallen arbeiten? Kommentar Franca: Ja, bei continous variable oder vielen values macht es keinen Sinn, Intervalle sinnvoller 
# table(VARIABLE) Examination und Education einzige Variablen mit gleichen Variablen 
# M?ssten wir nicht die Variablen durch die Anzahl der Beobachtungen teilen? 
# length(mydata)=6. D.h. wir m?ssten wenn dann z.B. Fertility/length(mydata$fertility) berechnen? 
# Kommentar Elias: Habe die Rel. angepasst.

Rel.Freq_Fertility = Fertility / count(mydata)
Rel.Freq_Agriculture = Agriculture / count(mydata)
Rel.Freq_Examination = Examination / count(mydata)
Rel.Freq_Education = Education / count(mydata)
Rel.Freq_Catholic = Catholic / count(mydata)
Rel.Freq_InfantMortality = Infant.Mortality / count(mydata)

### Empirical Distribution Function Plots ###
plot.ecdf(Fertility, main = "Empirical Distribution Function Fertility")
plot.ecdf(Agriculture, main="Empirical Distribution Function Agriculture")
plot.ecdf(Examination, main = "Empirical Distribution Function Examination")
plot.ecdf(Catholic, main = "Empirical Distribution Function Catholic") # looks like binary 
plot.ecdf(Infant.Mortality, main = "Empirical Distribution Function Infant Mortality")


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
plot(density(Fertility), main = "Fertility")
abline(v=c(68, 70.14, 70.4), col="gray94") # a little bit right skewed, mode is defined visually, check visually with hist if more data is on the right side 
#<> Isn't it skewed to the left? Kommentar Franca: right skewed: most of the data is on the right side of the mode, mean and median are > mode 
# Merker Elias: Discuss skewnwess again


#### Inspecting and plotting the covariance and correlation #### 
cov(mydata)
cor(mydata) # correlations with response variable lower than .8. 

## Plotting the correlation matrix ##
require(lattice)
levelplot(cor(mydata), xlab = "", ylab = "")

## General plot of all variables ##
pairs(mydata, upper.panel = NULL, pch=20,cex=1.25) # assumption: linear relationship between education and examination/examination and agriculture 
# Kommentar Elias: Adjusted the matrix for more clarity


################################
###### Exploring the Data ######
################################

#### Ranking of provinces with regards to fertility rates ####
## Top 10 provinces with high fertility ## 
mydata %>% 
  dplyr::select(Fertility) %>% 
  arrange(desc(Fertility)) %>% 
  head(10)

## Top 10 provinces with low fertility ## 


mydata %>% 
  dplyr::select(Fertility) %>% 
  arrange(desc(Fertility)) %>% 
  tail(10) # Cities: Geneve, Lausanne, Nyone 


## Top 10 provinces with high education percentage 
mydata %>%  
  dplyr::select(Education) %>% 
  arrange(desc(Education)) %>% 
  head(10) # Geneve 53 (outlier)


#### Mapping relationships between different variables IV und DV (with simple regression model) ####
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

mydata %>%
  ggplot() +
  xlab("Education") +
  ylab("Fertility") +
  geom_point(mapping = aes(
    x = Education,
    y = Fertility,
    color = Education,
    size = Fertility,
    alpha = 0.5)) +
  geom_smooth(mapping = aes(
    x = Education, 
    y = Fertility),
    method = "lm")


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
# correlation between provinces with a greater proportion of catholic and high fertility rates 
# Provinces which are catholic show the highest fertility rates
# Provinces with an equaly share of catholics and protestants (50%) have a low fertility rate 
# Provinces which are fully protestant show lower fertility rates compared to fully catholic 
mydata %>%  
  ggplot() +
  geom_point(mapping = aes(x = Catholic, y = Fertility)) +
  geom_smooth(mapping = aes(x = Catholic, y = Fertility), 
              method = "lm") # Two regions as either high in catholic or low 

## Relationship Infant Mortality Fertility ##
# Some correlation between Infant Mortality and Fertility: greater percentage of children living past the 1st year correpsonding with a higher fertility measure. 
mydata %>%  
  ggplot() +
  geom_point(mapping = aes(x = Infant.Mortality, y = Fertility)) +
  geom_smooth(mapping = aes(x = Infant.Mortality, y = Fertility), 
              method = "lm")

#### Mapping relationships between different variables IV und DV (with simple regression model) ####

## Relationship Examination and Agriculture regarding Fertility ##
# Fertility rates high: for high agriculture and low examination
mydata %>%
  ggplot() +
  xlab("Agriculture") +
  ylab("Examination") +
  geom_point(mapping = aes(
    x = Agriculture,
    y = Examination,
    color = Fertility,
    size = Fertility,
    alpha = 0.5)) +
  geom_smooth(mapping = aes(
    x = Agriculture, 
    y = Examination),
    method = "lm")

## Relationship Education and Agriculture regarding Fertility ##
# similar pattern
mydata %>%
  ggplot() +
  xlab("Agriculture") +
  ylab("Education") +
  geom_point(mapping = aes(
    x = Agriculture,
    y = Education,
    color = Fertility,
    size = Fertility,
    alpha = 0.5)) +
  geom_smooth(mapping = aes(
    x = Agriculture, 
    y = Education),
    method = "lm")


## Relationship Education and Examination regarding Fertility ##
# Positive correlation between Examination and Education. Low fertility rates for residents with high education and more examination
mydata %>%
  ggplot() +
  xlab("Examination") +
  ylab("Education") +
  geom_point(mapping = aes(
    x = Examination,
    y = Education,
    color = Fertility,
    size = Fertility,
    alpha = 0.5)) +
  geom_smooth(mapping = aes(
    x = Examination, 
    y = Education),
    method = "lm")


## Density Plot for Education and Fertility ## 

ggplot(mydata, aes(x = Education, y = Fertility)) +
  geom_bin2d() +
  theme_bw() 

################################
######## Model analysis ########
################################


## Simple linear regression Fertility and Education ##
reg_simple <- lm(Fertility ~ Education, data = mydata)
summary(reg_simple)

# removing geneve doesnt change much
#mydata <- rowid_to_column(mydata, "ID") 
#View(mydata)
#data_red <- mydata[-c(45), ]
#attach(data_red)
#reg_simple2 <- lm(Fertility ~ Education, data = data_red)
#summary(reg_simple)

## Multiple Regression 
Reg_full <- lm(Fertility ~  Agriculture + Education + Examination + Catholic + Infant.Mortality, data = mydata)
summary(Reg_full) # Examination not significant 

# Why exclude examination? (=> potential OVB )
Reg_fullwoexam <- lm(Fertility ~  Agriculture + Education + Catholic + Infant.Mortality, data = mydata)
summary(Reg_fullwoexam)


## Perform a stepwise regression

lm_swiss1 <- lm(Fertility ~ 1, data = mydata) # only intercept
lm_swiss2 <- lm(Fertility ~ ., data = mydata) # full regression 

# Forward Regression
step(lm_swiss1, direction = "forward",  scope = list(lower = lm_swiss1, upper = lm_swiss2))

# Backward Regression
step(lm_swiss2, direction = "backward")

# Graphical Representation 
model_eva <- leaps::regsubsets(Fertility ~ ., data = mydata, nbest = 2) # number of best models per number of included variables 
print(summary(model_eva))

print(summary(model_eva)$which)

# Visualizing BIC and AdjR2
par(mfrow = c(1, 2))
plot(model_eva) #the lower the bic the better the model best model includes agriculture, education, catholic and infant mortality 
plot(model_eva, scale = "adjr2") # highest adj r2 all iv, second highest adjr2 includes all ivs except examination

### Diagnostics ###
par(mfrow = c(2,2))
plot(Reg_full) 
# Plot 1: Residual plot, no pattern observed constant variance assumption holds 
# Plot 2: Normal Q-Q plot, normality assumption holds 
# Plot 3: some influential points 

## Identifying influential points 
mydata[cooks.distance(Reg_full) > 0.1,] # Sierre, Rive Gauche, Porrentruy, Neuchatel, Rive Droite, Rive Gauche 




