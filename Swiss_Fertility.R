###################################################################################
############################# SWISS FERTILITY #####################################
###################################################################################

############################ Preparatory steps ####################################

#### Loading all the relevant packages and data ####
install.packages("datasets.load")
install.packages("moments")
install.packages("stargazer")
install.packages("leaps")
install.packages("np")

#### Installing the necessary libraries ####
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
library(glmnet)
library(np)
library(lmtest)

#### Getting an overview of the Swiss Fertility dataset #### 
help(swiss)
View(swiss)
mydata = swiss

#### Re-naming the relevant variables for easier access ####
attach(mydata)


###################################################################################
############################ Descriptive Stats ####################################
###################################################################################

#### Getting a general overview of the data #### 
summary(mydata) # Comment: Catholic: Median and Mean are completely different, also high sd (41)
stargazer(mydata, type = "html", nobs = FALSE, style = "aer", iqr = FALSE , title = "Table 1 - Swiss Fertility Summary Statistics", digits = 2, out = "Summary Statistics")

#### Drawing a boxplot for first inspection ####
boxplot(mydata, ylab = "Occurrence", main = "Boxplot of the Swiss Fertility data set") # Comment: We have to watch out with "Catholic" as it is more or less a "binary" variable

#### Empirical Distribution Function Plots ####
plot.ecdf(Fertility, xlab = "Fertility", main = "Empirical Distribution Function Fertility")
plot.ecdf(Agriculture, xlab = "Agriculture", main ="Empirical Distribution Function Agriculture")
plot.ecdf(Examination, xlab = "Examination", main = "Empirical Distribution Function Examination")
plot.ecdf(Catholic, xlab = "Catholic", main = "Empirical Distribution Function Catholic") # Comment: looks almost binary 
plot.ecdf(Infant.Mortality, xlab = "Infant.Mortality", main = "Empirical Distribution Function Infant Mortality")

#### Histogram of the distribution of fertility and education across the whole dataset ####
hist(Fertility, main = "Fertility", xlab = "Fertility") # Fertility rates are mostly between 60 and 90% 
hist(Education, main = "Education", xlab = "Education") # Mostly lower levels of education in the dataset

#### Density plot of Fertility as dependent variable #### 
plot(density(Fertility), main = "Fertility")
abline(v=c(68, mean(Fertility), median(Fertility)), col="gray94") # a little bit right skewed, mode is defined visually, check visually with hist if more data is on the right side 
abline(v=c(mean(Fertility), median(Fertility)), col = "red") #negative skew given that the mean (70.14) is left of the median (70.4); mode does not make sense in my opinion given we have no value twice  
#<> Isn't it skewed to the left? Kommentar Franca: right skewed: most of the data is on the right side of the mode, mean and median are > mode 
# Merker Elias: Discuss skewnwess again

#### Creating a covariance and correlation matrix to observe potential dependencies #### 
cov(mydata)
cor_matrix = as.matrix(cor(mydata)) # correlations with response variable lower than .8.
cor_matrix[upper.tri(cor_matrix)] <- NA
print(cor_matrix, na.print = "")

#### Plotting correlation matrix for improved visual inspection ####
require(lattice)
levelplot(cor(mydata), xlab = "", ylab = "") # visible correlation between Fertility and Education & Examination. Also, highly negative correlation between Education and Agriculture (Instrumental Variable?)

#### General plot of all variables for preliminary assumptions regarding model fit ####
pairs(mydata, upper.panel = NULL, pch = 20, cex = 1.25) # assumption: linear relationship between Education and Examination/Examination and Agriculture 


############################ Exploring the Data ####################################

#### Ranking of provinces with regards to Fertility rates ####
## Top 10 provinces with highest fertility rates ##
mydata %>% 
  dplyr::select(Fertility) %>% 
  arrange(desc(Fertility)) %>% 
  head(10)
## Top 10 provinces with lowest fertility rates ## 
mydata %>% 
  dplyr::select(Fertility) %>% 
  arrange(desc(Fertility)) %>% 
  tail(10) # Cities: Geneve, Lausanne, Nyone 

#### Ranking of provinces with regards to Education ####
## Top 10 provinces with high education percentage ##
mydata %>%  
  dplyr::select(Education) %>% 
  arrange(desc(Education)) %>% 
  head(10) # Geneve 53 (outlier)
## Top 10 provinces with lowest education rates ##
mydata %>%  
  dplyr::select(Education) %>% 
  arrange(desc(Education)) %>% 
  tail(10)


###################################################################################
############################## Model Analysis #####################################
###################################################################################

######################## Simple Linear Regression #################################

##### First, we run a simple linear model with the two variables of interest, Fertility and Education, with Education being the regressor ####
reg_simple = lm(Fertility ~ Education, data = mydata)
summary(reg_simple)
## For a first inspection, we visualize the simple regression results ##
fit = fitted(reg_simple)
plot(Education, Fertility, main = "Fertility and Education Regression", xlab = "Education", ylab = "Fertility")
lines(Education, fit, col = 2)
# Additionally, we visualize the same data with a more complex model #
mydata %>%  
  ggplot() +
  ggtitle("Fertility and Education Regression") +
  geom_point(mapping = aes(x = Education, y = Fertility)) +
  geom_smooth(mapping = aes(x = Education, y = Fertility), 
              method = "lm") + 
  ylim(0, 100)
# And with another chart, with the size of the dots equaling the underlying values #
mydata %>%
  ggplot() +
  ggtitle("Fertility and Education Regression") +
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
    method = "lm") +
  ylim(0, 100)

## We then check the residuals to observe whether there is any pattern (homo- vs. heteroskedasticity) ##
# We plot the residuals for the simple model (check if X and U are not related) #
resi_simple = reg_simple$residuals
plot(mydata$Education, resi_simple, main = "Residuals from the linear regression (Fertility ~ Education)", xlab = "Education", ylab = "Residuals")
lines(mydata$Education, rep(0, times = length(mydata$Education)), col = 2) # no clear pattern can be observed in the residuals (can only be said for lower levels of Education given amount of datapoints)
# Additionally, we run a Q-Q plot to determine whether the residuals follow a normal distribution #
qqnorm(resi_simple)
qqline(resi_simple)
# Furthermore, a density plot of the residuals is inspected #
plot(density(resi_simple), main = "Residuals for the Simple Linear Regression Model")
# Lastly, a Breusch-Pagan test is applied to check whether there is heteroskedasticity in the data
library(lmtest)
bptest(reg_simple) #p-value of 0.5252
# Based on the large p-value from the Breusch-Pagan test and the visual inspection, we cannot reject the 0 hypothesis of homoskedasticity


######################### Polynomial Regression ##################################


## Test Linearity by adding polynomials
# Define variables
attach(mydata)
Education2 = Education^2
Education3 = Education^3

mydata <- cbind(mydata, Education2, Education3)

# Regression with polynomials 

reg_simple2 <- lm(Fertility ~ Education + Education2 + Education3, mydata)
summary(reg_simple2) # p values are very large indicate that I am not confident that my Education has an impact on the fertility

# Compare both models visually 
plot(Education, reg_simple2$fitted); 
lines(Education, reg_simple$fitted, col = "red") # if they are similar, the model should be linear, if the deviation is small the linear model is not wrong 

## Multicollinearity 
cor(Education, Education2) # highly correlated 0.9361279
cor(Education, Education3) # rel. highly correlated 0.8389176
cor(Education2, Education3) # highly correlated 0.9734444

## Smooth 
#install.packages("np")
library(np)

# Estimate the optimal band with 
Bandwidth=npregbw(Fertility~Education) # non parametric regression
summary(Bandwidth) # optimal bandwidth is 6.76351

# Estimate the function using the optimal band width 
npreg1=npreg(bws=Bandwidth)

## Compare the non-parametric regression with the univariate linear regression
# Extract the fitted from npreg1 in a standard way: 
My.fitted.values=fitted(npreg1)

# Plot it: Compare a linear model to the flexible model  
plot(Bandwidth,plot.errors.method="bootstrap")
lines(Education,fitted(reg_simple), col="2" ) # adding the linear simple model to the graph, non parametric estimate does not look linear and not very similar to the linear model 
dev.off()

### Diagnostics ###
# Fit the model and add a fitted line to the scatterplot 
plot(Fertility ~ Education, data = mydata, col = "grey", pch = 20, main = "Simple Model")
fit_1 = lm(Fertility ~ Education, data = mydata)
abline(fit_1, col = "green", lwd = 3)

# Fitted versus residuals plot: constant variance if the spread of the residuals is roughly the same; if the mean is roughly zero the linearity assumption holds 
plot(fitted(fit_1), resid(fit_1), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Simple Model: fitted vs. resiudals")
abline(h = 0, col = "darkorange", lwd = 2)

# Breusch Pagan Test: Formal test for homoscedasticity 
install.packages("lmtest")
library(lmtest)

bptest(fit_1) # large p value we dont reject h0 of homoscedasticity --> all good!

# Normal Q-Q Plots for normal distribution of residuals 
qqnorm(resid(fit_1), main = "Normal Q-Q Plot, fit_1", col = "darkgrey")
qqline(resid(fit_1), col = "dodgerblue", lwd = 2)

# formal solution for normal distribution of residuals 
shapiro.test(resid(fit_1))


par(mfrow = c(2,2))
plot(reg_simple) 
# Plot 1: Residual plot, no pattern observed constant variance assumption holds 
# Plot 2: Normal Q-Q plot, normality assumption holds 
# Plot 3: some influential points 



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

################################
############# Lasso ############
################################
# Highly correlated variables such as [example] effect OLS
# Variance explodes -> Multicolinearity
# Lasso or Ridge Regression is a Solution for Multicolinearity (variable selection and regularization)
# Lasso penalizes correlated variables: If there are two highly correlated variables, Lasso removes one randomly
# This is a caveat for interpretation and must be verified by economic reasoning
# Regularization parameter lambda governs degree of how much coefficients are penalized (lambda =0 would be normal OLS)
# Ridge adds quadratic parts to the penalty (SST: i think not necessary, Ridge supposed to work well if there are many large parameters of about the same value)
set.seed (20210508)
lasso_x <- as.matrix(swiss[ ,2:6])
lasso_y <- swiss$Fertility
#partitioning is needed for learning within the test/train dataset (splitting data into train and test data)
size <- floor(0.75 * nrow(swiss)) # Generate variable with the rows in training data
training_set <- sample(seq_len(nrow(swiss)), size = size)
# In theory one could loop this over different seeds to optimize

# Perform Lasso cross validation to find optimal lambda
lasso.cv <- cv.glmnet( 
  lasso_x[training_set,],
  lasso_y[training_set],
  type.measure = "mse", 
  family = "gaussian", # non binary
  nfolds = 10, # number of folds
  alpha = 1 # alpha = 1 means ridge penalty =0 and only lasso penalty remains (inbetween is a mix)
)

# Save and inspect coefficients
coef_lasso <- coef(lasso.cv, s = "lambda.min") # save for later comparison
print(coef_lasso)
# Seems like none have been dropped, to be compared to OLS

# Plot the cross-validation curve (red dotted line)
plot(lasso.cv)
# grey bars are standard deviations along the lambda sequence
# Lambda min is the value of lambda that gives the minimum mean cross-validated error (vertical dotted line)
best_lambda <- lasso.cv$lambda.min
print(best_lambda)

# Predict on test data
predlasso1 <- predict(lasso.cv, newx = lasso_x[-training_set,], s = lasso.cv$lambda.min)
# Calculate the MSE
predMSElasso <- mean((lasso_y[-training_set] - predlasso1)^2)
print(predMSElasso) # SST: that seems very far off, gotta check 
rss <- sum((predlasso1 - lasso_y[-training_set])^2) #residial sum of squares 
tss <- sum((lasso_y[-training_set] - mean(lasso_y[-training_set])) ^ 2)
rsq <- 1 - rss/tss
print(rsq) #R^2 of 0.22 is trash
# However, our goal is not estimating on a test sample but rather understanding the effect of edu on fertility
# For interpretation it would be interesting to have Lasso drop some variables (e.g Education)
# In order to get that done, penalty must increase (its relatively low now)
# Use Lasso without train/test split to see if edu gets penalized
# Perform Lasso cross validation to find optimal lambda
lasso2.cv <- cv.glmnet( 
  lasso_x,
  lasso_y,
  type.measure = "mse", 
  family = "gaussian", # non binary
  nfolds = 10, # number of folds
  alpha = 1 # alpha = 1 means ridge penalty =0 and only lasso penalty remains (inbetween is a mix)
)
coef_lasso2 <- coef(lasso2.cv, s = "lambda.min") # save for later comparison
print(coef_lasso2) #Not really helpful from first impression (gotta look at it again)



###################################################################################
################################## BACKUP #########################################
###################################################################################

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
print(Rel.Freq_Education)





## Creating frequency brackets for analysis ##
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


# VISUALIZATION OF PLOTS #

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
    method = "lm") +
  ylim(0, 100)


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


## What does this do? Was in chapter "Linear Regression model" with the residuals check
# Plot 
plot(reg_simple) # Normal QQ, Residuals vs. Leverage, Scale Location 

qnorm(0.95) 

