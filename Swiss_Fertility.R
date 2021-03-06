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
install.packages("mctest")
install.packages("faraway")
install.packages("lattice")
install.packages("olsrr")
install.packages("corrplot")
install.packages("GGally")

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
library(mctest)
library(faraway)
library(lattice)
library(olsrr)
library(corrplot)
library(GGally)


#### Getting an overview of the Swiss Fertility dataset #### 
help(swiss)
View(swiss)
mydata = swiss[, 1:6]
View(mydata)

#### Re-naming the relevant variables for easier access ####
attach(mydata)


###################################################################################
############################ Descriptive Stats ####################################
###################################################################################

#### Getting a general overview of the data #### 
summary(mydata) # Comment: Catholic: Median and Mean are completely different, also high sd (41)
stargazer(mydata, median =T, type = "html", nobs = FALSE, style = "qje", iqr = FALSE , title = "Table 1 - Swiss Fertility: Summary Statistics", digits = 2, out = "Summary statistics", column.sep.width="12pt")

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
abline(v=c(mean(Fertility), median(Fertility)), col="gray94") # Median and Mean close, almost normally distributed with some tail

#### Creating a covariance and correlation matrix to observe potential dependencies #### 
cov(mydata)
cor_matrix = as.matrix(cor(mydata)) # correlations with response variable lower than .8.
cor_matrix[upper.tri(cor_matrix)] <- NA
print(cor_matrix, na.print = "")
## Plotting correlation matrix for improved visual inspection ##
levelplot(cor(mydata), xlab = "", ylab = "") # visible correlation between Fertility and Education & Examination. Also, highly negative correlation between Education and Agriculture (Instrumental Variable?)

#### General plot of all variables for preliminary assumptions regarding model fit ####
pairs(mydata, upper.panel = NULL, pch = 20, cex = 1.25) # assumption: linear relationship between Education and Examination/Examination and Agriculture 
ggpairs(mydata)

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

# We check for the fit of the model #
ols_plot_obs_fit(reg_simple) # The fit does not seem completely off, but could be further improved

## We then check the residuals to observe whether there is any pattern (homo- vs. heteroskedasticity) ##
plot(reg_simple)
ols_plot_diagnostics(reg_simple)
# We plot the residuals for the simple model (check if X and U are not related) #
resi_simple = reg_simple$residuals
plot(mydata$Education, resi_simple, main = "Residuals from the linear regression (Fertility ~ Education)", xlab = "Education", ylab = "Residuals")
lines(mydata$Education, rep(0, times = length(mydata$Education)), col = 2) # no clear pattern can be observed in the residuals (can only be said for lower levels of Education given amount of datapoints)
# Additionally, we run a Q-Q plot to determine whether the residuals follow a normal distribution #
qqnorm(resi_simple)
qqline(resi_simple)
# Furthermore, a density plot of the residuals is inspected #
plot(density(resi_simple), main = "Residuals for the Simple Linear Regression Model")
# We check the normal distribution of the residuals with a Shapiro-Wilk test #
shapiro.test(resi_simple) # p-value of 0.0592 so we cannot reject the 0 hypothesis of normal distribution
# Lastly, a Breusch-Pagan test is applied to check whether there is heteroskedasticity in the data
bptest(reg_simple) #p-value of 0.5252
# Based on the large p-value from the Breusch-Pagan test and the visual inspection, we cannot reject the 0 hypothesis of homoskedasticity


######################### Polynomial Regression ##################################

#### To check whether a polynomial regression can increase explanatory power, we will run a quadratic and cubic regression ####
## We therefore first define the variables of interest ##
Education2 = Education^2
Education3 = Education^3
# And check the variables for multicollinearity #
cor(Education, Education2) # highly correlated 0.9361279
cor(Education, Education3) # rel. highly correlated 0.8389176
cor(Education2, Education3) # highly correlated 0.9734444

## We then run the polynomial models, once as quadratic and once as cubic ##
reg_simple2 <- lm(Fertility ~ Education + Education2)
reg_simple3 <- lm(Fertility ~ Education + Education2 + Education3)
summary(reg_simple2) # the very large p-values do not indicate a statistically significant impact of Education on Fertility for any power
summary(reg_simple3) # the very large p-values do not indicate a statistically significant impact of Education on Fertility for any power

## We want to extract the R squared and adj. R squared from all the models for better comparison ##
fit_matrix <- matrix(NA, nrow = 3, ncol = 2)
colnames(fit_matrix) <- c("R2", "AdjR2")
rownames(fit_matrix) <- c("Linear", "Quadratic", "Cubic")
fit_matrix[1,1] <- summary(reg_simple)$r.squared
fit_matrix[1,2] <- summary(reg_simple)$adj.r.squared
fit_matrix[2,1] <- summary(reg_simple2)$r.squared
fit_matrix[2,2] <- summary(reg_simple2)$adj.r.squared
fit_matrix[3,1] <- summary(reg_simple3)$r.squared
fit_matrix[3,2] <- summary(reg_simple3)$adj.r.squared
fit_matrix # we can see that the linear model offers the highest R squared

## Additionally, we want to inspect the different fits visually ##
plot(Education, reg_simple3$fitted.values, col = "blue", ylab = "Fitted Values", main = "Comparison of Value Fit for Polynomial Regression")
points(Education, reg_simple2$fitted.values, col = "black")
lines(Education, reg_simple$fitted.values, col = "red") # the small deviations of fit indicates that the linear model is sufficient to use

## Next, we run a non-parametric regression to support the visual findings ##
# We first estimate the optimal bandwidth given our dataset #
bandwidth <- npregbw(Fertility ~ Education)
summary(bandwidth) # the optimal bandwidth is 6.76351
# We then estimate the function using the optimal bandwidth #
npreg1 <- npreg(bws = bandwidth)
# We plot the results and add the linear regression to it #
plot(bandwith, plot.errors.method = "bootstrap", main = "Non-parametric regression")
lines(Education, simple_linear$fitted.values, col = "red")

## Additionally, we perform a cross validation to further support the findings that a linear regression model offers the best results ##
k <- 9
sigma_simple <- vector(length = k)
sigma_simple2 <- vector(length = k)
sigma_simple3 <- vector(length = k)
for(i in 1:k){
  ind <- rep(0, times = 47)
  a <- (i - 1) * 5 + 1
  b <- i * 5
  ind[a : b] = 1
  Test.F = swiss$Fertility[ind == 1]
  Test.E = swiss$Education[ind == 1]
  Train.F = swiss$Fertility[ind == 0]
  Train.E = swiss$Education[ind == 0]
  # Defining the test sets
  Train.E2 = Train.E^2
  Train.E3 = Train.E^3
  #Fitting the training data
  reg_linear <- lm(Train.F ~ Train.E)
  reg_quadratic <- lm(Train.F ~ Train.E + Train.E2)
  reg_cubic <- lm(Train.F ~ Train.E + Train.E2 + Train.E3)
  #Extract the fitted values
  coef.1 <- reg_linear$coef
  coef.2 <- reg_quadratic$coef
  coef.3 <- reg_cubic$coef
  #Calculate SSR on dataset
  sigma_simple[i] <- sum((Test.F - coef.1[1] - coef.1[2] * Test.E)^2)
  sigma_simple2[i] <- sum((Test.F - coef.2[1] - coef.2[2] * Test.E - coef.2[3] * (Test.E^2))^2)
  sigma_simple3[i] <- sum((Test.F - coef.3[1] - coef.3[2] * Test.E - coef.3[3] * (Test.E^2) - coef.3[4] * (Test.E^3))^2)
}
# We then calculate the average SSR for all three models #
avg_sigma_simple <- mean(sigma_simple)
avg_sigma_simple2 <- mean(sigma_simple2)
avg_sigma_simple3 <- mean(sigma_simple3)
avg_sigma_simple
avg_sigma_simple2
avg_sigma_simple3
# Finally, we create  a matrix to store the values #
cross_validation_matrix <- matrix(NA, nrow = 3, ncol = 1)
colnames(cross_validation_matrix) <- c("Average SSR")
rownames(cross_validation_matrix) <- c("Linear", "Quadratic", "Cubic")
cross_validation_matrix[1] <- avg_sigma_simple
cross_validation_matrix[2] <- avg_sigma_simple2
cross_validation_matrix[3] <- avg_sigma_simple3
cross_validation_matrix

## Lastly, we run a Ramsey RESET test for functional form ##
resettest(reg_simple, power = 2:3, type = "regressor") # p-value of 61.2% suggests that adding second and third order of the regressor makes no statistically significant contribution to the model

## Based on the previous findings, it can be concluded that the linear expression of the regressor seems reasonable


######################### Multivariate Regression ##################################

#### To further analyze the data and potentially increase the explanatory power of the model, we run different multivariate regressions ####

#### First, we run a full regression with all available variables ####
reg_full = lm(Fertility ~ Education + Agriculture + Examination + Catholic + Infant.Mortality, data = mydata)
summary(reg_full) # Education is still highly significant from a statistical standpoint, however Examination does not seem to have a significant influence on Fertility
# We check the model fit by investigating the observed vs. predicted plots #
ols_plot_obs_fit(reg_full) # The model seems to fit pretty well based on this visual inspection
# We run the model diagnostics on this extended model #
par(mfrow = c(2,2))
plot(reg_full) # There does not seem to be any heteroskedasticity in the model given the visual inspection of the residuals
# We also run an additional package to check the model diagnostics #
ols_plot_diagnostics(reg_full) # The residuals seem to be normally distributed and no heteroskedasticity present
mydata[cooks.distance(reg_full) > 0.1,] # Porrentruy, Sierre, Neuchatel, Rive Droite and Rive Gauche are influential points according to the Cook's Distance parameter
# We again check for normal distribution applying the Shapiro-Wilk test #
resi_full = reg_full$residuals
shapiro.test(resi_full) # p-value of 0.9318, so we cannot reject the 0 hypothesis of normal distribution
# Lastly, we run the Breusch-Pagan test on our extended model #
bptest(reg_full) #p-value of 0.321 lends support for homoskedasticity in the model
## Further, we check for multicollinearity in the model given the inclusion of different variables ##
ols_vif_tol(reg_full) # Based on the given values which are all below threshold of 4 / 5 and especially 10, there does not seem to be a problem with multicollinearity in the model
# Lastly, we check whether any of the relationships tested in the model could better be expressed by applying a higher power of the regressor (polynomial) #
# The residual plus component plots would indicate a possible non-linearity present and could suggest a transformation in the data #
ols_plot_comp_plus_resid(reg_full) #Given the current dataset, there seems to be no need for any possible transformations

#### Next, we want to check whether there could be a better (more efficient) model predicting the data, which could be based on fewer variables than the full model ####
# As observed before, Examination does not seem to be statistically significant in the full regression model. This is now checked by applying a stepwise regression #
step1 <- lm(Fertility ~ 1, data = mydata) # using only the intercept
step2 <- lm(Fertility ~ ., data = mydata) # running the full regression
# Next, we run the forward and backward regression #
step(step1, direction = "forward", scope = list(lower = step1, upper = step2))
step(step2, direction = "backward") # Both methods yield the same result: the best model does not include the variable "Examination"
# We then plot the results for better visual inspection #
model_eva <- leaps::regsubsets(Fertility ~ ., nbest = 2) # number of best models per number of included variables 
print(summary(model_eva))
print(summary(model_eva)$which) # it seems to become evident that including both variables Education and Examination decreases the goodness of the model
# We can further visualize the BIC factor and AdjR2
par(mfrow = c(1, 2))
plot(model_eva, main ="Comparison of goodness of different models") #the lower the BIC, the better the model. The  best model includes Agriculture, Education, Catholic and Infant.Mortality 
plot(model_eva, scale = "adjr2") # The highest and second highest adj. R2 are achieved by the full model and by excluding Examination
# The same results can be seen when drawing the added variable plots of the residuals of all the included variables (variables with low importance of contribution show only weak linear relationship)
ols_plot_added_variable(reg_full) # The variable Examniation does not seem to have a large contribution to the model
## We can therefore conclude that running the model without the variable Examination appears reasonable ##

#### Based on our observations from above, we run another multivariate model, excluding the variable Examination ####
reg_woEx = lm(Fertility ~ Education + Agriculture + Catholic + Infant.Mortality, data = mydata)
summary(reg_woEx)
# We again check the model fit #
ols_plot_obs_fit(reg_woEx) # again, the fit seems to be pretty well
# We run a reduced model diagnostics, not expecting any different results from before #
ols_plot_diagnostics(reg_woEx) # The residuals seem to be normally distributed and no heteroskedasticity present
mydata[cooks.distance(reg_woEx) > 0.1,] # Only Porrentruy, Sierre and Rive Gauche are now influential points according to the Cook's Distance parameter
# We run the Shapiro-Wilk and Breusch-Pagan test #
resi_woEx = reg_woEx$residuals
shapiro.test(resi_woEx) # large p value, therefore normal distribution of residuals
bptest(reg_woEx) # high p value, therefore no heteroskedasticity
# We check once more for multicollinearity between the variables #
ols_vif_tol(reg_woEx) # Unsurprisingly, there is no multicollinearity here either
# Lastly, we run a Ramsey RESET test for functional form #
resettest(reg_woEx, power = 2:3, type = "regressor") # p-value of 0.6311 suggests that adding second and third order of the regressor makes no statistically significant contribution to the model


#### Lastly, we want to make the variable Catholic a binary dummy variable (given its distribution in the dataset) and run the main regressions again ####
# First, we need to add a column with the dummy variables to the data set #
swiss$CatholicDummy = ifelse(swiss$Catholic >= 50, 1, 0)
View(swiss)
# Then, we can run the respective simple and multivariate regression models again, including the Catholic Dummy Variable #
reg_simple_Dummy = lm(Fertility ~ Education + CatholicDummy, data = swiss)
summary(reg_simple_Dummy)
reg_woEx_Dummy = lm(Fertility ~ Agriculture + Education + Infant.Mortality + CatholicDummy, data = swiss)
summary(reg_woEx_Dummy)

#### Finally, we create a Stargazer output of all the relevant models that were run ####
## Full table ##
stargazer(reg_simple, reg_simple2, reg_simple3, reg_full, reg_woEx, reg_simple_Dummy, reg_woEx_Dummy,
          type = "html",
          out = "RegressionTable.html",
          digits = 3,
          header = FALSE,
          align = TRUE,
          no.space = TRUE,
          title = "Regression Analysis of Education on Fertility",
          intercept.bottom = FALSE,
          dep.var.caption = "Impact on Fertility",
          dep.var.labels.include = FALSE,
          covariate.labels = c("Constant", "Education", "Education<sup>2</sup>", "Education<sup>3</sup>", "Agriculture", "Examination", "Catholic", "Infant.Mortality", "CatholicDummy"),
          column.labels = c("Simple", "Multivariate"),
          column.separate = c(3, 4),
          add.lines = list(c("Model", "Linear", "Quadratic", "Cubic", "Full", "w/o Exam.", "Dummy", "Dummy Full")),
          notes = "SE provided in parantheses",
          results = "asis")

## Full table 2 Pr�si##
stargazer(reg_simple, reg_simple2, reg_simple3, reg_full, reg_woEx, reg_simple_Dummy, reg_woEx_Dummy,
          type = "html",
          out = "RegressionTable2.html",
          digits = 3,
          header = FALSE,
          align = TRUE,
          no.space = TRUE,
          title = "Regression Analysis",
          dep.var.caption = "Dependent Variable: Fertility",
          intercept.bottom = FALSE,
          dep.var.labels.include = FALSE,
          covariate.labels = c("Constant", "Education", "Education<sup>2</sup>", "Education<sup>3</sup>", "Agriculture", "Examination", "Catholic", "Infant.Mortality", "CatholicDummy"),
          column.labels = c("Simple", "Multivariate"),
          column.separate = c(3, 4),
          column.sep.width= "12",
          add.lines = list(c("Model", "Linear", "Quadratic", "Cubic", "Full", "w/o Exam.", "Dummy", "Dummy Full")),
          notes = "SE provided in parantheses")
          
## Shortened table for paper ##
stargazer(reg_simple, reg_simple2, reg_simple3, reg_full, reg_woEx, reg_simple_Dummy, reg_woEx_Dummy,
          type = "html",
          out = "RegressionTableShort.html",
          digits = 3,
          header = FALSE,
          align = TRUE,
          no.space = TRUE,
          title = "Regression Analysis of Education on Fertility",
          intercept.bottom = FALSE,
          dep.var.caption = "Impact on Fertility (in %)",
          dep.var.labels.include = FALSE,
          covariate.labels = c("Constant", "Education", "Education<sup>2</sup>", "Education<sup>3</sup>", "Agriculture", "Examination", "Catholic", "Infant.Mortality", "CatholicDummy"),
          column.labels = c("Simple", "Multivariate"),
          column.separate = c(3, 4),
          omit = c("Agriculture", "Examination", "Catholic", "Infant.Mortality", "CatholicDummy"),
          add.lines = list(c("Model", "Linear", "Quadratic", "Cubic", "Full", "w/o Exam.", "Dummy", "Dummy Full")),
          omit.stat = c("rsq", "f", "ser"),
          notes = "SE provided in parantheses",
          results = "asis")


############################ Lasso Regression #####################################

#### Highly correlated variables could lead to an increased variance and introduce bias in the estimators
#### Lasso and Ridge Regression is a solution to potential multicollinearity (variable selection and regularization)
#### Lasso penalizes correlated variables: If there are two highly correlated variables, Lasso removes one randomly
#### This is a caveat for interpretation and must be verified by economic reasoning
#### The Regularization parameter Lambda governs to what degree the coefficients are penalized

## First, partitioning is needed for learning within the test / dataset
set.seed (20210508)
lasso_x <- as.matrix(mydata[ ,2:6])
lasso_y <- swiss$Fertility
size <- floor(0.75 * nrow(mydata)) # Generate variable with the rows in training data
training_set <- sample(seq_len(nrow(mydata)), size = size)
# We then perform the Lasso cross validation to find the optimal Lambda
lasso.cv <- cv.glmnet( 
  lasso_x[training_set,],
  lasso_y[training_set],
  type.measure = "mse", 
  family = "gaussian", # non binary
  nfolds = 10, # number of folds
  alpha = 1 # alpha = 1 means ridge penalty =0 and only lasso penalty remains (inbetween is a mix)
)
# The coefficients are saved for visual inspection 
coef_lasso <- coef(lasso.cv, s = "lambda.min") # save for later comparison
print(coef_lasso) # It seems like none have to be dropped to be compared to the OLS
# We then plot the cross-validation curve
plot(lasso.cv) # Grey bars are standard deviations along the Lambda sequence
# We further extract the optimal Lambda (min. Lambda) that gives minimum mean cross-validated error (vertical dotted line)
best_lambda <- lasso.cv$lambda.min
print(best_lambda)
# We then predict for the test set and calculate the MSE
predlasso1 <- predict(lasso.cv, newx = lasso_x[-training_set,], s = lasso.cv$lambda.min)
predMSElasso <- mean((lasso_y[-training_set] - predlasso1)^2)
print(predMSElasso)
rss <- sum((predlasso1 - lasso_y[-training_set])^2) #residual sum of squares 
tss <- sum((lasso_y[-training_set] - mean(lasso_y[-training_set])) ^ 2)
rsq <- 1 - rss/tss
print(rsq)

## However, our goal is not estimating on a test sample but rather understanding the effect of Education on Fertility
# For interpretation purposes, it would be interesting to have Lasso drop some variables. Therefore, the penalty must increase
# We therefore use Lasso without train/test split to see if Education gets penalized
# A Lasso cross validation is performed to find the optimal Lambda
lasso2.cv <- cv.glmnet( 
  lasso_x,
  lasso_y,
  type.measure = "mse", 
  family = "gaussian", # non binary
  nfolds = 10, # number of folds
  alpha = 1 # alpha = 1 means ridge penalty =0 and only lasso penalty remains (inbetween is a mix)
)
coef_lasso2 <- coef(lasso2.cv, s = "lambda.min") # save for later comparison
print(coef_lasso2) #From a visual inspection, this does not seem to add any additional explanatory power


############################################ END #############################################


###################################################################################
################################## BACKUP #########################################
################### Additional visualizations of plots ############################
###################################################################################

#### Another way of plotting the correlation matrix ####
corrplot(cor(mydata), method = "color")


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
# Density Plot for Education Fertility ## 
ggplot(mydata, aes(x = Education, y = Fertility)) +
  geom_bin2d() +
  theme_bw()

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


#### Mapping relationships between different variables IV und DV (with simple regression model) ####
## Relationship Examination and Agriculture regarding Fertility ##
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


#### Short Stargazer Plot v2 ####
stargazer(reg_simple, reg_simple2, reg_simple3, reg_full, reg_woEx, reg_simple_Dummy, reg_woEx_Dummy,
          type = "html",
          out = "RegressionTableShort2.html",
          digits = 3,
          header = FALSE,
          align = TRUE,
          no.space = TRUE,
          title = "Regression Analysis of Education on Fertility",
          intercept.bottom = FALSE,
          dep.var.caption = "Impact on Fertility (in %)",
          dep.var.labels.include = FALSE,
          covariate.labels = c("Constant", "Education", "Education<sup>2</sup>", "Education<sup>3</sup>", "Agriculture", "Examination", "Catholic", "Infant.Mortality", "CatholicDummy"),
          column.labels = c("Simple", "Multivariate"),
          column.separate = c(3, 4),
          omit = c("Agriculture", "Examination", "Catholic", "Infant.Mortality", "CatholicDummy"),
          add.lines = list(c("Model", "Linear", "Quadratic", "Cubic", "Full", "w/o Exam.", "Dummy", "Dummy Full"),
                           c("Additional Variables?", "No", "No", "No", "Yes", "Yes", "Yes", "Yes"),
                           c("Catholic Dummy?", "No", "No", "No", "No", "No", "Yes", "Yes")),
          omit.stat = c("rsq", "f", "ser"),
          notes = "SE provided in parantheses",
          results = "asis")