---
title: "GLM code"
author: "Sanni Sipola"
date: '2023-04-18'
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


# Exploratory data analysis

```{r}
data <- my_data <- read.delim("C:\\R\\data_report_data.txt", sep="")
head(data)
#str(data)
#fix(data)
```


```{r, fig.height=3}
# All character variables need to be converted to factors
cols <- c("breast", "pregnancy", "howfed", "howfedfr", "partner", "smokenow", "smokebf", "ethnic")
data[cols] <- lapply(data[cols], factor)
summary(data)
```


```{r}
#Plotting explanatory variables against response variable

# Boxplots for continuous explanatory variables
attach(data)
par(mfrow=c(1,2))
boxplot(age ~ breast)
text(y=fivenum(data$age), labels = fivenum(data$age), x=0.5, col=c("blue", "orange", "darkgreen", "orange", "blue"))
text(y=mean(data$age), labels =round(mean(data$age),2), x=1.5, col="red")
boxplot(educat ~ breast)
text(y=fivenum(data$educat), labels = fivenum(data$educat), x=0.5, col=c("blue", "orange", "darkgreen", "orange", "blue"))
text(y=mean(data$educat), labels =round(mean(data$educat),2), x=1.5, col="red")

# Plots for categorial explanatory variables
par(mfrow=c(2,4))
plot(pregnancy,breast,  xlab="Pregnancy", ylab="Breast")
plot(howfed,breast,  xlab="Howfed", ylab="Breast")
plot(howfedfr,breast,  xlab="Howfedfr", ylab="Breast")
plot(partner,breast,  xlab="Partner", ylab="Breast")
plot(smokenow,breast,  xlab="Smokenow", ylab="Breast")
plot(smokebf,breast,  xlab="Smokebf", ylab="Breast")
plot(ethnic,breast,  xlab="Ethnic", ylab="Breast")
```

VARIABLES AGAINST OTHER VARIABLES

```{r}
# Plots for explanatory variables  against other explanatory variables
attach(data)
par(mfrow=c(3,6))
plot(howfed,pregnancy,  xlab="Howfed", ylab="pregnancy")
plot(howfedfr,pregnancy,  xlab="Howfedfr", ylab="pregnancy")
plot(partner,pregnancy,  xlab="Partner", ylab="pregnancy")
plot(smokenow,pregnancy,  xlab="Smokenow", ylab="pregnancy")
plot(smokebf,pregnancy,  xlab="Smokebf", ylab="pregnancy")
plot(ethnic,pregnancy,  xlab="Ethnic", ylab="pregnancy")

plot(pregnancy,howfed,  xlab="Pregnancy", ylab="howfed")
plot(howfedfr,howfed,  xlab="Howfedfr", ylab="howfed")
plot(partner,howfed,  xlab="Partner", ylab="howfed")
plot(smokenow,howfed,  xlab="Smokenow", ylab="howfed")
plot(smokebf,howfed,  xlab="Smokebf", ylab="howfed")
plot(ethnic,howfed,  xlab="Ethnic", ylab="howfed")

plot(pregnancy,howfedfr,  xlab="Pregnancy", ylab="howfedfr")
plot(howfed,howfedfr,  xlab="Howfed", ylab="howfedfr")
plot(partner,howfedfr,  xlab="Partner", ylab="howfedfr")
plot(smokenow,howfedfr,  xlab="Smokenow", ylab="howfedfr")
plot(smokebf,howfedfr,  xlab="Smokebf", ylab="howfedfr")
plot(ethnic,howfedfr,  xlab="Ethnic", ylab="howfedfr")

plot(pregnancy,partner,  xlab="Pregnancy", ylab="partner")
plot(howfed,partner,  xlab="Howfed", ylab="partner")
plot(howfedfr,partner,  xlab="Howfedfr", ylab="partner")
plot(smokenow,partner,  xlab="Smokenow", ylab="partner")
plot(smokebf,partner,  xlab="Smokebf", ylab="partner")
plot(ethnic,partner,  xlab="Ethnic", ylab="partner")

plot(pregnancy,smokenow,  xlab="Pregnancy", ylab="smokenow")
plot(howfed,smokenow,  xlab="Howfed", ylab="smokenow")
plot(howfedfr,smokenow,  xlab="Howfedfr", ylab="smokenow")
plot(partner,smokenow,  xlab="Partner", ylab="smokenow")
plot(smokebf,smokenow,  xlab="Smokebf", ylab="smokenow")
plot(ethnic,smokenow,  xlab="Ethnic", ylab="smokenow")

plot(pregnancy,smokebf,  xlab="Pregnancy", ylab="smokebf")
plot(howfed,smokebf,  xlab="Howfed", ylab="smokebf")
plot(howfedfr,smokebf,  xlab="Howfedfr", ylab="smokebf")
plot(partner,smokebf,  xlab="Partner", ylab="smokebf")
plot(smokenow,smokebf,  xlab="Smokenow", ylab="smokebf")
plot(ethnic,smokebf,  xlab="Ethnic", ylab="smokebf")

par(mfrow=c(2,4))
boxplot(age ~ pregnancy)
boxplot(age ~ howfed)
boxplot(age ~ howfedfr)
boxplot(age ~ partner)
boxplot(age ~ smokenow)
boxplot(age ~ smokebf)
boxplot(age ~ ethnic)

par(mfrow=c(2,4))
boxplot(educat ~ pregnancy)
boxplot(educat ~ howfed)
boxplot(educat ~ howfedfr)
boxplot(educat ~ partner)
boxplot(educat ~ smokenow)
boxplot(educat ~ smokebf)
boxplot(educat ~ ethnic)

group <- ifelse(breast=="Breast", "Group_Breast", "Group_Bottle")

plot(age, educat, col=factor(group)) #group by breast and get different colors!

```

```{r}
#install.packages("psych")
library(psych)
pairs.panels(data) #to show color grouping
```

not ok for categorical variables?
```{r}
data2 <- data
data2[cols] <- lapply(data[cols], as.numeric)
library(ggcorrplot)
corr <- round(cor(data2), 1) # default pearson, correlation matrix
ggcorrplot(corr, hc.order = TRUE,
    type = "lower", p.mat = cor_pmat(corr)) # non significant (5%) banned
```

```{r}
# Studying the interaction between smokenow and smokebf
data3 <- data
data3$groups <- ifelse(smokenow=="Yes", 1,0)
par(mfrow=c(1,3))
plot(smokebf[data3$groups==1],breast[data3$groups==1],  xlab="Smokebf", ylab="Breast", main="smokenow = Yes")
plot(smokebf[data3$groups==0],breast[data3$groups==0],  xlab="Smokebf", ylab="Breast", main="smokenow = No")
plot(smokebf,breast,  xlab="Smokebf", ylab="Breast", main="Both smokenow Yes and No")
summary(data$smokebf)

#install.packages("confintr")
library(confintr)
cramersv(cbind(data$smokenow, data$smokebf))

cor(as.numeric(data$smokebf), as.numeric(data$smokenow))
```
The coefficient of "smokebf" is positive. Does it actually take into account only observations where smokenow=no? Of all women, for those who smoke before, the proportion of breast feeders is lower than for those who have not smoked. This should follow that the coefficient is negative?


# Model selection
- link function: three most popular are logit, probit and complementary (also ... , but those dont scale in 0...1)
 + from these logit is easiest to interpret, probit fits better models (and complementary log log?). and as our aim is interpretation and not prediction, I choose to use logit link function.

# Model selection
- Modelling binary data, David C, 2013
 + In practice, there are likely to be a number of equally good models, rather than a single ‘best’ model. For this reason, it is desirable to consider a wide range of possible models.
 + When the number of potential explanatory variables, including interactions, non-linear terms and so on, is not too large, it might be feasible to fit all possible combinations of terms, paying due regard to the hierarchic principle.
!!! for us now the number is large
 + When there are no subject matter grounds for model choice, the model chosen for initial consideration from a set of alternatives might be the one for which the value of the deviance is a minimum. (p. 92)
- variables with inter relationships
 + In some applications, information might be recorded on a number of variables, all of which relate to the same general feature. For example, the variables height, weight, head circumference, body mass index (weight/height2), and so on, are all concerned with the size of an individual. In view of inter relationships between these variables, a model for the response probability may not need to include each of them. It would then be appropriate to de termine which variables from this group should be included in the model, although it may not matter exactly which variables are chosen.
  + The number of variables is large (p>10, over thousand possible models). Therfore its good to use forward selection, backward elimination or stepwise procedure.

# Model Fitting

# model1

```{r}
str(breast)
# In the glm() help "For binomial and quasibinomial families the response can also be specified as a factor (when the first level denotes failure and all others success)" Meaning that Bottle = failure, Breast = success
model1 <- glm(breast~., family=binomial(link = "logit"), data)
summary(model1)
```

```{r}
dres1 <- rstandard(model1) #standardized deviance residuals
pres1 <- resid(model1, type="pearson")/sqrt(1 - hatvalues(model1)) #standardizer pearson residuals
```

# Goodness of fit of the model

- deviance not suitable for binary
- Hosmer and lemeshow (good as we have different covariate patterns for observations)
 + For ungrouped binary data we can use HL statistic to measure a goodness of fit of the model (Modelling binary data, David Collet 2013, p. 88)
 + However, since the numerical value of the statistic is dependent on the
number of groups chosen, the numbers of observations within the groups, and
on how sets of observations with a common fitted probability are split, the
P-value of the significance test should not be interpreted rigidly, and is best
taken to be an informal guide as to the adequacy of the model. (p. 88)
 + H0: fitted model is satisfactory

# Residuals plots for model1

- For GLM, residuals proposed have been deviance, pearson and response residuals.
- standardized residuals can be plotted against
 + fitted values (structure?)
 + normal quantiles
 + continuous explanatory variables (non linearity?)
- For binomial:
 + deviance residuals against linear predictor (not helpful)
 + mean linear predictor - mean dev res
 + mean residuals binned according to covariates (provided the numbers
of observations for each covariate pattern are not too small - which might be the case for me as the data is not that large) - what is covariate pattern, actually???
 
BOOK:
- logistic regression: pearson and deviance residuals
- should be plotted against:
 + each continuous explanatory variable (is assumption of linearity appropriate?)
 + order of measurement (if applicable - for me not!)
 + normality plot - should be approx normal, if number of observations for each covariate pattern is not too small (for me probably 1 - ??)
So I am going to plot:
- mean linear predictor - mean dev res
- stand dev - fitted
- stad dev - normal quantiles
- stand res - continuous explanatory (or mean res binned against covariates?)

```{r}
dres <- residuals(model1) #deviance residuals
linpred <- predict(model1) #values of the linear predictor of all the observations
fitted <- fitted(model1)
plot(dres ~ linpred, xlab="Linear predictor", ylab="Deviance residual")

require(lmtest)
require(faraway)
require(dplyr)
require(ggplot2)

# Binned deviance residuals against values of the linear predictor
data2 <- data
data2 <- mutate(data2, residuals=residuals(model1), linpred=predict(model1)) #add residuals and linear predictor
gdf <- group_by(data2, cut(linpred, breaks=unique(quantile(linpred, (1:10)/10))))
diagdf <- summarise(gdf, residuals=mean(residuals), linpred=mean(linpred))
plot(residuals ~ linpred, diagdf, xlab="Mean linear predictor", ylab="Mean deviance residual")

plot(dres, fitted)
```

```{r}
library(car)

par(mfrow=c(2,3))
qqPlot(dres1, id=FALSE, main = "QQ-Plot of Residuals")
plot(model1,1)
plot(model1,2)
plot(model1,3)
plot(model1,4, id.n=5)
plot(model1,5) #cooks distance lines- no obs outside - no influential obs

library(car)
outlierTest(model1) # no studentized residuals - we keep the observations
```

```{r}
library(faraway)
dres <- rstandard(model1) #standardized deviance residuals
pres <- resid(model1, type="pearson")/sqrt(1 - hatvalues(model1)) #standardizer pearson residuals

par(mfrow=c(2,3))
plot(dres, ylab = "Standardized Deviance Residuals")
plot(pres, ylab="Standardized Pearson residuals")
hist(dres)
qqnorm(residuals(model1))
halfnorm(hatvalues(model1)) # shows big values
hist(pres)

library(car)
par(mfrow=c(2,3))
qqPlot(dres1, id=FALSE, main = "QQ-Plot of Residuals")
plot(model1,1)
plot(model1,2)
plot(model1,3)
plot(model1,4, id.n=5)
plot(model1,5) #cooks distance lines- no obs outside - no influential obs

outlierTest(model1) # no studentized residuals - we keep the observations
library("generalhoslem")
logitgof(breast, fitted(model1), g = 10, ord = FALSE) # HL test

```






```{r}
pchisq(model1$deviance, model1$df.residual, lower=F)
pchisq(model1$null.deviance-model1$deviance, model1$df.null-model1$df.residual, lower=F)
```

# Variable selection:
```{r}
#drop1(model_full)
library(MASS)
stepAIC(model1)
```

```{r, BIC based step}
log(length(breast)) #4.905275

BICstep <- step(model1, k=log(length(breast))) # using k=n instead of 2, it´s as using BIC as a criterior instead of AIC. BIC penilize more and favors a simpler model.
BICstep
# https://www.maths.usyd.edu.au/u/UG/SM/STAT3022/r/current/Lecture/lecture11_2020JC.html#19
```

# model2 - from stepAIC
```{r}
model2 <- glm(breast ~ pregnancy + howfedfr + partner + smokenow + smokebf + educat + ethnic, family=binomial(link = "logit"), data)
summary(model2)
```

```{r}
library(faraway)
dres <- rstandard(model2) #standardized deviance residuals
pres <- resid(model2, type="pearson")/sqrt(1 - hatvalues(model2)) #standardizer pearson residuals

par(mfrow=c(2,3))
plot(dres, ylab = "Standardized Deviance Residuals")
plot(pres, ylab="Standardized Pearson residuals")
hist(dres)
qqnorm(residuals(model2))
halfnorm(hatvalues(model2)) # shows big values
hist(pres)

library(car)
par(mfrow=c(2,3))
qqPlot(dres1, id=FALSE, main = "QQ-Plot of Residuals")
plot(model2,1)
plot(model2,2)
plot(model2,3)
plot(model2,4, id.n=5)
plot(model2,5) #cooks distance lines- no obs outside - no influential obs

outlierTest(model2) # no studentized residuals - we keep the observations
library("generalhoslem")
logitgof(breast, fitted(model2), g = 10, ord = FALSE) # HL test

```
# model3 - without smokebf 
```{r}
model3 <- glm(breast ~ pregnancy + howfed + howfedfr + partner + smokenow + age + educat + ethnic, family=binomial(link = "logit"), data)
summary(model3)
```
AIC larger than for the one with smokebf

```{r}
library(faraway)
dres <- rstandard(model3) #standardized deviance residuals
pres <- resid(model3, type="pearson")/sqrt(1 - hatvalues(model3)) #standardizer pearson residuals

par(mfrow=c(2,3))
plot(dres, ylab = "Standardized Deviance Residuals")
plot(pres, ylab="Standardized Pearson residuals")
hist(dres)
qqnorm(residuals(model3))
halfnorm(hatvalues(model3)) # shows big values
hist(pres)

library(car)
par(mfrow=c(2,3))
qqPlot(dres1, id=FALSE, main = "QQ-Plot of Residuals")
plot(model3,1)
plot(model3,2)
plot(model3,3)
plot(model3,4, id.n=5)
plot(model3,5) #cooks distance lines- no obs outside - no influential obs

outlierTest(model3) # no studentized residuals - we keep the observations
library("generalhoslem")
logitgof(breast, fitted(model3), g = 10, ord = FALSE) # HL test

```

```{r, stepAIC}
stepAIC(model3)
```

stepAIC reduced model without smokebf
```{r}
model4 <- glm(breast ~ howfedfr + partner + smokenow + ethnic, family=binomial(link = "logit"), data)
summary(model4)
```

```{r}
library(faraway)
dres <- rstandard(model4) #standardized deviance residuals
pres <- resid(model4, type="pearson")/sqrt(1 - hatvalues(model4)) #standardizer pearson residuals

par(mfrow=c(2,3))
plot(dres, ylab = "Standardized Deviance Residuals")
plot(pres, ylab="Standardized Pearson residuals")
hist(dres)
qqnorm(residuals(model4))
halfnorm(hatvalues(model4)) # shows big values
hist(pres)

library(car)
par(mfrow=c(2,3))
qqPlot(dres1, id=FALSE, main = "QQ-Plot of Residuals")
plot(model4,1)
plot(model4,2)
plot(model4,3)
plot(model4,4, id.n=5)
plot(model4,5) #cooks distance lines- no obs outside - no influential obs

outlierTest(model4) # no studentized residuals - we keep the observations
library("generalhoslem")
logitgof(breast, fitted(model4), g = 10, ord = FALSE) # HL test

```


# Comparing nested models

```{r}
library(lmtest)
lrtest(model1, model2) # 2 better
lrtest(model3, model4) # 4 better
```

# Difference in deviances test to test if one variable is significant to the model.

# Quasibinomials of model1 and model2
```{r}
model1q <- glm(breast ~ ., family=quasibinomial(link = "logit"), data)
summary(model1q)
```

```{r}
library(faraway)
dres <- rstandard(model1q) #standardized deviance residuals
pres <- resid(model1q, type="pearson")/sqrt(1 - hatvalues(model1q)) #standardizer pearson residuals

par(mfrow=c(2,3))
plot(dres, ylab = "Standardized Deviance Residuals")
plot(pres, ylab="Standardized Pearson residuals")
hist(dres)
qqnorm(residuals(model1q))
halfnorm(hatvalues(model1q)) # shows big values
hist(pres)

library(car)
par(mfrow=c(2,3))
qqPlot(dres1, id=FALSE, main = "QQ-Plot of Residuals")
plot(model1q,1)
plot(model1q,2)
plot(model1q,3)
plot(model1q,4, id.n=5)
plot(model1q,5) #cooks distance lines- no obs outside - no influential obs

outlierTest(model1q) # no studentized residuals - we keep the observations
library("generalhoslem")
logitgof(breast, fitted(model1q), g = 10, ord = FALSE) # HL test

```


```{r}
model2q <- glm(breast ~ pregnancy + howfedfr + partner + smokenow + smokebf + educat + ethnic, family=quasibinomial(link = "logit"), data)
summary(model2q)
```

```{r}
library(faraway)
dres <- rstandard(model2q) #standardized deviance residuals
pres <- resid(model2q, type="pearson")/sqrt(1 - hatvalues(model2q)) #standardizer pearson residuals

par(mfrow=c(2,3))
plot(dres, ylab = "Standardized Deviance Residuals")
plot(pres, ylab="Standardized Pearson residuals")
hist(dres)
qqnorm(residuals(model2q))
halfnorm(hatvalues(model2q)) # shows big values
hist(pres)

library(car)
par(mfrow=c(2,3))
qqPlot(dres1, id=FALSE, main = "QQ-Plot of Residuals")
plot(model2q,1)
plot(model2q,2)
plot(model2q,3)
plot(model2q,4, id.n=5)
plot(model2q,5) #cooks distance lines- no obs outside - no influential obs

outlierTest(model2q) # no studentized residuals - we keep the observations
library("generalhoslem")
logitgof(breast, fitted(model2q), g = 10, ord = FALSE) # HL test

```


# 
***
# Other models I tried

```{r}
model1_interaction <- glm(formula = breast ~ pregnancy +howfed+ howfedfr +partner+ smokebf*smokenow + age + educat + ethnic, family = binomial, data = data)
summary(model1_interaction)
```

```{r, not in the report, include=FALSE}
# By choosing the variables that were significant in the full model:
model6 <- glm(formula = breast ~ howfedfr + smokenow +ethnic, family = binomial, data = data) # link=logit is default
summary(model6)
# Later I found that a model with more variables is better, so I leave this out of the report.
```




```{r}
model_smoke_interaction <- glm(formula = breast ~ pregnancy + smokenow*smokebf + howfedfr + partner + ethnic  + educat + age, family = binomial(link = "logit"),data = data) # link=logit is default
summary(model_smoke_interaction)
```

```{r}
model_full <- glm(formula = breast ~ pregnancy + smokenow*smokebf + howfedfr*ethnic + partner*ethnic + ethnic*smokebf + educat*smokenow + age + smokenow*ethnic + smokenow*howfedfr, family = binomial(link = "logit"),data = data) # link=logit is default
summary(model_full)

stepAIC(model_full)
```
What to do with the interaction smokenow:smokebf ?

from model_full stepAIC - **AIC 109.7**
```{r}
model_nested <- glm(breast ~ pregnancy + smokenow + smokebf + howfedfr + ethnic + partner + educat + ethnic:partner + smokebf:ethnic, family = binomial(link = "logit"), data = data)
summary(model_nested)
```
New model, more complex one, has smaller AIC and residual deviance than model2a.

```{r}
logitgof(breast, fitted(model_nested), g = 10, ord = FALSE) # model ok
lrtest(model_full, model_nested) #simpler better
lrtest(model2, model_nested) # simpler better, not so big difference though - we could leave ethnic-partner away
```
HL test suggest the model_nested is good. LR test suggest that nested is better than complex, but nested would not necessarily be better than model2a, only at 15% significance level! 

So which one to choose?


```{r}
sd_res_deviance2 <- rstandard(model2) #standardized deviance residuals
sd_res_pearson2 <- resid(model2, type="pearson")/sqrt(1 - hatvalues(model2)) #standardizer pearson residuals
par(mfrow=c(2,3))
plot(sd_res_deviance2, ylab = "Standardized Deviance Residuals")
plot(sd_res_pearson2, ylab="Standardized Pearson residuals")
hist(sd_res_deviance2)
qqnorm(residuals(model2))
halfnorm(hatvalues(model2)) # shows big values
hist(sd_res_pearson2)

dres_nested <- rstandard(model_nested) #standardized deviance residuals
pres_nested <- resid(model_nested, type="pearson")/sqrt(1 - hatvalues(model_nested)) #standardizer pearson residuals
par(mfrow=c(2,3))
plot(dres_nested, ylab = "Standardized Deviance Residuals")
plot(pres_nested, ylab="Standardized Pearson residuals")
hist(dres_nested)
qqnorm(residuals(model_nested))
halfnorm(hatvalues(model_nested)) # shows big values
hist(pres_nested)

```

Maybe residuals of the model_nested actually look a bit better?? Hard to say.

More tests for the normality of the residuals:
Shapiro-Wilk test or the Kolmogorov-Smirnov test. Alternatively, you can use the “Residuals vs. Fitted”-plot, a Q-Q plot, a histogram, or a boxplot.
```{r}
ks.test(sd_res_deviance2, pnorm)
ks.test(dres_nested, pnorm)

par(mfrow=c(2,2))
boxplot(sd_res_deviance2)
boxplot(sd_res_pearson2)
boxplot(dres_nested)
boxplot(pres_nested)


```
residuals do not seem normal, but they dont seem normal for any of the models

Let´s see how the model_nested would be as a quasibinomial
```{r}
model_nested_q <- glm(breast ~ pregnancy + smokenow + smokebf + howfedfr + ethnic + partner + educat + ethnic:partner + smokebf:ethnic, family = quasibinomial(link = "logit"), data = data)
summary(model_nested_q)
```


HL suggest good fit 
ks.test says the residuals are not normally distributed. p-values is though larger than for model_nested and model2a
The plots also suggest they are not normally distributed. Looks still slightly better than those for binomial.

As for now we would stay the model_nested_q: quasibinomial without age and howfed, with interactions ethnic:partner and ethnic:smokebefore, is the best one

Should we still examine more interaction terms before choosing the model?


# Another approach to model selection

Modelling Binomial Data, David C, 2013. Page 93. "Instead of using automatic variable selection procedures, the following general strategy for model selection is recommended".

- Process consists of 4 steps, shortly described before each.
- Author writes: *"When using this selection procedure, rigid application of a particular significance level should be avoided. In order to guide decisions on whether to include or omit a term, the significance level should not be too small; a level of around 10% is recommended."*
- In each step, when I add or remove a variable from the model, I compare that new model to the previous one by calculating:
 + difference in deviances (+ sign means the deviance increased by the change, - sign means the deviance decreased)
 + likelihood ratio test lrtest(modelA,modelB) (same as pchisq(difference in deviances, difference in degrees of freedom))
- I add these results to a table
 + difference in deviances
 + p-value of a LR-test
 
# Step1
- We compare models with single variables to the null model (only intercept).
- Variables that cause **significant (10% level) decrease in deviance** will be included in the model.
```{r}
table <- data.frame()
#------------------------------------------
model_ <- glm(breast ~ pregnancy, family = binomial(link = "logit"), data = data)
table <- rbind(table, c("only intercept", "+ pregnancy", model_$deviance-model_$null.deviance , lrtest(model_)$Pr[2], AIC(model_)))

model_ <- glm(breast ~ howfed, family = binomial(link = "logit"), data = data)
table <- rbind(table, c("only intercept", "+ howfed", model_$deviance-model_$null.deviance , lrtest(model_)$Pr[2], AIC(model_)))

model_ <- glm(breast ~ howfedfr, family = binomial(link = "logit"), data = data)
table <- rbind(table, c("only intercept", "+ howfedfr", model_$deviance-model_$null.deviance , lrtest(model_)$Pr[2], AIC(model_)))

model_ <- glm(breast ~ partner, family = binomial(link = "logit"), data = data)
table <- rbind(table, c("only intercept", "+ partner", model_$deviance-model_$null.deviance , lrtest(model_)$Pr[2], AIC(model_)))

model_ <- glm(breast ~ smokenow, family = binomial(link = "logit"), data = data)
table <- rbind(table, c("only intercept", "+ smokenow", model_$deviance-model_$null.deviance , lrtest(model_)$Pr[2], AIC(model_)))

model_ <- glm(breast ~ smokebf, family = binomial(link = "logit"), data = data)
table <- rbind(table, c("only intercept", "+ smokebf", model_$deviance-model_$null.deviance , lrtest(model_)$Pr[2], AIC(model_)))

model_ <- glm(breast ~ age, family = binomial(link = "logit"), data = data)
table <- rbind(table, c("only intercept", "+ age", model_$deviance-model_$null.deviance , lrtest(model_)$Pr[2], AIC(model_)))

model_ <- glm(breast ~ educat, family = binomial(link = "logit"), data = data)
table <- rbind(table, c("only intercept", "+ educat", model_$deviance-model_$null.deviance , lrtest(model_)$Pr[2], AIC(model_)))

model_ <- glm(breast ~ ethnic, family = binomial(link = "logit"), data = data)
table <- rbind(table, c("only intercept", "+ ethnic", model_$deviance-model_$null.deviance , lrtest(model_)$Pr[2],AIC(model_)))

#------------------------------------------
colnames(table) <- c("model A", "model B", "deviance B - deviance A", "LR-test p-value","AIC model B" )
table$`deviance B - deviance A` <- as.numeric(table$`deviance B - deviance A`)
table$`deviance B - deviance A` <- round(table$`deviance B - deviance A`, 4)
table$`AIC model B` <- as.numeric(table$`AIC model B`)
table$`AIC model B` <- round(table$`AIC model B`, 4)
table
```
Decrease in deviance is significant (at 10% level) when we add variables: **howfed, howfedfr, partner, smokenow, smokebf, educat and etchnic**.
We continue with a model without variables pregnancy and age.

breast ~ howfed + howfedfr + partner + smokenow + smokebf + educat + ethnic

# Step 2
- We compute the change in the value of the deviance when each variable on its own is omitted from the model above.
- Only variables whose deletion cause significant increase in deviance will stay in the model.
```{r}
compare_models <- function(modelA, modelB){
  # Model A is the model to start with, model B is the model after change
  # deviance: deviance(modelB)-deviance(modelA)
  c(modelB$deviance-modelA$deviance , lrtest(modelA,modelB)$Pr[2], AIC(modelB))}
#--------------------------------------------
model_all <- glm(breast ~ howfed + howfedfr + partner + smokenow + smokebf + educat + ethnic, family = binomial(link = "logit"), data = data) 

model_ <- glm(breast ~ howfedfr + partner + smokenow + smokebf + educat + ethnic, family = binomial(link = "logit"), data = data) 
table <- rbind(table, c("- age - pregnancy", "- age - pregnancy - howfed", compare_models(model_all,model_)))

model_ <- glm(breast ~ howfed + partner + smokenow + smokebf + educat + ethnic, family = binomial(link = "logit"), data = data) 
table <- rbind(table, c("- age - pregnancy", "- age - pregnancy - howfedfr", compare_models(model_all,model_)))

model_ <- glm(breast ~ howfed + howfedfr + smokenow + smokebf + educat + ethnic, family = binomial(link = "logit"), data = data) 
table <- rbind(table, c("- age - pregnancy", "- age - pregnancy - partner", compare_models(model_all,model_)))

model_ <- glm(breast ~ howfed + howfedfr + partner + smokebf + educat + ethnic, family = binomial(link = "logit"), data = data) 
table <- rbind(table, c("- age - pregnancy", "- age - pregnancy - smokenow", compare_models(model_all,model_)))


model_ <- glm(breast ~ howfed + howfedfr + partner + smokenow + educat + ethnic, family = binomial(link = "logit"), data = data) 
table <- rbind(table, c("- age - pregnancy", "- age - pregnancy - smokebf", compare_models(model_all,model_)))


model_ <- glm(breast ~ howfed + howfedfr + partner + smokenow + smokebf + ethnic, family = binomial(link = "logit"), data = data) 
table <- rbind(table, c("- age - pregnancy", "- age - pregnancy - educat", compare_models(model_all,model_)))


model_ <- glm(breast ~ howfed + howfedfr + partner + smokenow + smokebf + educat, family = binomial(link = "logit"), data = data) 
table <- rbind(table, c("- age - pregnancy", "- age - pregnancy - ethnic", compare_models(model_all,model_)))
#----------------------------------------------
table$`deviance B - deviance A` <- as.numeric(table$`deviance B - deviance A`)
table$`deviance B - deviance A` <- round(table$`deviance B - deviance A`, 4)
table$`AIC model B` <- as.numeric(table$`AIC model B`)
table$`AIC model B` <- round(table$`AIC model B`, 4)
table
```
Book: *Only those that lead to a significant increase in the value of the deviance are retained in the model. Once a variable has been dropped, the effect of omitting each of the remaining variables in turn should be examined.*

Increase in deviance is significant (at 10% level) when the following variables are removed: howfedfr, smokenow, smokebf, ethnic. We therefore drop variables howfed, partner and education. Also AIC of these models is smaller than for other.


# Step 2 continues
*Once a variable has been dropped, the effect of omitting each of the remaining variables in turn should be examined.*

```{r}
# Model with 4 vairables from previous step: 
model_4 <- glm(breast ~ howfedfr + smokenow + smokebf + ethnic, family = binomial(link = "logit"), data = data)
#-----------------
model_ <- glm(breast ~ smokenow + smokebf + ethnic, family = binomial(link = "logit"), data = data) 
table <- rbind(table, c("4", "- howfedfr", compare_models(model_4,model_)))

model_ <- glm(breast ~ howfedfr + smokebf + ethnic, family = binomial(link = "logit"), data = data) 
table <- rbind(table, c("4", "- smokenow", compare_models(model_4,model_)))

model_ <- glm(breast ~ howfedfr + smokenow + ethnic, family = binomial(link = "logit"), data = data) 
table <- rbind(table, c("4", "- smokebf", compare_models(model_4,model_)))

model_ <- glm(breast ~ howfedfr + smokenow + smokebf, family = binomial(link = "logit"), data = data) 
table <- rbind(table, c("4", "- ethnic", compare_models(model_4,model_)))
#----------------------
table$`deviance B - deviance A` <- as.numeric(table$`deviance B - deviance A`)
table$`deviance B - deviance A` <- round(table$`deviance B - deviance A`, 4)
table$`AIC model B` <- as.numeric(table$`AIC model B`)
table$`AIC model B` <- round(table$`AIC model B`, 4)
table
```
Result suggest that by removing smokebf the deviance does not increase significantly (at 10% level). We leave that out as well and continue with following model:

breast ~ howfedfr + smokenow + ethnic

Before continuing to the step 3, I also check what stepAIC and BIC would say about the model we got in step 1.

stepwise selection with AIC and BIC:
```{r}
stepAIC(model_all)

BICstep <- step(model_all, k=log(length(breast))) # using k=n instead of 2, it´s as using BIC as a criterior instead of AIC. BIC penilize more and favors a simpler model.
# https://www.maths.usyd.edu.au/u/UG/SM/STAT3022/r/current/Lecture/lecture11_2020JC.html#19
```
AIC suggest a model:
breast ~ howfedfr + partner + smokenow + smokebf + ethnic

BIC suggest a model:
breast ~ howfedfr + smokenow + ethnic

Stepwise selection using AIC would leave variables partner and smokebf to the model. Selection using BIC suggest the same model we got in our step 2.


# Step3
We try adding the two variables left out at the very beginning to this model with only three variables.

```{r}
# Model with 3 vairables from previous step: 
model_3 <- glm(breast ~ howfedfr + smokenow + ethnic, family = binomial(link = "logit"), data = data)
#-----------------
model_ <- glm(breast ~ howfedfr + smokenow + ethnic + pregnancy, family = binomial(link = "logit"), data = data)
table <- rbind(table, c("3", "+ pregnancy", compare_models(model_3,model_)))

model_ <- glm(breast ~ howfedfr + smokenow + ethnic + age, family = binomial(link = "logit"), data = data)
table <- rbind(table, c("3", "+ age", compare_models(model_3,model_)))
#----------------------
table$`deviance B - deviance A` <- as.numeric(table$`deviance B - deviance A`)
table$`deviance B - deviance A` <- round(table$`deviance B - deviance A`, 4)
table$`AIC model B` <- as.numeric(table$`AIC model B`)
table$`AIC model B` <- round(table$`AIC model B`, 4)
table
```
Decrease in deviance is not significant.

We now check if interactions make a differens in this model.
- among variables we already have:
howfedfr:smokenow
smokenow:ethnic
howfedfr:ethnic

- Possible interactions for other reasons:
- ethnicity of a person might tell them coming from differenc culture, and due to cultural reasons there could be interactions between ethnic and other variables like smoking, education or partner.
- educational level might have an impact on persons lifestyle and therefore to the decision to smoke
ethnic:educat
ethnic:smokebf
ethnic:smokenow
ethnic:partner
smokebf:educat
smokenow:educat


```{r}
# Model with 3 vairables from previous step: 
model_3 <- glm(breast ~ howfedfr + smokenow + ethnic, family = binomial(link = "logit"), data = data)
#-----------------
model_ <- glm(breast ~ howfedfr*smokenow + ethnic, family = binomial(link = "logit"), data = data)
table <- rbind(table, c("3", "+ howfedfr:smokenow", compare_models(model_3,model_)))

model_ <- glm(breast ~ howfedfr + smokenow*ethnic, family = binomial(link = "logit"), data = data)
table <- rbind(table, c("3", "+ smokenow:ethnic", compare_models(model_3,model_)))

model_ <- glm(breast ~ howfedfr*ethnic + smokenow, family = binomial(link = "logit"), data = data)
table <- rbind(table, c("3", "+ howfedfr:ethnic", compare_models(model_3,model_)))
#-----------------------
model_ <- glm(breast ~ howfedfr + smokenow + ethnic*educat, family = binomial(link = "logit"), data = data)
table <- rbind(table, c("3", "+ ethnic:educat + educat", compare_models(model_3,model_)))
model_ <- glm(breast ~ howfedfr + smokenow + ethnic*smokebf, family = binomial(link = "logit"), data = data)
table <- rbind(table, c("3", "+ ethnic:smokebf + smokebf", compare_models(model_3,model_)))
model_ <- glm(breast ~ howfedfr + smokenow + ethnic*partner, family = binomial(link = "logit"), data = data)
table <- rbind(table, c("3", "+ ethnic:partner + partner", compare_models(model_3,model_)))

model_ <- glm(breast ~ howfedfr + ethnic + smokenow*educat, family = binomial(link = "logit"), data = data)
table <- rbind(table, c("3", "+ smokenow:educat + educat", compare_models(model_3,model_)))
model_ <- glm(breast ~ howfedfr +ethnic + smokenow + smokebf*educat, family = binomial(link = "logit"), data = data)
table <- rbind(table, c("3", "+ smokebf:educat + smokebf + educat", compare_models(model_3,model_)))
#----------------------
table$`deviance B - deviance A` <- as.numeric(table$`deviance B - deviance A`)
table$`deviance B - deviance A` <- round(table$`deviance B - deviance A`, 4)
table$`AIC model B` <- as.numeric(table$`AIC model B`)
table$`AIC model B` <- round(table$`AIC model B`, 4)
table
```
adding following interactions (and the variables alone, according to hierarcical principle) causes significant decrease in deviance:

- ethnic:educat + educat
- ethnic:partner + partner
- smokebf:educat + smokebf + educat

The model including both of these:
```{r}
#-----------------------
model_ <- glm(breast ~ howfedfr + smokenow + ethnic*educat + ethnic*partner, family = binomial(link = "logit"), data = data)
table <- rbind(table, c("3", "+ ethnic*educat + ethnic*partner", compare_models(model_3,model_)))

model_ <- glm(breast ~ howfedfr + smokenow + ethnic*educat + smokebf*educat, family = binomial(link = "logit"), data = data)
table <- rbind(table, c("3", "ethnic*educat + smokebf*educat", compare_models(model_3,model_)))
model_ <- glm(breast ~ howfedfr + smokenow + smokebf*educat + ethnic*partner, family = binomial(link = "logit"), data = data)
table <- rbind(table, c("3", "smokebf*educat + ethnic*partner", compare_models(model_3,model_)))

model_ <- glm(breast ~ howfedfr + smokenow + smokebf*educat + ethnic*partner + ethnic*educat, family = binomial(link = "logit"), data = data)
table <- rbind(table, c("3", "three interactions", compare_models(model_3,model_)))
#----------------------
table$`deviance B - deviance A` <- as.numeric(table$`deviance B - deviance A`)
table$`deviance B - deviance A` <- round(table$`deviance B - deviance A`, 4)
table$`AIC model B` <- as.numeric(table$`AIC model B`)
table$`AIC model B` <- round(table$`AIC model B`, 4)
table
```
Adding any two of these interactions reduces the deviance significacntly. Also AIC is lower in two of these cases than in any of the earlier models. Adding all three interactions also seems a good idea. However thos should be compared to the models before:

```{r}
model_3_interactions <- glm(breast ~ howfedfr + smokenow + smokebf*educat + ethnic*partner + ethnic*educat, family = binomial(link = "logit"), data = data)
#-----------------------
model_ <- glm(breast ~ howfedfr + smokenow + ethnic*educat + ethnic*partner, family = binomial(link = "logit"), data = data)
table <- rbind(table, c("eth*edu + eth*part", "three interactions", compare_models(model_,model_3_interactions)))

model_ <- glm(breast ~ howfedfr + smokenow + ethnic*educat + smokebf*educat, family = binomial(link = "logit"), data = data)
table <- rbind(table, c("eth*edu + sbf*edu", "three interactions", compare_models(model_,model_3_interactions)))
model_ <- glm(breast ~ howfedfr + smokenow + smokebf*educat + ethnic*partner, family = binomial(link = "logit"), data = data)
table <- rbind(table, c("sbf*edu + eth*part", "three interactions", compare_models(model_,model_3_interactions)))
#----------------------
table$`deviance B - deviance A` <- as.numeric(table$`deviance B - deviance A`)
table$`deviance B - deviance A` <- round(table$`deviance B - deviance A`, 4)
table$`AIC model B` <- as.numeric(table$`AIC model B`)
table$`AIC model B` <- round(table$`AIC model B`, 4)
table

colnames(table) <- c("model A", "model B", "deviance B - deviance A", "LR-test p-value","AIC model B" )
```
Three interactions is better than ethnic:educat + smokebf:educat
ethnic:educat + ethnic:partner is better than three interactions
smokebf:educat + ethnic:partner is better than three interactions

In principle of parsimony, we continue with a models with only two interactions. They are not nested models so we can not do a difference in deviances test. 
- ethnic:educat + ethnic:partner has AIC 109.3145 (model A)
- smokebf:educat + ethnic:partner has AIC 110.0315 (model B)

I perform HL test for these two models (with differeng numbers of groups, as the test is very sensitive to those):
```{r}
model_A <- glm(breast ~ howfedfr + smokenow + ethnic*educat + ethnic*partner, family = binomial(link = "logit"), data = data)

model_B <- glm(breast ~ howfedfr + smokenow + smokebf*educat + ethnic*partner, family = binomial(link = "logit"), data = data)

library("generalhoslem")
logitgof(breast, fitted(model_A), g = 10, ord = FALSE)
logitgof(breast, fitted(model_B), g = 10, ord = FALSE)

logitgof(breast, fitted(model_A), g = 9, ord = FALSE)
logitgof(breast, fitted(model_B), g = 9, ord = FALSE)

logitgof(breast, fitted(model_A), g = 8, ord = FALSE)
logitgof(breast, fitted(model_B), g = 8, ord = FALSE)

logitgof(breast, fitted(model_A), g = 11, ord = FALSE)
logitgof(breast, fitted(model_B), g = 11, ord = FALSE)

logitgof(breast, fitted(model_A), g = 12, ord = FALSE)
logitgof(breast, fitted(model_B), g = 12, ord = FALSE)

```
All results suggest the models are a good fit.


# Step 4
We remove and add variables one at the time to see if there is a significant change in deviance.

```{r}
model_A <- glm(breast ~ howfedfr + smokenow + ethnic*educat + ethnic*partner, family = binomial(link = "logit"), data = data)

# add: howfed pregnancy smokebf age
model_ <- glm(breast ~ howfedfr + smokenow + ethnic*educat + ethnic*partner + howfed, family = binomial(link = "logit"), data = data)
table <- rbind(table, c("Model A ", "+howfed", compare_models(model_A,model_)))
model_ <- glm(breast ~ howfedfr + smokenow + ethnic*educat + ethnic*partner + pregnancy, family = binomial(link = "logit"), data = data)
table <- rbind(table, c("Model A ", "+pregnancy", compare_models(model_A,model_)))
model_ <- glm(breast ~ howfedfr + smokenow + ethnic*educat + ethnic*partner + smokebf, family = binomial(link = "logit"), data = data)
table <- rbind(table, c("Model A ", "+smokebf", compare_models(model_A,model_)))
model_ <- glm(breast ~ howfedfr + smokenow + ethnic*educat + ethnic*partner+age, family = binomial(link = "logit"), data = data)
table <- rbind(table, c("Model A ", "+age", compare_models(model_A,model_)))

# remove terms from model_A
model_ <- glm(breast ~ smokenow + ethnic*educat + ethnic*partner, family = binomial(link = "logit"), data = data)
table <- rbind(table, c("Model A ", "-howfedfr", compare_models(model_A,model_)))
model_ <- glm(breast ~ howfedfr + ethnic*educat + ethnic*partner, family = binomial(link = "logit"), data = data)
table <- rbind(table, c("Model A ", "-smokenow", compare_models(model_A,model_)))

model_ <- glm(breast ~ howfedfr + smokenow + ethnic*educat + ethnic*partner, family = binomial(link = "logit"), data = data)
table <- rbind(table, c("Model A ", "+", compare_models(model_A,model_)))

model_ <- glm(breast ~ howfedfr + smokenow + ethnic*educat + ethnic*partner, family = binomial(link = "logit"), data = data)
table <- rbind(table, c("Model A ", "+", compare_models(model_A,model_)))

model_ <- glm(breast ~ howfedfr + smokenow + ethnic*educat + ethnic*partner, family = binomial(link = "logit"), data = data)
table <- rbind(table, c("Model A ", "+", compare_models(model_A,model_)))

model_ <- glm(breast ~ howfedfr + smokenow + ethnic*educat + ethnic*partner, family = binomial(link = "logit"), data = data)
table <- rbind(table, c("Model A ", "+", compare_models(model_A,model_)))


#-----------------
model_B <- glm(breast ~ howfedfr + smokenow + smokebf*educat + ethnic*partner, family = binomial(link = "logit"), data = data)

# add terms to model_B add: howfed pregnancy age
model_ <- glm(breast ~ howfedfr + smokenow + smokebf*educat + ethnic*partner +howfed, family = binomial(link = "logit"), data = data)
table <- rbind(table, c("Model B ", "+howfed", compare_models(model_B,model_)))
model_ <- glm(breast ~ howfedfr + smokenow + smokebf*educat + ethnic*partner +pregnancy, family = binomial(link = "logit"), data = data)
table <- rbind(table, c("Model B ", "+pregnancy", compare_models(model_B,model_)))

model_ <- glm(breast ~ howfedfr + smokenow + smokebf*educat + ethnic*partner+age, family = binomial(link = "logit"), data = data)
table <- rbind(table, c("Model B ", "+age", compare_models(model_B,model_)))

# remove terms from model_B
model_ <- glm(breast ~ smokenow + smokebf*educat + ethnic*partner, family = binomial(link = "logit"), data = data)
table <- rbind(table, c("Model B ", "-howfedfr ", compare_models(model_B,model_)))
model_ <- glm(breast ~ howfedfr + smokebf*educat + ethnic*partner, family = binomial(link = "logit"), data = data)
table <- rbind(table, c("Model B ", "- smokenow", compare_models(model_B,model_)))





#----------------------
table$`deviance B - deviance A` <- as.numeric(table$`deviance B - deviance A`)
table$`deviance B - deviance A` <- round(table$`deviance B - deviance A`, 4)
table$`AIC model B` <- as.numeric(table$`AIC model B`)
table$`AIC model B` <- round(table$`AIC model B`, 4)
table

```
adding smokebf to the model A decreases the deviance significantly and leads to lower AIC 108.133942780054
removing terms only leads to increase in deviace with significant values.

Adding terms to model B does not reduce deviance significantly, removing increases significantly. (however with pregnancy could be considered?)

To further study model_A + smokebf: let´s call it model_C

```{r}
model_C<- glm(breast ~ howfedfr + smokenow + ethnic*educat + ethnic*partner + smokebf, family = binomial(link = "logit"), data = data)

# add: howfed pregnancy age
model_ <- glm(breast ~ howfedfr + smokenow + ethnic*educat + ethnic*partner + smokebf + howfed, family = binomial(link = "logit"), data = data)
table <- rbind(table, c("Model C ", "+howfed", compare_models(model_C,model_)))

model_ <- glm(breast ~ howfedfr + smokenow + ethnic*educat + ethnic*partner + smokebf + pregnancy, family = binomial(link = "logit"), data = data)
table <- rbind(table, c("Model C ", "+pregnancy", compare_models(model_C,model_)))

model_ <- glm(breast ~ howfedfr + smokenow + ethnic*educat + ethnic*partner + smokebf + age, family = binomial(link = "logit"), data = data)
table <- rbind(table, c("Model C ", "+age", compare_models(model_C,model_)))
#---------------------------
#removing variables
model_<- glm(breast ~  smokenow + ethnic*educat + ethnic*partner + smokebf, family = binomial(link = "logit"), data = data)
table <- rbind(table, c("Model C ", "-howfedfr ", compare_models(model_C,model_)))
model_<- glm(breast ~ howfedfr + ethnic*educat + ethnic*partner + smokebf, family = binomial(link = "logit"), data = data)
table <- rbind(table, c("Model C ", "- smokenow", compare_models(model_C,model_)))
model_<- glm(breast ~ howfedfr + smokenow +  ethnic*partner + smokebf, family = binomial(link = "logit"), data = data)
table <- rbind(table, c("Model C ", "- ethnic*educat", compare_models(model_C,model_)))
model_<- glm(breast ~ howfedfr + smokenow + ethnic*educat +  smokebf, family = binomial(link = "logit"), data = data)
table <- rbind(table, c("Model C ", "- ethnic*partner", compare_models(model_C,model_)))
```

Adding variables to model_C does not decrease deviance significantly. However, when pregnancy is added, the p-value is 0.12 (not that faraway from 10) and AIC is 107.7. This model could still be considered for further.
Removing variables increases deviane significantly.

we call model_C + pregnancy = model_D

```{r}
model_D <- glm(breast ~ howfedfr + smokenow + ethnic*educat + ethnic*partner + smokebf + pregnancy, family = binomial(link = "logit"), data = data)

# adding variables: howfed age
model_ <- glm(breast ~ howfedfr + smokenow + ethnic*educat + ethnic*partner + smokebf + pregnancy +howfed, family = binomial(link = "logit"), data = data)
table <- rbind(table, c("Model D ", "+howfed", compare_models(model_D,model_)))

model_ <- glm(breast ~ howfedfr + smokenow + ethnic*educat + ethnic*partner + smokebf + pregnancy +age, family = binomial(link = "logit"), data = data)
table <- rbind(table, c("Model D ", "+age", compare_models(model_D,model_)))

# removing variables:
model_ <- glm(breast ~  + smokenow + ethnic*educat + ethnic*partner + smokebf + pregnancy, family = binomial(link = "logit"), data = data)
table <- rbind(table, c("Model D ", "- howfedfr", compare_models(model_D,model_)))

model_ <- glm(breast ~ howfedfr + ethnic*educat + ethnic*partner + smokebf + pregnancy, family = binomial(link = "logit"), data = data)
table <- rbind(table, c("Model D ", "-smokenow ", compare_models(model_D,model_)))

model_ <- glm(breast ~ howfedfr + smokenow + ethnic*partner + smokebf + pregnancy, family = binomial(link = "logit"), data = data)
table <- rbind(table, c("Model D ", "- ethnic*educat", compare_models(model_D,model_)))

model_ <- glm(breast ~ howfedfr + smokenow + ethnic*educat +smokebf + pregnancy, family = binomial(link = "logit"), data = data)
table <- rbind(table, c("Model D ", "- ethnic*partner", compare_models(model_D,model_)))

model_ <- glm(breast ~ howfedfr + smokenow + ethnic*educat + ethnic*partner + pregnancy, family = binomial(link = "logit"), data = data)
table <- rbind(table, c("Model D ", "- smokebf", compare_models(model_D,model_)))

#------------------------------------
library(tidyverse)
table <- tibble::rowid_to_column(table, "index")
table
```
Adding variables to model_D does not decrease deviance significantly.
Removing variables to model_D increases deviance significantly.

To summarise:
Model B could not be improved by adding or removing one variable at the time.
Model A could be improved by adding smokebf - we call it Model C
Model C: AIC got lower by adding pregnancy, p-value of LR-test being 12% (just a little over our set 10%). We call it model D.
Model D could not be improved.

We are left with 3 models:
Model_B, Model_C and Model_D

# Model Checking
HL test:
```{r}
# HL test for these models
logitgof(breast, fitted(model_B), g = 10, ord = FALSE)
logitgof(breast, fitted(model_C), g = 10, ord = FALSE)
logitgof(breast, fitted(model_D), g = 10, ord = FALSE)

logitgof(breast, fitted(model_B), g = 9, ord = FALSE)
logitgof(breast, fitted(model_C), g = 9, ord = FALSE)
logitgof(breast, fitted(model_D), g = 9, ord = FALSE)

logitgof(breast, fitted(model_B), g = 8, ord = FALSE)
logitgof(breast, fitted(model_C), g = 8, ord = FALSE)
logitgof(breast, fitted(model_D), g = 8, ord = FALSE)

logitgof(breast, fitted(model_B), g = 11, ord = FALSE)
logitgof(breast, fitted(model_C), g = 11, ord = FALSE)
logitgof(breast, fitted(model_D), g = 11, ord = FALSE)

logitgof(breast, fitted(model_B), g = 12, ord = FALSE)
logitgof(breast, fitted(model_C), g = 12, ord = FALSE)
logitgof(breast, fitted(model_D), g = 12, ord = FALSE)
```
All values big.

Our data is binary, so suitable diagnostic methods are:
- HL statistic
- overdispersion can not happen with binary data (ungrouped) (book, page 170)
- distribution of binary data is not approximated by normal, even if the model is correct (p.171)
- plots of the residuals against the linear predictor, or explanatory variables in the model, will be uninformative
- index plots and half-normal plots of the residuals can provide useful information about the adequacy of the linear part of the model.
 + Since residuals from ungrouped binary data are not normally distributed, simulated envelopes for the half-normal plot will provide an essential aid to their proper interpretation

Steps:
- fitted prob, stand dev residuals, leverage
 + An index plot of the standardised deviance residuals,
 + a plot of these residuals against the values of the linear predictor
 + a halfnormal plot of the residuals with a simulated envelope

```{r}
model <- model_B
par(mfrow=c(2,2))
plot(rstandard(model), ylab="Standardized deviance residuals", main = "Model B") #standardizer deviance residuals against index
# Binned deviance residuals against values of the linear predictor
data2 <- data
data2 <- mutate(data2, residuals=residuals(model), linpred=predict(model)) #add residuals and linear predictor
gdf <- group_by(data2, cut(linpred, breaks=unique(quantile(linpred, (1:12)/12))))
diagdf <- summarise(gdf, residuals=mean(residuals), linpred=mean(linpred))
plot(residuals ~ linpred, diagdf, xlab="Mean linear predictor", ylab="Mean deviance residual")
# leverage
halfnorm(hatvalues(model), ylab="Leverage")
library(hnp)
hnp(model, ylab="absolute deviance residuals", xlab="theoretical halfnormal quantiles")


model <- model_C
par(mfrow=c(2,2))
plot(rstandard(model), ylab="Standardized deviance residuals", main = "Model C") #standardizer deviance residuals against index
# Binned deviance residuals against values of the linear predictor
data2 <- data
data2 <- mutate(data2, residuals=residuals(model), linpred=predict(model)) #add residuals and linear predictor
gdf <- group_by(data2, cut(linpred, breaks=unique(quantile(linpred, (1:12)/12))))
diagdf <- summarise(gdf, residuals=mean(residuals), linpred=mean(linpred))
plot(residuals ~ linpred, diagdf, xlab="Mean linear predictor", ylab="Mean deviance residual")
# leverage
halfnorm(hatvalues(model), ylab="Leverage")
hnp(model, ylab="absolute deviance residuals", xlab="theoretical halfnormal quantiles")

model <- model_D
par(mfrow=c(2,2))
plot(rstandard(model), ylab="Standardized deviance residuals", , main = "Model D") #standardizer deviance residuals against index
# Binned deviance residuals against values of the linear predictor
data2 <- data
data2 <- mutate(data2, residuals=residuals(model), linpred=predict(model)) #add residuals and linear predictor
gdf <- group_by(data2, cut(linpred, breaks=unique(quantile(linpred, (1:10)/10))))
diagdf <- summarise(gdf, residuals=mean(residuals), linpred=mean(linpred))
plot(residuals ~ linpred, diagdf, xlab="Mean linear predictor", ylab="Mean deviance residual")
# leverage
halfnorm(hatvalues(model), ylab="Leverage")
hnp(model, ylab="absolute deviance residuals", xlab="theoretical halfnormal quantiles")
```
Interpretation:
1. First plot quite the same for all
  + no clear outliers stand out!
2. mean dev res, smaller for model D
3. Leverage: for B: max 0.5 (12), for C,D: max 0.7 (18)
 + outliers usually occur when observations have extreme values.
 + 18: education 38 outlier
 + 14: education 14 outlier
 + 12: the observation is the only one smokebf=Yes and education>17, whose response is Bottle.
4. halfnorm: best for model D. Other two have some residuals on the borders of the envelope. Nothing outside
  + large values of residuals are inside the envelope

```{r}
filter(data, hatvalues(model_B) > 0.3) # education 23 , bottle, outlier in boxplot
filter(data, hatvalues(model_C) > 0.3)
filter(data, hatvalues(model_D) > 0.3)

# to study why the line 12 has high leverage
filter(data, data$educat > 18 & data$smokebf=="Yes")
# the only one with response Bottle
```

```{r}
#fit models without outliers:
dataB <- data[-12,]
model_B <- glm(breast ~ howfedfr + smokenow + smokebf*educat + ethnic*partner, family = binomial(link = "logit"), data = dataB)
dataC <- data[-c(12,18),]
model_C<- glm(breast ~ howfedfr + smokenow + ethnic*educat + ethnic*partner + smokebf, family = binomial(link = "logit"), data = dataC)
model_D <- glm(breast ~ howfedfr + smokenow + ethnic*educat + ethnic*partner + smokebf + pregnancy, family = binomial(link = "logit"), data = dataC)
#--------------------------
# plot again residual plots and HL test
model <- model_B
par(mfrow=c(2,2))
plot(rstandard(model), ylab="Standardized deviance residuals", main = "Model B") #standardizer deviance residuals against index
# Binned deviance residuals against values of the linear predictor
data2 <- dataB
data2 <- mutate(data2, residuals=residuals(model), linpred=predict(model)) #add residuals and linear predictor
gdf <- group_by(data2, cut(linpred, breaks=unique(quantile(linpred, (1:15)/15))))
diagdf <- summarise(gdf, residuals=mean(residuals), linpred=mean(linpred))
plot(residuals ~ linpred, diagdf, xlab="Mean linear predictor", ylab="Mean deviance residual")
# leverage
halfnorm(hatvalues(model), ylab="Leverage")
#library(hnp)
hnp(model, ylab="absolute deviance residuals", xlab="theoretical halfnormal quantiles")


model <- model_C
par(mfrow=c(2,2))
plot(rstandard(model), ylab="Standardized deviance residuals", main = "Model C") #standardizer deviance residuals against index
# Binned deviance residuals against values of the linear predictor
data2 <- dataC
data2 <- mutate(data2, residuals=residuals(model), linpred=predict(model)) #add residuals and linear predictor
gdf <- group_by(data2, cut(linpred, breaks=unique(quantile(linpred, (1:10)/10))))
diagdf <- summarise(gdf, residuals=mean(residuals), linpred=mean(linpred))
plot(residuals ~ linpred, diagdf, xlab="Mean linear predictor", ylab="Mean deviance residual")
# leverage
halfnorm(hatvalues(model), ylab="Leverage")
hnp(model, ylab="absolute deviance residuals", xlab="theoretical halfnormal quantiles")

model <- model_D
par(mfrow=c(2,2))
plot(rstandard(model), ylab="Standardized deviance residuals", , main = "Model D") #standardizer deviance residuals against index
# Binned deviance residuals against values of the linear predictor
data2 <- dataC
data2 <- mutate(data2, residuals=residuals(model), linpred=predict(model)) #add residuals and linear predictor
gdf <- group_by(data2, cut(linpred, breaks=unique(quantile(linpred, (1:10)/10))))
diagdf <- summarise(gdf, residuals=mean(residuals), linpred=mean(linpred))
plot(residuals ~ linpred, diagdf, xlab="Mean linear predictor", ylab="Mean deviance residual")
# leverage
halfnorm(hatvalues(model), ylab="Leverage")
hnp(model, ylab="absolute deviance residuals", xlab="theoretical halfnormal quantiles")

logitgof(dataB$breast, fitted(model_B), g = 10, ord = FALSE)
logitgof(dataC$breast, fitted(model_C), g = 10, ord = FALSE)
logitgof(dataC$breast, fitted(model_D), g = 10, ord = FALSE)

```


############# CONTINUE HERE !!!!!!!!!!!!!!!!!!!!

 - how to interpret the plots above?
 - confidence intervals/prediction intervals?
 - interpretation of interaction terms
 
 
 
# Outliers

# Influential observations



# then choose a model and interpret the result

```{r}
summary(model_B)
cbind("OR" = exp(coef(model_B)), exp(confint(model_B)))
```

```{r}
summary(model_C)
cbind("OR" = exp(coef(model_C)), exp(confint(model_C)))

summary(model_D)
cbind("OR" = exp(coef(model_D)), exp(confint(model_D)))
```
Significant variables at 5% level are
howfedfr      
 + the odds of breast feeding for a woman whose friends breast feed, is exp(1.84) times larger than for those whose friends bottle feed.
 + confidence interval is...
smokenow        
 + odds of breast
ethnic        
- odds of breast feeding for white woman are:
 + exp(-10) if partner=partner and
 + 
partner       
ethnic:educat   

Decrease in deviance is not big. LR test suggest we stick to the simpler model with only 3 variables.


```{r}


```




**Interactions**: In some applications, a small number of interactions and other higher-order terms, such as powers of certain variates, may need to be considered for inclusion in a model. Such terms would be added to the model identified in Step 3 above, after ensuring that any terms necessitated by the hierarchic principle have already been included.