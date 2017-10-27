
# Does the weight of those with high blood pressure differ with those without high blood pressure.
# OUtcome variable = WEIGHT
# Predictors = AGE, PA, HEIGHT, ETHNICITY, GENDER, and SMOKE

setwd("C:/Users/Dnyanada/Desktop/PH R")
source("C:/Users/Dnyanada/Desktop/PH R/Functions1.R")

# -------------------------------------------------------------------------------------------------
# Data management tasks
# 1. Read the data into R.
survey_data <- read.csv("survey.csv", header = TRUE, sep = ",")
str(survey_data)

# 2. Designate GENDER,HBP,SMOKE and ETHNICITY as factors.
survey_data$hbp <- as.factor(survey_data$hbp)
is.factor(survey_data$hbp)
survey_data$gender <- as.factor(survey_data$gender)
is.factor(survey_data$gender)
survey_data$smoke <- as.factor(survey_data$smoke)
is.factor(survey_data$smoke)
survey_data$ethnicity <- as.factor(survey_data$ethnicity)
is.factor(survey_data$ethnicity)

# 3.Create value labels for these factors. 
gender.label <- c('Male','Female')
survey_data$gender <- as.factor(gender.label[survey_data$gender])
hbp.label <- c('Yes','No')
survey_data$hbp <- as.factor(hbp.label[survey_data$hbp])
smoke.label <- c('Yes','No')
survey_data$smoke <- as.factor(smoke.label[survey_data$smoke])
ethnicity.label <- c('Other','Other','Other','Other','Latino','White')
survey_data$ethnicity <- as.factor(ethnicity.label[survey_data$ethnicity])

attach(survey_data)

# ---------------------------------------------------------------------------------------------
# Data analytic tasks
# Obtain descriptive statistics suitable for supporting the regression analysis.
# Is weight dependent on hbp

#x <- wt
#f <- hbp
#stats <- data.frame(cbind(n = table(f), mean = round(tapply(x, f, mean), round),min = round(tapply(x, f, min), round), max = round(tapply(x, f, max), round)))

means.grp(wt,smoke);means.grp(wt,hbp);means.grp(wt,ethnicity);means.grp(wt,gender);means.grp(wt,pa)
descript(wt); descript(hbp)

# a.Differences evident in the WEIGHT in the sample, by HBP.
boxplot(wt~hbp, col = 'lightgreen', xlab = "High blood pressure", ylab = "Weight (pounds)")
t.test(wt~hbp)

# b. The nature of bivariate relationship between weight and the control predictors.
#linear regression
# c. The bivariate relationship between all predictors.
round = 4
bivar_all <- round(cor(cbind(hbp,pa,gender,smoke,ht,age,wt,ethnicity), use = 'pairwise.complete.obs'),round)
bivar_all
cor.test(ht,wt)$p.value # p < 0.5, reject the null hypothesis
cor.test(wt,age)$p.value
cor.test(ht,age)$p.value

# coeff closer to 0 = not related

# --------------------------------------------------------------------------------------------------------------
# 2.With the predictors significantly related to WEIGHT, fit a set of multiple regression models. 
# In addition, program R to conduct General Linear Hypothesis (GLH) tests.

# Creating reference groups.
survey_data$hbp <- factor(hbp,levels=c('No','Yes'))

# Unadjusted associations that compliment the correlations */
#  The effect of hbp (2 levels);
u.model1 <- lm(wt~hbp, data = survey_data)
#  The effect of height;
u.model2 <- lm(wt~ht, data = survey_data)
#  The effect of age;
u.model3 <- lm(wt~age, data = survey_data)

summary(u.model1)
summary(u.model2)
summary(u.model3)

# Fitting one possible sensible set of nested regression models
a.model1 <- lm(wt~hbp, data = survey_data)
a.model2 <- lm(wt~hbp+ht, data = survey_data)
a.model3 <- lm(wt~hbp*ht, data = survey_data)
a.model4 <- lm(wt~hbp*ht+age, data = survey_data)
a.model5 <- lm(wt~hbp*ht*age, data = survey_data)
a.model6 <- lm(wt~hbp*ht+age*ethnicity, data = survey_data)

summary(a.model1)
summary(a.model2)
summary(a.model3)
summary(a.model4)
summary(a.model5)
summary(a.model6)

#Selecting model 2
coef(a.model2)

# Testing joint hypotheses
anova(a.model1, test = 'F')
anova(a.model2, test = 'F')
anova(a.model3, test = 'F')
anova(a.model4, test = 'F')
anova(a.model5, test = 'F')
anova(a.model6, test = 'F')

#install.packages("multcomp")
#library("multcomp")

# ----------------------------------------------------------------------------------------------------
# 3.Effects of hbp and other predictors in the model 

pred.wt <- 157.9141 - 1.5240*(as.numeric(hbp)-1) + 4.9171*ht
# plot only those points where hbp is no

plot(ht[hbp == 'No'], pred.wt[hbp == 'No'], ylab='Predicted weight', xlab='Height (Inches)',col='red',type='l')
lines(ht[hbp == 'Yes'],pred.wt[hbp == 'Yes'],col='blue',type="l")
legend('topleft',inset=0.02,title='HBP',legend=c('Yes','No'),col=c('blue','red'),lty=1)
title(main = list("Figure 2. Fitted values of weight versus the height 
                (in inches), by high blood pressure status.",font=3))

detach(survey_data)