
# Data = survey.csv
# Outcome variable = Heart Disease (HD)
# Predictors = DRINK, PA, HOURS, HEIGHT, WEIGHT, SMOKE, AGE, ETHNICITY, and GENDER.
# Are obese hypertensive subjects more likely to have heart disease?

setwd("C:/Users/Dnyanada/Desktop/PH R")
source("C:/Users/Dnyanada/Desktop/PH R/Functions1.R")

#--------------------------------------------------------------------
# Data Analytic tasks
# 1. Read the data into R.
data.survey <- read.csv("survey.csv", header = TRUE, sep = ",")
str(data.survey)
summary(data.survey)

# 1.a. Designate GENDER,DRINK,SMOKE and ETHNICITY as factors.
data.survey$DRINK <- as.factor(data.survey$DRINK)
is.factor(data.survey$DRINK)
data.survey$GENDER <- as.factor(data.survey$GENDER)
is.factor(data.survey$GENDER)
data.survey$SMOKE <- as.factor(data.survey$SMOKE)
is.factor(data.survey$SMOKE)
data.survey$ETHNICITY <- as.factor(data.survey$ETHNICITY)
is.factor(data.survey$ETHNICITY)
data.survey$HBP <- as.factor(data.survey$HBP)

# 1.b. Create value labels for these variables.
DRINK.label <- c('Yes','No')
data.survey$DRINK <- as.factor(DRINK.label[data.survey$DRINK])
GENDER.label <- c('Male','Female')
data.survey$GENDER <- as.factor(GENDER.label[data.survey$GENDER])
SMOKE.label <- c('Yes','No')
data.survey$SMOKE <- as.factor(SMOKE.label[data.survey$SMOKE])
HBP.label <- c('Yes','No')
data.survey$HBP <- as.factor(HBP.label[data.survey$HBP])

# 1.c. For ETHNICITY, combine categories and create three levels 
# with corresponding value labels (that's OTHER, LATINO, and WHITE).
ETHNICITY.label <- c('Other','Other','Other','Other','Latino','White')
data.survey$ETHNICITY <- as.factor(ETHNICITY.label[data.survey$ETHNICITY])

# 1.d. Create a BMI variable using HEIGHT and WEIGHT. Leave it as continuous.
BMI <- ((data.survey$WT * 703)/(data.survey$HT * data.survey$HT))

attach(data.survey)

#-------------------------------------------------------------------------------
# 2.a. Descriptive statistics
# means.grp(BMI,GENDER); means.grp(BMI,SMOKE); means.grp(BMI,ETHNICITY); means.grp(BMI,DRINK)
descript(WT); descript(AGE); descript(HOURS); descript(HT); descript(PA)
means.grp(WT,HD); means.grp(AGE,HD); means.grp(HOURS,HD); means.grp(HT,HD)
freq(ETHNICITY); freq(GENDER); freq(SMOKE); freq(DRINK); 

library("graphics")
sunflowerplot(x=BMI,y=HD,xlim=c(15,70),xlab="BMI",yaxt="n",ylab="Heart Disease",seg.col="blue")
axis(1,at=seq(15,70,5))
axis(2,at=c(1,2),labels=c("HD"," No HD"),las=1)

# 2.b. Unadjusted associations of all predictors with HD.

# Calculating the row percentages for levels of a factor. 
freq(HD[ETHNICITY=="White"])[2,]
freq(HD[ETHNICITY=="Latino"])[2,]
freq(HD[ETHNICITY=="Other"])#[2,]
freq(HD[GENDER=="Female"])[2,]
freq(HD[GENDER=="Male"])[2,]
freq(HD[SMOKE=="Yes"])[2,]
freq(HD[SMOKE=="No"])[2,]
freq(HD[DRINK=="Yes"])[2,]
freq(HD[DRINK=="No"])[2,]

# Calculating the odds ratio and the 95% confidence interval.
un.model1<-glm(relevel(as.factor(HD),"1")~DRINK,family="binomial"(link="logit"),data=data.survey)
#summary(un.model1)
exp(cbind(OR = coef(un.model1), confint(un.model1)))

un.model2<-glm(relevel(as.factor(HD),"1")~PA,family="binomial"(link="logit"),data=data.survey)
exp(cbind(OR = coef(un.model2), confint(un.model2)))

un.model3<-glm(relevel(as.factor(HD),"1")~HOURS,family="binomial"(link="logit"),data=data.survey)
exp(cbind(OR = coef(un.model3), confint(un.model3)))

un.model4<-glm(relevel(as.factor(HD),"1")~HT,family="binomial"(link="logit"),data=data.survey)
exp(cbind(OR = coef(un.model4), confint(un.model4)))

un.model5<-glm(relevel(as.factor(HD),"1")~WT,family="binomial"(link="logit"),data=data.survey)
exp(cbind(OR = coef(un.model5), confint(un.model5)))

un.model6<-glm(relevel(as.factor(HD),"1")~SMOKE,family="binomial"(link="logit"),data=data.survey)
exp(cbind(OR = coef(un.model6), confint(un.model6)))

un.model7<-glm(relevel(as.factor(HD),"1")~AGE,family="binomial"(link="logit"),data=data.survey)
exp(cbind(OR = coef(un.model7), confint(un.model7)))

un.model8<-glm(relevel(as.factor(HD),"1")~ETHNICITY,family="binomial"(link="logit"),data=data.survey)
exp(cbind(OR = coef(un.model8), confint(un.model8)))

un.model9<-glm(relevel(as.factor(HD),"1")~GENDER,family="binomial"(link="logit"),data=data.survey)
exp(cbind(OR = coef(un.model9), confint(un.model9)))

#-------------------------------------------------------------------------------------------
# 3. With the predictors significantly related to HD, fit a set of multiple logistic regression models.

ad.model1<-glm(relevel(as.factor(HD),"1")~BMI,family="binomial"(link="logit"),data=data.survey)
summary(ad.model1);round(exp(cbind(OR=coef(ad.model1),confint(ad.model1))),3)

ad.model2<-glm(relevel(as.factor(HD),"1")~BMI+relevel(HBP,"No"),family="binomial"(link="logit"),data=data.survey)
summary(ad.model2);round(exp(cbind(OR=coef(ad.model2),confint(ad.model2))),3)

ad.model3<-glm(relevel(as.factor(HD),"1")~BMI+relevel(HBP,"No")+WT+AGE+HOURS+HT+PA,family="binomial"(link="logit"),data=data.survey)
summary(ad.model3);round(exp(cbind(OR=coef(ad.model3),confint(ad.model3))),3)

ad.model4<-glm(relevel(as.factor(HD),"1")~BMI*relevel(HBP,"No")*AGE*HOURS,family="binomial"(link="logit"),data=data.survey)
summary(ad.model4);round(exp(cbind(OR=coef(ad.model4),confint(ad.model4))),3)

ad.model5<-glm(relevel(as.factor(HD),"1")~BMI+relevel(HBP,"No")+AGE+HOURS+relevel(ETHNICITY,"White")+relevel(DRINK,"Yes")+relevel(SMOKE,"Yes")+relevel(GENDER,"Male"),family="binomial"(link="logit"),data=data.survey)
summary(ad.model5);round(exp(cbind(OR=coef(ad.model5),confint(ad.model5))),3)

ad.model6<-glm(relevel(as.factor(HD),"1")~BMI+relevel(HBP,"No")+AGE+HOURS,family="binomial"(link="logit"),data=data.survey)
summary(ad.model6);round(exp(cbind(OR=coef(ad.model6),confint(ad.model6))),3)

# Finding the odds ratio and 95% confidence intervals for the final model.
exp(cbind(OR = coef(ad.model6), confint(ad.model6)))
#------------------------------------------------------------------------------------
# 4. Calculating and interpreting the odds ratio, with a 95% confidence interval.

install.packages("multcomp")
library("multcomp")

# a. Are obese hypertensive subjects more likely to have heart disease than normal weight
# hypertensive subjects, controlling for all other variables in the model?
# (31,1,48,37)) vs (19,1,48,37)
coeff.1 <- matrix(c(0,12,0,0,0),1)
t1 <- glht(ad.model6, linfct=coeff.1)
summary(t1)
exp(confint(t1)$confint)[1,]

# b. Are obese hypertensive subjects more likely to have heart disease than overweight hypertensive
# subjects, controlling for all other variables in the model?
# (31,1,48,37)) vs (27,1,48,37)
coeff.2 <- matrix(c(0,4,0,0,0),1)
t2 <- glht(ad.model6, linfct=coeff.2)
summary(t2)
exp(confint(t2)$confint)[1,]

# c. Are obese hypertensive subjects more likely to have heart disease than overweight nonhypertensive
# subjects, controlling for all other variables in the model?
# (31,1,48,37)) vs (27,0,48,37)
coeff.3 <- matrix(c(0,4,1,0,0),1)
t3 <- glht(ad.model6, linfct=coeff.3)
summary(t3)
exp(confint(t3)$confint)[1,]

# d. Are normal weight hypertensive subjects more likely to have heart disease than overweight nonhypertensive
# subjects, controlling for all other variables in the model?
# (19,1,48,37)) vs (27,0,48,37)
coeff.4 <- matrix(c(0,-8,1,0,0),1)
t4 <- glht(ad.model6, linfct=coeff.4)
summary(t4)
exp(confint(t4)$confint)[1,]

#------------------------------------------------------------------------------------
install.packages("pROC")
library(pROC)

# Plot the ROC Curve
fitted <- predict(ad.model6,type="response")
roc.curve <- roc(ad.model6$y~ad.model6$fitted.values)
plot(roc.curve)
auc(roc.curve)

# Plot the Sensitivities and Specificities for classification
plot(x=roc.curve$thresholds,y=roc.curve$sensitivities,type="l",ylim=c(0,1),xlim=c(0,1),ylab="Sensitivity",xlab="Probability Level")
points(x=roc.curve$thresholds,y=roc.curve$specificities,type="l",col="red")
mtext(side=4,text="Specificity",srt=45,col="red")

detach(data.survey)

