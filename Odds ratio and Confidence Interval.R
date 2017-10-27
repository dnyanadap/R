
#Read the data from 'heartbeats.txt' into a data frame named heartbeats. 
#Get an overview of the structure of the data frame.

heartbeats <- read.table("heartbeats.txt", header = TRUE, sep = "")
heartbeats
attach(heartbeats)
head(heartbeats)
str(heartbeats)

#The mean of the increase in weight of all newborns
mean(heartbeats$wghtincr)

#The mean of the weight increase of the control group
control_group <- subset(heartbeats, treatment == 0)
mean(control_group$wghtincr)

#The mean of the weight increase in the heartbeat group
heartbeat_group <- subset(heartbeats, treatment == 1)
mean(heartbeat_group$wghtincr)

#The mean of the weight increase in each weight class of the control group
#tapply(heartbeats[heartbeats$treatment == 1,]$wghtincr, as.factor(heartbeats[heartbeats$treatment == 1,]$wghtcls), mean) 
mean(control_group$wghtincr[which(control_group$wghtcls == 1)]) #Mean of wghtcls 1
mean(control_group$wghtincr[which(control_group$wghtcls == 2)]) #Mean of wghtcls 2
mean(control_group$wghtincr[which(control_group$wghtcls == 3)]) #Mean of wghtcls 3

#The mean of the weight increase in each weight class of the heartbeat group
mean(heartbeat_group$wghtincr[which(heartbeat_group$wghtcls == 1)]) #Mean of wghtcls 1
mean(heartbeat_group$wghtincr[which(heartbeat_group$wghtcls == 2)]) #Mean of wghtcls 2
mean(heartbeat_group$wghtincr[which(heartbeat_group$wghtcls == 3)]) #Mean of wghtcls 3

#The standard deviation of the weight increase of all newborns.
sd(heartbeats$wghtincr)

#The means obtained for the heartbeat group are more then those obtained for the control group.

detach(heartbeats)


#Exercise 14
#Write a function which.NA() which returns the vector of indices at which the function argument has NAs.
which.NA <- function(x){ans <- which(is.na(x) == TRUE)
return (ans)}
which.NA
which.NA(c(45,6,2,3,NA,67,4,NA,NA))

#Write a function rm.NA() which returns its argument without NAs
rm.NA <- function(y){
na <- which.NA(y)     #Calling function which.NA to determine NA values
val <- y[-na] 
return (val)}
rm.NA(c(45,6,2,3,NA,67,4,NA,NA))


#Exercise 15 
#Woolf's Confidence Interval for the OR

m <- matrix(c(52,166,63,219), nrow = 2)
colnames(m) <- c("Yes","No")
rownames(m) <- c("Male","Female")
m

ci.OR <- function(m, conf.level) {
  OR = (m[1,1] * m[2,2]) / (m[1,2] * m[2,1]) #Calculating the odds ratio
  nat.log <- log(OR)                         #Natural log of the odds ratio
  
  #Deciding the critical value based on the confidence level
  #zcrit <- qnorm(1-(1-conf.level)/2)
  if(conf.level == 0.9){
    crit.val <- 1.645
  }
  else if(conf.level == 0.95){
    crit.val <- 1.96
  }
  else if(conf.level == 0.99){
    crit.val <- 2.58
  }
  
  #The confidence coefficient is from standard normal distribution.
  #For 95% confidence level, coefficient = 1.96
  
  SE.nat.log <- sqrt((1/m[1,1]) + (1/m[1,2]) + (1/m[2,1]) + (1/m[2,2]) )  #Calculating the standard error
  
  #Calculating the upper and lower limits.
  upper.log <- nat.log + crit.val * SE.nat.log
  upper <- exp(upper.log)
  lower.log <- nat.log - crit.val * SE.nat.log
  lower <- exp(lower.log)
  climits <- c(lower,upper)
  
  return(list(OR = OR, climits = climits, conf.level = conf.level))
}

ci.OR(m,conf.level = 0.95)
#Odds ratio: ratio of the odds of an event occurring in one group to the odds of it occurring in another group
#Based on the value of the odds ratio obtained, there is association between the two variables.
