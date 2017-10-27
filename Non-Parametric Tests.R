
setwd("C:/Users/Dnyanada/Desktop/PH R")

# Data management
# 1.Read in the provided TXT file, MIDTERM.TXT into R.
anorexia <- read.table("Non parametric tests data.txt", header = TRUE, sep = "")
attach(anorexia)

# 2.Examine the structure of the data.
str(anorexia)
summary(anorexia)
head(anorexia)

# 3.Make the variable TREAT a factor.
anorexia$Treat <- as.factor(anorexia$Treat)
is.factor(anorexia$Treat)

# 4.Create and aaisgn value labels for each level of Treat.
# 1 - Control, 2 - CBT, 3 - FT
treatment.label <- c('Control','CBT','FT')
anorexia$Treat<-as.factor(treatment.label[anorexia$Treat])

# 5.Create a variable of the difference, DIFF. Defined as POSTWT - PREWT.
DIFF <- anorexia$Postwt - anorexia$Prewt
DIFF

# 6.Create a variable of the percent difference, PCNT.DIFF. 
PCNT.DIFF <- (DIFF/Prewt) * 100
PCNT.DIFF

# Data Analysis
# 1. Create a boxplot of the percent difference for each treatment group.
boxplot(PCNT.DIFF~anorexia$Treat, names = c("Cognitive Behavioral","Control","Family"), ylab = "Difference (%)", xlab = "Treatment",cex.lab = 1.2, col = c("grey50","grey100","grey80"))
title(main = "Weight change after treatment for each group", cex.main = 1.4)

# 2. Write a function that returns median, minimum, maximum, p-value of the non-parametric test.

#co <- DIFF[27:55]
#cbt <- DIFF[1:26]
#ft <- DIFF[56:72]
#f <- factor(rep(1:3, c(29, 26, 17)), labels = c("Control", "CBT", "FT"))
x <- PCNT.DIFF
f <- anorexia$Treat
            
median.test <- function(x, f, round = 3){
  
  statistics <- data.frame(cbind(n = table(f), median = round(tapply(x, f, median), round),min = round(tapply(x, f, min), round), max = round(tapply(x, f, max), round)))
  treatment.level <- factor(rownames(statistics), levels = c("Control", "CBT", "FT")) 
  statistics <- statistics[order(treatment.level),]
  
  if(length(unique(f)) < 3){
    wilcox <- wilcox.test(x,f)
    test <- ("Wilcoxon-rank sum test")
    pvalue <- wilcox$p.value
  }
  else{
    kruskal <- kruskal.test(x,f)
    test <- ("Kruskal-Wallis test")
    pvalue <- kruskal$p.value
  }
  return(list(statistics = statistics, pvalue = round(pvalue, round), test = test))
}

median.test(x,f)

detach(anorexia)