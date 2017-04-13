---
title: "PARCSSurveyRegression"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Grab data from both districts and put them into seperate data frames
```{r}
setwd("~/Desktop/QualData")
mccsc = read.csv("MCCSCStaffSurvey.csv", header = TRUE)
rbbcsc = read.csv("RBBCSCStaffSurvey.csv", header = TRUE)
```
Next we grab the variables of interest which are the SEL variables from each of them
Only have 1st through 12th for years worked, because we forgot to add K for MCCSC
```{r}
mccsc1 = mccsc[c("Q1_1", "Q1_2", "Q1_3", "Q1_4", "Q1_5", "Q1_6", "Q36", "Q38", "Q44",	"Q15_2_1",	"Q15_3_1",	"Q15_4_1",	"Q15_5_1",	"Q15_6_1",	"Q15_7_1",	"Q15_8_1",	"Q15_9_1",	"Q15_10_1",	"Q15_11_1",	"Q15_12_1",	"Q15_13_1")]
mccsc2 = mccsc1[-c(1:2), ]

rbbcsc1 = rbbcsc[c("Q1_1", "Q1_2", "Q1_3", "Q1_4", "Q1_5", "Q1_6", "Q10", "Q12", "Q15", "Q14_2_1", "Q14_3_1", "Q14_4_1", "Q14_5_1", "Q14_6_1", "Q14_7_1", "Q14_8_1", "Q14_9_1", "Q14_10_1", "Q14_11_1", "Q14_12_1", "Q14_13_1")]

rbbcsc2 = rbbcsc1[-c(1:2), ]

names(rbbcsc2) = c("Q1_1", "Q1_2", "Q1_3", "Q1_4", "Q1_5", "Q1_6", "Q36", "Q38", "Q44",	"Q15_2_1",	"Q15_3_1",	"Q15_4_1",	"Q15_5_1",	"Q15_6_1",	"Q15_7_1",	"Q15_8_1",	"Q15_9_1",	"Q15_10_1",	"Q15_11_1",	"Q15_12_1",	"Q15_13_1")


both = rbind(mccsc2, rbbcsc2)

```
Need to get the blanks for the years variable changed from blanks to zero's.  And then we need to bring the variables both into the same dataset with both
```{r}
both1 = both[c("Q15_2_1",	"Q15_3_1",	"Q15_4_1",	"Q15_5_1",	"Q15_6_1",	"Q15_7_1",	"Q15_8_1",	"Q15_9_1",	"Q15_10_1",	"Q15_11_1",	"Q15_12_1",	"Q15_13_1")]
write.csv(both1, "both1.csv")
both1 = read.csv("both1.csv", header = TRUE, , na.strings = c(""))
both1[is.na(both1)] = 0; both1
# Create a seperate dataset with the other non years variables so we don't duplicate them when combining them
both2 = both[c("Q1_1", "Q1_2", "Q1_3", "Q1_4", "Q1_5", "Q1_6", "Q36", "Q38", "Q44")]
# Want both two first to perserve the order from the original data
both = cbind(both2, both1)

```


Then we need to get rid of the missing values
```{r}
write.csv(both, "both.csv")
both1 = read.csv("both.csv", header = TRUE, , na.strings = c(""))
both2 = na.omit(both1)
```




Try to change the variable into factor without the apply function for ones that have no inherent order.

Need to create a variable for master's degree or higher.  Therefore, we need to identitfy what numerical factors will be associated with a master's degree or higher when we convert the data into a numerical structure.

Degree: Doctoral = 4; Professional = 9; Master's degree and professional  = 7; Master's degree = 6

Gender: Female = 1; Male = 2

Type of job: 5 = Primary; 14 = Secondary 
```{r}
Q38Factor = as.numeric(factor(both2$Q38)); head(Q38Factor)
Q38Factor = as.data.frame(Q38Factor)

Q36Factor = as.numeric(factor(both2$Q36)); head(Q36Factor)
Q36Factor = as.data.frame(Q36Factor)

Q44Factor = as.numeric(factor(both2$Q44)); head(Q44Factor)
Q44Factor = as.data.frame(Q44Factor)


```
Now we need to transform the numerical values that correspond to a master's degree or higher for 1 and all else zero. Probably just do a bunch of apply ifelse functions.  The last function we change to zero, because want everything below a master's degree to be zero.
```{r}
Q38Factor = apply(Q38Factor, 2, function(x){ifelse(x == 4, 1, x)})
Q38Factor = as.data.frame(Q38Factor)
Q38Factor = apply(Q38Factor, 2, function(x){ifelse(x == 9, 1, x)})
Q38Factor = as.data.frame(Q38Factor)
Q38Factor = apply(Q38Factor, 2, function(x){ifelse(x == 7, 1, x)})
Q38Factor = as.data.frame(Q38Factor)
Q38Factor = apply(Q38Factor, 2, function(x){ifelse(x == 6, 1, x)})
Q38Factor = as.data.frame(Q38Factor); head(Q38Factor)
Q38Factor = apply(Q38Factor, 2, function(x){ifelse(x == 6, 1, x)})
Q38Factor = as.data.frame(Q38Factor); head(Q38Factor)
Q38Factor = apply(Q38Factor, 2, function(x){ifelse(x == 1, 1, 0)})
Q38Factor = as.data.frame(Q38Factor); head(Q38Factor)

# Male is one and combining female and other gender identities
Q36Factor = apply(Q36Factor, 2, function(x){ifelse(x == 2, 1, 0)})
# Final version of this variable
Q36Factor = as.data.frame(Q36Factor); head(Q36Factor)

# This is for the invariance study needs binary grouping variable
Q36Invar = apply(Q36Factor, 2, function(x){ifelse(x == 1, "Male", "Female")})
Q36Invar = as.data.frame(Q36Invar); head(Q36Invar)

# Creating one variable one for secondary versus everything else 

Q44FactorSecon = apply(Q44Factor, 2, function(x){ifelse(x == 14, 1, 0)})
Q44FactorSecon = as.data.frame(Q44FactorSecon); head(Q44FactorSecon)



```

Now we need to create an average variable for the number of years worked with 1st through 12th grade.
There were some coding errors with with nine reponses across three respondents.  One of those was a error, but coded as zero for convience, the other was less than one and coded as zero, and the other was coded as 1 because is ~1.
```{r}
Q15Years = both2[c("Q15_2_1",	"Q15_3_1",	"Q15_4_1",	"Q15_5_1",	"Q15_6_1",	"Q15_7_1",	"Q15_8_1",	"Q15_9_1",	"Q15_10_1",	"Q15_11_1",	"Q15_12_1",	"Q15_13_1")]; head(Q15Years)

Q15Years = apply(Q15Years, 2, function(x){ifelse(x == "3-Feb", 0, x)})
Q15Years = as.data.frame(Q15Years)

Q15Years = apply(Q15Years, 2, function(x){ifelse(x == ">1", 0, x)})
Q15Years = as.data.frame(Q15Years)

Q15Years = apply(Q15Years, 2, function(x){ifelse(x == "~1", 1, x)})
Q15Years = as.data.frame(Q15Years)

Q15Years = apply(Q15Years, 2, function(x){ifelse(x == "30+", 30, x)})
Q15Years = as.data.frame(Q15Years)

Q15Years = apply(Q15Years, 2, function(x){ifelse(x == "One", 1, x)})
Q15Years = as.data.frame(Q15Years)

write.csv(Q15Years, "Q15Years.csv")
Q15Years = read.csv("Q15Years.csv", header = TRUE)
Q15Years = Q15Years[c("Q15_2_1",	"Q15_3_1",	"Q15_4_1",	"Q15_5_1",	"Q15_6_1",	"Q15_7_1",	"Q15_8_1",	"Q15_9_1",	"Q15_10_1",	"Q15_11_1",	"Q15_12_1",	"Q15_13_1")]

Q15Years2 = apply(Q15Years, 1, mean); head(Q15Years2)

```


Now we need to transform all of the categorical variables into numerical ones
```{r}
both2 = apply(both2, 2, function(x){ifelse(x == "Strongly agree", 7, x)})
both2 = as.data.frame(both2); head(both2)
both2 = apply(both2, 2, function(x){ifelse(x == "Agree", 6, x)})
both2 = as.data.frame(both2)
both2 = apply(both2, 2, function(x){ifelse(x == "Somewhat agree", 5, x)})
both2 = as.data.frame(both2)
both2 = apply(both2, 2, function(x){ifelse(x == "Neither agree nor disagree", 4, x)})
both2 = as.data.frame(both2)
both2 = apply(both2, 2, function(x){ifelse(x == "Somewhat disagree", 3, x)})
both2 = as.data.frame(both2)
both2 = apply(both2, 2, function(x){ifelse(x == "Disagree", 2, x)})
both2 = as.data.frame(both2)
both2 = apply(both2, 2, function(x){ifelse(x == "Strongly disagree", 1, x)})
both2 = as.data.frame(both2)
setwd("~/Desktop/QualData")
write.csv(both2, "both2.csv")
```


CFA: Need to get the factor scores for using in regression. 
```{r}
library(lavaan)
both2 = read.csv("both2.csv", header = TRUE)
cfaSEL = 'Satisfaction = ~ Q1_1 + Q1_2 + Q1_3 + Q1_4'
cfaSEL2 = cfa(cfaSEL, estimator = "MLR", data = both2)
SELSatis = lavPredict(cfaSEL2, type = "lv")
```
Now we need to combine the dataset with the altered variables into a new dataset ready for regression.  There is something werid with the names, but the data is the same.
```{r}
selRegress = cbind(Q36Factor, Q44FactorSecon, Q15Years2, SELSatis)
setwd("~/Desktop/DBDA2Eprograms")
write.csv(selRegress, "selRegress.csv")
```
Now we use Kruscke's High Level Script for robust multivariate regression to conduct the analysis
```{r}
# Example for Jags-Ymet-XmetMulti-Mrobust.R 
#------------------------------------------------------------------------------- 
# Optional generic preliminaries:
graphics.off() # This closes all of R's graphics windows.
rm(list=ls())  # Careful! This clears all of R's memory!
# UNCOMMENT ONE OF THE FOLLOWING SECTIONS (In RStudio, select and ctrl-shift-C)
#.............................................................................
setwd("~/Desktop/QualData")
myData = read.csv( file="selRegress.csv" )
yName = "Satisfaction" ; xName = c("Q36Factor", "Q44Factor", "Q15Years2")
fileNameRoot = "SELSatis-" 
numSavedSteps=15000 ; thinSteps=5
#.............................................................................

graphFileType = "eps" 
#------------------------------------------------------------------------------- 
# Load the relevant model into R's working memory:
setwd("~/Desktop/DBDA2Eprograms")
source("Jags-Ymet-XmetMulti-Mrobust.R")
#------------------------------------------------------------------------------- 
# Generate the MCMC chain:
#startTime = proc.time()
mcmcCoda = genMCMC( data=myData , xName=xName , yName=yName , 
                    numSavedSteps=numSavedSteps , thinSteps=thinSteps , 
                    saveName=fileNameRoot )
#stopTime = proc.time()
#duration = stopTime - startTime
#show(duration)
#------------------------------------------------------------------------------- 
# Display diagnostics of chain, for specified parameters:
parameterNames = varnames(mcmcCoda) # get all parameter names
for ( parName in parameterNames ) {
  diagMCMC( codaObject=mcmcCoda , parName=parName , 
            saveName=fileNameRoot , saveType=graphFileType )
}
#------------------------------------------------------------------------------- 
# Get summary statistics of chain:
summaryInfo = smryMCMC( mcmcCoda , 
                        saveName=fileNameRoot )
show(summaryInfo)
# Display posterior information:
plotMCMC( mcmcCoda , data=myData , xName=xName , yName=yName , 
          pairsPlot=TRUE , showCurve=FALSE ,
          saveName=fileNameRoot , saveType=graphFileType )
#------------------------------------------------------------------------------- 

```

