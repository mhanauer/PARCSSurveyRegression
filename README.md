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
mccsc1 = mccsc[c("Q1_1", "Q1_2", "Q1_3", "Q1_4", "Q1_5", "Q1_6", "Q36", "Q38",	"Q15_2_1",	"Q15_3_1",	"Q15_4_1",	"Q15_5_1",	"Q15_6_1",	"Q15_7_1",	"Q15_8_1",	"Q15_9_1",	"Q15_10_1",	"Q15_11_1",	"Q15_12_1",	"Q15_13_1", "Q44")]
mccsc2 = mccsc1[-c(1:2), ]

rbbcsc1 = rbbcsc[c("Q1_1", "Q1_2", "Q1_3", "Q1_4", "Q1_5", "Q1_6", "Q10", "Q12", "Q14_2_1", "Q14_3_1", "Q14_4_1", "Q14_5_1", "Q14_6_1", "Q14_7_1", "Q14_8_1", "Q14_9_1", "Q14_10_1", "Q14_11_1", "Q14_12_1", "Q14_13_1", "Q15")]

rbbcsc2 = rbbcsc1[-c(1:2), ]

names(rbbcsc2) = c("Q1_1", "Q1_2", "Q1_3", "Q1_4", "Q1_5", "Q1_6", "Q36", "Q38",	"Q15_2_1",	"Q15_3_1",	"Q15_4_1",	"Q15_5_1",	"Q15_6_1",	"Q15_7_1",	"Q15_8_1",	"Q15_9_1",	"Q15_10_1",	"Q15_11_1",	"Q15_12_1",	"Q15_13_1", "Q44")


both = rbind(mccsc2, rbbcsc2)

write.csv(both, "both.csv")
both1 = read.csv("both.csv", header = TRUE, , na.strings = c(""))
both2 = na.omit(both1); head(both2)
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
Q38Factor = as.data.frame(Q38Factor); head(Q38Factor)

# Creating two variables one for primary versus everything else and one for secondary versus everything else
Q44FactorPrim = apply(Q44Factor, 2, function(x){ifelse(x == 5, 1, 0)})
Q44FactorPrim = as.data.frame(Q44FactorPrim); head(Q44FactorPrim)

Q44FactorSecon = apply(Q44Factor, 2, function(x){ifelse(x == 14, 1, 0)})
Q44FactorSecon = as.data.frame(Q44FactorSecon); head(Q44FactorSecon)


```
Now we need to create an average variable for the number of years worked with 1st through 12th grade.




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
```
There is an extra "x" variable, so we need to get rid of that variable
```{r}
both3 = both2[c("Q1_1", "Q1_2", "Q1_3", "Q1_4", "Q1_5", "Q1_6")]
both3 = as.data.frame(both3)
```
Maybe take the average of question 15.

