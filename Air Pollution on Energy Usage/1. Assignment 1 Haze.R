#Preparation
rm(list = ls())
setwd("/Users/CaitlynLim/Documents/EC4383/Assignment 1")
library(dplyr)
library(data.table)

#read survey file
soep <- read.csv("QualtricsSurveyData.csv")

#checking data set (NAs and all respondent had to answer the question twice)
sum(is.na(soep)) #No NAs
sum(soep$question == "2")
sum(soep$question == "3")

#creating 2 levels for female and male
soep$femaleXmale <- "female"  
soep$femaleXmale <- ifelse(soep$female == "0", "male", soep$femaleXmale)

#check
soep[1:100, c("female", "femaleXmale")]

#group age clearly
min(soep$age)
max(soep$age)

setDT(soep)[age <1, agegroup := "0-1"]
soep[age >19 & age <25, agegroup := "20-24"]
soep[age >24 & age <35, agegroup := "25-34"]
soep[age >34 & age <45, agegroup := "35-44"]
soep[age >44 & age <55, agegroup := "45-54"]
soep[age >54 & age <65, agegroup := "55-64"]
soep[age >64 & age <86, agegroup := "65-85"]
soep[1:100, c("age", "agegroup")] #check

befQn <- filter(soep, soep$question == "2")
aftQn <- filter(soep, soep$question == "3")

#checking for potential interactions
summary(lm(aftQn$ac_behavior ~aftQn$dwellingtype + aftQn$own_ac + 
             aftQn$dwellingtype:aftQn$own_ac, aftQn ))
summary(lm(aftQn$purifier_behavior ~ aftQn$dwellingtype + aftQn$own_purifier + 
             aftQn$dwellingtype:aftQn$own_purifier, aftQn ))
summary(lm(aftQn$purifier_behavior ~ aftQn$agegroup + aftQn$own_purifier +
             aftQn$agegroup:aftQn$own_purifier, aftQn ))
summary(lm(aftQn$ac_behavior ~ aftQn$dwellingtype + aftQn$agegroup +aftQn$dwellingtype:aftQn$agegroup, aftQn ))
summary(lm(aftQn$ac_behavior ~ aftQn$female + aftQn$position + aftQn$position:aftQn$female, aftQn ))
summary(lm(aftQn$dineout_behavior ~ aftQn$dwellingtype + aftQn$own_fridge +
             aftQn$dwellingtype:aftQn$own_fridge, aftQn ))
#high p value for each component, limited interaction for possible terms

#
#
#
#

#GENERAL: LINEAR PROBABILITY MODEL + EQUALITY OF PROPORTIONS + MCNEMAR TEST
#ac usage
ac_model_bef <- lm(befQn$ac_behavior ~ befQn$femaleXmale + befQn$agegroup + 
                     befQn$position + befQn$dwellingtype + befQn$own_ac, befQn)
summary(ac_model_bef)

ac_model_aft <- lm(aftQn$ac_behavior ~ aftQn$femaleXmale + aftQn$agegroup + 
                     aftQn$position + aftQn$dwellingtype + aftQn$own_ac, aftQn)
summary(ac_model_aft)

sum(befQn$ac_behavior)
sum(aftQn$ac_behavior)
((203-160)/160)*100 #increased 26.875%
prop.test(x = c(160, 203 ), n = c(311, 311)) #the difference is statiscally significant

#correlation between AC and Fan
cor.test(befQn$ac_behavior, befQn$fan_behavior, method=c("pearson"))
cor.test(aftQn$ac_behavior, aftQn$fan_behavior, method=c("pearson"))



#McNemar test
table(befQn$ac_behavior, aftQn$ac_behavior, useNA = "always")
acsig = as.table(rbind(c(90, 61), 
                        c( 18, 142) ))
colnames(acsig) <- rownames(acsig) <- c("No", "Yes")
names(dimnames(acsig)) = c("Before", "After")
acsig
margin.table(acsig, 1)
margin.table(acsig, 2)
sum(acsig)

mcnemar.test(acsig, correct=FALSE)

#purifier usage
pur_model_bef <- lm(befQn$purifier_behavior ~ befQn$femaleXmale + befQn$agegroup + 
                      befQn$position + befQn$dwellingtype + befQn$own_purifier, befQn)
summary(pur_model_bef)

pur_model_aft <- lm(aftQn$purifier_behavior ~ aftQn$femaleXmale + aftQn$agegroup + 
                      aftQn$position + aftQn$dwellingtype + aftQn$own_purifier, aftQn)

summary(pur_model_aft)

sum(befQn$purifier_behavior)
sum(aftQn$purifier_behavior)
((137-34)/34)*100 #increased by 302.94%
prop.test(x = c(34, 137), n = c(311, 311)) #statistically significant

#McNemar test
table(befQn$purifier_behavior, aftQn$purifier_behavior, useNA = "always")
pusig = as.table(rbind(c(171, 106), 
                       c( 3, 31) ))
colnames(pusig) <- rownames(pusig) <- c("No", "Yes")
names(dimnames(pusig)) = c("Before", "After")
pusig
margin.table(pusig, 1)
margin.table(pusig, 2)
sum(pusig)

mcnemar.test(pusig, correct=FALSE)

#fan usage
fan_model_bef <- lm(befQn$fan_behavior ~ befQn$femaleXmale + befQn$agegroup + 
                      befQn$position + befQn$dwellingtype + befQn$own_fan, befQn)
summary(pur_model_bef)

fan_model_aft <- lm(aftQn$fan_behavior ~ aftQn$femaleXmale + aftQn$agegroup + 
                      aftQn$position + aftQn$dwellingtype + aftQn$own_fan, aftQn)

summary(fan_model_aft)

sum(befQn$fan_behavior)
sum(aftQn$fan_behavior)
((88-125)/125)*100 #decreased by 29.6%
prop.test(x = c(125, 88), n = c(311, 311)) #statistically significant

#McNemar test
table(befQn$fan_behavior, aftQn$fan_behavior, useNA = "always")
fansig = as.table(rbind(c(172, 12), 
                        c( 49, 76) ))
colnames(fansig) <- rownames(fansig) <- c("No", "Yes")
names(dimnames(fansig)) = c("Before", "After")
fansig
margin.table(fansig, 1)
margin.table(fansig, 2)
sum(fansig)

mcnemar.test(fansig, correct=FALSE)

#dineout behavior
dine_model_bef <- lm(befQn$dineout_behavior ~ befQn$femaleXmale + befQn$agegroup + 
                      befQn$position + befQn$dwellingtype + befQn$own_fridge, befQn)
summary(dine_model_bef)

dine_model_aft <- lm(aftQn$fan_behavior ~ aftQn$femaleXmale + aftQn$agegroup + 
                      aftQn$position + aftQn$dwellingtype + aftQn$own_fridge, aftQn)

summary(dine_model_aft)

sum(befQn$dineout_behavior)
sum(aftQn$dineout_behavior)
((96-214)/214)*100

table(befQn$dineout_behavior, aftQn$dineout_behavior, useNA = "always")
dinesig = as.table(rbind(c(92, 5), 
                        c( 123, 91) ))
colnames(dinesig) <- rownames(dinesig) <- c("No", "Yes")
names(dimnames(dinesig)) = c("Before", "After")
dinesig
margin.table(dinesig, 1)
margin.table(dinesig, 2)
sum(dinesig)

mcnemar.test(dinesig, correct=FALSE)


#windows open behavior
open_model_bef <- lm(befQn$windowopen_behavior ~ befQn$femaleXmale + befQn$agegroup + 
                      befQn$position + befQn$dwellingtype, befQn)
summary(open_model_bef)

open_model_aft <- lm(aftQn$fan_behavior ~ aftQn$femaleXmale + aftQn$agegroup + 
                      aftQn$position + aftQn$dwellingtype, aftQn)
summary(open_model_aft)

sum(befQn$windowopen_behavior)
sum(aftQn$windowopen_behavior)
((41-150)/150)*100

table(befQn$windowopen_behavior, aftQn$windowopen_behavior, useNA = "always")
opensig = as.table(rbind(c(156, 5), 
                         c( 114, 36) ))
colnames(opensig) <- rownames(opensig) <- c("No", "Yes")
names(dimnames(opensig)) = c("Before", "After")
opensig
margin.table(opensig, 1)
margin.table(opensig, 2)
sum(opensig)

mcnemar.test(opensig, correct=FALSE)

#correlation between Windows Open and Closed
cor.test(befQn$windowopen_behavior, befQn$windowclosed_behavior, method=c("pearson"))
cor.test(aftQn$windowopen_behavior, aftQn$windowclosed_behavior, method=c("pearson"))

#windows closed behavior
closed_model_bef <- lm(befQn$windowclosed_behavior ~ befQn$femaleXmale + befQn$agegroup + 
                       befQn$position + befQn$dwellingtype, befQn)
summary(closed_model_bef)

closed_model_aft <- lm(aftQn$windowclosed_behavior ~ aftQn$femaleXmale + aftQn$agegroup + 
                       aftQn$position + aftQn$dwellingtype, aftQn)
summary(closed_model_aft)

sum(befQn$windowclosed_behavior)
sum(aftQn$windowclosed_behavior)

((270-161)/161)*100

table(befQn$windowclosed_behavior, aftQn$windowclosed_behavior, useNA = "always")
closedsig = as.table(rbind(c(36, 114), 
                         c( 5, 156) ))
colnames(closedsig) <- rownames(closedsig) <- c("No", "Yes")
names(dimnames(closedsig)) = c("Before", "After")
closedsig
margin.table(closedsig, 1)
margin.table(closedsig, 2)
sum(closedsig)

mcnemar.test(closedsig, correct=FALSE)

#
#
#
#

#Slice by GENDER 

# ac usage status by gender before
table(befQn$femaleXmale, befQn$ac_behavior, useNA = "always")
prop.table(table(befQn$femaleXmale, befQn$ac_behavior, useNA = "always"), 1)

#after
table(aftQn$femaleXmale, aftQn$ac_behavior, useNA = "always")
prop.table(table(aftQn$femaleXmale, aftQn$ac_behavior, useNA = "always"), 1) 
(0.6533333-0.4866667)*100 #females
(0.6521739-0.5403727)*100 #males
prop.test(x = c(98, 105 ), n = c(150, 161))
#the difference between female and male choosing to use ac is not stat sig

# purifier behavior by gender before
table(befQn$femaleXmale, befQn$purifier_behavior, useNA = "always")
a <- (prop.table(table(befQn$femaleXmale, befQn$purifier_behavior, useNA = "always"), 1))*100 

#after
table(aftQn$femaleXmale, aftQn$purifier_behavior, useNA = "always")
b <- (prop.table(table(aftQn$femaleXmale, aftQn$purifier_behavior, useNA = "always"), 1))*100 
b - a
prop.test(x = c(66, 71), n = c(150, 161)) #statistically insignificant between genders


# fan behavior by gender before
table(befQn$femaleXmale, befQn$fan_behavior, useNA = "always")
c <- (prop.table(table(befQn$femaleXmale, befQn$fan_behavior, useNA = "always"), 1))*100 

#after
table(aftQn$femaleXmale, aftQn$fan_behavior, useNA = "always")
d <- (prop.table(table(aftQn$femaleXmale, aftQn$fan_behavior, useNA = "always"), 1))*100 
d-c
prop.test(x = c(43, 45), n = c(150, 161)) #statistically insignificant between genders

#dineout behavior by gender before
table(befQn$femaleXmale, befQn$dineout_behavior, useNA = "always")
e <- (prop.table(table(befQn$femaleXmale, befQn$dineout_behavior, useNA = "always"), 1))*100 

#after
table(aftQn$femaleXmale, aftQn$dineout_behavior, useNA = "always")
f <- (prop.table(table(aftQn$femaleXmale, aftQn$dineout_behavior, useNA = "always"), 1))*100
f-e
prop.test(x = c(36, 60), n = c(150, 161)) #statistically significant

# windows open behavior by gender before
table(befQn$femaleXmale, befQn$windowopen_behavior, useNA = "always")
g <- (prop.table(table(befQn$femaleXmale, befQn$windowopen_behavior, useNA = "always"), 1))*100 

#after
table(aftQn$femaleXmale, aftQn$windowopen_behavior, useNA = "always")
h <- (prop.table(table(aftQn$femaleXmale, aftQn$windowopen_behavior, useNA = "always"), 1))*100 
h-g
prop.test(x = c(22, 19), n = c(150, 161)) #statistically insignificant

# windows closed behavior by gender before
table(befQn$femaleXmale, befQn$windowclosed_behavior, useNA = "always")
j <- (prop.table(table(befQn$femaleXmale, befQn$windowclosed_behavior, useNA = "always"), 1))*100 

#after
table(aftQn$femaleXmale, aftQn$windowclosed_behavior, useNA = "always")
k <- (prop.table(table(aftQn$femaleXmale, aftQn$windowclosed_behavior, useNA = "always"), 1))*100 
prop.test(x = c(128, 142), n = c(150, 161)) #statistically insignificant

#Only dineout behavior differences between male and female are satistically significant

#Slice by HOUSING TYPE

#ac usage by housing type
table(befQn$dwellingtype, befQn$ac_behavior, useNA = "always")
(prop.table(table(befQn$dwellingtype, befQn$ac_behavior, useNA = "always"), 1))*100

table(aftQn$dwellingtype, aftQn$ac_behavior, useNA = "always")
(prop.table(table(aftQn$dwellingtype, aftQn$ac_behavior, useNA = "always"), 1))*100
prop.test(x = c(9, 27, 53, 65, 17, 32), n = c(18, 48, 95, 88, 20, 42)) #statistically different

# regression of purifier behavior on dwelling type
table(befQn$dwellingtype, befQn$purifier_behavior, useNA = "always")
(prop.table(table(befQn$dwellingtype, befQn$purifier_behavior, useNA = "always"), 1))*100

table(aftQn$dwellingtype, aftQn$purifier_behavior, useNA = "always")
(prop.table(table(aftQn$dwellingtype, aftQn$purifier_behavior, useNA = "always"), 1))*100
prop.test(x = c(20, 4, 19, 39, 44, 11), n = c(42, 18, 48, 95, 88, 20)) #not statistically significant

# fan behavior on dwelling type
table(befQn$dwellingtype, befQn$fan_behavior, useNA = "always")
(prop.table(table(befQn$dwellingtype, befQn$fan_behavior, useNA = "always"), 1))*100

table(aftQn$dwellingtype, aftQn$fan_behavior, useNA = "always")
(prop.table(table(aftQn$dwellingtype, aftQn$fan_behavior, useNA = "always"), 1))*100 
prop.test(x = c(14, 11, 10, 31, 19, 3), n = c(42, 18, 48, 95, 88, 20)) #statiscally different


# dineout behavior on dwelling type
table(befQn$dwellingtype, befQn$dineout_behavior, useNA = "always")
(prop.table(table(befQn$dwellingtype, befQn$dineout_behavior, useNA = "always"), 1))*100

table(aftQn$dwellingtype, aftQn$dineout_behavior, useNA = "always")
(prop.table(table(aftQn$dwellingtype, aftQn$dineout_behavior, useNA = "always"), 1))*100 
prop.test(x = c(13, 8, 12, 32, 24, 7), n = c(42, 18, 48, 95, 88, 20)) #not significant

# windows open behavior on dwelling type
table(befQn$dwellingtype, befQn$windowopen_behavior, useNA = "always")
(prop.table(table(befQn$dwellingtype, befQn$windowopen_behavior, useNA = "always"), 1))*100

table(aftQn$dwellingtype, aftQn$windowopen_behavior, useNA = "always")
(prop.table(table(aftQn$dwellingtype, aftQn$windowopen_behavior, useNA = "always"), 1))*100 
prop.test(x = c(3, 6, 12, 13, 6, 1), n = c(42, 18, 48, 95, 88, 20)) #incorrect

# windows closed behavior on dwelling type
table(befQn$dwellingtype, befQn$windowclosed_behavior, useNA = "always")
(prop.table(table(befQn$dwellingtype, befQn$windowclosed_behavior, useNA = "always"), 1))*100

table(aftQn$dwellingtype, aftQn$windowclosed_behavior, useNA = "always")
(prop.table(table(aftQn$dwellingtype, aftQn$windowclosed_behavior, useNA = "always"), 1))*100
prop.test(x = c(39, 12, 36, 82, 82, 19), n = c(42, 18, 48, 95, 88, 20)) #incorrect



#Slice by AGE

#ac behavior by age
summary(lm(befQn$ac_behavior ~ befQn$age, befQn))
summary(lm(aftQn$ac_behavior ~ aftQn$age, aftQn)) #insignificant

#Ac usage by age before
table(befQn$agegroup, befQn$ac_behavior, useNA = "always")
(prop.table(table(befQn$agegroup, befQn$ac_behavior, useNA = "always"), 1))*100

#after
table(aftQn$agegroup, aftQn$ac_behavior, useNA = "always")
(prop.table(table(aftQn$agegroup, aftQn$ac_behavior, useNA = "always"), 1))*100
prop.test(x = c(23, 45, 52, 50, 28, 5), n = c(35, 74, 73, 76, 41, 12 )) #incorrect/insignificant

#purifier by age
table(befQn$agegroup, befQn$purifier_behavior, useNA = "always")
(prop.table(table(befQn$agegroup, befQn$purifier_behavior, useNA = "always"), 1))*100

#after
table(aftQn$agegroup, aftQn$purifier_behavior, useNA = "always")
(prop.table(table(aftQn$agegroup, aftQn$purifier_behavior, useNA = "always"), 1))*100
prop.test(x = c(14, 34, 37, 31, 15, 6), n = c(35, 74, 73, 76, 41, 12 )) #insignificant

#fan by age
table(befQn$agegroup, befQn$fan_behavior, useNA = "always")
(prop.table(table(befQn$agegroup, befQn$fan_behavior, useNA = "always"), 1))*100

#after
table(aftQn$agegroup, aftQn$fan_behavior, useNA = "always")
(prop.table(table(aftQn$agegroup, aftQn$fan_behavior, useNA = "always"), 1))*100
prop.test(x = c(9, 19, 20, 27, 11, 2), n = c(35, 74, 73, 76, 41, 12 )) #insignificant

#dineout by age
table(befQn$agegroup, befQn$dineout_behavior, useNA = "always")
(prop.table(table(befQn$agegroup, befQn$dineout_behavior, useNA = "always"), 1))*100

#after
table(aftQn$agegroup, aftQn$dineout_behavior, useNA = "always")
(prop.table(table(aftQn$agegroup, aftQn$dineout_behavior, useNA = "always"), 1))*100
prop.test(x = c(9, 23, 19, 26, 13, 6), n = c(35, 74, 73, 76, 41, 12 )) #insignificant

#windowsopen by age
table(befQn$agegroup, befQn$windowopen_behavior, useNA = "always")
(prop.table(table(befQn$agegroup, befQn$windowopen_behavior, useNA = "always"), 1))*100

#after
table(aftQn$agegroup, aftQn$windowopen_behavior, useNA = "always")
(prop.table(table(aftQn$agegroup, aftQn$windowopen_behavior, useNA = "always"), 1))*100
prop.test(x = c(1, 11, 11, 10, 5, 3), n = c(35, 74, 73, 76, 41, 12 )) #insignificant

#windowsclosed by age
table(befQn$agegroup, befQn$windowclosed_behavior, useNA = "always")
(prop.table(table(befQn$agegroup, befQn$windowclosed_behavior, useNA = "always"), 1))*100

#after
table(aftQn$agegroup, aftQn$windowclosed_behavior, useNA = "always")
(prop.table(table(aftQn$agegroup, aftQn$windowclosed_behavior, useNA = "always"), 1))*100
prop.test(x = c(34, 63, 62, 66, 36, 9), n = c(35, 74, 73, 76, 41, 12 )) #insignificant

#appliances owned
sum(aftQn$own_ac)
sum(aftQn$own_fan)
sum(aftQn$own_purifier)
sum(aftQn$own_washer)
sum(aftQn$own_fridge)
sum(aftQn$own_car)




#Some questions, not in analysis
model3 <- glm(befQn$ac_behavior ~ befQn$age, family = binomial(link = "logit"), befQn)
summary(model3) #but it is not statiscally significant

model4 <- glm(aftQn$ac_behavior ~ aftQn$age, family = binomial(link = "logit"), aftQn)
summary(model4) #but it is not statiscally significant

fmodel1 <- glm(aftQn$ac_behavior ~ aftQn$femaleXmale
              + aftQn$age + aftQn$position + 
                aftQn$dwellingtype * aftQn$own_ac , 
              family = binomial(link = "logit"), 
              aftQn)

fmodel2 <- glm(aftQn$purifier_behavior ~ aftQn$femaleXmale
               + aftQn$age + aftQn$position + 
                 aftQn$dwellingtype * aftQn$own_purifier , 
               family = binomial(link = "logit"), 
               aftQn)
summary(fmodel2)
fmodel3 <- glm(aftQn$fan_behavior ~ aftQn$femaleXmale
               + aftQn$age + aftQn$position + 
                 aftQn$dwellingtype , 
               family = binomial(link = "logit"), 
               aftQn)

fmodel4 <- glm(aftQn$dineout_behavior ~ aftQn$femaleXmale
               + aftQn$age + aftQn$position + 
                 aftQn$dwellingtype , 
               family = binomial(link = "logit"), 
               aftQn)

#Chi-squared test cannot be used for within subject test aka the glm? due to (assumption for independent observation)
#For within subject binary: McNemar's chi-square
#3 or more options: could use Cochran Q test
#within subject binary:  within-subjects z-test, test of equality of proportions

#In this case, a before and after, binary, within subjects:
#within-subjects z-test of equality of proportions aka McNemar's test

#write.csv(befQn, file="beforedata.csv")
#write.csv(aftQn, file="afterdata.csv")















