library(haven)
library(tidyverse)
library(ggplot2)

# Change the data path below to the path on your computer
CPS_micro_data <- read_dta("D:/UPenn/MSSP 630 Quantitative Reasoning/CPS_micro_data.dta")

#Recode the education variable into years of education
CPS_micro_data$educ2 <- NA # create a new variable

CPS_micro_data$educ2[CPS_micro_data$chareduc=="1 year of college"] <- 13
CPS_micro_data$educ2[CPS_micro_data$chareduc=="12th grade, diploma unclear"] <- 11
CPS_micro_data$educ2[CPS_micro_data$chareduc=="12th grade, no diploma"] <- 11                                
CPS_micro_data$educ2[CPS_micro_data$chareduc=="2 years of college"] <-  14
CPS_micro_data$educ2[CPS_micro_data$chareduc=="3 years of college"] <-  15                                 
CPS_micro_data$educ2[CPS_micro_data$chareduc=="4 years of college"] <- 16
CPS_micro_data$educ2[CPS_micro_data$chareduc=="5 years of college"] <-   17                          
CPS_micro_data$educ2[CPS_micro_data$chareduc=="6+ years of college"] <- 19
CPS_micro_data$educ2[CPS_micro_data$chareduc=="Associate's degree, academic program"] <-  14
CPS_micro_data$educ2[CPS_micro_data$chareduc=="Associate's degree, occupational/vocational program"] <- 14  
CPS_micro_data$educ2[CPS_micro_data$chareduc=="Bachelor's degree"] <- 16
CPS_micro_data$educ2[CPS_micro_data$chareduc=="Doctorate degree"] <- 22
CPS_micro_data$educ2[CPS_micro_data$chareduc=="Grade 1"] <- 1                              
CPS_micro_data$educ2[CPS_micro_data$chareduc=="Grade 10"] <- 10
CPS_micro_data$educ2[CPS_micro_data$chareduc=="Grade 11"] <-   11                                      
CPS_micro_data$educ2[CPS_micro_data$chareduc=="Grade 2"] <- 2
CPS_micro_data$educ2[CPS_micro_data$chareduc=="Grade 3"] <-  3                                       
CPS_micro_data$educ2[CPS_micro_data$chareduc=="Grade 4"] <- 4
CPS_micro_data$educ2[CPS_micro_data$chareduc=="Grade 5"] <-  5                                       
CPS_micro_data$educ2[CPS_micro_data$chareduc=="Grade 6"] <- 6
CPS_micro_data$educ2[CPS_micro_data$chareduc=="Grade 7"] <-  7                                       
CPS_micro_data$educ2[CPS_micro_data$chareduc=="Grade 8"] <- 8
CPS_micro_data$educ2[CPS_micro_data$chareduc=="Grade 9"] <-  9                     
CPS_micro_data$educ2[CPS_micro_data$chareduc=="Grades 1, 2, 3, or 4"] <- 2.5  
CPS_micro_data$educ2[CPS_micro_data$chareduc=="Grades 5 or 6"] <- 5.5                           
CPS_micro_data$educ2[CPS_micro_data$chareduc=="Grades 7 or 8"] <- 7.5
CPS_micro_data$educ2[CPS_micro_data$chareduc=="High school diploma or equivalent"] <- 12                                     
CPS_micro_data$educ2[CPS_micro_data$chareduc=="Master's degree"] <- 18
CPS_micro_data$educ2[CPS_micro_data$chareduc=="NIU or blank"] <-  NA                          
CPS_micro_data$educ2[CPS_micro_data$chareduc=="None or preschool"] <- 0 
CPS_micro_data$educ2[CPS_micro_data$chareduc=="Professional school degree"] <-  19                         
CPS_micro_data$educ2[CPS_micro_data$chareduc=="Some college but no degree"] <- 13


### Part 1: Plotting wage over time for the low-educated

## 1.1-Nevade: minimum wage at 8.00 (2011-2016)
# Creating a subset of data for the low educated (less than 12 years of schooling)
by_year_state <- group_by(filter(CPS_micro_data,educ2<12 & year>=2011),year,state) 
# also restricting to year>=2011 to have a constant minimum wage in Nevada

View(CPS_micro_data)
View(by_year_state)

# Creating a dataset of means by year and state for the variables we are interested in
means <- summarise(by_year_state,
                   earnweek=mean(earnweek,na.rm=TRUE),
                   employed=mean(employed,na.rm=TRUE),
                   hourwage=mean(hourwage,na.rm=TRUE),
                   minwage=mean(minwage,na.rm=TRUE))

View(means)
# Plotting wages over time for the low educated

ggplot(data = means) + 
  geom_line(mapping = aes(x = year, y = minwage),subset(means,state=="Nevada"), size = 1, color="blue") +
  geom_line(mapping = aes(x = year, y = hourwage),subset(means,state=="Nevada"), size = 1, color="red") +
  ggtitle("Minimum wage and hourly wage in Nevada") +
  xlab("Year") + ylab("Wage") 

# EXERCISE: plot the same graph as above for California

## 1.1-California: minimum wage at 8.00 (2008-2013)
# Creating a subset of data for the low educated (less than 12 years of schooling)
# Restrcting the year to 2008-2013 when the minimum wage was the same at 8.00 in California
by_year_state_CA <- group_by(filter(CPS_micro_data,educ2<12 & 2008<=year & year<=2013),year,state) 

means1 <- summarise(by_year_state_CA,
                   earnweek=mean(earnweek,na.rm=TRUE),
                   employed=mean(employed,na.rm=TRUE),
                   hourwage=mean(hourwage,na.rm=TRUE),
                   minwage=mean(minwage,na.rm=TRUE))
View(means1)

ggplot(data = means1) + 
  geom_line(mapping = aes(x = year, y = minwage),subset(means1,state=="California"), size = 1, color="blue") +
  geom_line(mapping = aes(x = year, y = hourwage),subset(means1,state=="California"), size = 1, color="red") +
  ggtitle("Minimum wage and hourly wage in California") +
  xlab("Year") + ylab("Wage")

## 1.1-California: same time period as Nevade with changing minimum wage
ggplot(data = means) + 
  geom_line(mapping = aes(x = year, y = minwage),subset(means,state=="California"), size = 1, color="blue") +
  geom_line(mapping = aes(x = year, y = hourwage),subset(means,state=="California"), size = 1, color="red") +
  ggtitle("Minimum wage and hourly wage in California") +
  xlab("Year") + ylab("Wage")

## 1.2-minimum wage: minimum wage in NV and CA after 2011
ggplot(data = means) + 
  geom_line(mapping = aes(x = year, y = minwage),subset(means,state=="Nevada")) +
  geom_line(mapping = aes(x = year, y = minwage),subset(means,state=="California"),color="red") +
  geom_vline(xintercept=2013, linetype = 'dashed', color="red") + # CA increases minimum wage
  ggtitle("Minimum wage NV&CA since 2011") +
  xlab("Year") + ylab("Minimum wage")

# EXERCISE: plot the same graph as above for y= :
## 1.2-Hourly wages
ggplot(data = means) + 
  geom_line(mapping = aes(x = year, y = hourwage),subset(means,state=="Nevada")) +
  geom_line(mapping = aes(x = year, y = hourwage),subset(means,state=="California"),color="red") +
  geom_vline(xintercept=2013, linetype = 'dashed', color="red") + # CA increases minimum wage
  ggtitle("Hourly Wage NV&CA since 2011") +
  xlab("Year") + ylab("Hourly Wage")

## 1.2-Weekly earnings
ggplot(data = means) + 
  geom_line(mapping = aes(x = year, y = earnweek),subset(means,state=="Nevada")) +
  geom_line(mapping = aes(x = year, y = earnweek),subset(means,state=="California"),color="red") +
  geom_vline(xintercept=2013, linetype = 'dashed', color="red") + # CA increases minimum wage
  ggtitle("Weekly Earnings NV&CA since 2011") +
  xlab("Year") + ylab("Weekly Earnings")

## 1.2-Employed
ggplot(data = means) + 
  geom_line(mapping = aes(x = year, y = employed),subset(means,state=="Nevada")) +
  geom_line(mapping = aes(x = year, y = employed),subset(means,state=="California"),color="red") +
  geom_vline(xintercept=2013, linetype = 'dashed', color="red") + # CA increases minimum wage
  ggtitle("Unemployment Rate NV&CA since 2011") +
  xlab("Year") + ylab("Unemployment Rate")

### Part 2: effect of minimum wage on other variables
# EXERCISE: regress outcomes on the minimum wage variable for the following outcomes:
less_than_high_school <- group_by(filter(CPS_micro_data,educ2<12),year,state)
employment_rate <- summarise(less_than_high_school,
                    employed=mean(employed,na.rm=TRUE),
                    minwage=mean(minwage,na.rm=TRUE))
employment_rate <- na.omit(employment_rate)
View(employment_rate)

#View(means2)
View(less_than_high_school)
means2_NV <- subset(less_than_high_school, state=="Nevada")
means2_NV <- na.omit(means2_NV)
means2_CA <- subset(less_than_high_school, state=="California", na.rm=TRUE)
means2_CA <- na.omit(means2_CA)
View(means2_NV)
View(means2_CA)

## 2.1 Hourly wages
Model_NV = lm(data = means2_NV, hourwage ~ minwage)
Model_NV
summary(Model_NV)
Model_CA = lm(data = means2_CA, hourwage ~ minwage)
Model_CA
summary(Model_CA)

ggplot(data = less_than_high_school) + 
  geom_smooth(aes(x = minwage, y = hourwage, fill=state, colour=state),
              method = lm, se = FALSE, size = 1.3) +
  ggtitle("Regression: hourly wage ~ minimum wage") + 
  xlab("Minimum Wage (dollar)") +
  ylab("Hourly Wage (dollar)") +
  theme_classic()

## 2.2 Weekly earnings
Model_NV_ew = lm(data = means2_NV, earnweek ~ minwage)
Model_NV_ew
summary(Model_NV_ew)
Model_CA_ew = lm(data = means2_CA, earnweek ~ minwage)
Model_CA_ew
summary(Model_CA_ew)

ggplot(data = less_than_high_school) + 
  geom_smooth(aes(x = minwage, y = earnweek, fill=state, colour=state),
              method = lm, se = FALSE, size = 1.3) +
  ggtitle("Regression: weekly earning ~ minimum wage") + 
  xlab("Minimum Wage (dollar)") +
  ylab("Weekly Earning (dollar)") +
  theme_classic()

## 2.3 Employed (NB: this regression is not quite appropriate because employed is a categorical, not a continuous variable)
employment_nv <- subset(employment_rate, employment_rate$state == "Nevada")
employment_ca <- subset(employment_rate, employment_rate$state == "California")
Model_NV_em = lm(data = employment_nv, employed ~ minwage)
Model_NV_em
summary(Model_NV_em)
Model_CA_em = lm(data = employment_ca, employed ~ minwage)
Model_CA_em
summary(Model_CA_em)

ggplot(data = employment_rate) + 
  geom_smooth(aes(x = minwage, y = employed, fill=state, colour=state),
              method = lm, se = FALSE, size = 1.3) +
  ggtitle("Regression: employment ~ minimum wage") + 
  xlab("Minimum Wage (dollar)") +
  ylab("Employment Rate(%)") +
  theme_classic()

### Part 3
# Graphical analysis for the California minimum wage increase of 2014
# Before is: 2011-2013
# After is: 2014-2016

# EXERCISE: Plot the distribution of hourly wages in Nevada and California 
# before the minimum wage increase in California (see below for the after graph)
# and for 2016 when the minimum wage was 10 in California

# 2011-2013
ggplot(data = subset(by_year_state,year>=2011, year<=2013)) + 
  geom_density(aes(hourwage,fill=state,colour=state),alpha=0.1) +
  geom_vline(xintercept=8.25,linetype = 'dashed', colour="cyan3") + # Nevada minimum wage was 8.25
  geom_vline(xintercept=8.00,linetype = 'dashed', colour="red") + # California minimum wage was 8.00 in 2011-2013
  ggtitle("Hourly wage before CA min. wage increase") +
  theme_classic()

# 2014-2015
ggplot(data = subset(by_year_state,year>=2014, year<=2015)) + 
  geom_density(aes(hourwage,fill=state,colour=state),alpha=0.1) +
  geom_vline(xintercept=8.25,linetype = 'dashed', colour="cyan3") + # Nevada minimum wage is 8.25
  geom_vline(xintercept=9,linetype = 'dashed', colour="red") + # California minimum wage is 9 in 2014-2015
  ggtitle("Hourly wage after CA min. wage increase") +
  theme_classic()

# 2016
ggplot(data = subset(by_year_state,year=2016)) + 
  geom_density(aes(hourwage,fill=state,colour=state),alpha=0.1) +
  geom_vline(xintercept=8.25,linetype = 'dashed', colour="cyan3") +
  geom_vline(xintercept=10,linetype = 'dashed', colour="red") + # California minimum wage is 10 in 2016
  ggtitle("Hourly wage 2016") +
  theme_classic()

### Part 4: Difference-in-differences
# Difference in differences estimates using tests for mean differences & proportions
## Testing the difference before the increase in the minimum wage
t.test(subset(by_year_state,state=="Nevada"& year>=2011 & year<=2013)$hourwage, 
       subset(by_year_state,state=="California"& year>=2011 & year<=2013)$hourwage)

# EXERCISE: Test the difference in hourly wage after the increase in the minimum wage
t.test(subset(by_year_state,state=="Nevada"& year>=2014 & year<=2015)$hourwage, 
       subset(by_year_state,state=="California"& year>=2014 & year<=2015)$hourwage)

# EXERCISE: test the difference in weekly earnings before and after the increase in the minimum wage.
# Testing the difference before the increase in the minimum wage
t.test(subset(by_year_state,state=="Nevada"& year>=2011 & year<=2013)$earnweek, 
       subset(by_year_state,state=="California"& year>=2011 & year<=2013)$earnweek)

# Testing the difference after the increase in the minimum wage
t.test(subset(by_year_state,state=="Nevada"& year>=2014 & year<=2015)$earnweek, 
       subset(by_year_state,state=="California"& year>=2014 & year<=2015)$earnweek)

## Testing the difference before the increase in the minimum wage
by_year_state$femp <- as.factor(by_year_state$employed)
summary(by_year_state$femp)

tablebefore <- table(subset(by_year_state,year>=2011 & year<=2013)$state,
                     subset(by_year_state,year>=2011 & year<=2013)$femp)

prop.table(tablebefore,1)

prop.test(tablebefore)

## EXERCISE: testing the difference in employment after the increase in the minimum wage

tableafter <- table(subset(by_year_state,year>=2014 & year<=2015)$state,
                     subset(by_year_state,year>=2014 & year<=2015)$femp)

prop.table(tableafter,1)

prop.test(tableafter)
