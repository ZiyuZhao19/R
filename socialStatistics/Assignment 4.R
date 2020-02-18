### Ziyu Zhao 71435169

install.packages("psych")
library(psych)
library(haven)
library(tidyverse)

### preparing data
CPS_micro_data <- read_dta("D:/UPenn/MSSP 630 Quantitative Reasoning/CPS_micro_data.dta")
View(CPS_micro_data)
CPS_micro_data_2 <- CPS_micro_data[,c("state","educ","chareduc","race","earnweek")]
View(CPS_micro_data_2)

### excluding blanks
describe(CPS_micro_data_2, na.rm = TRUE)
CPS_micro_data_as4 <- na.omit(CPS_micro_data_2)
View(CPS_micro_data_as4)

### data processing 1: subcategorize by race
CPS_micro_data_as4$race2 <- CPS_micro_data_as4$race
CPS_micro_data_as4$race2 [CPS_micro_data_as4$race == "Asian only"
                          ]<-"Asian"
CPS_micro_data_as4$race2 [CPS_micro_data_as4$race == "White"
                          ]<-"White"
CPS_micro_data_as4$race2 [CPS_micro_data_as4$race == "Black/Negro"
                          ]<-"Black"
CPS_micro_data_as4$race2 [CPS_micro_data_as4$race == "Hawaiian/Pacific Islander only"
                          ]<-"Hawaiian/Pacific Islander"
CPS_micro_data_as4$race2 [CPS_micro_data_as4$race == "Asian or Pacific Islander"
                          ]<-"Asian/Pacific"
CPS_micro_data_as4$race2 [CPS_micro_data_as4$race == "Two or three races, unspecified" |
                            CPS_micro_data_as4$race == "Four or five races, unspecified" |
                            CPS_micro_data_as4$race == "Other (single) race, n.e.c." |
                            CPS_micro_data_as4$race == "American Indian-Asian" |
                            CPS_micro_data_as4$race == "American Indian-Hawaiian/Pacific Islander" |
                            CPS_micro_data_as4$race == "American Indian/Aleut/Eskimo" |
                            CPS_micro_data_as4$race == "Asian-Hawaiian/Pacific Islander" |
                            CPS_micro_data_as4$race == "Black-American Indian" |
                            CPS_micro_data_as4$race == "Black-Asian" |
                            CPS_micro_data_as4$race == "Black-Hawaiian/Pacific Islander" |
                            CPS_micro_data_as4$race == "White-American Indian" |
                            CPS_micro_data_as4$race == "White-American Indian-Asian" |
                            CPS_micro_data_as4$race == "White-American Indian-Hawaiian/Pacific Islander" |
                            CPS_micro_data_as4$race == "White-Asian" |
                            CPS_micro_data_as4$race == "White-Asian-Hawaiian/Pacific Islander" |
                            CPS_micro_data_as4$race == "White-Black" |
                            CPS_micro_data_as4$race == "White-Black--Hawaiian/Pacific Islander" |
                            CPS_micro_data_as4$race == "White-Black-American Indian" |
                            CPS_micro_data_as4$race == "White-Black-American Indian-Asian" |
                            CPS_micro_data_as4$race == "White-Black-Asian" |
                            CPS_micro_data_as4$race == "White-Hawaiian/Pacific Islander"
                          ]<-"Mixed"

### data processing 2: years of education
CPS_micro_data_as4$educ2 <- CPS_micro_data_as4$chareduc
CPS_micro_data_as4$educ2=as.numeric(CPS_micro_data_as4$educ2)
CPS_micro_data_as4$educ2 [CPS_micro_data_as4$chareduc == "None or preschool" |
                            CPS_micro_data_as4$chareduc == "NIU or blank"
                          ]<-0
CPS_micro_data_as4$educ2 [CPS_micro_data_as4$chareduc == "Grade 1"]<-1
CPS_micro_data_as4$educ2 [CPS_micro_data_as4$chareduc == "Grade 2"]<-2
CPS_micro_data_as4$educ2 [CPS_micro_data_as4$chareduc == "Grade 3"]<-3
CPS_micro_data_as4$educ2 [CPS_micro_data_as4$chareduc == "Grade 4" |
                            CPS_micro_data_as4$chareduc == "Grade 1, 2, 3 or 4"
                          ]<-4
CPS_micro_data_as4$educ2 [CPS_micro_data_as4$chareduc == "Grade 5"]<-5
CPS_micro_data_as4$educ2 [CPS_micro_data_as4$chareduc == "Grade 6" |
                            CPS_micro_data_as4$chareduc == "Grade 5 or 6"
                          ]<-6
CPS_micro_data_as4$educ2 [CPS_micro_data_as4$chareduc == "Grade 7"]<-7
CPS_micro_data_as4$educ2 [CPS_micro_data_as4$chareduc == "Grade 8" |
                            CPS_micro_data_as4$chareduc == "Grade 7 or 8"
                          ]<-8
CPS_micro_data_as4$educ2 [CPS_micro_data_as4$chareduc == "Grade 9"]<-9
CPS_micro_data_as4$educ2 [CPS_micro_data_as4$chareduc == "Grade 10"]<-10
CPS_micro_data_as4$educ2 [CPS_micro_data_as4$chareduc == "Grade 11"]<-11
CPS_micro_data_as4$educ2 [CPS_micro_data_as4$chareduc == "12th grade, diploma unclear" |
                            CPS_micro_data_as4$chareduc == "12th grade, no diploma"
                          ]<-11.5
CPS_micro_data_as4$educ2 [CPS_micro_data_as4$chareduc == "High school diploma or equivalent"]<-12
CPS_micro_data_as4$educ2 [CPS_micro_data_as4$chareduc == "1 year of college"]<-13
CPS_micro_data_as4$educ2 [CPS_micro_data_as4$chareduc == "2 years of college" |
                            CPS_micro_data_as4$chareduc == "Associate's degree, academic program" |
                            CPS_micro_data_as4$chareduc == "Associate's degree, occupational/vocational program"
                          ]<-14
CPS_micro_data_as4$educ2 [CPS_micro_data_as4$chareduc == "3 years of college" |
                            CPS_micro_data_as4$chareduc == "Some college but no degree"
                            ]<-15
CPS_micro_data_as4$educ2 [CPS_micro_data_as4$chareduc == "4 years of college" |
                            CPS_micro_data_as4$chareduc == "Bachelor's degree"
                            ]<-16
CPS_micro_data_as4$educ2 [CPS_micro_data_as4$chareduc == "5 years of college" |
                            CPS_micro_data_as4$chareduc == "Professional school degree"
                            ]<-17
CPS_micro_data_as4$educ2 [CPS_micro_data_as4$chareduc == "6+ years of college" |
                            CPS_micro_data_as4$chareduc == "Master's degree"
                            ]<-18
CPS_micro_data_as4$educ2 [CPS_micro_data_as4$chareduc == "Doctorate degree"]<-20


### data processing 3: years of education
CPS_micro_data_as4$educ3 <- CPS_micro_data_as4$educ2
CPS_micro_data_as4$educ3 [CPS_micro_data_as4$educ2 < 12]<-"less than high school"
CPS_micro_data_as4$educ3 [CPS_micro_data_as4$educ2 == 12]<-"high school graduate"
CPS_micro_data_as4$educ3 [CPS_micro_data_as4$educ2 > 12 & CPS_micro_data_as4$educ2 < 16]<-"some college"
CPS_micro_data_as4$educ3 [CPS_micro_data_as4$educ2 == 16]<-"college graduate"
CPS_micro_data_as4$educ3 [CPS_micro_data_as4$educ2 > 16]<-"more than college"


### 1
CA <- subset(CPS_micro_data_as4, CPS_micro_data_as4$state == "California")
NV <- subset(CPS_micro_data_as4, CPS_micro_data_as4$state == "Nevada")
sum(CA$race2 == "Asian")

prop.test( x=c(sum(CA$race2 == "Asian"), sum(NV$race2 == "Asian")), 
           n=c(sum(CPS_micro_data_as4$state == "California"),
               sum(CPS_micro_data_as4$state == "Nevada")),
           conf.level = 0.95)

### 2
prop.test( x=c(sum(CA$race2 == "Black"), sum(NV$race2 == "Black")), 
           n=c(sum(CPS_micro_data_as4$state == "California"),
               sum(CPS_micro_data_as4$state == "Nevada")),
           conf.level = 0.95)

### 3
by(CPS_micro_data_as4,CPS_micro_data_as4$educ3,
   function(CPS_micro_data_as4){
     educ_earn_mean <- t.test(CPS_micro_data_as4$earnweek,conf.level=0.95)
   })

### 4
t.test(subset(CPS_micro_data_as4$earnweek, 
              CPS_micro_data_as4$educ3 == "college graduate"),
       subset(CPS_micro_data_as4$earnweek, 
              CPS_micro_data_as4$educ3 == "high school graduate"),
       alternative = "greater",
       conf.level = 0.95)

### 5
by(CPS_micro_data_as4,CPS_micro_data_as4$race2,
   function(CPS_micro_data_as4){
     educ_earn_mean <- t.test(CPS_micro_data_as4$earnweek,conf.level=0.95)
   })

### 6
edu_race <- table(CPS_micro_data_as4$race2,CPS_micro_data_as4$educ3)
edu_race_1 <-as.data.frame(edu_race)
edu_race_2 <- matrix(edu_race_1$Freq,ncol = 5, byrow = FALSE)
colnames(edu_race_2) = edu_race_1$Var2[c(1,7,13,19,25)]
rownames(edu_race_2) = edu_race_1$Var1[1:6]
chisq.test(edu_race_2)

View(edu_race_2)

# by column
prop.table(edu_race_2,1)
# by row
prop.table(edu_race_2,2)


### 7
# Asian
t.test(subset(CPS_micro_data_as4$earnweek, 
              CPS_micro_data_as4$race2 == "Asian"),
       subset(CPS_micro_data_as4$earnweek, 
              CPS_micro_data_as4$race2 == "White"),
       conf.level = 0.95)

# Asian/Pacific
t.test(subset(CPS_micro_data_as4$earnweek, 
              CPS_micro_data_as4$race2 == "Asian/Pacific"),
       subset(CPS_micro_data_as4$earnweek, 
              CPS_micro_data_as4$race2 == "White"),
       conf.level = 0.95)

# Black
t.test(subset(CPS_micro_data_as4$earnweek, 
              CPS_micro_data_as4$race2 == "Black"),
       subset(CPS_micro_data_as4$earnweek, 
              CPS_micro_data_as4$race2 == "White"),
       conf.level = 0.95)

# Hawaiian/Pacific Islander
t.test(subset(CPS_micro_data_as4$earnweek, 
              CPS_micro_data_as4$race2 == "Hawaiian/Pacific Islander"),
       subset(CPS_micro_data_as4$earnweek, 
              CPS_micro_data_as4$race2 == "White"),
       conf.level = 0.95)

# Mixed
t.test(subset(CPS_micro_data_as4$earnweek, 
              CPS_micro_data_as4$race2 == "Mixed"),
       subset(CPS_micro_data_as4$earnweek, 
              CPS_micro_data_as4$race2 == "White"),
       conf.level = 0.95)

### 8
# Asian
t.test(subset(CPS_micro_data_as4$earnweek, 
              CPS_micro_data_as4$race2 == "Asian"
              & CPS_micro_data_as4$educ3 == "high school graduate"),
       subset(CPS_micro_data_as4$earnweek, 
              CPS_micro_data_as4$race2 == "White" 
              & CPS_micro_data_as4$educ3 == "high school graduate"),
       conf.level = 0.95)

# Asian/Pacific
t.test(subset(CPS_micro_data_as4$earnweek, 
              CPS_micro_data_as4$race2 == "Asian/Pacific"
              & CPS_micro_data_as4$educ3 == "high school graduate"),
       subset(CPS_micro_data_as4$earnweek, 
              CPS_micro_data_as4$race2 == "White" 
              & CPS_micro_data_as4$educ3 == "high school graduate"),
       conf.level = 0.95)

# Black
t.test(subset(CPS_micro_data_as4$earnweek, 
              CPS_micro_data_as4$race2 == "Black"
              & CPS_micro_data_as4$educ3 == "high school graduate"),
       subset(CPS_micro_data_as4$earnweek, 
              CPS_micro_data_as4$race2 == "White" 
              & CPS_micro_data_as4$educ3 == "high school graduate"),
       conf.level = 0.95)

# Hawaiian/Pacific Islander
t.test(subset(CPS_micro_data_as4$earnweek, 
              CPS_micro_data_as4$race2 == "Hawaiian/Pacific Islander"
              & CPS_micro_data_as4$educ3 == "high school graduate"),
       subset(CPS_micro_data_as4$earnweek, 
              CPS_micro_data_as4$race2 == "White" 
              & CPS_micro_data_as4$educ3 == "high school graduate"),
       conf.level = 0.95)

# Mixed
t.test(subset(CPS_micro_data_as4$earnweek, 
              CPS_micro_data_as4$race2 == "Mixed"
              & CPS_micro_data_as4$educ3 == "high school graduate"),
       subset(CPS_micro_data_as4$earnweek, 
              CPS_micro_data_as4$race2 == "White" 
              & CPS_micro_data_as4$educ3 == "high school graduate"),
       conf.level = 0.95)

### 9
# Asian
t.test(subset(CPS_micro_data_as4$earnweek, 
              CPS_micro_data_as4$race2 == "Asian"
              & CPS_micro_data_as4$educ3 == "college graduate"),
       subset(CPS_micro_data_as4$earnweek, 
              CPS_micro_data_as4$race2 == "White" 
              & CPS_micro_data_as4$educ3 == "college graduate"),
       conf.level = 0.95)

# Asian/Pacific
t.test(subset(CPS_micro_data_as4$earnweek, 
              CPS_micro_data_as4$race2 == "Asian/Pacific"
              & CPS_micro_data_as4$educ3 == "college graduate"),
       subset(CPS_micro_data_as4$earnweek, 
              CPS_micro_data_as4$race2 == "White" 
              & CPS_micro_data_as4$educ3 == "college graduate"),
       conf.level = 0.95)

# Black
t.test(subset(CPS_micro_data_as4$earnweek, 
              CPS_micro_data_as4$race2 == "Black"
              & CPS_micro_data_as4$educ3 == "college graduate"),
       subset(CPS_micro_data_as4$earnweek, 
              CPS_micro_data_as4$race2 == "White" 
              & CPS_micro_data_as4$educ3 == "college graduate"),
       conf.level = 0.95)

# Hawaiian/Pacific Islander
t.test(subset(CPS_micro_data_as4$earnweek, 
              CPS_micro_data_as4$race2 == "Hawaiian/Pacific Islander"
              & CPS_micro_data_as4$educ3 == "college graduate"),
       subset(CPS_micro_data_as4$earnweek, 
              CPS_micro_data_as4$race2 == "White" 
              & CPS_micro_data_as4$educ3 == "college graduate"),
       conf.level = 0.95)

# Mixed
t.test(subset(CPS_micro_data_as4$earnweek, 
              CPS_micro_data_as4$race2 == "Mixed"
              & CPS_micro_data_as4$educ3 == "college graduate"),
       subset(CPS_micro_data_as4$earnweek, 
              CPS_micro_data_as4$race2 == "White" 
              & CPS_micro_data_as4$educ3 == "college graduate"),
       conf.level = 0.95)

### 10
Model1 = lm(data = CPS_micro_data_as4, earnweek ~ educ2)
Model1
ggplot(data = CPS_micro_data_as4) + 
   geom_smooth(mapping = aes(x = educ2, y = earnweek), method = lm, se = FALSE, col = "red", size = 1.5) +
   ggtitle("Regression: earning/education") + 
   xlab("Education (years)") +
   ylab("Weekly Earnings (dollar)") +
   theme(panel.background = element_rect(fill = "light blue", 
                                         colour = "light blue",
                                         size = 1,
                                         linetype = "solid"),
         panel.grid.major = element_line(size = 1.5,
                                         linetype = "solid",
                                         colour = "white"),
         panel.grid.minor = element_line(size = 1,
                                         linetype = "solid",
                                         colour = "white"))
