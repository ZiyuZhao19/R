library(haven)
CPS_micro_data <- read_dta("D:/UPenn/MSSP 630 Quantitative Reasoning/CPS_micro_data.dta")
View(CPS_micro_data)
library(tidyverse)



CA <- subset(CPS_micro_data, CPS_micro_data$state == "California")
View(CA)
NV <- subset(CPS_micro_data, CPS_micro_data$state == "Nevada")
View(NV)

ggplot(CA) + aes(x = CA$chareduc) + geom_bar(stat = "count") +  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 


#education
edu <- table(CPS_micro_data$state, CPS_micro_data$educ)
View(edu)

summary(CA$educ)
summary(NV$educ)
hist(CA$educ, main = "California Education", xlab = "Educational Attainment", ylab = "Count", ylim = c(0,130000), col=c("orange"), breaks = 10)

#par(mfrow = c(2,2))
hist(CA$educ, main = "California Education", xlab = "Educational Attainment", ylab = "Count", ylim = c(0,130000), col=c("orange"), breaks = 10)
boxplot(CA$educ, range=0, xlab = "Education", horizontal = TRUE)
hist(NV$educ, main = "Nevada Education", xlab = "Educational Attainment", ylab = "Count", breaks = 5, col = c("pink"))
boxplot(NV$educ, range=0, xlab = "Education", horizontal = TRUE)

#race


ggplot(CA) + aes(x = CA$race) + geom_bar(stat = "count") + ylim(0,350000) + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + xlab("Races") + ylab("People") + ggtitle("Race in CA")
race_non_white_CA <- subset(CA, CA$race != "White")
ggplot(race_non_white_CA) + aes(x = race_non_white_CA$race) + geom_bar(stat = "count") + ylim(0,30000) + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + xlab("Races") + ylab("People") + ggtitle("Nonwhite Race in CA")




ggplot(NV) + aes(x = NV$race) + geom_bar(stat = "count") + ylim(0,60000) + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + xlab("Races") + ylab("People") + ggtitle("Race in NV")
race_non_white_NV <- subset(NV, NV$race != "White")

ggplot(race_non_white_NV) + aes(x = race_non_white_NV$race) + geom_bar(stat = "count") + ylim(0,5000) + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + xlab("Races") + ylab("People") + ggtitle("Nonwhite Race in NV")

#weekly earnings
CA_we <- c(CA$earnweek)
CA_we1 <- data.frame(na.omit(CA_we))
NV_we <- c(NV$earnweek)
NV_we1 <- data.frame(na.omit(NV_we))
summary(CA_we1)
summary(NV_we1)

par(mfrow = c(2,2))
ggplot(CA_we1) + aes(CA_we1$na.omit.CA_we.) + geom_histogram(aes(y=..density..), binwidth = 100, color="black", fill="white") + scale_x_log10() + xlim(0,4000) + geom_density(alpha=0.4, fill="orange") +
  ggtitle("Weekly Earning CA") + xlab("Earnings(USD)")
ggplot(NV_we1) + aes(NV_we1$na.omit.NV_we.) + geom_histogram(aes(y=..density..), binwidth = 100, color="black", fill="white") + geom_density(alpha=0.4, fill="pink") +
  ggtitle("Weekly Earning NV") + xlab("Earnings(USD)")

plot(density(CA_we1,NV_we1), stat = "identity")

hist(CA_we1, main = "CA Weekly Earnings", xlab = "Earnings(USD)", ylab = "People", ylim = c(0,10000), col=c("orange"), breaks = 20)

hist(NV_we1, main = "NV Weekly Earnings", xlab = "Earnings(USD)", ylab = "People", ylim = c(0,1800), col=c("pink"), breaks = 20) + density(stat = "identity")



p <- ggplot(data, aes(x = weight))
plot(density(CA_we1))
ggplot(CA_we1, aes(x = CA_we1$v1)) + geom_histogram(aes(y=..density..), binwidth = 2.5, color="black", fill="white") + geom_density(alpha=0.2, fill="#FF6666")
