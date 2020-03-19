setwd("D:/UPenn/MSSP 897 Applied Linear Modeling/hw1")
good <- read.csv('good.csv')
View(good)

# clean NA data
vars <- c('readss97','WICpreg','AFDCpreg','AGE97','faminc97','bthwht','HOME97')
good_sub <- na.omit(good[vars])
dim(good_sub)

# 3.1 Sample Descriptives
# Binary Variables
table(good_sub$WICpreg)
table(good_sub$AFDCpreg)
table(good_sub$bthwht)

# Continuos Variables
summary(good_sub$readss97)
sd(good_sub$readss97)
summary(good_sub$AGE97)
sd(good_sub$AGE97)
summary(good_sub$faminc97)
sd(good_sub$faminc97)
summary(good_sub$HOME97)
sd(good_sub$HOME97)

da <- c(1105,740,1545,300,1197,648)
for (i in da) {
  print(round(i/1845,4))
}


# 3.2 multiple regression

wic <- lm(readss97~WICpreg+AGE97+faminc97+bthwht+HOME97, data=good_sub)
wic_summary <- summary(wic)
wic_summary

afdc <- lm(readss97~AFDCpreg+AGE97+faminc97+bthwht+HOME97, data=good_sub)
afdc_summary <- summary(afdc)
afdc_summary

wic_afdc <- lm(readss97~WICpreg+AFDCpreg+AGE97+faminc97+bthwht+HOME97, data=good_sub)
wa_sum <- summary(wic_afdc)
wa_sum

wic_scale <- lm(scale(readss97)~scale(WICpreg)+scale(AGE97)+scale(bthwht)+scale(HOME97),data=good_sub)
afdc_scale <- lm(scale(readss97)~scale(AFDCpreg)+scale(AGE97)+scale(bthwht)+scale(HOME97),data=good_sub)

# library(stargazer)
stargazer(wic,afdc,wic_afdc,wic_scale,afdc_scale,type='text',title='Table 3',digits=4, align = TRUE, no.space = TRUE)

# library(lmSupport)
modelEffectSizes(wic)
modelEffectSizes(afdc)
modelEffectSizes(wic_afdc)
# stargazer(wic_me,afdc_me,type='text',title='Table 4',digits=4,align = TRUE)

wic_scale
afdc_scale
