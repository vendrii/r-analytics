setwd("#")
dir()
library(readxl)
df = read_excel("data Aligator (2).xlsx")
str(df)
attach(df)

#-------------------------SOAL A------------------
# Binary logistic regression Session
library(readr)
# Getting freq table
table(Makanan)
# Getting unique value
unique(Makanan)
# Binning data (Reduce level)
library(tidyverse)
# Generate food in binner -> makanan_bin
df = df %>%
  mutate(makanan_bin = ifelse(Makanan == "I", 1,
         ifelse(Makanan %in% c("F","O"),0 
         ,Makanan)))

# Generate food in ordinal -> makanan_ord
df = df %>%
  mutate(makanan_ord = ifelse(Makanan == "I",1,
                              ifelse(Makanan == "F",2,
                                     ifelse(Makanan == "O",3,
                                            Makanan))))

# Generate location in ordinal -> loc_ord
df = df %>%
  mutate(loc_ord = ifelse(Danau == "A",1,
                              ifelse(Danau == "B",2,
                                     ifelse(Danau == "C",3,
                                            Danau))))
attach(df)
#Checking freq number
table(makanan_ord)
table(makanan_bin)

#Checking the plot (Histogram)
library(ggplot2)
ggplot(df, aes(Ukuran))+
  geom_histogram(aes(fill = Sex), color = "black", binwidth = 2)

#Building the Model (incase we already have the subset data then we don't need the train/test)
require(caret)
Y_bin = as.numeric(makanan_bin)
X1 = Ukuran
X2 = as.numeric(loc_ord)
X3 = Sex
logistic_model = glm (Y_bin~X1, family = binomial(), data = df)
summary(logistic_model)



#---------------------------------Soal B--------------------------
#Multinomial Logistic Regression
#install.packages("MASS")
require(nnet)
multinom_model = multinom(Y_bin~X1+X2+X3,data = df)
summary(multinom_model)

z = summary(multinom_model)$coefficients/summary(multinom_model)$standard.errors
p = (1-pnorm(abs(z),0,1))*2 #p-value
exp(coef(multinom_model))
p
#Res : Variable X2 & X3 not significant

#Continue to remove X3
multinom_model = multinom(Y_bin~X1+X2,data = df)
summary(multinom_model)

z = summary(multinom_model)$coefficients/summary(multinom_model)$standard.errors
p = (1-pnorm(abs(z),0,1))*2 #p-value
exp(coef(multinom_model))
p

#Res : Variable X2 not significant

#Continue to remove X2
multinom_model = multinom(Y_bin~X1+X3,data = df)
summary(multinom_model)

z = summary(multinom_model)$coefficients/summary(multinom_model)$standard.errors
p = (1-pnorm(abs(z),0,1))*2 #p-value
exp(coef(multinom_model))
p

#Res : Variable X3 not significant
multinom_model = multinom(Y_bin~X1,data = df)
summary(multinom_model)

z = summary(multinom_model)$coefficients/summary(multinom_model)$standard.errors
p = (1-pnorm(abs(z),0,1))*2 #p-value
exp(coef(multinom_model))
p

#Conclution : Only variable X1 that significant to model
# Ref : https://www.analyticsvidhya.com/blog/2016/02/multinomial-ordinal-logistic-regression/

#---------------------------------Soal C--------------------------
#Logistic Ordinal Regression
require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)

Y_ord = makanan_ord
#Build model
ordinal_model = polr(as.factor(Makanan)~X1+X2,X3, data=df, Hess = TRUE, method = c("logistic"))
summary(ordinal_model)

#calculating essential metrics such as 
ctable = coef(summary(ordinal_model))

#calculating p-value
p = pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable = cbind(ctable, "p-value" = p)

#Confidence Intervals
ci = confint(ordinal_model)

exp(coef(ordinal_model))

# OR and CI
exp(cbind(OR = coef(ordinal_model), ci))

