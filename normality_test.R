# Normality test

library(haven)

# LOAD DATA
df <- import("F:/master degree uk/Data Analytics and Finance - course/Dissertation/Dataset_R/USA_dataset.xlsx")
head(df)
str(df)

install.packages("dplyr")
install.packages("ggpubr")

library(dplyr)
library(ggpubr)

# testing normality
ggdensity(df$GDP, main="GDP", xlab="US GDP")
ggdensity(df$M2, main="GDP", xlab="US GDP")

ggqqplot(df$GDP, main="GDP", xlab="US GDP")

shapiro.test(df$GDP) # since the test showed a p-value of <0.05 we assume that
# the data (GDP) is skewed. 
