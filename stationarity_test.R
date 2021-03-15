
# Load contributed packages with pacman
pacman::p_load(pacman, party, rio, tidyerse)

install.packages("tseries")
install.packages("lmtest")
library(tseries)
library(lmtest)

# Import CSV files with reader::read_csv() from tidyverse
df <- import("F:/master degree uk/Data Analytics and Finance - course/Dissertation/Dataset_R/USA_dataset_LN.xlsx")
head(df)
#series must be stationary
#Run time series regression

adf.test(df$logGDP) # This ckecks stationarity 
adf.test(df$logM2)  #  "      "        "
adf.test(df$logCPI)   #  "      "        "
adf.test(df$logIR)   #  "      "        "
adf.test(df$logUnemp) #  "      "        "

# plot data
plot(df$logGDP)

abline(reg=lm(df$GDP~time(df$GDP)))
