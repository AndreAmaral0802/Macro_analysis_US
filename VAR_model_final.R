# VAR model

library(urca)
library(vars)
library(mFilter)
install.packages("mFilter")
library(tseries)
library(forecast)
library(tidyverse)

pacman::p_load(pacman, rio, tidyverse)
# Load the dataset
df <- import("diff_us_data.csv",header=TRUE)
head(df)

# simple graf
ggplot(data = df) + geom_point(mapping = aes(x = M2, y = GDP))
# M2 and CPI
ggplot(data = df) + geom_point(mapping = aes(x = M2, y = CPI))

# GDP and Unemp
ggplot(data = df) + geom_point(mapping = aes(x = Unemp, y = GDP))

# plotting scatter plot
# plot data
plot(df$GDP)
abline(reg=lm(df$GDP~time(df$GDP)))

plot(df$M2)
abline(reg=lm(df$M2~time(df$M2)))

plot(df$CPI)
abline(reg=lm(df$CPI~time(df$CPI)))

plot(df$IR)
abline(reg=lm(df$IR~time(df$IR)))

plot(df$Unemp)
abline(reg=lm(df$Unemp~time(df$Unemp)))

# Declare our time series variables
GDP <- ts(df$GDP, start = c(1959,01,01), frequency = 4)
M2 <- ts(df$M2, start = c(1959,01,01), frequency = 4)
CPI <- ts(df$CPI, start = c(1959,01,01), frequency = 4)
IR <- ts(df$IR, start = c(1959,01,01), frequency = 4)
Unemp <- ts(df$Unemp, start = c(1959,01,01), frequency = 4)

#plot the series
autoplot(cbind(GDP,M2,CPI,IR,Unemp))

#OLS
OLS_model <- lm(GDP ~ M2 + CPI + IR + Unemp)
summary(OLS_model)
# The result can be spurious because the concept of VAR is not to set an 
# structure, or in other words, GDP can affect M2 and CPI and vice versa

# Determine the persistance of the model

acf(GDP, main = "ACF for nominal GDP")
pacf(GDP, main = "PACF for nominal GDP")

acf(M2, main = "ACF for M2")
pacf(M2, main = "PACF for M2")

acf(IR, main = "ACF for IR")
pacf(IR, main = "PACF for IR")

acf(Unemp, main = "ACF for Unemp")
pacf(Unemp, main = "PACF for Unemp")

#series must be stationary
#Run time series regression

adf.test(df$GDP) # This ckecks stationarity 
adf.test(df$M2)  #  "      "        "
adf.test(df$IR)   #  "      "        "
adf.test(df$Unemp) #  "      "        "

# finding the optimum lags

dfset <- cbind(GDP, M2,CPI, IR, Unemp)

# Lag selection
lags_sel <- VARselect(dfset, lag.max = 10, type="const")
lags_sel$selection

#building the VAR model
VAR_model <- VAR(dfset, p = 5, type = "const", exog = NULL)
summary(VAR_model)

# Test for autocorrelation
auto_correlation <- serial.test(VAR_model, lags.pt = 10, type = "PT.asymptotic")
auto_correlation  

# Test for Heteroscedasticity
Heteros_test <- arch.test(VAR_model, lags.multi = 10, multivariate.only = FALSE)
Heteros_test  # as p-value is greater than 0.05 there is no heteroscedasticity

#Normality of Residuals

Normality_test <- normality.test(VAR_model, multivariate.only = TRUE)
Normality_test  # as the p values for JB-test, Kurtosis and skewness are <0.05 our 
autoplot(Normality_test)  # fail the normality test

# testing for structural breaks in the residuals 

Struc_breaks <- stability(VAR_model, type = "OLS-CUSUM")
par(mar = rep(2, 4))
plot(Struc_breaks)

# Granger Causality Test

granger_GDP <- causality(VAR_model, cause = "GDP")
granger_GDP

granger_M2 <- causality(VAR_model, cause = "M2")
granger_M2

granger_CPI <- causality(VAR_model, cause = "CPI")
granger_CPI

granger_IR <- causality(VAR_model, cause = "IR")
granger_IR

granger_Unemp <- causality(VAR_model, cause = "Unemp")
granger_Unemp

# Impulse response functions

GDPirf <- irf(VAR_model, impulse = "M2", response = "GDP", n.ahead = 10, boot = TRUE)
plot(GDPirf, ylab = "GDP", main = "Shock in Money Supply to GDP")
# This is the shock of M2 to GDP

GDPirf1 <- irf(VAR_model, impulse = "IR", response = "GDP", n.ahead = 10, boot = TRUE)
plot(GDPirf1, ylab = "GDP", main = "Shock in Interest Rate to GDP")
# The impact of IR to GDP is positive

Unempirf <- irf(VAR_model, impulse = "CPI", response = "Unemp", n.ahead = 10, boot = TRUE)
plot(Unempirf, ylab = "Unemp", main = "Shock in CPI to Unemployment Rate")

Unempirf1 <- irf(VAR_model, impulse = "IR", response = "Unemp", n.ahead = 12, boot = TRUE)
plot(Unempirf1, ylab = "Unemp", main = "Shock in Interest Rate to Unemployment Rate")

# Variance Decomposition - measures how much these variables are influenced 
# by the shocks
FEVD1 <- fevd(VAR_model, n.ahead = 3)
par(mar = rep(2, 4))
plot(FEVD1)

# VAR forecast
forecast <- predict(VAR_model, n.ahead = 4, ci = 0.95)
fanchart(forecast, names = "GDP")

forecast <- predict(VAR_model, n.ahead = 4, ci = 0.95)
fanchart(forecast, names = "M2")

forecast <- predict(VAR_model, n.ahead = 4, ci = 0.95)
fanchart(forecast, names = "IR")

forecast <- predict(VAR_model, n.ahead = 4, ci = 0.95)
fanchart(forecast, names = "Unemp")
