# Vector Autocorrection Model R

install.packages("tsDyn")
library(tsDyn)
library(vars)

########################################
#JOHANSEN COINTEGRATION in R
########################################

#Calling the packages for use
library(urca)
library(forecast)
library(tidyverse)

# load the data #####################
pacman::p_load(pacman, rio, tidyverse)
df <- import("F:/master degree uk/Data Analytics and Finance - course/Dissertation/Dataset_R/USA_dataset_LN.xlsx")
head(df)

# Declare our time series variables
GDP <- ts(df$logGDP, start = c(1959,01,01), frequency = 4)
M2 <- ts(df$logM2, start = c(1959,01,01), frequency = 4)
CPI <- ts(df$logCPI, start = c(1959,01,01), frequency = 4)
IR <- ts(df$logIR, start = c(1959,01,01), frequency = 4)
Unemp <- ts(df$logUnemp, start = c(1959,01,01), frequency = 4)

# bind the variabloles into a system

dset <- cbind(GDP, M2, CPI, IR, Unemp)

# Lag selection criteria 
lagselect <- VARselect(dset, lag.max = 10, type="const")
lagselect$selection
lagselect$criteria

# based on the test above, 3 lags appeared more often thus this is the
# optimum value, therefore we use 3 - 1 = 2 (this is our lag)

# We now go for the Johansen testing(Trace)

ctest1t <- ca.jo(dset, type = "trace", ecdet = "none", spec = "longrun", K = 3)
summary(ctest1t)

# as the test r <= 1 is greater the my confidence level of 5% we accept the
# null hypothesis of at least one cointegrating relation in our system

# Now we will test again using the Johansen testing (MaxEigen)

ctest1e <- ca.jo(dset, type = "eigen", ecdet="const", K = 3)
summary(ctest1e)

####Vector Autocorrection Model R ##################################

#===========================================================================
  
#Build the VECM Model
  
Model1 <- VECM(dset, 5, r = 1, estim =("ML"))
summary(Model1)

#Diagnostic Tests

#Need to Transform VECM to VAR

Model1VAR <- vec2var(ctest1t, r = 1)

#Serial Correlation

Serial1 <- serial.test(Model1VAR, lags.pt = 10, type = "PT.asymptotic")
Serial1 # p-value > than 5% (cant reject H0 of NO AUTOCORRELATION)

#ARCH Effects

Arch1 <- arch.test(Model1VAR, lags.multi = 15, multivariate.only = TRUE)
Arch1 # Tests for heteroscedasticity

#Normality of Residuals

Norm1 <- normality.test(Model1VAR, multivariate.only = TRUE)
Norm1

#Impulse Response Functions

# this is the shock of GDP to money supply
M2irf <- irf(Model1VAR, impulse = "GDP", response = "M2", n.ahead = 10, boot = TRUE)
plot(M2irf, ylab = "M2", main = "GDP's shock to M2")

# this is the shock of money supply to GDP
GDP2irf <- irf(Model1VAR, impulse = "M2", response = "GDP", n.ahead = 10, boot = TRUE)
plot(GDP2irf, ylab = "GDP", main = "M2's shock to GDP")

# this is the shock of GDP to CPI
CPIirf <- irf(Model1VAR, impulse = "GDP", response = "CPI", n.ahead = 20, boot = TRUE)
plot(CPIirf, ylab = "CPI", main = "GDP's shock to CPI")

# THis is the shock of IR to GDP
GDPirf <- irf(Model1VAR, impulse = "IR", response = "GDP", n.ahead = 10, boot = TRUE)
plot(GDPirf, ylab = "GDP", main = "IR's shock to GDP")

# THis is the shock of M2 to IR
IRirf <- irf(Model1VAR, impulse = "M2", response = "IR", n.ahead = 10, boot = TRUE)
plot(IRirf, ylab = "IR", main = "M2's shock to IR")

# THis is the shock of CPI to Unemp
Unempirf <- irf(Model1VAR, impulse = "CPI", response = "Unemp", n.ahead = 10, boot = TRUE)
plot(IRirf, ylab = "Unemp", main = "CPI's shock to Unemp")

#Variance Decomposition

FEVD1 <- fevd(Model1VAR, n.ahead = 6)
par(mar = rep(2, 4))
plot(FEVD1)
