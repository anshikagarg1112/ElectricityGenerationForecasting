install.packages("zoo")
install.packages("forecast")
install.packages("tseries")
install.packages("ggplot2")
install.packages("fpp2")



library("zoo")
library("tseries")
library("forecast")
library("fpp2")
library("ggplot2")


delhi= read.csv("delhi.csv", header=TRUE, sep=",", na.strings="NA", dec=".",strip.white=TRUE) ###load the data


View(delhi)
str(delhi)
solar <- ts(delhi[,"Concentration"], start = c(2016,1), frequency = 325)   ####converting data into time series   --change  f=331 for jodh
is.ts(solar)                                                          ####checking if it isconverted into time series
plot(solar)
decomposition_solar <- decompose(solar)
plot(decomposition_solar)

Box.test((solar), lag =80, type = "Ljung")  #p-value greater than 0.05 suggests that the data are not significantly different from white noise                         
plot(solar,type='l',main='concentration')

ggAcf.solar=acf(solar,main='ACF solar',lag.max=80,ylim=c(- 0.5,1), na.action=na.pass)
pacf.solar=pacf(solar,main='PACF Solar',lag.max=100,ylim=c(-0.5,1) , na.action=na.pass)

qqnorm(solar)

##################Forecasting Models#############
#AUTO-ARIMA
BoxCox.lambda(solar)
arima0 <-auto.arima(solar,D=1,lambda=1.999924)    ### model fit
forecastedvalue <-forecast(arima0,h=120)
View(forecastedvalue)
plot(forecastedvalue)
write.table(forecastedvalue, "F:/ARIMApredictedvalue.csv", sep=" ,", row.names=FALSE)
summary(arima0)
arima0
as.numeric(forecastedvalue$mean)
res.arima0=arima0$res
squared.res.arima0=res.arima0^2
################################################################par(mfcol=c(3,1))
plot(squared.res.arima0,main='Squared Residuals')
acf.squared212=acf(squared.res.arima0,main='ACF Squared
                   Residuals',lag.max=100,ylim=c(-0.5,1))
pacf.squared212=pacf(squared.res.arima0,main='PACF Squared
                     Residuals',lag.max=100,ylim=c(-0.5,1))


checkresiduals(forecastedvalue)


##############
#SIMPLE EXPONENTIAL SMOOTHING -it is for seasonality

# Use ses() to forecast the next 120 days radiation
fc <- ses(solar, h = 120)
write.table(fc, "F:/SESpredictedvalue.csv", sep=" ,", row.names=FALSE)
# Use summary() to see the model parameters
summary(fc)
# Use autoplot() to plot the forecasts
autoplot(fc)
# Add the one-step forecasts for the training data to the plot
autoplot(fc) + autolayer(fitted(fc))


# Produce 120 forecasts of radiation using holt with trend()
fcholt <- holt(solar,h=120)
# Look at fitted model using summary()
summary(fcholt)
as.numeric(fcholt$mean)
# Plot the forecasts
autoplot(fcholt)
# Check that the residuals look like white noise
checkresiduals(fcholt)
write.table(fcholt, "F:/Holtpredictedvalue.csv", sep=" ,", row.names=FALSE)



#TBATS
# Fit a TBATS model to the solar data
tbts <- solar %>% tbats() %>% forecast(h = 120)
summary(tbts)
plot(tbts)
write.table(tbts, "F:/TBATSpredictedvalue.csv", sep=" ,", row.names=FALSE)
























#################################Jodhpur#######################
#####################################################

jodh= read.csv("jodhpur.csv", header=TRUE, sep=",", na.strings="NA", dec=".",strip.white=TRUE) ###load the data


View(jodh)
str(jodh)
solarj <- ts(jodh[,"Concentration"], start = c(2016,1), frequency = 331)   ####converting data into time series  
is.ts(solarj)                                                          ####checking if it isconverted into time series
plot(solarj)
decomposition_solarj <- decompose(solarj)
plot(decomposition_solarj)

Box.test((solarj), lag =80, type = "Ljung")  #p-value greater than 0.05 suggests that the data are not significantly different from white noise                         
autoplot(solarj,type='l',main='concentration')

ggAcf.solar=acf(solar,main='ACF solar',lag.max=80,ylim=c(- 0.5,1), na.action=na.pass)
pacf.solar=pacf(solar,main='PACF Solar',lag.max=100,ylim=c(-0.5,1) , na.action=na.pass)

qqnorm(solarj)

##################Forecasting Models#############
BoxCox.lambda(solarj)
#AUTO-ARIMA
arima0 <-auto.arima(solarj,D=1,lambda=0.48)    ### model fit
forecastedvalue <-forecast(arima0,h=120)
View(forecastedvalue)
plot(forecastedvalue)
write.table(forecastedvalue, "F:/jodhARIMApredictedvalue.csv", sep=" ,", row.names=FALSE)
summary(arima0)
arima0
as.numeric(forecastedvalue$mean)
res.arima0=arima0$res
squared.res.arima0=res.arima0^2
################################################################par(mfcol=c(3,1))
plot(squared.res.arima0,main='Squared Residuals')
acf.squared212=acf(squared.res.arima0,main='ACF Squared
                   Residuals',lag.max=100,ylim=c(-0.5,1))
pacf.squared212=pacf(squared.res.arima0,main='PACF Squared
                     Residuals',lag.max=100,ylim=c(-0.5,1))


checkresiduals(forecastedvalue)


##############
#SIMPLE EXPONENTIAL SMOOTHING -it is for seasonality

# Use ses() to forecast the next 120 days radiation
fc <- ses(solarj, h = 120)
write.table(fc, "F:/jodhSESpredictedvalue.csv", sep=" ,", row.names=FALSE)
# Use summary() to see the model parameters
summary(fc)
# Use autoplot() to plot the forecasts
autoplot(fc)
# Add the one-step forecasts for the training data to the plot
autoplot(fc) + autolayer(fitted(fc))


# Produce 120 forecasts of radiation using holt with trend()
fcholt <- holt(solarj,h=120)
# Look at fitted model using summary()
summary(fcholt)
as.numeric(fcholt$mean)
# Plot the forecasts
autoplot(fcholt)
# Check that the residuals look like white noise
checkresiduals(fcholt)
write.table(fcholt, "F:/jodhHoltpredictedvalue.csv", sep=" ,", row.names=FALSE)



#TBATS
# Fit a TBATS model to the solar data
tbts <- solarj %>% tbats() %>% forecast(h = 120)
summary(tbts)
plot(tbts)
write.table(tbts, "F:/jodhTBATSpredictedvalue.csv", sep=" ,", row.names=FALSE)



