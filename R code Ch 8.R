#Assignment 2 - Chapter 8
library(fpp2)
library(seasonal)
library(forecast)
library(ggplot2)

#2
autoplot(ibmclose)

plot(acf(ibmclose))

plot(pacf(ibmclose))

#3
box8.3 = function(x)
{lam = BoxCox.lambda(x)
transformed = BoxCox(x, lambda = lam)
tsdisplay(transformed)

return(paste("BClam = ", round(lam, 4)))}

box8.3(usnetelec)

box8.3(usgdp)

box8.3(mcopper)

box8.3(enplanements)

box8.3(visitors)

#6
y <- ts(numeric(100))
e <- rnorm(100)
my1 = function(phi, y, e){
  for(i in 2:100)
    y[i] <- 0.6*y[i-1] + e[i]}

autoplot(y)

#7
autoplot(wmurders)

wmurders %>% ndiffs()

wmurders %>% diff() %>% ggtsdisplay()

w.ar = auto.arima(wmurders)

summary(w.ar)

checkresiduals(w.ar)

forecast(w.ar,3)

autoplot(forecast(w.ar,3))

#8a
austamod = auto.arima(austa)

checkresiduals(austamod)

autoplot(forecast(austamod))

#8b
austa011 = forecast(arima(austa, c(0,1,1)), h=10)

autoplot(austa011)

austa010 = forecast(arima(austa, c(0,1,0)), h=10)

autoplot(austa010)

#8c
austa213 = forecast(Arima(austa, c(2,1,3), include.drift = TRUE), h=10)

autoplot(austa213)

#8d
austa001 = forecast(arima(austa, c(0,0,1)), include.constant=TRUE, h=10)

autoplot(austa001)

#8e
austa021 = forecast(arima(austa, c(0,2,1)), include.constant=FALSE, h=10)

autoplot(austa021)

#9a
str(usgdp)

autoplot(usgdp)

autoplot(BoxCox(usgdp,BoxCox.lambda(usgdp)))

bc.usgdp = BoxCox.lambda(usgdp)

#9b
ar.usgdp = auto.arima(usgdp, lambda = bc.usgdp)

checkresiduals(ar.usgdp)

summary(ar.usgdp)

#9c
usgdp001 = forecast(arima(usgdp, c(0,0,1)), include.constant=TRUE, h=10)

autoplot(usgdp001)

summary(usgdp001)

usgdp213 = forecast(Arima(usgdp, c(2,1,3), include.drift = TRUE), h=10)

autoplot(usgdp213)

summary(usgdp213)

usgdp210b = forecast(Arima(usgdp, c(2,1,0), include.drift = TRUE), h=10)

autoplot(usgdp210b)

summary(usgdp210b)

#9e
autoplot(forecast(usgdp,h=10))

#9f
ets.usgdp = ets(usgdp)

summary(ets.usgdp)

checkresiduals(ets.usgdp)

autoplot(forecast(ets.usgdp,h=10))

#10a
autoplot(austourists)

ggseasonplot(austourists)

ggAcf(austourists)

ggPacf(austourists)

autoplot(diff(austourists, lag=4))

#10d
par(mfrow=c(1,2))

acf(diff(austourists,lag=4))

pacf(diff(austourists,lag=41))

#10e
auto.arima(austourists)

#11a
usmelec.12 = ma(usmelec, order = 12)

autoplot(usmelec.12)

#11b
bc.usmelec = BoxCox.lambda(usmelec)

bc.usmelec2 = BoxCox(usmelec, lambda=bc.usmelec)

autoplot(bc.usmelec2)

#11c
ndiffs(bc.usmelec2)

nsdiffs(bc.usmelec2)

#11d
plot(decompose(bc.usmelec2))

bc.us = Arima(usmelec, order = c(12, 1, 0), seasonal = c(1, 0, 0), lambda = 0)

bc.us

bc.us2 = Arima(usmelec, order = c(6, 1, 0), seasonal = c(3, 1, 0), lambda = 0)

bc.us2

summary(bc.us)

summary(bc.us2)

#11e
checkresiduals(bc.us2)


auto.us = auto.arima(usmelec, seasonal = TRUE, stepwise = FALSE, approximation = FALSE, lambda = "auto")

summary(auto.us)

checkresiduals(auto.us)

#11g
f.energy = forecast(auto.us, h=15*12)

autoplot(f.energy)
