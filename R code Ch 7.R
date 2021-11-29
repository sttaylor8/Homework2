#Assignment 2 - Chapter 7
library(fpp2)
library(seasonal)
library(forecast)
library(ggplot2)
install.packages("seasonalview")

#Question 1 - pigs
head(pigs)
str(pigs)

#1a Find optimal values
sespigs = ses(pigs, h=4)
sespigs$model

#plot 1a
autoplot(sespigs, xlab="Year", ylab = "Volume", main ="Sespigs")

#1b 95%
sespigs$upper[1, "95%"]
sespigs$lower[1, "95%"]
sd = sd(sespigs$residuals)
sespigs$mean[1] + 1.96*sd
sespigs$mean[1] - 1.96*sd

#2 write your own function to forecast
forecastpigs <- pigs %>%  
  stlf(lambda = -.57) %>%  
  forecast(h = 4) %>%  
  autoplot()

autoplot(forecastpigs)

#3 Trying to find sse for my new model.  None of this is working.
pigtrain <- window(pigs, start=1980, end=1990)

#Dont know why this results in an error.  
pigvalid <- window(pigtrain, end=c(1996))

#4 Deleted attempts.  Several errors

#forecast on training data. Note this function errors.
#Only works with pure 'pigs' data.
forecastpigs2 <- pigstrain %>%  
  stlf(lambda = -0.57) %>%  
  forecast(h = 4) %>%  
  autoplot()

#5 Books
autoplot(books)

#5b forecast books
sespaperback= ses(books[,"Paperback"], h=4)
seshardcover = ses(books[,"Hardcover"], h=4)
autoplot(sespaperback)
autoplot(seshardcover)

#5c
spaper = sqrt(mean(sespaperback$residuals^2))
spaper
shard = sqrt(mean(seshardcover$residuals^2))
shard

#6a
holtpaperback = holt(books[,"Paperback"], h=4)
holtpaperback
holthardcover = holt(books[,"Hardcover"], h=4)
holthardcover

#6b and c
holtpaperbacksqrt = sqrt(mean(holtpaperback$residuals^2))
holtpaperbacksqrt
holthardcoversqrt = sqrt(mean(holthardcover$residuals^2))
holthardcoversqrt

#6d
holtpaperback$upper[1, "95%"]
holtpaperback$lower[1, "95%"]
holtpaperback$mean[1] +1.96*holtpaperbacksqrt
holtpaperback$mean[1] -1.96*holtpaperbacksqrt

holthardcover$upper[1, "95%"]
holthardcover$lower[1, "95%"]
holthardcover$mean[1] +1.96*holthardcoversqrt
holthardcover$mean[1] -1.96*holthardcoversqrt

#7
holtegg = holt(eggs, h=100)
holteggdamp = holt(eggs, damped=TRUE, h=100)
holteggdampexp = holt(eggs, damped=TRUE, exponential = TRUE, h=100)
holteggexp = holt(eggs, damped=FALSE, exponential = TRUE, h=100)
autoplot(holtegg)
autoplot(holteggdamp)
autoplot(holteggdampexp)
autoplot(holteggexp)

#8 Did not work on retail data
#9 Did not work on retail data

#10a
autoplot(ukcars)

#10b
stlcars = stl(ukcars, s.window=4, robust=TRUE)

autoplot(stlcars)

seasonstlcars = seasadj(stlcars)

autoplot(seasonstlcars)

#10c
adddampstl = stlf(seasonstlcars, etsmodel= "AAN", damped=TRUE, h=8)

autoplot(adddampstl)

#10d
addstl = stlf(seasonstlcars, etsmodel= "AAN", damped=FALSE, h=8)

autoplot(addstl)

#10e
etscars <- ets(ukcars)
summary(etscars)

#10f
accuracy(adddampstl)

accuracy(addstl)

accuracy(etscars)

#10g - Forecast for this question
autoplot(forecast(etscars, h=8))

#10f
checkresiduals(adddampstl)

#11a
autoplot(visitors)

#11b
vistest = window(visitors, start = c(2003,4))

vistrain = window(visitors, end = c(2004, 3))

holtmult = hw(vistrain, seasonal="multiplicative", h=24)

autoplot(holtmult)

#11di
etsfc <- vistrain %>% ets() %>% forecast(h=24)
autoplot(etsfc)

#11dii
etsaddBCfc <- vistrain %>% ets(lambda = BoxCox.lambda(vistrain), additive.only=TRUE) %>% forecast(h=24)

autoplot(etsaddBCfc)

#11diii
visnaive = snaive(vistrain, h=24)

autoplot(visnaive)

#11div
BCstlets <- vistrain %>% stlm(
  lambda = BoxCox.lambda(vistrain), 
  s.window = 13, 
  robust=TRUE, 
  method="ets") %>% forecast(h=24)

autoplot(BCstlets)

#11e
accuracy(holtmult)
accuracy(etsfc)
accuracy(etsaddBCfc)
accuracy(visnaive)
accuracy(BCstlets)

#11f
tscvhultmult <- tsCV(visitors, holtmult, h = 24)
tscvetsfc <- tsCV(visitors, etsfc, h = 24)
tscvetsaddBCfc <- tsCV(visitors, etsaddBCfc, h = 24)
tscvvisnaive <- tsCV(visitors, visnaive, h = 24)
tscvBCstlets <- tsCV(visitors, BCstlets, h = 24)

mean(tscvhultmult^2, na.rm = TRUE)
mean(tscvetsfc^2, na.rm = TRUE)

#12
fets <- function(qcement, h) {
  forecast(ets(qcement), h = 4)
}

#12b
mean(fets^2, na.rm = TRUE)
