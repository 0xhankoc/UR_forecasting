require(rpart)
require(ggplot2)
require(forecast)
library(quantmod)


setwd("/Users/orhankoc/Documents/uw/Q1/INDE-427/project")
series_name <- "UNRATE" 
UR <- getSymbols(Symbols = series_name, src = "FRED", auto.assign = FALSE)
head(UR)

UR_ts <- ts(UR[, c("UNRATE")], start = c(1948, 1), end=c(2020,1), frequency = 12)

autoplot(UR_ts)
autoplot(diff(UR_ts))
ggAcf(diff(UR_ts))
BoxCox.lambda(UR_ts)

fit2 <- auto.arima(UR_ts, stepwise = FALSE) 
summary(fit2)
residuals(fit2)[1:3]
checkresiduals(fit2)

autoplot(fit2, which = 1:6, ncol = 3, label.size = 3)

fit2 %>% forecast(h = 45)
fit2 %>% forecast(h = 42) %>% autoplot()
