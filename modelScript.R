library(Quandl)
library(dplyr)
library(forecast)
library(ggplot2)
library(scales)

modelSim <- function(nPeriods, mdl){
  # function to extract the last observation from the simulation
  sim <- simulate(mdl, nsim = nPeriods, bootstrap = TRUE)
  sim[nPeriods]
}

# import and clean up data
goldPx <- Quandl("LBMA/GOLD") %>%
  select(Date, `USD (PM)`)

colnames(goldPx) <- c("date", "px")
goldPx <- arrange(goldPx, date) %>%
  mutate(ret = order_by(date, (px / lag(px) - 1))) %>%
  na.omit()

# persistence of price and return
par(mfrow = c(1, 2))
Pacf(goldPx$px)
Pacf(goldPx$ret)
par(mfrow = c(1,1))

pxModel <- auto.arima(goldPx$px, d = 1)
pxForecast <- forecast(pxModel, h = 4, fan = TRUE, bootstrap = TRUE)

fcstVec <- as.matrix(rep(4, 10000))
pxSim <- apply(fcstVec, 1, modelSim, mdl = pxModel)
pxSim <- data.frame(f = pxSim)

p1 <- nrow(filter(pxSim, f < 1221)) / nrow(pxSim)
p2 <- nrow(filter(pxSim, f >= 1221, f <= 1303)) / nrow(pxSim)
p3 <- nrow(filter(pxSim, f > 1303, f < 1374)) / nrow(pxSim)
p4 <- nrow(filter(pxSim, f >= 1374, f <= 1456)) / nrow(pxSim)
p5 <- nrow(filter(pxSim, f > 1456)) / nrow(pxSim)

cats <- c("Less than 1,221", "1,221 to 1,303", "1,303 to 1,374", 
          "1,374 to 1,456", "More than 1,456")
probs <- percent(c(p1, p2, p3, p4, p5))

summaryTbl <- tibble(cats, probs)
print(summaryTbl)
