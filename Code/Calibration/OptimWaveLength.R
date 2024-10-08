Dat_All <- copy(Dat)[
  ,
  week2 := sprintf("%02d", week)
][
  ,
  week2 := paste0(Year, "-W", week2, "-1")
][
  ,
  time := ISOweek::ISOweek2date(week2)
][time >= as.Date("2017-03-31") &
  time <= as.Date("2020-03-31"), ][,
  .(summ = sum(summ)),
  by = .(week)
][, ID := 1:.N][order(-summ)]

Dat_All$AAP <- AAP(Dat_All, 0.75)
Dat_All <- setorder(Dat_All, ID)
sum(Dat_All$AAP) # 11.41071


GetFakeWavelength <- function(x, RealLength = 11.41071, threshold = 0.75) {
  x <- max(0.1, min(100, x))

  FakeData <- data.table(week = 1:52, summ = dnorm(1:52, mean = 26, sd = x))
  FakeData <- FakeData[, ID := 1:.N][order(-summ)]
  FakeData$AAP <- AAP(FakeData, threshold)
  return(abs(RealLength - sum(FakeData$AAP)))
}

set.seed(889)
optim(par = 4, fn = GetFakeWavelength, method = "SANN") # SANN is simulated annealing # "L-BFGS-B"
# 4.952299

# CAUTION: This code takes a long time to run
# set.seed(889)
# result <- replicate(1000, optim(par = 4, fn = GetFakeWavelength, method = "SANN"))
# save(result, file = "Output/OptimWaveLength.Rdata")
# quantile(do.call(rbind, result[1, ]), prob = c(0.025, 0.5, 0.975))
# 4.951590 4.951807 4.952020

# GetFakeWavelength(4.95181)
#
# plot(Dat_All$summ, type = "l")
# plot(dnorm(1:52, mean = 27, sd = 5), type = "l")
