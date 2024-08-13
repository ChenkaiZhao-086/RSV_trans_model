Dat_All <- copy(Dat)[, week2 := sprintf("%02d", week)
                     ][, week2 := paste0(Year, "-W", week2, "-1")
                       ][, time := ISOweek::ISOweek2date(week2)
                         ][time >= as.Date("2017-03-31") & time <= as.Date("2020-03-31"), 
                           ][, .(summ = sum(summ)), by = .(week)
                             ][, ID:= 1:.N
                               ][order(-summ)]

Dat_All$AAP <- AAP(Dat_All, 0.75)

Dat_All <- setorder(Dat_All, ID)

sum(Dat_All$AAP) # 11.41071


# FakeData <- data.table(week = 1:52, summ = dnorm(1:52, mean = 26, sd = 5.3))
# FakeData <- FakeData[, ID := 1:.N][order(-summ)]
# FakeData$AAP <- AAP(FakeData, 0.75)
# FakeData <- setorder(FakeData, ID)
# FakeData


GetFakeWavelength <- function(x, RealLength = 11.41071, threshold = 0.75){
  FakeData <- data.table(week = 1:52, summ = dnorm(1:52, mean = 26, sd = x))
  FakeData <- FakeData[, ID := 1:.N][order(-summ)]
  FakeData$AAP <- AAP(FakeData, threshold)
  return(abs(RealLength - sum(FakeData$AAP)))
}

set.seed(889)
optim(par = 4, fn = GetFakeWavelength, method = "SANN") # SANN is simulated annealing # "L-BFGS-B"
# 4.952299

# GetFakeWavelength(4.95181)
# 
# plot(Dat_All$summ, type = "l")
# plot(dnorm(1:52, mean = 27, sd = 5), type = "l")
