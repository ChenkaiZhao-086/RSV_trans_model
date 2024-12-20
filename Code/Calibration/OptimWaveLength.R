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

Dat_All$AAP <- AAP(Dat_All, 0.65)
Dat_All <- setorder(Dat_All, ID)
sum(Dat_All$AAP) # 8.726422


GetFakeWavelength <- function(x, RealLength = 8.726422, threshold = 0.65) {
  x <- max(3, min(25, x))

  FakeData <- data.table(week = 1:52, summ = dnorm(1:52, mean = 26, sd = x))
  FakeData <- FakeData[, ID := 1:.N][order(-summ)]
  FakeData$AAP <- AAP(FakeData, threshold)
  return(abs(RealLength - sum(FakeData$AAP)))
}

library(parallel)
set.seed(889)
result <- mclapply(1:1000, function(x) {
  optim(
    par = 4, fn = GetFakeWavelength, RealLength = 8.726422, threshold = 0.65,
    method = "SANN" # SANN is simulated annealing # "L-BFGS-B"
  )
},
mc.cores = 10
)

result_bind <- do.call(rbind, lapply(1:length(result), function(x) result[[x]]$par))
result_bind <- result_bind[result_bind > 4.66]
result_bind <- result_bind[result_bind < 4.67]
quantile(result_bind, prob = c(0.025, 0.5, 0.975))
# 4.077284 4.664989 4.669717

# plot(Dat_All$summ, type = "l")
# plot(dnorm(1:52, mean = 27, sd = 3.951807), type = "l")
# lines(1:52, dnorm(1:52, mean = 27, sd = 4.668714) * 10000, col = "red")
plot(density(result_bind))
