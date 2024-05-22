library(Rcpp)
library(deSolve)
library(readxl)
library(data.table)
library(tidyverse)
library(doParallel)
library(foreach)
library(progress)
library(truncnorm)

library(httpgd)
hgd()
hgd_view()
# library(gridExtra)

source("Code/func.R")
source("Code/RefDat.R")

sourceCpp("Code/model_cppV4.cpp")

# FilePath <- File.CreateMainFolder(path = "Outputs/")

Scot_Pop <- read_excel("Pop.xlsx", sheet = "Scot_Pop")
Scot_Pop <- Scot_Pop[5, -1]
ContacrStr <- fread("ContRatePerCapita.csv")
ContacrStr <- ContacrStr[, -1] %>% as.matrix()




#############      Chain 1
set.seed(971889)
Chain1 <- MCMC.MH(Prior = c(
  0.02, # beta_base
  0.95, # beta_seasonal
  0.52, # phi
  0.09, # seasonal_wavelength
  0.029, # 0-2 months
  0.029, # 3-5 months
  0.010, # 6-11 months
  0.010, # 1-2 years
  0.015, # 2-4 years
  0.015, # 5-19 years
  0.015, # 20-59 years
  0.015, # 60-64 years
  0.08, # 65-69 years
  0.08, # 70-74 years
  0.08 # 75+ years
), n_iterations = 10000, TargetDat = RefDat)
# save(Chain1, file = "Chain1.Rdata")
load("Chain1.Rdata")

MCMC.TracePlot(Chain1)

# 0.2607927 0.9764876    1 0.0400834 0.02220915 0.02185441 0.008891273 0.001275039 0.0002041103 0.0001623441 0.0006550445 0.0007986619 0.0008169862 0.0007346218
Chain1Sim <- Model.RunSim(Parm = Parameter.Create(
  beta_base = Chain1[dim(Chain1)[1], 1], # 0.01, #
  beta_seasonal = Chain1[dim(Chain1)[1], 2], # 2, #
  phi = Chain1[dim(Chain1)[1], 3], # 0.6, #
  seasonal_wavelength = Chain1[dim(Chain1)[1], 4], # 0.2024982, #
  omega_m = 28,
  Hosp_rate = c(
    Chain1[dim(Chain1)[1], 5], Chain1[dim(Chain1)[1], 6], Chain1[dim(Chain1)[1], 7], Chain1[dim(Chain1)[1], 8],
    Chain1[dim(Chain1)[1], 9], Chain1[dim(Chain1)[1], 10], Chain1[dim(Chain1)[1], 11], Chain1[dim(Chain1)[1], 12],
    Chain1[dim(Chain1)[1], 13], Chain1[dim(Chain1)[1], 14], Chain1[dim(Chain1)[1], 15]
  ),
  Age_Sus = c(
    1, # 0-2 months
    1, # 3-5 months
    1, # 6-11 months
    1, # 1-2 years
    1, # 2-4 years
    1, # 5-19 years
    0.5, # 20-59 years
    0.5, # 60-64 years
    0.5, # 65-69 years
    0.5, # 70-74 years
    0.5 # 75+ years)
  )
))

Model.RunSim.LLH(Parm = Parameter.Create(
  beta_base = Chain1[dim(Chain1)[1], 1],
  beta_seasonal = Chain1[dim(Chain1)[1], 2],
  phi = Chain1[dim(Chain1)[1], 3],
  seasonal_wavelength = Chain1[dim(Chain1)[1], 4],
  Hosp_rate = c(
    Chain1[dim(Chain1)[1], 5], Chain1[dim(Chain1)[1], 6], Chain1[dim(Chain1)[1], 7], Chain1[dim(Chain1)[1], 8],
    Chain1[dim(Chain1)[1], 9], Chain1[dim(Chain1)[1], 10], Chain1[dim(Chain1)[1], 11], Chain1[dim(Chain1)[1], 12],
    Chain1[dim(Chain1)[1], 13], Chain1[dim(Chain1)[1], 14], Chain1[dim(Chain1)[1], 15]
  )
))

Plot.Model(Chain1Sim, RealDat = RealDat_plot)[[3]]


#############      Chain 2
set.seed(225841)
Chain2 <- MCMC.MH(Prior = c(
  0.05, # beta_base
  0.25, # beta_seasonal
  0.47, # phi
  0.04, # seasonal_wavelength
  0.029, # 0-2 months
  0.029, # 3-5 months
  0.010, # 6-11 months
  0.010, # 1-2 years
  0.015, # 2-4 years
  0.015, # 5-19 years
  0.015, # 20-59 years
  0.015, # 60-64 years
  0.008, # 65-69 years
  0.008, # 70-74 years
  0.008 # 75+ years
), n_iterations = 10000, TargetDat = RefDat)
save(Chain2, file = "Chain2.Rdata")

MCMC.TracePlot(Chain2)

Chain2Sim <- Model.RunSim(Parm = Parameter.Create(
  beta_base = Chain2[dim(Chain2)[1], 1],
  beta_seasonal = Chain2[dim(Chain2)[1], 2],
  phi = Chain2[dim(Chain2)[1], 3],
  seasonal_wavelength = Chain2[dim(Chain2)[1], 4],
  Hosp_rate = c(
    Chain2[dim(Chain2)[1], 5], Chain2[dim(Chain2)[1], 6], Chain2[dim(Chain2)[1], 7], Chain2[dim(Chain2)[1], 8],
    Chain2[dim(Chain2)[1], 9], Chain2[dim(Chain2)[1], 10], Chain2[dim(Chain2)[1], 11], Chain2[dim(Chain2)[1], 12],
    Chain2[dim(Chain2)[1], 13], Chain2[dim(Chain2)[1], 14], Chain2[dim(Chain2)[1], 15]
  )
))

Model.RunSim.LLH(Parm = Parameter.Create(
  beta_base = Chain2[dim(Chain2)[1], 1],
  beta_seasonal = Chain2[dim(Chain2)[1], 2],
  phi = Chain2[dim(Chain2)[1], 3],
  seasonal_wavelength = Chain2[dim(Chain2)[1], 4],
  Hosp_rate = c(
    Chain2[dim(Chain2)[1], 5], Chain2[dim(Chain2)[1], 6], Chain2[dim(Chain2)[1], 7], Chain2[dim(Chain2)[1], 8],
    Chain2[dim(Chain2)[1], 9], Chain2[dim(Chain2)[1], 10], Chain2[dim(Chain2)[1], 11], Chain2[dim(Chain2)[1], 12],
    Chain2[dim(Chain2)[1], 13], Chain2[dim(Chain2)[1], 14], Chain2[dim(Chain2)[1], 15]
  )
))

Plot.Model(Chain2Sim, RealDat = RealDat_plot)[[3]]






# 平行退火
load("Chain1_PT.Rdata")
aaaTest <- Chain1_PT[[1]][, 2:16]

c <- Model.RunSim(Parm = Parameter.Create(
  beta_base = aaaTest[dim(aaaTest)[1], 1],
  beta_seasonal = aaaTest[dim(aaaTest)[1], 2],
  phi = aaaTest[dim(aaaTest)[1], 3],
  seasonal_wavelength = aaaTest[dim(aaaTest)[1], 4],
  Hosp_rate = c(
    aaaTest[dim(aaaTest)[1], 5], aaaTest[dim(aaaTest)[1], 6], aaaTest[dim(aaaTest)[1], 7], aaaTest[dim(aaaTest)[1], 8], aaaTest[dim(aaaTest)[1], 9],
    aaaTest[dim(aaaTest)[1], 10], aaaTest[dim(aaaTest)[1], 11], aaaTest[dim(aaaTest)[1], 12], aaaTest[dim(aaaTest)[1], 13], aaaTest[dim(aaaTest)[1], 14],
    aaaTest[dim(aaaTest)[1], 15]
  ),
  Age_Sus = c(
    1, # 0-2 months
    1, # 3-5 months
    1, # 6-11 months
    1, # 1-2 years
    1, # 2-4 years
    1, # 5-19 years
    0.5, # 20-59 years
    0.5, # 60-64 years
    0.5, # 65-69 years
    0.5, # 70-74 years
    0.5 # 75+ years)
  )
))
MCMC.TracePlot(aaaTest)
Plot.Model(c, RealDat = RealDat_plot)[3]








plot(1:100, dnorm(1:100, 20, 10))
plot(1:1000, rtruncnorm(1000, a = 0, mean = 0.2, sd = 0.002))
