library(Rcpp)
library(deSolve)
library(readxl)
library(data.table)
library(tidyverse)
library(doParallel)
library(foreach)
library(progress)
library(truncnorm)
library(coda)

library(httpgd)
hgd()
hgd_view()
# library(gridExtra)

source("Code/func.R")
source("Code/RefDat.R")
# source("Code/RefDat_MA.R")

sourceCpp("Code/model_cppV4.cpp")

# FilePath <- File.CreateMainFolder(path = "Outputs/")

Scot_Pop <- read_excel("Pop.xlsx", sheet = "Scot_Pop")
Scot_Pop <- Scot_Pop[5, -1]
ContacrStr <- fread("ContRatePerCapita.csv")
ContacrStr <- ContacrStr[, -1] %>% as.matrix()




#############      Chain 1
set.seed(971889)
Chain1New <- MCMC.MH(Prior = c(
  1, # beta_base
  1.5, # beta_seasonal
  0.52, # phi
  0.09, # seasonal_wavelength
  0.029, # 0-2 months
  0.029, # 3-5 months
  0.010, # 6-11 months
  0.010, # 1-2 years
  0.015, # 2-4 years
  0.015, # 5-19 years
  0.008, # 20-59 years
  0.08, # 60-64 years
  0.08, # 65-69 years
  0.08, # 70-74 years
  0.08 # 75+ years
), n_iterations = 30, TargetDat = RefDat, lag = FALSE, Sus_reduce = 0.02)
# save(Chain1, file = "Chain1.Rdata")
load("Chain1.Rdata")

Chain1 <- scenario_1[[1]]
Chain1 <- scenario_4.1[[1]]
Chain1 <- scenario_4.2[[1]]
Chain1 <- scenario_4.7[[1]]
Chain1 <- Chain1New #
MCMC.TracePlot(Chain1)

# 0.01945017 0.8300863 0.519742 0.07957287 0.03216404 0.01938432 0.01808235 0.007480558 0.001561427 0.0002265782 0.0001592134 0.000479665 0.0006534253 0.0007193454 0.0005689789
Chain1Sim <- Model.RunSim(
  Parm = Parameter.Create(
    beta_base = Chain1[dim(Chain1)[1], 1],
    beta_seasonal = Chain1[dim(Chain1)[1], 2],
    phi = -30, # Chain1[dim(Chain1)[1], 3],
    seasonal_wavelength = Chain1[dim(Chain1)[1], 4],
    Hosp_rate = c(
      Chain1[dim(Chain1)[1], 5], Chain1[dim(Chain1)[1], 6], Chain1[dim(Chain1)[1], 7], Chain1[dim(Chain1)[1], 8],
      Chain1[dim(Chain1)[1], 9], Chain1[dim(Chain1)[1], 10], Chain1[dim(Chain1)[1], 11], Chain1[dim(Chain1)[1], 12],
      Chain1[dim(Chain1)[1], 13], Chain1[dim(Chain1)[1], 14], Chain1[dim(Chain1)[1], 15]
    ),
    Age_Sus = 0.4 # 1
  ),
  lag = TRUE # FALSE
)

Model.RunSim.LLH(
  Parm = Parameter.Create(
    beta_base = Chain1[dim(Chain1)[1], 1],
    beta_seasonal = Chain1[dim(Chain1)[1], 2],
    phi = Chain1[dim(Chain1)[1], 3],
    seasonal_wavelength = Chain1[dim(Chain1)[1], 4],
    Hosp_rate = c(
      Chain1[dim(Chain1)[1], 5], Chain1[dim(Chain1)[1], 6], Chain1[dim(Chain1)[1], 7], Chain1[dim(Chain1)[1], 8],
      Chain1[dim(Chain1)[1], 9], Chain1[dim(Chain1)[1], 10], Chain1[dim(Chain1)[1], 11], Chain1[dim(Chain1)[1], 12],
      Chain1[dim(Chain1)[1], 13], Chain1[dim(Chain1)[1], 14], Chain1[dim(Chain1)[1], 15]
    ),
    Age_Sus = 1, # 0.02,
  ),
  lag = FALSE
)


Chain1Sim <- Model.RunSim(Parm = Parameter.Create(
  beta_base = Chain1[dim(Chain1)[1], 1], # 0.7, # 2.436346
  beta_seasonal = 5.5, # 5.5,# 6.711163, # Chain1[dim(Chain1)[1], 2], # 12, # 1.615794
  phi = Chain1[dim(Chain1)[1], 3], # -30, # 0.5156857
  seasonal_wavelength = 15, # Chain1[dim(Chain1)[1], 4], # 25, # 0.11161094
  Hosp_rate = # c(.1, .1, .01, .01, .01, .001, .001, .001, .01, .01, .001),
    c(
      Chain1[dim(Chain1)[1], 5], Chain1[dim(Chain1)[1], 6], Chain1[dim(Chain1)[1], 7], Chain1[dim(Chain1)[1], 8],
      Chain1[dim(Chain1)[1], 9], Chain1[dim(Chain1)[1], 10], Chain1[dim(Chain1)[1], 11], Chain1[dim(Chain1)[1], 12],
      Chain1[dim(Chain1)[1], 13], Chain1[dim(Chain1)[1], 14], Chain1[dim(Chain1)[1], 15]
    ),
  Age_Sus = 0.02
), lag = T)
Plot.Model(Chain1Sim, RealDat = RealDat_plot)[[1]]
Plot.Model(Chain1Sim, RealDat = RealDat_plot)[[3]]


#############      Chain 2
set.seed(225841)
Chain2New <- MCMC.MH(Prior = c(
  0.05, # beta_base
  0.5, # beta_seasonal
  0.47, # phi
  0.04, # seasonal_wavelength
  0.04, # 0-2 months
  0.04, # 3-5 months
  0.04, # 6-11 months
  0.04, # 1-2 years
  0.02, # 2-4 years
  0.02, # 5-19 years
  0.02, # 20-59 years
  0.02, # 60-64 years
  0.001, # 65-69 years
  0.001, # 70-74 years
  0.001, # 75+ years,
  0.2
), n_iterations = 10000, TargetDat = RefDat)
save(Chain2, file = "Chain2.Rdata")
load("Chain2.Rdata")

Chain2 <- Chain2New[[1]]
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
  ),
  Age_Sus = Chain2[dim(Chain2)[1], 16]
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
  ),
  Age_Sus = Chain2[dim(Chain2)[1], 16]
))

Plot.Model(Chain2Sim, RealDat = RealDat_plot)[[3]]






# # 平行退火
# load("Chain1_PT.Rdata")
# aaaTest <- Chain1_PT[[1]][, 2:16]

# c <- Model.RunSim(Parm = Parameter.Create(
#   beta_base = aaaTest[dim(aaaTest)[1], 1],
#   beta_seasonal = aaaTest[dim(aaaTest)[1], 2],
#   phi = aaaTest[dim(aaaTest)[1], 3],
#   seasonal_wavelength = aaaTest[dim(aaaTest)[1], 4],
#   Hosp_rate = c(
#     aaaTest[dim(aaaTest)[1], 5], aaaTest[dim(aaaTest)[1], 6], aaaTest[dim(aaaTest)[1], 7], aaaTest[dim(aaaTest)[1], 8], aaaTest[dim(aaaTest)[1], 9],
#     aaaTest[dim(aaaTest)[1], 10], aaaTest[dim(aaaTest)[1], 11], aaaTest[dim(aaaTest)[1], 12], aaaTest[dim(aaaTest)[1], 13], aaaTest[dim(aaaTest)[1], 14],
#     aaaTest[dim(aaaTest)[1], 15]
#   ),
#   Age_Sus = 0.5
# ))
# MCMC.TracePlot(aaaTest)
# Plot.Model(c, RealDat = RealDat_plot)[3]
