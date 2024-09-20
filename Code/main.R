library(httpgd)
hgd()
hgd_view()

#------------------------------------------
library(Rcpp)
library(deSolve)
library(readxl)
library(data.table)
library(tidyverse)
library(doParallel)
library(foreach)
library(progress)
library(truncnorm)
library(extraDistr)
library(coda)

source("Code/func.R")
source("Code/RefDat.R")
# source("Code/RefDat_MA.R")

sourceCpp("Code/model_cppV4.cpp")
# sourceCpp("Code/model_SEIR_cpp.cpp")

Scot_Pop <- read_excel("Pop.xlsx", sheet = "Scot_Pop")
Scot_Pop <- Scot_Pop[5, -1]
ContacrStr <- fread("ContRatePerCapita.csv")
ContacrStr <- ContacrStr[, -1] %>% as.matrix()
Scot_HospRate <- c(59.6206, 44.81968, 24.28517, 9.1871, 2.72915, 0.42175, 0.19087, 0.92575, 0.92575, 0.92575, 4.340780728)

#------------------------------------------
FilePath <- File.CreateMainFolder(path = "Output/", FolderName = "V1")

# Model calibration, load parameters and plot calibration results
source("Code/Calibration.R")

# Find the best parameter set (by serological prevalence)
source("Code/Serological.r")




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

Chain1 <- S_base[[2]]
Chain1 <- S_42[[2]]
Chain1 <- S_44[[3]]
Chain1 <- S_62[[3]]
Chain1 <- S_63[[2]]
Chain1 <- scenario_3.2[[1]]

Chain1 <- Chain1New #
MCMC.TracePlot(Chain1)

#------------------------------------------
Chain1Sim <- Model.RunSim(
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
    Age_Sus = c(0.4, 0.2)
  ),
  lag = FALSE
)

Parm$Age_Sus <- c(1, 1, 1, 1, 1, 1, 0.8, 0.8, 0.8, 0.8, 0.8)
Parm$phi <- 0
Parm$gamma <- c(rep(1 / 2, 4), rep(1 / 7, 7)) #
Chain1Sim <- Model.RunSim(
  Parm = Parm,
  lag = FALSE
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
    Age_Sus = c(0.6, 0.2), # 0.02,
  ),
  lag = FALSE, AgeDistribution = TRUE
)


Chain1Sim <- Model.RunSim(Parm = Parameter.Create(
  beta_base = 0.88, # Chain1[dim(Chain1)[1], 1], # 0.7, # 2.436346
  beta_seasonal = 11, # Chain1[dim(Chain1)[1], 2], # 12, # 1.615794
  phi = -10, # Chain1[dim(Chain1)[1], 3], # -30, # 0.5156857
  seasonal_wavelength = Chain1[dim(Chain1)[1], 4], # 25, # 0.11161094
  Hosp_rate = # c(.1, .1, .01, .01, .01, .001, .001, .001, .01, .01, .001),
    c(
      Chain1[dim(Chain1)[1], 5], Chain1[dim(Chain1)[1], 6], Chain1[dim(Chain1)[1], 7], Chain1[dim(Chain1)[1], 8],
      Chain1[dim(Chain1)[1], 9], Chain1[dim(Chain1)[1], 10], Chain1[dim(Chain1)[1], 11], Chain1[dim(Chain1)[1], 12],
      Chain1[dim(Chain1)[1], 13], Chain1[dim(Chain1)[1], 14], Chain1[dim(Chain1)[1], 15]
    ),
  Age_Sus = c(0.4, 0.2)
), lag = F)
Plot.Model(Chain1Sim, RealDat = RealDat_plot)[[1]]
Plot.Model(Chain1Sim, RealDat = RealDat_plot)[[3]]
