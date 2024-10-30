library(httpgd)
hgd()
hgd_view()

# ---------------------------------------------------------------------------
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

# save.image(file = paste0(FilePath, "20241015.Rdata"))
load("Output/V1/20241015.Rdata")

source("Code/func.R")
source("Code/RefDat.R")
# source("Code/RefDat_MA.R")

sourceCpp("Code/model_cppV4.cpp")
sourceCpp("Code/model_immuV4.cpp")

Scot_Pop <- read_excel("Pop.xlsx", sheet = "Scot_Pop")
Scot_Pop <- Scot_Pop[5, -1]
ContacrStr <- fread("ContRatePerCapita.csv")
ContacrStr <- ContacrStr[, -1] %>% as.matrix()
Scot_HospRate <- c(
  59.6206, 44.81968, 24.28517, 9.1871, 2.72915,
  0.42175, 0.19087, 0.92575, 0.92575, 0.92575, 4.340780728
)

# ---------------------------------------------------------------------------
FilePath <- File.CreateMainFolder(path = "Output/", FolderName = "V1")

# Model calibration ---------------------------------------------------------
### Model calibration, load parameters and plot calibration results
source("Code/Calibration/Calibration.r")

# Immunisation ---------------------------------------------------------------
### Run immunisation scenarios
source("Code/Immunisation/Immune.R")

# Section 1 ------------------------------------------------------------------
### Find the best parameter set (by serological prevalence)
source("Code/1.Serological.r")
save(Serologocal_CI, file = paste0(FilePath, "Serologocal_CI.Rdata"))

# Plot age-specific incidence rate
source("Code/2.IncidenceRate.r")

a1 <- Model.RunSim(Parm = Parameter.Create(
  beta_base = Posteriori_Median_Sbase[1],
  beta_seasonal = Posteriori_Median_Sbase[2],
  phi = Posteriori_Median_Sbase[3],
  seasonal_wavelength = Posteriori_Median_Sbase[4],
  Hosp_rate = rep(1, 11),
  Age_Sus = c(1, 1)
), lag = FALSE)[[2]]

a1 <- a1[time >= as.Date("2019-06-20")]
a1[, (3:13) := lapply(.SD, sum), .SDcols = 3:13]
a1_IR <- as.numeric(a1[1, 3:13]) / Scot_Pop


a2 <- Model.RunSim(Parm = Parameter.Create(
  beta_base = Posteriori_Median_S42[1],
  beta_seasonal = Posteriori_Median_S42[2],
  phi = Posteriori_Median_S42[3],
  seasonal_wavelength = Posteriori_Median_S42[4],
  Hosp_rate = rep(1, 11),
  # c(
  # 0.0986271103, 0.0457296935, 0.0298217308, 0.0097070635 * 2, 0.0015671755,
  # 0.0003507343, 0.0003733301, 0.0025458518, 0.0038241801, 0.0062840480, 0.0054163341
  # ), # rep(1, 11),
  Age_Sus = c(0.4, 0.2)
), lag = FALSE)[[2]]

a2 <- a2[time >= as.Date("2019-06-20")]
a2[, (3:13) := lapply(.SD, sum), .SDcols = 3:13]
a2_IR <- as.numeric(a2[1, 3:13]) / Scot_Pop


# Plot age-specific infection-hospitalisation ratio
source("Code/3.I2H.r")

# Section 2 ------------------------------------------------------------------
#### For Figure 2 and appendix -----------------------------------------------
### Plot proportion & net reduction in all age groups
source("Code/4.Portect_All.r")

### Plot proportion & net reduction in immunised and non-immunised groups
source("Code/6.Portect_TwoGroup.r")

### Plot Average reduction per vaccine
source("Code/5.AvertHospPerVac.r")

### Plot age-specific incidence rate after immunisation
source("Code/7.IncidenceRate_AfterVac.r")


#### For Figure 3 -----------------------------------------------------------
source("Code/8.Figure3.r")

### Age distribution for appendix
source("Code/9.AgeDis.r")

### Peak reduction for appendix
source("Code/10.PeakReduction.r")
