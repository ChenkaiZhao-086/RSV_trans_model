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
library(scales)
library(ggh4x)

### All resuls
# save.image(file = paste0(FilePath, "20250614.Rdata"))
load("Output/V1/20250614.Rdata")

source("Code/func.R")
source("Code/RefDat.R")
source("Code/Plot_func.r")
# source("Code/RefDat_MA.R")

sourceCpp("Code/model_cppV4.cpp")
sourceCpp("Code/model_immuV4.cpp")

Scot_Pop <- read_excel("Pop.xlsx", sheet = "Scot_Pop")
Scot_Pop <- Scot_Pop[9, -1]
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
source("Code/Immunisation/ModifyBase.R")

# Section 1 ------------------------------------------------------------------
### Find the best parameter set (by serological prevalence)
source("Code/1.Serological.r")

# Plot age-specific incidence rate
source("Code/2.IncidenceRate.r")

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
# source("Code/7.IncidenceRate_AfterVac.r")


#### For Figure 3 -----------------------------------------------------------
source("Code/8.Figure3.r")

### Age distribution for appendix
source("Code/9.AgeDis.r")

### Peak reduction for appendix
source("Code/10.PeakReduction.r")

source("Code/Cal_Rt.R")


#### I2H for children <2 years old
Calu.I2H.Combine(Posteriori_Median_S44,
  lag = FALSE,
  Age_Sus = c(.4, .4),
  Vac_start = "2019-08-30",
  Effacy_I = 0,
  Effacy_Hosp = 0,
  VacProp = .8,
  model = "SIRVV"
)
