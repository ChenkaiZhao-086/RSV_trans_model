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

# save.image(file = paste0(FilePath, "20241206.Rdata"))
load("Output/V1/20241206.Rdata")

source("Code/func.R")
source("Code/RefDat.R")
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

### All resuls
save.image(file = paste0(FilePath, "20241210.Rdata"))
load("Output/V1/20241210.Rdata")
