# sourceCpp("Code/model_immu.cpp")
sourceCpp("Code/model_immuV3.cpp")


### Base scenario
#-----------------------------------------------------------
# scenario 1, No Age_Sus, No lag, 2 year vaccination, 40% efficacy, 40% coverage
#-----------------------------------------------------------
a <- Vac.Batch(
  MCMC_Result = PosterioriCheck_Sbase, VacAgeGroup = "S2", Age_Sus = c(1, 1),
  Vac_start = "2018-08-30", Effacy_I = 0.75, Effacy_Hosp = 0.8,
  VacProp = 0.9, lag = FALSE, ModelType = "Inpatient",
  Plot = TRUE, save = F
)


# load("Output/Scenario_4.7.RData")
PosterioriCheck_S4.7 <- MCMC.PostProcess(scenario_3.2, burn_in = 0000, thin = 10)
Posteriori_Median_S4.7 <- PosterioriCheck_S4.7$Median
Posteriori_CI_S4.7 <- PosterioriCheck_S4.7$CI
Posteriori_Sample_S4.7 <- PosterioriCheck_S4.7$SampleChain


### Test code, calculate the inpatients when the vaccination is applied
#-------------------
ImmuHosp <- Model.RunSim.Immu(Parm = Parameter.Create(
  beta_base = PosterioriCheck_S4.7[["Median"]][1],
  beta_seasonal = PosterioriCheck_S4.7[["Median"]][2],
  phi = PosterioriCheck_S4.7[["Median"]][3],
  seasonal_wavelength = PosterioriCheck_S4.7[["Median"]][4],
  Hosp_rate = rep(1, 11), # PosterioriCheck_S4.7[["Median"]][5:15], #
  Age_Sus = c(1, 1),
  Vac_start = "2018-08-30",
  Effacy_I = 0.4,
  VacProp = 0.0
), lag = TRUE)


InfeRate <- Calu.InfeRate(ImmuHosp)
#                   0-2m     3-5m      6-11m      1-<2y     2-<5y     5-<19y   19-<60y     60-64y    65-69y   70-74y      75y
#                  299.6109 276.9436.  227.4204  802.4372  376.8368  500.9606  329.9347   317.309   275.7766  266.388   548.6086
Scot_HospRate <- c(59.6206, 44.81968, 24.28517, 9.1871, 2.72915, 0.42175, 0.19087, 0.19087, 0.92575, 0.92575, 4.340780728)

Prop_H2I <- Scot_HospRate / InfeRate
# 0.1989934 0.1618369 0.1067854 0.011449 0.00724226 0.0008418826 0.0005785084 0.0006015272 0.003356884 0.003475193 0.007912346

RealHospCase <- Scot_Pop * (Scot_HospRate / 1000)
# 566.9323 638.7701 807.1905 539.0531 486.7958 367.4826 553.1645 60.56477 275.3014 208.2197 1843.078
# 567      639      807       539      487      367      553       61         275    208     1843
# c(417,   322,      97,     724,     167,     126,     270,     119,     122,    125,       442)


### Test code, calculate the base inpatients when the vaccination is not applied
#-------------------
# BaseHosp <- Model.RunSim.Immu(Parm = Parameter.Create(
#   beta_base = PosterioriCheck_S4.7[["Median"]][1],
#   beta_seasonal = PosterioriCheck_S4.7[["Median"]][2],
#   phi = PosterioriCheck_S4.7[["Median"]][3],
#   seasonal_wavelength = PosterioriCheck_S4.7[["Median"]][4],
#   Hosp_rate = PosterioriCheck_S4.7[["Median"]][5:15],
#   Age_Sus = 0.02,
#   Vac_start = "2018-08-30",
#   Effacy_I = 0,
#   VacProp = 0.4
# ), lag = TRUE)

### Just run the model with the vaccination without CI
#-------------------
# Vac.Protection(
#   ModelParm = Posteriori_Median_S4.7,
#   lag = TRUE, Age_Sus = 0.02,
#   Vac_start = "2018-08-30",
#   Effacy_I = 0.4, VacProp = 0.4,
#   ModelType = "Inpatient",
#   Plot = FALSE
# )
#-------------------
# Vac.Protection(
#   ModelParm = Posteriori_Median_S4.7,
#   lag = TRUE, Age_Sus = 0.02,
#   Vac_start = "2018-08-30",
#   Effacy_I = 0.4, VacProp = 0.4,
#   ModelType = "Infection",
#   Plot = TRUE
# )

### Simulation 500 times to get the CI
#-- simulation part
# Parallel.Regist(10)
# Parallel.Import(list(
#   "CLI.Print", "Get.InitState", "Get.StateName", "Model.GetI", "Model.RunSim",
#   "Parameter.Create", "RefDat", "Scot_Pop", "ContacrStr",
#   "MCMC.PosteriorSample", "Posteriori_CI_S4.7", "Model.RunSim.Immu", "Vac.Protection",
#   "Vac.Protection.Simulation"
# ))
# PosteriorResult_S1 <- parLapply(ParallelNodesInfo[[1]], 1:500, \(x) {
#   setDTthreads(1)
#   sample <- sapply(1:15, \(n_col) MCMC.PosteriorSample(Posteriori_CI_S4.7[n_col, ]))
#
#   SimResult <- Vac.Protection(
#     ModelParm = sample,
#     lag = TRUE, Age_Sus = 0.02,
#     Vac_start = "2018-08-30",
#     Effacy_I = 0.4, VacProp = 0.4,
#     ModelType = "Inpatient",
#     Plot = FALSE
#   )
#
#   return(SimResult)
# })
# Parallel.Stop()
#--- get CI
# Vac.Posterior(PosteriorResult_S1)

### Plot the effection of vaccination
### It is now integrated into Vac.Protection() and cannot be used alone
# Vac.Plot(ModelParm = Posteriori_Median_S4.7,
#          lag = TRUE, Age_Sus = 0.02,
#          Vac_start = "2018-08-30",
#          Effacy_I = 0.9, VacProp = 0.4, save = F)


### Base scenario
#-----------------------------------------------------------
# scenario 1, No Age_Sus, No lag, 2 year vaccination, 40% efficacy, 40% coverage
#-----------------------------------------------------------
load("Output/Scenario_1.RData")
Base_scenario <- Vac.Batch(
  MCMC_Result = scenario_1, Age_Sus = 1, Vac_start = "2018-08-30",
  Effacy_I = 0.4, VacProp = 0.4, lag = FALSE, ModelType = "Inpatient", Plot = TRUE
)
#-----------------------------------------------------------
# scenario 2, No Age_Sus, lag = TRUE, 2 year vaccination, 40% efficacy, 40% coverage
#-----------------------------------------------------------
load("Output/Scenario_2.RData")
Base_scenario_2 <- Vac.Batch(
  MCMC_Result = scenario_2, Age_Sus = 1, Vac_start = "2018-08-30",
  Effacy_I = 0.4, VacProp = 0.4, lag = TRUE, ModelType = "Inpatient", Plot = TRUE
)
#-----------------------------------------------------------
# scenario 3, Age_Sus = 0.02, No lag, 2 year vaccination, 40% efficacy, 40% coverage
#-----------------------------------------------------------
load("Output/Scenario_3.8.RData")
Base_scenario_3 <- Vac.Batch(
  MCMC_Result = scenario_3.8, Age_Sus = 0.02, Vac_start = "2018-08-30",
  Effacy_I = 0.4, VacProp = 0.4, lag = FALSE, ModelType = "Inpatient", Plot = TRUE
)
#-----------------------------------------------------------
# scenario 4, Age_Sus = 0.02, lag = TRUE, 2 year vaccination, 40% efficacy, 40% coverage
#-----------------------------------------------------------
load("Output/Scenario_4.7.RData")
Base_scenario_4 <- Vac.Batch(
  MCMC_Result = scenario_4.7, Age_Sus = 0.02, Vac_start = "2018-08-30",
  Effacy_I = 0.4, VacProp = 0.4, lag = TRUE, ModelType = "Inpatient", Plot = TRUE
)

### 3 year vaccination
#-----------------------------------------------------------
# scenario 5, Age_Sus = 0.02, lag = TRUE, 3 year vaccination, 40% efficacy, 40% coverage
#-----------------------------------------------------------
load("Output/Scenario_4.7.RData")
Three_Year <- Vac.Batch(
  MCMC_Result = scenario_4.7, Age_Sus = 0.02, Vac_start = "2017-08-30",
  Effacy_I = 0.4, VacProp = 0.4, lag = TRUE, ModelType = "Inpatient", Plot = TRUE
)
### 10 year vaccination
#-----------------------------------------------------------
# scenario 6, Age_Sus = 0.02, lag = TRUE, 10 year vaccination, 40% efficacy, 40% coverage
#-----------------------------------------------------------
load("Output/Scenario_4.7.RData")
Ten_Year <- Vac.Batch(
  MCMC_Result = scenario_4.7, Age_Sus = 0.02, Vac_start = "2010-08-30",
  Effacy_I = 0.4, VacProp = 0.4, lag = TRUE, ModelType = "Inpatient", Plot = TRUE
)

# Vac.Protection(
#   ModelParm = Posteriori_Median_S4.7,
#   lag = TRUE, Age_Sus = 0.02,
#   Vac_start = "2000-08-30",
#   Effacy_I = 0.4, VacProp = 0.4,
#   ModelType = "Inpatient",
#   Plot = F
# )

### 1 year vaccination
#-----------------------------------------------------------
# scenario 7, Age_Sus = 0.02, lag = TRUE, 1 year vaccination, 40% efficacy, 40% coverage
#-----------------------------------------------------------
load("Output/Scenario_4.7.RData")
One_Year <- Vac.Batch(
  MCMC_Result = scenario_4.7, Age_Sus = 0.02, Vac_start = "2019-08-30",
  Effacy_I = 0.4, VacProp = 0.4, lag = TRUE, ModelType = "Inpatient", Plot = TRUE
)

### High efficacy & coverage
#-----------------------------------------------------------
# scenario 8, Age_Sus = 0.02, lag = TRUE, 2 year vaccination, 90% efficacy, 90% coverage
#-----------------------------------------------------------
load("Output/Scenario_4.7.RData")
High_Efficacy <- Vac.Batch(
  MCMC_Result = scenario_4.7, Age_Sus = 0.02, Vac_start = "2018-08-30",
  Effacy_I = 0.9, VacProp = 0.9, lag = TRUE, ModelType = "Inpatient", Plot = TRUE
)

### Middle efficacy & coverage
#-----------------------------------------------------------
# scenario 9, Age_Sus = 0.02, lag = TRUE, 2 year vaccination, 50% efficacy, 50% coverage
#-----------------------------------------------------------
load("Output/Scenario_4.7.RData")
Middle_Efficacy <- Vac.Batch(
  MCMC_Result = scenario_4.7, Age_Sus = 0.02, Vac_start = "2018-08-30",
  Effacy_I = 0.5, VacProp = 0.5, lag = TRUE, ModelType = "Inpatient", Plot = TRUE
)

### Low efficacy & coverage
#-----------------------------------------------------------
# scenario 10, Age_Sus = 0.02, lag = TRUE, 2 year vaccination, 10% efficacy, 10% coverage
#-----------------------------------------------------------
load("Output/Scenario_4.7.RData")
Low_Efficacy <- Vac.Batch(
  MCMC_Result = scenario_4.7, Age_Sus = 0.02, Vac_start = "2018-08-30",
  Effacy_I = 0.1, VacProp = 0.1, lag = TRUE, ModelType = "Inpatient", Plot = TRUE
)

### Infection
#-----------------------------------------------------------
# scenario 11, Age_Sus = 0.02, lag = TRUE, 2 year vaccination, 40% efficacy, 40% coverage
#-----------------------------------------------------------
load("Output/Scenario_4.7.RData")
Infection <- Vac.Batch(
  MCMC_Result = scenario_4.7, Age_Sus = 0.02, Vac_start = "2018-08-30",
  Effacy_I = 0.4, VacProp = 0.4, lag = TRUE, ModelType = "Infection", Plot = TRUE
)


### Different age susceptibility
#-----------------------------------------------------------
# scenario 12, Age_Sus = 0.4, lag = TRUE, 2 year vaccination, 40% efficacy, 40% coverage
#-----------------------------------------------------------
load("Output/scenario_4.2.RData")
Age_Susceptibility <- Vac.Batch(
  MCMC_Result = scenario_4.2, Age_Sus = 0.4, Vac_start = "2018-08-30",
  Effacy_I = 0.4, VacProp = 0.4, lag = TRUE, ModelType = "Inpatient", Plot = TRUE
)



# a <- Model.RunSim.Immu(Parm = Parameter.Create(
#   beta_base = PosterioriCheck_S4.7[["Median"]][1],
#   beta_seasonal = PosterioriCheck_S4.7[["Median"]][2],
#   phi = PosterioriCheck_S4.7[["Median"]][3],
#   seasonal_wavelength = PosterioriCheck_S4.7[["Median"]][4],
#   Hosp_rate = PosterioriCheck_S4.7[["Median"]][5:15],
#   Age_Sus = 0.02,
#   Vac_start = "2008-08-30",
#   Effacy_I = 0.5,
#   VacProp = 0.1
# ), lag = TRUE)
#
# a1 <- a[[1]][time > "2015-01-01"]
# plot(x = a1$time, y = a1$V0_G2)
#
# Plot.Model(a)
