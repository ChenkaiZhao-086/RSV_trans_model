# sourceCpp("Code/model_immu.cpp")
sourceCpp("Code/model_immuV3.cpp")



# load("Output/Scenario_4.7.RData")
# PosterioriCheck_S4.7 <- MCMC.PostProcess(scenario_4.7, burn_in = 10000, thin = 10)
# Posteriori_Median_S4.7 <- PosterioriCheck_S4.7$Median
# Posteriori_CI_S4.7 <- PosterioriCheck_S4.7$CI
# Posteriori_Sample_S4.7 <- PosterioriCheck_S4.7$SampleChain


### Test code, calculate the inpatients when the vaccination is applied
#-------------------
# ImmuHosp <- Model.RunSim.Immu(Parm = Parameter.Create(
#   beta_base = PosterioriCheck_S4.7[["Median"]][1],
#   beta_seasonal = PosterioriCheck_S4.7[["Median"]][2],
#   phi = PosterioriCheck_S4.7[["Median"]][3],
#   seasonal_wavelength = PosterioriCheck_S4.7[["Median"]][4],
#   Hosp_rate = PosterioriCheck_S4.7[["Median"]][5:15],
#   Age_Sus = 0.02,
#   Vac_start = "2018-08-30",
#   Effacy = 0.4,
#   VacProp = 0.4
# ), lag = TRUE)
 
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
#   Effacy = 0,
#   VacProp = 0.4
# ), lag = TRUE)

### Just run the model with the vaccination without CI
#-------------------
# Vac.Protection(
#   ModelParm = Posteriori_Median_S4.7,
#   lag = TRUE, Age_Sus = 0.02,
#   Vac_start = "2018-08-30",
#   Effacy = 0.4, VacProp = 0.4,
#   ModelType = "Inpatient",
#   Plot = FALSE
# )
#-------------------
# Vac.Protection(
#   ModelParm = Posteriori_Median_S4.7,
#   lag = TRUE, Age_Sus = 0.02,
#   Vac_start = "2018-08-30",
#   Effacy = 0.4, VacProp = 0.4,
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
#     Effacy = 0.4, VacProp = 0.4,
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
#          Effacy = 0.9, VacProp = 0.4, save = F)


### Base scenario
#-----------------------------------------------------------
# scenario 1, No Age_Sus, No lag, 2 year vaccination, 40% efficacy, 40% coverage
#-----------------------------------------------------------
load("Output/Scenario_1.RData")
Base_scenario <- Vac.Batch(MCMC_Result = scenario_1, Age_Sus = 1, Vac_start = "2018-08-30", 
                           Effacy = 0.4, VacProp = 0.4, lag = FALSE, ModelType = "Inpatient", Plot = TRUE)
#-----------------------------------------------------------
# scenario 2, No Age_Sus, lag = TRUE, 2 year vaccination, 40% efficacy, 40% coverage
#-----------------------------------------------------------
load("Output/Scenario_2.RData")
Base_scenario_2 <- Vac.Batch(MCMC_Result = scenario_2, Age_Sus = 1, Vac_start = "2018-08-30", 
                           Effacy = 0.4, VacProp = 0.4, lag = TRUE, ModelType = "Inpatient", Plot = TRUE)
#-----------------------------------------------------------
# scenario 3, Age_Sus = 0.02, No lag, 2 year vaccination, 40% efficacy, 40% coverage
#-----------------------------------------------------------
load("Output/Scenario_3.8.RData")
Base_scenario_3 <- Vac.Batch(MCMC_Result = scenario_3.8, Age_Sus = 0.02, Vac_start = "2018-08-30", 
                           Effacy = 0.4, VacProp = 0.4, lag = FALSE, ModelType = "Inpatient", Plot = TRUE)
#-----------------------------------------------------------
# scenario 4, Age_Sus = 0.02, lag = TRUE, 2 year vaccination, 40% efficacy, 40% coverage
#-----------------------------------------------------------
load("Output/Scenario_4.7.RData")
Base_scenario_4 <- Vac.Batch(MCMC_Result = scenario_4.7, Age_Sus = 0.02, Vac_start = "2018-08-30", 
                           Effacy = 0.4, VacProp = 0.4, lag = TRUE, ModelType = "Inpatient", Plot = TRUE)

### 3 year vaccination
#-----------------------------------------------------------
# scenario 5, Age_Sus = 0.02, lag = TRUE, 3 year vaccination, 40% efficacy, 40% coverage
#-----------------------------------------------------------
load("Output/Scenario_4.7.RData")
Three_Year <- Vac.Batch(MCMC_Result = scenario_4.7, Age_Sus = 0.02, Vac_start = "2017-08-30", 
                             Effacy = 0.4, VacProp = 0.4, lag = TRUE, ModelType = "Inpatient", Plot = TRUE)
### 10 year vaccination
#-----------------------------------------------------------
# scenario 6, Age_Sus = 0.02, lag = TRUE, 10 year vaccination, 40% efficacy, 40% coverage
#-----------------------------------------------------------
load("Output/Scenario_4.7.RData")
Ten_Year <- Vac.Batch(MCMC_Result = scenario_4.7, Age_Sus = 0.02, Vac_start = "2010-08-30", 
                             Effacy = 0.4, VacProp = 0.4, lag = TRUE, ModelType = "Inpatient", Plot = TRUE)

# Vac.Protection(
#   ModelParm = Posteriori_Median_S4.7,
#   lag = TRUE, Age_Sus = 0.02,
#   Vac_start = "2000-08-30",
#   Effacy = 0.4, VacProp = 0.4,
#   ModelType = "Inpatient",
#   Plot = F
# )

### 1 year vaccination
#-----------------------------------------------------------
# scenario 7, Age_Sus = 0.02, lag = TRUE, 1 year vaccination, 40% efficacy, 40% coverage
#-----------------------------------------------------------
load("Output/Scenario_4.7.RData")
One_Year <- Vac.Batch(MCMC_Result = scenario_4.7, Age_Sus = 0.02, Vac_start = "2019-08-30", 
                             Effacy = 0.4, VacProp = 0.4, lag = TRUE, ModelType = "Inpatient", Plot = TRUE)

### High efficacy & coverage
#-----------------------------------------------------------
# scenario 8, Age_Sus = 0.02, lag = TRUE, 2 year vaccination, 90% efficacy, 90% coverage
#-----------------------------------------------------------
load("Output/Scenario_4.7.RData")
High_Efficacy <- Vac.Batch(MCMC_Result = scenario_4.7, Age_Sus = 0.02, Vac_start = "2018-08-30", 
                             Effacy = 0.9, VacProp = 0.9, lag = TRUE, ModelType = "Inpatient", Plot = TRUE)

### Middle efficacy & coverage
#-----------------------------------------------------------
# scenario 9, Age_Sus = 0.02, lag = TRUE, 2 year vaccination, 50% efficacy, 50% coverage
#-----------------------------------------------------------
load("Output/Scenario_4.7.RData")
Middle_Efficacy <- Vac.Batch(MCMC_Result = scenario_4.7, Age_Sus = 0.02, Vac_start = "2018-08-30", 
                             Effacy = 0.5, VacProp = 0.5, lag = TRUE, ModelType = "Inpatient", Plot = TRUE)

### Low efficacy & coverage
#-----------------------------------------------------------
# scenario 10, Age_Sus = 0.02, lag = TRUE, 2 year vaccination, 10% efficacy, 10% coverage
#-----------------------------------------------------------
load("Output/Scenario_4.7.RData")
Low_Efficacy <- Vac.Batch(MCMC_Result = scenario_4.7, Age_Sus = 0.02, Vac_start = "2018-08-30", 
                             Effacy = 0.1, VacProp = 0.1, lag = TRUE, ModelType = "Inpatient", Plot = TRUE)

### Infection
#-----------------------------------------------------------
# scenario 11, Age_Sus = 0.02, lag = TRUE, 2 year vaccination, 40% efficacy, 40% coverage
#-----------------------------------------------------------
load("Output/Scenario_4.7.RData")
Infection <- Vac.Batch(MCMC_Result = scenario_4.7, Age_Sus = 0.02, Vac_start = "2018-08-30", 
                             Effacy = 0.4, VacProp = 0.4, lag = TRUE, ModelType = "Infection", Plot = TRUE)


### Different age susceptibility
#-----------------------------------------------------------
# scenario 12, Age_Sus = 0.4, lag = TRUE, 2 year vaccination, 40% efficacy, 40% coverage
#-----------------------------------------------------------
load("Output/scenario_4.2.RData")
Age_Susceptibility <- Vac.Batch(MCMC_Result = scenario_4.2, Age_Sus = 0.4, Vac_start = "2018-08-30", 
                                Effacy = 0.4, VacProp = 0.4, lag = TRUE, ModelType = "Inpatient", Plot = TRUE)



# a <- Model.RunSim.Immu(Parm = Parameter.Create(
#   beta_base = PosterioriCheck_S4.7[["Median"]][1],
#   beta_seasonal = PosterioriCheck_S4.7[["Median"]][2],
#   phi = PosterioriCheck_S4.7[["Median"]][3],
#   seasonal_wavelength = PosterioriCheck_S4.7[["Median"]][4],
#   Hosp_rate = PosterioriCheck_S4.7[["Median"]][5:15],
#   Age_Sus = 0.02,
#   Vac_start = "2008-08-30",
#   Effacy = 0.5,
#   VacProp = 0.1
# ), lag = TRUE)
# 
# a1 <- a[[1]][time > "2015-01-01"]
# plot(x = a1$time, y = a1$V0_G2)
# 
# Plot.Model(a)

