source("Code/BM_PA_func.R")
Max_Iteration <- 1000
Each_Iteration <- 100
Indep_Iteration <- 5
n_chains <- 16
Temperatures <- c(1, 2, 3, 6.5, 12, 25, 50, 70, 100, 200, 300, 400, 500, 600, 800, 1000) # c(1, 2, 3, 6.5, 12, 25, 50, 70)
AdaptionStarter <- 10000
AdaptRate <- 0.25
InitialParm <- c(
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
)

Parallel.Regist(8)
Parallel.Import(list(
    "CLI.Print", "Get.InitState", "Get.StateName", "Model.GetI", "MCMC.Proposal", "Model.RunSim", "Model.RunSim.LLH",
    "Parameter.Create", "RefDat", "Scot_Pop", "ContacrStr",
    "Max_Iteration", "Each_Iteration", "Indep_Iteration", "n_chains", "Temperatures", "AdaptionStarter",
    "AdaptRate", "InitialParm"
))

aaa <- TraceWrapper(
    InitialParm = InitialParm, Max_Iteration = Max_Iteration, Each_Iteration = Each_Iteration,
    Indep_Iteration = Indep_Iteration, n_chains = n_chains, AdaptionStarter = AdaptionStarter,
    AdaptRate = AdaptRate, Temperatures = Temperatures, ContinueTrace = FALSE,
    step1 = 0.5, step2 = 5, TargetDat = RefDat, Save = F
)

Parallel.Stop()
