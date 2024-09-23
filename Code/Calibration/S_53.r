# PriorList For 0.5 -----------------------------------------------------------------
PriorList <- list(
    c( # Chain 1
        0.65, # beta_base
        8, # beta_seasonal
        -35, # phi
        4.952299 * 7, # seasonal_wavelength
        0.09, #  0-2 months
        0.04, # 3-5 months
        0.03, # 6-11 months
        0.01, # 1-2 years
        0.003, # 2-4 years
        0.0002, # 5-19 years
        0.0002, # 20-59 years
        0.005, # 60-64 years
        0.005, # 65-69 years
        0.005, # 70-74 years
        0.008 # 75+ years
    ),
    c( # Chain 2
        0.75, # beta_base
        10, # beta_seasonal
        -35, # phi
        4.952299 * 7, # seasonal_wavelength
        0.08, # 0-2 months
        0.06, # 3-5 months
        0.04, # 6-11 months
        0.01, # 1-2 years
        0.002, # 2-4 years
        0.0005, # 5-19 years
        0.0003, # 20-59 years
        0.002, # 60-64 years
        0.002, # 65-69 years
        0.004, # 70-74 years
        0.006 # 75+ years
    ),
    c( # Chain 3
        0.7, # beta_base
        6, # beta_seasonal
        -35, # phi
        4.952299 * 7, # seasonal_wavelength
        0.1, # 0-2 months
        0.06, # 3-5 months
        0.05, # 6-11 months
        0.01, # 1-2 years
        0.002, # 2-4 years
        0.0005, # 5-19 years
        0.0005, # 20-59 years
        0.003, # 60-64 years
        0.003, # 65-69 years
        0.007, # 70-74 years
        0.006 # 75+ years
    )
)


Parallel.Regist(ncores = 3, seed = 350)
Parallel.Import(list(
    "CLI.Print", "Get.InitState", "Get.StateName", "Model.GetI", "MCMC.Proposal", "Model.RunSim", "Model.RunSim.LLH",
    "Parameter.Create", "RefDat", "Scot_Pop", "ContacrStr",
    "MCMC.MH", "PriorList", "RealDatDistribution"
))
S_53 <- parLapply(ParallelNodesInfo[[1]], PriorList, \(x) {
    setDTthreads(1)
    MCMC.MH(
        Prior = x, n_iterations = 100000, TargetDat = RefDat, lag = FALSE, Sus_reduce = c(0.5, 0.3),
        model = "SIR", AgeDistribution = TRUE
    )
})
save(S_53, file = "Output/S_53.RData")
Parallel.Stop()

# PosterioriCheck_S53 <- MCMC.PostProcess(S_53, burn_in = 0000, thin = 10)
