# PriorList For 1.0 -----------------------------------------------------------------
PriorList <- list(
    c( # Chain 1
        0.4, # beta_base
        6, # beta_seasonal
        -35, # phi
        4.952299 * 7, # seasonal_wavelength
        0.15, #  0-2 months
        0.08, # 3-5 months
        0.03, # 6-11 months
        0.01, # 1-2 years
        0.002, # 2-4 years
        0.0004, # 5-19 years
        0.0002, # 20-59 years
        0.002, # 60-64 years
        0.003, # 65-69 years
        0.003, # 70-74 years
        0.003 # 75+ years
    ),
    c( # Chain 2
        0.35, # beta_base
        4, # beta_seasonal
        -35, # phi
        4.952299 * 7, # seasonal_wavelength
        0.2, # 0-2 months
        0.06, # 3-5 months
        0.05, # 6-11 months
        0.02, # 1-2 years
        0.002, # 2-4 years
        0.0005, # 5-19 years
        0.0005, # 20-59 years
        0.003, # 60-64 years
        0.003, # 65-69 years
        0.003, # 70-74 years
        0.003 # 75+ years
    ),
    c( # Chain 3
        0.3, # beta_base
        5, # beta_seasonal
        -35, # phi
        4.952299 * 7, # seasonal_wavelength
        0.25, # 0-2 months
        0.12, # 3-5 months
        0.12, # 6-11 months
        0.03, # 1-2 years
        0.004, # 2-4 years
        0.001, # 5-19 years
        0.0003, # 20-59 years
        0.002, # 60-64 years
        0.002, # 65-69 years
        0.004, # 70-74 years
        0.004 # 75+ years
    )
)


Parallel.Regist(ncores = 3, seed = 350)
Parallel.Import(list(
    "CLI.Print", "Get.InitState", "Get.StateName", "Model.GetI", "MCMC.Proposal", "Model.RunSim", "Model.RunSim.LLH",
    "Parameter.Create", "RefDat", "Scot_Pop", "ContacrStr",
    "MCMC.MH", "PriorList", "RealDatDistribution"
))
S_base <- parLapply(ParallelNodesInfo[[1]], PriorList, \(x) {
    setDTthreads(1)
    MCMC.MH(
        Prior = x, n_iterations = 100000, TargetDat = RefDat, lag = FALSE, Sus_reduce = c(1, 1),
        model = "SIR", AgeDistribution = TRUE
    )
})
save(S_base, file = "Output/base.RData")
Parallel.Stop()

# PosterioriCheck_S_base <- MCMC.PostProcess(S_base, burn_in = 0000, thin = 10)
