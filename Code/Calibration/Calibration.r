set.seed(380)

lapply(paste0("Output/", list.files("Output", pattern = "\\.RData$")), load, envir = .GlobalEnv)


### scenario base -----------------------------------------------------
# source("Code/Calibration/S_base.r")
PosterioriCheck_Sbase <- MCMC.PostProcess(S_base, burn_in = 20000, thin = 10, n_sample = 1000)
Posteriori_Median_Sbase <- PosterioriCheck_Sbase$Median
Posteriori_Sample_Sbase <- PosterioriCheck_Sbase$SampleChain %>% split(., row(.))

### scenario 0.4 & 0.2 -----------------------------------------------------
# source("Code/Calibration/S_42.r")
PosterioriCheck_S42 <- MCMC.PostProcess(S_42, burn_in = 20000, thin = 10, n_sample = 1000)
Posteriori_Median_S42 <- PosterioriCheck_S42$Median
Posteriori_Sample_S42 <- PosterioriCheck_S42$SampleChain %>% split(., row(.))

### scenario 0.4 & 0.3 -----------------------------------------------------
# source("Code/Calibration/S_43.r")
PosterioriCheck_S43 <- MCMC.PostProcess(S_43, burn_in = 20000, thin = 10, n_sample = 1000)
Posteriori_Median_S43 <- PosterioriCheck_S43$Median
Posteriori_Sample_S43 <- PosterioriCheck_S43$SampleChain %>% split(., row(.))

### scenario 0.4 & 0.4 -----------------------------------------------------
# source("Code/Calibration/S_44.r")
PosterioriCheck_S44 <- MCMC.PostProcess(S_44, burn_in = 20000, thin = 10, n_sample = 1000)
Posteriori_Median_S44 <- PosterioriCheck_S44$Median
Posteriori_Sample_S44 <- PosterioriCheck_S44$SampleChain %>% split(., row(.))

### scenario 0.5 & 0.2 -----------------------------------------------------
# source("Code/Calibration/S_52.r")
PosterioriCheck_S52 <- MCMC.PostProcess(S_52, burn_in = 20000, thin = 10, n_sample = 1000)
Posteriori_Median_S52 <- PosterioriCheck_S52$Median
Posteriori_Sample_S52 <- PosterioriCheck_S52$SampleChain %>% split(., row(.))

### scenario 0.5 & 0.3 -----------------------------------------------------
# source("Code/Calibration/S_53.r")
PosterioriCheck_S53 <- MCMC.PostProcess(S_53, burn_in = 20000, thin = 10, n_sample = 1000)
Posteriori_Median_S53 <- PosterioriCheck_S53$Median
Posteriori_Sample_S53 <- PosterioriCheck_S53$SampleChain %>% split(., row(.))

### scenario 0.5 & 0.4 -----------------------------------------------------
# source("Code/Calibration/S_54.r")
PosterioriCheck_S54 <- MCMC.PostProcess(S_54, burn_in = 20000, thin = 10, n_sample = 1000)
Posteriori_Median_S54 <- PosterioriCheck_S54$Median
Posteriori_Sample_S54 <- PosterioriCheck_S54$SampleChain %>% split(., row(.))

### scenario 0.6 & 0.2 -----------------------------------------------------
# source("Code/Calibration/S_62.r")
PosterioriCheck_S62 <- MCMC.PostProcess(S_62, burn_in = 20000, thin = 10, n_sample = 1000)
Posteriori_Median_S62 <- PosterioriCheck_S62$Median
Posteriori_Sample_S62 <- PosterioriCheck_S62$SampleChain %>% split(., row(.))

### scenario 0.6 & 0.3 -----------------------------------------------------
# source("Code/Calibration/S_63.r")
PosterioriCheck_S63 <- MCMC.PostProcess(S_63, burn_in = 20000, thin = 10, n_sample = 1000)
Posteriori_Median_S63 <- PosterioriCheck_S63$Median
Posteriori_Sample_S63 <- PosterioriCheck_S63$SampleChain %>% split(., row(.))

### scenario 0.6 & 0.4 -----------------------------------------------------
# source("Code/Calibration/S_64.r")
PosterioriCheck_S64 <- MCMC.PostProcess(S_64, burn_in = 20000, thin = 10, n_sample = 1000)
Posteriori_Median_S64 <- PosterioriCheck_S64$Median
Posteriori_Sample_S64 <- PosterioriCheck_S64$SampleChain %>% split(., row(.))

# source("Code/Calibration/PlotCalibration.r")
