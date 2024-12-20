### Get wawe length parameter ----------------------------------------
# source("Code/Calibration/OptimWaveLength.R")

### Import data -----------------------------------------------------
set.seed(380)
lapply(paste0("Output/", list.files("Output", pattern = "\\.RData$")), load, envir = .GlobalEnv)

SF_Calibration <- File.CreateSubFolder(FilePath, "Calibration")

### scenario base -----------------------------------------------------
# source("Code/Calibration/S_base.r")
PosterioriCheck_Sbase <- MCMC.PostProcess(S_base, burn_in = 20000, thin = 10, n_sample = 1000)
Posteriori_Median_Sbase <- PosterioriCheck_Sbase$Median
Posteriori_Sample_Sbase <- PosterioriCheck_Sbase$SampleChain %>% split(., row(.))
MCMC.TracePlot(S_base, paste0(SF_Calibration, "Sbase_"))

### scenario 0.4 & 0.2 -----------------------------------------------------
# source("Code/Calibration/S_42.r")
PosterioriCheck_S42 <- MCMC.PostProcess(S_42, burn_in = 20000, thin = 10, n_sample = 1000)
Posteriori_Median_S42 <- PosterioriCheck_S42$Median
Posteriori_Sample_S42 <- PosterioriCheck_S42$SampleChain %>% split(., row(.))
MCMC.TracePlot(S_42, paste0(SF_Calibration, "S42_"))

### scenario 0.4 & 0.3 -----------------------------------------------------
# source("Code/Calibration/S_43.r")
PosterioriCheck_S43 <- MCMC.PostProcess(S_43, burn_in = 20000, thin = 10, n_sample = 1000)
Posteriori_Median_S43 <- PosterioriCheck_S43$Median
Posteriori_Sample_S43 <- PosterioriCheck_S43$SampleChain %>% split(., row(.))
MCMC.TracePlot(S_43, paste0(SF_Calibration, "S43_"))

### scenario 0.4 & 0.4 -----------------------------------------------------
# source("Code/Calibration/S_44.r")
PosterioriCheck_S44 <- MCMC.PostProcess(S_44, burn_in = 20000, thin = 10, n_sample = 1000)
Posteriori_Median_S44 <- PosterioriCheck_S44$Median
Posteriori_Sample_S44 <- PosterioriCheck_S44$SampleChain %>% split(., row(.))
MCMC.TracePlot(S_44, paste0(SF_Calibration, "S44_"))

### scenario 0.5 & 0.2 -----------------------------------------------------
# source("Code/Calibration/S_52.r")
PosterioriCheck_S52 <- MCMC.PostProcess(S_52, burn_in = 20000, thin = 10, n_sample = 1000)
Posteriori_Median_S52 <- PosterioriCheck_S52$Median
Posteriori_Sample_S52 <- PosterioriCheck_S52$SampleChain %>% split(., row(.))
MCMC.TracePlot(S_52, paste0(SF_Calibration, "S52_"))

### scenario 0.5 & 0.3 -----------------------------------------------------
# source("Code/Calibration/S_53.r")
PosterioriCheck_S53 <- MCMC.PostProcess(S_53, burn_in = 20000, thin = 10, n_sample = 1000)
Posteriori_Median_S53 <- PosterioriCheck_S53$Median
Posteriori_Sample_S53 <- PosterioriCheck_S53$SampleChain %>% split(., row(.))
MCMC.TracePlot(S_53, paste0(SF_Calibration, "S53_"))

### scenario 0.5 & 0.4 -----------------------------------------------------
# source("Code/Calibration/S_54.r")
PosterioriCheck_S54 <- MCMC.PostProcess(S_54, burn_in = 20000, thin = 10, n_sample = 1000)
Posteriori_Median_S54 <- PosterioriCheck_S54$Median
Posteriori_Sample_S54 <- PosterioriCheck_S54$SampleChain %>% split(., row(.))
MCMC.TracePlot(S_54, paste0(SF_Calibration, "S54_"))

### scenario 0.6 & 0.2 -----------------------------------------------------
# source("Code/Calibration/S_62.r")
PosterioriCheck_S62 <- MCMC.PostProcess(S_62, burn_in = 20000, thin = 10, n_sample = 1000)
Posteriori_Median_S62 <- PosterioriCheck_S62$Median
Posteriori_Sample_S62 <- PosterioriCheck_S62$SampleChain %>% split(., row(.))
MCMC.TracePlot(S_62, paste0(SF_Calibration, "S62_"))

### scenario 0.6 & 0.3 -----------------------------------------------------
# source("Code/Calibration/S_63.r")
PosterioriCheck_S63 <- MCMC.PostProcess(S_63, burn_in = 20000, thin = 10, n_sample = 1000)
Posteriori_Median_S63 <- PosterioriCheck_S63$Median
Posteriori_Sample_S63 <- PosterioriCheck_S63$SampleChain %>% split(., row(.))
MCMC.TracePlot(S_63, paste0(SF_Calibration, "S63_"))

### scenario 0.6 & 0.4 -----------------------------------------------------
# source("Code/Calibration/S_64.r")
PosterioriCheck_S64 <- MCMC.PostProcess(S_64, burn_in = 20000, thin = 10, n_sample = 1000)
Posteriori_Median_S64 <- PosterioriCheck_S64$Median
Posteriori_Sample_S64 <- PosterioriCheck_S64$SampleChain %>% split(., row(.))
MCMC.TracePlot(S_64, paste0(SF_Calibration, "S64_"))

# source("Code/Calibration/PlotCalibration.r")

a <- signif(cbind(
    PosterioriCheck_Sbase$Median,
    PosterioriCheck_Sbase$CI,
    PosterioriCheck_S44$Median,
    PosterioriCheck_S44$CI,
    PosterioriCheck_S43$Median,
    PosterioriCheck_S43$CI,
    PosterioriCheck_S42$Median,
    PosterioriCheck_S42$CI,
    PosterioriCheck_S54$Median,
    PosterioriCheck_S54$CI,
    PosterioriCheck_S53$Median,
    PosterioriCheck_S53$CI,
    PosterioriCheck_S52$Median,
    PosterioriCheck_S52$CI,
    PosterioriCheck_S64$Median,
    PosterioriCheck_S64$CI,
    PosterioriCheck_S63$Median,
    PosterioriCheck_S63$CI,
    PosterioriCheck_S62$Median,
    PosterioriCheck_S62$CI
), digits = 3)

write.csv(
    cbind(
        paste0(a[, 1], " (", a[, 2], ", ", a[, 3], ")"),
        paste0(a[, 4], " (", a[, 5], ", ", a[, 6], ")"),
        paste0(a[, 7], " (", a[, 8], ", ", a[, 9], ")"),
        paste0(a[, 10], " (", a[, 11], ", ", a[, 12], ")"),
        paste0(a[, 13], " (", a[, 14], ", ", a[, 15], ")"),
        paste0(a[, 16], " (", a[, 17], ", ", a[, 18], ")"),
        paste0(a[, 19], " (", a[, 20], ", ", a[, 21], ")"),
        paste0(a[, 22], " (", a[, 23], ", ", a[, 24], ")"),
        paste0(a[, 25], " (", a[, 26], ", ", a[, 27], ")"),
        paste0(a[, 28], " (", a[, 29], ", ", a[, 30], ")")
    ),
    file = paste0("Parm.csv"), row.names = FALSE
)
