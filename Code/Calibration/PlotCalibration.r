FilePath.Calibration_appendix <- File.CreateSubFolder(FilePath, "Calibration_appendix")

### scenario base -----------------------------------------------------
### Run the model with the median of the posteriori
MedianResult_Sbase <- Model.RunSim(Parm = Parameter.Create(
    beta_base = Posteriori_Median_Sbase[1],
    beta_seasonal = Posteriori_Median_Sbase[2],
    phi = Posteriori_Median_Sbase[3],
    seasonal_wavelength = Posteriori_Median_Sbase[4],
    Hosp_rate = c(Posteriori_Median_Sbase[5:15]),
    Age_Sus = c(1, 1)
), lag = FALSE)[[2]]

### Run the model with the posteriori
Parallel.Regist(10)
Parallel.Import(list(
    "CLI.Print", "Get.InitState", "Get.StateName", "Model.GetI", "Model.RunSim",
    "Parameter.Create", "RefDat", "Scot_Pop", "ContacrStr",
    "MCMC.MH", "MCMC.PosteriorSample"
))
PosteriorResult_Sbase <- parLapply(ParallelNodesInfo[[1]], Posteriori_Sample_Sbase, \(sample) {
    setDTthreads(1)

    SimResult <- Model.RunSim(Parm = Parameter.Create(
        beta_base = sample[1],
        beta_seasonal = sample[2],
        phi = sample[3],
        seasonal_wavelength = sample[4],
        Hosp_rate = c(sample[5:15]),
        Age_Sus = c(1, 1)
    ), lag = FALSE)[[2]]

    return(SimResult)
})
Parallel.Stop()


# Plot the posteriori
MCMC.PosteriorPlot.V2_Appendix(
    dat = PosteriorResult_Sbase, MedianDat = MedianResult_Sbase,
    save = TRUE, path = paste0(FilePath.Calibration_appendix, "Sbase_appendix"),
    width = 18, height = 10
)




### scenario 0.4 & 0.2 -----------------------------------------------------
### Run the model with the median of the posteriori
MedianResult_S42 <- Model.RunSim(Parm = Parameter.Create(
    beta_base = Posteriori_Median_S42[1],
    beta_seasonal = Posteriori_Median_S42[2],
    phi = Posteriori_Median_S42[3],
    seasonal_wavelength = Posteriori_Median_S42[4],
    Hosp_rate = c(Posteriori_Median_S42[5:15]),
    Age_Sus = c(0.4, 0.2)
), lag = FALSE)[[2]]

### Run the model with the posteriori
Parallel.Regist(10)
Parallel.Import(list(
    "CLI.Print", "Get.InitState", "Get.StateName", "Model.GetI", "Model.RunSim",
    "Parameter.Create", "RefDat", "Scot_Pop", "ContacrStr",
    "MCMC.MH", "MCMC.PosteriorSample"
))
PosteriorResult_S42 <- parLapply(ParallelNodesInfo[[1]], Posteriori_Sample_S42, \(sample) {
    setDTthreads(1)

    SimResult <- Model.RunSim(Parm = Parameter.Create(
        beta_base = sample[1],
        beta_seasonal = sample[2],
        phi = sample[3],
        seasonal_wavelength = sample[4],
        Hosp_rate = c(sample[5:15]),
        Age_Sus = c(0.4, 0.2)
    ), lag = FALSE)[[2]]

    return(SimResult)
})
Parallel.Stop()


# Plot the posteriori
# PosteriorResult_S42_11age <- copy(PosteriorResult_S42)
MCMC.PosteriorPlot.V2_Appendix(
    dat = PosteriorResult_S42, MedianDat = MedianResult_S42,
    save = TRUE, path = paste0(FilePath.Calibration_appendix, "S42_appendix"),
    width = 18, height = 10
)



### scenario 0.4 & 0.3 -----------------------------------------------------
### Run the model with the median of the posteriori
MedianResult_S43 <- Model.RunSim(Parm = Parameter.Create(
    beta_base = Posteriori_Median_S43[1],
    beta_seasonal = Posteriori_Median_S43[2],
    phi = Posteriori_Median_S43[3],
    seasonal_wavelength = Posteriori_Median_S43[4],
    Hosp_rate = c(Posteriori_Median_S43[5:15]),
    Age_Sus = c(0.4, 0.3)
), lag = FALSE)[[2]]

### Run the model with the posteriori
Parallel.Regist(10)
Parallel.Import(list(
    "CLI.Print", "Get.InitState", "Get.StateName", "Model.GetI", "Model.RunSim",
    "Parameter.Create", "RefDat", "Scot_Pop", "ContacrStr",
    "MCMC.MH", "MCMC.PosteriorSample"
))
PosteriorResult_S43 <- parLapply(ParallelNodesInfo[[1]], Posteriori_Sample_S43, \(sample) {
    setDTthreads(1)

    SimResult <- Model.RunSim(Parm = Parameter.Create(
        beta_base = sample[1],
        beta_seasonal = sample[2],
        phi = sample[3],
        seasonal_wavelength = sample[4],
        Hosp_rate = c(sample[5:15]),
        Age_Sus = c(0.4, 0.3)
    ), lag = FALSE)[[2]]

    return(SimResult)
})
Parallel.Stop()


# Plot the posteriori
MCMC.PosteriorPlot.V2_Appendix(
    dat = PosteriorResult_S43, MedianDat = MedianResult_S43,
    save = TRUE, path = paste0(FilePath.Calibration_appendix, "S43_appendix"),
    width = 18, height = 10
)




### scenario 0.4 & 0.4 -----------------------------------------------------
### Run the model with the median of the posteriori
MedianResult_S44 <- Model.RunSim(Parm = Parameter.Create(
    beta_base = Posteriori_Median_S44[1],
    beta_seasonal = Posteriori_Median_S44[2],
    phi = Posteriori_Median_S44[3],
    seasonal_wavelength = Posteriori_Median_S44[4],
    Hosp_rate = c(Posteriori_Median_S44[5:15]),
    Age_Sus = c(0.4, 0.4)
), lag = FALSE)[[2]]

### Run the model with the posteriori
Parallel.Regist(10)
Parallel.Import(list(
    "CLI.Print", "Get.InitState", "Get.StateName", "Model.GetI", "Model.RunSim",
    "Parameter.Create", "RefDat", "Scot_Pop", "ContacrStr",
    "MCMC.MH", "MCMC.PosteriorSample"
))
PosteriorResult_S44 <- parLapply(ParallelNodesInfo[[1]], Posteriori_Sample_S44, \(sample) {
    setDTthreads(1)

    SimResult <- Model.RunSim(Parm = Parameter.Create(
        beta_base = sample[1],
        beta_seasonal = sample[2],
        phi = sample[3],
        seasonal_wavelength = sample[4],
        Hosp_rate = c(sample[5:15]),
        Age_Sus = c(0.4, 0.4)
    ), lag = FALSE)[[2]]

    return(SimResult)
})
Parallel.Stop()


# Plot the posteriori
MCMC.PosteriorPlot.V2_Appendix(
    dat = PosteriorResult_S44, MedianDat = MedianResult_S44,
    save = TRUE, path = paste0(FilePath.Calibration_appendix, "S44_appendix"),
    width = 18, height = 10
)



### scenario 0.5 & 0.2 -----------------------------------------------------
### Run the model with the median of the posteriori
MedianResult_S52 <- Model.RunSim(Parm = Parameter.Create(
    beta_base = Posteriori_Median_S52[1],
    beta_seasonal = Posteriori_Median_S52[2],
    phi = Posteriori_Median_S52[3],
    seasonal_wavelength = Posteriori_Median_S52[4],
    Hosp_rate = c(Posteriori_Median_S52[5:15]),
    Age_Sus = c(0.5, 0.2)
), lag = FALSE)[[2]]

### Run the model with the posteriori
Parallel.Regist(10)
Parallel.Import(list(
    "CLI.Print", "Get.InitState", "Get.StateName", "Model.GetI", "Model.RunSim",
    "Parameter.Create", "RefDat", "Scot_Pop", "ContacrStr",
    "MCMC.MH", "MCMC.PosteriorSample"
))
PosteriorResult_S52 <- parLapply(ParallelNodesInfo[[1]], Posteriori_Sample_S52, \(sample) {
    setDTthreads(1)

    SimResult <- Model.RunSim(Parm = Parameter.Create(
        beta_base = sample[1],
        beta_seasonal = sample[2],
        phi = sample[3],
        seasonal_wavelength = sample[4],
        Hosp_rate = c(sample[5:15]),
        Age_Sus = c(0.5, 0.2)
    ), lag = FALSE)[[2]]

    return(SimResult)
})
Parallel.Stop()


# Plot the posteriori
MCMC.PosteriorPlot.V2_Appendix(
    dat = PosteriorResult_S52, MedianDat = MedianResult_S52,
    save = TRUE, path = paste0(FilePath.Calibration_appendix, "S52_appendix"),
    width = 18, height = 10
)



### scenario 0.5 & 0.3 -----------------------------------------------------
### Run the model with the median of the posteriori
MedianResult_S53 <- Model.RunSim(Parm = Parameter.Create(
    beta_base = Posteriori_Median_S53[1],
    beta_seasonal = Posteriori_Median_S53[2],
    phi = Posteriori_Median_S53[3],
    seasonal_wavelength = Posteriori_Median_S53[4],
    Hosp_rate = c(Posteriori_Median_S53[5:15]),
    Age_Sus = c(0.5, 0.3)
), lag = FALSE)[[2]]

### Run the model with the posteriori
Parallel.Regist(10)
Parallel.Import(list(
    "CLI.Print", "Get.InitState", "Get.StateName", "Model.GetI", "Model.RunSim",
    "Parameter.Create", "RefDat", "Scot_Pop", "ContacrStr",
    "MCMC.MH", "MCMC.PosteriorSample"
))
PosteriorResult_S53 <- parLapply(ParallelNodesInfo[[1]], Posteriori_Sample_S53, \(sample) {
    setDTthreads(1)

    SimResult <- Model.RunSim(Parm = Parameter.Create(
        beta_base = sample[1],
        beta_seasonal = sample[2],
        phi = sample[3],
        seasonal_wavelength = sample[4],
        Hosp_rate = c(sample[5:15]),
        Age_Sus = c(0.5, 0.3)
    ), lag = FALSE)[[2]]

    return(SimResult)
})
Parallel.Stop()


# Plot the posteriori
MCMC.PosteriorPlot.V2_Appendix(
    dat = PosteriorResult_S53, MedianDat = MedianResult_S53,
    save = TRUE, path = paste0(FilePath.Calibration_appendix, "S53_appendix"),
    width = 18, height = 10
)



### scenario 0.5 & 0.4 -----------------------------------------------------
### Run the model with the median of the posteriori
MedianResult_S54 <- Model.RunSim(Parm = Parameter.Create(
    beta_base = Posteriori_Median_S54[1],
    beta_seasonal = Posteriori_Median_S54[2],
    phi = Posteriori_Median_S54[3],
    seasonal_wavelength = Posteriori_Median_S54[4],
    Hosp_rate = c(Posteriori_Median_S54[5:15]),
    Age_Sus = c(0.5, 0.4)
), lag = FALSE)[[2]]

### Run the model with the posteriori
Parallel.Regist(10)
Parallel.Import(list(
    "CLI.Print", "Get.InitState", "Get.StateName", "Model.GetI", "Model.RunSim",
    "Parameter.Create", "RefDat", "Scot_Pop", "ContacrStr",
    "MCMC.MH", "MCMC.PosteriorSample"
))
PosteriorResult_S54 <- parLapply(ParallelNodesInfo[[1]], Posteriori_Sample_S54, \(sample) {
    setDTthreads(1)

    SimResult <- Model.RunSim(Parm = Parameter.Create(
        beta_base = sample[1],
        beta_seasonal = sample[2],
        phi = sample[3],
        seasonal_wavelength = sample[4],
        Hosp_rate = c(sample[5:15]),
        Age_Sus = c(0.5, 0.4)
    ), lag = FALSE)[[2]]

    return(SimResult)
})
Parallel.Stop()


# Plot the posteriori
MCMC.PosteriorPlot.V2_Appendix(
    dat = PosteriorResult_S54, MedianDat = MedianResult_S54,
    save = TRUE, path = paste0(FilePath.Calibration_appendix, "S54_appendix"),
    width = 18, height = 10
)




### scenario 0.6 & 0.2 -----------------------------------------------------
### Run the model with the median of the posteriori
MedianResult_S62 <- Model.RunSim(Parm = Parameter.Create(
    beta_base = Posteriori_Median_S62[1],
    beta_seasonal = Posteriori_Median_S62[2],
    phi = Posteriori_Median_S62[3],
    seasonal_wavelength = Posteriori_Median_S62[4],
    Hosp_rate = c(Posteriori_Median_S62[5:15]),
    Age_Sus = c(0.6, 0.2)
), lag = FALSE)[[2]]

### Run the model with the posteriori
Parallel.Regist(10)
Parallel.Import(list(
    "CLI.Print", "Get.InitState", "Get.StateName", "Model.GetI", "Model.RunSim",
    "Parameter.Create", "RefDat", "Scot_Pop", "ContacrStr",
    "MCMC.MH", "MCMC.PosteriorSample"
))
PosteriorResult_S62 <- parLapply(ParallelNodesInfo[[1]], Posteriori_Sample_S62, \(sample) {
    setDTthreads(1)

    SimResult <- Model.RunSim(Parm = Parameter.Create(
        beta_base = sample[1],
        beta_seasonal = sample[2],
        phi = sample[3],
        seasonal_wavelength = sample[4],
        Hosp_rate = c(sample[5:15]),
        Age_Sus = c(0.6, 0.2)
    ), lag = FALSE)[[2]]

    return(SimResult)
})
Parallel.Stop()


# Plot the posteriori
MCMC.PosteriorPlot.V2_Appendix(
    dat = PosteriorResult_S62, MedianDat = MedianResult_S62,
    save = TRUE, path = paste0(FilePath.Calibration_appendix, "S62_appendix"),
    width = 18, height = 10
)



### scenario 0.6 & 0.3 -----------------------------------------------------
### Run the model with the median of the posteriori
MedianResult_S63 <- Model.RunSim(Parm = Parameter.Create(
    beta_base = Posteriori_Median_S63[1],
    beta_seasonal = Posteriori_Median_S63[2],
    phi = Posteriori_Median_S63[3],
    seasonal_wavelength = Posteriori_Median_S63[4],
    Hosp_rate = c(Posteriori_Median_S63[5:15]),
    Age_Sus = c(0.6, 0.3)
), lag = FALSE)[[2]]

### Run the model with the posteriori
Parallel.Regist(10)
Parallel.Import(list(
    "CLI.Print", "Get.InitState", "Get.StateName", "Model.GetI", "Model.RunSim",
    "Parameter.Create", "RefDat", "Scot_Pop", "ContacrStr",
    "MCMC.MH", "MCMC.PosteriorSample"
))
PosteriorResult_S63 <- parLapply(ParallelNodesInfo[[1]], Posteriori_Sample_S63, \(sample) {
    setDTthreads(1)

    SimResult <- Model.RunSim(Parm = Parameter.Create(
        beta_base = sample[1],
        beta_seasonal = sample[2],
        phi = sample[3],
        seasonal_wavelength = sample[4],
        Hosp_rate = c(sample[5:15]),
        Age_Sus = c(0.6, 0.3)
    ), lag = FALSE)[[2]]

    return(SimResult)
})
Parallel.Stop()


# Plot the posteriori
MCMC.PosteriorPlot.V2_Appendix(
    dat = PosteriorResult_S63, MedianDat = MedianResult_S63,
    save = TRUE, path = paste0(FilePath.Calibration_appendix, "S63_appendix"),
    width = 18, height = 10
)



### scenario 0.6 & 0.4 -----------------------------------------------------
### Run the model with the median of the posteriori
MedianResult_S64 <- Model.RunSim(Parm = Parameter.Create(
    beta_base = Posteriori_Median_S64[1],
    beta_seasonal = Posteriori_Median_S64[2],
    phi = Posteriori_Median_S64[3],
    seasonal_wavelength = Posteriori_Median_S64[4],
    Hosp_rate = c(Posteriori_Median_S64[5:15]),
    Age_Sus = c(0.6, 0.4)
), lag = FALSE)[[2]]

### Run the model with the posteriori
Parallel.Regist(10)
Parallel.Import(list(
    "CLI.Print", "Get.InitState", "Get.StateName", "Model.GetI", "Model.RunSim",
    "Parameter.Create", "RefDat", "Scot_Pop", "ContacrStr",
    "MCMC.MH", "MCMC.PosteriorSample"
))
PosteriorResult_S64 <- parLapply(ParallelNodesInfo[[1]], Posteriori_Sample_S64, \(sample) {
    setDTthreads(1)

    SimResult <- Model.RunSim(Parm = Parameter.Create(
        beta_base = sample[1],
        beta_seasonal = sample[2],
        phi = sample[3],
        seasonal_wavelength = sample[4],
        Hosp_rate = c(sample[5:15]),
        Age_Sus = c(0.6, 0.4)
    ), lag = FALSE)[[2]]

    return(SimResult)
})
Parallel.Stop()


# Plot the posteriori
MCMC.PosteriorPlot.V2_Appendix(
    dat = PosteriorResult_S64, MedianDat = MedianResult_S64,
    save = TRUE, path = paste0(FilePath.Calibration_appendix, "S64_appendix"),
    width = 18, height = 10
)


### Plot calibration results ------------------------------------------------
# Median result
MedianResult_Sbase[, source := "Sbase"]
MedianResult_S42[, source := "S42"]
MedianResult_S43[, source := "S43"]
MedianResult_S44[, source := "S44"]
MedianResult_S52[, source := "S52"]
MedianResult_S53[, source := "S53"]
MedianResult_S54[, source := "S54"]
MedianResult_S62[, source := "S62"]
MedianResult_S63[, source := "S63"]
MedianResult_S64[, source := "S64"]

MedianResult <- rbind(
    MedianResult_Sbase, MedianResult_S42, MedianResult_S43, MedianResult_S44,
    MedianResult_S52, MedianResult_S53, MedianResult_S54,
    MedianResult_S62, MedianResult_S63, MedianResult_S64
)

MedianResult[, Summ := rowSums(.SD), .SDcols = 3:13][, 3:13 := NULL][, week := ISOweek::ISOweek2date(paste0(week, "-1"))][, source := factor(source,
    levels = c(
        "Sbase", "S42", "S43", "S44",
        "S52", "S53", "S54", "S62", "S63", "S64"
    ),
    labels = c(
        "Sbase", "S42", "S43", "S44",
        "S52", "S53", "S54", "S62", "S63", "S64"
    )
)]
MedianResult <- MedianResult %>% as.data.frame()

# Real result
RealResult <- copy(RealDat_plot)
RealResult[, Summ := sum(summ, na.rm = T), by = week][, 2:4 := NULL][, week := ISOweek::ISOweek2date(paste0(week, "-1"))]
RealResult <- unique(RealResult) %>%
    as.data.frame() %>%
    mutate(Summ = ifelse(Summ == 0, NA, Summ))



# Simulated result
PosteriorResult_Sbase <- lapply(seq_along(PosteriorResult_Sbase), function(x) {
    PosteriorResult_Sbase[[x]][, source := "Sbase"][, summ := rowSums(.SD), .SDcols = 3:13][, 3:13 := NULL][, SimNum := x]
})
PosteriorResult_Sbase <- rbindlist(PosteriorResult_Sbase)

PosteriorResult_S42 <- lapply(seq_along(PosteriorResult_S42), function(x) {
    PosteriorResult_S42[[x]][, source := "S42"][, summ := rowSums(.SD), .SDcols = 3:13][, 3:13 := NULL][, SimNum := x]
})
PosteriorResult_S42 <- rbindlist(PosteriorResult_S42)

PosteriorResult_S43 <- lapply(seq_along(PosteriorResult_S43), function(x) {
    PosteriorResult_S43[[x]][, source := "S43"][, summ := rowSums(.SD), .SDcols = 3:13][, 3:13 := NULL][, SimNum := x]
})
PosteriorResult_S43 <- rbindlist(PosteriorResult_S43)

PosteriorResult_S44 <- lapply(seq_along(PosteriorResult_S44), function(x) {
    PosteriorResult_S44[[x]][, source := "S44"][, summ := rowSums(.SD), .SDcols = 3:13][, 3:13 := NULL][, SimNum := x]
})
PosteriorResult_S44 <- rbindlist(PosteriorResult_S44)

PosteriorResult_S52 <- lapply(seq_along(PosteriorResult_S52), function(x) {
    PosteriorResult_S52[[x]][, source := "S52"][, summ := rowSums(.SD), .SDcols = 3:13][, 3:13 := NULL][, SimNum := x]
})
PosteriorResult_S52 <- rbindlist(PosteriorResult_S52)

PosteriorResult_S53 <- lapply(seq_along(PosteriorResult_S53), function(x) {
    PosteriorResult_S53[[x]][, source := "S53"][, summ := rowSums(.SD), .SDcols = 3:13][, 3:13 := NULL][, SimNum := x]
})
PosteriorResult_S53 <- rbindlist(PosteriorResult_S53)

PosteriorResult_S54 <- lapply(seq_along(PosteriorResult_S54), function(x) {
    PosteriorResult_S54[[x]][, source := "S54"][, summ := rowSums(.SD), .SDcols = 3:13][, 3:13 := NULL][, SimNum := x]
})
PosteriorResult_S54 <- rbindlist(PosteriorResult_S54)

PosteriorResult_S62 <- lapply(seq_along(PosteriorResult_S62), function(x) {
    PosteriorResult_S62[[x]][, source := "S62"][, summ := rowSums(.SD), .SDcols = 3:13][, 3:13 := NULL][, SimNum := x]
})
PosteriorResult_S62 <- rbindlist(PosteriorResult_S62)

PosteriorResult_S63 <- lapply(seq_along(PosteriorResult_S63), function(x) {
    PosteriorResult_S63[[x]][, source := "S63"][, summ := rowSums(.SD), .SDcols = 3:13][, 3:13 := NULL][, SimNum := x]
})
PosteriorResult_S63 <- rbindlist(PosteriorResult_S63)

PosteriorResult_S64 <- lapply(seq_along(PosteriorResult_S64), function(x) {
    PosteriorResult_S64[[x]][, source := "S64"][, summ := rowSums(.SD), .SDcols = 3:13][, 3:13 := NULL][, SimNum := x]
})
PosteriorResult_S64 <- rbindlist(PosteriorResult_S64)


SimulationResult <- rbind(
    PosteriorResult_Sbase, PosteriorResult_S42, PosteriorResult_S43, PosteriorResult_S44,
    PosteriorResult_S52, PosteriorResult_S53, PosteriorResult_S54,
    PosteriorResult_S62, PosteriorResult_S63, PosteriorResult_S64
)

SimulationResult[, week := ISOweek::ISOweek2date(paste0(week, "-1"))][, source := factor(source,
    levels = c(
        "Sbase", "S42", "S43", "S44",
        "S52", "S53", "S54", "S62", "S63", "S64"
    ),
    labels = c(
        "Sbase", "S42", "S43", "S44",
        "S52", "S53", "S54", "S62", "S63", "S64"
    )
)]
SimulationResult <- SimulationResult %>% as.data.frame()



PosterPlot <- ggplot() +
    geom_line(data = RealResult, aes(x = week, y = Summ), linewidth = 1.2) +
    geom_line(
        data = SimulationResult, aes(x = week, y = summ, group = interaction(SimNum, source)),
        alpha = 0.01, colour = "#fe4b65", linewidth = 0.8
    ) +
    geom_line(data = MedianResult, aes(x = week, y = Summ, group = source), colour = "#c9283a", linewidth = 1) +
    geom_vline(xintercept = as.Date(c("2018-01-01", "2019-01-01", "2020-01-01")), linetype = "dashed", alpha = 0.2) +
    labs(x = "Date", y = "Number of cases") +
    theme_minimal() +
    scale_x_date(date_labels = "%Y") +
    facet_wrap(~source, nrow = 2) +
    theme(
        axis.text.x = element_text(size = 18, hjust = 1),
        axis.text.y = element_text(size = 18),
        axis.title = element_text(size = 24),
        strip.text = element_text(size = 24)
    )

ggsave(PosterPlot, file = "Output/V1/Calibration.pdf", width = 20, height = 10)

rm(
    MedianResult, MedianResult_Sbase, MedianResult_S42, MedianResult_S43, MedianResult_S44,
    MedianResult_S52, MedianResult_S53, MedianResult_S54, MedianResult_S62, MedianResult_S63,
    MedianResult_S64, RealResult,
    SimulationResult, PosteriorResult_Sbase, PosteriorResult_S42, PosteriorResult_S43,
    PosteriorResult_S44, PosteriorResult_S52, PosteriorResult_S53, PosteriorResult_S54,
    PosteriorResult_S62, PosteriorResult_S63, PosteriorResult_S64, PosterPlot
)
