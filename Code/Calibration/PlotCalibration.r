SF_CalibrationAppendix <- File.CreateSubFolder(SF_Calibration, "Scenario")

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
        Hosp_rate = rep(1, 11), # c(sample[5:15]),
        Age_Sus = c(1, 1)
    ), lag = FALSE)[[2]]

    return(SimResult)
})
Parallel.Stop()


# Plot the posteriori
MCMC.PosteriorPlot.V2_Appendix(
    dat = PosteriorResult_Sbase, MedianDat = MedianResult_Sbase,
    save = TRUE, path = paste0(SF_CalibrationAppendix, "Sbase_appendix"),
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
        Hosp_rate = rep(1, 11), # c(sample[5:15]),
        Age_Sus = c(0.4, 0.2)
    ), lag = FALSE)[[2]]

    return(SimResult)
})
Parallel.Stop()


# Plot the posteriori
# PosteriorResult_S42_11age <- copy(PosteriorResult_S42)
MCMC.PosteriorPlot.V2_Appendix(
    dat = PosteriorResult_S42, MedianDat = MedianResult_S42,
    save = TRUE, path = paste0(SF_CalibrationAppendix, "S42_appendix"),
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
        Hosp_rate = rep(1, 11), # c(sample[5:15]),
        Age_Sus = c(0.4, 0.3)
    ), lag = FALSE)[[2]]

    return(SimResult)
})
Parallel.Stop()


# Plot the posteriori
MCMC.PosteriorPlot.V2_Appendix(
    dat = PosteriorResult_S43, MedianDat = MedianResult_S43,
    save = TRUE, path = paste0(SF_CalibrationAppendix, "S43_appendix"),
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
        Hosp_rate = rep(1, 11), # c(sample[5:15]),
        Age_Sus = c(0.4, 0.4)
    ), lag = FALSE)[[2]]

    return(SimResult)
})
Parallel.Stop()


# Plot the posteriori
MCMC.PosteriorPlot.V2_Appendix(
    dat = PosteriorResult_S44, MedianDat = MedianResult_S44,
    save = TRUE, path = paste0(SF_CalibrationAppendix, "S44_appendix"),
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
        Hosp_rate = rep(1, 11), # c(sample[5:15]),
        Age_Sus = c(0.5, 0.2)
    ), lag = FALSE)[[2]]

    return(SimResult)
})
Parallel.Stop()


# Plot the posteriori
MCMC.PosteriorPlot.V2_Appendix(
    dat = PosteriorResult_S52, MedianDat = MedianResult_S52,
    save = TRUE, path = paste0(SF_CalibrationAppendix, "S52_appendix"),
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
        Hosp_rate = rep(1, 11), # c(sample[5:15]),
        Age_Sus = c(0.5, 0.3)
    ), lag = FALSE)[[2]]

    return(SimResult)
})
Parallel.Stop()


# Plot the posteriori
MCMC.PosteriorPlot.V2_Appendix(
    dat = PosteriorResult_S53, MedianDat = MedianResult_S53,
    save = TRUE, path = paste0(SF_CalibrationAppendix, "S53_appendix"),
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
        Hosp_rate = rep(1, 11), # c(sample[5:15]),
        Age_Sus = c(0.5, 0.4)
    ), lag = FALSE)[[2]]

    return(SimResult)
})
Parallel.Stop()


# Plot the posteriori
MCMC.PosteriorPlot.V2_Appendix(
    dat = PosteriorResult_S54, MedianDat = MedianResult_S54,
    save = TRUE, path = paste0(SF_CalibrationAppendix, "S54_appendix"),
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
        Hosp_rate = rep(1, 11), # c(sample[5:15]),
        Age_Sus = c(0.6, 0.2)
    ), lag = FALSE)[[2]]

    return(SimResult)
})
Parallel.Stop()


# Plot the posteriori
MCMC.PosteriorPlot.V2_Appendix(
    dat = PosteriorResult_S62, MedianDat = MedianResult_S62,
    save = TRUE, path = paste0(SF_CalibrationAppendix, "S62_appendix"),
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
        Hosp_rate = rep(1, 11), # c(sample[5:15]),
        Age_Sus = c(0.6, 0.3)
    ), lag = FALSE)[[2]]

    return(SimResult)
})
Parallel.Stop()


# Plot the posteriori
MCMC.PosteriorPlot.V2_Appendix(
    dat = PosteriorResult_S63, MedianDat = MedianResult_S63,
    save = TRUE, path = paste0(SF_CalibrationAppendix, "S63_appendix"),
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
        Hosp_rate = rep(1, 11), # c(sample[5:15]),
        Age_Sus = c(0.6, 0.4)
    ), lag = FALSE)[[2]]

    return(SimResult)
})
Parallel.Stop()


# Plot the posteriori
MCMC.PosteriorPlot.V2_Appendix(
    dat = PosteriorResult_S64, MedianDat = MedianResult_S64,
    save = TRUE, path = paste0(SF_CalibrationAppendix, "S64_appendix"),
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

MedianResult[, Summ := rowSums(.SD),
    .SDcols = 3:13
][, 3:13 := NULL][, week := ISOweek::ISOweek2date(paste0(week, "-1"))][, source := factor(source,
    levels = c(
        "Sbase",
        "S44", "S43", "S42",
        "S54", "S53", "S52",
        "S64", "S63", "S62"
    ),
    labels = c(
        # "Sbase", "S42", "S43", "S44",
        # "S52", "S53", "S54", "S62", "S63", "S64"
        "Base",
        "Scenario 1", "Scenario 2", "Scenario 3",
        "Scenario 4", "Scenario 5", "Scenario 6",
        "Scenario 7", "Scenario 8", "Scenario 9"
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
        "Sbase",
        "S44", "S43", "S42",
        "S54", "S53", "S52",
        "S64", "S63", "S62"
    ),
    labels = c(
        # "Sbase", "S42", "S43", "S44",
        # "S52", "S53", "S54", "S62", "S63", "S64"
        "Base",
        "Scenario 1", "Scenario 2", "Scenario 3",
        "Scenario 4", "Scenario 5", "Scenario 6",
        "Scenario 7", "Scenario 8", "Scenario 9"
    )
)]
SimulationResult <- SimulationResult %>% as.data.frame()

#### Fig 1A ---------------------------------------------------------------
PosterPlot <- ggplot() +
    geom_line(data = RealResult, aes(x = week, y = Summ), alpha = 0.8, linewidth = 0.8) +
    geom_line(
        data = MedianResult, aes(x = week, y = Summ, colour = source),
        alpha = 0.6, linewidth = 1
    ) +
    labs(x = "Date", y = "Number of cases", colour = "Susceptibility") +
    theme_classic() +
    scale_x_date(date_labels = "%Y") +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 399)) +
    scale_color_manual(values = c(
        "#1A5354FF",
        "#FF6F00FF", "#C71000FF", "#008EA0FF",
        "#8A4198FF", "#5A9599FF", "#FF6348FF",
        "#84D7E1FF", "#FF95A8FF", "#ADE2D0FF"
    )) +
    theme(
        axis.text = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 28, face = "bold"),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20, face = "bold"),
        legend.position = c(0.93, 0.75)
    )

ggsave(PosterPlot,
    file = paste0(SF_Calibration, "Calibration.pdf"),
    width = 16, height = 8, dpi = 300, bg = "white"
)

#### Fig 1B ---------------------------------------------------------------
SimulationResult_summ <- SimulationResult %>%
    group_by(source, time) %>%
    summarise(
        median = median(summ) / sum(Scot_Pop) * 100,
        lci = quantile(summ, 0.025) / sum(Scot_Pop) * 100,
        uci = quantile(summ, 0.975) / sum(Scot_Pop) * 100
    )
InfePlot <- ggplot() +
    geom_line(
        data = SimulationResult_summ, aes(x = time, y = median, colour = source),
        alpha = 0.7, linewidth = 1
    ) +
    # geom_ribbon(
    #     data = SimulationResult_summ, aes(x = time, ymin = lci, ymax = uci, fill = source),
    #     alpha = 0.05
    # ) +
    # geom_vline(xintercept = as.Date(c("2018-01-01", "2019-01-01", "2020-01-01")), linetype = "dashed", alpha = 0.2) +
    labs(x = "Date", y = "Infection rate (%)", colour = "Susceptibility", fill = "Susceptibility") +
    scale_color_manual(values = c(
        "#1A5354FF",
        "#FF6F00FF", "#C71000FF", "#008EA0FF",
        "#8A4198FF", "#5A9599FF", "#FF6348FF",
        "#84D7E1FF", "#FF95A8FF", "#ADE2D0FF"
    )) +
    scale_fill_manual(values = c(
        "#1A5354FF",
        "#FF6F00FF", "#C71000FF", "#008EA0FF",
        "#8A4198FF", "#5A9599FF", "#FF6348FF",
        "#84D7E1FF", "#FF95A8FF", "#ADE2D0FF"
    )) +
    theme_classic() +
    scale_x_date(date_labels = "%Y") +
    # facet_wrap(~source, nrow = 2) +
    theme(
        axis.text = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 28, face = "bold"),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20, face = "bold"),
        legend.position = c(0.93, 0.75)
    )

ggsave(InfePlot,
    file = paste0(SF_Calibration, "InfeTrance.pdf"),
    width = 16, height = 8, dpi = 300, bg = "white"
)


#### Fig 1C ---------------------------------------------------------------
IR_AllAge <- copy(SimulationResult)
IR_AllAge <- IR_AllAge %>%
    filter(week >= as.Date("2019-06-23")) %>%
    group_by(source, SimNum) %>%
    summarise(summ = sum(summ) / sum(Scot_Pop)) %>%
    group_by(source) %>%
    summarise(
        median = median(summ) * 100,
        lci = quantile(summ, 0.025) * 100,
        uci = quantile(summ, 0.975) * 100
    ) %>%
    as.data.frame()

IR_AllAge_Fig <- ggplot(IR_AllAge, aes(x = source, y = median)) +
    geom_bar(stat = "identity", position = "dodge", fill = "#1e685a", alpha = 0.7, width = 0.5) +
    geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.25, linewidth = 1) +
    # geom_pointrange(aes(ymin = lci, ymax = uci), colour = "black") +
    labs(x = "Age-specific susceptibility coefficients", y = "Average infection rate (%)") +
    theme_classic() +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 42)) +
    theme(
        axis.text = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 24, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.text.x = element_text(margin = margin(b = 5), angle = 45, hjust = 1),
        strip.text = element_blank(), # element_text(size = 18, face = "bold"),
        strip.background = element_blank(),
        strip.placement = "outside"
    )

ggsave(IR_AllAge_Fig, file = paste0(SF_Calibration, "1c.IR_AllAge_Fig.pdf"), width = 16, height = 8)



# PosterPlot <- ggplot() +
#     geom_line(data = RealResult, aes(x = week, y = Summ), linewidth = 1.2) +
#     # geom_line(
#     #     data = SimulationResult, aes(x = week, y = summ, group = interaction(SimNum, source)),
#     #     alpha = 0.01, colour = "#fe4b65", linewidth = 0.8
#     # ) +
#     geom_line(data = MedianResult, aes(x = week, y = Summ, colour = source), alpha = 0.5, linewidth = 1) +
#     geom_vline(xintercept = as.Date(c("2018-01-01", "2019-01-01", "2020-01-01")), linetype = "dashed", alpha = 0.2) +
#     labs(x = "Date", y = "Number of cases") +
#     theme_bw() +
#     scale_x_date(date_labels = "%Y") +
#     # facet_wrap(~source, nrow = 2) +
#     theme(
#         axis.text.x = element_text(size = 14, hjust = 1, face = "bold"),
#         axis.text.y = element_text(size = 14, face = "bold"),
#         axis.title = element_text(size = 18, face = "bold")
#     )


# AgeDistributionData <- rbind(
#     MedianResult_Sbase, MedianResult_S42, MedianResult_S43, MedianResult_S44,
#     MedianResult_S52, MedianResult_S53, MedianResult_S54,
#     MedianResult_S62, MedianResult_S63, MedianResult_S64
# )

# AgeDistributionData[, (3:13) := lapply(.SD, sum), .SDcols = 3:13, by = source]
# AgeDistributionData <- unique(AgeDistributionData, by = "source")
# AgeDistributionData$sum <- rowSums(AgeDistributionData[, 3:13])
# AgeDistributionData[, (3:13) := lapply(.SD, \(x) x / sum * 100), .SDcols = 3:13][, sum := NULL][, time := NULL][, week := NULL]

# AgeDistributionData <- AgeDistributionData %>%
#     pivot_longer(cols = -12, names_to = "Group", values_to = "value") %>%
#     mutate(Group = factor(Group,
#         levels = rev(c(
#             "Reported_G1", "Reported_G2", "Reported_G3", "Reported_G4", "Reported_G5",
#             "Reported_G6", "Reported_G7", "Reported_G8", "Reported_G9", "Reported_G10", "Reported_G11"
#         )),
#         labels = rev(c(
#             "0-2 M", "3-5 M", "6-11 M", "1-2 Y", "3-4 Y", "5-19 Y",
#             "20-59 Y", "60-64 Y", "65-69 Y", "70-74 Y", "75+ Y"
#         ))
#     ))


# AgeDistributionData_Fig <- ggplot(AgeDistributionData, aes(y = source, x = value, fill = Group)) +
#     geom_bar(position = "fill", stat = "identity") +
#     scale_x_continuous(labels = scales::percent, expand = c(0, 0)) +
#     scale_y_discrete(expand = c(0, 0.6)) +
#     labs(x = "Percentage", y = "Category", fill = "Age group") +
#     scale_fill_manual(values = c(
#         "#a6cee3", "#2ca02c", "#9467bd", "#ff7f0e", "#1b9e77",
#         "#8c564b", "#17becf", "#e377c2", "#bcbd22", "#d95f02", "#7570b3"
#     )) +
#     theme_bw() +
#     theme(
#         axis.text = element_text(size = 14, face = "bold"),
#         axis.title = element_text(size = 18, face = "bold"),
#         axis.title.y = element_text(margin = margin(r = 10)),
#         axis.text.x = element_text(margin = margin(b = 5)),
#         legend.title = element_text(size = 18, face = "bold"),
#         legend.text = element_text(size = 14, face = "bold"),
#         legend.position = "right"
#     ) +
#     guides(fill = guide_legend(reverse = TRUE)) + # Reverse the legend order
#     coord_cartesian(xlim = c(0, 1.01), clip = "on") # Set the x-axis limit

# ggsave(AgeDistributionData_Fig,
#     file = paste0(FilePath, "AgeDistributionData_Fig.pdf"),
#     width = 16, height = 8
# )


# rm(
#     MedianResult, MedianResult_Sbase, MedianResult_S42, MedianResult_S43, MedianResult_S44,
#     MedianResult_S52, MedianResult_S53, MedianResult_S54, MedianResult_S62, MedianResult_S63,
#     MedianResult_S64, RealResult,
#     SimulationResult, PosteriorResult_Sbase, PosteriorResult_S42, PosteriorResult_S43,
#     PosteriorResult_S44, PosteriorResult_S52, PosteriorResult_S53, PosteriorResult_S54,
#     PosteriorResult_S62, PosteriorResult_S63, PosteriorResult_S64, PosterPlot
# )
