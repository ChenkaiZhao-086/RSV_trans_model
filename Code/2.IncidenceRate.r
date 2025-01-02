### scenario base -----------------------------------------------------
Parallel.Regist(10)
Parallel.Import(list(
    "CLI.Print", "Get.InitState", "Get.StateName", "Model.GetI", "Model.RunSim",
    "Parameter.Create", "RefDat", "Scot_Pop", "ContacrStr",
    "MCMC.MH", "MCMC.PosteriorSample"
))
IR_base <- parLapply(ParallelNodesInfo[[1]], Posteriori_Sample_Sbase, \(sample) {
    setDTthreads(1)

    SimResult <- Model.RunSim(Parm = Parameter.Create(
        beta_base = sample[1],
        beta_seasonal = sample[2],
        phi = sample[3],
        seasonal_wavelength = sample[4],
        Hosp_rate = rep(1, 11),
        Age_Sus = c(1, 1)
    ), lag = FALSE)[[2]]

    SimResult <- SimResult[time >= as.Date("2019-06-20")]
    SimResult[, (3:13) := lapply(.SD, sum), .SDcols = 3:13]
    SimResult <- as.numeric(SimResult[1, 3:13]) / Scot_Pop

    return(SimResult)
})
Parallel.Stop()

IR_base <- do.call(rbind, IR_base)

IR_base <- IR_base %>%
    apply(., 2, \(x){
        quantile(x, c(0.025, 0.5, 0.975))
    }) %>%
    t() %>%
    as.data.frame()
IR_base$age_group <- rownames(IR_base)
IR_base$Scenario <- "Base"
colnames(IR_base) <- c("lci", "median", "uci", "age_group", "Scenario")
IR_base <- IR_base %>%
    mutate(age_group = factor(age_group,
        levels = c(
            "0-2m", "3-5m", "6-11m", "1-<2y", "2-<5y", "5-<19y",
            "19-<60y", "60-64y", "65-69y", "70-74y", "75y"
        )
    ))


### scenario 42 -----------------------------------------------------
Parallel.Regist(10)
Parallel.Import(list(
    "CLI.Print", "Get.InitState", "Get.StateName", "Model.GetI", "Model.RunSim",
    "Parameter.Create", "RefDat", "Scot_Pop", "ContacrStr",
    "MCMC.MH", "MCMC.PosteriorSample"
))
IR_42 <- parLapply(ParallelNodesInfo[[1]], Posteriori_Sample_S42, \(sample) {
    setDTthreads(1)

    SimResult <- Model.RunSim(Parm = Parameter.Create(
        beta_base = sample[1],
        beta_seasonal = sample[2],
        phi = sample[3],
        seasonal_wavelength = sample[4],
        Hosp_rate = rep(1, 11),
        Age_Sus = c(0.4, 0.2)
    ), lag = FALSE)[[2]]

    SimResult <- SimResult[time >= as.Date("2019-06-20")]
    SimResult[, (3:13) := lapply(.SD, sum), .SDcols = 3:13]
    SimResult <- as.numeric(SimResult[1, 3:13]) / Scot_Pop

    return(SimResult)
})
Parallel.Stop()

IR_42 <- do.call(rbind, IR_42)

IR_42 <- IR_42 %>%
    apply(., 2, \(x){
        quantile(x, c(0.025, 0.5, 0.975))
    }) %>%
    t() %>%
    as.data.frame()
IR_42$age_group <- rownames(IR_42)
IR_42$Scenario <- "S42"
colnames(IR_42) <- c("lci", "median", "uci", "age_group", "Scenario")
IR_42 <- IR_42 %>%
    mutate(age_group = factor(age_group,
        levels = c(
            "0-2m", "3-5m", "6-11m", "1-<2y", "2-<5y", "5-<19y",
            "19-<60y", "60-64y", "65-69y", "70-74y", "75y"
        )
    ))


### scenario 43 -----------------------------------------------------
Parallel.Regist(10)
Parallel.Import(list(
    "CLI.Print", "Get.InitState", "Get.StateName", "Model.GetI", "Model.RunSim",
    "Parameter.Create", "RefDat", "Scot_Pop", "ContacrStr",
    "MCMC.MH", "MCMC.PosteriorSample"
))
IR_43 <- parLapply(ParallelNodesInfo[[1]], Posteriori_Sample_S43, \(sample) {
    setDTthreads(1)

    SimResult <- Model.RunSim(Parm = Parameter.Create(
        beta_base = sample[1],
        beta_seasonal = sample[2],
        phi = sample[3],
        seasonal_wavelength = sample[4],
        Hosp_rate = rep(1, 11),
        Age_Sus = c(0.4, 0.3)
    ), lag = FALSE)[[2]]

    SimResult <- SimResult[time >= as.Date("2019-06-20")]
    SimResult[, (3:13) := lapply(.SD, sum), .SDcols = 3:13]
    SimResult <- as.numeric(SimResult[1, 3:13]) / Scot_Pop

    return(SimResult)
})
Parallel.Stop()

IR_43 <- do.call(rbind, IR_43)

IR_43 <- IR_43 %>%
    apply(., 2, \(x){
        quantile(x, c(0.025, 0.5, 0.975))
    }) %>%
    t() %>%
    as.data.frame()
IR_43$age_group <- rownames(IR_43)
IR_43$Scenario <- "S43"
colnames(IR_43) <- c("lci", "median", "uci", "age_group", "Scenario")
IR_43 <- IR_43 %>%
    mutate(age_group = factor(age_group,
        levels = c(
            "0-2m", "3-5m", "6-11m", "1-<2y", "2-<5y", "5-<19y",
            "19-<60y", "60-64y", "65-69y", "70-74y", "75y"
        )
    ))

### scenario 44 -----------------------------------------------------
Parallel.Regist(10)
Parallel.Import(list(
    "CLI.Print", "Get.InitState", "Get.StateName", "Model.GetI", "Model.RunSim",
    "Parameter.Create", "RefDat", "Scot_Pop", "ContacrStr",
    "MCMC.MH", "MCMC.PosteriorSample"
))
IR_44 <- parLapply(ParallelNodesInfo[[1]], Posteriori_Sample_S44, \(sample) {
    setDTthreads(1)

    SimResult <- Model.RunSim(Parm = Parameter.Create(
        beta_base = sample[1],
        beta_seasonal = sample[2],
        phi = sample[3],
        seasonal_wavelength = sample[4],
        Hosp_rate = rep(1, 11),
        Age_Sus = c(0.4, 0.4)
    ), lag = FALSE)[[2]]

    SimResult <- SimResult[time >= as.Date("2019-06-20")]
    SimResult[, (3:13) := lapply(.SD, sum), .SDcols = 3:13]
    SimResult <- as.numeric(SimResult[1, 3:13]) / Scot_Pop

    return(SimResult)
})
Parallel.Stop()

IR_44 <- do.call(rbind, IR_44)

IR_44 <- IR_44 %>%
    apply(., 2, \(x){
        quantile(x, c(0.025, 0.5, 0.975))
    }) %>%
    t() %>%
    as.data.frame()
IR_44$age_group <- rownames(IR_44)
IR_44$Scenario <- "S44"
colnames(IR_44) <- c("lci", "median", "uci", "age_group", "Scenario")
IR_44 <- IR_44 %>%
    mutate(age_group = factor(age_group,
        levels = c(
            "0-2m", "3-5m", "6-11m", "1-<2y", "2-<5y", "5-<19y",
            "19-<60y", "60-64y", "65-69y", "70-74y", "75y"
        )
    ))

### scenario 52 -----------------------------------------------------
Parallel.Regist(10)
Parallel.Import(list(
    "CLI.Print", "Get.InitState", "Get.StateName", "Model.GetI", "Model.RunSim",
    "Parameter.Create", "RefDat", "Scot_Pop", "ContacrStr",
    "MCMC.MH", "MCMC.PosteriorSample"
))
IR_52 <- parLapply(ParallelNodesInfo[[1]], Posteriori_Sample_S52, \(sample) {
    setDTthreads(1)

    SimResult <- Model.RunSim(Parm = Parameter.Create(
        beta_base = sample[1],
        beta_seasonal = sample[2],
        phi = sample[3],
        seasonal_wavelength = sample[4],
        Hosp_rate = rep(1, 11),
        Age_Sus = c(0.5, 0.2)
    ), lag = FALSE)[[2]]

    SimResult <- SimResult[time >= as.Date("2019-06-20")]
    SimResult[, (3:13) := lapply(.SD, sum), .SDcols = 3:13]
    SimResult <- as.numeric(SimResult[1, 3:13]) / Scot_Pop

    return(SimResult)
})
Parallel.Stop()

IR_52 <- do.call(rbind, IR_52)

IR_52 <- IR_52 %>%
    apply(., 2, \(x){
        quantile(x, c(0.025, 0.5, 0.975))
    }) %>%
    t() %>%
    as.data.frame()
IR_52$age_group <- rownames(IR_52)
IR_52$Scenario <- "S52"
colnames(IR_52) <- c("lci", "median", "uci", "age_group", "Scenario")
IR_52 <- IR_52 %>%
    mutate(age_group = factor(age_group,
        levels = c(
            "0-2m", "3-5m", "6-11m", "1-<2y", "2-<5y", "5-<19y",
            "19-<60y", "60-64y", "65-69y", "70-74y", "75y"
        )
    ))

### scenario 53 -----------------------------------------------------
Parallel.Regist(10)
Parallel.Import(list(
    "CLI.Print", "Get.InitState", "Get.StateName", "Model.GetI", "Model.RunSim",
    "Parameter.Create", "RefDat", "Scot_Pop", "ContacrStr",
    "MCMC.MH", "MCMC.PosteriorSample"
))
IR_53 <- parLapply(ParallelNodesInfo[[1]], Posteriori_Sample_S53, \(sample) {
    setDTthreads(1)

    SimResult <- Model.RunSim(Parm = Parameter.Create(
        beta_base = sample[1],
        beta_seasonal = sample[2],
        phi = sample[3],
        seasonal_wavelength = sample[4],
        Hosp_rate = rep(1, 11),
        Age_Sus = c(0.5, 0.3)
    ), lag = FALSE)[[2]]

    SimResult <- SimResult[time >= as.Date("2019-06-20")]
    SimResult[, (3:13) := lapply(.SD, sum), .SDcols = 3:13]
    SimResult <- as.numeric(SimResult[1, 3:13]) / Scot_Pop

    return(SimResult)
})
Parallel.Stop()

IR_53 <- do.call(rbind, IR_53)

IR_53 <- IR_53 %>%
    apply(., 2, \(x){
        quantile(x, c(0.025, 0.5, 0.975))
    }) %>%
    t() %>%
    as.data.frame()
IR_53$age_group <- rownames(IR_53)
IR_53$Scenario <- "S53"
colnames(IR_53) <- c("lci", "median", "uci", "age_group", "Scenario")
IR_53 <- IR_53 %>%
    mutate(age_group = factor(age_group,
        levels = c(
            "0-2m", "3-5m", "6-11m", "1-<2y", "2-<5y", "5-<19y",
            "19-<60y", "60-64y", "65-69y", "70-74y", "75y"
        )
    ))

### scenario 54 -----------------------------------------------------
Parallel.Regist(10)
Parallel.Import(list(
    "CLI.Print", "Get.InitState", "Get.StateName", "Model.GetI", "Model.RunSim",
    "Parameter.Create", "RefDat", "Scot_Pop", "ContacrStr",
    "MCMC.MH", "MCMC.PosteriorSample"
))
IR_54 <- parLapply(ParallelNodesInfo[[1]], Posteriori_Sample_S54, \(sample) {
    setDTthreads(1)

    SimResult <- Model.RunSim(Parm = Parameter.Create(
        beta_base = sample[1],
        beta_seasonal = sample[2],
        phi = sample[3],
        seasonal_wavelength = sample[4],
        Hosp_rate = rep(1, 11),
        Age_Sus = c(0.5, 0.4)
    ), lag = FALSE)[[2]]

    SimResult <- SimResult[time >= as.Date("2019-06-20")]
    SimResult[, (3:13) := lapply(.SD, sum), .SDcols = 3:13]
    SimResult <- as.numeric(SimResult[1, 3:13]) / Scot_Pop

    return(SimResult)
})
Parallel.Stop()

IR_54 <- do.call(rbind, IR_54)

IR_54 <- IR_54 %>%
    apply(., 2, \(x){
        quantile(x, c(0.025, 0.5, 0.975))
    }) %>%
    t() %>%
    as.data.frame()
IR_54$age_group <- rownames(IR_54)
IR_54$Scenario <- "S54"
colnames(IR_54) <- c("lci", "median", "uci", "age_group", "Scenario")
IR_54 <- IR_54 %>%
    mutate(age_group = factor(age_group,
        levels = c(
            "0-2m", "3-5m", "6-11m", "1-<2y", "2-<5y", "5-<19y",
            "19-<60y", "60-64y", "65-69y", "70-74y", "75y"
        )
    ))

### scenario 62 -----------------------------------------------------
Parallel.Regist(10)
Parallel.Import(list(
    "CLI.Print", "Get.InitState", "Get.StateName", "Model.GetI", "Model.RunSim",
    "Parameter.Create", "RefDat", "Scot_Pop", "ContacrStr",
    "MCMC.MH", "MCMC.PosteriorSample"
))
IR_62 <- parLapply(ParallelNodesInfo[[1]], Posteriori_Sample_S62, \(sample) {
    setDTthreads(1)

    SimResult <- Model.RunSim(Parm = Parameter.Create(
        beta_base = sample[1],
        beta_seasonal = sample[2],
        phi = sample[3],
        seasonal_wavelength = sample[4],
        Hosp_rate = rep(1, 11),
        Age_Sus = c(0.6, 0.2)
    ), lag = FALSE)[[2]]

    SimResult <- SimResult[time >= as.Date("2019-06-20")]
    SimResult[, (3:13) := lapply(.SD, sum), .SDcols = 3:13]
    SimResult <- as.numeric(SimResult[1, 3:13]) / Scot_Pop

    return(SimResult)
})
Parallel.Stop()

IR_62 <- do.call(rbind, IR_62)

IR_62 <- IR_62 %>%
    apply(., 2, \(x){
        quantile(x, c(0.025, 0.5, 0.975))
    }) %>%
    t() %>%
    as.data.frame()
IR_62$age_group <- rownames(IR_62)
IR_62$Scenario <- "S62"
colnames(IR_62) <- c("lci", "median", "uci", "age_group", "Scenario")
IR_62 <- IR_62 %>%
    mutate(age_group = factor(age_group,
        levels = c(
            "0-2m", "3-5m", "6-11m", "1-<2y", "2-<5y", "5-<19y",
            "19-<60y", "60-64y", "65-69y", "70-74y", "75y"
        )
    ))

### scenario 63 -----------------------------------------------------
Parallel.Regist(10)
Parallel.Import(list(
    "CLI.Print", "Get.InitState", "Get.StateName", "Model.GetI", "Model.RunSim",
    "Parameter.Create", "RefDat", "Scot_Pop", "ContacrStr",
    "MCMC.MH", "MCMC.PosteriorSample"
))
IR_63 <- parLapply(ParallelNodesInfo[[1]], Posteriori_Sample_S63, \(sample) {
    setDTthreads(1)

    SimResult <- Model.RunSim(Parm = Parameter.Create(
        beta_base = sample[1],
        beta_seasonal = sample[2],
        phi = sample[3],
        seasonal_wavelength = sample[4],
        Hosp_rate = rep(1, 11),
        Age_Sus = c(0.6, 0.3)
    ), lag = FALSE)[[2]]

    SimResult <- SimResult[time >= as.Date("2019-06-20")]
    SimResult[, (3:13) := lapply(.SD, sum), .SDcols = 3:13]
    SimResult <- as.numeric(SimResult[1, 3:13]) / Scot_Pop

    return(SimResult)
})
Parallel.Stop()

IR_63 <- do.call(rbind, IR_63)

IR_63 <- IR_63 %>%
    apply(., 2, \(x){
        quantile(x, c(0.025, 0.5, 0.975))
    }) %>%
    t() %>%
    as.data.frame()
IR_63$age_group <- rownames(IR_63)
IR_63$Scenario <- "S63"
colnames(IR_63) <- c("lci", "median", "uci", "age_group", "Scenario")
IR_63 <- IR_63 %>%
    mutate(age_group = factor(age_group,
        levels = c(
            "0-2m", "3-5m", "6-11m", "1-<2y", "2-<5y", "5-<19y",
            "19-<60y", "60-64y", "65-69y", "70-74y", "75y"
        )
    ))

### scenario 64 -----------------------------------------------------
Parallel.Regist(10)
Parallel.Import(list(
    "CLI.Print", "Get.InitState", "Get.StateName", "Model.GetI", "Model.RunSim",
    "Parameter.Create", "RefDat", "Scot_Pop", "ContacrStr",
    "MCMC.MH", "MCMC.PosteriorSample"
))
IR_64 <- parLapply(ParallelNodesInfo[[1]], Posteriori_Sample_S64, \(sample) {
    setDTthreads(1)

    SimResult <- Model.RunSim(Parm = Parameter.Create(
        beta_base = sample[1],
        beta_seasonal = sample[2],
        phi = sample[3],
        seasonal_wavelength = sample[4],
        Hosp_rate = rep(1, 11),
        Age_Sus = c(0.6, 0.4)
    ), lag = FALSE)[[2]]

    SimResult <- SimResult[time >= as.Date("2019-06-20")]
    SimResult[, (3:13) := lapply(.SD, sum), .SDcols = 3:13]
    SimResult <- as.numeric(SimResult[1, 3:13]) / Scot_Pop

    return(SimResult)
})
Parallel.Stop()

IR_64 <- do.call(rbind, IR_64)

IR_64 <- IR_64 %>%
    apply(., 2, \(x){
        quantile(x, c(0.025, 0.5, 0.975))
    }) %>%
    t() %>%
    as.data.frame()
IR_64$age_group <- rownames(IR_64)
IR_64$Scenario <- "S64"
colnames(IR_64) <- c("lci", "median", "uci", "age_group", "Scenario")
IR_64 <- IR_64 %>%
    mutate(age_group = factor(age_group,
        levels = c(
            "0-2m", "3-5m", "6-11m", "1-<2y", "2-<5y", "5-<19y",
            "19-<60y", "60-64y", "65-69y", "70-74y", "75y"
        )
    ))


### Combine all results and plot -------------------------------------
IR <- rbind(IR_base, IR_42, IR_43, IR_44, IR_52, IR_53, IR_54, IR_62, IR_63, IR_64)
IR$Group <- c(
    rep("Group 4", 11),
    rep("Group 1", 11 * 3),
    rep("Group 2", 11 * 3),
    rep("Group 3", 11 * 3)
)

IR <- IR %>%
    mutate(
        Scenario = factor(Scenario, levels = c(
            "Base",
            "S44", "S43", "S42",
            "S54", "S53", "S52",
            "S64", "S63", "S62"
        ), labels = c(
            "Base",
            "Scenario 1", "Scenario 2", "Scenario 3",
            "Scenario 4", "Scenario 5", "Scenario 6",
            "Scenario 7", "Scenario 8", "Scenario 9"
        )),
        age_group = factor(age_group,
            levels = c(
                "0-2m", "3-5m", "6-11m", "1-<2y", "2-<5y", "5-<19y",
                "19-<60y", "60-64y", "65-69y", "70-74y", "75y"
            ), labels = c(
                "0-2m", "3-5m", "6-11m", "12-23m", "2-4y", "5-19y",
                "20-59y", "60-64y", "65-69y", "70-74y", "75+y"
            )
        )
    )

# IR_Main <- IR %>%
#     filter(Scenario %in% c("0.4 & 0.2", "0.4 & 0.3", "0.4 & 0.4", "1.0 & 1.0"))

# IR_Main_Fig <- ggplot(IR_Main, aes(x = age_group, y = median, group = Scenario)) +
#     geom_line(aes(colour = Scenario), alpha = 0.5) +
#     geom_ribbon(aes(ymin = lci, ymax = uci, fill = Scenario), alpha = 0.2) +
#     geom_point(aes(colour = Scenario), alpha = 0.5) +
#     theme_bw() +
#     labs(
#         x = "Age group",
#         y = "Age-specific annual\ninfection rates",
#         color = "Susceptibility", fill = "Susceptibility"
#     ) +
#     scale_fill_manual(values = c(
#         "#64beef", "#2ca02c", "#ff7f0e", "#9467bd"
#     )) +
#     scale_color_manual(values = c(
#         "#64beef", "#2ca02c", "#ff7f0e", "#9467bd"
#     )) +
#     theme(
#         axis.text = element_text(size = 20, face = "bold"),
#         axis.title = element_text(size = 24, face = "bold"),
#         axis.title.y = element_text(margin = margin(r = 10)),
#         axis.text.x = element_text(margin = margin(b = 5)),
#         legend.title = element_blank(), # element_text(size = 18, face = "bold"),
#         legend.text = element_text(size = 18, face = "bold"),
#         legend.position = "right"
#     )
IR_Main_Fig <- IR %>%
    mutate(
        lci = lci * 100,
        median = median * 100,
        uci = uci * 100
    ) %>%
    ggplot(., aes(x = age_group, y = median, group = Scenario)) +
    geom_line(aes(colour = Scenario), alpha = 0.7) +
    geom_ribbon(aes(ymin = lci, ymax = uci, fill = Scenario), alpha = 0.2) +
    geom_point(aes(colour = Scenario), alpha = 0.5) +
    theme_classic() +
    labs(
        x = "Age group",
        y = "Age-specific annual infection rates (%)",
        color = "Susceptibility", fill = "Susceptibility"
    ) +
    scale_fill_manual(values = c(
        "#1A5354FF",
        "#FF6F00FF", "#C71000FF", "#008EA0FF",
        "#8A4198FF", "#5A9599FF", "#FF6348FF",
        "#84D7E1FF", "#FF95A8FF", "#ADE2D0FF"
    )) +
    scale_color_manual(values = c(
        "#1A5354FF",
        "#FF6F00FF", "#C71000FF", "#008EA0FF",
        "#8A4198FF", "#5A9599FF", "#FF6348FF",
        "#84D7E1FF", "#FF95A8FF", "#ADE2D0FF"
    )) +
    theme(
        axis.text = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 24, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.text.x = element_text(margin = margin(b = 5)),
        legend.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 20),
        legend.position = c(0.8, 0.75)
    )
ggsave(IR_Main_Fig, file = paste0(FilePath, "1b.IR.pdf"), width = 16, height = 8)

# IR_Fig <- ggplot(IR, aes(x = age_group, y = median, group = Scenario)) +
#     geom_line(aes(colour = Scenario), alpha = 1) +
#     geom_ribbon(aes(ymin = lci, ymax = uci, fill = Scenario), alpha = 0.4) +
#     geom_point(aes(colour = Scenario), alpha = 0.5) +
#     theme_bw() +
#     labs(
#         x = "Age group",
#         y = "Age-specific annual infection rates",
#         color = "Susceptibility", fill = "Susceptibility"
#     ) +
#     scale_fill_manual(values = c(
#         "#FF6F00FF", "#C71000FF", "#008EA0FF", "#8A4198FF",
#         "#5A9599FF", "#FF6348FF", "#84D7E1FF", "#FF95A8FF",
#         "#ADE2D0FF", "#1A5354FF"
#     )) +
#     scale_color_manual(values = c(
#         "#FF6F00FF", "#C71000FF", "#008EA0FF", "#8A4198FF",
#         "#5A9599FF", "#FF6348FF", "#84D7E1FF", "#FF95A8FF",
#         "#ADE2D0FF", "#1A5354FF"
#     )) +
#     # facet_wrap(vars(Scenario), nrow = 4) + # ~Group
#     theme(
#         axis.text = element_text(size = 10, face = "bold"),
#         axis.title.y = element_text(margin = margin(r = 10)),
#         axis.text.x = element_text(margin = margin(b = 5)),
#         axis.title = element_text(size = 18, face = "bold"),
#         strip.text = element_text(size = 14, face = "bold"),
#         strip.background = element_rect(fill = "grey95"),
#         strip.placement = "outside",
#         legend.position = "none"
#     )


# ggsave(IR_Fig, file = paste0(FilePath, "1b.IR_appendix.pdf"), width = 20, height = 10)
