### Sbase-----------------------------------------------------------
Parallel.Regist(ncores = 10, seed = 350)
Parallel.Import(list(
    "Get.InitState", "Get.StateName", "Model.GetI", "Model.RunSim",
    "Parameter.Create", "Scot_Pop", "ContacrStr"
))
Sbase_serologocal <- parLapply(ParallelNodesInfo[[1]], Posteriori_Sample_Sbase, \(x){
    setDTthreads(1)

    RunModel <- Model.RunSim(
        Parm = Parameter.Create(
            beta_base = x[1],
            beta_seasonal = x[2],
            phi = x[3],
            seasonal_wavelength = x[4],
            Hosp_rate = rep(1, 11),
            Age_Sus = c(1, 1)
        )
    )

    RunModelResult <- copy(RunModel[[2]])
    RunModelResult[, SumCase := Reported_G1 + Reported_G2 + Reported_G3]
    InfeRate_prob <- (sum(RunModelResult$SumCase) / 3) / sum(Scot_Pop[1:3])
    return(InfeRate_prob)
})

Sbase_serologocal_CI <- quantile(unlist(Sbase_serologocal), probs = c(0.025, 0.5, 0.975))
Parallel.Stop()


### S42-----------------------------------------------------------
Parallel.Regist(ncores = 10, seed = 350)
Parallel.Import(list(
    "Get.InitState", "Get.StateName", "Model.GetI", "Model.RunSim",
    "Parameter.Create", "Scot_Pop", "ContacrStr"
))
S42_serologocal <- parLapply(ParallelNodesInfo[[1]], Posteriori_Sample_S42, \(x){
    setDTthreads(1)

    RunModel <- Model.RunSim(
        Parm = Parameter.Create(
            beta_base = x[1],
            beta_seasonal = x[2],
            phi = x[3],
            seasonal_wavelength = x[4],
            Hosp_rate = rep(1, 11),
            Age_Sus = c(0.4, 0.2)
        )
    )

    RunModelResult <- copy(RunModel[[2]])
    RunModelResult[, SumCase := Reported_G1 + Reported_G2 + Reported_G3]
    InfeRate_prob <- (sum(RunModelResult$SumCase) / 3) / sum(Scot_Pop[1:3])
    return(InfeRate_prob)
})

S42_serologocal_CI <- quantile(unlist(S42_serologocal), probs = c(0.025, 0.5, 0.975))
Parallel.Stop()


### S43-----------------------------------------------------------
Parallel.Regist(ncores = 10, seed = 350)
Parallel.Import(list(
    "Get.InitState", "Get.StateName", "Model.GetI", "Model.RunSim",
    "Parameter.Create", "Scot_Pop", "ContacrStr"
))
S43_serologocal <- parLapply(ParallelNodesInfo[[1]], Posteriori_Sample_S43, \(x){
    setDTthreads(1)

    RunModel <- Model.RunSim(
        Parm = Parameter.Create(
            beta_base = x[1],
            beta_seasonal = x[2],
            phi = x[3],
            seasonal_wavelength = x[4],
            Hosp_rate = rep(1, 11),
            Age_Sus = c(0.4, 0.3)
        )
    )

    RunModelResult <- copy(RunModel[[2]])
    RunModelResult[, SumCase := Reported_G1 + Reported_G2 + Reported_G3]
    InfeRate_prob <- (sum(RunModelResult$SumCase) / 3) / sum(Scot_Pop[1:3])
    return(InfeRate_prob)
})

S43_serologocal_CI <- quantile(unlist(S43_serologocal), probs = c(0.025, 0.5, 0.975))
Parallel.Stop()


### S44-----------------------------------------------------------
Parallel.Regist(ncores = 10, seed = 350)
Parallel.Import(list(
    "Get.InitState", "Get.StateName", "Model.GetI", "Model.RunSim",
    "Parameter.Create", "Scot_Pop", "ContacrStr"
))
S44_serologocal <- parLapply(ParallelNodesInfo[[1]], Posteriori_Sample_S44, \(x){
    setDTthreads(1)

    RunModel <- Model.RunSim(
        Parm = Parameter.Create(
            beta_base = x[1],
            beta_seasonal = x[2],
            phi = x[3],
            seasonal_wavelength = x[4],
            Hosp_rate = rep(1, 11),
            Age_Sus = c(0.4, 0.4)
        )
    )

    RunModelResult <- copy(RunModel[[2]])
    RunModelResult[, SumCase := Reported_G1 + Reported_G2 + Reported_G3]
    InfeRate_prob <- (sum(RunModelResult$SumCase) / 3) / sum(Scot_Pop[1:3])
    return(InfeRate_prob)
})

S44_serologocal_CI <- quantile(unlist(S44_serologocal), probs = c(0.025, 0.5, 0.975))
Parallel.Stop()


### S52-----------------------------------------------------------
Parallel.Regist(ncores = 10, seed = 350)
Parallel.Import(list(
    "Get.InitState", "Get.StateName", "Model.GetI", "Model.RunSim",
    "Parameter.Create", "Scot_Pop", "ContacrStr"
))
S52_serologocal <- parLapply(ParallelNodesInfo[[1]], Posteriori_Sample_S52, \(x){
    setDTthreads(1)

    RunModel <- Model.RunSim(
        Parm = Parameter.Create(
            beta_base = x[1],
            beta_seasonal = x[2],
            phi = x[3],
            seasonal_wavelength = x[4],
            Hosp_rate = rep(1, 11),
            Age_Sus = c(0.5, 0.2)
        )
    )

    RunModelResult <- copy(RunModel[[2]])
    RunModelResult[, SumCase := Reported_G1 + Reported_G2 + Reported_G3]
    InfeRate_prob <- (sum(RunModelResult$SumCase) / 3) / sum(Scot_Pop[1:3])
    return(InfeRate_prob)
})

S52_serologocal_CI <- quantile(unlist(S52_serologocal), probs = c(0.025, 0.5, 0.975))
Parallel.Stop()


### S53-----------------------------------------------------------
Parallel.Regist(ncores = 10, seed = 350)
Parallel.Import(list(
    "Get.InitState", "Get.StateName", "Model.GetI", "Model.RunSim",
    "Parameter.Create", "Scot_Pop", "ContacrStr"
))
S53_serologocal <- parLapply(ParallelNodesInfo[[1]], Posteriori_Sample_S53, \(x){
    setDTthreads(1)

    RunModel <- Model.RunSim(
        Parm = Parameter.Create(
            beta_base = x[1],
            beta_seasonal = x[2],
            phi = x[3],
            seasonal_wavelength = x[4],
            Hosp_rate = rep(1, 11),
            Age_Sus = c(0.5, 0.3)
        )
    )

    RunModelResult <- copy(RunModel[[2]])
    RunModelResult[, SumCase := Reported_G1 + Reported_G2 + Reported_G3]
    InfeRate_prob <- (sum(RunModelResult$SumCase) / 3) / sum(Scot_Pop[1:3])
    return(InfeRate_prob)
})

S53_serologocal_CI <- quantile(unlist(S53_serologocal), probs = c(0.025, 0.5, 0.975))
Parallel.Stop()


### S54-----------------------------------------------------------
Parallel.Regist(ncores = 10, seed = 350)
Parallel.Import(list(
    "Get.InitState", "Get.StateName", "Model.GetI", "Model.RunSim",
    "Parameter.Create", "Scot_Pop", "ContacrStr"
))
S54_serologocal <- parLapply(ParallelNodesInfo[[1]], Posteriori_Sample_S54, \(x){
    setDTthreads(1)

    RunModel <- Model.RunSim(
        Parm = Parameter.Create(
            beta_base = x[1],
            beta_seasonal = x[2],
            phi = x[3],
            seasonal_wavelength = x[4],
            Hosp_rate = rep(1, 11),
            Age_Sus = c(0.5, 0.4)
        )
    )

    RunModelResult <- copy(RunModel[[2]])
    RunModelResult[, SumCase := Reported_G1 + Reported_G2 + Reported_G3]
    InfeRate_prob <- (sum(RunModelResult$SumCase) / 3) / sum(Scot_Pop[1:3])
    return(InfeRate_prob)
})

S54_serologocal_CI <- quantile(unlist(S54_serologocal), probs = c(0.025, 0.5, 0.975))
Parallel.Stop()


### S62-----------------------------------------------------------
Parallel.Regist(ncores = 10, seed = 350)
Parallel.Import(list(
    "Get.InitState", "Get.StateName", "Model.GetI", "Model.RunSim",
    "Parameter.Create", "Scot_Pop", "ContacrStr"
))
S62_serologocal <- parLapply(ParallelNodesInfo[[1]], Posteriori_Sample_S62, \(x){
    setDTthreads(1)

    RunModel <- Model.RunSim(
        Parm = Parameter.Create(
            beta_base = x[1],
            beta_seasonal = x[2],
            phi = x[3],
            seasonal_wavelength = x[4],
            Hosp_rate = rep(1, 11),
            Age_Sus = c(0.6, 0.2)
        )
    )

    RunModelResult <- copy(RunModel[[2]])
    RunModelResult[, SumCase := Reported_G1 + Reported_G2 + Reported_G3]
    InfeRate_prob <- (sum(RunModelResult$SumCase) / 3) / sum(Scot_Pop[1:3])
    return(InfeRate_prob)
})

S62_serologocal_CI <- quantile(unlist(S62_serologocal), probs = c(0.025, 0.5, 0.975))
Parallel.Stop()


### S63-----------------------------------------------------------
Parallel.Regist(ncores = 10, seed = 350)
Parallel.Import(list(
    "Get.InitState", "Get.StateName", "Model.GetI", "Model.RunSim",
    "Parameter.Create", "Scot_Pop", "ContacrStr"
))
S63_serologocal <- parLapply(ParallelNodesInfo[[1]], Posteriori_Sample_S63, \(x){
    setDTthreads(1)

    RunModel <- Model.RunSim(
        Parm = Parameter.Create(
            beta_base = x[1],
            beta_seasonal = x[2],
            phi = x[3],
            seasonal_wavelength = x[4],
            Hosp_rate = rep(1, 11),
            Age_Sus = c(0.6, 0.3)
        )
    )

    RunModelResult <- copy(RunModel[[2]])
    RunModelResult[, SumCase := Reported_G1 + Reported_G2 + Reported_G3]
    InfeRate_prob <- (sum(RunModelResult$SumCase) / 3) / sum(Scot_Pop[1:3])
    return(InfeRate_prob)
})

S63_serologocal_CI <- quantile(unlist(S63_serologocal), probs = c(0.025, 0.5, 0.975))
Parallel.Stop()


### S64-----------------------------------------------------------
Parallel.Regist(ncores = 10, seed = 350)
Parallel.Import(list(
    "Get.InitState", "Get.StateName", "Model.GetI", "Model.RunSim",
    "Parameter.Create", "Scot_Pop", "ContacrStr"
))
S64_serologocal <- parLapply(ParallelNodesInfo[[1]], Posteriori_Sample_S64, \(x){
    setDTthreads(1)

    RunModel <- Model.RunSim(
        Parm = Parameter.Create(
            beta_base = x[1],
            beta_seasonal = x[2],
            phi = x[3],
            seasonal_wavelength = x[4],
            Hosp_rate = rep(1, 11),
            Age_Sus = c(0.6, 0.4)
        )
    )

    RunModelResult <- copy(RunModel[[2]])
    RunModelResult[, SumCase := Reported_G1 + Reported_G2 + Reported_G3]
    InfeRate_prob <- (sum(RunModelResult$SumCase) / 3) / sum(Scot_Pop[1:3])
    return(InfeRate_prob)
})

S64_serologocal_CI <- quantile(unlist(S64_serologocal), probs = c(0.025, 0.5, 0.975))
Parallel.Stop()


# rm(
#     Sbase_serologocal,
#     S42_serologocal, S43_serologocal, S44_serologocal,
#     S52_serologocal, S53_serologocal, S54_serologocal,
#     S62_serologocal, S63_serologocal, S64_serologocal
# )

Serologocal_CI <- rbind(
    Sbase_serologocal_CI,
    S42_serologocal_CI, S43_serologocal_CI, S44_serologocal_CI,
    S52_serologocal_CI, S53_serologocal_CI, S54_serologocal_CI,
    S62_serologocal_CI, S63_serologocal_CI, S64_serologocal_CI
)

### Reference data
ReferenceDat <- data.frame(lci = 0.48, median = 0.53, uci = 0.57, ID = "Reference", group = "Reference")

### Plot results
Serologocal_CI <- as.data.frame(Serologocal_CI)
Serologocal_CI$ID <- rownames(Serologocal_CI)
Serologocal_CI$group <- c(
    "Group 4",
    "Group 1", "Group 1", "Group 1",
    "Group 2", "Group 2", "Group 2",
    "Group 3", "Group 3", "Group 3"
)

colnames(Serologocal_CI) <- c("lci", "median", "uci", "ID", "group")

Serologocal_CI <- rbind(Serologocal_CI, ReferenceDat)

Serologocal_CI <- Serologocal_CI %>%
    mutate(
        ID = factor(ID,
            levels = c(
                "Reference", "Sbase_serologocal_CI",
                "S44_serologocal_CI", "S43_serologocal_CI", "S42_serologocal_CI",
                "S54_serologocal_CI", "S53_serologocal_CI", "S52_serologocal_CI",
                "S64_serologocal_CI", "S63_serologocal_CI", "S62_serologocal_CI"
            ),
            labels = c(
                "Reference",
                "Base",
                "Scenario 1", "Scenario 2", "Scenario 3",
                "Scenario 4", "Scenario 5", "Scenario 6",
                "Scenario 7", "Scenario 8", "Scenario 9"
            )
        ),
        group = factor(group,
            levels = c("Reference", "Group 1", "Group 2", "Group 3", "Group 4")
        ),
        lci = lci * 100,
        median = median * 100,
        uci = uci * 100
    )


Serologocal_Fig <- ggplot(Serologocal_CI, aes(x = ID, y = median, ymin = lci, ymax = uci, group = group)) +
    # geom_pointrange() +
    geom_bar(stat = "identity", position = "dodge", fill = "#83ac5a", alpha = 0.7, width = 0.5) +
    geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.25, linewidth = 1) +
    scale_y_continuous(limits = c(0, 69), breaks = seq(0, 70, 10), expand = c(0, 0)) +
    theme_classic() +
    # facet_wrap(~group, nrow = 1, scales = "free_x") +
    theme(
        axis.text = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 24, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.text.x = element_text(margin = margin(b = 5), angle = 45, hjust = 1),
        strip.text = element_blank(), # element_text(size = 18, face = "bold"),
        strip.background = element_blank(),
        strip.placement = "outside"
    ) +
    labs(
        title = NULL,
        x = "Age-specific susceptibility coefficients",
        y = "Modelled annual infection rates\nand seroprevalence estimates (%)"
    )

ggsave(Serologocal_Fig, file = paste0(FilePath, "1a.Serologocal_Fig.pdf"), width = 16, height = 8)
