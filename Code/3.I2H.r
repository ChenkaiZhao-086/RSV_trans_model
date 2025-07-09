I2H.Batch <- function(
    MCMC_Result, Age_Sus, VacAgeGroup, Vac_start,
    ReducedSus_1 = 1, ReducedSus_2 = 1,
    VacProp, lag, model = "SIRVV", seed = 380) {
    # Extract data from MCMC
    Posteriori_Median <- MCMC_Result$Median
    Posteriori_Sample <- MCMC_Result$SampleChain %>% split(., row(.))

    # Run simulation
    Parallel.Regist(10, seed = seed)
    Parallel.Import(list(
        "CLI.Print", "Get.InitState", "Get.StateName", "Model.GetI", "Model.GetI.Immu",
        "Parameter.Create", "RefDat", "Scot_Pop", "ContacrStr", "Scot_HospRate",
        "MCMC.PosteriorSample", "Model.RunSim.Immu",
        "Calu.PropI2H", "Calu.InfeRate"
    ))
    PosteriorResult <- parLapply(ParallelNodesInfo[[1]], Posteriori_Sample, \(sample) {
        setDTthreads(1)

        Prop_I2H <- Calu.PropI2H(
            ModelParm = sample, lag, Age_Sus, Vac_start,
            ReducedSus_1 = ReducedSus_1, ReducedSus_2 = ReducedSus_2,
            Effacy_I = 0, Effacy_Hosp = 0, VacProp, model = "SIRVV"
        )

        return(Prop_I2H)
    })
    Parallel.Stop()

    # Get posterior result of vaccination protection
    result_CI <- do.call(rbind, PosteriorResult)
    result_CI <- result_CI %>%
        apply(., 2, \(x){
            quantile(x, c(0.025, 0.5, 0.975))
        }) %>%
        t()

    return(result_CI)
}



S52_I2H <- I2H.Batch(
    MCMC_Result = PosterioriCheck_S52, VacAgeGroup = "S3", Age_Sus = c(0.5, 0.2),
    Vac_start = "2019-08-30", VacProp = 0.8, lag = FALSE
)

S53_I2H <- I2H.Batch(
    MCMC_Result = PosterioriCheck_S53, VacAgeGroup = "S3", Age_Sus = c(0.5, 0.3),
    Vac_start = "2019-08-30", VacProp = 0.8, lag = FALSE
)

S54_I2H <- I2H.Batch(
    MCMC_Result = PosterioriCheck_S54, VacAgeGroup = "S3", Age_Sus = c(0.5, 0.4),
    Vac_start = "2019-08-30", VacProp = 0.8, lag = FALSE
)

S62_I2H <- I2H.Batch(
    MCMC_Result = PosterioriCheck_S62, VacAgeGroup = "S3", Age_Sus = c(0.6, 0.2),
    Vac_start = "2019-08-30", VacProp = 0.8, lag = FALSE
)

S63_I2H <- I2H.Batch(
    MCMC_Result = PosterioriCheck_S63, VacAgeGroup = "S3", Age_Sus = c(0.6, 0.3),
    Vac_start = "2019-08-30", VacProp = 0.8, lag = FALSE
)

S64_I2H <- I2H.Batch(
    MCMC_Result = PosterioriCheck_S64, VacAgeGroup = "S3", Age_Sus = c(0.6, 0.4),
    Vac_start = "2019-08-30", VacProp = 0.8, lag = FALSE
)

IR_ratio_All <- rbind(
    SBase_E6C6A[[1]],
    S42_E6C6A[[1]], S43_E6C6A[[1]], S44_E6C6A[[1]],
    S52_I2H, S53_I2H, S54_I2H,
    S62_I2H, S63_I2H, S64_I2H
)
IR_ratio_All <- IR_ratio_All %>% as.data.frame(IR_ratio_All)
IR_ratio_All$age_group <- rep(c(1:11), 10)
IR_ratio_All$Scenario <- rep(
    c(
        "SBase",
        "S42", "S43", "S44",
        "S52", "S53", "S54",
        "S62", "S63", "S64"
    ),
    each = 11
)
colnames(IR_ratio_All) <- c("lci", "median", "uci", "age_group", "Scenario")
IR_ratio_All <- IR_ratio_All %>%
    mutate(Scenario = factor(Scenario, levels = c(
        "SBase",
        "S44", "S43", "S42",
        "S54", "S53", "S52",
        "S64", "S63", "S62"
    ), labels = c(
        "Base",
        "Scenario 1", "Scenario 2", "Scenario 3",
        "Scenario 4", "Scenario 5", "Scenario 6",
        "Scenario 7", "Scenario 8", "Scenario 9"
    )))

IR_ratio_Appendix <- ggplot(IR_ratio_All, aes(x = age_group, y = median, group = Scenario)) +
    geom_line(aes(colour = Scenario), alpha = 1) +
    geom_ribbon(aes(ymin = lci, ymax = uci, fill = Scenario), alpha = 0.4) +
    geom_point(aes(colour = Scenario), alpha = 0.5) +
    theme_bw() +
    labs(
        x = "Age group",
        y = "Age-specific infection-hospitalisation ratio",
        color = "Scenario", fill = "Scenario"
    ) +
    scale_x_continuous(
        breaks = 1:11,
        labels = c(
            "0-2m", "3-5m", "6-11m", "12-23m", "2-4y", "5-19y",
            "20-59y", "60-64y", "65-69y", "70-74y", "75+y"
        )
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
    facet_wrap(~Scenario, nrow = 4) +
    theme(
        axis.text = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.text.x = element_text(, angle = 45, hjust = 1, margin = margin(b = 5)),
        axis.title = element_text(size = 16, face = "bold"),
        strip.text = element_text(size = 14, face = "bold"),
        strip.background = element_rect(fill = "grey95"),
        strip.placement = "outside",
        legend.position = "none"
    )
ggsave(IR_ratio_Appendix, file = paste0(FilePath, "1c.I2H_ratio_Appendix.pdf"), width = 20, height = 10)




I2H_ratio <- rbind(SBase_E6C6A[[1]], S44_E6C6A[[1]]) # S42_E6C6A[[1]], S43_E6C6A[[1]],
I2H_ratio <- I2H_ratio %>% as.data.frame(I2H_ratio)
I2H_ratio$age_group <- rep(c(1:11), 2)
I2H_ratio$Scenario <- rep(c("Base", "Scenario 1"), each = 11) # , "Scenario 4", "Scenario 3"

I2H_ratio <- I2H_ratio %>%
    mutate(
        Scenario = factor(Scenario, levels = c(
            "Base", "Scenario 1"
        ))
    )

colnames(I2H_ratio) <- c("lci", "median", "uci", "age_group", "Scenario")

I2H_ratio_Fig <- ggplot(I2H_ratio, aes(x = age_group, y = median, group = Scenario)) +
    geom_bar(aes(y = median, fill = Scenario), stat = "identity", position = "dodge") +
    # geom_line(aes(colour = Scenario), alpha = 0.5) +
    # geom_ribbon(aes(ymin = lci, ymax = uci, fill = Scenario), alpha = 0.2) +
    # geom_point(aes(colour = Scenario), alpha = 0.5) +
    theme_classic() +
    labs(
        x = "Age group",
        y = "Age-specific infection-\nhospitalisation ratio",
        color = "Susceptibility", fill = "Susceptibility"
    ) +
    scale_x_continuous(
        breaks = 1:11,
        labels = c(
            "0-2m", "3-5m", "6-11m", "12-23m", "2-4y", "5-19y",
            "20-59y", "60-64y", "65-69y", "70-74y", "75+y"
        )
    ) +
    scale_fill_manual(values = c(
        "#1A5354FF", "#FF6F00FF"
    )) +
    # scale_color_manual(values = c(
    #     "#64beef", "#2ca02c", "#ff7f0e", "#9467bd"
    # )) +
    theme(
        axis.text = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 24, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.text.x = element_text(margin = margin(b = 5)),
        legend.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 20),
        legend.position = c(0.93, 0.75)
    )

ggsave(I2H_ratio_Fig, file = paste0(FilePath, "1c.I2H_ratio.pdf"), width = 16, height = 8)
