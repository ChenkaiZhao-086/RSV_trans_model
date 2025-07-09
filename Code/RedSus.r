RedSus_path <- File.CreateSubFolder(FilePath, "RedSus")

#################################
## Parameter calibration
#################################
# PriorList
PriorList <- list(
  c( # Chain 1
    0.5, # beta_base
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
    0.45, # beta_base
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
    0.55, # beta_base
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
Base_Reduced_Sus <- parLapply(ParallelNodesInfo[[1]], PriorList, \(x) {
  setDTthreads(1)
  MCMC.MH(
    Prior = x, n_iterations = 100000, TargetDat = RefDat, lag = FALSE, Sus_reduce = c(1, 1),
    ReducedSus_1 = 0.76, ReducedSus_2 = 0.88,
    model = "SIR", AgeDistribution = TRUE
  )
})
# save(S_base, file = "Output/RedSus.RData")
Parallel.Stop()

# PosterioriCheck_Redsus <- MCMC.PostProcess(Base_Reduced_Sus, burn_in = 0000, thin = 10)

# Posterior check for ReduceSus
PosterioriCheck_Redsus <- MCMC.PostProcess(Base_Reduced_Sus, burn_in = 20000, thin = 10, n_sample = 1000)
Posteriori_Median_Redsus <- PosterioriCheck_Redsus$Median
Posteriori_Sample_Redsus <- PosterioriCheck_Redsus$SampleChain %>% split(., row(.))
MCMC.TracePlot(Base_Reduced_Sus, paste0(SF_Calibration, "Redsus_"))


MCMC.MH(
  Prior = PriorList[[1]], n_iterations = 100, TargetDat = RefDat, lag = FALSE, Sus_reduce = c(1, 1),
  ReducedSus_1 = 0.76, ReducedSus_2 = 0.88,
  model = "SIR", AgeDistribution = TRUE
)


Chain1 <- Base_Reduced_Sus[[1]]

Chain1Sim <- Model.RunSim(
  Parm = Parameter.Create(
    beta_base = .5, # Chain1[dim(Chain1)[1], 1],
    beta_seasonal = Chain1[dim(Chain1)[1], 2],
    phi = Chain1[dim(Chain1)[1], 3],
    seasonal_wavelength = Chain1[dim(Chain1)[1], 4],
    Hosp_rate = c(
      Chain1[dim(Chain1)[1], 5], Chain1[dim(Chain1)[1], 6], Chain1[dim(Chain1)[1], 7], Chain1[dim(Chain1)[1], 8],
      Chain1[dim(Chain1)[1], 9], Chain1[dim(Chain1)[1], 10], Chain1[dim(Chain1)[1], 11], Chain1[dim(Chain1)[1], 12],
      Chain1[dim(Chain1)[1], 13], Chain1[dim(Chain1)[1], 14], Chain1[dim(Chain1)[1], 15]
    ), ReducedSus_1 = 0.76, ReducedSus_2 = 0.88,
    Age_Sus = c(1, 1)
  ),
  lag = FALSE
)

Plot.Model(Chain1Sim, RealDat = RealDat_plot)


#################################
## Average infection rate for all age groups
#################################
Parallel.Regist(10)
Parallel.Import(list(
  "CLI.Print", "Get.InitState", "Get.StateName", "Model.GetI", "Model.RunSim",
  "Parameter.Create", "RefDat", "Scot_Pop", "ContacrStr",
  "MCMC.MH", "MCMC.PosteriorSample"
))
PosteriorResult_RedusedSus <- parLapply(ParallelNodesInfo[[1]], Posteriori_Sample_Redsus, \(sample) {
  setDTthreads(1)

  SimResult <- Model.RunSim(Parm = Parameter.Create(
    beta_base = sample[1],
    beta_seasonal = sample[2],
    phi = sample[3],
    seasonal_wavelength = sample[4],
    Hosp_rate = rep(1, 11), # c(sample[5:15]),
    ReducedSus_1 = 0.76, ReducedSus_2 = 0.88,
    Age_Sus = c(1.0, 1.0)
  ), lag = FALSE)[[2]]

  return(SimResult)
})
Parallel.Stop()


PosteriorResult_RedusedSus <- lapply(seq_along(PosteriorResult_RedusedSus), function(x) {
  PosteriorResult_RedusedSus[[x]][, source := "RedSus"][, summ := rowSums(.SD), .SDcols = 3:13][, 3:13 := NULL][, SimNum := x]
})
PosteriorResult_RedusedSus <- rbindlist(PosteriorResult_RedusedSus)

PosteriorResult_RedusedSus[, week := ISOweek::ISOweek2date(paste0(week, "-1"))]
SimulationResult_RedusedSus <- PosteriorResult_RedusedSus %>% as.data.frame()

IR_AllAge_RedSus <- rbind(SimulationResult_RedusedSus, SimulationResult[SimulationResult$source %in% c("Scenario 1", "Base"), ])
IR_AllAge_RedSus <- IR_AllAge_RedSus %>%
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

IR_AllAge_Fig_RedSus <- ggplot(IR_AllAge_RedSus, aes(x = source, y = median)) +
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

ggsave(IR_AllAge_Fig_RedSus, file = paste0(RedSus_path, "1c.IR_AllAge_RedSus.pdf"), width = 12, height = 8)



#################################
## infat infection rate
#################################
Parallel.Regist(ncores = 10, seed = 350)
Parallel.Import(list(
  "Get.InitState", "Get.StateName", "Model.GetI", "Model.RunSim",
  "Parameter.Create", "Scot_Pop", "ContacrStr"
))
RedSus_serologocal <- parLapply(ParallelNodesInfo[[1]], Posteriori_Sample_Redsus, \(x){
  setDTthreads(1)

  RunModel <- Model.RunSim(
    Parm = Parameter.Create(
      beta_base = x[1],
      beta_seasonal = x[2],
      phi = x[3],
      seasonal_wavelength = x[4],
      Hosp_rate = rep(1, 11),
      ReducedSus_1 = 0.76, ReducedSus_2 = 0.88,
      Age_Sus = c(1.0, 1.0)
    )
  )

  RunModelResult <- copy(RunModel[[2]])
  RunModelResult[, SumCase := Reported_G1 + Reported_G2 + Reported_G3]
  InfeRate_prob <- (sum(RunModelResult$SumCase) / 3) / sum(Scot_Pop[1:3])
  return(InfeRate_prob)
})

RedSus_serologocal_CI <- quantile(unlist(S64_serologocal), probs = c(0.025, 0.5, 0.975))
Parallel.Stop()

RedSus_serologocal_CI <- RedSus_serologocal_CI %>%
  as.data.frame() %>%
  t() %>%
  as.data.frame() %>%
  mutate(
    lci = `2.5%` * 100,
    median = `50%` * 100,
    uci = `97.5%` * 100,
    ID = "RedSus",
    # factor(ID, levels = c("Reference", "Sbase_serologocal_CI", "S44_serologocal_CI", "RedSus_serologocal_CI")),
    group = "RedSus"
    # factor(group, levels = c("Reference", "Group 1", "Group 2", "Group 3", "Group 4"))
  ) %>%
  dplyr::select(lci, median, uci, ID, group)

Serologocal_CI_RedSus <- rbind(
  Serologocal_CI[c("Sbase_serologocal_CI", "S44_serologocal_CI", "1"), ],
  RedSus_serologocal_CI
)


### Plot results
Serologocal_Fig_RedSus <- ggplot(Serologocal_CI_RedSus, aes(x = ID, y = median, ymin = lci, ymax = uci, group = group)) +
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

ggsave(Serologocal_Fig_RedSus, file = paste0(RedSus_path, "1a.Serologocal_RedSus.pdf"), width = 12, height = 8)





#################################
## Age-specific infection rate
#################################
Parallel.Regist(10)
Parallel.Import(list(
  "CLI.Print", "Get.InitState", "Get.StateName", "Model.GetI", "Model.RunSim",
  "Parameter.Create", "RefDat", "Scot_Pop", "ContacrStr",
  "MCMC.MH", "MCMC.PosteriorSample"
))
IR_RedSus <- parLapply(ParallelNodesInfo[[1]], Posteriori_Sample_Redsus, \(sample) {
  setDTthreads(1)

  SimResult <- Model.RunSim(Parm = Parameter.Create(
    beta_base = sample[1],
    beta_seasonal = sample[2],
    phi = sample[3],
    seasonal_wavelength = sample[4],
    Hosp_rate = rep(1, 11),
    ReducedSus_1 = 0.76, ReducedSus_2 = 0.88,
    Age_Sus = c(1.0, 1.0)
  ), lag = FALSE)[[2]]

  SimResult <- SimResult[time >= as.Date("2019-06-20")]
  SimResult[, (3:13) := lapply(.SD, sum), .SDcols = 3:13]
  SimResult <- as.numeric(SimResult[1, 3:13]) / Scot_Pop

  return(SimResult)
})
Parallel.Stop()

IR_RedSus <- do.call(rbind, IR_RedSus)

IR_RedSus <- IR_RedSus %>%
  apply(., 2, \(x){
    quantile(x, c(0.025, 0.5, 0.975))
  }) %>%
  t() %>%
  as.data.frame()
IR_RedSus$age_group <- rownames(IR_RedSus)
IR_RedSus$Scenario <- "RedSus"
colnames(IR_RedSus) <- c("lci", "median", "uci", "age_group", "Scenario")
IR_RedSus <- IR_RedSus %>%
  mutate(
    age_group = factor(age_group,
      levels = c(
        "0-2m", "3-5m", "6-11m", "1-<2y", "2-<5y", "5-<19y",
        "19-<60y", "60-64y", "65-69y", "70-74y", "75y"
      )
    )
  )


IR_RedSus_Combine <- rbind(IR_base, IR_44, IR_RedSus)
IR_RedSus_Combine$Group <- c(
  rep("Group 1", 11),
  rep("Group 2", 11),
  rep("Group 3", 11)
)

IR_RedSus_Combine <- IR_RedSus_Combine %>%
  mutate(
    Scenario = factor(Scenario, levels = c(
      "Base", "S44", "RedSus"
    ), labels = c(
      "Base", "Scenario 1", "RedSus"
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


IR_RedSus_Fig <- IR_RedSus_Combine %>%
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
    "#1A5354FF", "#FF6F00FF", "#05b348"
  )) +
  scale_color_manual(values = c(
    "#1A5354FF", "#FF6F00FF", "#05b348"
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
ggsave(IR_RedSus_Fig, file = paste0(RedSus_path, "1b.IR_RedSus.pdf"), width = 12, height = 8)




#################################
## Infection-hospitalisation ratio
#################################
RedSus_I2H <- I2H.Batch(
  MCMC_Result = PosterioriCheck_Redsus, VacAgeGroup = "S3", Age_Sus = c(1.0, 1.0),
  ReducedSus_1 = 0.76, ReducedSus_2 = 0.88,
  Vac_start = "2019-08-30", VacProp = 0.8, lag = FALSE
)

IR_ratio_RedSus <- rbind(
  SBase_E6C6A[[1]],
  S44_E6C6A[[1]],
  RedSus_I2H
)
IR_ratio_RedSus <- IR_ratio_RedSus %>% as.data.frame(IR_ratio_RedSus)
IR_ratio_RedSus$age_group <- rep(c(1:11), 3)
IR_ratio_RedSus$Scenario <- rep(
  c(
    "SBase", "S44", "RedSus"
  ),
  each = 11
)
colnames(IR_ratio_RedSus) <- c("lci", "median", "uci", "age_group", "Scenario")
IR_ratio_RedSus <- IR_ratio_RedSus %>%
  mutate(Scenario = factor(Scenario, levels = c(
    "SBase", "S44", "RedSus"
  ), labels = c(
    "Base", "Scenario 1", "RedSus"
  )))


I2H_RedSus_Fig <- ggplot(IR_ratio_RedSus, aes(x = age_group, y = median, group = Scenario)) +
  geom_bar(aes(y = median, fill = Scenario), stat = "identity", position = "dodge") +
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
    "#1A5354FF", "#FF6F00FF", "#c72f09"
  )) +
  theme(
    axis.text = element_text(size = 20, face = "bold"),
    axis.title = element_text(size = 24, face = "bold"),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.text.x = element_text(margin = margin(b = 5)),
    legend.title = element_text(size = 20, face = "bold"),
    legend.text = element_text(size = 20),
    legend.position = c(0.93, 0.75)
  )

ggsave(I2H_RedSus_Fig, file = paste0(RedSus_path, "1c.I2H_RedSUs.pdf"), width = 12, height = 8)




RedSus_E7C8A <- Vac.Batch(
  MCMC_Result = PosterioriCheck_Redsus, VacAgeGroup = "S2", Age_Sus = c(1.0, 1.0),
  Vac_start = "2019-08-30", Effacy_I = 0.7, Effacy_Hosp = 0.8, VacProp = 0.8,
  lag = FALSE, ReducedSus_1 = 0.76, ReducedSus_2 = 0.88,
  Plot = TRUE, save = TRUE, path = paste0(RedSus_path, "RedSus_E7C8A"),
  width = 14, height = 8
)
