### Base scenario ###########################################################################################
### Coverage: 60%, A+B --------------------------------------------------------------------------------------
#-----------------------------------------------------------
# efficacy: 60%; coverage: 60%; A+B
#-----------------------------------------------------------
SBaseM_E6C6A <- Vac.Batch(
    MCMC_Result = PosterioriCheck_Sbase, Prop_I2H = S44_E6C6A[[1]], VacAgeGroup = "S2", Age_Sus = c(1, 1),
    Vac_start = "2019-08-30", Effacy_I = 0.6, Effacy_Hosp = 0.7,
    VacProp = 0.6, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "SBaseM_E6C6A"),
    width = 14, height = 8
)

#-----------------------------------------------------------
# efficacy: 70%: coverage: 60%, A+B
#-----------------------------------------------------------
SBaseM_E7C6A <- Vac.Batch(
    MCMC_Result = PosterioriCheck_Sbase, Prop_I2H = S44_E6C6A[[1]], VacAgeGroup = "S2", Age_Sus = c(1, 1),
    Vac_start = "2019-08-30", Effacy_I = 0.7, Effacy_Hosp = 0.8,
    VacProp = 0.6, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "SBaseM_E7C6A"),
    width = 14, height = 8
)

#-----------------------------------------------------------
# efficacy: 80%: coverage: 60%, A+B
#-----------------------------------------------------------
SBaseM_E8C6A <- Vac.Batch(
    MCMC_Result = PosterioriCheck_Sbase, Prop_I2H = S44_E6C6A[[1]], VacAgeGroup = "S2", Age_Sus = c(1, 1),
    Vac_start = "2019-08-30", Effacy_I = 0.8, Effacy_Hosp = 0.9,
    VacProp = 0.6, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "SBaseM_E8C6A"),
    width = 14, height = 8
)



### Coverage: 80%, A+B --------------------------------------------------------------------------------------
#-----------------------------------------------------------
# efficacy: 60%; coverage: 80%; A+B
#-----------------------------------------------------------
SBaseM_E6C8A <- Vac.Batch(
    MCMC_Result = PosterioriCheck_Sbase, Prop_I2H = S44_E6C6A[[1]], VacAgeGroup = "S2", Age_Sus = c(1, 1),
    Vac_start = "2019-08-30", Effacy_I = 0.6, Effacy_Hosp = 0.7,
    VacProp = 0.8, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "SBaseM_E6C8A"),
    width = 14, height = 8
)

#-----------------------------------------------------------
# efficacy: 70%: coverage: 80%, A+B
#-----------------------------------------------------------
SBaseM_E7C8A <- Vac.Batch(
    MCMC_Result = PosterioriCheck_Sbase, Prop_I2H = S44_E6C6A[[1]], VacAgeGroup = "S2", Age_Sus = c(1, 1),
    Vac_start = "2019-08-30", Effacy_I = 0.7, Effacy_Hosp = 0.8,
    VacProp = 0.8, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "SBaseM_E7C8A"),
    width = 14, height = 8
)

#-----------------------------------------------------------
# efficacy: 80%: coverage: 80%, A+B
#-----------------------------------------------------------
SBaseM_E8C8A <- Vac.Batch(
    MCMC_Result = PosterioriCheck_Sbase, Prop_I2H = S44_E6C6A[[1]], VacAgeGroup = "S2", Age_Sus = c(1, 1),
    Vac_start = "2019-08-30", Effacy_I = 0.8, Effacy_Hosp = 0.9,
    VacProp = 0.8, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "SBaseM_E8C8A"),
    width = 14, height = 8
)



### Coverage: 60%, A+B+C --------------------------------------------------------------------------------------
#-----------------------------------------------------------
# efficacy: 60%; coverage: 60%; coverage, A+B+C
#-----------------------------------------------------------
SBaseM_E6C6B <- Vac.Batch(
    MCMC_Result = PosterioriCheck_Sbase, Prop_I2H = S44_E6C6A[[1]], VacAgeGroup = "S3", Age_Sus = c(1, 1),
    Vac_start = "2019-08-30", Effacy_I = 0.6, Effacy_Hosp = 0.7,
    VacProp = 0.6, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "SBaseM_E6C6B"),
    width = 14, height = 8
)

#-----------------------------------------------------------
# efficacy: 70%: coverage: 60%, A+B+C
#-----------------------------------------------------------
SBaseM_E7C6B <- Vac.Batch(
    MCMC_Result = PosterioriCheck_Sbase, Prop_I2H = S44_E6C6A[[1]], VacAgeGroup = "S3", Age_Sus = c(1, 1),
    Vac_start = "2019-08-30", Effacy_I = 0.7, Effacy_Hosp = 0.8,
    VacProp = 0.6, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "SBaseM_E7C6B"),
    width = 14, height = 8
)

#-----------------------------------------------------------
# efficacy: 80%: coverage: 60%, A+B+C
#-----------------------------------------------------------
SBaseM_E8C6B <- Vac.Batch(
    MCMC_Result = PosterioriCheck_Sbase, Prop_I2H = S44_E6C6A[[1]], VacAgeGroup = "S3", Age_Sus = c(1, 1),
    Vac_start = "2019-08-30", Effacy_I = 0.8, Effacy_Hosp = 0.9,
    VacProp = 0.6, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "SBaseM_E8C6B"),
    width = 14, height = 8
)


### Coverage: 80%, A+B+C --------------------------------------------------------------------------------------
#-----------------------------------------------------------
# efficacy: 60%; coverage: 80%; coverage, A+B+C
#-----------------------------------------------------------
SBaseM_E6C8B <- Vac.Batch(
    MCMC_Result = PosterioriCheck_Sbase, Prop_I2H = S44_E6C6A[[1]], VacAgeGroup = "S3", Age_Sus = c(1, 1),
    Vac_start = "2019-08-30", Effacy_I = 0.6, Effacy_Hosp = 0.7,
    VacProp = 0.8, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "SBaseM_E6C8B"),
    width = 14, height = 8
)

#-----------------------------------------------------------
# efficacy: 70%: coverage: 80%, A+B+C
#-----------------------------------------------------------
SBaseM_E7C8B <- Vac.Batch(
    MCMC_Result = PosterioriCheck_Sbase, Prop_I2H = S44_E6C6A[[1]], VacAgeGroup = "S3", Age_Sus = c(1, 1),
    Vac_start = "2019-08-30", Effacy_I = 0.7, Effacy_Hosp = 0.8,
    VacProp = 0.8, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "SBaseM_E7C8B"),
    width = 14, height = 8
)

#-----------------------------------------------------------
# efficacy: 80%: coverage: 80%, A+B+C
#-----------------------------------------------------------
SBaseM_E8C8B <- Vac.Batch(
    MCMC_Result = PosterioriCheck_Sbase, Prop_I2H = S44_E6C6A[[1]], VacAgeGroup = "S3", Age_Sus = c(1, 1),
    Vac_start = "2019-08-30", Effacy_I = 0.8, Effacy_Hosp = 0.9,
    VacProp = 0.8, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "SBaseM_E8C8B"),
    width = 14, height = 8
)



### Coverage: 60%, A+B+C+D --------------------------------------------------------------------------------------
#-----------------------------------------------------------
# efficacy: 60%; coverage: 60%; coverage, A+B+C+D
#-----------------------------------------------------------
SBaseM_E6C6C <- Vac.Batch(
    MCMC_Result = PosterioriCheck_Sbase, Prop_I2H = S44_E6C6A[[1]], VacAgeGroup = "S4", Age_Sus = c(1, 1),
    Vac_start = "2019-08-30", Effacy_I = 0.6, Effacy_Hosp = 0.7,
    VacProp = 0.6, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "SBaseM_E6C6C"),
    width = 14, height = 8
)

#-----------------------------------------------------------
# efficacy: 70%: coverage: 60%, A+B+C+D
#-----------------------------------------------------------
SBaseM_E7C6C <- Vac.Batch(
    MCMC_Result = PosterioriCheck_Sbase, Prop_I2H = S44_E6C6A[[1]], VacAgeGroup = "S4", Age_Sus = c(1, 1),
    Vac_start = "2019-08-30", Effacy_I = 0.7, Effacy_Hosp = 0.8,
    VacProp = 0.6, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "SBaseM_E7C6C"),
    width = 14, height = 8
)

#-----------------------------------------------------------
# efficacy: 80%: coverage: 60%, A+B+C+D
#-----------------------------------------------------------
SBaseM_E8C6C <- Vac.Batch(
    MCMC_Result = PosterioriCheck_Sbase, Prop_I2H = S44_E6C6A[[1]], VacAgeGroup = "S4", Age_Sus = c(1, 1),
    Vac_start = "2019-08-30", Effacy_I = 0.8, Effacy_Hosp = 0.9,
    VacProp = 0.6, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "SBaseM_E8C6C"),
    width = 14, height = 8
)


### Coverage: 80%, A+B+C+D --------------------------------------------------------------------------------------
#-----------------------------------------------------------
# efficacy: 60%; coverage: 80%; coverage, A+B+C+D
#-----------------------------------------------------------
SBaseM_E6C8C <- Vac.Batch(
    MCMC_Result = PosterioriCheck_Sbase, Prop_I2H = S44_E6C6A[[1]], VacAgeGroup = "S4", Age_Sus = c(1, 1),
    Vac_start = "2019-08-30", Effacy_I = 0.6, Effacy_Hosp = 0.7,
    VacProp = 0.8, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "SBaseM_E6C8C"),
    width = 14, height = 8
)

#-----------------------------------------------------------
# efficacy: 70%: coverage: 80%, A+B+C+D
#-----------------------------------------------------------
SBaseM_E7C8C <- Vac.Batch(
    MCMC_Result = PosterioriCheck_Sbase, Prop_I2H = S44_E6C6A[[1]], VacAgeGroup = "S4", Age_Sus = c(1, 1),
    Vac_start = "2019-08-30", Effacy_I = 0.7, Effacy_Hosp = 0.8,
    VacProp = 0.8, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "SBaseM_E7C8C"),
    width = 14, height = 8
)

#-----------------------------------------------------------
# efficacy: 80%: coverage: 80%, A+B+C+D
#-----------------------------------------------------------
SBaseM_E8C8C <- Vac.Batch(
    MCMC_Result = PosterioriCheck_Sbase, Prop_I2H = S44_E6C6A[[1]], VacAgeGroup = "S4", Age_Sus = c(1, 1),
    Vac_start = "2019-08-30", Effacy_I = 0.8, Effacy_Hosp = 0.9,
    VacProp = 0.8, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "SBaseM_E8C8C"),
    width = 14, height = 8
)
