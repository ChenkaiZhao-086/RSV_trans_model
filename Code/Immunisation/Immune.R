sourceCpp("Code/model_immuV4.cpp")

ImmuPath <- File.CreateSubFolder(FilePath, "Immunisation")

## efficacy: 60, 70, 80

## coverage: 60, 80

## A. 0-5m; B. 6-11m; C. 12-23m; D. 2-4y;
## A(S2): A+B; B(S3): A+B+C; C(S4): A+B+C+D

### Base scenario ###########################################################################################
### Coverage: 60%, A+B --------------------------------------------------------------------------------------
#-----------------------------------------------------------
# efficacy: 60%; coverage: 60%; A+B
#-----------------------------------------------------------
SBase_E6C6A <- Vac.Batch(
    MCMC_Result = PosterioriCheck_Sbase, VacAgeGroup = "S2", Age_Sus = c(1, 1),
    Vac_start = "2019-08-30", Effacy_I = 0.6, Effacy_Hosp = 0.7,
    VacProp = 0.6, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "SBase_E6C6A"),
    width = 14, height = 8
)

#-----------------------------------------------------------
# efficacy: 70%: coverage: 60%, A+B
#-----------------------------------------------------------
SBase_E7C6A <- Vac.Batch(
    MCMC_Result = PosterioriCheck_Sbase, VacAgeGroup = "S2", Age_Sus = c(1, 1),
    Vac_start = "2019-08-30", Effacy_I = 0.7, Effacy_Hosp = 0.8,
    VacProp = 0.6, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "SBase_E7C6A"),
    width = 14, height = 8
)

#-----------------------------------------------------------
# efficacy: 80%: coverage: 60%, A+B
#-----------------------------------------------------------
SBase_E8C6A <- Vac.Batch(
    MCMC_Result = PosterioriCheck_Sbase, VacAgeGroup = "S2", Age_Sus = c(1, 1),
    Vac_start = "2019-08-30", Effacy_I = 0.8, Effacy_Hosp = 0.9,
    VacProp = 0.6, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "SBase_E8C6A"),
    width = 14, height = 8
)



### Coverage: 80%, A+B --------------------------------------------------------------------------------------
#-----------------------------------------------------------
# efficacy: 60%; coverage: 80%; A+B
#-----------------------------------------------------------
SBase_E6C8A <- Vac.Batch(
    MCMC_Result = PosterioriCheck_Sbase, VacAgeGroup = "S2", Age_Sus = c(1, 1),
    Vac_start = "2019-08-30", Effacy_I = 0.6, Effacy_Hosp = 0.7,
    VacProp = 0.8, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "SBase_E6C8A"),
    width = 14, height = 8
)

#-----------------------------------------------------------
# efficacy: 70%: coverage: 80%, A+B
#-----------------------------------------------------------
SBase_E7C8A <- Vac.Batch(
    MCMC_Result = PosterioriCheck_Sbase, VacAgeGroup = "S2", Age_Sus = c(1, 1),
    Vac_start = "2019-08-30", Effacy_I = 0.7, Effacy_Hosp = 0.8,
    VacProp = 0.8, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "SBase_E7C8A"),
    width = 14, height = 8
)

#-----------------------------------------------------------
# efficacy: 80%: coverage: 80%, A+B
#-----------------------------------------------------------
SBase_E8C8A <- Vac.Batch(
    MCMC_Result = PosterioriCheck_Sbase, VacAgeGroup = "S2", Age_Sus = c(1, 1),
    Vac_start = "2019-08-30", Effacy_I = 0.8, Effacy_Hosp = 0.9,
    VacProp = 0.8, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "SBase_E8C8A"),
    width = 14, height = 8
)



### Coverage: 60%, A+B+C --------------------------------------------------------------------------------------
#-----------------------------------------------------------
# efficacy: 60%; coverage: 60%; coverage, A+B+C
#-----------------------------------------------------------
SBase_E6C6B <- Vac.Batch(
    MCMC_Result = PosterioriCheck_Sbase, VacAgeGroup = "S3", Age_Sus = c(1, 1),
    Vac_start = "2019-08-30", Effacy_I = 0.6, Effacy_Hosp = 0.7,
    VacProp = 0.6, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "SBase_E6C6B"),
    width = 14, height = 8
)

#-----------------------------------------------------------
# efficacy: 70%: coverage: 60%, A+B+C
#-----------------------------------------------------------
SBase_E7C6B <- Vac.Batch(
    MCMC_Result = PosterioriCheck_Sbase, VacAgeGroup = "S3", Age_Sus = c(1, 1),
    Vac_start = "2019-08-30", Effacy_I = 0.7, Effacy_Hosp = 0.8,
    VacProp = 0.6, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "SBase_E7C6B"),
    width = 14, height = 8
)

#-----------------------------------------------------------
# efficacy: 80%: coverage: 60%, A+B+C
#-----------------------------------------------------------
SBase_E8C6B <- Vac.Batch(
    MCMC_Result = PosterioriCheck_Sbase, VacAgeGroup = "S3", Age_Sus = c(1, 1),
    Vac_start = "2019-08-30", Effacy_I = 0.8, Effacy_Hosp = 0.9,
    VacProp = 0.6, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "SBase_E8C6B"),
    width = 14, height = 8
)


### Coverage: 80%, A+B+C --------------------------------------------------------------------------------------
#-----------------------------------------------------------
# efficacy: 60%; coverage: 80%; coverage, A+B+C
#-----------------------------------------------------------
SBase_E6C8B <- Vac.Batch(
    MCMC_Result = PosterioriCheck_Sbase, VacAgeGroup = "S3", Age_Sus = c(1, 1),
    Vac_start = "2019-08-30", Effacy_I = 0.6, Effacy_Hosp = 0.7,
    VacProp = 0.8, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "SBase_E6C8B"),
    width = 14, height = 8
)

#-----------------------------------------------------------
# efficacy: 70%: coverage: 80%, A+B+C
#-----------------------------------------------------------
SBase_E7C8B <- Vac.Batch(
    MCMC_Result = PosterioriCheck_Sbase, VacAgeGroup = "S3", Age_Sus = c(1, 1),
    Vac_start = "2019-08-30", Effacy_I = 0.7, Effacy_Hosp = 0.8,
    VacProp = 0.8, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "SBase_E7C8B"),
    width = 14, height = 8
)

#-----------------------------------------------------------
# efficacy: 80%: coverage: 80%, A+B+C
#-----------------------------------------------------------
SBase_E8C8B <- Vac.Batch(
    MCMC_Result = PosterioriCheck_Sbase, VacAgeGroup = "S3", Age_Sus = c(1, 1),
    Vac_start = "2019-08-30", Effacy_I = 0.8, Effacy_Hosp = 0.9,
    VacProp = 0.8, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "SBase_E8C8B"),
    width = 14, height = 8
)



### Coverage: 60%, A+B+C+D --------------------------------------------------------------------------------------
#-----------------------------------------------------------
# efficacy: 60%; coverage: 60%; coverage, A+B+C+D
#-----------------------------------------------------------
SBase_E6C6C <- Vac.Batch(
    MCMC_Result = PosterioriCheck_Sbase, VacAgeGroup = "S4", Age_Sus = c(1, 1),
    Vac_start = "2019-08-30", Effacy_I = 0.6, Effacy_Hosp = 0.7,
    VacProp = 0.6, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "SBase_E6C6C"),
    width = 14, height = 8
)

#-----------------------------------------------------------
# efficacy: 70%: coverage: 60%, A+B+C+D
#-----------------------------------------------------------
SBase_E7C6C <- Vac.Batch(
    MCMC_Result = PosterioriCheck_Sbase, VacAgeGroup = "S4", Age_Sus = c(1, 1),
    Vac_start = "2019-08-30", Effacy_I = 0.7, Effacy_Hosp = 0.8,
    VacProp = 0.6, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "SBase_E7C6C"),
    width = 14, height = 8
)

#-----------------------------------------------------------
# efficacy: 80%: coverage: 60%, A+B+C+D
#-----------------------------------------------------------
SBase_E8C6C <- Vac.Batch(
    MCMC_Result = PosterioriCheck_Sbase, VacAgeGroup = "S4", Age_Sus = c(1, 1),
    Vac_start = "2019-08-30", Effacy_I = 0.8, Effacy_Hosp = 0.9,
    VacProp = 0.6, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "SBase_E8C6C"),
    width = 14, height = 8
)


### Coverage: 80%, A+B+C+D --------------------------------------------------------------------------------------
#-----------------------------------------------------------
# efficacy: 60%; coverage: 80%; coverage, A+B+C+D
#-----------------------------------------------------------
SBase_E6C8C <- Vac.Batch(
    MCMC_Result = PosterioriCheck_Sbase, VacAgeGroup = "S4", Age_Sus = c(1, 1),
    Vac_start = "2019-08-30", Effacy_I = 0.6, Effacy_Hosp = 0.7,
    VacProp = 0.8, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "SBase_E6C8C"),
    width = 14, height = 8
)

#-----------------------------------------------------------
# efficacy: 70%: coverage: 80%, A+B+C+D
#-----------------------------------------------------------
SBase_E7C8C <- Vac.Batch(
    MCMC_Result = PosterioriCheck_Sbase, VacAgeGroup = "S4", Age_Sus = c(1, 1),
    Vac_start = "2019-08-30", Effacy_I = 0.7, Effacy_Hosp = 0.8,
    VacProp = 0.8, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "SBase_E7C8C"),
    width = 14, height = 8
)

#-----------------------------------------------------------
# efficacy: 80%: coverage: 80%, A+B+C+D
#-----------------------------------------------------------
SBase_E8C8C <- Vac.Batch(
    MCMC_Result = PosterioriCheck_Sbase, VacAgeGroup = "S4", Age_Sus = c(1, 1),
    Vac_start = "2019-08-30", Effacy_I = 0.8, Effacy_Hosp = 0.9,
    VacProp = 0.8, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "SBase_E8C8C"),
    width = 14, height = 8
)





### S42 ###########################################################################################
### Coverage: 60%, A+B --------------------------------------------------------------------------------------
#-----------------------------------------------------------
# efficacy: 60%; coverage: 60%; A+B
#-----------------------------------------------------------
S42_E6C6A <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S42, VacAgeGroup = "S2", Age_Sus = c(0.4, 0.2),
    Vac_start = "2019-08-30", Effacy_I = 0.6, Effacy_Hosp = 0.7,
    VacProp = 0.6, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "S42_E6C6A"),
    width = 14, height = 8
)

#-----------------------------------------------------------
# efficacy: 70%: coverage: 60%, A+B
#-----------------------------------------------------------
S42_E7C6A <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S42, VacAgeGroup = "S2", Age_Sus = c(0.4, 0.2),
    Vac_start = "2019-08-30", Effacy_I = 0.7, Effacy_Hosp = 0.8,
    VacProp = 0.6, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "S42_E7C6A"),
    width = 14, height = 8
)

#-----------------------------------------------------------
# efficacy: 80%: coverage: 60%, A+B
#-----------------------------------------------------------
S42_E8C6A <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S42, VacAgeGroup = "S2", Age_Sus = c(0.4, 0.2),
    Vac_start = "2019-08-30", Effacy_I = 0.8, Effacy_Hosp = 0.9,
    VacProp = 0.6, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "S42_E8C6A"),
    width = 14, height = 8
)



### Coverage: 80%, A+B --------------------------------------------------------------------------------------
#-----------------------------------------------------------
# efficacy: 60%; coverage: 80%; A+B
#-----------------------------------------------------------
S42_E6C8A <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S42, VacAgeGroup = "S2", Age_Sus = c(0.4, 0.2),
    Vac_start = "2019-08-30", Effacy_I = 0.6, Effacy_Hosp = 0.7,
    VacProp = 0.8, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "S42_E6C8A"),
    width = 14, height = 8
)

#-----------------------------------------------------------
# efficacy: 70%: coverage: 80%, A+B
#-----------------------------------------------------------
S42_E7C8A <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S42, VacAgeGroup = "S2", Age_Sus = c(0.4, 0.2),
    Vac_start = "2019-08-30", Effacy_I = 0.7, Effacy_Hosp = 0.8,
    VacProp = 0.8, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "S42_E7C8A"),
    width = 14, height = 8
)

#-----------------------------------------------------------
# efficacy: 80%: coverage: 80%, A+B
#-----------------------------------------------------------
S42_E8C8A <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S42, VacAgeGroup = "S2", Age_Sus = c(0.4, 0.2),
    Vac_start = "2019-08-30", Effacy_I = 0.8, Effacy_Hosp = 0.9,
    VacProp = 0.8, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "S42_E8C8A"),
    width = 14, height = 8
)



### Coverage: 60%, A+B+C --------------------------------------------------------------------------------------
#-----------------------------------------------------------
# efficacy: 60%; coverage: 60%; coverage, A+B+C
#-----------------------------------------------------------
S42_E6C6B <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S42, VacAgeGroup = "S3", Age_Sus = c(0.4, 0.2),
    Vac_start = "2019-08-30", Effacy_I = 0.6, Effacy_Hosp = 0.7,
    VacProp = 0.6, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "S42_E6C6B"),
    width = 14, height = 8
)

#-----------------------------------------------------------
# efficacy: 70%: coverage: 60%, A+B+C
#-----------------------------------------------------------
S42_E7C6B <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S42, VacAgeGroup = "S3", Age_Sus = c(0.4, 0.2),
    Vac_start = "2019-08-30", Effacy_I = 0.7, Effacy_Hosp = 0.8,
    VacProp = 0.6, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "S42_E7C6B"),
    width = 14, height = 8
)

#-----------------------------------------------------------
# efficacy: 80%: coverage: 60%, A+B+C
#-----------------------------------------------------------
S42_E8C6B <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S42, VacAgeGroup = "S3", Age_Sus = c(0.4, 0.2),
    Vac_start = "2019-08-30", Effacy_I = 0.8, Effacy_Hosp = 0.9,
    VacProp = 0.6, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "S42_E8C6B"),
    width = 14, height = 8
)


### Coverage: 80%, A+B+C --------------------------------------------------------------------------------------
#-----------------------------------------------------------
# efficacy: 60%; coverage: 80%; coverage, A+B+C
#-----------------------------------------------------------
S42_E6C8B <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S42, VacAgeGroup = "S3", Age_Sus = c(0.4, 0.2),
    Vac_start = "2019-08-30", Effacy_I = 0.6, Effacy_Hosp = 0.7,
    VacProp = 0.8, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "S42_E6C8B"),
    width = 14, height = 8
)

#-----------------------------------------------------------
# efficacy: 70%: coverage: 80%, A+B+C
#-----------------------------------------------------------
S42_E7C8B <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S42, VacAgeGroup = "S3", Age_Sus = c(0.4, 0.2),
    Vac_start = "2019-08-30", Effacy_I = 0.7, Effacy_Hosp = 0.8,
    VacProp = 0.8, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "S42_E7C8B"),
    width = 14, height = 8
)

#-----------------------------------------------------------
# efficacy: 80%: coverage: 80%, A+B+C
#-----------------------------------------------------------
S42_E8C8B <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S42, VacAgeGroup = "S3", Age_Sus = c(0.4, 0.2),
    Vac_start = "2019-08-30", Effacy_I = 0.8, Effacy_Hosp = 0.9,
    VacProp = 0.8, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "S42_E8C8B"),
    width = 14, height = 8
)



### Coverage: 60%, A+B+C+D --------------------------------------------------------------------------------------
#-----------------------------------------------------------
# efficacy: 60%; coverage: 60%; coverage, A+B+C+D
#-----------------------------------------------------------
S42_E6C6C <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S42, VacAgeGroup = "S4", Age_Sus = c(0.4, 0.2),
    Vac_start = "2019-08-30", Effacy_I = 0.6, Effacy_Hosp = 0.7,
    VacProp = 0.6, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "S42_E6C6C"),
    width = 14, height = 8
)

#-----------------------------------------------------------
# efficacy: 70%: coverage: 60%, A+B+C+D
#-----------------------------------------------------------
S42_E7C6C <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S42, VacAgeGroup = "S4", Age_Sus = c(0.4, 0.2),
    Vac_start = "2019-08-30", Effacy_I = 0.7, Effacy_Hosp = 0.8,
    VacProp = 0.6, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "S42_E7C6C"),
    width = 14, height = 8
)

#-----------------------------------------------------------
# efficacy: 80%: coverage: 60%, A+B+C+D
#-----------------------------------------------------------
S42_E8C6C <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S42, VacAgeGroup = "S4", Age_Sus = c(0.4, 0.2),
    Vac_start = "2019-08-30", Effacy_I = 0.8, Effacy_Hosp = 0.9,
    VacProp = 0.6, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "S42_E8C6C"),
    width = 14, height = 8
)


### Coverage: 80%, A+B+C+D --------------------------------------------------------------------------------------
#-----------------------------------------------------------
# efficacy: 60%; coverage: 80%; coverage, A+B+C+D
#-----------------------------------------------------------
S42_E6C8C <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S42, VacAgeGroup = "S4", Age_Sus = c(0.4, 0.2),
    Vac_start = "2019-08-30", Effacy_I = 0.6, Effacy_Hosp = 0.7,
    VacProp = 0.8, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "S42_E6C8C"),
    width = 14, height = 8
)

#-----------------------------------------------------------
# efficacy: 70%: coverage: 80%, A+B+C+D
#-----------------------------------------------------------
S42_E7C8C <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S42, VacAgeGroup = "S4", Age_Sus = c(0.4, 0.2),
    Vac_start = "2019-08-30", Effacy_I = 0.7, Effacy_Hosp = 0.8,
    VacProp = 0.8, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "S42_E7C8C"),
    width = 14, height = 8
)

#-----------------------------------------------------------
# efficacy: 80%: coverage: 80%, A+B+C+D
#-----------------------------------------------------------
S42_E8C8C <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S42, VacAgeGroup = "S4", Age_Sus = c(0.4, 0.2),
    Vac_start = "2019-08-30", Effacy_I = 0.8, Effacy_Hosp = 0.9,
    VacProp = 0.8, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "S42_E8C8C"),
    width = 14, height = 8
)





### S43 ###########################################################################################
### Coverage: 60%, A+B --------------------------------------------------------------------------------------
#-----------------------------------------------------------
# efficacy: 60%; coverage: 60%; A+B
#-----------------------------------------------------------
S43_E6C6A <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S43, VacAgeGroup = "S2", Age_Sus = c(0.4, 0.3),
    Vac_start = "2019-08-30", Effacy_I = 0.6, Effacy_Hosp = 0.7,
    VacProp = 0.6, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "S43_E6C6A"),
    width = 14, height = 8
)

#-----------------------------------------------------------
# efficacy: 70%: coverage: 60%, A+B
#-----------------------------------------------------------
S43_E7C6A <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S43, VacAgeGroup = "S2", Age_Sus = c(0.4, 0.3),
    Vac_start = "2019-08-30", Effacy_I = 0.7, Effacy_Hosp = 0.8,
    VacProp = 0.6, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "S43_E7C6A"),
    width = 14, height = 8
)

#-----------------------------------------------------------
# efficacy: 80%: coverage: 60%, A+B
#-----------------------------------------------------------
S43_E8C6A <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S43, VacAgeGroup = "S2", Age_Sus = c(0.4, 0.3),
    Vac_start = "2019-08-30", Effacy_I = 0.8, Effacy_Hosp = 0.9,
    VacProp = 0.6, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "S43_E8C6A"),
    width = 14, height = 8
)



### Coverage: 80%, A+B --------------------------------------------------------------------------------------
#-----------------------------------------------------------
# efficacy: 60%; coverage: 80%; A+B
#-----------------------------------------------------------
S43_E6C8A <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S43, VacAgeGroup = "S2", Age_Sus = c(0.4, 0.3),
    Vac_start = "2019-08-30", Effacy_I = 0.6, Effacy_Hosp = 0.7,
    VacProp = 0.8, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "S43_E6C8A"),
    width = 14, height = 8
)

#-----------------------------------------------------------
# efficacy: 70%: coverage: 80%, A+B
#-----------------------------------------------------------
S43_E7C8A <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S43, VacAgeGroup = "S2", Age_Sus = c(0.4, 0.3),
    Vac_start = "2019-08-30", Effacy_I = 0.7, Effacy_Hosp = 0.8,
    VacProp = 0.8, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "S43_E7C8A"),
    width = 14, height = 8
)

#-----------------------------------------------------------
# efficacy: 80%: coverage: 80%, A+B
#-----------------------------------------------------------
S43_E8C8A <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S43, VacAgeGroup = "S2", Age_Sus = c(0.4, 0.3),
    Vac_start = "2019-08-30", Effacy_I = 0.8, Effacy_Hosp = 0.9,
    VacProp = 0.8, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "S43_E8C8A"),
    width = 14, height = 8
)



### Coverage: 60%, A+B+C --------------------------------------------------------------------------------------
#-----------------------------------------------------------
# efficacy: 60%; coverage: 60%; coverage, A+B+C
#-----------------------------------------------------------
S43_E6C6B <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S43, VacAgeGroup = "S3", Age_Sus = c(0.4, 0.3),
    Vac_start = "2019-08-30", Effacy_I = 0.6, Effacy_Hosp = 0.7,
    VacProp = 0.6, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "S43_E6C6B"),
    width = 14, height = 8
)

#-----------------------------------------------------------
# efficacy: 70%: coverage: 60%, A+B+C
#-----------------------------------------------------------
S43_E7C6B <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S43, VacAgeGroup = "S3", Age_Sus = c(0.4, 0.3),
    Vac_start = "2019-08-30", Effacy_I = 0.7, Effacy_Hosp = 0.8,
    VacProp = 0.6, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "S43_E7C6B"),
    width = 14, height = 8
)

#-----------------------------------------------------------
# efficacy: 80%: coverage: 60%, A+B+C
#-----------------------------------------------------------
S43_E8C6B <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S43, VacAgeGroup = "S3", Age_Sus = c(0.4, 0.3),
    Vac_start = "2019-08-30", Effacy_I = 0.8, Effacy_Hosp = 0.9,
    VacProp = 0.6, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "S43_E8C6B"),
    width = 14, height = 8
)


### Coverage: 80%, A+B+C --------------------------------------------------------------------------------------
#-----------------------------------------------------------
# efficacy: 60%; coverage: 80%; coverage, A+B+C
#-----------------------------------------------------------
S43_E6C8B <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S43, VacAgeGroup = "S3", Age_Sus = c(0.4, 0.3),
    Vac_start = "2019-08-30", Effacy_I = 0.6, Effacy_Hosp = 0.7,
    VacProp = 0.8, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "S43_E6C8B"),
    width = 14, height = 8
)

#-----------------------------------------------------------
# efficacy: 70%: coverage: 80%, A+B+C
#-----------------------------------------------------------
S43_E7C8B <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S43, VacAgeGroup = "S3", Age_Sus = c(0.4, 0.3),
    Vac_start = "2019-08-30", Effacy_I = 0.7, Effacy_Hosp = 0.8,
    VacProp = 0.8, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "S43_E7C8B"),
    width = 14, height = 8
)

#-----------------------------------------------------------
# efficacy: 80%: coverage: 80%, A+B+C
#-----------------------------------------------------------
S43_E8C8B <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S43, VacAgeGroup = "S3", Age_Sus = c(0.4, 0.3),
    Vac_start = "2019-08-30", Effacy_I = 0.8, Effacy_Hosp = 0.9,
    VacProp = 0.8, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "S43_E8C8B"),
    width = 14, height = 8
)



### Coverage: 60%, A+B+C+D --------------------------------------------------------------------------------------
#-----------------------------------------------------------
# efficacy: 60%; coverage: 60%; coverage, A+B+C+D
#-----------------------------------------------------------
S43_E6C6C <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S43, VacAgeGroup = "S4", Age_Sus = c(0.4, 0.3),
    Vac_start = "2019-08-30", Effacy_I = 0.6, Effacy_Hosp = 0.7,
    VacProp = 0.6, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "S43_E6C6C"),
    width = 14, height = 8
)

#-----------------------------------------------------------
# efficacy: 70%: coverage: 60%, A+B+C+D
#-----------------------------------------------------------
S43_E7C6C <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S43, VacAgeGroup = "S4", Age_Sus = c(0.4, 0.3),
    Vac_start = "2019-08-30", Effacy_I = 0.7, Effacy_Hosp = 0.8,
    VacProp = 0.6, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "S43_E7C6C"),
    width = 14, height = 8
)

#-----------------------------------------------------------
# efficacy: 80%: coverage: 60%, A+B+C+D
#-----------------------------------------------------------
S43_E8C6C <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S43, VacAgeGroup = "S4", Age_Sus = c(0.4, 0.3),
    Vac_start = "2019-08-30", Effacy_I = 0.8, Effacy_Hosp = 0.9,
    VacProp = 0.6, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "S43_E8C6C"),
    width = 14, height = 8
)


### Coverage: 80%, A+B+C+D --------------------------------------------------------------------------------------
#-----------------------------------------------------------
# efficacy: 60%; coverage: 80%; coverage, A+B+C+D
#-----------------------------------------------------------
S43_E6C8C <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S43, VacAgeGroup = "S4", Age_Sus = c(0.4, 0.3),
    Vac_start = "2019-08-30", Effacy_I = 0.6, Effacy_Hosp = 0.7,
    VacProp = 0.8, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "S43_E6C8C"),
    width = 14, height = 8
)

#-----------------------------------------------------------
# efficacy: 70%: coverage: 80%, A+B+C+D
#-----------------------------------------------------------
S43_E7C8C <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S43, VacAgeGroup = "S4", Age_Sus = c(0.4, 0.3),
    Vac_start = "2019-08-30", Effacy_I = 0.7, Effacy_Hosp = 0.8,
    VacProp = 0.8, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "S43_E7C8C"),
    width = 14, height = 8
)

#-----------------------------------------------------------
# efficacy: 80%: coverage: 80%, A+B+C+D
#-----------------------------------------------------------
S43_E8C8C <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S43, VacAgeGroup = "S4", Age_Sus = c(0.4, 0.3),
    Vac_start = "2019-08-30", Effacy_I = 0.8, Effacy_Hosp = 0.9,
    VacProp = 0.8, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "S43_E8C8C"),
    width = 14, height = 8
)




### S44 ###########################################################################################
### Coverage: 60%, A+B --------------------------------------------------------------------------------------
#-----------------------------------------------------------
# efficacy: 60%; coverage: 60%; A+B
#-----------------------------------------------------------
S44_E6C6A <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S44, VacAgeGroup = "S2", Age_Sus = c(0.4, 0.4),
    Vac_start = "2019-08-30", Effacy_I = 0.6, Effacy_Hosp = 0.7,
    VacProp = 0.6, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "S44_E6C6A"),
    width = 14, height = 8
)

#-----------------------------------------------------------
# efficacy: 70%: coverage: 60%, A+B
#-----------------------------------------------------------
S44_E7C6A <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S44, VacAgeGroup = "S2", Age_Sus = c(0.4, 0.4),
    Vac_start = "2019-08-30", Effacy_I = 0.7, Effacy_Hosp = 0.8,
    VacProp = 0.6, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "S44_E7C6A"),
    width = 14, height = 8
)

#-----------------------------------------------------------
# efficacy: 80%: coverage: 60%, A+B
#-----------------------------------------------------------
S44_E8C6A <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S44, VacAgeGroup = "S2", Age_Sus = c(0.4, 0.4),
    Vac_start = "2019-08-30", Effacy_I = 0.8, Effacy_Hosp = 0.9,
    VacProp = 0.6, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "S44_E8C6A"),
    width = 14, height = 8
)



### Coverage: 80%, A+B --------------------------------------------------------------------------------------
#-----------------------------------------------------------
# efficacy: 60%; coverage: 80%; A+B
#-----------------------------------------------------------
S44_E6C8A <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S44, VacAgeGroup = "S2", Age_Sus = c(0.4, 0.4),
    Vac_start = "2019-08-30", Effacy_I = 0.6, Effacy_Hosp = 0.7,
    VacProp = 0.8, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "S44_E6C8A"),
    width = 14, height = 8
)

#-----------------------------------------------------------
# efficacy: 70%: coverage: 80%, A+B
#-----------------------------------------------------------
S44_E7C8A <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S44, VacAgeGroup = "S2", Age_Sus = c(0.4, 0.4),
    Vac_start = "2019-08-30", Effacy_I = 0.7, Effacy_Hosp = 0.8,
    VacProp = 0.8, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "S44_E7C8A"),
    width = 14, height = 8
)

#-----------------------------------------------------------
# efficacy: 80%: coverage: 80%, A+B
#-----------------------------------------------------------
S44_E8C8A <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S44, VacAgeGroup = "S2", Age_Sus = c(0.4, 0.4),
    Vac_start = "2019-08-30", Effacy_I = 0.8, Effacy_Hosp = 0.9,
    VacProp = 0.8, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "S44_E8C8A"),
    width = 14, height = 8
)



### Coverage: 60%, A+B+C --------------------------------------------------------------------------------------
#-----------------------------------------------------------
# efficacy: 60%; coverage: 60%; coverage, A+B+C
#-----------------------------------------------------------
S44_E6C6B <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S44, VacAgeGroup = "S3", Age_Sus = c(0.4, 0.4),
    Vac_start = "2019-08-30", Effacy_I = 0.6, Effacy_Hosp = 0.7,
    VacProp = 0.6, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "S44_E6C6B"),
    width = 14, height = 8
)

#-----------------------------------------------------------
# efficacy: 70%: coverage: 60%, A+B+C
#-----------------------------------------------------------
S44_E7C6B <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S44, VacAgeGroup = "S3", Age_Sus = c(0.4, 0.4),
    Vac_start = "2019-08-30", Effacy_I = 0.7, Effacy_Hosp = 0.8,
    VacProp = 0.6, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "S44_E7C6B"),
    width = 14, height = 8
)

#-----------------------------------------------------------
# efficacy: 80%: coverage: 60%, A+B+C
#-----------------------------------------------------------
S44_E8C6B <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S44, VacAgeGroup = "S3", Age_Sus = c(0.4, 0.4),
    Vac_start = "2019-08-30", Effacy_I = 0.8, Effacy_Hosp = 0.9,
    VacProp = 0.6, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "S44_E8C6B"),
    width = 14, height = 8
)


### Coverage: 80%, A+B+C --------------------------------------------------------------------------------------
#-----------------------------------------------------------
# efficacy: 60%; coverage: 80%; coverage, A+B+C
#-----------------------------------------------------------
S44_E6C8B <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S44, VacAgeGroup = "S3", Age_Sus = c(0.4, 0.4),
    Vac_start = "2019-08-30", Effacy_I = 0.6, Effacy_Hosp = 0.7,
    VacProp = 0.8, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "S44_E6C8B"),
    width = 14, height = 8
)

#-----------------------------------------------------------
# efficacy: 70%: coverage: 80%, A+B+C
#-----------------------------------------------------------
S44_E7C8B <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S44, VacAgeGroup = "S3", Age_Sus = c(0.4, 0.4),
    Vac_start = "2019-08-30", Effacy_I = 0.7, Effacy_Hosp = 0.8,
    VacProp = 0.8, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "S44_E7C8B"),
    width = 14, height = 8
)

#-----------------------------------------------------------
# efficacy: 80%: coverage: 80%, A+B+C
#-----------------------------------------------------------
S44_E8C8B <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S44, VacAgeGroup = "S3", Age_Sus = c(0.4, 0.4),
    Vac_start = "2019-08-30", Effacy_I = 0.8, Effacy_Hosp = 0.9,
    VacProp = 0.8, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "S44_E8C8B"),
    width = 14, height = 8
)



### Coverage: 60%, A+B+C+D --------------------------------------------------------------------------------------
#-----------------------------------------------------------
# efficacy: 60%; coverage: 60%; coverage, A+B+C+D
#-----------------------------------------------------------
S44_E6C6C <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S44, VacAgeGroup = "S4", Age_Sus = c(0.4, 0.4),
    Vac_start = "2019-08-30", Effacy_I = 0.6, Effacy_Hosp = 0.7,
    VacProp = 0.6, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "S44_E6C6C"),
    width = 14, height = 8
)

#-----------------------------------------------------------
# efficacy: 70%: coverage: 60%, A+B+C+D
#-----------------------------------------------------------
S44_E7C6C <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S44, VacAgeGroup = "S4", Age_Sus = c(0.4, 0.4),
    Vac_start = "2019-08-30", Effacy_I = 0.7, Effacy_Hosp = 0.8,
    VacProp = 0.6, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "S44_E7C6C"),
    width = 14, height = 8
)

#-----------------------------------------------------------
# efficacy: 80%: coverage: 60%, A+B+C+D
#-----------------------------------------------------------
S44_E8C6C <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S44, VacAgeGroup = "S4", Age_Sus = c(0.4, 0.4),
    Vac_start = "2019-08-30", Effacy_I = 0.8, Effacy_Hosp = 0.9,
    VacProp = 0.6, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "S44_E8C6C"),
    width = 14, height = 8
)


### Coverage: 80%, A+B+C+D --------------------------------------------------------------------------------------
#-----------------------------------------------------------
# efficacy: 60%; coverage: 80%; coverage, A+B+C+D
#-----------------------------------------------------------
S44_E6C8C <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S44, VacAgeGroup = "S4", Age_Sus = c(0.4, 0.4),
    Vac_start = "2019-08-30", Effacy_I = 0.6, Effacy_Hosp = 0.7,
    VacProp = 0.8, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "S44_E6C8C"),
    width = 14, height = 8
)

#-----------------------------------------------------------
# efficacy: 70%: coverage: 80%, A+B+C+D
#-----------------------------------------------------------
S44_E7C8C <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S44, VacAgeGroup = "S4", Age_Sus = c(0.4, 0.4),
    Vac_start = "2019-08-30", Effacy_I = 0.7, Effacy_Hosp = 0.8,
    VacProp = 0.8, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "S44_E7C8C"),
    width = 14, height = 8
)

#-----------------------------------------------------------
# efficacy: 80%: coverage: 80%, A+B+C+D
#-----------------------------------------------------------
S44_E8C8C <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S44, VacAgeGroup = "S4", Age_Sus = c(0.4, 0.4),
    Vac_start = "2019-08-30", Effacy_I = 0.8, Effacy_Hosp = 0.9,
    VacProp = 0.8, lag = FALSE,
    Plot = TRUE, save = TRUE, path = paste0(ImmuPath, "S44_E8C8C"),
    width = 14, height = 8
)
