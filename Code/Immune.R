# sourceCpp("Code/model_immu.cpp")
sourceCpp("Code/model_immuV3.cpp")

## efficacy: 60, 70, 80

## coverage: 60, 80

## A. 0-5m; B. 6-11m; C. 12-23m; D. 2-4y;
## A+B; A+B+C; A+B+C+D？

### Base scenario ###########################################################################################
### Coverage: 60%, A+B --------------------------------------------------------------------------------------
#-----------------------------------------------------------
# efficacy: 60%; coverage: 60%; A+B
#-----------------------------------------------------------
SBase_E6C6A <- Vac.Batch(
    MCMC_Result = PosterioriCheck_Sbase, VacAgeGroup = "S2", Age_Sus = c(1, 1),
    Vac_start = "2018-08-30", Effacy_I = 0.6, Effacy_Hosp = 0.7,
    VacProp = 0.6, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)

#-----------------------------------------------------------
# efficacy: 70%: coverage: 60%, A+B
#-----------------------------------------------------------
SBase_E7C6A <- Vac.Batch(
    MCMC_Result = PosterioriCheck_Sbase, VacAgeGroup = "S2", Age_Sus = c(1, 1),
    Vac_start = "2018-08-30", Effacy_I = 0.7, Effacy_Hosp = 0.8,
    VacProp = 0.6, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)

#-----------------------------------------------------------
# efficacy: 80%: coverage: 60%, A+B
#-----------------------------------------------------------
SBase_E8C6A <- Vac.Batch(
    MCMC_Result = PosterioriCheck_Sbase, VacAgeGroup = "S2", Age_Sus = c(1, 1),
    Vac_start = "2018-08-30", Effacy_I = 0.8, Effacy_Hosp = 0.9,
    VacProp = 0.6, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)



### Coverage: 80%, A+B --------------------------------------------------------------------------------------
#-----------------------------------------------------------
# efficacy: 60%; coverage: 80%; A+B
#-----------------------------------------------------------
SBase_E6C8A <- Vac.Batch(
    MCMC_Result = PosterioriCheck_Sbase, VacAgeGroup = "S2", Age_Sus = c(1, 1),
    Vac_start = "2018-08-30", Effacy_I = 0.6, Effacy_Hosp = 0.7,
    VacProp = 0.8, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)

#-----------------------------------------------------------
# efficacy: 70%: coverage: 80%, A+B
#-----------------------------------------------------------
SBase_E7C8A <- Vac.Batch(
    MCMC_Result = PosterioriCheck_Sbase, VacAgeGroup = "S2", Age_Sus = c(1, 1),
    Vac_start = "2018-08-30", Effacy_I = 0.7, Effacy_Hosp = 0.8,
    VacProp = 0.8, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)

#-----------------------------------------------------------
# efficacy: 80%: coverage: 80%, A+B
#-----------------------------------------------------------
SBase_E8C8A <- Vac.Batch(
    MCMC_Result = PosterioriCheck_Sbase, VacAgeGroup = "S2", Age_Sus = c(1, 1),
    Vac_start = "2018-08-30", Effacy_I = 0.8, Effacy_Hosp = 0.9,
    VacProp = 0.8, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)



### Coverage: 60%, A+B+C --------------------------------------------------------------------------------------
#-----------------------------------------------------------
# efficacy: 60%; coverage: 60%; coverage, A+B+C
#-----------------------------------------------------------
SBase_E6C6B <- Vac.Batch(
    MCMC_Result = PosterioriCheck_Sbase, VacAgeGroup = "S3", Age_Sus = c(1, 1),
    Vac_start = "2018-08-30", Effacy_I = 0.6, Effacy_Hosp = 0.7,
    VacProp = 0.6, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)

#-----------------------------------------------------------
# efficacy: 70%: coverage: 60%, A+B+C
#-----------------------------------------------------------
SBase_E7C6B <- Vac.Batch(
    MCMC_Result = PosterioriCheck_Sbase, VacAgeGroup = "S3", Age_Sus = c(1, 1),
    Vac_start = "2018-08-30", Effacy_I = 0.7, Effacy_Hosp = 0.8,
    VacProp = 0.6, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)

#-----------------------------------------------------------
# efficacy: 80%: coverage: 60%, A+B+C
#-----------------------------------------------------------
SBase_E8C6B <- Vac.Batch(
    MCMC_Result = PosterioriCheck_Sbase, VacAgeGroup = "S3", Age_Sus = c(1, 1),
    Vac_start = "2018-08-30", Effacy_I = 0.8, Effacy_Hosp = 0.9,
    VacProp = 0.6, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)


### Coverage: 80%, A+B+C --------------------------------------------------------------------------------------
#-----------------------------------------------------------
# efficacy: 60%; coverage: 80%; coverage, A+B+C
#-----------------------------------------------------------
SBase_E6C8B <- Vac.Batch(
    MCMC_Result = PosterioriCheck_Sbase, VacAgeGroup = "S3", Age_Sus = c(1, 1),
    Vac_start = "2018-08-30", Effacy_I = 0.6, Effacy_Hosp = 0.7,
    VacProp = 0.8, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)

#-----------------------------------------------------------
# efficacy: 70%: coverage: 80%, A+B+C
#-----------------------------------------------------------
SBase_E7C8B <- Vac.Batch(
    MCMC_Result = PosterioriCheck_Sbase, VacAgeGroup = "S3", Age_Sus = c(1, 1),
    Vac_start = "2018-08-30", Effacy_I = 0.7, Effacy_Hosp = 0.8,
    VacProp = 0.8, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)

#-----------------------------------------------------------
# efficacy: 80%: coverage: 80%, A+B+C
#-----------------------------------------------------------
SBase_E8C8B <- Vac.Batch(
    MCMC_Result = PosterioriCheck_Sbase, VacAgeGroup = "S3", Age_Sus = c(1, 1),
    Vac_start = "2018-08-30", Effacy_I = 0.8, Effacy_Hosp = 0.9,
    VacProp = 0.8, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)



### Coverage: 60%, A+B+C+D --------------------------------------------------------------------------------------
#-----------------------------------------------------------
# efficacy: 60%; coverage: 60%; coverage, A+B+C+D
#-----------------------------------------------------------
SBase_E6C6C <- Vac.Batch(
    MCMC_Result = PosterioriCheck_Sbase, VacAgeGroup = "S4", Age_Sus = c(1, 1),
    Vac_start = "2018-08-30", Effacy_I = 0.6, Effacy_Hosp = 0.7,
    VacProp = 0.6, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)

#-----------------------------------------------------------
# efficacy: 70%: coverage: 60%, A+B+C+D
#-----------------------------------------------------------
SBase_E7C6C <- Vac.Batch(
    MCMC_Result = PosterioriCheck_Sbase, VacAgeGroup = "S4", Age_Sus = c(1, 1),
    Vac_start = "2018-08-30", Effacy_I = 0.7, Effacy_Hosp = 0.8,
    VacProp = 0.6, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)

#-----------------------------------------------------------
# efficacy: 80%: coverage: 60%, A+B+C+D
#-----------------------------------------------------------
SBase_E8C6C <- Vac.Batch(
    MCMC_Result = PosterioriCheck_Sbase, VacAgeGroup = "S4", Age_Sus = c(1, 1),
    Vac_start = "2018-08-30", Effacy_I = 0.8, Effacy_Hosp = 0.9,
    VacProp = 0.6, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)


### Coverage: 80%, A+B+C+D --------------------------------------------------------------------------------------
#-----------------------------------------------------------
# efficacy: 60%; coverage: 80%; coverage, A+B+C+D
#-----------------------------------------------------------
SBase_E6C8C <- Vac.Batch(
    MCMC_Result = PosterioriCheck_Sbase, VacAgeGroup = "S4", Age_Sus = c(1, 1),
    Vac_start = "2018-08-30", Effacy_I = 0.6, Effacy_Hosp = 0.7,
    VacProp = 0.8, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)

#-----------------------------------------------------------
# efficacy: 70%: coverage: 80%, A+B+C+D
#-----------------------------------------------------------
SBase_E7C8C <- Vac.Batch(
    MCMC_Result = PosterioriCheck_Sbase, VacAgeGroup = "S4", Age_Sus = c(1, 1),
    Vac_start = "2018-08-30", Effacy_I = 0.7, Effacy_Hosp = 0.8,
    VacProp = 0.8, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)

#-----------------------------------------------------------
# efficacy: 80%: coverage: 80%, A+B+C+D
#-----------------------------------------------------------
SBase_E8C8C <- Vac.Batch(
    MCMC_Result = PosterioriCheck_Sbase, VacAgeGroup = "S4", Age_Sus = c(1, 1),
    Vac_start = "2018-08-30", Effacy_I = 0.8, Effacy_Hosp = 0.9,
    VacProp = 0.8, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)





### S42 ###########################################################################################
### Coverage: 60%, A+B --------------------------------------------------------------------------------------
#-----------------------------------------------------------
# efficacy: 60%; coverage: 60%; A+B
#-----------------------------------------------------------
S42_E6C6A <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S42, VacAgeGroup = "S2", Age_Sus = c(0.4, 0.2),
    Vac_start = "2018-08-30", Effacy_I = 0.6, Effacy_Hosp = 0.7,
    VacProp = 0.6, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)

#-----------------------------------------------------------
# efficacy: 70%: coverage: 60%, A+B
#-----------------------------------------------------------
S42_E7C6A <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S42, VacAgeGroup = "S2", Age_Sus = c(0.4, 0.2),
    Vac_start = "2018-08-30", Effacy_I = 0.7, Effacy_Hosp = 0.8,
    VacProp = 0.6, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)

#-----------------------------------------------------------
# efficacy: 80%: coverage: 60%, A+B
#-----------------------------------------------------------
S42_E8C6A <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S42, VacAgeGroup = "S2", Age_Sus = c(0.4, 0.2),
    Vac_start = "2018-08-30", Effacy_I = 0.8, Effacy_Hosp = 0.9,
    VacProp = 0.6, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)



### Coverage: 80%, A+B --------------------------------------------------------------------------------------
#-----------------------------------------------------------
# efficacy: 60%; coverage: 80%; A+B
#-----------------------------------------------------------
S42_E6C8A <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S42, VacAgeGroup = "S2", Age_Sus = c(0.4, 0.2),
    Vac_start = "2018-08-30", Effacy_I = 0.6, Effacy_Hosp = 0.7,
    VacProp = 0.8, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)

#-----------------------------------------------------------
# efficacy: 70%: coverage: 80%, A+B
#-----------------------------------------------------------
S42_E7C8A <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S42, VacAgeGroup = "S2", Age_Sus = c(0.4, 0.2),
    Vac_start = "2018-08-30", Effacy_I = 0.7, Effacy_Hosp = 0.8,
    VacProp = 0.8, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)

#-----------------------------------------------------------
# efficacy: 80%: coverage: 80%, A+B
#-----------------------------------------------------------
S42_E8C8A <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S42, VacAgeGroup = "S2", Age_Sus = c(0.4, 0.2),
    Vac_start = "2018-08-30", Effacy_I = 0.8, Effacy_Hosp = 0.9,
    VacProp = 0.8, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)



### Coverage: 60%, A+B+C --------------------------------------------------------------------------------------
#-----------------------------------------------------------
# efficacy: 60%; coverage: 60%; coverage, A+B+C
#-----------------------------------------------------------
S42_E6C6B <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S42, VacAgeGroup = "S3", Age_Sus = c(0.4, 0.2),
    Vac_start = "2018-08-30", Effacy_I = 0.6, Effacy_Hosp = 0.7,
    VacProp = 0.6, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)

#-----------------------------------------------------------
# efficacy: 70%: coverage: 60%, A+B+C
#-----------------------------------------------------------
S42_E7C6B <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S42, VacAgeGroup = "S3", Age_Sus = c(0.4, 0.2),
    Vac_start = "2018-08-30", Effacy_I = 0.7, Effacy_Hosp = 0.8,
    VacProp = 0.6, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)

#-----------------------------------------------------------
# efficacy: 80%: coverage: 60%, A+B+C
#-----------------------------------------------------------
S42_E8C6B <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S42, VacAgeGroup = "S3", Age_Sus = c(0.4, 0.2),
    Vac_start = "2018-08-30", Effacy_I = 0.8, Effacy_Hosp = 0.9,
    VacProp = 0.6, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)


### Coverage: 80%, A+B+C --------------------------------------------------------------------------------------
#-----------------------------------------------------------
# efficacy: 60%; coverage: 80%; coverage, A+B+C
#-----------------------------------------------------------
S42_E6C8B <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S42, VacAgeGroup = "S3", Age_Sus = c(0.4, 0.2),
    Vac_start = "2018-08-30", Effacy_I = 0.6, Effacy_Hosp = 0.7,
    VacProp = 0.8, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)

#-----------------------------------------------------------
# efficacy: 70%: coverage: 80%, A+B+C
#-----------------------------------------------------------
S42_E7C8B <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S42, VacAgeGroup = "S3", Age_Sus = c(0.4, 0.2),
    Vac_start = "2018-08-30", Effacy_I = 0.7, Effacy_Hosp = 0.8,
    VacProp = 0.8, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)

#-----------------------------------------------------------
# efficacy: 80%: coverage: 80%, A+B+C
#-----------------------------------------------------------
S42_E8C8B <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S42, VacAgeGroup = "S3", Age_Sus = c(0.4, 0.2),
    Vac_start = "2018-08-30", Effacy_I = 0.8, Effacy_Hosp = 0.9,
    VacProp = 0.8, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)



### Coverage: 60%, A+B+C+D --------------------------------------------------------------------------------------
#-----------------------------------------------------------
# efficacy: 60%; coverage: 60%; coverage, A+B+C+D
#-----------------------------------------------------------
S42_E6C6C <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S42, VacAgeGroup = "S4", Age_Sus = c(0.4, 0.2),
    Vac_start = "2018-08-30", Effacy_I = 0.6, Effacy_Hosp = 0.7,
    VacProp = 0.6, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)

#-----------------------------------------------------------
# efficacy: 70%: coverage: 60%, A+B+C+D
#-----------------------------------------------------------
S42_E7C6C <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S42, VacAgeGroup = "S4", Age_Sus = c(0.4, 0.2),
    Vac_start = "2018-08-30", Effacy_I = 0.7, Effacy_Hosp = 0.8,
    VacProp = 0.6, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)

#-----------------------------------------------------------
# efficacy: 80%: coverage: 60%, A+B+C+D
#-----------------------------------------------------------
S42_E8C6C <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S42, VacAgeGroup = "S4", Age_Sus = c(0.4, 0.2),
    Vac_start = "2018-08-30", Effacy_I = 0.8, Effacy_Hosp = 0.9,
    VacProp = 0.6, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)


### Coverage: 80%, A+B+C+D --------------------------------------------------------------------------------------
#-----------------------------------------------------------
# efficacy: 60%; coverage: 80%; coverage, A+B+C+D
#-----------------------------------------------------------
S42_E6C8C <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S42, VacAgeGroup = "S4", Age_Sus = c(0.4, 0.2),
    Vac_start = "2018-08-30", Effacy_I = 0.6, Effacy_Hosp = 0.7,
    VacProp = 0.8, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)

#-----------------------------------------------------------
# efficacy: 70%: coverage: 80%, A+B+C+D
#-----------------------------------------------------------
S42_E7C8C <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S42, VacAgeGroup = "S4", Age_Sus = c(0.4, 0.2),
    Vac_start = "2018-08-30", Effacy_I = 0.7, Effacy_Hosp = 0.8,
    VacProp = 0.8, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)

#-----------------------------------------------------------
# efficacy: 80%: coverage: 80%, A+B+C+D
#-----------------------------------------------------------
S42_E8C8C <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S42, VacAgeGroup = "S4", Age_Sus = c(0.4, 0.2),
    Vac_start = "2018-08-30", Effacy_I = 0.8, Effacy_Hosp = 0.9,
    VacProp = 0.8, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)





### S43 ###########################################################################################
### Coverage: 60%, A+B --------------------------------------------------------------------------------------
#-----------------------------------------------------------
# efficacy: 60%; coverage: 60%; A+B
#-----------------------------------------------------------
S43_E6C6A <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S43, VacAgeGroup = "S2", Age_Sus = c(0.4, 0.3),
    Vac_start = "2018-08-30", Effacy_I = 0.6, Effacy_Hosp = 0.7,
    VacProp = 0.6, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)

#-----------------------------------------------------------
# efficacy: 70%: coverage: 60%, A+B
#-----------------------------------------------------------
S43_E7C6A <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S43, VacAgeGroup = "S2", Age_Sus = c(0.4, 0.3),
    Vac_start = "2018-08-30", Effacy_I = 0.7, Effacy_Hosp = 0.8,
    VacProp = 0.6, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)

#-----------------------------------------------------------
# efficacy: 80%: coverage: 60%, A+B
#-----------------------------------------------------------
S43_E8C6A <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S43, VacAgeGroup = "S2", Age_Sus = c(0.4, 0.3),
    Vac_start = "2018-08-30", Effacy_I = 0.8, Effacy_Hosp = 0.9,
    VacProp = 0.6, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)



### Coverage: 80%, A+B --------------------------------------------------------------------------------------
#-----------------------------------------------------------
# efficacy: 60%; coverage: 80%; A+B
#-----------------------------------------------------------
S43_E6C8A <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S43, VacAgeGroup = "S2", Age_Sus = c(0.4, 0.3),
    Vac_start = "2018-08-30", Effacy_I = 0.6, Effacy_Hosp = 0.7,
    VacProp = 0.8, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)

#-----------------------------------------------------------
# efficacy: 70%: coverage: 80%, A+B
#-----------------------------------------------------------
S43_E7C8A <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S43, VacAgeGroup = "S2", Age_Sus = c(0.4, 0.3),
    Vac_start = "2018-08-30", Effacy_I = 0.7, Effacy_Hosp = 0.8,
    VacProp = 0.8, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)

#-----------------------------------------------------------
# efficacy: 80%: coverage: 80%, A+B
#-----------------------------------------------------------
S43_E8C8A <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S43, VacAgeGroup = "S2", Age_Sus = c(0.4, 0.3),
    Vac_start = "2018-08-30", Effacy_I = 0.8, Effacy_Hosp = 0.9,
    VacProp = 0.8, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)



### Coverage: 60%, A+B+C --------------------------------------------------------------------------------------
#-----------------------------------------------------------
# efficacy: 60%; coverage: 60%; coverage, A+B+C
#-----------------------------------------------------------
S43_E6C6B <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S43, VacAgeGroup = "S3", Age_Sus = c(0.4, 0.3),
    Vac_start = "2018-08-30", Effacy_I = 0.6, Effacy_Hosp = 0.7,
    VacProp = 0.6, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)

#-----------------------------------------------------------
# efficacy: 70%: coverage: 60%, A+B+C
#-----------------------------------------------------------
S43_E7C6B <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S43, VacAgeGroup = "S3", Age_Sus = c(0.4, 0.3),
    Vac_start = "2018-08-30", Effacy_I = 0.7, Effacy_Hosp = 0.8,
    VacProp = 0.6, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)

#-----------------------------------------------------------
# efficacy: 80%: coverage: 60%, A+B+C
#-----------------------------------------------------------
S43_E8C6B <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S43, VacAgeGroup = "S3", Age_Sus = c(0.4, 0.3),
    Vac_start = "2018-08-30", Effacy_I = 0.8, Effacy_Hosp = 0.9,
    VacProp = 0.6, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)


### Coverage: 80%, A+B+C --------------------------------------------------------------------------------------
#-----------------------------------------------------------
# efficacy: 60%; coverage: 80%; coverage, A+B+C
#-----------------------------------------------------------
S43_E6C8B <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S43, VacAgeGroup = "S3", Age_Sus = c(0.4, 0.3),
    Vac_start = "2018-08-30", Effacy_I = 0.6, Effacy_Hosp = 0.7,
    VacProp = 0.8, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)

#-----------------------------------------------------------
# efficacy: 70%: coverage: 80%, A+B+C
#-----------------------------------------------------------
S43_E7C8B <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S43, VacAgeGroup = "S3", Age_Sus = c(0.4, 0.3),
    Vac_start = "2018-08-30", Effacy_I = 0.7, Effacy_Hosp = 0.8,
    VacProp = 0.8, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)

#-----------------------------------------------------------
# efficacy: 80%: coverage: 80%, A+B+C
#-----------------------------------------------------------
S43_E8C8B <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S43, VacAgeGroup = "S3", Age_Sus = c(0.4, 0.3),
    Vac_start = "2018-08-30", Effacy_I = 0.8, Effacy_Hosp = 0.9,
    VacProp = 0.8, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)



### Coverage: 60%, A+B+C+D --------------------------------------------------------------------------------------
#-----------------------------------------------------------
# efficacy: 60%; coverage: 60%; coverage, A+B+C+D
#-----------------------------------------------------------
S43_E6C6C <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S43, VacAgeGroup = "S4", Age_Sus = c(0.4, 0.3),
    Vac_start = "2018-08-30", Effacy_I = 0.6, Effacy_Hosp = 0.7,
    VacProp = 0.6, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)

#-----------------------------------------------------------
# efficacy: 70%: coverage: 60%, A+B+C+D
#-----------------------------------------------------------
S43_E7C6C <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S43, VacAgeGroup = "S4", Age_Sus = c(0.4, 0.3),
    Vac_start = "2018-08-30", Effacy_I = 0.7, Effacy_Hosp = 0.8,
    VacProp = 0.6, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)

#-----------------------------------------------------------
# efficacy: 80%: coverage: 60%, A+B+C+D
#-----------------------------------------------------------
S43_E8C6C <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S43, VacAgeGroup = "S4", Age_Sus = c(0.4, 0.3),
    Vac_start = "2018-08-30", Effacy_I = 0.8, Effacy_Hosp = 0.9,
    VacProp = 0.6, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)


### Coverage: 80%, A+B+C+D --------------------------------------------------------------------------------------
#-----------------------------------------------------------
# efficacy: 60%; coverage: 80%; coverage, A+B+C+D
#-----------------------------------------------------------
S43_E6C8C <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S43, VacAgeGroup = "S4", Age_Sus = c(0.4, 0.3),
    Vac_start = "2018-08-30", Effacy_I = 0.6, Effacy_Hosp = 0.7,
    VacProp = 0.8, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)

#-----------------------------------------------------------
# efficacy: 70%: coverage: 80%, A+B+C+D
#-----------------------------------------------------------
S43_E7C8C <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S43, VacAgeGroup = "S4", Age_Sus = c(0.4, 0.3),
    Vac_start = "2018-08-30", Effacy_I = 0.7, Effacy_Hosp = 0.8,
    VacProp = 0.8, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)

#-----------------------------------------------------------
# efficacy: 80%: coverage: 80%, A+B+C+D
#-----------------------------------------------------------
S43_E8C8C <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S43, VacAgeGroup = "S4", Age_Sus = c(0.4, 0.3),
    Vac_start = "2018-08-30", Effacy_I = 0.8, Effacy_Hosp = 0.9,
    VacProp = 0.8, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)




### S44 ###########################################################################################
### Coverage: 60%, A+B --------------------------------------------------------------------------------------
#-----------------------------------------------------------
# efficacy: 60%; coverage: 60%; A+B
#-----------------------------------------------------------
S44_E6C6A <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S44, VacAgeGroup = "S2", Age_Sus = c(0.4, 0.4),
    Vac_start = "2018-08-30", Effacy_I = 0.6, Effacy_Hosp = 0.7,
    VacProp = 0.6, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)

#-----------------------------------------------------------
# efficacy: 70%: coverage: 60%, A+B
#-----------------------------------------------------------
S44_E7C6A <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S44, VacAgeGroup = "S2", Age_Sus = c(0.4, 0.4),
    Vac_start = "2018-08-30", Effacy_I = 0.7, Effacy_Hosp = 0.8,
    VacProp = 0.6, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)

#-----------------------------------------------------------
# efficacy: 80%: coverage: 60%, A+B
#-----------------------------------------------------------
S44_E8C6A <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S44, VacAgeGroup = "S2", Age_Sus = c(0.4, 0.4),
    Vac_start = "2018-08-30", Effacy_I = 0.8, Effacy_Hosp = 0.9,
    VacProp = 0.6, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)



### Coverage: 80%, A+B --------------------------------------------------------------------------------------
#-----------------------------------------------------------
# efficacy: 60%; coverage: 80%; A+B
#-----------------------------------------------------------
S44_E6C8A <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S44, VacAgeGroup = "S2", Age_Sus = c(0.4, 0.4),
    Vac_start = "2018-08-30", Effacy_I = 0.6, Effacy_Hosp = 0.7,
    VacProp = 0.8, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)

#-----------------------------------------------------------
# efficacy: 70%: coverage: 80%, A+B
#-----------------------------------------------------------
S44_E7C8A <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S44, VacAgeGroup = "S2", Age_Sus = c(0.4, 0.4),
    Vac_start = "2018-08-30", Effacy_I = 0.7, Effacy_Hosp = 0.8,
    VacProp = 0.8, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)

#-----------------------------------------------------------
# efficacy: 80%: coverage: 80%, A+B
#-----------------------------------------------------------
S44_E8C8A <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S44, VacAgeGroup = "S2", Age_Sus = c(0.4, 0.4),
    Vac_start = "2018-08-30", Effacy_I = 0.8, Effacy_Hosp = 0.9,
    VacProp = 0.8, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)



### Coverage: 60%, A+B+C --------------------------------------------------------------------------------------
#-----------------------------------------------------------
# efficacy: 60%; coverage: 60%; coverage, A+B+C
#-----------------------------------------------------------
S44_E6C6B <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S44, VacAgeGroup = "S3", Age_Sus = c(0.4, 0.4),
    Vac_start = "2018-08-30", Effacy_I = 0.6, Effacy_Hosp = 0.7,
    VacProp = 0.6, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)

#-----------------------------------------------------------
# efficacy: 70%: coverage: 60%, A+B+C
#-----------------------------------------------------------
S44_E7C6B <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S44, VacAgeGroup = "S3", Age_Sus = c(0.4, 0.4),
    Vac_start = "2018-08-30", Effacy_I = 0.7, Effacy_Hosp = 0.8,
    VacProp = 0.6, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)

#-----------------------------------------------------------
# efficacy: 80%: coverage: 60%, A+B+C
#-----------------------------------------------------------
S44_E8C6B <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S44, VacAgeGroup = "S3", Age_Sus = c(0.4, 0.4),
    Vac_start = "2018-08-30", Effacy_I = 0.8, Effacy_Hosp = 0.9,
    VacProp = 0.6, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)


### Coverage: 80%, A+B+C --------------------------------------------------------------------------------------
#-----------------------------------------------------------
# efficacy: 60%; coverage: 80%; coverage, A+B+C
#-----------------------------------------------------------
S44_E6C8B <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S44, VacAgeGroup = "S3", Age_Sus = c(0.4, 0.4),
    Vac_start = "2018-08-30", Effacy_I = 0.6, Effacy_Hosp = 0.7,
    VacProp = 0.8, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)

#-----------------------------------------------------------
# efficacy: 70%: coverage: 80%, A+B+C
#-----------------------------------------------------------
S44_E7C8B <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S44, VacAgeGroup = "S3", Age_Sus = c(0.4, 0.4),
    Vac_start = "2018-08-30", Effacy_I = 0.7, Effacy_Hosp = 0.8,
    VacProp = 0.8, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)

#-----------------------------------------------------------
# efficacy: 80%: coverage: 80%, A+B+C
#-----------------------------------------------------------
S44_E8C8B <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S44, VacAgeGroup = "S3", Age_Sus = c(0.4, 0.4),
    Vac_start = "2018-08-30", Effacy_I = 0.8, Effacy_Hosp = 0.9,
    VacProp = 0.8, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)



### Coverage: 60%, A+B+C+D --------------------------------------------------------------------------------------
#-----------------------------------------------------------
# efficacy: 60%; coverage: 60%; coverage, A+B+C+D
#-----------------------------------------------------------
S44_E6C6C <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S44, VacAgeGroup = "S4", Age_Sus = c(0.4, 0.4),
    Vac_start = "2018-08-30", Effacy_I = 0.6, Effacy_Hosp = 0.7,
    VacProp = 0.6, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)

#-----------------------------------------------------------
# efficacy: 70%: coverage: 60%, A+B+C+D
#-----------------------------------------------------------
S44_E7C6C <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S44, VacAgeGroup = "S4", Age_Sus = c(0.4, 0.4),
    Vac_start = "2018-08-30", Effacy_I = 0.7, Effacy_Hosp = 0.8,
    VacProp = 0.6, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)

#-----------------------------------------------------------
# efficacy: 80%: coverage: 60%, A+B+C+D
#-----------------------------------------------------------
S44_E8C6C <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S44, VacAgeGroup = "S4", Age_Sus = c(0.4, 0.4),
    Vac_start = "2018-08-30", Effacy_I = 0.8, Effacy_Hosp = 0.9,
    VacProp = 0.6, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)


### Coverage: 80%, A+B+C+D --------------------------------------------------------------------------------------
#-----------------------------------------------------------
# efficacy: 60%; coverage: 80%; coverage, A+B+C+D
#-----------------------------------------------------------
S44_E6C8C <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S44, VacAgeGroup = "S4", Age_Sus = c(0.4, 0.4),
    Vac_start = "2018-08-30", Effacy_I = 0.6, Effacy_Hosp = 0.7,
    VacProp = 0.8, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)

#-----------------------------------------------------------
# efficacy: 70%: coverage: 80%, A+B+C+D
#-----------------------------------------------------------
S44_E7C8C <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S44, VacAgeGroup = "S4", Age_Sus = c(0.4, 0.4),
    Vac_start = "2018-08-30", Effacy_I = 0.7, Effacy_Hosp = 0.8,
    VacProp = 0.8, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)

#-----------------------------------------------------------
# efficacy: 80%: coverage: 80%, A+B+C+D
#-----------------------------------------------------------
S44_E8C8C <- Vac.Batch(
    MCMC_Result = PosterioriCheck_S44, VacAgeGroup = "S4", Age_Sus = c(0.4, 0.4),
    Vac_start = "2018-08-30", Effacy_I = 0.8, Effacy_Hosp = 0.9,
    VacProp = 0.8, lag = FALSE, ModelType = "Inpatient",
    Plot = TRUE, save = F
)
