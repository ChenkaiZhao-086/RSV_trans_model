#' @title Print message to command line interface
#'
#' @description Command line interface: print message
#'
#' @param ... (parts of the) message to print
#' @param WARNING boolean, to print the message in red
#'
#' @keywords internal
CLI.Print <- function(..., WARNING = FALSE, FORCED = FALSE) {
  # get function arguments
  function_arguments <- as.list(match.call(expand.dots = FALSE))$...
  # return: CLI.Print(... = pairlist("Create folder:", FolderPath))

  # get function-call environment (to retrieve variable from that environment)
  pf <- parent.frame()
  # 每当在R中调用一个函数时，都会创建一个新的环境来存放该函数的局部变量。这个新环境是当前环境的子环境。
  # parent.frame() 返回的是这个子环境的父环境，即调用当前函数所在的环境。在这个函数中是调用CLI.Print创造的环境

  # parse list => make character vector
  f_out <- " "
  for (i in 1:length(function_arguments)) {
    f_out <- cbind(f_out, eval(unlist(function_arguments[[i]]), envir = pf))
  }

  # add a space to each function arguments
  function_arguments <- paste(f_out, collapse = " ")

  # set text color: black (default) or red (warning)
  WordColorNormal <- "\033[0;36m"
  WordColorWarning <- "\033[0;31m"
  text_color <- ifelse(WARNING, WordColorWarning, WordColorNormal)

  # print time + arguments (without spaces)
  cli_out <- paste0(c(
    'echo "', text_color, "[", format(Sys.time(), "%H:%M:%S"), "]",
    function_arguments, WordColorNormal, '"'
  ), collapse = "")
  # 'echo "'得到的结果是"echo \""
  # 输出的结构: echo \"[11:21:27]  Create folder: Output/20240129/\"
  # echo后要有反斜线，输出内容用引号扩住，最后要加正斜线

  # print if function is called by master-node or first slave
  if (!exists("ParallelNodesInfo") ||
    Sys.getpid() == ParallelNodesInfo$pid_master ||
    FORCED) {
    system(cli_out)
  }

  # add to R warnings
  if (WARNING) {
    cli_warning <- paste0(c(
      text_color, "[", format(Sys.time(), "%H:%M:%S"), "]",
      function_arguments, WordColorNormal
    ), collapse = "")
    warning(cli_warning,
      call. = FALSE, immediate. = FALSE
    )
  }
}


#' @title Create one Main folder for Result output
#'
#' @description Create one Main folder for Result output. The folder name is current date or a specific name
File.CreateMainFolder <- function(path, FolderName = NULL, Date = NULL) {
  if (!is.null(FolderName)) {
    if (!file.exists(paste0(path, FolderName))) {
      dir.create(paste0(path, FolderName))
    }
    FolderPath <- paste0(path, FolderName, "/")
  }

  if (!is.null(Date)) {
    CurrentDate <- format(Sys.time(), "%Y%m%d")
    if (!file.exists(paste0(path, CurrentDate))) {
      dir.create(paste0(path, CurrentDate))
    }
    FolderPath <- paste0(path, CurrentDate, "/")
  }

  CLI.Print("Create folder:", FolderPath)

  return(FolderPath)
}

#' @title Create one sub folder for Result output
#'
#' @description Create one sub folder for Result output.
File.CreateSubFolder <- function(path, SubFolderName) {
  if (!file.exists(paste0(path, SubFolderName))) {
    dir.create(paste0(path, SubFolderName))
  }
  return(paste0(path, SubFolderName, "/"))
}


Parallel.Regist <- function(ncores = NULL, seed = NULL) {
  ## SETUP PARALLEL NODES
  # note: they will be removed after 600 seconds inactivity
  if (is.null(ncores)) {
    ncores <- detectCores() - 2
  }
  par_cluster <- makeCluster(ncores, cores = ncores) # , timeout = 600
  registerDoParallel(par_cluster)

  # set seed for each node
  if (!is.null(seed)) {
    clusterSetRNGStream(par_cluster, seed)
  }

  # store the process id (pid) of the first slave
  pid_slave1 <- clusterEvalQ(par_cluster, { # 用于在集群的每个节点上执行相同的表达式
    Sys.getpid() # 这是一个基础 R 函数，用于获取当前 R 会话的进程 ID
  })[[1]]

  # CREATE GLOBAL VARIABLE
  ParallelNodesInfo <<- list( # 节点信息
    par_cluster = par_cluster,
    pid_master = Sys.getpid(),
    pid_slave1 = pid_slave1
  )

  CLI.Print("START PARALLEL WORKERS")
}


#' @param Func_Vir A list, The name of functions and virables that need to be exported
Parallel.Import <- function(Func_Vir) {
  clusterExport(ParallelNodesInfo[[1]], Func_Vir)

  clusterEvalQ(ParallelNodesInfo[[1]], library("Rcpp"))
  clusterEvalQ(ParallelNodesInfo[[1]], library("RcppArmadillo"))
  clusterEvalQ(ParallelNodesInfo[[1]], library("data.table"))
  clusterEvalQ(ParallelNodesInfo[[1]], library("tidyverse"))
  clusterEvalQ(ParallelNodesInfo[[1]], library("extraDistr"))
  clusterEvalQ(ParallelNodesInfo[[1]], library("truncnorm"))
  clusterEvalQ(ParallelNodesInfo[[1]], library("progress"))
  clusterEvalQ(ParallelNodesInfo[[1]], {
    library("deSolve")
    sourceCpp("Code/model_cppV4.cpp")
    sourceCpp("Code/model_immuV3.cpp")
  }) # 在使用c++的时候，要在传递的时候直接把代码编译进去
}


Parallel.Stop <- function() {
  ## CLOSE NODES AND NODE INFO
  if (exists("ParallelNodesInfo")) {
    CLI.Print("STOP PARALLEL WORKERS")

    stopCluster(ParallelNodesInfo$par_cluster)
    rm(ParallelNodesInfo, envir = .GlobalEnv) # REMOVE GLOBAL VARIABLE
  }
}


Parallel.Check <- function() {
  if (!exists("ParallelNodesInfo")) {
    Parallel.Regist()
  } else if (!any(grepl(ParallelNodesInfo$pid_slave1, system("ps -A", intern = TRUE)))) {
    Parallel.Regist()
  }
}


#' @title Create Parmeters for model
#'
#' @description Reset cluster and start a new cluster
# reset parallel workers
Parallel.Reset <- function() {
  Parallel.Stop()
  gc()
  Parallel.Regist()
}


#' @title function to create state names
#' @param num_age: number of age group
Get.StateName <- function(num_age = num_age, model = c("SIR", "SIRV")) {
  # num_age: number of age group
  state_nam <- c()
  if (model == "SIR") {
    for (i in 1:num_age) {
      if (i == 1) {
        state_nam_temp <- c(
          paste0("M"),
          paste0("S0_G", i), paste0("I0_G", i), paste0("R0_G", i),
          paste0("S1_G", i), paste0("I1_G", i), paste0("R1_G", i),
          paste0("S2_G", i), paste0("I2_G", i), paste0("R2_G", i),
          paste0("Reported_G", i)
        )
      } else {
        state_nam_temp <- c(
          paste0("S0_G", i), paste0("I0_G", i), paste0("R0_G", i),
          paste0("S1_G", i), paste0("I1_G", i), paste0("R1_G", i),
          paste0("S2_G", i), paste0("I2_G", i), paste0("R2_G", i),
          paste0("Reported_G", i)
        )
      }
      state_nam <- c(state_nam, state_nam_temp)
    }
  } else if (model == "SIRV") {
    for (i in 1:num_age) {
      if (i == 1) {
        state_nam_temp <- c(
          paste0("M"),
          paste0("S0_G", i), paste0("V0_G", i), paste0("I0_G", i), paste0("R0_G", i),
          paste0("S1_G", i), paste0("V1_G", i), paste0("I1_G", i), paste0("R1_G", i),
          paste0("S2_G", i), paste0("V2_G", i), paste0("I2_G", i), paste0("R2_G", i),
          paste0("Reported_G", i)
        )
      } else {
        state_nam_temp <- c(
          paste0("S0_G", i), paste0("V0_G", i), paste0("I0_G", i), paste0("R0_G", i),
          paste0("S1_G", i), paste0("V1_G", i), paste0("I1_G", i), paste0("R1_G", i),
          paste0("S2_G", i), paste0("V2_G", i), paste0("I2_G", i), paste0("R2_G", i),
          paste0("Reported_G", i)
        )
      }
      state_nam <- c(state_nam, state_nam_temp)
    }
  }
  return(state_nam)
}


#' @title function to create states
#' @param num_age: number of age group
#' @param M_num: the number of newborn at the beginning
#' @param population: healthy population by age group
#' @param inf_num: the number of infected cases in the beginning
Get.InitState <- function(
    num_age = 11, M_num = 6000, population = Scot_Pop,
    inf_num = 20, model = "SIR") {
  if (model == "SIR") {
    state <- vector("numeric", 10 * num_age + 1)
    names(state) <- Get.StateName(num_age = num_age, model = "SIR")
    state[1] <- M_num
    for (i in 1:num_age) {
      state[2 + (i - 1) * 10] <- as.numeric(population[i])
    }
    for (i in 1:num_age) { # assign the initial number of infected cases, 20
      state[3 + (i - 1) * 10] <- inf_num
    }
  } else if (model == "SIRV") {
    state <- vector("numeric", 13 * num_age + 1)
    names(state) <- Get.StateName(num_age = num_age, model = "SIRV")
    state[1] <- M_num
    for (i in 1:num_age) {
      state[2 + (i - 1) * 13] <- as.numeric(population[i])
    }
    for (i in 1:num_age) { # assign the initial number of infected cases, 20
      state[4 + (i - 1) * 13] <- inf_num
    }
  }
  return(state)
}


Parameter.Create <- function(
    # Base state
    num_age = 11,
    M_num = 6000,
    population = Scot_Pop,
    inf_num = 20,
    # Simulation time
    years = 5,
    year_start = "1999-01-01",
    year_end = "2020-03-29",
    # Model parameters
    LiveBirth = 55101 / 365, # weekly live birth ## 55101 per year, average from 1999 to 2019
    Deaths = 55101 / 365,
    StartDyingOff = 8, # the 8th age group (> 60 years old)
    age_rates = c(
      1 / (30 * 3), # 0-2 months
      1 / (30 * 3), # 3-5 months
      1 / (30 * 6), # 6-11 months
      1 / (365 * 2), # 1-2 years
      1 / (365 * 3), # 2-4 years
      1 / (365 * 15), # 5-19 years
      1 / (365 * 40), # 20-59 years
      1 / (365 * 5), # 60-64 years
      1 / (365 * 5), # 65-69 years
      1 / (365 * 5), # 70-74 years
      0 # 75+ years
    ),
    omega_m = 28, # Duration of immunity from maternal
    beta_base = 1,
    beta_seasonal = 0.5,
    seasonal_wavelength = 0.3,
    phi = 0,
    contact_str = ContacrStr,
    gamma = 1 / 7, # 0.1428571; 7 days or 11 days
    omega_inf = 1 / 300, # 0.005847953; Duration of immunity from infection
    ReducedSus_1 = 0.76, # Reduced sus, 2 vs 1
    ReducedSus_2 = 0.88, # Reduced sus, n vs 2
    ReducedRec_1 = 1 / 0.87, # Reduced recovery rate, 2 vs 1
    ReducedRec_2 = 1 / 0.79, # Reduced recovery rate, n vs 2
    # Hospitalization rate
    Hosp_rate = c(
      0.29, # 0-2 months
      0.29, # 3-5 months
      0.10, # 6-11 months
      0.10, # 1-2 years
      0.015, # 2-4 years
      0.015, # 5-19 years
      0.015, # 20-59 years
      0.015, # 60-64 years
      0.08, # 65-69 years
      0.08, # 70-74 years
      0.08 # 75+ years
    ),
    Age_Sus = 0.2,
    # Vaccine
    Vac_start = "2017-03-29",
    Effacy = 0.4,
    VacProp = 0.4,
    omega_vac = 1 / 180) {
  # Modify the first age group
  age_rates_1 <- 1 / ((1 / age_rates[1]) - omega_m)
  age_rates <- c(age_rates_1, age_rates[-1])

  # Calibration of vaccined proportion
  NeedVacPopulation <- -log(1 - (Effacy * VacProp)) # 用这个替换Efface*VacProp

  # Expand the vaccine proportion
  # VacProp <- c(
  #   VacProp, # 0-2 months
  #   VacProp, # 3-5 months
  #   VacProp, # 6-11 months
  #   VacProp, # 1-2 years
  #   VacProp, # 2-4 years
  #   0, # 5-19 years
  #   0, # 20-59 years
  #   0, # 60-64 years
  #   0, # 65-69 years
  #   0, # 70-74 years
  #   0 # 75+ years
  # )

  # Calculate Vaccine date
  Vac_start <- as.numeric(seq(as.Date(Vac_start), max(as.Date(year_end)), by = "year"))

  Parmeters <-
    list(
      # Base state
      num_age = num_age,
      M_num = M_num,
      population = Scot_Pop,
      inf_num = inf_num,

      # Simulation time
      years = years,
      year_start = year_start,
      year_end = year_end,

      # Model parameters
      LiveBirth = LiveBirth,
      Deaths = Deaths,
      StartDyingOff = StartDyingOff,
      age_rates = age_rates,
      omega_m = 1 / omega_m,
      beta_base = beta_base,
      beta_seasonal = beta_seasonal,
      seasonal_wavelength = seasonal_wavelength,
      phi = phi,
      contact_str = contact_str,
      gamma = gamma,
      omega_inf = omega_inf,
      ReducedSus_1 = ReducedSus_1,
      ReducedSus_2 = ReducedSus_2,
      ReducedRec_1 = ReducedRec_1,
      ReducedRec_2 = ReducedRec_2,

      # Hospitalization rate
      Hosp_rate = Hosp_rate,
      Age_Sus = c(
        1, # 0-2 months
        1, # 3-5 months
        1, # 6-11 months
        1, # 1-2 years
        1, # 2-4 years
        1, # 5-19 years
        Age_Sus, # 20-59 years
        Age_Sus, # 60-64 years
        Age_Sus, # 65-69 years
        Age_Sus, # 70-74 years
        Age_Sus # 75+ years
      ),

      # Vaccine
      Vac_start = Vac_start,
      # Effacy = Effacy,
      NeedVacPopulation = NeedVacPopulation,
      # VacProp = VacProp,
      VacAgeGroup = c(
        1, # 0-2 months
        1, # 3-5 months
        1, # 6-11 months
        1, # 1-2 years
        1, # 2-4 years
        0, # 5-19 years
        0, # 20-59 years
        0, # 60-64 years
        0, # 65-69 years
        0, # 70-74 years
        0 # 75+ years
      ),
      omega_vac = omega_vac
    )

  return(Parmeters)
}


#' @title Calculate the number of infected cases
#' @param dat: data
#' @param Hosp_rate: hospitalization rate
Model.GetI <- function(dat, Hosp_rate = parameters[["Hosp_rate"]], lag = FALSE) {
  NewDat <- dat %>% as.data.table()
  NewDat <- NewDat[, time := as.Date(time)]
  ReportName <- grep("Reported", names(NewDat), value = TRUE)

  Simulation <- copy(NewDat)
  Simulation <- Simulation[, !..ReportName] # 这里的两个点指的是从环境中找相应的内容


  RealI <- copy(NewDat)
  RealI <- RealI[, ..ReportName] # 这里的两个点指的是从环境中找相应的内容
  RealI <- cbind(NewDat[, 1], RealI)
  ExcludeCol <- setdiff(names(RealI), c("time", "week"))
  # 使用真实时间模拟
  if (lag) {
    RealI <- RealI[, week := substr(ISOweek::date2ISOweek(time), 1, 8)][, ":="(Reported_G1 = shift(Reported_G1, 1, type = "lag", fill = 0),
      Reported_G2 = shift(Reported_G2, 1, type = "lag", fill = 0),
      Reported_G3 = shift(Reported_G3, 1, type = "lag", fill = 0),
      Reported_G4 = shift(Reported_G4, 2, type = "lag", fill = 0),
      Reported_G5 = shift(Reported_G5, 2, type = "lag", fill = 0),
      Reported_G6 = shift(Reported_G6, 2, type = "lag", fill = 0),
      Reported_G7 = shift(Reported_G7, 7, type = "lag", fill = 0),
      Reported_G8 = shift(Reported_G8, 7, type = "lag", fill = 0),
      Reported_G9 = shift(Reported_G9, 7, type = "lag", fill = 0),
      Reported_G10 = shift(Reported_G10, 7, type = "lag", fill = 0),
      Reported_G11 = shift(Reported_G11, 7, type = "lag", fill = 0))][time >= as.Date("2017-06-26") & time <= as.Date("2020-03-29"), ][, (ExcludeCol) := lapply(.SD, sum), by = .(week), .SDcols = ExcludeCol]
  } else {
    RealI <- RealI[, week := substr(ISOweek::date2ISOweek(time), 1, 8)][time >= as.Date("2017-06-26") & time <= as.Date("2020-03-29"), ][, (ExcludeCol) := lapply(.SD, sum), by = .(week), .SDcols = ExcludeCol]
  }


  # 截止时间修改为2018-03-29，默认情况（使用三年的数据）的截止时间是2020-03-29
  # "2017-06-26" "2020-03-29"
  # 使用固定的时间长度模拟
  # RealI <- RealI[, week := ceiling(seq_len(nrow(RealI)) / 7)
  #                ][, lapply(.SD, sum), by = .(week)
  #                  ]

  # Calculate cases for each group
  RealI <- RealI[, (2:(ncol(RealI) - 1)) := lapply(2:(ncol(RealI) - 1), function(i) RealI[[i]] * Hosp_rate[i - 1])]

  NameOrder <- c("time", "week", ExcludeCol)
  RealI <- unique(RealI[, ..NameOrder], by = "week")


  return(list(Simulation, RealI))
}


#' @title Run simulation
#' @param Parm: parameters for simulatio
Model.RunSim <- function(Parm, lag = FALSE) {
  # times <- seq(from = 1, to = 365 * Parm[["years"]])
  times <- as.numeric(seq(from = as.Date(Parm[["year_start"]]), to = as.Date(Parm[["year_end"]]), by = 1))
  state <- Get.InitState(
    num_age = Parm[["num_age"]],
    M_num = Parm[["M_num"]],
    population = Parm[["population"]],
    inf_num = Parm[["inf_num"]],
    model = "SIR"
  )

  SimResult <- ode(y = state, times = times, func = ModelSimCpp, parms = Parm, method = "lsoda") # "rk4"
  SimResult <- Model.GetI(SimResult, Parm[["Hosp_rate"]], lag = lag)

  return(SimResult)
}


Plot.Model <- function(dat, RealDat = RealDat_plot) {
  SimResult <- dat[[1]]
  CasesResult <- dat[[2]]

  # Simulaton of SIR in age group2
  Fig1 <- SimResult %>%
    as.data.frame() %>%
    ggplot() +
    geom_line(aes(x = time, y = S0_G2), colour = "red") +
    geom_line(aes(x = time, y = I0_G2), colour = "blue") +
    geom_line(aes(x = time, y = R0_G2), colour = "#034337") +
    geom_line(aes(x = time, y = S1_G2), colour = "red", linetype = 2) +
    geom_line(aes(x = time, y = I1_G2), colour = "blue", linetype = 2) +
    geom_line(aes(x = time, y = R1_G2), colour = "#034337", linetype = 2) +
    geom_line(aes(x = time, y = S2_G2), colour = "red", linetype = 3, linewidth = 1.2) +
    geom_line(aes(x = time, y = I2_G2), colour = "blue", linetype = 3, linewidth = 1.2) +
    geom_line(aes(x = time, y = R2_G2), colour = "#034337", linetype = 3, linewidth = 1.2) +
    theme_minimal()


  SummDat <- SimResult %>% as.data.table()
  ColWithS0 <- grep("^S0", colnames(SummDat), value = TRUE)
  ColWithI0 <- grep("^I0", colnames(SummDat), value = TRUE)
  ColWithR0 <- grep("^R0", colnames(SummDat), value = TRUE)
  ColWithS1 <- grep("^S1", colnames(SummDat), value = TRUE)
  ColWithI1 <- grep("^I1", colnames(SummDat), value = TRUE)
  ColWithR1 <- grep("^R1", colnames(SummDat), value = TRUE)
  ColWithS2 <- grep("^S2", colnames(SummDat), value = TRUE)
  ColWithI2 <- grep("^I2", colnames(SummDat), value = TRUE)
  ColWithR2 <- grep("^R2", colnames(SummDat), value = TRUE)
  SummDat[, R0 := rowSums(.SD), .SDcols = ColWithR0][, I0 := rowSums(.SD), .SDcols = ColWithI0][, S0 := rowSums(.SD), .SDcols = ColWithS0][, R1 := rowSums(.SD), .SDcols = ColWithR1][, I1 := rowSums(.SD), .SDcols = ColWithI1][, S1 := rowSums(.SD), .SDcols = ColWithS1][, R2 := rowSums(.SD), .SDcols = ColWithR2][, I2 := rowSums(.SD), .SDcols = ColWithI2][, S2 := rowSums(.SD), .SDcols = ColWithS2]

  # Simulaton of SIR in all age
  Fig2 <- SummDat %>%
    as.data.frame() %>%
    ggplot() +
    geom_line(aes(x = time, y = S0), colour = "red") +
    geom_line(aes(x = time, y = I0), colour = "blue") +
    geom_line(aes(x = time, y = R0), colour = "#034337") +
    geom_line(aes(x = time, y = S1), colour = "red", linetype = 2) +
    geom_line(aes(x = time, y = I1), colour = "blue", linetype = 2) +
    geom_line(aes(x = time, y = R1), colour = "#034337", linetype = 2) +
    geom_line(aes(x = time, y = S2), colour = "red", linetype = 3, linewidth = 1.5) +
    geom_line(aes(x = time, y = I2), colour = "blue", linetype = 3, linewidth = 1.5) +
    geom_line(aes(x = time, y = R2), colour = "#034337", linetype = 3, linewidth = 1.5) +
    theme_minimal()

  # Simualtion result of reported infected cases
  CasesResult <- CasesResult %>% as.data.table()
  CasesResult <- melt.data.table(CasesResult, id.vars = c("time", "week"), variable.name = "age_group", value.name = "Cases")

  MergeDat <- merge(CasesResult, RealDat, by = c("week", "age_group"))

  Fig3 <- MergeDat %>%
    mutate(
      age_group = factor(age_group,
        levels = c(
          "Reported_G1", "Reported_G2", "Reported_G3",
          "Reported_G4", "Reported_G5", "Reported_G6",
          "Reported_G7", "Reported_G8", "Reported_G9",
          "Reported_G10", "Reported_G11"
        ),
        labels = c(
          "0-2m", "3-5m", "6-11m", "12-23m", "2-4y", "5-19y",
          "20-59y", "60-64y", "65-69y", "70-74y", "75+y"
        )
      ),
      week = ISOweek::ISOweek2date(paste0(week, "-1"))
    ) %>%
    ggplot(.) +
    geom_line(aes(x = week, y = summ, group = age_group)) +
    geom_line(aes(x = week, y = Cases, group = age_group), colour = "red") +
    geom_vline(xintercept = as.Date(c("2018-01-01", "2019-01-01", "2020-01-01")), linetype = "dashed", alpha = 0.2) +
    labs(
      x = "Date",
      y = "Number of cases"
    ) +
    theme_minimal() +
    scale_x_date(date_labels = "%Y") +
    facet_wrap(~age_group) +
    theme(
      axis.text.x = element_text(size = 18, hjust = 1),
      axis.text.y = element_text(size = 18),
      axis.title = element_text(size = 24),
      strip.text = element_text(size = 24)
    )

  # Fig3 <- CasesResult %>%
  #   as.data.frame() %>%
  #   ggplot(aes(x = time, y = Cases, group = AgeGroup)) +
  #   geom_line() +
  #   facet_wrap(~AgeGroup) +
  #   theme_minimal()

  return(list(Fig1, Fig2, Fig3))
}


#' @title Calculate the likelihood
#' @param Parm: parameters for simulation
#' @param TargetDat: target data
Model.RunSim.LLH <- function(Parm, TargetDat = RefDat, lag = FALSE) {
  Dat <- Model.RunSim(Parm, lag = lag)
  SimDat <- Dat[[2]]

  SimDat <- melt(SimDat, id = c("time", "week"))
  CombineTable <- merge(SimDat, TargetDat, by.x = c("week", "variable"), by.y = c("week", "age_group"), all.y = T)
  CombineTable <- na.omit(CombineTable)

  # Calculate the likelihood based on negative binomial distribution
  CombineTable[variable == "Reported_G1", likelihood := dnbinom(x = true_value, size = 1.45, mu = ceiling(value), log = T)]
  CombineTable[variable == "Reported_G2", likelihood := dnbinom(x = true_value, size = 1.45, mu = ceiling(value), log = T)]
  CombineTable[variable == "Reported_G3", likelihood := dnbinom(x = true_value, size = 1.45, mu = ceiling(value), log = T)]
  CombineTable[variable == "Reported_G4", likelihood := dnbinom(x = true_value, size = 1.45, mu = ceiling(value), log = T)]
  CombineTable[variable == "Reported_G5", likelihood := dnbinom(x = true_value, size = 1.45, mu = ceiling(value), log = T)]
  CombineTable[variable == "Reported_G6", likelihood := dnbinom(x = true_value, size = 1.45, mu = ceiling(value), log = T)]
  CombineTable[variable == "Reported_G7", likelihood := dnbinom(x = true_value, size = 1.45, mu = ceiling(value), log = T)]
  CombineTable[variable == "Reported_G8", likelihood := dnbinom(x = true_value, size = 1.45, mu = ceiling(value), log = T)]
  CombineTable[variable == "Reported_G9", likelihood := dnbinom(x = true_value, size = 1.45, mu = ceiling(value), log = T)]
  CombineTable[variable == "Reported_G10", likelihood := dnbinom(x = true_value, size = 1.45, mu = ceiling(value), log = T)]
  CombineTable[variable == "Reported_G11", likelihood := dnbinom(x = true_value, size = 1.45, mu = ceiling(value), log = T)]

  LLH_Summ <- CombineTable[, sum(likelihood, na.rm = T)]
  LLH_Out <- LLH_Summ

  if (LLH_Out == 0) {
    LLH_Out <- -10000000
  }

  if (any(is.nan(CombineTable$likelihood))) {
    LLH_Out <- -10000000
  }
  return(LLH_Out)
}


#' @title Proposal function for MCMC
#' @param Parm: parameters for simulation
#' @param covmat: covariance matrix
#' @param lower_bounds: lower bounds for parameters
#' @param upper_bounds: upper bounds for parameters
#' @param theta_names: names of parameters
MCMC.Proposal <- function(Parm) {
  ParmNew_base <- rtruncnorm(1, a = 0, b = 1, mean = Parm[1], sd = Parm[1] / 30) # Parm[1] / 10 0.005 # beta_base

  # ParmNew_seasonal <- rtruncnorm(1, a = 0, mean = Parm[2], sd = Parm[2] / 15) # Parm[2] / 10  0.005 # beta_seasonal
  ParmNew_seasonal <- rtruncnorm(1, a = 5, b = 9.5, mean = Parm[2], sd = 0.05) # Parm[2] / 10  0.005 # beta_seasonal

  ParmNew2 <- rtruncnorm(1, a = -30, b = 10, mean = Parm[3], sd = 5) # Parm[3] + runif(1, -1.8, 1.8) # phi

  # ParmNew3 <- rtruncnorm(1, a = 0.02, mean = Parm[4], sd = Parm[4] / 15) # 0.0004 seasonal_wavelength
  ParmNew3 <- rtruncnorm(1, a = 28, b = 31, mean = Parm[4], sd = 0.0) # 0.0004 seasonal_wavelength

  # ParmNew4 <- rtruncnorm(11, a = 0, b = 0.05, mean = Parm[c(5:15)], sd = Parm[c(5:15)] / 12) # Hosp_rate
  ParmNew4.1 <- rtruncnorm(6, a = 0, b = 0.5, mean = Parm[c(5:10)], sd = Parm[c(5:10)] / 30) # Hosp_rate 0.0005
  ParmNew4.2 <- rtruncnorm(1, a = 0, b = 0.1, mean = Parm[c(11)], sd = Parm[c(11)] / 15) # Parm[c(11)] / 10  0.00004# Hosp_rate
  ParmNew4.3 <- rtruncnorm(1, a = 0, b = 0.1, mean = Parm[c(12)], sd = Parm[c(12)] / 15) # Parm[c(12)] / 10 0.00004# Hosp_rate
  ParmNew4.4 <- rtruncnorm(1, a = 0, b = 0.1, mean = Parm[c(15)], sd = Parm[c(15)] / 15) # Parm[c(15)] / 10 0.00004# Hosp_rate

  ParmNew4 <- c(
    ParmNew4.1,
    ParmNew4.2,
    ParmNew4.3, ParmNew4.3, ParmNew4.3,
    ParmNew4.4
  )

  ParmOut <- c(ParmNew_base, ParmNew_seasonal, ParmNew2, ParmNew3, ParmNew4)
  return(ParmOut)
}


#' @title Run MCMC model
#' @param Prior: prior parameters
MCMC.MH <- function(Prior, n_iterations, TargetDat = RefDat, lag = FALSE, Sus_reduce = 0.2) {
  chain <- matrix(NA, nrow = n_iterations, ncol = 15 + 1)
  chain[1, -16] <- Prior
  accepted <- 0

  current_log_likelihood <- Model.RunSim.LLH(
    Parm = Parameter.Create(
      beta_base = chain[1, 1], beta_seasonal = chain[1, 2], phi = chain[1, 3],
      seasonal_wavelength = chain[1, 4],
      Hosp_rate = c(chain[1, 5:15]), Age_Sus = Sus_reduce
    ), TargetDat = TargetDat, lag = lag
  )
  chain[1, 16] <- current_log_likelihood

  pb <- progress_bar$new(total = n_iterations, clear = TRUE, format = "  [:bar] :percent :etas")
  pb$tick()
  for (i in 2:n_iterations) {
    # proposal <- MCMC.Proposal(
    #   Parm = chain[i - 1, ], covmat = covmat, lower_bounds = lower_bounds,
    #   upper_bounds = upper_bounds
    # )
    proposal <- MCMC.Proposal(Parm = chain[i - 1, -16])
    proposal_log_likelihood <- Model.RunSim.LLH(
      Parm = Parameter.Create(
        beta_base = proposal[1], beta_seasonal = proposal[2], phi = proposal[3],
        seasonal_wavelength = proposal[4], Hosp_rate = c(proposal[5:15]), Age_Sus = Sus_reduce
      ), TargetDat = TargetDat, lag = lag
    )

    # Info <- sprintf("n_iteration is: %d Current LLH is: %f Proposal LLH is: %f", i, current_log_likelihood, proposal_log_likelihood)
    # CLI.Print(Info)
    if (i %% 200 == 0) {
      CLI.Print("Current iteration is: ", i / n_iterations * 100)
    }

    acceptance_ratio <- min(1, exp(proposal_log_likelihood - current_log_likelihood))
    if (runif(1) < acceptance_ratio) {
      chain[i, -16] <- proposal
      current_log_likelihood <- proposal_log_likelihood
      chain[i, 16] <- current_log_likelihood
      accepted <- accepted + 1
    } else {
      chain[i, ] <- chain[i - 1, ]
    }
    # print(proposal)
    # print(chain[i, ])
    pb$tick()
  }
  acceptance_rate <- accepted / n_iterations
  print(acceptance_rate)
  return(chain)
}



MCMC.TracePlot <- function(dat) {
  colnames(dat) <- c(
    "beta_base", "beta_seasonal", "phi", "seasonal_wavelength",
    "Hosp_rateM0_2", "Hosp_rateM3_5", "Hosp_rateM6_11", "Hosp_rateY1_2", "Hosp_rateY2_4", "Hosp_rateY5_19",
    "Hosp_rateY20_59", "Hosp_rateY60_64", "Hosp_rateY65_69", "Hosp_rateY70_74", "Hosp_rateY75_" # , "Age_Sus"
  )
  dat <- dat %>%
    as.data.frame() %>%
    mutate(iter = 1:n()) %>%
    pivot_longer(cols = -iter, names_to = "Parameter", values_to = "Value") %>%
    mutate(Parameter = factor(Parameter, levels = colnames(dat))) %>%
    ggplot() +
    geom_line(aes(x = iter, y = Value, group = Parameter)) +
    facet_wrap(~Parameter, scales = "free_y") +
    theme_minimal()
  return(dat)
}


MCMC.PosteriorSample <- function(dat) {
  runif(1, min = dat[1], max = dat[2])
}


MCMC.PostProcess <- function(dat, burn_in = 5000, thin = 10) {
  Chain <- lapply(seq_along(dat), \(i){
    Chain <- as.mcmc(dat[[i]][, -ncol(dat[[i]])])
    Chain <- window(Chain, start = burn_in + 1, thin = thin)
    colnames(Chain) <- c(
      "beta_base", "beta_seasonal", "phi", "seasonal_wavelength",
      "Hosp_rateM0_2", "Hosp_rateM3_5", "Hosp_rateM6_11", "Hosp_rateY1_2", "Hosp_rateY2_4", "Hosp_rateY5_19",
      "Hosp_rateY20_59", "Hosp_rateY60_64", "Hosp_rateY65_69", "Hosp_rateY70_74", "Hosp_rateY75_"
    )
    return(Chain)
  })

  Chain <- mcmc.list(Chain)

  par(mfrow = c(4, 4))
  Traceplot <- traceplot(Chain)

  par(mfrow = c(4, 4))
  Densplot <- densplot(Chain)

  # Geltest <- try(gelman.diag(Chain))
  Heitest <- heidel.diag(Chain)

  BindChain <- do.call(rbind, Chain)
  BindChain <- as.mcmc(BindChain)

  Median <- apply(BindChain, 2, median)
  CI <- HPDinterval(BindChain, prob = 0.95)

  Result <- cbind(Median, CI)

  posteriori <- paste(
    apply(Result, 1, function(x) sprintf("%.6f (%.6f, %.6f)", x[1], x[2], x[3])),
    sep = " "
  )

  return(list(
    Traceplot = Traceplot,
    Densplot = Densplot,
    # Geltest = Geltest,
    Heitest = Heitest,
    Median = Median,
    CI = CI,
    posteriori = posteriori
  ))
}



FindPeak <- function(SimDat, span = 29, TargetDat = RefPeak) {
  Peak <- lapply(SimDat[, 3:13], splus2R::peaks, span = span)

  PeakFind <- lapply(Peak, \(x) SimDat[x, 1:2])

  PeakFind <- lapply(names(PeakFind), \(name) {
    dt <- PeakFind[[name]]
    dt[, age_group := name]
    dt[, week := paste0(week, "-1")]
    dt <- dt[!month(time) %in% c(3, 4, 5, 6, 7, 8), ]
    return(dt)
  })

  PeakFindModify <- do.call(rbind, PeakFind)

  result <- cbind(TargetDat, PeakFindModify)

  result[, age_group := NULL][, TimeInterval := time - RefDate]

  setcolorder(result, c("age_group", "RefWeek", "week", "RefDate", "time"))

  return(result)
}


FindPeak.2age <- function(SimDat, span = 29, TargetDat = RefPeak_2age) {
  SimDat[, Reported_G1 := rowSums(.SD), .SDcols = paste0("Reported_G", 1:5)]
  SimDat[, Reported_G2 := rowSums(.SD), .SDcols = paste0("Reported_G", 6:11)]
  SimDat[, paste0("Reported_G", 3:11) := NULL]

  Peak <- lapply(SimDat[, 3:4], splus2R::peaks, span = span)

  PeakFind <- lapply(Peak, \(x) SimDat[x, 1:2])

  PeakFind <- lapply(names(PeakFind), \(name) {
    dt <- PeakFind[[name]]
    dt[, age_group := name]
    dt[, week := paste0(week, "-1")]
    dt <- dt[!month(time) %in% c(3, 4, 5, 6, 7, 8), ]
    return(dt)
  })

  PeakFindModify <- do.call(rbind, PeakFind)

  result <- cbind(TargetDat, PeakFindModify)

  result[, age_group := NULL][, TimeInterval := time - RefDate]

  setcolorder(result, c("age_group", "RefWeek", "week", "RefDate", "time"))

  return(result)
}




MCMC.PosteriorPlot <- function(
    dat, MedianDat, PeakDat, RealDat = RealDat_plot,
    save = FALSE, path = NULL, width = 12, height = 6) {
  Real <- copy(RealDat)
  Real <- Real[, week := ISOweek::ISOweek2date(paste0(week, "-1"))][, age_group := factor(age_group,
    levels = c(
      "Reported_G1", "Reported_G2", "Reported_G3",
      "Reported_G4", "Reported_G5", "Reported_G6",
      "Reported_G7", "Reported_G8", "Reported_G9",
      "Reported_G10", "Reported_G11"
    ),
    labels = c(
      "0-2m", "3-5m", "6-11m", "12-23m", "2-4y", "5-19y",
      "20-59y", "60-64y", "65-69y", "70-74y", "75+y"
    )
  )]

  MedianDat <- melt.data.table(MedianDat, id.vars = c("time", "week"), variable.name = "age_group", value.name = "Cases")
  MedianDat <- MedianDat[, week := ISOweek::ISOweek2date(paste0(week, "-1"))][, age_group := factor(age_group,
    levels = c(
      "Reported_G1", "Reported_G2", "Reported_G3",
      "Reported_G4", "Reported_G5", "Reported_G6",
      "Reported_G7", "Reported_G8", "Reported_G9",
      "Reported_G10", "Reported_G11"
    ),
    labels = c(
      "0-2m", "3-5m", "6-11m", "12-23m", "2-4y", "5-19y",
      "20-59y", "60-64y", "65-69y", "70-74y", "75+y"
    )
  )] %>% as.data.frame()


  Posteriori_Dat <- copy(dat)
  CasesResult <- lapply(seq_along(Posteriori_Dat), \(n_Sim){
    SubCases <- dat[[n_Sim]]
    MeltTable <- melt.data.table(SubCases, id.vars = c("time", "week"), variable.name = "age_group", value.name = "Cases")
    MeltTable <- MeltTable[, age_group := factor(age_group,
      levels = c(
        "Reported_G1", "Reported_G2", "Reported_G3",
        "Reported_G4", "Reported_G5", "Reported_G6",
        "Reported_G7", "Reported_G8", "Reported_G9",
        "Reported_G10", "Reported_G11"
      ),
      labels = c(
        "0-2m", "3-5m", "6-11m", "12-23m", "2-4y", "5-19y",
        "20-59y", "60-64y", "65-69y", "70-74y", "75+y"
      )
    )][, ":="(week = ISOweek::ISOweek2date(paste0(week, "-1")),
      SimNum = n_Sim)]
  })

  CasesResult_Bind <- rbindlist(CasesResult) %>% as.data.frame()

  PeakLead <- copy(PeakDat)
  PeakLead <- PeakLead[TimeInterval > 0][, age_group := factor(age_group,
    levels = c(
      "Reported_G1", "Reported_G2", "Reported_G3",
      "Reported_G4", "Reported_G5", "Reported_G6",
      "Reported_G7", "Reported_G8", "Reported_G9",
      "Reported_G10", "Reported_G11"
    ),
    labels = c(
      "0-2m", "3-5m", "6-11m", "12-23m", "2-4y", "5-19y",
      "20-59y", "60-64y", "65-69y", "70-74y", "75+y"
    )
  )][, TimeInterval := as.numeric(TimeInterval)] %>% as.data.frame()

  PeakBehind <- copy(PeakDat)
  PeakBehind <- PeakBehind[TimeInterval < 0][, age_group := factor(age_group,
    levels = c(
      "Reported_G1", "Reported_G2", "Reported_G3",
      "Reported_G4", "Reported_G5", "Reported_G6",
      "Reported_G7", "Reported_G8", "Reported_G9",
      "Reported_G10", "Reported_G11"
    ),
    labels = c(
      "0-2m", "3-5m", "6-11m", "12-23m", "2-4y", "5-19y",
      "20-59y", "60-64y", "65-69y", "70-74y", "75+y"
    )
  )][, TimeInterval := as.numeric(TimeInterval)] %>% as.data.frame()

  PeakSame <- copy(PeakDat)
  PeakSame <- PeakSame[TimeInterval == 0][, age_group := factor(age_group,
    levels = c(
      "Reported_G1", "Reported_G2", "Reported_G3",
      "Reported_G4", "Reported_G5", "Reported_G6",
      "Reported_G7", "Reported_G8", "Reported_G9",
      "Reported_G10", "Reported_G11"
    ),
    labels = c(
      "0-2m", "3-5m", "6-11m", "12-23m", "2-4y", "5-19y",
      "20-59y", "60-64y", "65-69y", "70-74y", "75+y"
    )
  )][, TimeInterval := as.numeric(TimeInterval)] %>% as.data.frame()

  PeakDat <- copy(PeakDat)
  PeakDat <- PeakDat[, age_group := factor(age_group,
    levels = c(
      "Reported_G1", "Reported_G2", "Reported_G3",
      "Reported_G4", "Reported_G5", "Reported_G6",
      "Reported_G7", "Reported_G8", "Reported_G9",
      "Reported_G10", "Reported_G11"
    ),
    labels = c(
      "0-2m", "3-5m", "6-11m", "12-23m", "2-4y", "5-19y",
      "20-59y", "60-64y", "65-69y", "70-74y", "75+y"
    )
  )][, TimeInterval := as.numeric(TimeInterval)] %>% as.data.frame()


  Fig <- ggplot() +
    geom_line(data = Real, aes(x = week, y = summ, group = age_group), linewidth = 1.2) +
    geom_line(data = CasesResult_Bind, aes(x = week, y = Cases, group = interaction(SimNum, age_group)), alpha = 0.05, colour = "#fe4b65", linewidth = 0.8) +
    geom_line(data = MedianDat, aes(x = week, y = Cases, group = age_group), colour = "#c9283a", linewidth = 1) +
    geom_vline(xintercept = as.Date(c("2018-01-01", "2019-01-01", "2020-01-01")), linetype = "dashed", alpha = 0.2) +
    geom_vline(data = PeakDat, aes(xintercept = as.Date(RefDate), group = age_group), alpha = 0.7) +
    geom_rect(data = PeakLead, aes(xmin = as.Date(RefDate), xmax = as.Date(time), ymin = 100, ymax = 105, group = age_group), fill = "#fe3627") +
    geom_label(data = PeakLead, aes(x = as.Date(RefDate) + 55, y = 85, label = TimeInterval, group = age_group), color = "black", fill = "white", size = 4, vjust = -0.3) +
    geom_rect(data = PeakBehind, aes(xmin = time, xmax = RefDate, ymin = 95, ymax = 100, group = age_group), fill = "#049143") +
    geom_label(data = PeakBehind, aes(x = as.Date(RefDate) - 55, y = 85, label = TimeInterval, group = age_group), color = "black", fill = "white", size = 4, vjust = -0.3) +
    geom_label(data = PeakSame, aes(x = as.Date(RefDate), y = 85, label = TimeInterval, group = age_group), color = "black", fill = "white", size = 4, vjust = -0.3) +
    labs(x = "Date", y = "Number of cases") +
    theme_minimal() +
    scale_x_date(date_labels = "%Y") +
    facet_wrap(~age_group) +
    theme(
      axis.text.x = element_text(size = 18, hjust = 1),
      axis.text.y = element_text(size = 18),
      axis.title = element_text(size = 24),
      strip.text = element_text(size = 24)
    )

  if (save == TRUE) {
    ggsave(Fig, file = paste0(path, ".pdf"), width = width, height = height)
  }

  return(Fig)
}


MCMC.PosteriorPlot.2age <- function(
    dat, MedianDat, PeakDat, RealDat = RealDat_plot_2age,
    save = FALSE, path = NULL, width = 12, height = 6) {
  Real <- copy(RealDat)
  Real <- Real[, week := ISOweek::ISOweek2date(paste0(week, "-1"))][, age_group := factor(age_group,
    levels = c("Reported_G1", "Reported_G2"),
    labels = c("<5y", ">=5y")
  )]

  MedianDat <- copy(MedianDat)
  MedianDat[, Reported_G1 := rowSums(.SD), .SDcols = paste0("Reported_G", 1:5)]
  MedianDat[, Reported_G2 := rowSums(.SD), .SDcols = paste0("Reported_G", 6:11)]
  MedianDat[, paste0("Reported_G", 3:11) := NULL]

  MedianDat <- melt.data.table(MedianDat, id.vars = c("time", "week"), variable.name = "age_group", value.name = "Cases")
  MedianDat <- MedianDat[, week := ISOweek::ISOweek2date(paste0(week, "-1"))][, age_group := factor(age_group,
    levels = c("Reported_G1", "Reported_G2"),
    labels = c("<5y", ">=5y")
  )] %>% as.data.frame()



  Posteriori_Dat <- copy(dat)
  CasesResult <- lapply(seq_along(Posteriori_Dat), \(n_Sim){
    SubCases <- dat[[n_Sim]]
    SubCases[, Reported_G1 := rowSums(.SD), .SDcols = paste0("Reported_G", 1:5)]
    SubCases[, Reported_G2 := rowSums(.SD), .SDcols = paste0("Reported_G", 6:11)]
    SubCases[, paste0("Reported_G", 3:11) := NULL]


    MeltTable <- melt.data.table(SubCases, id.vars = c("time", "week"), variable.name = "age_group", value.name = "Cases")
    MeltTable <- MeltTable[, age_group := factor(age_group,
      levels = c("Reported_G1", "Reported_G2"),
      labels = c("<5y", ">=5y")
    )][, ":="(week = ISOweek::ISOweek2date(paste0(week, "-1")),
      SimNum = n_Sim)]
  })

  CasesResult_Bind <- rbindlist(CasesResult) %>% as.data.frame()

  PeakLead <- copy(PeakDat)
  PeakLead <- PeakLead[TimeInterval > 0][, age_group := factor(age_group,
    levels = c("Reported_G1", "Reported_G2"),
    labels = c("<5y", ">=5y")
  )][, TimeInterval := as.numeric(TimeInterval)] %>% as.data.frame()

  PeakBehind <- copy(PeakDat)
  PeakBehind <- PeakBehind[TimeInterval < 0][, age_group := factor(age_group,
    levels = c("Reported_G1", "Reported_G2"),
    labels = c("<5y", ">=5y")
  )][, TimeInterval := as.numeric(TimeInterval)] %>% as.data.frame()

  PeakSame <- copy(PeakDat)
  PeakSame <- PeakSame[TimeInterval == 0][, age_group := factor(age_group,
    levels = c("Reported_G1", "Reported_G2"),
    labels = c("<5y", ">=5y")
  )][, TimeInterval := as.numeric(TimeInterval)] %>% as.data.frame()

  PeakDat <- copy(PeakDat)
  PeakDat <- PeakDat[, age_group := factor(age_group,
    levels = c("Reported_G1", "Reported_G2"),
    labels = c("<5y", ">=5y")
  )][, TimeInterval := as.numeric(TimeInterval)] %>% as.data.frame()


  Fig <- ggplot() +
    geom_line(data = Real, aes(x = week, y = summ, group = age_group), linewidth = 1.2) +
    geom_line(data = CasesResult_Bind, aes(x = week, y = Cases, group = interaction(SimNum, age_group)), alpha = 0.05, colour = "#fe4b65", linewidth = 0.8) +
    geom_line(data = MedianDat, aes(x = week, y = Cases, group = age_group), colour = "#c9283a", linewidth = 1) +
    geom_vline(xintercept = as.Date(c("2018-01-01", "2019-01-01", "2020-01-01")), linetype = "dashed", alpha = 0.2) +
    geom_vline(data = PeakDat, aes(xintercept = as.Date(RefDate), group = age_group), alpha = 0.7) +
    geom_rect(data = PeakLead, aes(xmin = as.Date(RefDate), xmax = as.Date(time), ymin = 320, ymax = 325, group = age_group), fill = "#fe3627") +
    geom_label(data = PeakLead, aes(x = as.Date(RefDate) + 55, y = 325, label = TimeInterval, group = age_group), color = "black", fill = "white", size = 4, vjust = -0.3) +
    geom_rect(data = PeakBehind, aes(xmin = time, xmax = RefDate, ymin = 320, ymax = 325, group = age_group), fill = "#049143") +
    geom_label(data = PeakBehind, aes(x = as.Date(RefDate) - 55, y = 325, label = TimeInterval, group = age_group), color = "black", fill = "white", size = 4, vjust = -0.3) +
    geom_label(data = PeakSame, aes(x = as.Date(RefDate), y = 325, label = TimeInterval, group = age_group), color = "black", fill = "white", size = 4, vjust = -0.3) +
    labs(x = "Date", y = "Number of cases") +
    theme_minimal() +
    scale_x_date(date_labels = "%Y") +
    facet_wrap(~age_group) +
    theme(
      axis.text.x = element_text(size = 18, hjust = 1),
      axis.text.y = element_text(size = 18),
      axis.title = element_text(size = 24),
      strip.text = element_text(size = 24)
    )

  if (save == TRUE) {
    ggsave(Fig, file = paste0(path, ".pdf"), width = width, height = height)
  }

  return(Fig)
}



Model.RunSim.Immu <- function(Parm, lag = FALSE) {
  # times <- seq(from = 1, to = 365 * Parm[["years"]])
  times <- as.numeric(seq(from = as.Date(Parm[["year_start"]]), to = as.Date(Parm[["year_end"]]), by = 1))

  state <- Get.InitState(
    num_age = Parm[["num_age"]],
    M_num = Parm[["M_num"]],
    population = Parm[["population"]],
    inf_num = Parm[["inf_num"]],
    model = "SIRV"
  )

  SimResult <- ode(
    y = state, times = times, func = ModelSimCpp_Immu, parms = Parm, method = "lsoda"
  )
  SimResult <- Model.GetI(SimResult, Parm[["Hosp_rate"]], lag = lag)

  return(SimResult)
}


# Calculate the vaccination protection
Vac.Protection <- function(
    ModelParm, lag, Age_Sus, Vac_start, Effacy, VacProp,
    ModelType = c("Inpatient", "Infection")) {
  if (ModelType == "Infection") {
    ModelParm[5:15] <- 1
  }

  ImmuHosp <- Model.RunSim.Immu(Parm = Parameter.Create(
    beta_base = ModelParm[1],
    beta_seasonal = ModelParm[2],
    phi = ModelParm[3],
    seasonal_wavelength = ModelParm[4],
    Hosp_rate = ModelParm[5:15],
    Age_Sus = Age_Sus,
    Vac_start = Vac_start,
    Effacy = Effacy,
    VacProp = VacProp
  ), lag = lag)

  ImmuHosp_NewConfirm <- copy(ImmuHosp[[2]])

  ImmuHosp_NewConfirm <- ImmuHosp_NewConfirm[, (3:13) := lapply(.SD, round),
    .SDcols = c(3:13)
  ][, week_num := substr(week, 7, 8)]

  ImmuHosp_NewConfirm_filter <- ImmuHosp_NewConfirm[week_num %in% c(1:10, 40:53), ]

  ImmuHosp_ByWeek <- ImmuHosp_NewConfirm_filter[, lapply(.SD, sum), .SDcols = c(3:13), by = .(week_num)]

  ImmuHosp_ByWeek_long <- melt(ImmuHosp_ByWeek, id.vars = "week_num", variable.name = "age_group", value.name = "cases")

  ImmuHosp_summ <- ImmuHosp_ByWeek_long[, .(cases = sum(cases)), by = .(age_group)]


  ImmuHosp_summ_2age <- copy(ImmuHosp_summ)[, age_group := fcase(
    age_group %in% c("Reported_G1", "Reported_G2", "Reported_G3", "Reported_G4", "Reported_G5"), "Reported_G1",
    default = "Reported_G2"
  )][, .(cases = sum(cases)), by = .(age_group)]




  BaseHosp <- Model.RunSim.Immu(Parm = Parameter.Create(
    beta_base = ModelParm[1],
    beta_seasonal = ModelParm[2],
    phi = ModelParm[3],
    seasonal_wavelength = ModelParm[4],
    Hosp_rate = ModelParm[5:15],
    Age_Sus = Age_Sus,
    Vac_start = Vac_start,
    Effacy = 0,
    VacProp = VacProp
  ), lag = lag)

  BaseHosp_NewConfirm <- copy(BaseHosp[[2]])

  BaseHosp_NewConfirm <- BaseHosp_NewConfirm[, (3:13) := lapply(.SD, round),
    .SDcols = c(3:13)
  ][, week_num := substr(week, 7, 8)]

  BaseHosp_NewConfirm_filter <- BaseHosp_NewConfirm[week_num %in% c(1:10, 40:53), ]

  BaseHosp_ByWeek <- BaseHosp_NewConfirm_filter[, lapply(.SD, sum), .SDcols = c(3:13), by = .(week_num)]

  BaseHosp_ByWeek_long <- melt(BaseHosp_ByWeek, id.vars = "week_num", variable.name = "age_group", value.name = "cases")

  BaseHosp_summ <- BaseHosp_ByWeek_long[, .(cases = sum(cases)), by = .(age_group)]


  BaseHosp_summ_2age <- copy(BaseHosp_summ)[, age_group := fcase(
    age_group %in% c("Reported_G1", "Reported_G2", "Reported_G3", "Reported_G4", "Reported_G5"), "Reported_G1",
    default = "Reported_G2"
  )][, .(cases = sum(cases)), by = .(age_group)]


  NetProtect <- copy(BaseHosp_summ)[, cases := cases - ImmuHosp_summ$cases]
  NetProtect_2age <- copy(BaseHosp_summ_2age)[, cases := cases - ImmuHosp_summ_2age$cases]

  VacProtection <- copy(BaseHosp_summ)[, protect := round(((cases - ImmuHosp_summ$cases) / cases) * 100, 2)]
  VacProtection_2age <- copy(BaseHosp_summ_2age)[, protect := round(((cases - ImmuHosp_summ_2age$cases) / cases) * 100, 2)]

  DirectProtect <- VacProtection_2age[1, 3]
  IndirectProtect <- VacProtection_2age[2, 3]


  BaseCase <- copy(BaseHosp_summ_2age)[, cases := sum(cases)][1, 2]
  ImmuCase <- copy(ImmuHosp_summ_2age)[, cases := sum(cases)][1, 2]

  TotalProtect <- round(((BaseCase - ImmuCase) / BaseCase) * 100, 2)

  return(list(
    NetProtect = NetProtect,
    NetProtect_2age = NetProtect_2age,
    VacProtection = VacProtection,
    DirectProtect = DirectProtect,
    IndirectProtect = IndirectProtect,
    TotalProtect = TotalProtect
  ))
}




Vac.Posterior <- function(VacList) {
  DirectProtect <- do.call(rbind, lapply(VacList, \(x) x[["DirectProtect"]]))
  IndirectProtect <- do.call(rbind, lapply(VacList, \(x) x[["IndirectProtect"]]))
  TotalProtect <- do.call(rbind, lapply(VacList, \(x) x[["TotalProtect"]]))

  DirectProtect_CI <- round(quantile(unlist(DirectProtect), probs = c(0.025, 0.5, 0.975)), 2)
  IndirectProtect_CI <- round(quantile(unlist(IndirectProtect), probs = c(0.025, 0.5, 0.975)), 2)
  TotalProtect_CI <- round(quantile(unlist(TotalProtect), probs = c(0.025, 0.5, 0.975)), 2)

  result <- data.frame(
    type = c("DirectProtect", "IndirectProtect", "TotalProtect"),
    median = c(DirectProtect_CI[2], IndirectProtect_CI[2], TotalProtect_CI[2]),
    lci = c(DirectProtect_CI[1], IndirectProtect_CI[1], TotalProtect_CI[1]),
    uci = c(DirectProtect_CI[3], IndirectProtect_CI[3], TotalProtect_CI[3])
  )

  result$CI <- paste0(result$median, " (", result$lci, " - ", result$uci, ")")
  return(result)
}
