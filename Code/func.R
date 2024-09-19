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

  clusterEvalQ(ParallelNodesInfo[[1]], {
    library("Rcpp")
    library("RcppArmadillo")
    library("data.table")
    library("tidyverse")
    library("extraDistr")
    library("truncnorm")
    library("progress")
    library("deSolve")
    sourceCpp("Code/model_cppV4.cpp")
    # sourceCpp("Code/model_SEIR_cpp.cpp")

    # sourceCpp("Code/model_immuV3.cpp")
    sourceCpp("Code/model_immuV4.cpp")
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
Get.StateName <- function(num_age = num_age, model = c("SIR", "SEIR", "SIRV", "SIRVV")) {
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
  } else if (model == "SEIR") {
    for (i in 1:num_age) {
      if (i == 1) {
        state_nam_temp <- c(
          paste0("M"),
          paste0("S0_G", i), paste0("E0_G", i), paste0("I0_G", i), paste0("R0_G", i),
          paste0("S1_G", i), paste0("E1_G", i), paste0("I1_G", i), paste0("R1_G", i),
          paste0("S2_G", i), paste0("E2_G", i), paste0("I2_G", i), paste0("R2_G", i),
          paste0("Reported_G", i)
        )
      } else {
        state_nam_temp <- c(
          paste0("S0_G", i), paste0("E0_G", i), paste0("I0_G", i), paste0("R0_G", i),
          paste0("S1_G", i), paste0("E1_G", i), paste0("I1_G", i), paste0("R1_G", i),
          paste0("S2_G", i), paste0("E2_G", i), paste0("I2_G", i), paste0("R2_G", i),
          paste0("Reported_G", i)
        )
      }
      state_nam <- c(state_nam, state_nam_temp)
    }
  } else if (model == "SIRVV") {
    for (i in 1:num_age) {
      if (i == 1) {
        state_nam_temp <- c(
          paste0("M"),
          paste0("S0_G", i), paste0("V0_G", i), paste0("SV0_G", i), paste0("I0_G", i), paste0("IV0_G", i), paste0("R0_G", i),
          paste0("S1_G", i), paste0("V1_G", i), paste0("SV1_G", i), paste0("I1_G", i), paste0("IV1_G", i), paste0("R1_G", i),
          paste0("S2_G", i), paste0("V2_G", i), paste0("SV2_G", i), paste0("I2_G", i), paste0("IV2_G", i), paste0("R2_G", i),
          paste0("Reported_G", i), paste0("Reported_IV_G", i)
        )
      } else {
        state_nam_temp <- c(
          paste0("S0_G", i), paste0("V0_G", i), paste0("SV0_G", i), paste0("I0_G", i), paste0("IV0_G", i), paste0("R0_G", i),
          paste0("S1_G", i), paste0("V1_G", i), paste0("SV1_G", i), paste0("I1_G", i), paste0("IV1_G", i), paste0("R1_G", i),
          paste0("S2_G", i), paste0("V2_G", i), paste0("SV2_G", i), paste0("I2_G", i), paste0("IV2_G", i), paste0("R2_G", i),
          paste0("Reported_G", i), paste0("Reported_IV_G", i)
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
  } else if (model == "SEIR") {
    state <- vector("numeric", 13 * num_age + 1)
    names(state) <- Get.StateName(num_age = num_age, model = "SEIR")
    state[1] <- M_num
    for (i in 1:num_age) {
      state[2 + (i - 1) * 13] <- as.numeric(population[i])
    }
    for (i in 1:num_age) { # assign the initial number of infected cases, 20
      state[4 + (i - 1) * 13] <- inf_num
    }
  } else if (model == "SIRVV") {
    state <- vector("numeric", 20 * num_age + 1)
    names(state) <- Get.StateName(num_age = num_age, model = "SIRVV")
    state[1] <- M_num
    for (i in 1:num_age) {
      state[2 + (i - 1) * 20] <- as.numeric(population[i])
    }
    for (i in 1:num_age) { # assign the initial number of infected cases, 20
      state[5 + (i - 1) * 20] <- inf_num
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
    year_end = "2020-06-27",
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
    seasonal_wavelength = 34.66609,
    phi = 0,
    contact_str = ContacrStr,
    gamma = 1 / 7, # 0.1428571; 7 days or 11 days
    omega_inf = 1 / 300, # 0.005847953; Duration of immunity from infection
    ReducedSus_1 = 1, # 0.76, # Reduced sus, 2 vs 1
    ReducedSus_2 = 1, # 0.88, # Reduced sus, n vs 2
    ReducedRec_1 = 1.37, # 1 / 0.87, # Reduced recovery rate, 2 vs 1
    ReducedRec_2 = 1, # 1 / 0.79, # Reduced recovery rate, n vs 2
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
    Age_Sus = c(0.2, 0.6),
    # Vaccine
    Vac_start = "2017-03-29",
    Effacy_I = 0.75,
    Effacy_Hosp = 0.8,
    VacProp = 0.4,
    omega_vac = 1 / 180,
    VacAgeGroup = "S1") {
  # Modify the first age group
  age_rates_1 <- 1 / ((1 / age_rates[1]) - omega_m)
  age_rates <- c(age_rates_1, age_rates[-1])

  # Calibration of vaccined proportion
  NeedVacPopulation <- -log(1 - (Effacy_I * VacProp)) # 用这个替换Efface*VacProp

  # Calibration of vaccined proportion
  NeedVacPopulation_SV <- -log(1 - ((Effacy_Hosp - Effacy_I) * VacProp)) # 用这个替换Efface*VacProp

  switch(VacAgeGroup,
    "S1" = {
      VacAgeGroup <- c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
      # 0-2 m, 3-5 m, 6-11 m, 1-2 y, 2-4 y, 5-19 y, 20-59 y, 60-64 y, 65-69 y, 70-74 y, 75+ y
    },
    "S2" = {
      VacAgeGroup <- c(1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)
    },
    "S3" = {
      VacAgeGroup <- c(1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0)
    },
    "S4" = {
      VacAgeGroup <- c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0)
    }
  )


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
        Age_Sus[1], # 5-19 years
        Age_Sus[1], # 20-59 years
        Age_Sus[2], # 60-64 years
        Age_Sus[2], # 65-69 years
        Age_Sus[2], # 70-74 years
        Age_Sus[2] # 75+ years
      ),

      # Vaccine
      Vac_start = Vac_start,
      Effacy_I = Effacy_I,
      Effacy_Hosp = Effacy_Hosp,
      NeedVacPopulation = NeedVacPopulation,
      NeedVacPopulation_SV = NeedVacPopulation_SV,
      VacAgeGroup = VacAgeGroup,
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
      Reported_G11 = shift(Reported_G11, 7, type = "lag", fill = 0))][time >= as.Date("2017-06-27") & time <= as.Date("2020-06-27")][, (ExcludeCol) := lapply(.SD, sum),
      by = .(week), .SDcols = ExcludeCol
    ]
  } else {
    RealI <- RealI[, week := substr(ISOweek::date2ISOweek(time), 1, 8)][time >= as.Date("2017-06-27") & time <= as.Date("2020-06-27")][, (ExcludeCol) := lapply(.SD, sum), by = .(week), .SDcols = ExcludeCol]
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
Model.RunSim <- function(Parm, lag = FALSE, model = "SIR") {
  # times <- seq(from = 1, to = 365 * Parm[["years"]])
  times <- as.numeric(seq(from = as.Date(Parm[["year_start"]]), to = as.Date(Parm[["year_end"]]), by = 1))
  state <- Get.InitState(
    num_age = Parm[["num_age"]],
    M_num = Parm[["M_num"]],
    population = Parm[["population"]],
    inf_num = Parm[["inf_num"]],
    model = model
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
  if (length(grep("^V0", colnames(SummDat), value = TRUE)) != 0) {
    ColWithV0 <- grep("^V0", colnames(SummDat), value = TRUE)
    ColWithV1 <- grep("^V1", colnames(SummDat), value = TRUE)
    ColWithV2 <- grep("^V2", colnames(SummDat), value = TRUE)
    SummDat[, R0 := rowSums(.SD),
      .SDcols = ColWithR0
    ][, I0 := rowSums(.SD),
      .SDcols = ColWithI0
    ][, S0 := rowSums(.SD),
      .SDcols = ColWithS0
    ][, R1 := rowSums(.SD),
      .SDcols = ColWithR1
    ][, I1 := rowSums(.SD),
      .SDcols = ColWithI1
    ][, S1 := rowSums(.SD),
      .SDcols = ColWithS1
    ][, R2 := rowSums(.SD),
      .SDcols = ColWithR2
    ][, I2 := rowSums(.SD),
      .SDcols = ColWithI2
    ][, S2 := rowSums(.SD),
      .SDcols = ColWithS2
    ][, V0 := rowSums(.SD),
      .SDcols = ColWithV0
    ][, V1 := rowSums(.SD),
      .SDcols = ColWithV1
    ][, V2 := rowSums(.SD),
      .SDcols = ColWithV2
    ]

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
      geom_line(aes(x = time, y = V0), colour = "green") +
      geom_line(aes(x = time, y = V1), colour = "orange", linetype = 2) +
      geom_line(aes(x = time, y = V2), colour = "cyan", linetype = 3, linewidth = 1.5) +
      theme_minimal()
  } else {
    SummDat[, R0 := rowSums(.SD),
      .SDcols = ColWithR0
    ][, I0 := rowSums(.SD),
      .SDcols = ColWithI0
    ][, S0 := rowSums(.SD),
      .SDcols = ColWithS0
    ][, R1 := rowSums(.SD),
      .SDcols = ColWithR1
    ][, I1 := rowSums(.SD),
      .SDcols = ColWithI1
    ][, S1 := rowSums(.SD),
      .SDcols = ColWithS1
    ][, R2 := rowSums(.SD),
      .SDcols = ColWithR2
    ][, I2 := rowSums(.SD),
      .SDcols = ColWithI2
    ][, S2 := rowSums(.SD), .SDcols = ColWithS2]

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
  }


  # Simualtion result of reported infected cases
  CasesResult <- CasesResult %>% as.data.table()
  CasesResult <- melt.data.table(CasesResult, id.vars = c("time", "week"), variable.name = "age_group", value.name = "Cases")

  MergeDat <- merge(CasesResult, RealDat, by = c("week", "age_group"), all.x = T)

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
Model.RunSim.LLH <- function(
    Parm, TargetDat = RefDat, lag = FALSE, model = "SIR",
    AgeDistribution = FALSE, AgeDistributionDat = RealDatDistribution) {
  Dat <- Model.RunSim(Parm, lag = lag, model = model)
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


  # InfeRate <- copy(Dat[[2]])
  # InfeRate[, SumCase := Reported_G1/Parm$Hosp_rate[1] + Reported_G2/Parm$Hosp_rate[2] + Reported_G3/Parm$Hosp_rate[3]]
  # InfeRate_prob <- (sum(InfeRate$SumCase) / 3) / sum(Scot_Pop[1:3])
  # LLH_InfRate <- dbinom(x = 258, size = 490, prob = InfeRate_prob, log = T)
  #
  # LLH_Out <- LLH_Out + LLH_InfRate

  if (is.infinite(LLH_Out)) {
    LLH_Out <- -10000000
  }


  if (AgeDistribution == TRUE) { ## Calculate the distribution of the simulated data
    SimResult <- copy(Dat[[2]])
    ## 1 group, no time difference
    # MonthDat <- SimResult[month(time) %in% c(10, 11, 12, 1)]
    # MonthDat <- MonthDat[, Month := paste0(year(time), "-", month(time))][,
    #   lapply(.SD, sum),
    #   .SDcol = c(3:13), by = Month
    # ][
    #   ,
    #   Mark := c(rep(1, 4), rep(2, 4), rep(3, 4))
    # ][,
    #   lapply(.SD, sum),
    #   .SDcol = -c("Month", "Mark"), by = Mark
    # ][,
    #   lapply(.SD, \(x) x / sum(.SD)),
    #   by = Mark, .SDcol = -"Mark"
    # ][, Mark := NULL]

    ### 2 groups, with time difference: 10-11, 12-1
    MonthDat <- SimResult[month(time) %in% c(10, 11, 12, 1)]
    MonthDat <- MonthDat[, TimeLabel := fcase(month(time) %in% 10:11, "Before", month(time) %in% 12:1, "After")][
      ,
      Month := paste0(year(time), "-", month(time))
    ][,
      lapply(.SD, sum),
      .SDcol = c(3:13), by = c("Month", "TimeLabel")
    ][
      ,
      Mark := c(rep(1, 4), rep(2, 4), rep(3, 4))
    ][,
      lapply(.SD, sum),
      .SDcol = -c("Month", "Mark", "TimeLabel"), by = c("Mark", "TimeLabel")
    ][, lapply(.SD, round, 0), .SDcol = -c("Mark", "TimeLabel")]

    LLH_Group <- sapply(seq_len(nrow(RealDatDistribution)), function(i) {
      ddirmnom(as.numeric(RealDatDistribution[i, ]),
        sum(as.numeric(RealDatDistribution[i, ])),
        alpha = (as.numeric(MonthDat[i, ]) / sum(MonthDat[i, ])) * 100, log = TRUE
      )
    })

    LLH_Out <- LLH_Out + sum(LLH_Group)

    if (is.nan(LLH_Out) | is.na(LLH_Out) | is.infinite(LLH_Out)) {
      LLH_Out <- -10000000
    }
  }

  # Penalize parameters that produce more than 3 peaks in 1 year and the minimum peak is less than 20
  Peak_Num <- splus2R::peaks(SimDat[variable == "Reported_G1", 4], span = 21)
  MinPeak <- min(SimDat[variable == "Reported_G1" & Peak_Num$value, 4])

  if (sum(Peak_Num) != 3 | MinPeak < 20) {
    LLH_Out <- -10000000
  }

  if (SimDat[time == "2018-06-25" & variable == "Reported_G1", 4] > 5) {
    LLH_Out <- -10000000
  }

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
  ParmNew_base <- rtruncnorm(1, a = 0.3, b = 1, mean = Parm[1], sd = 0.0009) # 0.02, Parm[1] / 20 0.005 # beta_base

  ParmNew_seasonal <- rtruncnorm(1, a = 2, b = 10, mean = Parm[2], sd = 0.09) # 0.2 Parm[2] / 10  0.005 # beta_seasonal

  ParmNew2 <- rtruncnorm(1, a = -45, b = 20, mean = Parm[3], sd = 0) # 0.3) # Parm[3] + runif(1, -1.8, 1.8) # phi
  # a = -40, b = -10,
  # a = -182, b = 183
  # a = -30, b = 20

  ParmNew3 <- Parm[4] # seasonal_wavelength
  # ParmNew3 <- rtruncnorm(1, a = 28, b = 35, mean = Parm[4], sd = 0.03 * step) # 0.0004 seasonal_wavelength

  # ParmNew4 <- rtruncnorm(11, a = 0, b = 0.05, mean = Parm[c(5:15)], sd = Parm[c(5:15)] / 12) # Hosp_rate
  ParmNew4.0 <- rtruncnorm(1, a = 0.05, b = 0.25, mean = Parm[c(5)], sd = 0.01) # Parm[c(5)] / 20 Hosp_rate 0.0005
  # b = 0.15
  ParmNew4.1 <- rtruncnorm(1, a = 0.0001, b = 0.1, mean = Parm[c(6)], sd = 0.005) # Parm[c(6:10)] / 20 Hosp_rate 0.0005
  ParmNew4.2 <- rtruncnorm(1, a = 0.0001, b = 0.1, mean = Parm[c(7)], sd = 0.002) # Parm[c(6:10)] / 20 Hosp_rate 0.0005
  ParmNew4.3 <- rtruncnorm(1, a = 0.0001, b = 0.1, mean = Parm[c(8)], sd = 0.0005) # Parm[c(6:10)] / 20 Hosp_rate 0.0005
  ParmNew4.4 <- rtruncnorm(1, a = 0.0001, b = 0.01, mean = Parm[c(9)], sd = 0.0001) # Parm[c(6:10)] / 20 Hosp_rate 0.0005
  ParmNew4.5 <- rtruncnorm(1, a = 0.0001, b = 0.01, mean = Parm[c(10)], sd = 0.00002) # Parm[c(6:10)] / 20 Hosp_rate 0.0005

  ParmNew4.6 <- rtruncnorm(1, a = 0.0001, b = 0.001, mean = Parm[c(11)], sd = 0.00002) # Parm[c(11)] / 12  0.00004# Hosp_rate
  ParmNew4.7 <- rtruncnorm(1, a = 0.0001, b = 0.01, mean = Parm[c(12)], sd = 0.0002) # Parm[c(12)] / 12 0.00004# Hosp_rate
  # a = 0.001
  ParmNew4.8 <- rtruncnorm(1, a = 0.0001, b = 0.03, mean = Parm[c(13)], sd = 0.0002) # 0.001 # Parm[c(13)] / 12 0.00004# Hosp_rate
  ParmNew4.9 <- rtruncnorm(1, a = 0.0001, b = 0.03, mean = Parm[c(14)], sd = 0.0002) # 0.001 # Parm[c(14)] / 12 0.00004# Hosp_rate
  ParmNew4.10 <- rtruncnorm(1, a = 0.001, b = 0.01, mean = Parm[c(15)], sd = 0.0002) # 0.001 # Parm[c(15)] / 12 0.00004# Hosp_rate

  ParmNew4 <- c(
    ParmNew4.0,
    ParmNew4.1,
    ParmNew4.2,
    ParmNew4.3, # ParmNew4.3, ParmNew4.3,
    ParmNew4.4,
    ParmNew4.5,
    ParmNew4.6,
    ParmNew4.7,
    ParmNew4.8,
    ParmNew4.9,
    ParmNew4.10
  )

  ParmOut <- c(ParmNew_base, ParmNew_seasonal, ParmNew2, ParmNew3, ParmNew4)
  return(ParmOut)
}


#' @title Run MCMC model
#' @param Prior: prior parameters
MCMC.MH <- function(
    Prior, n_iterations, TargetDat = RefDat, lag = FALSE, Sus_reduce, model = "SIR",
    AgeDistribution = FALSE, AgeDistributionDat = RealDatDistribution) {
  chain <- matrix(NA, nrow = n_iterations, ncol = 15 + 1)
  chain[1, -16] <- Prior
  # accepted <- 0

  current_log_likelihood <- Model.RunSim.LLH(
    Parm = Parameter.Create(
      beta_base = chain[1, 1], beta_seasonal = chain[1, 2], phi = chain[1, 3],
      seasonal_wavelength = chain[1, 4],
      Hosp_rate = c(chain[1, 5:15]), Age_Sus = Sus_reduce
    ), TargetDat = TargetDat, lag = lag, model = model, AgeDistribution, AgeDistributionDat
  )
  chain[1, 16] <- current_log_likelihood

  pb <- progress_bar$new(total = n_iterations, clear = TRUE, format = "  [:bar] :percent :etas")
  pb$tick()
  for (i in 2:n_iterations) {
    proposal <- MCMC.Proposal(Parm = chain[i - 1, -16])
    proposal_log_likelihood <- Model.RunSim.LLH(
      Parm = Parameter.Create(
        beta_base = proposal[1], beta_seasonal = proposal[2], phi = proposal[3],
        seasonal_wavelength = proposal[4], Hosp_rate = c(proposal[5:15]), Age_Sus = Sus_reduce
      ), TargetDat = TargetDat, lag = lag, model = model, AgeDistribution, AgeDistributionDat
    )

    # Info <- sprintf("n_iteration is: %d Current LLH is: %f Proposal LLH is: %f", i, current_log_likelihood, proposal_log_likelihood)
    # CLI.Print(Info)
    if (i %% 200 == 0) {
      CLI.Print("Current iteration is: ", i / n_iterations * 100)

      # acceptance_rate <- accepted / n_iterations
      # if(acceptance_rate < 0.2) {
      #   step <- step * 0.5
      # } else if(acceptance_rate > 0.5) {
      #   step <- step * 1.5
      # }
      # accepted <- 0
    }

    acceptance_ratio <- min(1, exp(proposal_log_likelihood - current_log_likelihood))
    if (runif(1) < acceptance_ratio) {
      chain[i, -16] <- proposal
      current_log_likelihood <- proposal_log_likelihood
      chain[i, 16] <- current_log_likelihood
      # accepted <- accepted + 1
    } else {
      chain[i, ] <- chain[i - 1, ]
    }

    pb$tick()
  }

  return(chain)
}

MCMC.Batch <- function(seed = 380, Prior, n_iterations, TargetDat = RefDat, lag = FALSE, Sus_reduce, model = "SIR",
                       AgeDistribution = FALSE, AgeDistributionDat = RealDatDistribution,
                       save = FALSE, file) {
  Parallel.Regist(ncores = 3, seed = seed)
  Parallel.Import(list(
    "CLI.Print", "Get.InitState", "Get.StateName", "Model.GetI", "MCMC.Proposal", "Model.RunSim",
    "Model.RunSim.LLH", "Parameter.Create", "Scot_Pop", "ContacrStr",
    "MCMC.MH", "RefDat", "PriorList", "RealDatDistribution"
  ))
  MCMC_Result <- parLapply(ParallelNodesInfo[[1]], PriorList, \(x) {
    setDTthreads(1)
    MCMC.MH(
      Prior = x, n_iterations = n_iterations, TargetDat = TargetDat, lag = lag,
      Sus_reduce = Sus_reduce, model = model,
      AgeDistribution = AgeDistribution, AgeDistributionDat = AgeDistributionDat
    )
  })

  if (save) {
    save(MCMC_Result, file = file)
  }

  Parallel.Stop()
  return(MCMC_Result)
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


MCMC.PostProcess <- function(dat, burn_in = 5000, thin = 10, n_sample = 500) {
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

  if (n_sample != 0) {
    LineNum <- sample(1:nrow(BindChain), n_sample, replace = TRUE)
    SampleChain <- BindChain[LineNum, ]
  } else {
    SampleChain <- NULL
  }

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
    SampleChain = SampleChain,
    Median = Median,
    CI = CI,
    posteriori = posteriori
  ))
}



FindPeak <- function(SimDat, span = 23, TargetDat = RefPeak) {
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


FindPeak.2age <- function(SimDat, span = 23, TargetDat = RefPeak_2age) {
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


Calu.LeadTimeCI <- function(Dat, AgeGroup_Num = 2) {
  if (AgeGroup_Num == 2) {
    CaluTimeDiff_batch <- lapply(Dat, FindPeak.2age)
  } else {
    CaluTimeDiff_batch <- lapply(Dat, FindPeak)
  }

  CaluTimeDiff_batch <- CaluTimeDiff_batch[sapply(CaluTimeDiff_batch, nrow) != 34]
  CaluTimeDiff_batch <- do.call(rbind, CaluTimeDiff_batch)

  CaluCI <- by(CaluTimeDiff_batch, CaluTimeDiff_batch$age_group, function(x) {
    Median <- median(x$TimeInterval)
    CI <- quantile(x$TimeInterval, c(0.025, 0.975), type = 1)
    return(data.frame(Median = Median, lci = CI[1], uci = CI[2]))
  })
  CaluCI <- do.call(rbind, CaluCI)
  CaluCI$age_group <- rownames(CaluCI)

  setcolorder(CaluCI, c("age_group", "Median", "lci", "uci"))

  return(CaluCI)
}


MCMC.PosteriorPlot.V2 <- function(
    dat, MedianDat, PeakDat, RealDat = RealDat_plot,
    save = FALSE, path = NULL, width = 12, height = 6) {
  ### Prepare Real data
  Real_Allage <- copy(RealDat)
  Real_Allage[, summ := sum(summ), by = week][, age_group := "All_age"]
  Real_Allage <- unique(Real_Allage)

  Real <- copy(RealDat)
  Real <- rbind(Real, Real_Allage)

  Real <- Real[, week := ISOweek::ISOweek2date(paste0(week, "-1"))][, age_group := factor(age_group,
    levels = c(
      "Reported_G1", "Reported_G2", "Reported_G3",
      "Reported_G4", "Reported_G5", "Reported_G6",
      "Reported_G7", "Reported_G8", "Reported_G9",
      "Reported_G10", "Reported_G11", "All_age"
    ),
    labels = c(
      "0-2m", "3-5m", "6-11m", "12-23m", "2-4y", "5-19y",
      "20-59y", "60-64y", "65-69y", "70-74y", "75+y", "All age"
    )
  )]

  ### Prepare Median data
  MedianDat <- melt.data.table(MedianDat, id.vars = c("time", "week"), variable.name = "age_group", value.name = "Cases")

  Median_Allage <- copy(MedianDat)
  Median_Allage[, Cases := sum(Cases), by = week][, age_group := "All_age"]
  Median_Allage <- unique(Median_Allage)
  MedianDat <- rbind(MedianDat, Median_Allage)

  MedianDat <- MedianDat[, week := ISOweek::ISOweek2date(paste0(week, "-1"))][, age_group := factor(age_group,
    levels = c(
      "Reported_G1", "Reported_G2", "Reported_G3",
      "Reported_G4", "Reported_G5", "Reported_G6",
      "Reported_G7", "Reported_G8", "Reported_G9",
      "Reported_G10", "Reported_G11", "All_age"
    ),
    labels = c(
      "0-2m", "3-5m", "6-11m", "12-23m", "2-4y", "5-19y",
      "20-59y", "60-64y", "65-69y", "70-74y", "75+y", "All age"
    )
  )] %>% as.data.frame()


  Posteriori_Dat <- copy(dat)
  CasesResult <- lapply(seq_along(Posteriori_Dat), \(n_Sim){
    SubCases <- dat[[n_Sim]]
    MeltTable <- melt.data.table(SubCases, id.vars = c("time", "week"), variable.name = "age_group", value.name = "Cases")

    MeltTable_Allage <- copy(MeltTable)
    MeltTable_Allage[, Cases := sum(Cases), by = week][, age_group := "All_age"]
    MeltTable_Allage <- unique(MeltTable_Allage)
    MeltTable <- rbind(MeltTable, MeltTable_Allage)

    MeltTable <- MeltTable[, age_group := factor(age_group,
      levels = c(
        "Reported_G1", "Reported_G2", "Reported_G3",
        "Reported_G4", "Reported_G5", "Reported_G6",
        "Reported_G7", "Reported_G8", "Reported_G9",
        "Reported_G10", "Reported_G11", "All_age"
      ),
      labels = c(
        "0-2m", "3-5m", "6-11m", "12-23m", "2-4y", "5-19y",
        "20-59y", "60-64y", "65-69y", "70-74y", "75+y", "All age"
      )
    )][, ":="(week = ISOweek::ISOweek2date(paste0(week, "-1")),
      SimNum = n_Sim)]
  })

  CasesResult_Bind <- rbindlist(CasesResult) %>% as.data.frame()

  Fig <- ggplot() +
    geom_line(data = Real, aes(x = week, y = summ, group = age_group), linewidth = 1.2) +
    geom_line(
      data = CasesResult_Bind, aes(x = week, y = Cases, group = interaction(SimNum, age_group)),
      alpha = 0.05, colour = "#fe4b65", linewidth = 0.8
    ) +
    geom_line(data = MedianDat, aes(x = week, y = Cases, group = age_group), colour = "#c9283a", linewidth = 1) +
    geom_vline(xintercept = as.Date(c("2018-01-01", "2019-01-01", "2020-01-01")), linetype = "dashed", alpha = 0.2) +
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

MCMC.PosteriorPlot.V2_Appendix <- function(
    dat, MedianDat, RealDat = RealDat_plot,
    save = FALSE, path = NULL, width = 12, height = 6) {
  ### Prepare Real data
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

  ### Prepare Median data
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

  Fig <- ggplot() +
    geom_line(data = Real, aes(x = week, y = summ, group = age_group), linewidth = 1.2) +
    geom_line(
      data = CasesResult_Bind, aes(x = week, y = Cases, group = interaction(SimNum, age_group)),
      alpha = 0.01, colour = "#fe4b65", linewidth = 0.8
    ) +
    geom_line(data = MedianDat, aes(x = week, y = Cases, group = age_group), colour = "#c9283a", linewidth = 1) +
    geom_vline(xintercept = as.Date(c("2018-01-01", "2019-01-01", "2020-01-01")), linetype = "dashed", alpha = 0.2) +
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


# MCMC.PosteriorPlot <- function(
#     dat, MedianDat, PeakDat, RealDat = RealDat_plot,
#     save = FALSE, path = NULL, width = 12, height = 6) {
#   Real <- copy(RealDat)
#   Real <- Real[, week := ISOweek::ISOweek2date(paste0(week, "-1"))][, age_group := factor(age_group,
#     levels = c(
#       "Reported_G1", "Reported_G2", "Reported_G3",
#       "Reported_G4", "Reported_G5", "Reported_G6",
#       "Reported_G7", "Reported_G8", "Reported_G9",
#       "Reported_G10", "Reported_G11"
#     ),
#     labels = c(
#       "0-2m", "3-5m", "6-11m", "12-23m", "2-4y", "5-19y",
#       "20-59y", "60-64y", "65-69y", "70-74y", "75+y"
#     )
#   )]
#
#   MedianDat <- melt.data.table(MedianDat, id.vars = c("time", "week"), variable.name = "age_group", value.name = "Cases")
#   MedianDat <- MedianDat[, week := ISOweek::ISOweek2date(paste0(week, "-1"))][, age_group := factor(age_group,
#     levels = c(
#       "Reported_G1", "Reported_G2", "Reported_G3",
#       "Reported_G4", "Reported_G5", "Reported_G6",
#       "Reported_G7", "Reported_G8", "Reported_G9",
#       "Reported_G10", "Reported_G11"
#     ),
#     labels = c(
#       "0-2m", "3-5m", "6-11m", "12-23m", "2-4y", "5-19y",
#       "20-59y", "60-64y", "65-69y", "70-74y", "75+y"
#     )
#   )] %>% as.data.frame()
#
#
#   Posteriori_Dat <- copy(dat)
#   CasesResult <- lapply(seq_along(Posteriori_Dat), \(n_Sim){
#     SubCases <- dat[[n_Sim]]
#     MeltTable <- melt.data.table(SubCases, id.vars = c("time", "week"), variable.name = "age_group", value.name = "Cases")
#     MeltTable <- MeltTable[, age_group := factor(age_group,
#       levels = c(
#         "Reported_G1", "Reported_G2", "Reported_G3",
#         "Reported_G4", "Reported_G5", "Reported_G6",
#         "Reported_G7", "Reported_G8", "Reported_G9",
#         "Reported_G10", "Reported_G11"
#       ),
#       labels = c(
#         "0-2m", "3-5m", "6-11m", "12-23m", "2-4y", "5-19y",
#         "20-59y", "60-64y", "65-69y", "70-74y", "75+y"
#       )
#     )][, ":="(week = ISOweek::ISOweek2date(paste0(week, "-1")),
#       SimNum = n_Sim)]
#   })
#
#   CasesResult_Bind <- rbindlist(CasesResult) %>% as.data.frame()
#
#   PeakLead <- copy(PeakDat)
#   PeakLead <- PeakLead[TimeInterval > 0][, age_group := factor(age_group,
#     levels = c(
#       "Reported_G1", "Reported_G2", "Reported_G3",
#       "Reported_G4", "Reported_G5", "Reported_G6",
#       "Reported_G7", "Reported_G8", "Reported_G9",
#       "Reported_G10", "Reported_G11"
#     ),
#     labels = c(
#       "0-2m", "3-5m", "6-11m", "12-23m", "2-4y", "5-19y",
#       "20-59y", "60-64y", "65-69y", "70-74y", "75+y"
#     )
#   )][, TimeInterval := as.numeric(TimeInterval)] %>% as.data.frame()
#
#   PeakBehind <- copy(PeakDat)
#   PeakBehind <- PeakBehind[TimeInterval < 0][, age_group := factor(age_group,
#     levels = c(
#       "Reported_G1", "Reported_G2", "Reported_G3",
#       "Reported_G4", "Reported_G5", "Reported_G6",
#       "Reported_G7", "Reported_G8", "Reported_G9",
#       "Reported_G10", "Reported_G11"
#     ),
#     labels = c(
#       "0-2m", "3-5m", "6-11m", "12-23m", "2-4y", "5-19y",
#       "20-59y", "60-64y", "65-69y", "70-74y", "75+y"
#     )
#   )][, TimeInterval := as.numeric(TimeInterval)] %>% as.data.frame()
#
#   PeakSame <- copy(PeakDat)
#   PeakSame <- PeakSame[TimeInterval == 0][, age_group := factor(age_group,
#     levels = c(
#       "Reported_G1", "Reported_G2", "Reported_G3",
#       "Reported_G4", "Reported_G5", "Reported_G6",
#       "Reported_G7", "Reported_G8", "Reported_G9",
#       "Reported_G10", "Reported_G11"
#     ),
#     labels = c(
#       "0-2m", "3-5m", "6-11m", "12-23m", "2-4y", "5-19y",
#       "20-59y", "60-64y", "65-69y", "70-74y", "75+y"
#     )
#   )][, TimeInterval := as.numeric(TimeInterval)] %>% as.data.frame()
#
#   PeakDat <- copy(PeakDat)
#   PeakDat <- PeakDat[, age_group := factor(age_group,
#     levels = c(
#       "Reported_G1", "Reported_G2", "Reported_G3",
#       "Reported_G4", "Reported_G5", "Reported_G6",
#       "Reported_G7", "Reported_G8", "Reported_G9",
#       "Reported_G10", "Reported_G11"
#     ),
#     labels = c(
#       "0-2m", "3-5m", "6-11m", "12-23m", "2-4y", "5-19y",
#       "20-59y", "60-64y", "65-69y", "70-74y", "75+y"
#     )
#   )][, TimeInterval := as.numeric(TimeInterval)] %>% as.data.frame()
#
#
# Fig <- ggplot() +
#   geom_line(data = Real, aes(x = week, y = summ, group = age_group), linewidth = 1.2) +
#   geom_line(
#     data = CasesResult_Bind, aes(x = week, y = Cases, group = interaction(SimNum, age_group)),
#     alpha = 0.05, colour = "#fe4b65", linewidth = 0.8
#   ) +
#   geom_line(data = MedianDat, aes(x = week, y = Cases, group = age_group), colour = "#c9283a", linewidth = 1) +
#   geom_vline(xintercept = as.Date(c("2018-01-01", "2019-01-01", "2020-01-01")), linetype = "dashed", alpha = 0.2) +
#   geom_vline(data = PeakDat, aes(xintercept = as.Date(RefDate), group = age_group), alpha = 0.7) +
#   geom_rect(data = PeakLead, aes(
#     xmin = as.Date(RefDate), xmax = as.Date(time),
#     ymin = 100, ymax = 105, group = age_group
#   ), fill = "#fe3627") +
#   geom_label(
#     data = PeakLead, aes(x = as.Date(RefDate) + 55, y = 85, label = TimeInterval, group = age_group),
#     color = "black", fill = "white", size = 4, vjust = -0.3
#   ) +
#   geom_rect(data = PeakBehind, aes(xmin = time, xmax = RefDate, ymin = 95, ymax = 100, group = age_group), fill = "#049143") +
#   geom_label(
#     data = PeakBehind, aes(x = as.Date(RefDate) - 55, y = 85, label = TimeInterval, group = age_group),
#     color = "black", fill = "white", size = 4, vjust = -0.3
#   ) +
#   geom_label(
#     data = PeakSame, aes(x = as.Date(RefDate), y = 85, label = TimeInterval, group = age_group),
#     color = "black", fill = "white", size = 4, vjust = -0.3
#   ) +
#   labs(x = "Date", y = "Number of cases") +
#   theme_minimal() +
#   scale_x_date(date_labels = "%Y") +
#   facet_wrap(~age_group) +
#   theme(
#     axis.text.x = element_text(size = 18, hjust = 1),
#     axis.text.y = element_text(size = 18),
#     axis.title = element_text(size = 24),
#     strip.text = element_text(size = 24)
#   )
#
#   if (save == TRUE) {
#     ggsave(Fig, file = paste0(path, ".pdf"), width = width, height = height)
#   }
#
#   return(Fig)
# }


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
    geom_line(
      data = CasesResult_Bind, aes(x = week, y = Cases, group = interaction(SimNum, age_group)),
      alpha = 0.05, colour = "#fe4b65", linewidth = 0.8
    ) +
    geom_line(data = MedianDat, aes(x = week, y = Cases, group = age_group), colour = "#c9283a", linewidth = 1) +
    geom_vline(xintercept = as.Date(c("2018-01-01", "2019-01-01", "2020-01-01")), linetype = "dashed", alpha = 0.2) +
    geom_vline(data = PeakDat, aes(xintercept = as.Date(RefDate), group = age_group), alpha = 0.7) +
    geom_rect(data = PeakLead, aes(
      xmin = as.Date(RefDate), xmax = as.Date(time), ymin = 320, ymax = 325,
      group = age_group
    ), fill = "#fe3627") +
    geom_label(data = PeakLead, aes(
      x = as.Date(RefDate) + 55, y = 325, label = TimeInterval,
      group = age_group
    ), color = "black", fill = "white", size = 4, vjust = -0.3) +
    geom_rect(data = PeakBehind, aes(
      xmin = time, xmax = RefDate, ymin = 320, ymax = 325, group = age_group
    ), fill = "#049143") +
    geom_label(
      data = PeakBehind, aes(x = as.Date(RefDate) - 55, y = 325, label = TimeInterval, group = age_group),
      color = "black", fill = "white", size = 4, vjust = -0.3
    ) +
    geom_label(
      data = PeakSame, aes(x = as.Date(RefDate), y = 325, label = TimeInterval, group = age_group),
      color = "black", fill = "white", size = 4, vjust = -0.3
    ) +
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


#' @description: Simulation of the model with vaccination protection
Model.RunSim.Immu <- function(Parm, lag = FALSE, model = "SIRVV") {
  # times <- seq(from = 1, to = 365 * Parm[["years"]])
  times <- as.numeric(seq(from = as.Date(Parm[["year_start"]]), to = as.Date(Parm[["year_end"]]), by = 1))

  state <- Get.InitState(
    num_age = Parm[["num_age"]],
    M_num = Parm[["M_num"]],
    population = Parm[["population"]],
    inf_num = Parm[["inf_num"]],
    model = model
  )

  SimResult <- ode(
    y = state, times = times, func = ModelSimCpp_Immu, parms = Parm, method = "lsoda"
  )
  SimResult <- Model.GetI.Immu(SimResult,
    Hosp_rate = Parm[["Hosp_rate"]], Effacy_I = Parm[["Effacy_I"]],
    Effacy_Hosp = Parm[["Effacy_Hosp"]], Vac_start = Parm[["Vac_start"]][1],
    VacAgeGroup = Parm[["VacAgeGroup"]], lag = lag
  )

  return(SimResult)
}

Model.GetI.Immu <- function(dat, Hosp_rate, Effacy_I, Effacy_Hosp,
                            Vac_start, VacAgeGroup, lag = FALSE) {
  NewDat <- dat %>% as.data.table()
  NewDat <- NewDat[, time := as.Date(time)]
  ReportName <- grep("Reported_G", names(NewDat), value = TRUE) # grep("Reported", names(NewDat), value = TRUE)
  ReportName_IV <- grep("Reported_IV", names(NewDat), value = TRUE)

  Simulation <- copy(NewDat)
  Simulation <- Simulation[, !..ReportName] # 这里的两个点指的是从环境中找相应的内容
  Simulation <- Simulation[, !..ReportName_IV]


  RealI <- copy(NewDat)
  RealI <- RealI[, ..ReportName]
  RealI <- cbind(NewDat[, 1], RealI)
  ExcludeCol <- setdiff(names(RealI), c("time", "week"))

  RealIV <- copy(NewDat)
  RealIV <- RealIV[, ..ReportName_IV]
  RealIV <- cbind(NewDat[, 1], RealIV)
  ExcludeCol_IV <- setdiff(names(RealIV), c("time", "week"))

  # 使用真实时间模拟
  if (lag) {
    RealI <- RealI[, week := substr(ISOweek::date2ISOweek(time), 1, 8)][
      ,
      ":="(Reported_G1 = shift(Reported_G1, 1, type = "lag", fill = 0),
        Reported_G2 = shift(Reported_G2, 1, type = "lag", fill = 0),
        Reported_G3 = shift(Reported_G3, 1, type = "lag", fill = 0),
        Reported_G4 = shift(Reported_G4, 2, type = "lag", fill = 0),
        Reported_G5 = shift(Reported_G5, 2, type = "lag", fill = 0),
        Reported_G6 = shift(Reported_G6, 2, type = "lag", fill = 0),
        Reported_G7 = shift(Reported_G7, 7, type = "lag", fill = 0),
        Reported_G8 = shift(Reported_G8, 7, type = "lag", fill = 0),
        Reported_G9 = shift(Reported_G9, 7, type = "lag", fill = 0),
        Reported_G10 = shift(Reported_G10, 7, type = "lag", fill = 0),
        Reported_G11 = shift(Reported_G11, 7, type = "lag", fill = 0))
    ][time >= as.Date("2017-06-27") & time <= as.Date("2020-06-27")][, (ExcludeCol) := lapply(.SD, sum),
      by = .(week), .SDcols = ExcludeCol
    ]
    RealIV <- RealIV[, week := substr(ISOweek::date2ISOweek(time), 1, 8)][
      ,
      ":="(Reported_IV1 = shift(Reported_IV1, 1, type = "lag", fill = 0),
        Reported_IV2 = shift(Reported_IV2, 1, type = "lag", fill = 0),
        Reported_IV3 = shift(Reported_IV3, 1, type = "lag", fill = 0),
        Reported_IV4 = shift(Reported_IV4, 2, type = "lag", fill = 0),
        Reported_IV5 = shift(Reported_IV5, 2, type = "lag", fill = 0),
        Reported_IV6 = shift(Reported_IV6, 2, type = "lag", fill = 0),
        Reported_IV7 = shift(Reported_IV7, 7, type = "lag", fill = 0),
        Reported_IV8 = shift(Reported_IV8, 7, type = "lag", fill = 0),
        Reported_IV9 = shift(Reported_IV9, 7, type = "lag", fill = 0),
        Reported_IV10 = shift(Reported_IV10, 7, type = "lag", fill = 0),
        Reported_IV11 = shift(Reported_IV11, 7, type = "lag", fill = 0))
    ][time >= as.Date("2017-06-27") & time <= as.Date("2020-06-27")][, (ExcludeCol_IV) := lapply(.SD, sum),
      by = .(week), .SDcols = ExcludeCol_IV
    ]
  } else {
    RealI <- RealI[, week := substr(ISOweek::date2ISOweek(time), 1, 8)][time >= as.Date("2017-06-27") &
      time <= as.Date("2020-06-27")][, (ExcludeCol) := lapply(.SD, sum),
      by = .(week), .SDcols = ExcludeCol
    ]
    RealIV <- RealIV[, week := substr(ISOweek::date2ISOweek(time), 1, 8)][time >= as.Date("2017-06-27") &
      time <= as.Date("2020-06-27")][, (ExcludeCol_IV) := lapply(.SD, sum),
      by = .(week), .SDcols = ExcludeCol_IV
    ]
  }

  # Extract the infection cases (No hospitalization)
  Real_Infection <- copy(RealI)

  # Calculate cases for each group
  Effacy_H2I <- (1 - Effacy_Hosp) / (1 - Effacy_I)
  Effacy_H2I <- Effacy_H2I * VacAgeGroup
  Effacy_H2I[Effacy_H2I == 0] <- 1

  BeforeVac <- RealI[time < as.Date(Vac_start)]
  BeforeVac[, (2:(ncol(BeforeVac) - 1)) := lapply(2:(ncol(BeforeVac) - 1), function(i) BeforeVac[[i]] * Hosp_rate[i - 1])]
  AfterVac <- RealI[time >= as.Date(Vac_start)]
  AfterVac[, (2:(ncol(AfterVac) - 1)) := lapply(2:(ncol(AfterVac) - 1), function(i) AfterVac[[i]] * Hosp_rate[i - 1] * Effacy_H2I[i - 1])]
  RealI <- rbind(BeforeVac, AfterVac)

  # RealI <- RealI[, (2:(ncol(RealI) - 1)) := lapply(2:(ncol(RealI) - 1), function(i) RealI[[i]] * Hosp_rate[i - 1])]

  NameOrder <- c("time", "week", ExcludeCol)
  RealI <- unique(RealI[, ..NameOrder], by = "week")
  Real_Infection <- unique(Real_Infection[, ..NameOrder], by = "week")
  NameOrder_IV <- c("time", "week", ExcludeCol_IV)
  RealIV <- unique(RealIV[, ..NameOrder_IV], by = "week")

  return(list(
    Simulation = Simulation,
    RealI = RealI,
    RealIV = RealIV,
    Real_Infection = Real_Infection
  ))
}


#' @description Calculate the infection rate per 1000 people
Calu.InfeRate <- function(Dat) {
  CalI_case <- copy(Dat[["Real_Infection"]])[time >= "2018-06-29" & time < "2019-06-29", ] # filter one year of infection data

  Infe_Summary <- CalI_case[, (3:13) := lapply(.SD, sum),
    .SDcols = c(3:13)
  ][, (3:13) := lapply(.SD, round, 0),
    .SDcols = 3:13
  ]
  Infe_Summary <- Infe_Summary[1, 3:13]

  return((as.numeric(Infe_Summary) / Scot_Pop) * 1000)
}

#' @description Calculate the proportion of hospitalization to infection
Calu.PropH2I <- function(ModelParm, lag, Age_Sus, Vac_start, Effacy_I = 0, Effacy_Hosp = 0, VacProp, model = "SIRVV") {
  InfeCase <- Model.RunSim.Immu(Parm = Parameter.Create(
    beta_base = ModelParm[1],
    beta_seasonal = ModelParm[2],
    phi = ModelParm[3],
    seasonal_wavelength = ModelParm[4],
    Hosp_rate = rep(1, 11),
    Age_Sus = Age_Sus,
    Vac_start = Vac_start,
    Effacy_I = Effacy_I,
    Effacy_Hosp = Effacy_Hosp,
    VacProp = VacProp
  ), lag = lag, model = model)

  InfeRate <- Calu.InfeRate(InfeCase)

  Prop_H2I <- Scot_HospRate / InfeRate
  return(as.numeric(Prop_H2I))
}

#' @title Calculate the vaccination protection
#' @description Calculate the vaccination protection
#' @return NetProtect = 年龄别净保护人数
#' @return VacProtection = 年龄别疫苗保护率
#' @return DirectProtect = 总疫苗直接保护率,
#' @return IndirectProtect = 总疫苗间接保护率,
#' @return TotalProtect = 总疫苗保护率
Vac.Protection <- function(
    ModelParm, VacAgeGroup, lag, Age_Sus, Vac_start, Effacy_I, Effacy_Hosp, VacProp, model = "SIRVV",
    Plot = FALSE, save = FALSE, path, width, height) {
  Prop_H2I <- Calu.PropH2I(ModelParm, lag, Age_Sus, Vac_start, Effacy_I = 0, Effacy_Hosp = 0, VacProp, model)

  ImmuSim <- Vac.Protection.Simulation(
    ModelParm = ModelParm, VacAgeGroup = VacAgeGroup, Prop_H2I = Prop_H2I, Effacy_I = Effacy_I, Effacy_Hosp = Effacy_Hosp,
    lag = lag, Age_Sus = Age_Sus, Vac_start = Vac_start, VacProp = VacProp, model = model
  )

  ImmuHosp_summ <- ImmuSim[["HospResult"]]
  ImmuHosp_summ_2age <- ImmuSim[["HospResult_2age"]]
  ImmuInfe_summ <- ImmuSim[["InfectionResult"]]
  ImmuInfe_summ_2age <- ImmuSim[["InfectionResult_2age"]]

  BaseSim <- Vac.Protection.Simulation(
    ModelParm = ModelParm, VacAgeGroup = VacAgeGroup, Prop_H2I = Prop_H2I, Effacy_I = 0, Effacy_Hosp = 0,
    lag = lag, Age_Sus = Age_Sus, Vac_start = Vac_start, VacProp = VacProp, model = model
  )

  BaseHosp_summ <- BaseSim[["HospResult"]]
  BaseHosp_summ_2age <- BaseSim[["HospResult_2age"]]
  BaseInfe_summ <- BaseSim[["InfectionResult"]]
  BaseInfe_summ_2age <- BaseSim[["InfectionResult_2age"]]

  ### Calculate the net protection
  # [Hospitalization]
  NetProtectHosp <- copy(BaseHosp_summ)[, cases := cases - ImmuHosp_summ$cases]
  NetProtectHosp_2age <- copy(BaseHosp_summ_2age)[, cases := cases - ImmuHosp_summ_2age$cases]
  # [Infection]
  NetProtectInfe <- copy(BaseInfe_summ)[, cases := cases - ImmuInfe_summ$cases]
  NetProtectInfe_2age <- copy(BaseInfe_summ_2age)[, cases := cases - ImmuInfe_summ_2age$cases]

  ### Calculate the vaccination protection
  # [Hospitalization]
  VacProtectionHosp <- copy(BaseHosp_summ)[, protect := round(((cases - ImmuHosp_summ$cases) / cases) * 100, 2)][, cases := NULL]
  VacProtectionHosp_2age <- copy(BaseHosp_summ_2age)[, protect := round(((cases - ImmuHosp_summ_2age$cases) / cases) * 100, 2)][, cases := NULL]
  # [Infection]
  VacProtectionInfe <- copy(BaseInfe_summ)[, protect := round(((cases - ImmuInfe_summ$cases) / cases) * 100, 2)][, cases := NULL]
  VacProtectionInfe_2age <- copy(BaseInfe_summ_2age)[, protect := round(((cases - ImmuInfe_summ_2age$cases) / cases) * 100, 2)][, cases := NULL]


  ### Calculate the direct and indirect protection
  # [Hospitalization]
  DirectProtectHosp <- VacProtectionHosp_2age[1, 2]
  InDirectProtectHosp <- VacProtectionHosp_2age[2, 2]
  # [Infection]
  DirectProtectInfe <- VacProtectionInfe_2age[1, 2]
  InDirectProtectInfe <- VacProtectionInfe_2age[2, 2]

  ### Calculate the total protection
  # [Hospitalization]
  BaseCaseHosp <- copy(BaseHosp_summ_2age)[, cases := sum(cases)][1, 2]
  ImmuCaseHosp <- copy(ImmuHosp_summ_2age)[, cases := sum(cases)][1, 2]
  TotalProtectHosp <- round(((BaseCaseHosp - ImmuCaseHosp) / BaseCaseHosp) * 100, 2)
  # [Infection]
  BaseCaseInfe <- copy(BaseInfe_summ_2age)[, cases := sum(cases)][1, 2]
  ImmuCaseInfe <- copy(ImmuInfe_summ_2age)[, cases := sum(cases)][1, 2]
  TotalProtectInfe <- round(((BaseCaseInfe - ImmuCaseInfe) / BaseCaseInfe) * 100, 2)

  # Calculate the averted hospitalization per vaccination
  if (VacAgeGroup == "S1") {
    FindV <- c(
      "V0_G1", "SV0_G1", "V1_G1", "SV1_G1", "V2_G1", "SV2_G1"
    )
  } else if (VacAgeGroup == "S2") {
    FindV <- c(
      "V0_G1", "SV0_G1", "V1_G1", "SV1_G1", "V2_G1", "SV2_G1",
      "V0_G2", "SV0_G2", "V1_G2", "SV1_G2", "V2_G2", "SV2_G2"
    )
  } else if (VacAgeGroup == "S3") {
    FindV <- c(
      "V0_G1", "SV0_G1", "V1_G1", "SV1_G1", "V2_G1", "SV2_G1",
      "V0_G2", "SV0_G2", "V1_G2", "SV1_G2", "V2_G2", "SV2_G2",
      "V0_G3", "SV0_G3", "V1_G3", "SV1_G3", "V2_G3", "SV2_G3"
    )
  } else if (VacAgeGroup == "S4") {
    FindV <- c(
      "V0_G1", "SV0_G1", "V1_G1", "SV1_G1", "V2_G1", "SV2_G1",
      "V0_G2", "SV0_G2", "V1_G2", "SV1_G2", "V2_G2", "SV2_G2",
      "V0_G3", "SV0_G3", "V1_G3", "SV1_G3", "V2_G3", "SV2_G3",
      "V0_G4", "SV0_G4", "V1_G4", "SV1_G4", "V2_G4", "SV2_G4"
    )
  }
  RawSimuData <- copy(ImmuSim[[4]])
  TotalVacc <- sum(RawSimuData[time == as.Date(Vac_start) + 1, ..FindV])

  NetProtectHospAll <- copy(ImmuSim[["AvertHospInfe"]])[, (3:13) := lapply(.SD, sum), .SDcols = c(3:13)] 
  NetProtectHospAll <- sum(NetProtectHospAll[1, 3:13]) + sum(NetProtectHosp_2age$cases) # Total averted hospitalization (None vacc + Cases in IV)
  AvertHospPerVac <- NetProtectHospAll / TotalVacc

  if (Plot == TRUE) {
    Fig <- Vac.Plot(
      copy(ImmuSim[[3]]), copy(BaseSim[[3]]), lag, Age_Sus, Vac_start, Effacy_I,
      VacProp, save, path, width, height
    )
    return(list(
      Prop_H2I = Prop_H2I,
      NetProtectHosp = NetProtectHosp,
      NetProtectHosp_2age = NetProtectHosp_2age,
      VacProtectionHosp = VacProtectionHosp,
      DirectProtectHosp = DirectProtectHosp,
      InDirectProtectHosp = InDirectProtectHosp,
      TotalProtectHosp = TotalProtectHosp,
      AvertHospPerVac = AvertHospPerVac,
      NetProtectInfe = NetProtectInfe,
      NetProtectInfe_2age = NetProtectInfe_2age,
      VacProtectionInfe = VacProtectionInfe,
      DirectProtectInfe = DirectProtectInfe,
      InDirectProtectInfe = InDirectProtectInfe,
      TotalProtectInfe = TotalProtectInfe,
      Fig = Fig
    ))
  } else {
    return(list(
      Prop_H2I = Prop_H2I,
      NetProtectHosp = NetProtectHosp,
      NetProtectHosp_2age = NetProtectHosp_2age,
      VacProtectionHosp = VacProtectionHosp,
      DirectProtectHosp = DirectProtectHosp,
      InDirectProtectHosp = InDirectProtectHosp,
      TotalProtectHosp = TotalProtectHosp,
      AvertHospPerVac = AvertHospPerVac,
      NetProtectInfe = NetProtectInfe,
      NetProtectInfe_2age = NetProtectInfe_2age,
      VacProtectionInfe = VacProtectionInfe,
      DirectProtectInfe = DirectProtectInfe,
      InDirectProtectInfe = InDirectProtectInfe,
      TotalProtectInfe = TotalProtectInfe
    ))
  }
}

#' @title Simulation of the model with vaccination protection
#' @description Simulation of the model with vaccination protection (Effacy_I > 0) or without vaccination (Effacy_I = 0)
#' This function is used in Vac.Protection twice to simulate the model with and without vaccination respectively
#' @return SummCase: calculate the summary of new confirmed cases for 11 age groups by week between 1-10 and 40-53
#' @return SummCase_2age: calculate the summary of new confirmed cases for 2 age groups by week between 1-10 and 40-53
#' @return SimResult[[2]]: the raw simulation result for the plot
Vac.Protection.Simulation <- function(ModelParm, Prop_H2I, VacAgeGroup, lag, Age_Sus,
                                      Vac_start, Effacy_I, Effacy_Hosp, VacProp, model = "SIRVV") {
  SimResult <- Model.RunSim.Immu(Parm = Parameter.Create(
    beta_base = ModelParm[1],
    beta_seasonal = ModelParm[2],
    phi = ModelParm[3],
    seasonal_wavelength = ModelParm[4],
    Hosp_rate = Prop_H2I,
    Age_Sus = Age_Sus,
    Vac_start = Vac_start,
    Effacy_I = Effacy_I,
    Effacy_Hosp = Effacy_Hosp,
    VacProp = VacProp,
    VacAgeGroup = VacAgeGroup
  ), lag = lag, model = model)

  ### Determine the age group for the 2 age groups
  ColumnSelect <- switch(VacAgeGroup,
    "S1" = c("0-2m"),
    "S2" = c("0-2m", "3-5m", "6-11m"),
    "S3" = c("0-2m", "3-5m", "6-11m", "12-23m"),
    "S4" = c("0-2m", "3-5m", "6-11m", "12-23m", "2-4y")
  )

  HospResult <- Vac.Protection.Simulation.Calculate(SimResult[["RealI"]], ColumnSelect)
  InfectionResult <- Vac.Protection.Simulation.Calculate(SimResult[["Real_Infection"]], ColumnSelect)

  NewConfirmIV <- copy(SimResult[["RealIV"]])

  return(list(
    HospResult = HospResult[["SummCase"]],
    HospResult_2age = HospResult[["SummCase_2age"]],
    SimResult[[2]],
    SimResult[[1]],
    InfectionResult = InfectionResult[["SummCase"]],
    InfectionResult_2age = InfectionResult[["SummCase_2age"]],
    AvertHospInfe = NewConfirmIV
  ))
}


Vac.Protection.Simulation.Calculate <- function(Dat, ColumnSelect) {
  NewConfirm <- copy(Dat)

  NewConfirm_ByWeek <- NewConfirm[, week_num := substr(week, 7, 8)][, (3:13) := lapply(.SD, sum),
    .SDcols = c(3:13), by = .(week_num)
  ]

  NewConfirm_ByWeek <- unique(NewConfirm_ByWeek, by = "week_num")
  NewConfirm_ByWeek <- NewConfirm_ByWeek[, 3:14] # time >= "2019-06-29"

  NewConfirm_ByWeek_long <- melt(NewConfirm_ByWeek, id.vars = "week_num", variable.name = "age_group", value.name = "cases") %>%
    mutate(age_group = factor(age_group,
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
    ))

  SummCase <- NewConfirm_ByWeek_long[, .(cases = sum(cases)), by = .(age_group)]
  SummCase <- SummCase[, cases := round(cases, 0)]

  SummCase_2age <- copy(SummCase)[, age_group := fcase(
    age_group %in% ..ColumnSelect, "Direct",
    default = "Indirect"
  )][, .(cases = sum(cases)), by = .(age_group)]
  SummCase_2age <- SummCase_2age[, cases := round(cases, 0)]

  return(
    list(
      SummCase = SummCase,
      SummCase_2age = SummCase_2age
    )
  )
}


#' @title Calculate the vaccination protection posterior
Vac.Posterior <- function(VacList) {
  ### Prop_H2I CI
  Prop_H2I_CI <- do.call(rbind, lapply(VacList, \(x) x[["Prop_H2I"]]))
  Prop_H2I_CI <- Prop_H2I_CI %>%
    apply(., 2, \(x){
      quantile(x, c(0.025, 0.5, 0.975))
    }) %>%
    t()

  ### NetProtect CI
  # [Hospitalization]
  NetProtectHosp_CI <- rbindlist(lapply(VacList, \(x) x[["NetProtectHosp"]]))
  NetProtectHosp_CI <- NetProtectHosp_CI[, .(
    median = round(median(cases), 0),
    lci = round(quantile(cases, probs = 0.025, type = 1), 0),
    uci = round(quantile(cases, probs = 0.975, type = 1), 0)
  ), by = age_group][, CI := paste0(median, " (", lci, ", ", uci, ")")]

  # [Infection]
  NetProtectInfe_CI <- rbindlist(lapply(VacList, \(x) x[["NetProtectInfe"]]))
  NetProtectInfe_CI <- NetProtectInfe_CI[, .(
    median = round(median(cases), 0),
    lci = round(quantile(cases, probs = 0.025, type = 1), 0),
    uci = round(quantile(cases, probs = 0.975, type = 1), 0)
  ), by = age_group][, CI := paste0(median, " (", lci, ", ", uci, ")")]

  ### NetProtectHosp_2age CI
  # [Hospitalization]
  NetProtectHosp_2age_CI <- rbindlist(lapply(VacList, \(x) x[["NetProtectHosp_2age"]]))
  NetProtectHosp_2age_CI <- NetProtectHosp_2age_CI[, .(
    median = round(median(cases), 0),
    lci = round(quantile(cases, probs = 0.025, type = 1), 0),
    uci = round(quantile(cases, probs = 0.975, type = 1), 0)
  ), by = age_group][, CI := paste0(median, " (", lci, ", ", uci, ")")]

  # [Infection]
  NetProtectInfe_2age_CI <- rbindlist(lapply(VacList, \(x) x[["NetProtectInfe_2age"]]))
  NetProtectInfe_2age_CI <- NetProtectInfe_2age_CI[, .(
    median = round(median(cases), 0),
    lci = round(quantile(cases, probs = 0.025, type = 1), 0),
    uci = round(quantile(cases, probs = 0.975, type = 1), 0)
  ), by = age_group][, CI := paste0(median, " (", lci, ", ", uci, ")")]


  ### VacProtection CI
  # [Hospitalization]
  VacProtectionHosp_CI <- rbindlist(lapply(VacList, \(x) x[["VacProtectionHosp"]]))
  VacProtectionHosp_CI <- VacProtectionHosp_CI[, .(
    median = median(protect),
    lci = round(quantile(protect, probs = 0.025), 2),
    uci = round(quantile(protect, probs = 0.975), 2)
  ), by = age_group][, CI := paste0(median, " (", lci, ", ", uci, ")")]

  # [Infection]
  VacProtectionInfe_CI <- rbindlist(lapply(VacList, \(x) x[["VacProtectionInfe"]]))
  VacProtectionInfe_CI <- VacProtectionInfe_CI[, .(
    median = median(protect),
    lci = round(quantile(protect, probs = 0.025), 2),
    uci = round(quantile(protect, probs = 0.975), 2)
  ), by = age_group][, CI := paste0(median, " (", lci, ", ", uci, ")")]


  ### DirectProtect, IndeirectProtect, TotalProtect CI
  # [Hospitalization]
  DirectProtectHosp <- do.call(rbind, lapply(VacList, \(x) x[["DirectProtectHosp"]]))
  InDirectProtectHosp <- do.call(rbind, lapply(VacList, \(x) x[["InDirectProtectHosp"]]))
  TotalProtectHosp <- do.call(rbind, lapply(VacList, \(x) x[["TotalProtectHosp"]]))
  DirectProtectHosp_CI <- round(quantile(unlist(DirectProtectHosp), probs = c(0.025, 0.5, 0.975)), 2)
  InDirectProtectHosp_CI <- round(quantile(unlist(InDirectProtectHosp), probs = c(0.025, 0.5, 0.975)), 2)
  TotalProtectHosp_CI <- round(quantile(unlist(TotalProtectHosp), probs = c(0.025, 0.5, 0.975)), 2)
  resultHosp <- data.frame(
    type = c("DirectProtectHosp", "InDirectProtectHosp", "TotalProtectHosp"),
    median = c(DirectProtectHosp_CI[2], InDirectProtectHosp_CI[2], TotalProtectHosp_CI[2]),
    lci = c(DirectProtectHosp_CI[1], InDirectProtectHosp_CI[1], TotalProtectHosp_CI[1]),
    uci = c(DirectProtectHosp_CI[3], InDirectProtectHosp_CI[3], TotalProtectHosp_CI[3])
  )
  resultHosp$CI <- paste0(resultHosp$median, " (", resultHosp$lci, " - ", resultHosp$uci, ")")

  # [Infection]
  DirectProtectInfe <- do.call(rbind, lapply(VacList, \(x) x[["DirectProtectInfe"]]))
  InDirectProtectInfe <- do.call(rbind, lapply(VacList, \(x) x[["InDirectProtectInfe"]]))
  TotalProtectInfe <- do.call(rbind, lapply(VacList, \(x) x[["TotalProtectInfe"]]))
  DirectProtectInfe_CI <- round(quantile(unlist(DirectProtectInfe), probs = c(0.025, 0.5, 0.975), type = 1), 2)
  InDirectProtectInfe_CI <- round(quantile(unlist(InDirectProtectInfe), probs = c(0.025, 0.5, 0.975), type = 1), 2)
  TotalProtectInfe_CI <- round(quantile(unlist(TotalProtectInfe), probs = c(0.025, 0.5, 0.975), type = 1), 2)
  resultInfe <- data.frame(
    type = c("DirectProtectInfe", "InDirectProtectInfe", "TotalProtectInfe"),
    median = c(DirectProtectInfe_CI[2], InDirectProtectInfe_CI[2], TotalProtectInfe_CI[2]),
    lci = c(DirectProtectInfe_CI[1], InDirectProtectInfe_CI[1], TotalProtectInfe_CI[1]),
    uci = c(DirectProtectInfe_CI[3], InDirectProtectInfe_CI[3], TotalProtectInfe_CI[3])
  )
  resultInfe$CI <- paste0(resultInfe$median, " (", resultInfe$lci, " - ", resultInfe$uci, ")")

  AvertHospPerVac_CI <- do.call(rbind, lapply(VacList, \(x) x[["AvertHospPerVac"]]))
  AvertHospPerVac_CI <- round(quantile(unlist(AvertHospPerVac_CI), probs = c(0.025, 0.5, 0.975), type = 1), 5)

  return(list(
    NetProtectHosp = NetProtectHosp_CI,
    NetProtectHosp_2age = NetProtectHosp_2age_CI,
    VacProtectionHosp_age = VacProtectionHosp_CI,
    VacProtectionHosp = resultHosp,
    Prop_H2I_CI = Prop_H2I_CI,
    AvertHospPerVac_CI = AvertHospPerVac_CI,
    NetProtectInfe = NetProtectInfe_CI,
    NetProtectInfe_2age = NetProtectInfe_2age_CI,
    VacProtectionInfe_age = VacProtectionInfe_CI,
    VacProtectionInfe = resultInfe
  ))
}


#' @description Plot the simulation result with or without vaccination used in the Vac.Protection
#' @param  ImmuData: the simulation result with vaccination (produced by Model.RunSim.Immu)
#' @param BaseData: the simulation result without vaccination (produced by Model.RunSim.Immu with Effacy_I = 0)
Vac.Plot <- function(
    ImmuData, BaseData, lag, Age_Sus, Vac_start, Effacy_I,
    VacProp, save = FALSE, path, width, height) {
  ### Simulation of the vaccinated cases
  # ImmuData <- Model.RunSim.Immu(Parm = Parameter.Create(
  #   beta_base = ModelParm[1],
  #   beta_seasonal = ModelParm[2],
  #   phi = ModelParm[3],
  #   seasonal_wavelength = ModelParm[4],
  #   Hosp_rate = ModelParm[5:15],
  #   Age_Sus = Age_Sus,
  #   Vac_start = Vac_start,
  #   Effacy_I = Effacy_I,
  #   VacProp = VacProp
  # ), lag = lag)[[2]]

  ### Simulation of the base cases
  # BaseData <- Model.RunSim.Immu(Parm = Parameter.Create(
  #   beta_base = ModelParm[1],
  #   beta_seasonal = ModelParm[2],
  #   phi = ModelParm[3],
  #   seasonal_wavelength = ModelParm[4],
  #   Hosp_rate = ModelParm[5:15],
  #   Age_Sus = Age_Sus,
  #   Vac_start = Vac_start,
  #   Effacy_I = 0,
  #   VacProp = VacProp
  # ), lag = lag)[[2]]

  # Simualtion result of reported infected cases
  ImmuInfe <- ImmuData %>% as.data.table()
  ImmuInfe <- melt.data.table(ImmuInfe, id.vars = c("time", "week"), variable.name = "age_group", value.name = "Cases")

  BaseInfe <- BaseData %>% as.data.table()
  BaseInfe <- melt.data.table(BaseInfe, id.vars = c("time", "week"), variable.name = "age_group", value.name = "Cases")

  MergeDat <- merge(ImmuInfe, BaseInfe, by = c("time", "week", "age_group"))
  setnames(MergeDat, c("time", "week", "age_group", "ImmuInfe", "BaseInfe"))

  MergeDat <- MergeDat %>%
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
    )

  Fig <- MergeDat %>%
    ggplot(.) +
    # geom_line(aes(x = week, y = summ, group = age_group), alpha = 0.2) +
    geom_line(aes(x = week, y = ImmuInfe, group = age_group), colour = "blue", alpha = 0.7, linewidth = 1.2) + # linetype = 3,
    geom_line(aes(x = week, y = BaseInfe, group = age_group), colour = "red") +
    geom_vline(xintercept = as.Date(c("2018-01-01", "2019-01-01", "2020-01-01")), linetype = "dashed", alpha = 0.2) +
    labs(
      x = "Date",
      y = "Number of cases"
    ) +
    theme_minimal() +
    scale_x_date(date_labels = "%Y") +
    theme(
      axis.text.x = element_text(size = 18, hjust = 1),
      axis.text.y = element_text(size = 18),
      axis.title = element_text(size = 24),
      strip.text = element_text(size = 24)
    ) +
    facet_wrap(~age_group) # , scales = "free_y"

  if (save == TRUE) {
    ggsave(Fig, file = paste0(path, ".pdf"), width = width, height = height)
  }

  return(Fig)
}

#' @description A batch version of the vaccination simulation, return all results simultaneously
#'
Vac.Batch <- function(MCMC_Result,
                      Age_Sus, VacAgeGroup, Vac_start, Effacy_I, Effacy_Hosp,
                      VacProp, lag, model = "SIRVV",
                      Plot = TRUE, save = FALSE, path = NULL, width, height,
                      seed = 380) {
  # Extract data from MCMC
  Posteriori_Median <- MCMC_Result$Median
  Posteriori_Sample <- MCMC_Result$SampleChain %>% split(., row(.))


  # Run simulation
  Parallel.Regist(10, seed = seed)
  Parallel.Import(list(
    "CLI.Print", "Get.InitState", "Get.StateName", "Model.GetI", "Model.GetI.Immu",
    "Model.RunSim",
    "Parameter.Create", "RefDat", "Scot_Pop", "ContacrStr", "Scot_HospRate",
    "MCMC.PosteriorSample", "Model.RunSim.Immu", "Vac.Protection",
    "Vac.Protection.Simulation", "Calu.PropH2I", "Calu.InfeRate",
    "Vac.Protection.Simulation.Calculate"
  ))
  PosteriorResult <- parLapply(ParallelNodesInfo[[1]], Posteriori_Sample, \(sample) {
    setDTthreads(1)
    # sample <- sapply(1:15, \(n_col) MCMC.PosteriorSample(Posteriori_CI[n_col, ]))

    SimResult <- Vac.Protection(
      ModelParm = sample,
      VacAgeGroup = VacAgeGroup,
      lag = lag,
      Age_Sus = Age_Sus,
      Vac_start = Vac_start,
      Effacy_I = Effacy_I,
      Effacy_Hosp = Effacy_Hosp,
      VacProp = VacProp,
      model = model,
      Plot = FALSE
    )

    return(SimResult)
  })
  Parallel.Stop()

  # Get posterior result of vaccination protection
  result_CI <- Vac.Posterior(PosteriorResult)

  # Plot the result
  if (Plot == TRUE) {
    Fig <- Vac.Protection(
      ModelParm = Posteriori_Median,
      VacAgeGroup = VacAgeGroup,
      lag = lag,
      Age_Sus = Age_Sus,
      Vac_start = Vac_start,
      Effacy_I = Effacy_I,
      Effacy_Hosp = Effacy_Hosp,
      VacProp = VacProp,
      Plot = TRUE, model = model,
      save = save, path, width, height
    )
    return(list(
      Prop_H2I = result_CI[[5]],
      NetProtectHosp = result_CI[["NetProtectHosp"]],
      NetProtectHosp_2age = result_CI[["NetProtectHosp_2age"]],
      VacProtectionHosp_age = result_CI[["VacProtectionHosp_age"]],
      VacProtectionHosp = result_CI[["VacProtectionHosp"]],
      AvertHospPerVac = result_CI[["AvertHospPerVac_CI"]],
      NetProtectInfe = result_CI[["NetProtectInfe"]],
      NetProtectInfe_2age = result_CI[["NetProtectInfe_2age"]],
      VacProtectionInfe_age = result_CI[["VacProtectionInfe_age"]],
      VacProtectionInfe = result_CI[["VacProtectionInfe"]],
      Fig = Fig$Fig
    ))
  } else {
    return(list(
      Prop_H2I = result_CI[[5]],
      NetProtectHosp = result_CI[["NetProtectHosp"]],
      NetProtectHosp_2age = result_CI[["NetProtectHosp_2age"]],
      VacProtectionHosp_age = result_CI[["VacProtectionHosp_age"]],
      VacProtectionHosp = result_CI[["VacProtectionHosp"]],
      AvertHospPerVac = result_CI[["AvertHospPerVac_CI"]],
      NetProtectInfe = result_CI[["NetProtectInfe"]],
      NetProtectInfe_2age = result_CI[["NetProtectInfe_2age"]],
      VacProtectionInfe_age = result_CI[["VacProtectionInfe_age"]],
      VacProtectionInfe = result_CI[["VacProtectionInfe"]]
    ))
  }
}



















AAP <- function(Dat, threshold) {
  Cases <- Dat$summ
  CasesProportion <- Cases / sum(Cases)
  CasesCumProp <- cumsum(Cases) / sum(Cases)
  epidemics <- ifelse(CasesCumProp - threshold < 0, 1,
    with(
      Dat,
      ifelse(CasesCumProp - CasesProportion - threshold < 0,
        (threshold - (CasesCumProp - CasesProportion)) / CasesProportion,
        0
      )
    )
  )
  return(epidemics)
}
