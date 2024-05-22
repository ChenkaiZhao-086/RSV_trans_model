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


Parallel.Regist <- function(ncores = NULL) {
  ## SETUP PARALLEL NODES
  # note: they will be removed after 600 seconds inactivity
  if (is.null(ncores)) {
    ncores <- detectCores() - 2
  }
  par_cluster <- makeCluster(ncores, cores = ncores) # , timeout = 600
  registerDoParallel(par_cluster)

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
  clusterEvalQ(ParallelNodesInfo[[1]], {
    library("deSolve")
    sourceCpp("Code/model_cppV4.cpp")
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
Get.StateName <- function(num_age = num_age) {
  # num_age: number of age group
  state_nam <- c()
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
  return(state_nam)
}


#' @title function to create states
#' @param num_age: number of age group
#' @param M_num: the number of newborn at the beginning
#' @param population: healthy population by age group
#' @param inf_num: the number of infected cases in the beginning
Get.InitState <- function(num_age = 11, M_num = 6000, population = Scot_Pop, inf_num = 20) {
  state <- vector("numeric", 10 * num_age + 1)
  names(state) <- Get.StateName(num_age = num_age)
  state[1] <- M_num
  for (i in 1:num_age) {
    state[2 + (i - 1) * 10] <- as.numeric(population[i])
  }
  for (i in 1:num_age) { # assign the initial number of infected cases, 20
    state[3 + (i - 1) * 10] <- inf_num
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
    E_time = 1 / 3, # 3 days
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
    Age_Sus = c(
      1, # 0-2 months
      1, # 3-5 months
      1, # 6-11 months
      1, # 1-2 years
      1, # 2-4 years
      1, # 5-19 years
      1, # 20-59 years
      1, # 60-64 years
      1, # 65-69 years
      1, # 70-74 years
      1 # 75+ years
    )) {
  age_rates_1 <- 1 / ((1 / age_rates[1]) - omega_m)
  age_rates <- c(age_rates_1, age_rates[-1])
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
      Age_Sus = Age_Sus
    )

  return(Parmeters)
}


#' @title Calculate the number of infected cases
#' @param dat: data
#' @param Hosp_rate: hospitalization rate
Model.GetI <- function(dat, Hosp_rate = parameters[["Hosp_rate"]]) {
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
  RealI <- RealI[, week := substr(ISOweek::date2ISOweek(time), 1, 8)][time >= as.Date("2017-06-26") & time <= as.Date("2020-03-29"), ][, (ExcludeCol) := lapply(.SD, sum), by = .(week), .SDcols = ExcludeCol]
  # RealI <- RealI[, week := substr(ISOweek::date2ISOweek(time), 1, 8)
  # ][, ":="(Reported_G1 = shift(Reported_G1, 1, type = "lag", fill = 0),
  #   Reported_G2 = shift(Reported_G2, 1, type = "lag", fill = 0),
  #   Reported_G3 = shift(Reported_G3, 1, type = "lag", fill = 0),
  #   Reported_G4 = shift(Reported_G4, 2, type = "lag", fill = 0),
  #   Reported_G5 = shift(Reported_G5, 2, type = "lag", fill = 0),
  #   Reported_G6 = shift(Reported_G6, 2, type = "lag", fill = 0),
  #   Reported_G7 = shift(Reported_G7, 7, type = "lag", fill = 0),
  #   Reported_G8 = shift(Reported_G8, 7, type = "lag", fill = 0),
  #   Reported_G9 = shift(Reported_G9, 7, type = "lag", fill = 0),
  #   Reported_G10 = shift(Reported_G10, 7, type = "lag", fill = 0),
  #   Reported_G11 = shift(Reported_G11, 7, type = "lag", fill = 0))][time >= as.Date("2017-06-26") & time <= as.Date("2020-03-29"), ][, (ExcludeCol) := lapply(.SD, sum), by = .(week), .SDcols = ExcludeCol]
  # 截止时间修改为2018-03-29，默认情况（使用三年的数据）的截止时间是2020-03-29
  # "2017-06-26" "2020-03-29"
  # 使用固定的时间长度模拟
  # RealI <- RealI[, week := ceiling(seq_len(nrow(RealI)) / 7)
  #                ][, lapply(.SD, sum), by = .(week)
  #                  ]

  # Calculate cases for each group
  # 下面这一句是使用泊松分布的时候使用的，用来给每个年龄组乘以相应的hospitalization rate，使用二项分布和非二项分布的时候不需要
  RealI <- RealI[, (2:(ncol(RealI) - 1)) := lapply(2:(ncol(RealI) - 1), function(i) RealI[[i]] * Hosp_rate[i - 1])]

  NameOrder <- c("time", "week", ExcludeCol)
  RealI <- unique(RealI[, ..NameOrder], by = "week")


  return(list(Simulation, RealI))
}


#' @title Run simulation
#' @param Parm: parameters for simulatio
Model.RunSim <- function(Parm) {
  # times <- seq(from = 1, to = 365 * Parm[["years"]])
  times <- as.numeric(seq(from = as.Date(Parm[["year_start"]]), to = as.Date(Parm[["year_end"]]), by = 1))
  state <- Get.InitState(
    num_age = Parm[["num_age"]],
    M_num = Parm[["M_num"]],
    population = Parm[["population"]],
    inf_num = Parm[["inf_num"]]
  )

  SimResult <- ode(y = state, times = times, func = ModelSimCpp, parms = Parm, method = "rk4")
  SimResult <- Model.GetI(SimResult, Parm[["Hosp_rate"]])

  # return(list(
  #   Data = SimResult,
  #   fig1 = fig1,
  #   fig2 = fig2
  # ))
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
Model.RunSim.LLH <- function(Parm, TargetDat = RefDat) {
  Dat <- Model.RunSim(Parm)
  SimDat <- Dat[[2]]

  SimDat <- melt(SimDat, id = c("time", "week"))
  CombineTable <- merge(SimDat, TargetDat, by.x = c("week", "variable"), by.y = c("week", "age_group"), all.y = T)
  CombineTable <- na.omit(CombineTable)

  # Calaulate the likelihood based on Poisson distribution
  # CombineTable[variable == "Reported_G1", likelihood := dpois(x = true_value, lambda = round(value), log = T)]
  # CombineTable[variable == "Reported_G2", likelihood := dpois(x = true_value, lambda = round(value), log = T)]
  # CombineTable[variable == "Reported_G3", likelihood := dpois(x = true_value, lambda = round(value), log = T)]
  # CombineTable[variable == "Reported_G4", likelihood := dpois(x = true_value, lambda = round(value), log = T)]
  # CombineTable[variable == "Reported_G5", likelihood := dpois(x = true_value, lambda = round(value), log = T)]
  # CombineTable[variable == "Reported_G6", likelihood := dpois(x = true_value, lambda = round(value), log = T)]
  # CombineTable[variable == "Reported_G7", likelihood := dpois(x = true_value, lambda = round(value), log = T)]
  # CombineTable[variable == "Reported_G8", likelihood := dpois(x = true_value, lambda = round(value), log = T)]
  # CombineTable[variable == "Reported_G9", likelihood := dpois(x = true_value, lambda = round(value), log = T)]
  # CombineTable[variable == "Reported_G10", likelihood := dpois(x = true_value, lambda = round(value), log = T)]
  # CombineTable[variable == "Reported_G11", likelihood := dpois(x = true_value, lambda = round(value), log = T)]

  # Calculate the likelihood based on negative binomial distribution
  CombineTable[variable == "Reported_G1", likelihood := dnbinom(x = true_value, size = 1.45, mu = round(value), log = T)]
  CombineTable[variable == "Reported_G2", likelihood := dnbinom(x = true_value, size = 1.45, mu = round(value), log = T)]
  CombineTable[variable == "Reported_G3", likelihood := dnbinom(x = true_value, size = 1.45, mu = round(value), log = T)]
  CombineTable[variable == "Reported_G4", likelihood := dnbinom(x = true_value, size = 1.45, mu = round(value), log = T)]
  CombineTable[variable == "Reported_G5", likelihood := dnbinom(x = true_value, size = 1.45, mu = round(value), log = T)]
  CombineTable[variable == "Reported_G6", likelihood := dnbinom(x = true_value, size = 1.45, mu = round(value), log = T)]
  CombineTable[variable == "Reported_G7", likelihood := dnbinom(x = true_value, size = 1.45, mu = round(value), log = T)]
  CombineTable[variable == "Reported_G8", likelihood := dnbinom(x = true_value, size = 1.45, mu = round(value), log = T)]
  CombineTable[variable == "Reported_G9", likelihood := dnbinom(x = true_value, size = 1.45, mu = round(value), log = T)]
  CombineTable[variable == "Reported_G10", likelihood := dnbinom(x = true_value, size = 1.45, mu = round(value), log = T)]
  CombineTable[variable == "Reported_G11", likelihood := dnbinom(x = true_value, size = 1.45, mu = round(value), log = T)]

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
  # ParmNew_base <- rtruncnorm(1, a = 0, mean = Parm[1], sd = 1) # # beta_base
  ParmNew_base <- exp(log(Parm[1]) + runif(1, -0.02, 0.02)) # beta_base
  # ParmNew_seasonal <- rtruncnorm(1, a = 0, mean = Parm[2], sd = 1) # beta_seasonal
  ParmNew_seasonal <- exp(log(Parm[2]) + runif(1, -0.02, 0.02)) # beta_seasonal

  ParmNew2 <- rtruncnorm(1, a = 0.4, b = 0.6, mean = Parm[3], sd = 0.002) # Parm[3] + runif(1, -1.8, 1.8) # phi

  ParmNew3 <- rtruncnorm(1, a = 0.02, b = 0.15, mean = Parm[4], sd = 0.0025) # seasonal_wavelength
  # ParmNew3 <- exp(log(Parm[4]) + runif(1, -1, 1)) # seasonal_wavelength

  ParmNew4 <- rtruncnorm(1, a = 0, b = 0.05, mean = Parm[c(5:15)], sd = Parm[c(5:15)] / 5) # Hosp_rate
  # 0-2 months # 3-5 months # 6-11 months # 1-2 years # 2-4 years # 5-19 years
  # 20-59 years # 60-64 years # 65-69 years # 70-74 years # 75+ years

  ParmOut <- c(ParmNew_base, ParmNew_seasonal, ParmNew2, ParmNew3, ParmNew4)
  return(ParmOut)
}
# MCMC.Proposal <- function(Parm) {
#   ParmBata1 <- log(Parm[1])
#   ParmBata2 <- log((1 - Parm[2]) / Parm[2])
#   ParmHosp <- log((1 - Parm[c(4:14)]) / Parm[c(4:14)]) # 4, 6, 8, 10, 12
#   # 0-2 months # 3-5 months # 6-11 months # 1-2 years # 2-4 years # 5-19 years
#   # 20-59 years # 60-64 years # 65-69 years # 70-74 years # 75+ years
#
#   ParmNew1 <- ParmBata1 + runif(1, -0.2, 0.2) # beta_base
#   ParmNew1.1 <- ParmBata2 + runif(1, -0.2, 0.2) # beta_seasonal
#   # while (exp(ParmNew1) > 1 / (1 + exp(ParmNew1.1))) {
#   #   ParmNew1 <- ParmBata1 + runif(1, -0.2, 0.2)
#   #   ParmNew1.1 <- ParmBata2 + runif(1, -0.5, 0.5)
#   # }
#
#   ParmPhi <- asin((Parm[3] - 1.75) / 0.75) / 0.25 # 最后的0.25用来调整步长
#   ParmPhiUpdate <- ParmPhi + runif(1, -0, 0) # step2
#   ParmNew2 <- 0.75 * sin(0.25 * ParmPhiUpdate) + 1.75 # phi
#
#   ParmNew3 <- ParmHosp + runif(11, -0.2, 0.2) # Hosp_rate
#
#   ParmOut <- c(exp(ParmNew1), 1 / (1 + exp(ParmNew1.1)), ParmNew2, 1 / (1 + exp(ParmNew3)))
#   return(ParmOut)
# }
# MCMC.Proposal <- function(Parm) { # , covmat, lower_bounds, upper_bounds
#   ParmHosp <- log((1 - Parm[c(4:14)]) / Parm[c(4:14)]) # 4, 6, 8, 10, 12
#   # log((1 - Parm) / Parm) / -0.1
#   ParmOld <- c(Parm[1], Parm[2], ParmHosp)
#   # 0-2 months # 3-5 months # 6-11 months # 1-2 years # 2-4 years # 5-19 years
#   # 20-59 years # 60-64 years # 65-69 years # 70-74 years # 75+ years

#   ParmNew1 <- Parm[1:2] + runif(2, -0.05, 0.05)
#   ParmNew2 <- Parm[3] + runif(1, -1, 1)
#   ParmNew3 <- ParmHosp + runif(11, -0.05, 0.05)
#   ParmOut <- c(ParmNew1, ParmNew2, 1 / (1 + exp(ParmNew3)))
#   # ParmNew <-  c(rtmvnorm(1,
#   #   mean = ParmOld,
#   #   sigma = covmat,
#   #   lower = lower_bounds,
#   #   upper = upper_bounds,
#   #   algorithm = "gibbs",
#   #   thinning = 2
#   # ))

#   # names(ParmNew) <- c(
#   #   "beta_base", "beta_seasonal", "phi", "Hosp_rateM0_5", "Hosp_rateM6_12",
#   #   "Hosp_rateY3_18", "Hosp_rateY19_65", "Hosp_rateY66_"
#   # )
#   # Hosp_rate <- vector("numeric", length = 11)
#   # Hosp_rate[1:2] <- 1 / (1 + exp(ParmNew["Hosp_rateM0_5"]))
#   # Hosp_rate[3:4] <- 1 / (1 + exp(ParmNew["Hosp_rateM6_12"]))
#   # Hosp_rate[5:6] <- 1 / (1 + exp(ParmNew["Hosp_rateY3_18"]))
#   # Hosp_rate[7:8] <- 1 / (1 + exp(ParmNew["Hosp_rateY19_65"]))
#   # Hosp_rate[9:11] <- 1 / (1 + exp(ParmNew["Hosp_rateY66_"]))

#   # ParmOut <- c(ParmNew[1:3], Hosp_rate)
#   # Parm[["beta_base"]] <- ParmNew[["beta_base"]]
#   # Parm[["beta_seasonal"]] <- ParmNew[["beta_seasonal"]]
#   # Parm[["phi"]] <- ParmNew[["phi"]]
#   # Parm[["Hosp_rate"]][c(1, 2)] <- ParmNew["Hosp_rateM0_5"]
#   # Parm[["Hosp_rate"]][c(3, 4)] <- ParmNew["Hosp_rateM6_12"]
#   # Parm[["Hosp_rate"]][c(5, 6)] <- ParmNew["Hosp_rateY3_18"]
#   # Parm[["Hosp_rate"]][c(7, 8)] <- ParmNew["Hosp_rateY19_65"]
#   # Parm[["Hosp_rate"]][c(9, 10, 11)] <- ParmNew["Hosp_rateY66_"]

#   # for(param_name in names(parameters_new)){
#   #   parameters[param_name] <- parameters_new[param_name]}

#   return(ParmOut)
# }


#' @title Run MCMC model
#' @param Prior: prior parameters
MCMC.MH <- function(Prior, n_iterations, TargetDat = RefDat) {
  chain <- matrix(NA, nrow = n_iterations, ncol = 15)
  chain[1, ] <- Prior
  accepted <- 0

  current_log_likelihood <- Model.RunSim.LLH(
    Parm = Parameter.Create(
      beta_base = chain[1, 1], beta_seasonal = chain[1, 2], phi = chain[1, 3],
      seasonal_wavelength = chain[1, 4],
      Hosp_rate = c(chain[1, 5:15])
    ), TargetDat = TargetDat
  )

  pb <- progress_bar$new(total = n_iterations, clear = TRUE, format = "  [:bar] :percent :etas")
  pb$tick()
  for (i in 2:n_iterations) {
    # proposal <- MCMC.Proposal(
    #   Parm = chain[i - 1, ], covmat = covmat, lower_bounds = lower_bounds,
    #   upper_bounds = upper_bounds
    # )
    proposal <- MCMC.Proposal(Parm = chain[i - 1, ])
    proposal_log_likelihood <- Model.RunSim.LLH(
      Parm = Parameter.Create(
        beta_base = proposal[1], beta_seasonal = proposal[2], phi = proposal[3],
        seasonal_wavelength = proposal[4], Hosp_rate = c(proposal[5:15])
      ), TargetDat = TargetDat
    )

    # Info <- sprintf("n_iteration is: %d Current LLH is: %f Proposal LLH is: %f", i, current_log_likelihood, proposal_log_likelihood)
    # CLI.Print(Info)

    acceptance_ratio <- min(1, exp(proposal_log_likelihood - current_log_likelihood))
    if (runif(1) < acceptance_ratio) {
      chain[i, ] <- proposal
      current_log_likelihood <- proposal_log_likelihood
      accepted <- accepted + 1
    } else {
      chain[i, ] <- chain[i - 1, ]
    }
    # print(proposal)
    # print(chain[i, ])
    pb$tick()
  }
  return(chain)
}



MCMC.TracePlot <- function(dat) {
  colnames(dat) <- c(
    "beta_base", "beta_seasonal", "phi", "seasonal_wavelength",
    "Hosp_rateM0_2", "Hosp_rateM3_5", "Hosp_rateM6_11", "Hosp_rateY1_2", "Hosp_rateY2_4", "Hosp_rateY5_19",
    "Hosp_rateY20_59", "Hosp_rateY60_64", "Hosp_rateY65_69", "Hosp_rateY70_74", "Hosp_rateY75_"
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











run_model_get_logliks_seasonalonly <- function(parameter_guesses, bb = "no",
                                               model_type) {
  # combine the parameters

  parameters <- create_parameters(unlist(parameter_guesses))

  # print(parameters)
  # Run the model - seasonal corona only. For 20 years + a bit, to find low point (lp)
  output_s <- run_model_seasonal(parameters, model_type)
  colnames(output_s) <- naming_states(model_type)
  # Get the incidence of reporting
  reporting <- summary_stats_reported_seasonal(output_s, type = model_type)

  # reporting_selection2 <- reporting[required_set, c("time", ..all_others)
  # ][ , rowSums(.SD), by= time]

  # reject based on Techumseh
  likelihood_test <- 0 # reject_seasonal_Tech(output_s, model_type)

  if (likelihood_test == 0) {
    # summaries
    reportin_2020 <- summary_stats_reported_seasonal(output_s, type = model_type)
    reportin_2020_daily <- summary_groups(reportin_2020)
    # calculate likelihood of the data: monthly seasonal age
    # if(bb == "no"){
    #   lik_seasonal_ages <- calc_lik_seasonal_ages(reportin_2020_daily, parameters)
    # } else if (bb == "yes"){
    #   lik_seasonal_ages <- calc_lik_seasonal_ages_bb(reportin_2020_daily, parameters)
    # }
    if (bb == "binomial") {
      lik_seasonal_ages <- calc_lik_seasonal_ages_binomial(reportin_2020_daily, parameters)
    }

    likelihood_data <- lik_seasonal_ages
  } else {
    likelihood_data <- likelihood_test

    lik_seasonal_ages <- NA
    lik_covid_deaths <- NA
  }

  likelihood_total <- likelihood_data #+ get_llprior
  return(c(likelihood_total))
}



calc_lik_seasonal_ages_binomial <- function(reportin_2020_daily, parameters,
                                            covid_run = F) {
  # reported seasonal cases by age over time
  reportin_seasonal <- melt.data.table(reportin_2020_daily[, c("time", ..all_oths)],
    id = "time"
  ) # 对原始数据操作，宽表转长表

  # get the right values
  if (covid_run == T) {
    relevant_dates <- seasonal_dates
    real_data <- seasonal_19_20
  } else if (covid_run == F) {
    relevant_dates <- seasonal_dates_15
    real_data <- seasonal_15_20
  }
  reportin_seasonal[, date := as.Date(time, origin = lp_15)]
  # change from daily to monthly
  for (i in 1:(length(relevant_dates) - 1)) {
    reportin_seasonal[
      date >= relevant_dates[i] &
        date < relevant_dates[i + 1],
      year_week := relevant_dates[i]
    ]
  } # 找到日期对应的周数

  to_match <- na.omit(reportin_seasonal[, sum(value, na.rm = T), by = c(
    "year_week",
    "variable"
  )])
  # add the real data
  to_match[real_data, on = c("year_week", "variable"), true_value := i.value]


  to_match[variable == "OTHER_p0", likelihood := dbinom(
    x = true_value,
    size = round(V1),
    prob = parameters$seasonal_reported[1],
    log = T
  )]
  to_match[variable == "OTHER_p5", likelihood := dbinom(
    x = true_value,
    size = round(V1),
    prob = parameters$seasonal_reported[2],
    log = T
  )]
  to_match[variable == "OTHER_p15", likelihood := dbinom(
    x = true_value,
    size = round(V1),
    prob = parameters$seasonal_reported[3],
    log = T
  )]
  to_match[variable == "OTHER_p45", likelihood := dbinom(
    x = true_value,
    size = round(V1),
    prob = parameters$seasonal_reported[4],
    log = T
  )]
  to_match[variable == "OTHER_p65", likelihood := dbinom(
    x = true_value,
    size = round(V1),
    prob = parameters$seasonal_reported[5],
    log = T
  )]


  # for( stepper in 1:dim(to_match)[1]){
  #   if(to_match[stepper,"variable"] == "OTHER_p0"){age_set = 1
  #   } else if(to_match[stepper,"variable"] == "OTHER_p5"){age_set = 2
  #   } else if(to_match[stepper,"variable"] == "OTHER_p15"){age_set = 3
  #   } else if(to_match[stepper,"variable"] == "OTHER_p45"){age_set = 4
  #   } else if(to_match[stepper,"variable"] == "OTHER_p65"){age_set = 5}
  #   # work out quantile intervals
  #
  #   to_match[stepper,"likelihood"] <- dbinom(x=as.numeric(to_match[stepper, "true_value"]),
  #                                            size = round(as.numeric(to_match[stepper,"V1"])),
  #                                            prob = parameters$seasonal_reported[age_set]
  #                                            , log=T)
  #
  # }

  # weight the off_seasons by half
  lik_summary <- to_match[, sum(likelihood, na.rm = T)]
  lik_out <- lik_summary

  if (any(is.nan(to_match$likelihood))) {
    lik_out <- -10000
  }
  return(lik_out)
}
