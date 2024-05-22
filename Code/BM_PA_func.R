#' @title
#'
#' @description
#' @param InitialParm The initial parameters to start the MCMC from
#' @param Each_Iteration The number of iterations to run the model for each part of the trace. Each_Iteration should be a multiple of Indep_Iteration
#' @param Indep_Iteration The number of intervals to run the model independently
#' @param Adspt_Start The number of iterations to run the model before starting to adapt the temperatures
#' @param Part The current part of the trace 这里的pt是指运行的第几部分，在下面的trace_wrapper中会将一个整体的代码分割成多个部分进行运行，pt用于标记当前运行到了第几个部分，并且根据这个部分的标记决定
#' @return one value of the iteration step size
ParallelAnneling <- function(
    InitialParm, Each_Iteration, Indep_Iteration = 100, n_chains, TargetDat = TargetDat, Temperatures,
    Adspt_Start = Inf, AdaptRate = 1, prev_swap_proposed, prev_swap_accepted, Part, previous_trace = 0, ...) {
    # 记录温度
    Temperatures_store <- matrix(nrow = Each_Iteration / Indep_Iteration, ncol = n_chains)
    adapt_switch <- TRUE
    pars <- InitialParm # 初始化参数

    # 存储每个链的likelihood，如果只有一个链就建立一个df，否则根据链的数量建立一个list
    if (n_chains == 1) {
        likelihood <- data.frame()
    } else {
        likelihood <- list(length = n_chains)
    }

    # 创建每个链的空trace
    chains <- lapply(1:n_chains, function(i) {
        mat <- matrix(NA, nrow = Each_Iteration, ncol = 19)
        # 13列的内容：
        ## 1. log_likelihood
        ## 2-16. 参数
        ## 17. Accepted，使用数字01表示是否被接受
        ## 18. 储存交换到了哪条链
        ## 19. store swap attempted，是否需要交换，交换的是1，否则是0 [需要交换不代表进行了交换，需要根据概率判断]

        colnames(mat) <- c(
            "Likelihood", "beta_base", "beta_seasonal", "phi", "seasonal_wavelength", "Hosp_rateM0_2",
            "Hosp_rateM3_5", "Hosp_rateM6_11", "Hosp_rateY1_2", "Hosp_rateY2_4", "Hosp_rateY5_19",
            "Hosp_rateY20_59", "Hosp_rateY60_64", "Hosp_rateY65_69", "Hosp_rateY70_74", "Hosp_rateY75_",
            "Accepted", "Switch_To", "Swap_Attempted"
        )


        # set the first row of each trace to 0
        mat[, 18] <- 0 # 交换标记记录为0
        return(mat)
    })

    # The independent intervals, run model in parallel for each chain
    Interval <- matrix(1:Each_Iteration, nrow = Indep_Iteration)
    # interval是所有循环次数的索引，Indep_Iteration（每次交换前独立运行的时间
    ## 如第一列为1——Indep_Iteration，第二列为Indep_Iteration+1——2*Indep_Iteration

    for (iter in 1:(Each_Iteration / Indep_Iteration)) {
        # 每个链，在indep的步长内独立进行迭代，没有参数的交互
        pars <- lapply(1:n_chains, \(i) unlist(pars[[i]]))
        # 后面的代码是以list的形式保存每个参数的值，为了保证代码可以运行所以最开始传入的数值也是以list的形式传入。但是后面的计算需要向量的形式，因此这里需要进行转换
        out <- parLapply(
            ParallelNodesInfo[[1]], 1:n_chains,
            function(i) {
                # 这个函数的问题：1、out第一行的起始参数似乎没有放进去，要怎么放进去
                # 2、中间为什么要调整log acceptance

                # matrix to store the output
                out <- matrix(NA, nrow = Indep_Iteration, ncol = 19) # 9个参数，4个标记
                # 13列的内容同上面的chains

                for (t in 1:Indep_Iteration) {
                    # if first step, calculate the likelihood of the current parameter
                    if (t == 1) {
                        current_log_likelihood <- Model.RunSim.LLH(
                            Parm = Parameter.Create(
                                beta_base = pars[[i]][[1]], beta_seasonal = pars[[i]][[2]], phi = pars[[i]][[3]],
                                seasonal_wavelength = pars[[i]][[4]],
                                Hosp_rate = pars[[i]][5:15]
                            ), TargetDat = TargetDat
                        )
                    } else {
                        current_log_likelihood <- out[t - 1, 1]
                    }

                    # propose a new parameter
                    proposal <- MCMC.Proposal(Parm = pars[[i]])

                    proposal_log_likelihood <- Model.RunSim.LLH(
                        Parm = Parameter.Create(
                            beta_base = proposal[1], beta_seasonal = proposal[2], phi = proposal[3],
                            Hosp_rate = c(proposal[4:14])
                        ), TargetDat = TargetDat
                    )

                    log_acceptance <- proposal_log_likelihood - current_log_likelihood
                    # adjst the acceptance for the unsymmetrical proposal distribution
                    # 似乎由于作者使用的是截断多元正态分布，所以需要调整，使用拉普拉斯分布似乎不需要调整
                    # log_acceptance <- adjust_unsymmetrical()

                    # calculate the temperature weighting for the chain
                    beta_to_use <- 1 / Temperatures[i]

                    # alpha is the weighted liklihood of the proposal by the temperature
                    alpha <- exp(beta_to_use * log_acceptance)
                    if (is.nan(alpha)) {
                        alpha <- -Inf
                    }
                    if (runif(1) < alpha) {
                        pars[[i]] <- proposal
                        Liklihood_stored <- proposal_log_likelihood
                        Accepted <- 1
                        # out[t, 1] <- proposal_log_likelihood
                        # out[t, 2:10] <- proposal
                        # out[t, 11] <- 1
                        # out[t, 13:14] <- 0
                    } else {
                        Liklihood_stored <- current_log_likelihood
                        Accepted <- 0
                        # out[t, 1] <- out[t - 1, 1]
                        # out[t, 2:10] <- out[t - 1, 2:10]
                        # out[t, 11] <- 0
                        # out[t, 13:14] <- 0
                    }
                    out[t, ] <- c(Liklihood_stored, unlist(pars[[i]]), Accepted, 0, 0) # pars后面可能要再加一个[1]
                }
                out
            } # ParLapply 函数结束
        )

        # save the output from each Indep_Iteration into the main chains
        for (i in 1:n_chains) {
            # copy the trace for the interval into the overall trace (chain)
            chains[[i]][Interval[, iter], ] <- out[[i]]
            pars[[i]] <- as.list(out[[i]][Indep_Iteration, ][-c(1, 17, 18, 19)]) # 这里要不要list再考虑。后面的四个数是指out中非参数的内容
            # 这里要不要再加一个每个list元素的命名？
            if (n_chains == 1) {
                likelihood <- out[[i]][Indep_Iteration, ][1] # 只取了每个indep的最后一次循环的likelihood
            } else {
                likelihood[[i]] <- out[[i]][Indep_Iteration, ][1]
            }
        }


        # Propose a chain swaps every "Indep_Iteration" time steps. num swaps - num chains
        if (n_chains > 1) {
            for (swap_test in 1:n_chains) {
                # Choose two out of total number of chains and store their numbers in 'pick'
                pick <- sample(1:(n_chains - 1), 1)
                # 这里也可以直接抽取两个链，但是使用选择的链和相邻的链是为了保证链之间的交换具有一定的连续性和梯度，如果完全随机的两个链进行交换可能会因为温度相差太大导致交换次的成功率很低
                Chain_1 <- pick
                Chain_2 <- pick + 1 # for convience
                # calculate the swap potential
                # 只取了每个indep的最后一次循环的likelihood
                if (is.finite(likelihood[[Chain_1]]) & is.finite(likelihood[[Chain_2]])) {
                    SwapProb <- exp((likelihood[[Chain_1]] - likelihood[[Chain_2]]) * (1 / Temperatures[Chain_2] - 1 / Temperatures[Chain_1]))
                } else {
                    SwapProb <- -Inf
                }

                # store swap attempted
                chains[[Chain_1]][Interval[dim(Interval)[1], iter], 19] <- 1
                # [Interval[dim(Interval)[1], iter], 13]是找到每个独立循环的最后一行的行号，s是当前的循环次数，即找到每个独立循环的最后一次对应的循环数，
                ## 随后再用这个循环数载每个chain里找到对应行和第十三列对应的位置
                # accept or reject swap
                if (runif(1) < SwapProb) {
                    pars_temp <- pars[[Chain_1]] # 使用这个模式比直接更改chain更简单，更改chain尝试过了，太复杂
                    pars[[Chain_1]] <- pars[[Chain_2]]
                    pars[[Chain_2]] <- pars_temp

                    # swap the likrliooh. stops a swap back to the worse prameters
                    likelihood_temp <- likelihood[[Chain_1]]
                    likelihood[[Chain_1]] <- likelihood[[Chain_2]]
                    likelihood[[Chain_2]] <- likelihood_temp

                    # store the number it swapped to
                    chains[[Chain_1]][Interval[dim(Interval)[1], iter], 18] <- Chain_2
                }
            }
        }

        if (Interval[1, iter] > Adspt_Start) { # adapt_start相当于burn in的时间

            if (adapt_switch == TRUE) {
                CLI.Print("Starting to adapt at this point.......")
                adapt_switch <- FALSE
            }

            # calculate swap rate for each temperatre
            swap_rates <- sapply(1:n_chains, function(i) {
                swapping_rates(chains[[i]],
                    prev_swap_proposed = prev_swap_proposed[i],
                    prev_swap_accepted = prev_swap_accepted[i]
                )
            })
            # swap_rates[1] is between 1 and 2, swap_rates[2] is between 2 and 3 etc.
            # calculate kappa
            if (previous_trace == 0) { # 这个previous_trace似乎可以去掉，直接用等于0的就可以
                time_adapting <- Interval[dim(Interval)[1], iter] + (Part - 1) * Each_Iteration - Adspt_Start
            } else {
                time_adapting <- (Interval[dim(Interval)[1], iter] +
                    (Part - 1) * Each_Iteration - Adspt_Start) +
                    (previous_trace - Adspt_Start)
            }

            kappa <- 1 / (1 + time_adapting)^(AdaptRate) # 时间依赖的调整系数，随时间增大而减小
            # store current temperatures
            temps_store <- Temperatures
            for (tem in 2:(n_chains - 1)) {
                # calculate s = log(Ti - Ti+1) for first chain
                S2 <- log(temps_store[tem] - temps_store[tem - 1]) # 当前温度雨相邻温度的对数差
                # calculate change in Schange = k[Ai - Ai+1] + S for first chain
                s2new <- kappa * (swap_rates[tem] - swap_rates[tem + 1]) + S2
                # calculate new temperature, based on new temperature for previous one
                Tnew <- exp(s2new) + Temperatures[tem - 1]
                Temperatures[tem] <- Tnew
                # 当前的温度用上一个温度加上两个温度之间的间隔得到，在两个温度之间的间隔加上swap_rates的变化就完成了降温，
                ## 如果swap_rates的变化为正数则增温，否则就是降温。swap_rates是当前与下一个温度的差
            }
        } # 当pt<pt_start时,Part=Inf,time_adapting=Inf，此时kappa=Inf，s2new=0+S2, Tnew=exp(0+S2)+Temperatures[tem-1]

        # print at right point in time
        percent_points <- seq(from = 0, to = Each_Iteration / Indep_Iteration, by = (Each_Iteration / Indep_Iteration) / 10)
        if (iter %in% percent_points) {
            CLI.Print(paste0("Part complete: ", iter / (Each_Iteration / Indep_Iteration) * 100, "%"))
        }

        Temperatures_store[iter, ] <- Temperatures
    } # 每个indep循环结束

    # Returns the full history of all chains
    swaps_proposed <- sapply(1:n_chains, function(i) {
        swapping_props(chains[[i]],
            prev_swap_proposed = prev_swap_proposed[i]
        )
    })
    swaps_accepted <- sapply(1:n_chains, function(i) {
        swapping_acceps(chains[[i]],
            prev_swap_accepted = prev_swap_accepted[i]
        )
    })

    # return the swap attempts and swaps accepted for each chain
    chains[["temperatures"]] <- Temperatures_store
    chains[["swaps_proposed"]] <- swaps_proposed
    chains[["swaps_accepted"]] <- swaps_accepted
    chains[["Parm"]] <- pars
    # 增加以一个pars的输出，如果使用原始的方法，在每一个Each_Iteration结束后的参数交换无法继承到下一个Each_Iteration
    return(chains)
}



swapping_rates <- function(trace_in, prev_swap_proposed, prev_swap_accepted) {
    swap_tot <- sum(trace_in[, 19], na.rm = T) # total swaps proposed
    swap_tot <- swap_tot + prev_swap_proposed
    swap_accep <- length(which(trace_in[, 18] != 0)) # total swaps accepted
    swap_accep <- swap_accep + prev_swap_accepted

    if (swap_tot == 0) {
        swap_rate <- 0
    } else {
        swap_rate <- swap_accep / swap_tot
    }
    return(swap_rate)
}

# number swaps proposed(takes into account those from previous wrap)
swapping_props <- function(trace_in, prev_swap_proposed) {
    swap_tot <- sum(trace_in[, 19], na.rm = T) # total swaps proposed
    swap_tot <- swap_tot + prev_swap_proposed

    return(swap_tot)
}

# number swaps accepted (takes into account those from previous wrap)
swapping_acceps <- function(trace_in, prev_swap_accepted) {
    swap_accep <- length(which(trace_in[, 18] != 0)) # total swaps accepted
    swap_accep <- swap_accep + prev_swap_accepted

    return(swap_accep)
}

# add on to previous
add_on <- function(tt, t) {
    tout <- rbind(tt, t)
    return(tout)
}


# 在原始的代码中length_run = 1[总迭代次数]。each_run = 1[每一个自适应调整的部分中运行多少次], Indep_Iteration = 1[独立运行一段（如100次）之后再进行交换和降温]

#' @param InitialParm The initial parameters to start the MCMC from
#' @param Max_Iteration The total number of iterations to run the model
#' @param Each_Iteration The number of iterations to run the model for each part of the trace
#' @param Indep_Iteration The number of iterations to run the model independently
#' @param AdaptionStarter Where to start the adaption, before this point the model will run without adaption
#' @param AdaptRate Temperature adaption cooling rate
#' @param ContinueTrace Whether to continue the trace from the previous run
# 是否继续上一次的trace，F代表每一次都从初始参数开始，T代表从上一次的trace开始，一些情况下可以用来继续上一次的trace，用于多阶段运行MCMC，需要配合trace的存储和读取
TraceWrapper <- function(
    InitialParm, Max_Iteration, Each_Iteration, Indep_Iteration = 100, n_chains,
    AdaptionStarter, AdaptRate = 1, Temperatures, ContinueTrace = FALSE,
    TargetDat = TargetDat, Save = TRUE, Path, Note, ...) {
    CLI.Print("Starting Running ......")
    ResultNames <- c(
        sapply(1:length(Temperatures), \(x) paste0("Temp_", Temperatures[x])),
        "Temperatures", "swaps_proposed", "swaps_accepted"
    )
    # calculate in which Part to start the adaption
    # which one should it start in and the remainder
    Part_Start <- ceiling(AdaptionStarter / Each_Iteration)
    # 这里指从哪里开始自适应调整，在此之前按照初始参数进行运行，之后按照自适应调整的参数进行运行，与burn in类似，但不是仅指burn in结束的时间
    Part_Remain <- AdaptionStarter %% Each_Iteration # 剩余多少个部分

    # wrapper around traces
    for (Part in 1:(Max_Iteration / Each_Iteration)) {
        # specify the start and end of the subset
        if (Part == Part_Start) {
            if (Part_Remain == 0) {
                Adspt_Start <- Each_Iteration
            } else {
                Adspt_Start <- Part_Remain
            }
        } else if (Part < Part_Start) {
            Adspt_Start <- Inf
        } else if (Part > Part_Start) {
            Adspt_Start <- 0
        }

        # specify the InitialParm as the main one if the first one
        if (Part == 1) {
            if (ContinueTrace == FALSE) {
                InitialParm <- lapply(1:n_chains, function(x) as.list(InitialParm))
            }
            prev_swap_proposed <- rep(0, n_chains)
            prev_swap_accepted <- rep(0, n_chains)
        }

        # run the parallel annealing
        trace <- ParallelAnneling(
            InitialParm = InitialParm,
            Each_Iteration = Each_Iteration,
            TargetDat = TargetDat,
            Threshold = Threshold,
            OnsetThreshold = OnsetThreshold,
            Indep_Iteration = Indep_Iteration,
            n_chains = n_chains,
            Temperatures = Temperatures,
            Adspt_Start = Adspt_Start,
            AdaptRate = AdaptRate,
            prev_swap_proposed = prev_swap_proposed,
            prev_swap_accepted = prev_swap_accepted,
            Part = Part,
            # previous_trace = previous_trace # 这个也许不需要单独写出来
        )

        # update the InitialParm to the new one
        UpdateParm <- trace[["Parm"]]
        trace[["Parm"]] <- NULL
        #   lapply(1:n_chains, function(x) {
        #     as.list(tail(total_trace[[x]], 1)[, 2:10]) # 找到每个trace的最后一行（最后一次循环）的参数
        # })
        # for (section in 1:n_chains) {
        #     names(UpdateParm[[section]]) <- names(proposal_sd)
        # }
        InitialParm <- UpdateParm

        # save the trace into total_trace
        if (Part == 1) {
            total_trace <- trace
        } else {
            total_trace <- lapply(1:length(trace), function(x) {
                add_on(total_trace[[x]], trace[[x]])
            })
        }

        # update the temperatures to the correct ones
        Temperatures <- c(tail(trace[["temperatures"]], 1))
        prev_swap_proposed <- trace[["swaps_proposed"]]
        prev_swap_accepted <- trace[["swaps_accepted"]]

        CLI.Print(paste0("Overall: ", Part / (Max_Iteration / Each_Iteration) * 100, "%"))

        if (Save) {
            save(total_trace, file = paste0(Path, Note, "_", Part, "of", (Max_Iteration / Each_Iteration), "_", Sys.Date(), ".Rdata"))
        }

        # print(total_trace)
    }
    names(total_trace) <- ResultNames
    rownames(total_trace[[length(total_trace) - 1]]) <- 1:length(total_trace[[length(total_trace) - 1]][, 1])
    rownames(total_trace[[length(total_trace)]]) <- 1:length(total_trace[[length(total_trace)]][, 1])
    return(total_trace)
}
