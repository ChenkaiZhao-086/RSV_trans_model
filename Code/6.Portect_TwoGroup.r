All_Variable <- ls()
FindVarable <- All_Variable[grep("^.+_E.C..$", All_Variable)]
FindVarableList <- as.list(FindVarable)

### Hospitalisation protection -------------------------------------------------
#-------------------------------------------------------------------------------
### Net Protect (hospitalisation) AKA. Number of Hospitalisation cases averted in immunised and unimmunised groups
#-------------------------------------------------------------------------------
NetProtectHosp <- lapply(FindVarableList, \(x) {
    dat <- get(x)[["NetProtectHosp_2age"]]

    parts <- strsplit(x, "_")[[1]]

    if (parts[1] == "S44") {
        prefix <- "S2"
    } else if (parts[1] == "S43") {
        prefix <- "S3"
    } else if (parts[1] == "S42") {
        prefix <- "S4"
    } else if (parts[1] == "SBase") {
        prefix <- "S1"
    }

    x <- paste(prefix, parts[2], sep = "_")

    dat$Scenario <- x
    return(dat)
})

NetProtectHosp <- do.call(rbind, NetProtectHosp)
NetProtectHosp$CI <- NULL

NetProtectHosp <- NetProtectHosp %>%
    as.data.frame() %>%
    mutate(
        Scenario = factor(Scenario),
        age_group = ifelse(age_group == "Direct", "Immunised", "Unimmunised")
    )

### Plot Fig 2d1
NetProtectHosp_Main <- NetProtectHosp %>%
    filter(Scenario %in% c(
        "S1_E7C8A", "S1_E7C8B", "S1_E7C8C",
        "S2_E7C8A", "S2_E7C8B", "S2_E7C8C"
    )) %>%
    mutate(
        class = substr(Scenario, 1, 2),
        class = factor(class, levels = c("S2", "S1")),
        Scenario = factor(Scenario,
            levels = c(
                "S1_E7C8A", "S2_E7C8A",
                "S1_E7C8B", "S2_E7C8B",
                "S1_E7C8C", "S2_E7C8C"
            ),
            labels = c(
                "0-11m", "0-11m",
                "0-23m", "0-23m",
                "0-4y", "0-4y"
            )
        )
    )

NetProtectHosp_Main_Fig <- ggplot(NetProtectHosp_Main, aes(x = median, y = Scenario, fill = class)) +
    geom_bar(stat = "identity", alpha = 0.9, position = position_dodge(width = 0.6), width = 0.6) +
    geom_errorbar(aes(xmin = lci, xmax = uci),
        width = 0.3, position = position_dodge(width = 0.6)
    ) +
    scale_fill_manual(values = c(
        "#79AF97FF", "#DF8F44FF", "#00A1D5FF"
    )) +
    theme_classic() +
    # geom_hline(yintercept = seq(0.5, 37.5, 1), color = "gray75", linetype = "longdash") +
    # geom_hline(yintercept = seq(3.5, 37.5, 6), color = "gray40", linetype = "longdash") +
    geom_hline(yintercept = seq(1.5, 37.5, 1), color = "gray10", linetype = "longdash") +
    scale_y_discrete(limits = rev(levels(NetProtectHosp_Main$Scenario))) +
    facet_wrap(~age_group, scales = "free_x") +
    labs(
        y = "Programmes",
        x = "Number of cases averted",
        color = "Age group"
    ) +
    theme(
        axis.text = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(margin = margin(r = 10)),
        axis.title = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10)),
        legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(size = 14, face = "bold"),
        panel.spacing = unit(1.5, "lines"),
        plot.margin = margin(r = 10)
    )
ggsave(NetProtectHosp_Main_Fig, file = paste0(FilePath, "2d1.NetProtect_Hosp.pdf"), width = 8, height = 3)



### For appendix: all scenarios
NetProtect_Hosp_Fig <- ggplot(NetProtectHosp, aes(x = median, y = Scenario, color = age_group)) +
    geom_point(alpha = 0.5, position = position_dodge(width = 0.7)) +
    geom_errorbar(aes(xmin = lci, xmax = uci),
        width = 0.4, position = position_dodge(width = 0.7)
    ) +
    scale_color_manual(values = c(
        "#008537", "#f25235"
    )) +
    theme_bw() +
    geom_hline(yintercept = seq(0.5, 74.5, 1), color = "gray75", linetype = "longdash") +
    geom_hline(yintercept = seq(6.5, 74.5, 6), color = "gray40", linetype = "longdash") +
    geom_hline(yintercept = seq(18.5, 74.5, 18), color = "gray10", linetype = "solid") +
    scale_y_discrete(limits = rev(levels(NetProtectHosp$Scenario))) +
    labs(
        y = "Programmes",
        x = "Number of cases averted",
        color = "Age group"
    ) +
    theme(
        axis.text = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.text.x = element_text(margin = margin(b = 5)),
        legend.title = element_blank(),
        legend.text = element_text(size = 14, face = "bold"),
        legend.position = "right",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
    )

ggsave(NetProtect_Hosp_Fig, file = paste0(FilePath, "2d1.NetProtect_Hosp_Appendix.pdf"), width = 12, height = 16)


#-------------------------------------------------------------------------------
### Proportion (hospitalisation) AKA. Proportion of RSV hospitalisation cases averted in immunised and unimmunised groups
#-------------------------------------------------------------------------------
PropHosp <- lapply(FindVarableList, \(x) {
    dat <- get(x)[["VacProtectionHosp"]]

    parts <- strsplit(x, "_")[[1]]

    if (parts[1] == "S44") {
        prefix <- "S2"
    } else if (parts[1] == "S43") {
        prefix <- "S3"
    } else if (parts[1] == "S42") {
        prefix <- "S4"
    } else if (parts[1] == "SBase") {
        prefix <- "S1"
    }

    x <- paste(prefix, parts[2], sep = "_")

    dat$Scenario <- x
    return(dat)
})

PropHosp <- do.call(rbind, PropHosp)
PropHosp$CI <- NULL

PropHosp <- PropHosp %>%
    as.data.frame() %>%
    mutate(
        Scenario = factor(Scenario),
        type = ifelse(type == "DirectProtectHosp", "Immunised",
            ifelse(type == "InDirectProtectHosp", "Unimmunised", type)
        )
    ) %>%
    filter(type != "TotalProtectHosp")

### Plot Fig 2d2
PropHosp_Main <- PropHosp %>%
    filter(Scenario %in% c(
        "S1_E7C8A", "S1_E7C8B", "S1_E7C8C",
        "S2_E7C8A", "S2_E7C8B", "S2_E7C8C"
    )) %>%
    mutate(
        class = substr(Scenario, 1, 2),
        class = factor(class, levels = c("S2", "S1")),
        Scenario = factor(Scenario,
            levels = c(
                "S1_E7C8A", "S2_E7C8A",
                "S1_E7C8B", "S2_E7C8B",
                "S1_E7C8C", "S2_E7C8C"
            ),
            labels = c(
                "0-11m", "0-11m",
                "0-23m", "0-23m",
                "0-4y", "0-4y"
            )
        )
    )


PropHosp_Main_Fig <- ggplot(PropHosp_Main, aes(x = median, y = Scenario, fill = class)) +
    geom_bar(stat = "identity", alpha = 0.9, position = position_dodge(width = 0.6), width = 0.6) +
    geom_errorbar(aes(xmin = lci, xmax = uci),
        width = 0.3, position = position_dodge(width = 0.6)
    ) +
    scale_fill_manual(values = c(
        "#79AF97FF", "#DF8F44FF", "#00A1D5FF"
    )) +
    theme_classic() +
    # geom_hline(yintercept = seq(0.5, 37.5, 1), color = "gray75", linetype = "longdash") +
    # geom_hline(yintercept = seq(3.5, 37.5, 6), color = "gray40", linetype = "longdash") +
    geom_hline(yintercept = seq(1.5, 37.5, 1), color = "gray10", linetype = "longdash") +
    scale_y_discrete(limits = rev(levels(PropHosp_Main$Scenario))) +
    labs(
        y = "Programmes",
        x = "Proportion of cases averted (%)",
        color = "Age group"
    ) +
    facet_wrap(~type, scales = "free_x") +
    theme(
        axis.text = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(margin = margin(r = 10)),
        axis.title = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10)),
        legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(size = 14, face = "bold"),
        panel.spacing = unit(1.5, "lines"),
        plot.margin = margin(r = 10),
        axis.ticks.y = element_blank(), # 删除y轴刻度线
        axis.line.y = element_blank() # 删除y轴线
    )
ggsave(PropHosp_Main_Fig, file = paste0(FilePath, "2d2.PropHosp.pdf"), width = 8, height = 3)

### For appendix: all scenarios -------------------------------------------------------------
PropHosp_Fig <- ggplot(PropHosp, aes(x = median, y = Scenario, colour = type)) +
    geom_point(alpha = 0.5, position = position_dodge(width = 0.7)) +
    geom_errorbar(aes(xmin = lci, xmax = uci),
        width = 0.4, position = position_dodge(width = 0.7)
    ) +
    scale_color_manual(values = c(
        "#008537", "#f25235"
    )) +
    theme_bw() +
    geom_hline(yintercept = seq(0.5, 74.5, 1), color = "gray75", linetype = "longdash") +
    geom_hline(yintercept = seq(6.5, 74.5, 6), color = "gray40", linetype = "longdash") +
    geom_hline(yintercept = seq(18.5, 74.5, 18), color = "gray10", linetype = "solid") +
    geom_vline(xintercept = 0, color = "gray85") +
    scale_y_discrete(limits = rev(levels(PropHosp$Scenario))) +
    labs(
        y = "Programmes",
        x = "Proportion of cases averted (%)",
        color = "Age group"
    ) +
    theme(
        axis.text = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.text.x = element_text(margin = margin(b = 5)),
        legend.title = element_blank(),
        legend.text = element_text(size = 14, face = "bold"),
        legend.position = "right",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
    )

ggsave(PropHosp_Fig, file = paste0(FilePath, "2d2.PropHosp_Appendix.pdf"), width = 12, height = 16)







### Infection protection -------------------------------------------------------
#-------------------------------------------------------------------------------
### Net Protect (infection) AKA. Number of infection cases averted in immunised and unimmunised groups
#-------------------------------------------------------------------------------
NetProtectInfe <- lapply(FindVarableList, \(x) {
    dat <- get(x)[["NetProtectInfe_2age"]]

    parts <- strsplit(x, "_")[[1]]

    if (parts[1] == "S44") {
        prefix <- "S2"
    } else if (parts[1] == "S43") {
        prefix <- "S3"
    } else if (parts[1] == "S42") {
        prefix <- "S4"
    } else if (parts[1] == "SBase") {
        prefix <- "S1"
    }

    x <- paste(prefix, parts[2], sep = "_")

    dat$Scenario <- x
    return(dat)
})

NetProtectInfe <- do.call(rbind, NetProtectInfe)
NetProtectInfe$CI <- NULL

NetProtectInfe <- NetProtectInfe %>%
    as.data.frame() %>%
    mutate(
        Scenario = factor(Scenario),
        age_group = ifelse(age_group == "Direct", "Immunised", "Unimmunised")
    )

### Plot Fig 2e1
NetProtectInfe_Main <- NetProtectInfe %>%
    filter(Scenario %in% c(
        "S1_E7C8A", "S1_E7C8B", "S1_E7C8C",
        "S2_E7C8A", "S2_E7C8B", "S2_E7C8C"
    )) %>%
    mutate(
        class = substr(Scenario, 1, 2),
        class = factor(class, levels = c("S2", "S1")),
        Scenario = factor(Scenario,
            levels = c(
                "S1_E7C8A", "S2_E7C8A",
                "S1_E7C8B", "S2_E7C8B",
                "S1_E7C8C", "S2_E7C8C"
            ),
            labels = c(
                "0-11m", "0-11m",
                "0-23m", "0-23m",
                "0-4y", "0-4y"
            )
        )
    )

NetProtectInfe_Main_Fig <- ggplot(NetProtectInfe_Main, aes(x = median, y = Scenario, fill = class)) +
    geom_bar(stat = "identity", alpha = 0.9, position = position_dodge(width = 0.6), width = 0.6) +
    geom_errorbar(aes(xmin = lci, xmax = uci),
        width = 0.3, position = position_dodge(width = 0.6)
    ) +
    scale_fill_manual(values = c(
        "#79AF97FF", "#DF8F44FF", "#00A1D5FF"
    )) +
    theme_classic() +
    # geom_hline(yintercept = seq(0.5, 37.5, 1), color = "gray75", linetype = "longdash") +
    # geom_hline(yintercept = seq(3.5, 37.5, 6), color = "gray40", linetype = "longdash") +
    geom_hline(yintercept = seq(1.5, 37.5, 1), color = "gray10", linetype = "longdash") +
    scale_x_continuous(labels = label_number(big.mark = "")) + # 禁用科学计数法
    scale_y_discrete(limits = rev(levels(NetProtectInfe_Main$Scenario))) +
    labs(
        y = "Programmes",
        x = "Number of cases averted",
        color = "Age group"
    ) +
    facet_wrap(~age_group, scales = "free_x") +
    theme(
        axis.text = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(margin = margin(r = 10)),
        axis.title = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10)),
        legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(size = 14, face = "bold"),
        panel.spacing = unit(1.5, "lines"),
        plot.margin = margin(r = 10)
    )
ggsave(NetProtectInfe_Main_Fig, file = paste0(FilePath, "2e1.NetProtect_Infe.pdf"), width = 8, height = 3)

### Plot Fig 2e1 For appendix (All scenarios) -------------------------------------------------------------
NetProtect_Infe_Fig <- ggplot(NetProtectInfe, aes(x = median, y = Scenario, color = age_group)) +
    geom_point(alpha = 0.5, position = position_dodge(width = 0.7)) +
    geom_errorbar(aes(xmin = lci, xmax = uci),
        width = 0.4, position = position_dodge(width = 0.7)
    ) +
    scale_color_manual(values = c(
        "#008537", "#f25235"
    )) +
    theme_bw() +
    geom_hline(yintercept = seq(0.5, 74.5, 1), color = "gray75", linetype = "longdash") +
    geom_hline(yintercept = seq(6.5, 74.5, 6), color = "gray40", linetype = "longdash") +
    geom_hline(yintercept = seq(18.5, 74.5, 18), color = "gray10", linetype = "solid") +
    scale_y_discrete(limits = rev(levels(NetProtectInfe$Scenario))) +
    labs(
        y = "Programmes",
        x = "Number of cases averted",
        color = "Age group"
    ) +
    theme(
        axis.text = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.text.x = element_text(margin = margin(b = 5)),
        legend.title = element_blank(),
        legend.text = element_text(size = 14, face = "bold"),
        legend.position = "right",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
    )

ggsave(NetProtect_Infe_Fig, file = paste0(FilePath, "2e1.NetProtect_Infe_Appendix.pdf"), width = 12, height = 16)


#-------------------------------------------------------------------------------
### Proportion (infection)
#-------------------------------------------------------------------------------
PropInfe <- lapply(FindVarableList, \(x) {
    dat <- get(x)[["VacProtectionInfe"]]

    parts <- strsplit(x, "_")[[1]]

    if (parts[1] == "S44") {
        prefix <- "S2"
    } else if (parts[1] == "S43") {
        prefix <- "S3"
    } else if (parts[1] == "S42") {
        prefix <- "S4"
    } else if (parts[1] == "SBase") {
        prefix <- "S1"
    }

    x <- paste(prefix, parts[2], sep = "_")

    dat$Scenario <- x
    return(dat)
})

PropInfe <- do.call(rbind, PropInfe)
PropInfe$CI <- NULL

PropInfe <- PropInfe %>%
    as.data.frame() %>%
    mutate(
        Scenario = factor(Scenario),
        type = ifelse(type == "DirectProtectInfe", "Immunised",
            ifelse(type == "InDirectProtectInfe", "Unimmunised", type)
        )
    ) %>%
    filter(type != "TotalProtectInfe")

### Plot Fig 2e2
PropInfe_Main <- PropInfe %>%
    filter(Scenario %in% c(
        "S1_E7C8A", "S1_E7C8B", "S1_E7C8C",
        "S2_E7C8A", "S2_E7C8B", "S2_E7C8C"
    )) %>%
    mutate(
        class = substr(Scenario, 1, 2),
        class = factor(class, levels = c("S2", "S1")),
        Scenario = factor(Scenario,
            levels = c(
                "S1_E7C8A", "S2_E7C8A",
                "S1_E7C8B", "S2_E7C8B",
                "S1_E7C8C", "S2_E7C8C"
            ),
            labels = c(
                "0-11m", "0-11m",
                "0-23m", "0-23m",
                "0-4y", "0-4y"
            )
        )
    )

PropInfe_Main_Fig <- ggplot(PropInfe_Main, aes(x = median, y = Scenario, fill = class)) +
    geom_bar(stat = "identity", alpha = 0.9, position = position_dodge(width = 0.6), width = 0.6) +
    geom_errorbar(aes(xmin = lci, xmax = uci),
        width = 0.3, position = position_dodge(width = 0.6)
    ) +
    scale_fill_manual(values = c(
        "#79AF97FF", "#DF8F44FF", "#00A1D5FF"
    )) +
    theme_classic() +
    # geom_hline(yintercept = seq(0.5, 37.5, 1), color = "gray75", linetype = "longdash") +
    # geom_hline(yintercept = seq(3.5, 37.5, 6), color = "gray40", linetype = "longdash") +
    geom_hline(yintercept = seq(1.5, 37.5, 1), color = "gray10", linetype = "longdash") +
    scale_y_discrete(limits = rev(levels(PropInfe_Main$Scenario))) +
    labs(
        y = "Programmes",
        x = "Proportion of cases averted (%)",
        color = "Age group"
    ) +
    facet_wrap(~type, scales = "free_x") +
    theme(
        axis.text = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(margin = margin(r = 10)),
        axis.title = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10)),
        legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(size = 14, face = "bold"),
        panel.spacing = unit(1.5, "lines"),
        plot.margin = margin(r = 10),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank()
    )
ggsave(PropInfe_Main_Fig, file = paste0(FilePath, "2e2.PropInfe.pdf"), width = 8, height = 3)

### Plot Fig 2e2 For appendix (All scenarios) -------------------------------------------------------------
PropInfe_Fig <- ggplot(PropInfe, aes(x = median, y = Scenario, color = type)) +
    geom_point(alpha = 0.5, position = position_dodge(width = 0.7)) +
    geom_errorbar(aes(xmin = lci, xmax = uci),
        width = 0.4, position = position_dodge(width = 0.7)
    ) +
    scale_color_manual(values = c(
        "#008537", "#f25235"
    )) +
    theme_bw() +
    geom_hline(yintercept = seq(0.5, 74.5, 1), color = "gray75", linetype = "longdash") +
    geom_hline(yintercept = seq(6.5, 74.5, 6), color = "gray40", linetype = "longdash") +
    geom_hline(yintercept = seq(18.5, 74.5, 18), color = "gray10", linetype = "solid") +
    geom_vline(xintercept = 0, color = "gray85") +
    scale_y_discrete(limits = rev(levels(PropInfe$Scenario))) +
    labs(
        y = "Programmes",
        x = "Proportion of cases averted (%)",
        color = "Age group"
    ) +
    theme(
        axis.text = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.text.x = element_text(margin = margin(b = 5)),
        legend.title = element_blank(),
        legend.text = element_text(size = 14, face = "bold"),
        legend.position = "right",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
    )

ggsave(PropInfe_Fig, file = paste0(FilePath, "2e2.PropInfe_Appendix.pdf"), width = 12, height = 16)
