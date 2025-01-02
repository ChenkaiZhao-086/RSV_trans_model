All_Variable <- ls()
FindVarable <- All_Variable[grep("^.+_E.C..$", All_Variable)]
FindVarableList <- as.list(FindVarable)

#---------------------------------------------------------------
### Hospitalisation Net Reduction
#---------------------------------------------------------------
VacProtectNet_All <- lapply(FindVarableList, \(x) {
    dat <- get(x)[["NetProtectHosp_all"]]
    dat <- dat %>%
        t() %>%
        as.data.frame()

    parts <- strsplit(x, "_")[[1]]

    if (parts[1] == "S44") {
        prefix <- "S2"
    } else if (parts[1] == "S43") {
        prefix <- "S3"
    } else if (parts[1] == "S42") {
        prefix <- "S4"
    } else if (parts[1] == "SBase") {
        prefix <- "S1"
    } else if (parts[1] == "SBaseM") {
        prefix <- "S5"
    }

    x <- paste(prefix, parts[2], sep = "_")

    dat$Scenario <- x
    return(dat)
})

VacProtectNet_All <- do.call(rbind, VacProtectNet_All)
VacProtectNet_All$type <- "Hospitalisation"

colnames(VacProtectNet_All) <- c("lci", "median", "uci", "Scenario", "type")


#---------------------------------------------------------------
### Infection Net Reduction
#---------------------------------------------------------------
VacProtectNet_Infe <- lapply(FindVarableList, \(x) {
    dat <- get(x)[["NetProtectInfe_all"]]
    dat <- dat %>%
        t() %>%
        as.data.frame()

    parts <- strsplit(x, "_")[[1]]

    if (parts[1] == "S44") {
        prefix <- "S2"
    } else if (parts[1] == "S43") {
        prefix <- "S3"
    } else if (parts[1] == "S42") {
        prefix <- "S4"
    } else if (parts[1] == "SBase") {
        prefix <- "S1"
    } else if (parts[1] == "SBaseM") {
        prefix <- "S5"
    }

    x <- paste(prefix, parts[2], sep = "_")

    dat$Scenario <- x
    return(dat)
})

VacProtectNet_Infe <- do.call(rbind, VacProtectNet_Infe)
VacProtectNet_Infe$type <- "Infection"

colnames(VacProtectNet_Infe) <- c("lci", "median", "uci", "Scenario", "type")

### Bind the two dataframes
VacProtectNet_All <- rbind(VacProtectNet_All, VacProtectNet_Infe) %>%
    mutate(
        Scenario = factor(Scenario),
        type = factor(type)
    )


### Plot Fig 2a1 and 2b1
NetProtectAll_Main <- VacProtectNet_All %>%
    filter(Scenario %in% c(
        "S1_E7C8A", "S1_E7C8B", "S1_E7C8C",
        "S5_E7C8A", "S5_E7C8B", "S5_E7C8C",
        "S2_E7C8A", "S2_E7C8B", "S2_E7C8C"
    )) %>%
    mutate(
        class = substr(Scenario, 1, 2),
        class = factor(class, levels = c("S2", "S5", "S1")),
        Scenario = factor(Scenario,
            levels = c(
                "S1_E7C8A", "S5_E7C8A", "S2_E7C8A",
                "S1_E7C8B", "S5_E7C8B", "S2_E7C8B",
                "S1_E7C8C", "S5_E7C8C", "S2_E7C8C"
            ),
            labels = c(
                "0-11m", "0-11m", "0-11m",
                "0-23m", "0-23m", "0-23m",
                "0-4y", "0-4y", "0-4y"
            )
        )
    )

NetProtectAll_Fig <- ggplot(NetProtectAll_Main, aes(x = median, y = Scenario, fill = class)) +
    geom_bar(stat = "identity", alpha = 0.9, position = position_dodge(width = 0.6), width = 0.6) +
    geom_errorbar(aes(xmin = lci, xmax = uci),
        width = 0.3, position = position_dodge(width = 0.6)
    ) +
    scale_fill_manual(values = c(
        "#79AF97FF", "#DF8F44FF", "#00A1D5FF"
    )) +
    theme_classic() +
    # geom_hline(yintercept = seq(0.5, 37.5, 1), color = "gray75", linetype = "longdash") +
    geom_hline(yintercept = seq(1.5, 37.5, 1), color = "gray10", linetype = "longdash") +
    scale_y_discrete(limits = rev(levels(NetProtectAll_Main$Scenario))) +
    scale_x_continuous(limits = function(x) c(min(x), max(x) * 1.18)) +
    facet_wrap(~type, scales = "free_x") +
    labs(
        y = "Programmes",
        x = "Number of cases averted"
    ) +
    theme(
        axis.text = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(margin = margin(r = 10)),
        axis.title = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10)),
        legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(size = 14, face = "bold"),
        panel.spacing = unit(1.7, "lines"),
        plot.margin = margin(r = 15)
    ) +
    geom_text(aes(x = uci, label = round(median, 1)),
        position = position_dodge(width = 0.6),
        hjust = -0.2,
        size = 3.5,
        fontface = "bold"
    )
ggsave(NetProtectAll_Fig, file = paste0(FilePath, "2a1.NetProtectAll.pdf"), width = 8, height = 3)

### For appendix: all scenarios
VacProtectNet_All_Fig <- ggplot(VacProtectNet_All, aes(x = median, y = Scenario, color = type)) +
    geom_point(alpha = 0.5, position = position_dodge(width = 0.7)) +
    geom_errorbar(aes(xmin = lci, xmax = uci),
        width = 0.4, position = position_dodge(width = 0.7)
    ) +
    scale_color_manual(values = c(
        "#074ba9", "#be134c"
    )) +
    theme_bw() +
    geom_hline(yintercept = seq(0.5, 92.5, 1), color = "gray75", linetype = "longdash") +
    geom_hline(yintercept = seq(6.5, 92.5, 6), color = "gray40", linetype = "longdash") +
    geom_hline(yintercept = seq(18.5, 92.5, 18), color = "gray10", linetype = "solid") +
    scale_y_discrete(limits = rev(levels(VacProtectNet_All$Scenario))) +
    # scale_x_log10() +
    facet_wrap(~type, scales = "free_x") +
    labs(
        y = "Programmes",
        x = "Number of cases averted"
    ) +
    theme(
        axis.text = element_text(size = 6, face = "bold"),
        axis.title = element_text(size = 7, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.text.x = element_text(margin = margin(b = 5)),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "grey95"),
        strip.text = element_text(size = 7, face = "bold"),
        plot.margin = margin(l = 6, r = 6, b = 6)
    )

ggsave(VacProtectNet_All_Fig, file = paste0(FilePath, "2a1.NetProtectAll_Appendix.pdf"), width = 6, height = 8)




#----------------------------------------------------------------
### Hospitalization Proportion
#----------------------------------------------------------------
VacProtect_All <- lapply(FindVarableList, \(x) {
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
    } else if (parts[1] == "SBaseM") {
        prefix <- "S5"
    }

    x <- paste(prefix, parts[2], sep = "_")

    dat$Scenario <- x
    return(dat)
})

VacProtect_All <- do.call(rbind, VacProtect_All)
VacProtect_All$CI <- NULL

VacProtect_All <- VacProtect_All %>%
    filter(type == "TotalProtectHosp")

#----------------------------------------------------------------
### Infection Proportion
#----------------------------------------------------------------
VacProtectInfe_All <- lapply(FindVarableList, \(x) {
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
    } else if (parts[1] == "SBaseM") {
        prefix <- "S5"
    }

    x <- paste(prefix, parts[2], sep = "_")

    dat$Scenario <- x
    return(dat)
})

VacProtectInfe_All <- do.call(rbind, VacProtectInfe_All)
VacProtectInfe_All$CI <- NULL

VacProtectInfe_All <- VacProtectInfe_All %>%
    filter(type == "TotalProtectInfe")


### Bind the two dataframes
VacProtect_All <- rbind(VacProtect_All, VacProtectInfe_All) %>%
    mutate(
        Scenario = factor(Scenario),
        type = ifelse(type == "TotalProtectHosp", "Hospitalisation", "Infection")
    )

### Plot Fig 2a2 and 2b2
VacProtectProp_All <- VacProtect_All %>%
    filter(Scenario %in% c(
        "S1_E7C8A", "S1_E7C8B", "S1_E7C8C",
        "S5_E7C8A", "S5_E7C8B", "S5_E7C8C",
        "S2_E7C8A", "S2_E7C8B", "S2_E7C8C"
    )) %>%
    mutate(
        class = substr(Scenario, 1, 2),
        class = factor(class, levels = c("S2", "S5", "S1")),
        Scenario = factor(Scenario,
            levels = c(
                "S1_E7C8A", "S5_E7C8A", "S2_E7C8A",
                "S1_E7C8B", "S5_E7C8B", "S2_E7C8B",
                "S1_E7C8C", "S5_E7C8C", "S2_E7C8C"
            ),
            labels = c(
                "0-11m", "0-11m", "0-11m",
                "0-23m", "0-23m", "0-23m",
                "0-4y", "0-4y", "0-4y"
            )
        )
    )

VacProtectProp_Fig <- ggplot(VacProtectProp_All, aes(x = median, y = Scenario, fill = class)) +
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
    scale_y_discrete(limits = rev(levels(VacProtectProp_All$Scenario))) +
    scale_x_continuous(limits = function(x) c(min(x), max(x) * 1.15)) +
    facet_wrap(~type, scales = "free_x") +
    labs(
        y = "Programmes",
        x = "Proportion reduction (%)"
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
        plot.margin = margin(r = 10),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank()
    ) +
    geom_text(aes(x = uci, label = round(median, 1)),
        position = position_dodge(width = 0.6),
        hjust = -0.3,
        size = 3.5,
        fontface = "bold"
    )
ggsave(VacProtectProp_Fig, file = paste0(FilePath, "2a2.VacProtectProp.pdf"), width = 8, height = 3)


### For appendix: all scenarios
VacProtectProp_All_Fig <- ggplot(VacProtect_All, aes(x = median, y = Scenario, color = type)) +
    geom_point(alpha = 0.5, position = position_dodge(width = 0.7)) +
    geom_errorbar(aes(xmin = lci, xmax = uci),
        width = 0.4, position = position_dodge(width = 0.7)
    ) +
    scale_color_manual(values = c(
        "#074ba9", "#be134c"
    )) +
    theme_bw() +
    geom_hline(yintercept = seq(0.5, 92.5, 1), color = "gray75", linetype = "longdash") +
    geom_hline(yintercept = seq(6.5, 92.5, 6), color = "gray40", linetype = "longdash") +
    geom_hline(yintercept = seq(18.5, 92.5, 18), color = "gray10", linetype = "solid") +
    geom_vline(xintercept = 0, color = "gray85") +
    scale_y_discrete(limits = rev(levels(VacProtect_All$Scenario))) +
    labs(
        y = "Programmes",
        x = "Proportion reduction in all age groups (%)"
    ) +
    theme(
        axis.text = element_text(size = 6, face = "bold"),
        axis.title = element_text(size = 7, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.text.x = element_text(margin = margin(b = 5)),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "grey95"),
        strip.text = element_text(size = 7, face = "bold"),
        plot.margin = margin(l = 6, r = 6, b = 6)
    )

ggsave(VacProtectProp_All_Fig, file = paste0(FilePath, "2a2.VacProtectProp_Appendix.pdf"), width = 6, height = 8)
