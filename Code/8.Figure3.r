### Nothine new here, just plotting the figure 3

### Get the mark for all the S44 scenarios ---------------------------------------
S2_label <- grep("^S2_*", VacProtectNet_All$Scenario, value = TRUE)

### Plot Fig 3a ------------------------------------------------------------------
Fig3a1_dat <- VacProtectNet_All %>%
    filter(Scenario %in% S2_label) %>%
    mutate(
        class = substr(Scenario, 8, 8),
        class = factor(class, levels = c("C", "B", "A")),
        Scenario = factor(Scenario),
        Scenario = substr(Scenario, 4, 7),
        Scenario = case_when(
            Scenario == "E6C6" ~ "Eff: 60%; Cov: 60%",
            Scenario == "E6C7" ~ "Eff: 60%; Cov: 70%",
            Scenario == "E6C8" ~ "Eff: 60%; Cov: 80%",
            Scenario == "E7C6" ~ "Eff: 70%; Cov: 60%",
            Scenario == "E7C7" ~ "Eff: 70%; Cov: 70%",
            Scenario == "E7C8" ~ "Eff: 70%; Cov: 80%",
            Scenario == "E8C6" ~ "Eff: 80%; Cov: 60%",
            Scenario == "E8C7" ~ "Eff: 80%; Cov: 70%",
            Scenario == "E8C8" ~ "Eff: 80%; Cov: 80%"
        )
    )

Fig3a1 <- ggplot(Fig3a1_dat, aes(x = median, y = Scenario, fill = class)) +
    geom_bar(stat = "identity", alpha = 0.9, position = position_dodge(width = 0.7), width = 0.7) +
    geom_errorbar(aes(xmin = lci, xmax = uci),
        width = 0.35, position = position_dodge(width = 0.7)
    ) +
    scale_fill_manual(values = c(
        "#a80000", "#a4ac50", "#1b7bab"
    )) +
    theme_classic() +
    # geom_hline(yintercept = seq(0.5, 37.5, 1), color = "gray75", linetype = "longdash") +
    geom_hline(yintercept = seq(2.5, 37.5, 2), color = "gray10", linetype = "longdash") +
    # geom_hline(yintercept = seq(0.5, 37.5, 6), color = "gray10", linetype = "solid") +
    scale_y_discrete(limits = rev(levels(Fig3a1_dat$Scenario))) +
    scale_x_continuous(limits = function(x) c(min(x), max(x) * 1.15)) +
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
        axis.text.x = element_text(margin = margin(b = 5)),
        legend.position = "none",
        strip.text = element_text(size = 14, face = "bold"),
        strip.background = element_blank(),
        plot.margin = margin(r = 20),
        panel.spacing = unit(1.5, "lines")
    ) +
    geom_text(aes(x = uci, label = round(median, 1)),
        position = position_dodge(width = 0.7),
        hjust = -0.2,
        size = 3.5,
        fontface = "bold"
    )
ggsave(Fig3a1, file = paste0(FilePath, "3a1.NetProtectAll.pdf"), width = 10, height = 4)


### Plot Fig 3a2 ------------------------------------------------------------------
Fig3a2_dat <- VacProtect_All %>%
    filter(Scenario %in% S2_label) %>%
    mutate(
        class = substr(Scenario, 8, 8),
        class = factor(class, levels = c("C", "B", "A")),
        Scenario = factor(Scenario),
        Scenario = substr(Scenario, 4, 7),
        Scenario = case_when(
            Scenario == "E6C6" ~ "Eff: 60%; Cov: 60%",
            Scenario == "E6C7" ~ "Eff: 60%; Cov: 70%",
            Scenario == "E6C8" ~ "Eff: 60%; Cov: 80%",
            Scenario == "E7C6" ~ "Eff: 70%; Cov: 60%",
            Scenario == "E7C7" ~ "Eff: 70%; Cov: 70%",
            Scenario == "E7C8" ~ "Eff: 70%; Cov: 80%",
            Scenario == "E8C6" ~ "Eff: 80%; Cov: 60%",
            Scenario == "E8C7" ~ "Eff: 80%; Cov: 70%",
            Scenario == "E8C8" ~ "Eff: 80%; Cov: 80%"
        )
    )

Fig3a2 <- ggplot(Fig3a2_dat, aes(x = median, y = Scenario, fill = class)) +
    geom_bar(stat = "identity", alpha = 0.9, position = position_dodge(width = 0.7), width = 0.7) +
    geom_errorbar(aes(xmin = lci, xmax = uci),
        width = 0.35, position = position_dodge(width = 0.7)
    ) +
    scale_fill_manual(values = c(
        "#a80000", "#a4ac50", "#1b7bab"
    )) +
    theme_classic() +
    # geom_hline(yintercept = seq(0.5, 37.5, 1), color = "gray75", linetype = "longdash") +
    geom_hline(yintercept = seq(2.5, 37.5, 2), color = "gray10", linetype = "longdash") +
    # geom_hline(yintercept = seq(0.5, 37.5, 6), color = "gray10", linetype = "solid") +
    scale_y_discrete(limits = rev(levels(Fig3a2_dat$Scenario))) +
    scale_x_continuous(limits = function(x) c(min(x), max(x) * 1.05)) +
    facet_wrap(~type, scales = "free_x") +
    labs(
        y = "Programmes",
        x = "Proportion reduction in all age groups (%)"
    ) +
    theme(
        axis.text = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(margin = margin(r = 10)),
        axis.title = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.text.x = element_text(margin = margin(b = 5)),
        legend.position = "none",
        strip.text = element_text(size = 14, face = "bold"),
        strip.background = element_blank(),
        plot.margin = margin(r = 20),
        panel.spacing = unit(1.5, "lines"),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank()
    ) +
    geom_text(aes(x = uci, label = round(median, 1)),
        position = position_dodge(width = 0.7),
        hjust = -0.3,
        size = 3.5,
        fontface = "bold"
    )
ggsave(Fig3a2, file = paste0(FilePath, "3a2.VacProtectProp.pdf"), width = 10, height = 4)


### Plot Fig 3b ------------------------------------------------------------------
Fig3b_dat <- AvertHospPerVac %>%
    filter(Scenario %in% S2_label) %>%
    mutate(
        class = substr(Scenario, 8, 8),
        class = factor(class, levels = c("C", "B", "A")),
        Scenario = factor(Scenario),
        Scenario = substr(Scenario, 4, 7),
        Scenario = case_when(
            Scenario == "E6C6" ~ "Eff: 60%; Cov: 60%",
            Scenario == "E6C7" ~ "Eff: 60%; Cov: 70%",
            Scenario == "E6C8" ~ "Eff: 60%; Cov: 80%",
            Scenario == "E7C6" ~ "Eff: 70%; Cov: 60%",
            Scenario == "E7C7" ~ "Eff: 70%; Cov: 70%",
            Scenario == "E7C8" ~ "Eff: 70%; Cov: 80%",
            Scenario == "E8C6" ~ "Eff: 80%; Cov: 60%",
            Scenario == "E8C7" ~ "Eff: 80%; Cov: 70%",
            Scenario == "E8C8" ~ "Eff: 80%; Cov: 80%"
        )
    )

Fig3b <- ggplot(Fig3b_dat, aes(x = median, y = Scenario, fill = class)) +
    geom_bar(stat = "identity", alpha = 0.9, position = position_dodge(width = 0.7), width = 0.7) +
    geom_errorbar(aes(xmin = lci, xmax = uci),
        width = 0.35, position = position_dodge(width = 0.7)
    ) +
    scale_fill_manual(values = c(
        "#a80000", "#a4ac50", "#1b7bab"
    )) +
    theme_classic() +
    # geom_hline(yintercept = seq(0.5, 37.5, 1), color = "gray75", linetype = "longdash") +
    geom_hline(yintercept = seq(2.5, 37.5, 2), color = "gray10", linetype = "longdash") +
    # geom_hline(yintercept = seq(0.5, 37.5, 6), color = "gray10", linetype = "solid") +
    scale_y_discrete(limits = rev(levels(Fig3b_dat$Scenario))) +
    scale_x_continuous(limits = function(x) c(min(x), max(x) * 1.05)) +
    # scale_x_continuous(limits = c(5, 15)) +
    labs(
        y = "Programmes",
        x = "Number of people needed to vaccinate"
    ) +
    theme(
        axis.text = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(margin = margin(r = 10)),
        axis.title = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.text.x = element_text(margin = margin(b = 5)),
        legend.position = "none",
        strip.text = element_text(size = 14, face = "bold"),
        strip.background = element_blank(),
        plot.margin = margin(r = 20),
        panel.spacing = unit(1.5, "lines")
    ) +
    geom_text(aes(x = uci, label = round(median, 1)),
        position = position_dodge(width = 0.7),
        hjust = -0.2,
        size = 3.5,
        fontface = "bold"
    )
ggsave(Fig3b, file = paste0(FilePath, "3b.NNV.pdf"), width = 10, height = 3.8)


### Plot Fig 3c1 ------------------------------------------------------------------
Fig3c1_dat <- NetProtectHosp %>%
    filter(Scenario %in% S2_label) %>%
    mutate(
        class = substr(Scenario, 8, 8),
        class = factor(class, levels = c("C", "B", "A")),
        Scenario = factor(Scenario),
        Scenario = substr(Scenario, 4, 7),
        Scenario = case_when(
            Scenario == "E6C6" ~ "Eff: 60%; Cov: 60%",
            Scenario == "E6C7" ~ "Eff: 60%; Cov: 70%",
            Scenario == "E6C8" ~ "Eff: 60%; Cov: 80%",
            Scenario == "E7C6" ~ "Eff: 70%; Cov: 60%",
            Scenario == "E7C7" ~ "Eff: 70%; Cov: 70%",
            Scenario == "E7C8" ~ "Eff: 70%; Cov: 80%",
            Scenario == "E8C6" ~ "Eff: 80%; Cov: 60%",
            Scenario == "E8C7" ~ "Eff: 80%; Cov: 70%",
            Scenario == "E8C8" ~ "Eff: 80%; Cov: 80%"
        )
    )

Fig3c1 <- ggplot(Fig3c1_dat, aes(x = median, y = Scenario, fill = class)) +
    geom_bar(stat = "identity", alpha = 0.9, position = position_dodge(width = 0.7), width = 0.7) +
    geom_errorbar(aes(xmin = lci, xmax = uci),
        width = 0.35, position = position_dodge(width = 0.7)
    ) +
    scale_fill_manual(values = c(
        "#a80000", "#a4ac50", "#1b7bab"
    )) +
    theme_classic() +
    # geom_hline(yintercept = seq(0.5, 37.5, 1), color = "gray75", linetype = "longdash") +
    geom_hline(yintercept = seq(2.5, 37.5, 2), color = "gray10", linetype = "longdash") +
    # geom_hline(yintercept = seq(0.5, 37.5, 6), color = "gray10", linetype = "solid") +
    scale_y_discrete(limits = rev(levels(Fig3c1_dat$Scenario))) +
    scale_x_continuous(limits = function(x) c(min(x), max(x) * 1.1)) +
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
        axis.text.x = element_text(margin = margin(b = 5)),
        legend.position = "none",
        strip.text = element_text(size = 14, face = "bold"),
        strip.background = element_blank(),
        plot.margin = margin(r = 20),
        panel.spacing = unit(1.5, "lines")
    ) +
    geom_text(aes(x = uci, label = round(median, 1)),
        position = position_dodge(width = 0.7),
        hjust = -0.2,
        size = 3.5,
        fontface = "bold"
    )
ggsave(Fig3c1, file = paste0(FilePath, "3c1.NetProtect_Hosp.pdf"), width = 10, height = 4)


### Plot Fig 3c2 ------------------------------------------------------------------
Fig3c2_dat <- PropHosp %>%
    filter(Scenario %in% S2_label) %>%
    mutate(
        class = substr(Scenario, 8, 8),
        class = factor(class, levels = c("C", "B", "A")),
        Scenario = factor(Scenario),
        Scenario = substr(Scenario, 4, 7),
        Scenario = case_when(
            Scenario == "E6C6" ~ "Eff: 60%; Cov: 60%",
            Scenario == "E6C7" ~ "Eff: 60%; Cov: 70%",
            Scenario == "E6C8" ~ "Eff: 60%; Cov: 80%",
            Scenario == "E7C6" ~ "Eff: 70%; Cov: 60%",
            Scenario == "E7C7" ~ "Eff: 70%; Cov: 70%",
            Scenario == "E7C8" ~ "Eff: 70%; Cov: 80%",
            Scenario == "E8C6" ~ "Eff: 80%; Cov: 60%",
            Scenario == "E8C7" ~ "Eff: 80%; Cov: 70%",
            Scenario == "E8C8" ~ "Eff: 80%; Cov: 80%"
        )
    )

Fig3c2 <- ggplot(Fig3c2_dat, aes(x = median, y = Scenario, fill = class)) +
    geom_bar(stat = "identity", alpha = 0.9, position = position_dodge(width = 0.7), width = 0.7) +
    geom_errorbar(aes(xmin = lci, xmax = uci),
        width = 0.35, position = position_dodge(width = 0.7)
    ) +
    scale_fill_manual(values = c(
        "#a80000", "#a4ac50", "#1b7bab"
    )) +
    theme_classic() +
    # geom_hline(yintercept = seq(0.5, 37.5, 1), color = "gray75", linetype = "longdash") +
    geom_hline(yintercept = seq(2.5, 37.5, 2), color = "gray10", linetype = "longdash") +
    # geom_hline(yintercept = seq(0.5, 37.5, 6), color = "gray10", linetype = "solid") +
    scale_y_discrete(limits = rev(levels(Fig3c2_dat$Scenario))) +
    scale_x_continuous(limits = function(x) c(min(x), max(x) * 1.1)) +
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
        axis.text.x = element_text(margin = margin(b = 5)),
        legend.position = "none",
        strip.text = element_text(size = 14, face = "bold"),
        strip.background = element_blank(),
        plot.margin = margin(r = 20),
        panel.spacing = unit(1.5, "lines"),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank()
    ) +
    geom_text(aes(x = uci, label = round(median, 1)),
        position = position_dodge(width = 0.7),
        hjust = -0.2,
        size = 3.5,
        fontface = "bold"
    )
ggsave(Fig3c2, file = paste0(FilePath, "3c2.PropHosp.pdf"), width = 10, height = 4)
