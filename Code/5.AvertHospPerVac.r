All_Variable <- ls()
FindVarable <- All_Variable[grep("^.+_E.C..$", All_Variable)]
FindVarableList <- as.list(FindVarable)
# Number needed to vaccinate
### Proportion
AvertHospPerVac <- lapply(FindVarableList, \(x) {
    dat <- get(x)[["AvertHospPerVac"]]
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

AvertHospPerVac <- do.call(rbind, AvertHospPerVac)
colnames(AvertHospPerVac) <- c("lci", "median", "uci", "Scenario")

AvertHospPerVac <- AvertHospPerVac %>%
    mutate(Scenario = factor(Scenario))

### Plot Fig 2c
NNV_Main <- AvertHospPerVac %>%
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

NNV_Main_Fig <- ggplot(NNV_Main, aes(x = median, y = Scenario, fill = class)) +
    geom_bar(stat = "identity", alpha = 0.9, position = position_dodge(width = 0.6), width = 0.6) +
    geom_errorbar(aes(xmin = lci, xmax = uci), position = position_dodge(width = 0.6), width = 0.25) +
    theme_classic() +
    # geom_hline(yintercept = seq(0.5, 37.5, 1), color = "gray75", linetype = "longdash") +
    # geom_hline(yintercept = seq(3.5, 37.5, 6), color = "gray40", linetype = "longdash") +
    geom_hline(yintercept = seq(1.5, 37.5, 1), color = "gray10", linetype = "longdash") +
    scale_y_discrete(limits = rev(levels(NNV_Main$Scenario))) +
    scale_x_continuous(limits = function(x) c(min(x), max(x) * 1.05)) +
    scale_fill_manual(values = c(
        "#79AF97FF", "#DF8F44FF", "#00A1D5FF"
    )) +
    labs(
        y = "Programmes",
        x = "Number of people needed to vaccinate"
    ) +
    theme(
        axis.text = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(margin = margin(r = 10)),
        axis.title = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10)),
        legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(size = 14, face = "bold"),
        plot.margin = margin(r = 10)
    ) +
    geom_text(aes(x = uci, label = round(median, 1)),
        position = position_dodge(width = 0.6),
        hjust = -0.2,
        size = 3.5,
        fontface = "bold"
    )
ggsave(NNV_Main_Fig, file = paste0(FilePath, "2c.NNV.pdf"), width = 8, height = 3)


### For appendix: all scenarios
AvertHospPerVac_Fig <- ggplot(AvertHospPerVac, aes(x = median, y = Scenario)) +
    geom_point(alpha = 0.5, position = position_dodge(width = 0.7), colour = "#074ba9") +
    geom_errorbar(aes(xmin = lci, xmax = uci), width = 0.6, colour = "#074ba9") +
    theme_bw() +
    geom_hline(yintercept = seq(0.5, 92.5, 1), color = "gray75", linetype = "longdash") +
    geom_hline(yintercept = seq(6.5, 92.5, 6), color = "gray40", linetype = "longdash") +
    geom_hline(yintercept = seq(18.5, 92.5, 18), color = "gray10", linetype = "solid") +
    scale_y_discrete(limits = rev(levels(AvertHospPerVac$Scenario))) +
    labs(
        y = "Programmes",
        x = "Number of people needed to vaccinate"
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

ggsave(AvertHospPerVac_Fig, file = paste0(FilePath, "2c.NNV_appendix.pdf"), width = 6, height = 8)
