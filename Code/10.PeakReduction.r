PeakReductionDat <- lapply(FindVarableList, \(x) {
    PeakReduction <- get(x)[["PeakReduction"]] %>% t()
    colnames(PeakReduction) <- c("lci", "median", "uci")
    PeakReduction <- PeakReduction %>% as.data.frame()

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

    PeakReduction$Scenario <- x
    return(PeakReduction)
})
PeakReductionDat <- do.call(rbind, PeakReductionDat) %>%
    filter(!startsWith(Scenario, "S5"))


PeakReductionDat <- PeakReductionDat %>%
    mutate(
        Class1 = paste0(substr(Scenario, 1, 2), substr(Scenario, 8, 8)),
        Class1 = factor(Class1),
        Class2 = substr(Scenario, 4, 7),
        Class2 = case_when(
            Class2 == "E6C6" ~ "Eff: 60/70%\nCov: 60%",
            Class2 == "E6C7" ~ "Eff: 60/70%\nCov: 70%",
            Class2 == "E6C8" ~ "Eff: 60/70%\nCov: 80%",
            Class2 == "E7C6" ~ "Eff: 70/80%\nCov: 60%",
            Class2 == "E7C7" ~ "Eff: 70/80%\nCov: 70%",
            Class2 == "E7C8" ~ "Eff: 70/80%\nCov: 80%",
            Class2 == "E8C6" ~ "Eff: 80/90%\nCov: 60%",
            Class2 == "E8C7" ~ "Eff: 80/90%\nCov: 70%",
            Class2 == "E8C8" ~ "Eff: 80/90%\nCov: 80%"
        )
    )

PeakReductionDat$Class1 <- str_replace_all(
    PeakReductionDat$Class1,
    c(
        "A$" = "(0-11m)",
        "B$" = "(0-23m)",
        "C$" = "(0-4y)"
    )
)

PeakReductionDat$Class1 <- str_replace_all(
    PeakReductionDat$Class1,
    c(
        "^S1" = "Base ",
        "^S2" = "Sce.1 ",
        "^S3" = "Sce.2 ",
        "^S4" = "Sce.3 "
    )
)

PeakReduction_Fig <- ggplot(PeakReductionDat, aes(y = Class1, x = Class2)) +
    geom_tile(aes(fill = median), color = "white") +
    geom_text(aes(label = sprintf("%.1f", median)), color = "white", size = 4, fontface = "bold") +
    scale_fill_gradient2(low = "#ce1c4e", mid = "#f69c73", high = "#f9d7c0", midpoint = 35, na.value = "grey90") +
    scale_y_discrete(limits = rev(levels(PeakReductionDat$Class1))) +
    theme_minimal() +
    labs(
        x = "Immunisation efficacy and coverage",
        y = "Susceptibility scenario and\nimmunised age groups"
    ) +
    theme(
        axis.text = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        legend.title = element_blank(), # element_text(size = 10, face = "bold"),
        axis.text.x = element_text(margin = margin(b = 10)),
        axis.text.y = element_text(margin = margin(l = 10)),
        panel.grid = element_blank(),
        legend.position = "right"
    )

ggsave(PeakReduction_Fig, file = paste0(FilePath, "PeakReduction_Fig_Appendix.pdf"), width = 8, height = 6)



PeakReductionDat_Main <- PeakReductionDat %>%
    filter(grepl("^S1|^S2", Scenario)) %>%
    mutate(Class1 = factor(Class1))

PeakReductionDat_Main_Fig <- ggplot(PeakReductionDat_Main, aes(y = Class1, x = Class2)) +
    geom_tile(aes(fill = median), color = "white") +
    geom_text(aes(label = sprintf("%.1f", median)), color = "white", size = 8, fontface = "bold") +
    scale_y_discrete(limits = rev(levels(PeakReductionDat_Main$Class1))) +
    scale_fill_gradient2(low = "#ce1c4e", mid = "#f69c73", high = "#f9d7c0", midpoint = 35, na.value = "grey90") +
    theme_minimal() +
    labs(
        x = "Immunisation efficacy and coverage",
        y = "Susceptibility scenario and\nimmunised age groups"
    ) +
    theme(
        axis.text = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 20, face = "bold"),
        legend.title = element_blank(), # element_text(size = 10, face = "bold"),
        axis.text.x = element_text(margin = margin(b = 10)),
        axis.text.y = element_text(margin = margin(l = 10)),
        panel.grid = element_blank(),
        legend.position = "right",
    )

ggsave(PeakReductionDat_Main_Fig, file = paste0(FilePath, "PeakReduction_Fig.pdf"), width = 11, height = 8)
