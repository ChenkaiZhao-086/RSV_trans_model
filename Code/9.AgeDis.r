Plot.AgeDistribution <- function(dat) {
    HospDat <- dat[["NetProtectHosp"]]
    HospDat <- HospDat %>% as.data.frame()
    HospDat$class <- "Hosp"

    InfeDat <- dat[["NetProtectInfe"]]
    InfeDat <- InfeDat %>% as.data.frame()
    InfeDat$class <- "Infe"

    BindDat <- rbind(HospDat, InfeDat)

    DatName <- as.character(substitute(dat))
    DatName <- strsplit(DatName, "_")[[1]]
    if (DatName[1] == "S44") {
        prefix <- "S2"
    } else if (DatName[1] == "S43") {
        prefix <- "S3"
    } else if (DatName[1] == "S42") {
        prefix <- "S4"
    } else if (DatName[1] == "SBase") {
        prefix <- "S1"
    } else if (DatName[1] == "SBaseM") {
        prefix <- "S5"
    }
    DatName <- paste(prefix, DatName[2], sep = "_")

    BindDat$Scenario <- DatName

    BindDat <- BindDat %>%
        mutate(
            age_group = factor(age_group, levels = c(rev(unique(BindDat$age_group)))),
            class = factor(class, levels = c("Infe", "Hosp"))
        )
    return(BindDat)
}


AgeDis_All <- lapply(FindVarableList, \(x) {
    dat <- eval(parse(text = paste0("Plot.AgeDistribution(", x, ")")))
    return(dat)
})
AgeDis_All <- do.call(rbind, AgeDis_All)


AgeDis_All <- AgeDis_All %>%
    as.data.frame() %>%
    mutate(
        age_group = factor(age_group, levels = c(unique(AgeDis_All$age_group))),
        class = factor(class, levels = c("Infe", "Hosp"), labels = c("Infection", "Hospitalisation"))
    )

AgeDis_S1 <- AgeDis_All %>%
    filter(Scenario %in% grep("^S1_", unique(AgeDis_All$Scenario), value = TRUE))

AgeDis_S2 <- AgeDis_All %>%
    filter(Scenario %in% grep("^S2_", unique(AgeDis_All$Scenario), value = TRUE))

AgeDis_S3 <- AgeDis_All %>%
    filter(Scenario %in% grep("^S3_", unique(AgeDis_All$Scenario), value = TRUE))

AgeDis_S4 <- AgeDis_All %>%
    filter(Scenario %in% grep("^S4_", unique(AgeDis_All$Scenario), value = TRUE))



AgeDis_S1_Fig <- ggplot(AgeDis_S1, aes(x = age_group, y = median, fill = class)) +
    geom_col(colour = "#000000", alpha = 0.4) +
    theme_bw() +
    scale_fill_manual(values = rev(c(
        "#074ba9", "#be134c"
    ))) +
    # scale_x_discrete(limits = rev(levels(AgeDistribution_Main$age_group))) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    facet_wrap(Scenario ~ class, scales = "free", ncol = 6) +
    labs(
        x = NULL,
        y = NULL
    ) +
    theme(
        axis.text = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 8, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.text.x = element_text(margin = margin(b = 5), angle = 45, vjust = 1, hjust = 1),
        legend.position = "none",
        panel.spacing = unit(0, "lines"),
        strip.placement = "outside",
        strip.background = element_rect(fill = "grey95"),
        strip.text = element_text(size = 12, face = "bold")
    )
ggsave(AgeDis_S1_Fig, file = paste0(FilePath, "AgeDis_S1.pdf"), width = 28, height = 18)


AgeDis_S2_Fig <- ggplot(AgeDis_S2, aes(x = age_group, y = median, fill = class)) +
    geom_col(colour = "#000000", alpha = 0.4) +
    theme_bw() +
    scale_fill_manual(values = rev(c(
        "#074ba9", "#be134c"
    ))) +
    # scale_x_discrete(limits = rev(levels(AgeDistribution_Main$age_group))) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    facet_wrap(Scenario ~ class, scales = "free", ncol = 6) +
    labs(
        x = NULL,
        y = NULL
    ) +
    theme(
        axis.text = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 8, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.text.x = element_text(margin = margin(b = 5), angle = 45, vjust = 1, hjust = 1),
        legend.position = "none",
        panel.spacing = unit(0, "lines"),
        strip.placement = "outside",
        strip.background = element_rect(fill = "grey95"),
        strip.text = element_text(size = 12, face = "bold")
    )
ggsave(AgeDis_S2_Fig, file = paste0(FilePath, "AgeDis_S2.pdf"), width = 28, height = 18)


AgeDis_S3_Fig <- ggplot(AgeDis_S3, aes(x = age_group, y = median, fill = class)) +
    geom_col(colour = "#000000", alpha = 0.4) +
    theme_bw() +
    scale_fill_manual(values = rev(c(
        "#074ba9", "#be134c"
    ))) +
    # scale_x_discrete(limits = rev(levels(AgeDistribution_Main$age_group))) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    facet_wrap(Scenario ~ class, scales = "free", ncol = 6) +
    labs(
        x = NULL,
        y = NULL
    ) +
    theme(
        axis.text = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 8, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.text.x = element_text(margin = margin(b = 5), angle = 45, vjust = 1, hjust = 1),
        legend.position = "none",
        panel.spacing = unit(0, "lines"),
        strip.placement = "outside",
        strip.background = element_rect(fill = "grey95"),
        strip.text = element_text(size = 12, face = "bold")
    )
ggsave(AgeDis_S3_Fig, file = paste0(FilePath, "AgeDis_S3.pdf"), width = 28, height = 18)

AgeDis_S4_Fig <- ggplot(AgeDis_S4, aes(x = age_group, y = median, fill = class)) +
    geom_col(colour = "#000000", alpha = 0.4) +
    theme_bw() +
    scale_fill_manual(values = rev(c(
        "#074ba9", "#be134c"
    ))) +
    # scale_x_discrete(limits = rev(levels(AgeDistribution_Main$age_group))) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    facet_wrap(Scenario ~ class, scales = "free", ncol = 6) +
    labs(
        x = NULL,
        y = NULL
    ) +
    theme(
        axis.text = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 8, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.text.x = element_text(margin = margin(b = 5), angle = 45, vjust = 1, hjust = 1),
        legend.position = "none",
        panel.spacing = unit(0, "lines"),
        strip.placement = "outside",
        strip.background = element_rect(fill = "grey95"),
        strip.text = element_text(size = 12, face = "bold")
    )
ggsave(AgeDis_S4_Fig, file = paste0(FilePath, "AgeDis_S4.pdf"), width = 28, height = 18)


# VacAgeDistribution_Fig <- ggplot(AgeDistribution_Main, aes(y = class, x = median, fill = age_group)) +
#   geom_bar(stat = "identity", position = "fill") +
#   scale_x_continuous(labels = scales::percent, expand = c(0, 0)) +
#   scale_y_discrete(expand = c(0, 0.6)) +
#   labs(x = "Number of cases averted", y = "Category", fill = "Age group") +
#   scale_fill_manual(values = rev(c(
#     "#a6cee3", "#2ca02c", "#9467bd", "#ff7f0e", "#1b9e77",
#     "#8c564b", "#17becf", "#e377c2", "#bcbd22", "#d95f02", "#7570b3"
#   ))) +
#   theme_bw() +
#   guides(fill = guide_legend(reverse = TRUE)) + # Reverse the legend order
#   coord_cartesian(xlim = c(0, 1.01), clip = "on") + # Set the x-axis limit
#   facet_wrap(~Scenario, ncol = 4, dir = "v", strip.position = "right") +
#   theme(
#     axis.text = element_text(size = 14, face = "bold"),
#     axis.title = element_text(size = 18, face = "bold"),
#     axis.title.y = element_text(margin = margin(r = 10)),
#     axis.text.x = element_text(margin = margin(b = 5)),
#     legend.title = element_blank(),
#     legend.text = element_text(size = 14, face = "bold"),
#     legend.position = "right",
#     panel.spacing = unit(0, "lines"),
#     strip.placement = "outside",
#     strip.background = element_blank(),
#     strip.text = element_text(size = 14, face = "bold")
#   )
# ggsave(VacAgeDistribution_Fig, file = paste0(FilePath, "VacAgeDistribution_Fig.pdf"), width = 22, height = 28)


# VacAgeDistribution_type2_Fig_v2 <- ggplot(AgeDistribution_Main, aes(x = age_group, y = median, fill = class)) +
#   geom_col(colour = "#000000", alpha = 0.4) +
#   theme_bw() +
#   scale_fill_manual(values = rev(c(
#     "#074ba9", "#be134c"
#   ))) +
#   scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
#   facet_wrap(~Scenario, ncol = 4, dir = "v", strip.position = "top", scales = "fixed") +
#   labs(
#     x = "Age group",
#     y = "Number of cases averted"
#   ) +
#   theme(
#     axis.text = element_text(size = 14, face = "bold"),
#     axis.title = element_text(size = 18, face = "bold"),
#     axis.title.y = element_text(margin = margin(r = 10)),
#     axis.text.x = element_text(margin = margin(b = 5), angle = 45, vjust = 1, hjust = 1),
#     legend.title = element_blank(),
#     legend.text = element_text(size = 14, face = "bold"),
#     panel.spacing = unit(0, "lines"),
#     strip.placement = "outside",
#     strip.background = element_rect(fill = "grey95"),
#     strip.text = element_text(size = 14, face = "bold")
#   )
# ggsave(VacAgeDistribution_type2_Fig_v2, file = paste0(FilePath, "AgeDistribution_v2.pdf"), width = 18, height = 10)
