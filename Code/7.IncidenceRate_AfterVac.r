All_Variable <- ls()
FindVarable <- All_Variable[grep("^.+_E.C..$", All_Variable)]
FindVarableList <- as.list(FindVarable)

### Incidence rate after vaccination -------------------------------------------------
IR_Aftervac <- lapply(FindVarableList, \(x) {
    dat <- get(x)[["InfeRate"]] %>% as.data.frame()

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
    dat$age_group <- rownames(dat)
    return(dat)
})

IR_Aftervac <- do.call(rbind, IR_Aftervac)
colnames(IR_Aftervac) <- c("lci", "median", "uci", "Scenario", "age_group")

IR_Aftervac <- IR_Aftervac %>%
    mutate(age_group = factor(age_group,
        levels = c(
            "0-2m", "3-5m", "6-11m", "1-<2y", "2-<5y", "5-<19y",
            "19-<60y", "60-64y", "65-69y", "70-74y", "75y"
        ), labels = c(
            "0-3 M", "3-6 M", "6-12 M", "1-2 Y", "2-5 Y", "5-20 Y",
            "20-60 Y", "60-65 Y", "65-70 Y", "70-75 Y", "75+ Y"
        )
    ))

IR_Aftervac_S1 <- IR_Aftervac %>%
    filter(Scenario %in% grep("^S1_", unique(IR_Aftervac$Scenario), value = TRUE))

IR_Aftervac_S2 <- IR_Aftervac %>%
    filter(Scenario %in% grep("^S2_", unique(IR_Aftervac$Scenario), value = TRUE))

IR_Aftervac_S3 <- IR_Aftervac %>%
    filter(Scenario %in% grep("^S3_", unique(IR_Aftervac$Scenario), value = TRUE))

IR_Aftervac_S4 <- IR_Aftervac %>%
    filter(Scenario %in% grep("^S4_", unique(IR_Aftervac$Scenario), value = TRUE))


IR_Main_S1 <- IR_Main %>%
    filter(Scenario == "1.0 & 1.0") %>%
    slice(rep(1:n(), each = 18)) %>%
    mutate(Scenario = rep(unique(IR_Aftervac_S1$Scenario), times = 11))

IR_Main_S2 <- IR_Main %>%
    filter(Scenario == "0.4 & 0.4") %>%
    slice(rep(1:n(), each = 18)) %>%
    mutate(Scenario = rep(unique(IR_Aftervac_S2$Scenario), times = 11))

IR_Main_S3 <- IR_Main %>%
    filter(Scenario == "0.4 & 0.3") %>%
    slice(rep(1:n(), each = 18)) %>%
    mutate(Scenario = rep(unique(IR_Aftervac_S3$Scenario), times = 11))

IR_Main_S4 <- IR_Main %>%
    filter(Scenario == "0.4 & 0.2") %>%
    slice(rep(1:n(), each = 18)) %>%
    mutate(Scenario = rep(unique(IR_Aftervac_S4$Scenario), times = 11))




### Plot incidence rate after vaccination in 0.1 & 0.1 scenarios -----------------------
IR_Aftervac_S1_Fig <- ggplot() +
    geom_line(data = IR_Main_S1, aes(x = age_group, y = median, group = Scenario), linewidth = 0.5) +
    geom_ribbon(data = IR_Main_S1, aes(x = age_group, ymin = lci, ymax = uci, group = Scenario), alpha = 0.4) +
    geom_point(data = IR_Main_S1, aes(x = age_group, y = median, group = Scenario), size = 0.5) +
    geom_line(data = IR_Aftervac_S1, aes(x = age_group, y = median, group = Scenario, color = Scenario), alpha = 0.5) +
    geom_ribbon(data = IR_Aftervac_S1, aes(
        x = age_group, ymin = lci, ymax = uci,
        group = Scenario, fill = Scenario
    ), alpha = 0.4) +
    geom_point(data = IR_Aftervac_S1, aes(x = age_group, y = median, colour = Scenario), alpha = 0.5, size = 0.7) +
    theme_bw() +
    labs(
        x = "Age group",
        y = "Incidence rate after vaccination",
        color = "Scenario", fill = "Scenario"
    ) +
    scale_x_discrete(
        breaks = c(
            "0-3 M", "3-6 M", "6-12 M", "1-2 Y", "2-5 Y", "5-20 Y",
            "20-60 Y", "60-65 Y", "65-70 Y", "70-75 Y", "75+ Y"
        )
    ) +
    scale_y_continuous(limits = c(0, 0.7)) +
    # scale_fill_manual(values = c(
    #     "#64beef", "#2ca02c", "#ff7f0e", "#9467bd"
    # )) +
    # scale_color_manual(values = c(
    #     "#64beef", "#2ca02c", "#ff7f0e", "#9467bd"
    # )) +
    facet_wrap(~Scenario, scales = "free_y", ncol = 3) +
    theme(
        axis.text.y = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 18, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.text.x = element_text(size = 12, face = "bold", margin = margin(b = 5), angle = 45, vjust = 1, hjust = 1),
        legend.position = "none",
        strip.background = element_rect(fill = "grey95"),
        strip.text = element_text(size = 14, face = "bold"),
    )
ggsave(IR_Aftervac_S1_Fig, file = paste0(FilePath, "IR_Aftervac_S1.pdf"), width = 16, height = 18)


### Plot incidence rate after vaccination in 0.4 & 0.4 scenarios -----------------------
IR_Aftervac_S2_Fig <- ggplot() +
    geom_line(data = IR_Main_S2, aes(x = age_group, y = median, group = Scenario), linewidth = 0.5) +
    geom_ribbon(data = IR_Main_S2, aes(x = age_group, ymin = lci, ymax = uci, group = Scenario), alpha = 0.4) +
    geom_point(data = IR_Main_S2, aes(x = age_group, y = median, group = Scenario), size = 0.5) +
    geom_line(data = IR_Aftervac_S2, aes(x = age_group, y = median, group = Scenario, color = Scenario), alpha = 0.5) +
    geom_ribbon(data = IR_Aftervac_S2, aes(
        x = age_group, ymin = lci, ymax = uci,
        group = Scenario, fill = Scenario
    ), alpha = 0.4) +
    geom_point(data = IR_Aftervac_S2, aes(x = age_group, y = median, colour = Scenario), alpha = 0.5, size = 0.7) +
    theme_bw() +
    labs(
        x = "Age group",
        y = "Incidence rate after vaccination",
        color = "Scenario", fill = "Scenario"
    ) +
    scale_x_discrete(
        breaks = c(
            "0-3 M", "3-6 M", "6-12 M", "1-2 Y", "2-5 Y", "5-20 Y",
            "20-60 Y", "60-65 Y", "65-70 Y", "70-75 Y", "75+ Y"
        )
    ) +
    scale_y_continuous(limits = c(0, 0.7)) +
    # scale_fill_manual(values = c(
    #     "#64beef", "#2ca02c", "#ff7f0e", "#9467bd"
    # )) +
    # scale_color_manual(values = c(
    #     "#64beef", "#2ca02c", "#ff7f0e", "#9467bd"
    # )) +
    facet_wrap(~Scenario, scales = "free_y", ncol = 3) +
    theme(
        axis.text.y = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 18, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.text.x = element_text(size = 12, face = "bold", margin = margin(b = 5), angle = 45, vjust = 1, hjust = 1),
        legend.position = "none",
        strip.background = element_rect(fill = "grey95"),
        strip.text = element_text(size = 14, face = "bold"),
    )
ggsave(IR_Aftervac_S2_Fig, file = paste0(FilePath, "IR_Aftervac_S2.pdf"), width = 16, height = 18)


### Plot incidence rate after vaccination in 0.4 & 0.3 scenarios -----------------------
IR_Aftervac_S3_Fig <- ggplot() +
    geom_line(data = IR_Main_S3, aes(x = age_group, y = median, group = Scenario), linewidth = 0.5) +
    geom_ribbon(data = IR_Main_S3, aes(x = age_group, ymin = lci, ymax = uci, group = Scenario), alpha = 0.4) +
    geom_point(data = IR_Main_S3, aes(x = age_group, y = median, group = Scenario), size = 0.5) +
    geom_line(data = IR_Aftervac_S3, aes(x = age_group, y = median, group = Scenario, color = Scenario), alpha = 0.5) +
    geom_ribbon(data = IR_Aftervac_S3, aes(
        x = age_group, ymin = lci, ymax = uci,
        group = Scenario, fill = Scenario
    ), alpha = 0.4) +
    geom_point(data = IR_Aftervac_S3, aes(x = age_group, y = median, colour = Scenario), alpha = 0.5, size = 0.7) +
    theme_bw() +
    labs(
        x = "Age group",
        y = "Incidence rate after vaccination",
        color = "Scenario", fill = "Scenario"
    ) +
    scale_x_discrete(
        breaks = c(
            "0-3 M", "3-6 M", "6-12 M", "1-2 Y", "2-5 Y", "5-20 Y",
            "20-60 Y", "60-65 Y", "65-70 Y", "70-75 Y", "75+ Y"
        )
    ) +
    scale_y_continuous(limits = c(0, 0.7)) +
    # scale_fill_manual(values = c(
    #     "#64beef", "#2ca02c", "#ff7f0e", "#9467bd"
    # )) +
    # scale_color_manual(values = c(
    #     "#64beef", "#2ca02c", "#ff7f0e", "#9467bd"
    # )) +
    facet_wrap(~Scenario, scales = "free_y", ncol = 3) +
    theme(
        axis.text.y = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 18, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.text.x = element_text(size = 12, face = "bold", margin = margin(b = 5), angle = 45, vjust = 1, hjust = 1),
        legend.position = "none",
        strip.background = element_rect(fill = "grey95"),
        strip.text = element_text(size = 14, face = "bold"),
    )
ggsave(IR_Aftervac_S3_Fig, file = paste0(FilePath, "IR_Aftervac_S3.pdf"), width = 16, height = 18)


### Plot incidence rate after vaccination in 0.4 & 0.2 scenarios -----------------------
IR_Aftervac_S4_Fig <- ggplot() +
    geom_line(data = IR_Main_S4, aes(x = age_group, y = median, group = Scenario), linewidth = 0.5) +
    geom_ribbon(data = IR_Main_S4, aes(x = age_group, ymin = lci, ymax = uci, group = Scenario), alpha = 0.4) +
    geom_point(data = IR_Main_S4, aes(x = age_group, y = median, group = Scenario), size = 0.5) +
    geom_line(data = IR_Aftervac_S4, aes(x = age_group, y = median, group = Scenario, color = Scenario), alpha = 0.5) +
    geom_ribbon(data = IR_Aftervac_S4, aes(
        x = age_group, ymin = lci, ymax = uci,
        group = Scenario, fill = Scenario
    ), alpha = 0.4) +
    geom_point(data = IR_Aftervac_S4, aes(x = age_group, y = median, colour = Scenario), alpha = 0.5, size = 0.7) +
    theme_bw() +
    labs(
        x = "Age group",
        y = "Incidence rate after vaccination",
        color = "Scenario", fill = "Scenario"
    ) +
    scale_x_discrete(
        breaks = c(
            "0-3 M", "3-6 M", "6-12 M", "1-2 Y", "2-5 Y", "5-20 Y",
            "20-60 Y", "60-65 Y", "65-70 Y", "70-75 Y", "75+ Y"
        )
    ) +
    scale_y_continuous(limits = c(0, 0.7)) +
    # scale_fill_manual(values = c(
    #     "#64beef", "#2ca02c", "#ff7f0e", "#9467bd"
    # )) +
    # scale_color_manual(values = c(
    #     "#64beef", "#2ca02c", "#ff7f0e", "#9467bd"
    # )) +
    facet_wrap(~Scenario, scales = "free_y", ncol = 3) +
    theme(
        axis.text.y = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 18, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.text.x = element_text(size = 12, face = "bold", margin = margin(b = 5), angle = 45, vjust = 1, hjust = 1),
        legend.position = "none",
        strip.background = element_rect(fill = "grey95"),
        strip.text = element_text(size = 14, face = "bold"),
    )
ggsave(IR_Aftervac_S4_Fig, file = paste0(FilePath, "IR_Aftervac_S4.pdf"), width = 16, height = 18)
