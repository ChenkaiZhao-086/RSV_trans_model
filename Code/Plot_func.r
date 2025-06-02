Plot.Appendix.2a <- function(Dat, xlab, Path, Title) {
    MaxValue <- summarise(Dat, max(uci), .by = type)
    type_values <- as.character(MaxValue$type)
    names(type_values) <- type_values

    x_scales_list <- lapply(type_values, \(x) {
        max <- MaxValue[which(MaxValue$type == x), 2]
        scale_x_continuous(limits = c(0, max * 1.1))
    })

    Fig <- Dat %>%
        filter(str_starts(Scenario, Title)) %>%
        ggplot(., aes(x = median, y = Scenario2, color = type)) +
        geom_point(alpha = 0.5, position = position_dodge(width = 0.7)) +
        geom_errorbar(aes(xmin = lci, xmax = uci),
            width = 0.4, position = position_dodge(width = 0.7)
        ) +
        scale_color_manual(values = c(
            "#074ba9", "#be134c"
        )) +
        theme_bw() +
        geom_hline(yintercept = seq(0.5, 92.5, 1), color = "gray75", linetype = "longdash") +
        geom_hline(yintercept = seq(3.5, 92.5, 3), color = "gray40", linetype = "longdash") +
        geom_hline(yintercept = seq(6.5, 92.5, 6), color = "gray10", linetype = "solid") +
        scale_y_discrete(limits = rev(levels(Dat$Scenario2))) +
        scale_x_continuous(limits = function(x) c(min(x), max(x) * 1.18)) +
        facet_wrap2(~type, scales = "free_x") +
        facetted_pos_scales(x = x_scales_list) +
        labs(
            y = "Programmes",
            x = xlab
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
            plot.margin = margin(l = 6, r = 15, b = 6)
        )

    ggsave(Fig, file = paste0(Path, paste0(Title, ".pdf")), width = 8, height = 6)
}


Plot.Appendix.2de <- function(Dat, xlab, Path, Title) {
    MaxValue <- summarise(Dat, max(uci), .by = type)
    type_values <- as.character(MaxValue$type)
    names(type_values) <- type_values

    x_scales_list <- lapply(type_values, \(x) {
        max <- MaxValue[which(MaxValue$type == x), 2]
        scale_x_continuous(limits = c(0, max * 1.1))
    })

    Fig <- Dat %>%
        filter(str_starts(Scenario, Title)) %>%
        ggplot(., aes(x = median, y = Scenario2, color = type)) +
        geom_point(alpha = 0.5, position = position_dodge(width = 0.7)) +
        geom_errorbar(aes(xmin = lci, xmax = uci),
            width = 0.4, position = position_dodge(width = 0.7)
        ) +
        scale_color_manual(values = c(
            "#008537", "#f25235"
        )) +
        theme_bw() +
        geom_hline(yintercept = seq(0.5, 92.5, 1), color = "gray75", linetype = "longdash") +
        geom_hline(yintercept = seq(3.5, 92.5, 3), color = "gray40", linetype = "longdash") +
        geom_hline(yintercept = seq(6.5, 92.5, 6), color = "gray10", linetype = "solid") +
        scale_y_discrete(limits = rev(levels(Dat$Scenario2))) +
        labs(
            y = "Programmes",
            x = xlab
        ) +
        facet_wrap2(~type, scales = "free_x") +
        facetted_pos_scales(x = x_scales_list) +
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
            plot.margin = margin(l = 6, r = 15, b = 6)
        )

    ggsave(Fig, file = paste0(Path, paste0(Title, ".pdf")), width = 8, height = 6)
}
