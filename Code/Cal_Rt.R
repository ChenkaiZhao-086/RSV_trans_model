Less1 <- read_excel("RSV_hosp_posBySpecimenCollectonWeek_check.xlsx",
    sheet = "<1y"
) %>% as.data.table()
Over1 <- read_excel("RSV_hosp_posBySpecimenCollectonWeek_check.xlsx",
    sheet = ">1y"
) %>% as.data.table()


Less1[, age_group := fcase(
    `age-month` <= 2, "Reported_G1",
    `age-month` <= 5, "Reported_G2",
    `age-month` <= 11, "Reported_G3",
    default = "Reported_G4"
)][, `age-month` := NULL]
Over1[, age_group := fcase(
    `age-year` <= 2, "Reported_G4",
    `age-year` <= 4, "Reported_G5",
    `age-year` <= 19, "Reported_G6",
    `age-year` <= 59, "Reported_G7",
    `age-year` <= 64, "Reported_G8",
    `age-year` <= 69, "Reported_G9",
    `age-year` <= 74, "Reported_G10",
    default = "Reported_G11"
)][, `age-year` := NULL]

Dat <- rbind(Less1, Over1)
Dat <- Dat[, .(summ = sum(number)), by = .(Year, week)] # , age_group

## Filter the epidemic season Week 40-53 and Week 1-10
RefDat <- Dat[week %in% c(1:53), ] # 1:10, 40:53 1:5, 42:53

RefDat <- RefDat[, week := sprintf("%02d", week)][, week := paste0(Year, "-W", week)]

tempTable <- expand.grid(
    week = substr(ISOweek::date2ISOweek(seq(from = as.Date("2017-03-30"), to = as.Date("2023-06-20"), by = "week")), 1, 8) # ,
    # age_group = unique(RefDat$age_group)
)

RefDat <- merge(tempTable, RefDat, all.x = TRUE) %>% as.data.table()

RefDat2 <- copy(RefDat)
RefDat2 <- RefDat2[, ":="(week2 = paste0(week, "-1"),
    Year = substr(week, 1, 4),
    Week = substr(week, 7, 8)
)][, date := ISOweek::ISOweek2date(week2)][, summ := nafill(summ, fill = 0)][, week2 := NULL]

dat17_18 <- RefDat2[date >= as.Date("2017-06-19") & date <= as.Date("2018-06-11")]
dat18_19 <- RefDat2[date >= as.Date("2018-06-11") & date <= as.Date("2019-06-03")]
dat19_20 <- RefDat2[date >= as.Date("2019-06-10") & date <= as.Date("2020-06-01")]

Merge1 <- merge(dat17_18, dat18_19,
    by = c("Week"),
    all = TRUE, suffixes = c(".1", ".2")
)
MergeDat <- merge(Merge1, dat19_20,
    by = c("Week"), all = TRUE
)

MergeDat <- MergeDat[
    ,
    summ := (summ.1 + summ.2 + summ) / 3
][
    ,
    ":="(summ.1 = NULL, summ.2 = NULL, week.1 = NULL, week.2 = NULL, Year.1 = NULL, Year.2 = NULL, date.1 = NULL, date.2 = NULL)
][order(date)]


dat21_23 <- RefDat2[date >= as.Date("2021-05-07") & date <= as.Date("2023-05-01")]

RealDat_plot <- copy(RefDat)
## Plot the number of cases by week and age group (2 age groups)
# ggsave(
#   RealDat_plot %>%
#     mutate(
#       age_group = factor(age_group,
#         levels = c("Reported_G1", "Reported_G2"),
#         labels = c("<5y", ">=5y")
#       ),
#       week = ISOweek::ISOweek2date(paste0(week, "-1"))
#     ) %>%
#     ggplot(., aes(x = week, y = summ, group = age_group)) +
#     geom_line() +
#     # add vertical lines to indicate the start and end of the epidemic season
#     # geom_vline(xintercept = as.Date("2017-10-16"), linetype = "dashed") +
#     # geom_vline(xintercept = as.Date("2018-01-29"), linetype = "dashed") +
#     labs(
#       x = "Date",
#       y = "Number of cases"
#     ) +
#     theme_minimal() +
#     scale_x_date(date_labels = "%Y") +
#     facet_wrap(~age_group) +
#     theme(
#       axis.text.x = element_text(size = 18, hjust = 1),
#       axis.text.y = element_text(size = 18),
#       axis.title = element_text(size = 24),
#       strip.text = element_text(size = 24)
#     ),
#   file = "Output/RealCases_2age.pdf", width = 28, height = 14
# )

# ## Plot the number of cases by week and age group (11 age groups)
# ggsave(
#     RealDat_plot %>%
#         mutate(
#             age_group = factor(age_group,
#                 levels = c(
#                     "Reported_G1", "Reported_G2", "Reported_G3",
#                     "Reported_G4", "Reported_G5", "Reported_G6",
#                     "Reported_G7", "Reported_G8", "Reported_G9",
#                     "Reported_G10", "Reported_G11"
#                 ),
#                 labels = c(
#                     "0-2m", "3-5m", "6-11m", "12-23m", "2-4y", "5-19y",
#                     "20-59y", "60-64y", "65-69y", "70-74y", "75+y"
#                 )
#             ),
#             week = ISOweek::ISOweek2date(paste0(week, "-1"))
#         ) %>%
#         ggplot(., aes(x = week, y = summ, group = age_group)) +
#         geom_line() +
#         # add vertical lines to indicate the start and end of the epidemic season
#         # geom_vline(xintercept = as.Date("2017-10-16"), linetype = "dashed") +
#         # geom_vline(xintercept = as.Date("2018-01-29"), linetype = "dashed") +
#         labs(
#             x = "Date",
#             y = "Number of cases"
#         ) +
#         theme_minimal() +
#         scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
#         facet_wrap(~age_group) +
#         theme(
#             axis.text.x = element_text(size = 18, hjust = 1, angle = 45),
#             axis.text.y = element_text(size = 18),
#             axis.title = element_text(size = 24),
#             strip.text = element_text(size = 24)
#         ),
#     file = "Output/RealCases_11age_2023.pdf", width = 15, height = 8
# )

# RefDat <- RefDat[, Year := NULL] # [is.na(summ), summ := 1] # 这里看下对于某一年龄组没有病例的如何处理

# RefDat <- RefDat[, week := as.character(week)]
# colnames(RefDat) <- c("week", "age_group", "true_value")

# RefDat <- na.omit(RefDat)

# ISOweek::ISOweek2date("2017-W42-1")
# ISOweek::ISOweek2date("2018-W05-1")


library(EpiEstim)
set.seed(380)
# 计算Rt
BeforePandemic <- estimate_R(
    incid = MergeDat$summ,
    method = "parametric_si",
    config = make_config(
        t_start = 2:52, t_end = 2:52,
        mean_si = 7.4, std_si = 2.5
    )
)
plot(BeforePandemic)

PostPandemic <- estimate_R(
    incid = dat21_23$summ,
    method = "parametric_si",
    config = make_config(
        t_start = 2:104, t_end = 2:104,
        mean_si = 7.4, std_si = 2.5
    )
)
plot(PostPandemic)

Pre_Rt <- BeforePandemic$R
Pre_Rt <- Pre_Rt[, -c(2:4, 6:7, 9, 10)]
Post_Rt <- PostPandemic$R
colnames(Pre_Rt) <- c("time", "lci", "median", "uci")
Post_Rt <- Post_Rt[, -c(2:4, 6:7, 9, 10)]
colnames(Post_Rt) <- c("time", "lci", "median", "uci")
Post_Rt_22 <- Post_Rt[1:51, ]
Post_Rt_23 <- Post_Rt[52:102, ]
Post_Rt_23$time <- 2:52


fig <- ggplot() +
    geom_ribbon(data = Pre_Rt, aes(x = time, ymin = lci, ymax = uci, fill = "17-20"), alpha = 0.2) +
    geom_line(data = Pre_Rt, aes(x = time, y = median, color = "17-20")) +
    geom_ribbon(data = Post_Rt_22, aes(x = time, ymin = lci, ymax = uci, fill = "21-22"), alpha = 0.2) +
    geom_line(data = Post_Rt_22, aes(x = time, y = median, color = "21-22")) +
    geom_ribbon(data = Post_Rt_23, aes(x = time, ymin = lci, ymax = uci, fill = "22-23"), alpha = 0.2) +
    geom_line(data = Post_Rt_23, aes(x = time, y = median, color = "22-23")) +
    geom_hline(yintercept = 1, linetype = "dashed") +
    labs(
        x = "Week",
        y = "Rt"
    ) +
    scale_color_manual(
        name = NULL,
        values = c(
            "17-20" = "#049143",
            "21-22" = "#c9283a",
            "22-23" = "#0e5385"
        )
    ) +
    scale_fill_manual(
        name = NULL,
        values = c(
            "17-20" = "#049143",
            "21-22" = "#c9283a",
            "22-23" = "#0e5385"
        )
    ) +
    scale_x_continuous(
        breaks = seq(1, 52, by = 3),
        labels = c(seq(24, 52, by = 3), seq(2, 23, by = 3))
    ) +
    theme_classic() +
    theme(
        axis.text = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.text.x = element_text(margin = margin(b = 5)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # strip.background = element_rect(fill = "grey95"),
        # strip.text = element_text(size = 14, face = "bold"),
        # plot.margin = margin(l = 6, r = 6, b = 6),
        legend.text = element_text(size = 14),
        legend.position = c(0.85, 0.7)
    )
ggsave(fig, file = "Output/V1/Rt.pdf", width = 8, height = 5)

save.image(file = "Output/V1/Rt.Rdata")
