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
Dat <- Dat[, .(summ = sum(number)), by = .(Year, week, age_group)]

## Filter the epidemic season Week 40-53 and Week 1-10
RefDat <- Dat[week %in% c(1:10, 40:53), ] # 1:5, 42:53

RefDat <- RefDat[, week := sprintf("%02d", week)][, week := paste0(Year, "-W", week)]

tempTable <- expand.grid(
  week = substr(ISOweek::date2ISOweek(seq(from = as.Date("2017-06-30"), to = as.Date("2020-03-31"), by = "week")), 1, 8),
  age_group = unique(RefDat$age_group)
)

RefDat <- merge(tempTable, RefDat, all.x = TRUE) %>% as.data.table()

## Calculate moving average of RefDat
TempWeek <- expand.grid(
  week = 1:52, Year = 2017:2020,
  age_group = unique(RefDat$age_group)
) %>% as.data.table()

RefDat_MA <- merge(TempWeek, Dat, all.x = TRUE) %>% as.data.table()

RefDat_MA <- RefDat_MA[, week := sprintf("%02d", week)][, week := paste0(Year, "-W", week)]
RefDat_MA <- RefDat_MA[order(age_group, week)]
RefDat_MA <- RefDat_MA[, summ := round(frollmean(summ, n = 3)), by = .(age_group)]

## Filter the epidemic season Week 40-53 and Week 1-10 in RefDat_MA
RefDat_MA <- RefDat_MA[grepl("^201[7-9]-W(0[1-9]|10|4[0-9]|5[0-3])$|^2020-W(0[1-9]|10|4[0-9]|5[0-3])$", week)]


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
#   RealDat_plot %>%
#     mutate(
#       age_group = factor(age_group,
#         levels = c(
#           "Reported_G1", "Reported_G2", "Reported_G3",
#           "Reported_G4", "Reported_G5", "Reported_G6",
#           "Reported_G7", "Reported_G8", "Reported_G9",
#           "Reported_G10", "Reported_G11"
#         ),
#         labels = c(
#           "0-2m", "3-5m", "6-11m", "12-23m", "2-4y", "5-19y",
#           "20-59y", "60-64y", "65-69y", "70-74y", "75+y"
#         )
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
#   file = "Output/RealCases_11age.pdf", width = 28, height = 14
# )

RefDat <- RefDat[, Year := NULL] # [is.na(summ), summ := 1] # 这里看下对于某一年龄组没有病例的如何处理

RefDat <- RefDat[, week := as.character(week)]
colnames(RefDat) <- c("week", "age_group", "true_value")

RefDat <- na.omit(RefDat)

# ISOweek::ISOweek2date("2017-W42-1")
# ISOweek::ISOweek2date("2018-W05-1")


### For all 11 age groups, find the peak week of the year for each age group

# WideRefDat <- dcast(RealDat_plot, week ~ age_group, value.var = "summ")
# fwrite(WideRefDat, "WideRefDat.csv")
WideRefDat <- fread("WideRefDat.csv")
RefPeak <- lapply(WideRefDat[, 2:12], \(x) WideRefDat[x, 1])

lapply(names(RefPeak), \(name) {
  dt <- RefPeak[[name]]
  dt[, age_group := name]
  dt[, RefWeek := paste0(week, "-1")][, week := NULL]
  dt[, RefDate := ISOweek::ISOweek2date(RefWeek)]
  return(dt)
})

RefPeak <- do.call(rbind, RefPeak)


### For 2 age groups (<5 & >=5), find the peak week of the year for each age group
RealDat_plot_2age <- copy(RefDat_MA)
RealDat_plot_2age[, age_group := fcase(
  age_group %in% c("Reported_G1", "Reported_G2", "Reported_G3", "Reported_G4", "Reported_G5"), "Reported_G1",
  default = "Reported_G2"
)]
RealDat_plot_2age <- RealDat_plot_2age[, .(summ = sum(summ, na.rm = TRUE)), by = .(week, age_group)]
RealDat_plot_2age <- RealDat_plot_2age[, summ := lapply(.SD, function(x) replace(x, x == 0, NA)), .SDcols = c("summ")]
# WideRefDat_2age <- dcast(RealDat_plot_2age, week ~ age_group, value.var = "summ")
# fwrite(WideRefDat_2age, "WideRefDat_2age.csv")
# fwrite(WideRefDat_2age, "WideRefDat_2age_MA.csv")

WideRefDat_2age <- fread("WideRefDat_2age_MA.csv")
RefPeak_2age <- lapply(WideRefDat_2age[, 2:3], \(x) WideRefDat_2age[x, 1])

lapply(names(RefPeak_2age), \(name) {
  dt <- RefPeak_2age[[name]]
  dt[, age_group := name]
  dt[, RefWeek := paste0(week, "-1")][, week := NULL]
  dt[, RefDate := ISOweek::ISOweek2date(RefWeek)]
  return(dt)
})

RefPeak_2age <- do.call(rbind, RefPeak_2age)
