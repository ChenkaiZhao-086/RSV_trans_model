library(readxl)
library(data.table)
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

RefDat <- rbind(Less1, Over1)
RefDat <- RefDat[, .(summ = sum(number)), by = .(Year, week, age_group)]

# Filter the epidemic season Week 40-53 and Week 1-10
RefDat <- RefDat[week %in% c(1:10, 40:53), ] # 1:5, 42:53

RefDat <- RefDat[, week := sprintf("%02d", week)][, week := paste0(Year, "-W", week)]

tempTable <- expand.grid(
  week = substr(ISOweek::date2ISOweek(seq(from = as.Date("2017-06-30"), to = as.Date("2020-03-31"), by = "week")), 1, 8),
  age_group = unique(RefDat$age_group)
)

RefDat <- merge(tempTable, RefDat, all.x = TRUE) %>% as.data.table()

RealDat_plot <- copy(RefDat)
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
#   file = "Output/RealCases.pdf", width = 28, height = 14
# )

RefDat <- RefDat[, Year := NULL] # [is.na(summ), summ := 1] # 这里看下对于某一年龄组没有病例的如何处理

RefDat <- RefDat[, week := as.character(week)]
colnames(RefDat) <- c("week", "age_group", "true_value")

RefDat <- na.omit(RefDat)

# ISOweek::ISOweek2date("2017-W42-1")
# ISOweek::ISOweek2date("2018-W05-1")
