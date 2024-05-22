library(readxl)
library(tidyverse)

# Import data -------------------------------------------------------------
# Import population data
Scot_Pop <- read_excel("Pop.xlsx", sheet = "Scot_expand")
Scot_Pop <- Scot_Pop[5, -1] %>% as.matrix()
colnames(Scot_Pop) <- c(
    "m0_2", "m3_5", "m6_11", "y1_2", "y2_4", "y5_9", "y10_14", "y15_19",
    "y20_24", "y25_29", "y30_34", "y35_39", "y40_44", "y45_49", "y50_54",
    "y55_59", "y60_64", "y65_69", "y70_74", "y75"
)

# Import contact rate data
ContactRate <- read_excel("ContactRate2.xlsx", sheet = "newage") %>%
    replace(., is.na(.), 0) # %>% as.matrix()
ContactRate <- ContactRate[, -1] %>% as.matrix()
colnames(ContactRate) <- rownames(ContactRate) <- c(
    "m0_2", "m3_5", "m6_11", "y1_2", "y2_4", "y5_9", "y10_14", "y15_19",
    "y20_24", "y25_29", "y30_34", "y35_39", "y40_44", "y45_49", "y50_54",
    "y55_59", "y60_64", "y65_69", "y70_74", "y75"
)

# Calculate contact intensity (Scot) --------------------------------------
# 0-2 months, 3-5 months, 6-11 months, 1-<2 years, 2-4 years,
# 5-19 years, 20-59y, 60-64y, 65-69y, 70-74y and 75+y
# 为什么要乘自己的人口数，再除总人数
totalcont_m0_2 <- ContactRate[, 1] * Scot_Pop[1] * Scot_Pop
totalcont_m3_5 <- ContactRate[, 2] * Scot_Pop[2] * Scot_Pop
totalcont_m6_11 <- ContactRate[, 3] * Scot_Pop[3] * Scot_Pop
totalcont_y1_2 <- ContactRate[, 4] * Scot_Pop[4] * Scot_Pop
totalcont_y2_4 <- ContactRate[, 5] * Scot_Pop[5] * Scot_Pop

totalcont_y5_19 <- 0
for (i in 6:8) {
    totalcont_y5_19_temp <- ContactRate[, i] * Scot_Pop[i] * Scot_Pop
    totalcont_y5_19 <- totalcont_y5_19 + totalcont_y5_19_temp
}

totalcont_y20_59 <- 0
for (i in 9:16) {
    totalcont_y20_59_temp <- ContactRate[, i] * Scot_Pop[i] * Scot_Pop
    totalcont_y20_59 <- totalcont_y20_59 + totalcont_y20_59_temp
}

totalcont_y60_64 <- ContactRate[, 17] * Scot_Pop[17] * Scot_Pop
totalcont_y65_69 <- ContactRate[, 18] * Scot_Pop[18] * Scot_Pop
totalcont_y70_74 <- ContactRate[, 19] * Scot_Pop[19] * Scot_Pop
totalcont_y75 <- ContactRate[, 20] * Scot_Pop[20] * Scot_Pop

totalcont_all <- cbind(
    t(totalcont_m0_2), t(totalcont_m3_5), t(totalcont_m6_11), t(totalcont_y1_2),
    t(totalcont_y2_4), t(totalcont_y5_19), t(totalcont_y20_59), t(totalcont_y60_64),
    t(totalcont_y65_69), t(totalcont_y70_74), t(totalcont_y75)
) %>% as_tibble()
totalcont_all$cat <- c(
    "m0_2", "m3_5", "m6_11", "y1_2", "y2_4", "y5_19", "y5_19", "y5_19",
    "y20_59", "y20_59", "y20_59", "y20_59", "y20_59", "y20_59", "y20_59",
    "y20_59", "y60_64", "y65_69", "y70_74", "y75"
)
totalcont_all$cat <- factor(totalcont_all$cat, levels = c(
    "m0_2", "m3_5", "m6_11", "y1_2", "y2_4", "y5_19", "y20_59", "y60_64",
    "y65_69", "y70_74", "y75"
))
totalcont_all <- aggregate(totalcont_all[, 1:11], by = list(cat = totalcont_all$cat), sum)
totalcont_all <- totalcont_all[, -1] %>% as.matrix()

colnames(totalcont_all) <- rownames(totalcont_all) <- c(
    "m0_2", "m3_5", "m6_11", "y1_2", "y2_4", "y5_19", "y20_59", "y60_64",
    "y65_69", "y70_74", "y75"
)


# Calculate contact rate --------------------------------------------------
Scot_Pop2 <- Scot_Pop
colnames(Scot_Pop2) <- c(
    "m0_2", "m3_5", "m6_11", "y1_2", "y2_4", "y5_19", "y5_19", "y5_19",
    "y20_59", "y20_59", "y20_59", "y20_59", "y20_59", "y20_59", "y20_59",
    "y20_59", "y60_64", "y65_69", "y70_74", "y75"
)
Scot_Pop2 <- tapply(Scot_Pop2, colnames(Scot_Pop2), sum)

ColOrder <- factor(names(Scot_Pop2), levels = c(
    "m0_2", "m3_5", "m6_11", "y1_2", "y2_4", "y5_19", "y20_59", "y60_64",
    "y65_69", "y70_74", "y75"
))
Scot_Pop2 <- Scot_Pop2[order(ColOrder)]


ContMean <- c()
for (i in 1:11) {
    ContMean_temp <- totalcont_all[, i] / Scot_Pop2[i] / Scot_Pop2
    ContMean <- cbind(ContMean, ContMean_temp)
}
colnames(ContMean) <- rownames(ContMean)

ContMean <- ContMean + t(ContMean) - diag(diag(ContMean))

write.csv(ContMean, "ContRatePerCapita.csv", row.names = TRUE)
