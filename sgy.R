DaneTravelMeansByTime <- getCountyMeansTravelByTime(state = 55,county = 025)
DaneTravelMeansByTime <- within(DaneTravelMeansByTime, travelTime <- factor(travelTime, levels = c("minLT10", "min10to14", "min15to19","min20to24","min25to29","min30to34","min35to44","min45to59","minOver60")))
DaneTravelMeansByTime <- within(DaneTravelMeansByTime, mode <- factor(mode, levels = c("drive","walk","bicycle","transit")))
ggplot(DaneTravelMeansByTime, aes(x = travelTime, y = estimate, fill = mode)) + geom_bar(stat = "identity", position = "dodge") + theme_bw() + ylab("Number of Commuters") + xlab("Duration of Commute")
