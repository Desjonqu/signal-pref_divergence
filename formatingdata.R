rm(list=ls())

#loading data
females <- read.csv("spline_summaries_formated.csv") 
males <- read.csv("male_IGE_fulldataset.csv")

#checking data
head(females)
head(males)

#preparing empty vectors to fuse male and female data
females$dom.freq <- NA
males$peak_pref <- NA

#binding male and female data keeping only columns of interest
xdata <- rbind(females[,c(10,15:22,14,3,33,30,28)], males[,c(9, 15:22,14, 33,4,30,28)])
xdata$n.fem <- as.numeric(!is.na(xdata[,11]))
xdata$n.males <- as.numeric(!is.na(xdata[,12]))
head(xdata)

#formating columns
xdata$year <- as.factor(xdata$year)
xdata$z.temp <- scale(xdata$temp)
xdata$z.days <- scale(xdata$daysintreat)
xdata$sp <- factor(xdata$sp, levels=c('LF','HF'))
xdata$treat <- factor(xdata$treat, levels=c('homo','hete'))

#aggregating female and male data by rearing plant
xdata.aggreg <- cbind(aggregate(xdata[,c(1:9, 13)], by = list(xdata$plants),FUN = unique)[2:11], aggregate(xdata[,c(10:12, 17:18)], by = list(xdata$plants),FUN = median, na.rm=TRUE)[2:6], aggregate(xdata[,10:12], by = list(xdata$plants),FUN = sd, na.rm=TRUE)[2:4], aggregate(xdata[,15:16], by = list(xdata$plants),FUN = sum)[2:3])
colnames(xdata.aggreg)[16:18] <- paste('sd', colnames(xdata.aggreg)[16:18], sep='.')


#scaling dominant frequency by species (mean of 0 and sd of 1 for each species)
xdata.aggreg$z.domf <- NA
xdata.aggreg$z.domf[xdata.aggreg$sp=='LF'] <- scale(xdata.aggreg$dom.freq[xdata.aggreg$sp=='LF'], scale = FALSE)
xdata.aggreg$z.domf[xdata.aggreg$sp=='HF'] <- scale(xdata.aggreg$dom.freq[xdata.aggreg$sp=='HF'], scale = FALSE)

#scaling peak preference by species (mean of 0 and sd of 1 for each species)
xdata.aggreg$z.pref <- NA
xdata.aggreg$z.pref[xdata.aggreg$sp=='LF'] <- scale(xdata.aggreg$peak_pref[xdata.aggreg$sp=='LF'], scale = FALSE)
xdata.aggreg$z.pref[xdata.aggreg$sp=='HF'] <- scale(xdata.aggreg$peak_pref[xdata.aggreg$sp=='HF'], scale = FALSE)

#check number of aggregations with both males and females for each species and treatment combination
length(xdata.aggreg$peak_pref[(!(is.na(xdata.aggreg$dom.freq)|is.na(xdata.aggreg$peak_pref)))&xdata.aggreg$treat=='homo'&xdata.aggreg$sp=='LF'])
length(xdata.aggreg$peak_pref[(!(is.na(xdata.aggreg$dom.freq)|is.na(xdata.aggreg$peak_pref)))&xdata.aggreg$treat=='hete'&xdata.aggreg$sp=='LF'])
length(xdata.aggreg$peak_pref[(!(is.na(xdata.aggreg$dom.freq)|is.na(xdata.aggreg$peak_pref)))&xdata.aggreg$treat=='homo'&xdata.aggreg$sp=='HF'])
length(xdata.aggreg$peak_pref[(!(is.na(xdata.aggreg$dom.freq)|is.na(xdata.aggreg$peak_pref)))&xdata.aggreg$treat=='hete'&xdata.aggreg$sp=='HF'])

#average dominant frequency per species and treatment
mean(xdata.aggreg$dom.freq[(!(is.na(xdata.aggreg$dom.freq)|is.na(xdata.aggreg$peak_pref)))&xdata.aggreg$treat=='homo'&xdata.aggreg$sp=='LF'])
mean(xdata.aggreg$dom.freq[(!(is.na(xdata.aggreg$dom.freq)|is.na(xdata.aggreg$peak_pref)))&xdata.aggreg$treat=='hete'&xdata.aggreg$sp=='LF'])
mean(xdata.aggreg$dom.freq[(!(is.na(xdata.aggreg$dom.freq)|is.na(xdata.aggreg$peak_pref)))&xdata.aggreg$treat=='homo'&xdata.aggreg$sp=='HF'])
mean(xdata.aggreg$dom.freq[(!(is.na(xdata.aggreg$dom.freq)|is.na(xdata.aggreg$peak_pref)))&xdata.aggreg$treat=='hete'&xdata.aggreg$sp=='HF'])

#average peak preference per species and treatment
mean(xdata.aggreg$peak_pref[(!(is.na(xdata.aggreg$dom.freq)|is.na(xdata.aggreg$peak_pref)))&xdata.aggreg$treat=='homo'&xdata.aggreg$sp=='LF'])
mean(xdata.aggreg$peak_pref[(!(is.na(xdata.aggreg$dom.freq)|is.na(xdata.aggreg$peak_pref)))&xdata.aggreg$treat=='hete'&xdata.aggreg$sp=='LF'])
mean(xdata.aggreg$peak_pref[(!(is.na(xdata.aggreg$dom.freq)|is.na(xdata.aggreg$peak_pref)))&xdata.aggreg$treat=='homo'&xdata.aggreg$sp=='HF'])
mean(xdata.aggreg$peak_pref[(!(is.na(xdata.aggreg$dom.freq)|is.na(xdata.aggreg$peak_pref)))&xdata.aggreg$treat=='hete'&xdata.aggreg$sp=='HF'])

#removing rearing plants without male or female measurements
xdata.aggreg <- xdata.aggreg[!(is.na(xdata.aggreg$dom.freq)|is.na(xdata.aggreg$peak_pref)),]

write.csv(xdata.aggreg, "medians.csv", row.names = FALSE)
