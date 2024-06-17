rm(list=ls())

xdata.aggreg <- read.csv("medians.csv")

#standard deviation of each sex and species
sd(xdata.aggreg$peak_pref[xdata.aggreg$sp=='LF'], na.rm = TRUE)
sd(xdata.aggreg$peak_pref[xdata.aggreg$sp=='HF'], na.rm = TRUE)
sd(xdata.aggreg$dom.freq[xdata.aggreg$sp=='LF'], na.rm = TRUE)
sd(xdata.aggreg$dom.freq[xdata.aggreg$sp=='HF'], na.rm = TRUE)

#creating vectors for male signal frequency and female peak preference for each species
fem.LF <- xdata.aggreg$peak_pref[xdata.aggreg$sp=='LF']
male.LF <- xdata.aggreg$dom.freq[xdata.aggreg$sp=='LF']
fem.HF <- xdata.aggreg$peak_pref[xdata.aggreg$sp=='HF']
male.HF <- xdata.aggreg$dom.freq[xdata.aggreg$sp=='HF']

#Four variance tests comparing male and female between species and within species
var.test(fem.HF,fem.LF)
var.test(male.HF,male.LF)
var.test(male.HF, fem.HF)
var.test(fem.LF, male.LF)