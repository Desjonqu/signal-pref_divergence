rm(list=ls())
library(ggplot2)

a <- rnorm(10000)
b <- a+1
c <- data.frame(x=c(a,b), col=rep(c('a', 'b'), each=10000))

p1 <- ggplot(c) + theme_classic() + stat_density(aes(x, color=col),position="identity",geom="line", bw=0.5, show.legend = FALSE) + geom_vline(xintercept=0, linetype="dashed", color = 'grey', size = 1) + geom_vline(xintercept=1, linetype="dashed", color = 'grey', size = 1) + scale_x_continuous(name = "signal trait value", breaks = 0, labels = '') + scale_y_continuous(name = "attractiveness", breaks=0, labels='') + labs(title='Preference peak', size=2)+theme(plot.title = element_text(hjust = 0.5)) + theme(axis.ticks = element_blank())

ggsave('figS1_preftraits.tiff', width =10, height=8, units='cm')
