rm(list=ls())
library(car)

xdata.aggreg <- read.csv('medians.csv')

xdata.aggreg$year <- as.factor(xdata.aggreg$year)
xdata.aggreg$treat <- factor(xdata.aggreg$treat, levels=c('homo','hete'))
xdata.aggreg$sp <- factor(xdata.aggreg$sp, levels=c('LF','HF'))

res <- lm(dom.freq~z.pref*treat*sp+z.temp+year, data=xdata.aggreg)

Anova(res)
summary(res)

#residuals
source("diagnostic_fcns.r")
diagnostics.plot(res)

#colinearity
res.lm <- lm(z.domf~z.pref+treat+sp+z.temp+year, data=xdata.aggreg)
vif(res.lm)# Ok, <4

#Stability
max(abs(dffits(res)))#OK <2
round(cbind(res$coefficients, res$coefficients+t(apply(X=dfbeta(res), MARGIN=2, FUN=range))), digits=3)
max(cooks.distance(res))#OK <1

summary(res)
cbind(coefficients(res), confint(res))
Anova(res)

#Figure ----
xdata.aggreg$pred.data <- xdata.aggreg$dom.freq-(res$coefficients[5]*xdata.aggreg$z.temp+res$coefficients[6]*as.numeric(xdata.aggreg$year=='2019')+res$coefficients[7]*as.numeric(xdata.aggreg$year=='2020'))
xdata.aggreg <- xdata.aggreg[!is.na(xdata.aggreg$z.pref)&!is.na(xdata.aggreg$z.domf),]

min.ho.lf <- min(xdata.aggreg$z.pref[xdata.aggreg$treat=='homo'&xdata.aggreg$sp=='LF'], na.rm = TRUE)
max.ho.lf <- max(xdata.aggreg$z.pref[xdata.aggreg$treat=='homo'&xdata.aggreg$sp=='LF'], na.rm = TRUE)
min.he.lf <- min(xdata.aggreg$z.pref[xdata.aggreg$treat=='hete'&xdata.aggreg$sp=='LF'], na.rm = TRUE)
max.he.lf <- max(xdata.aggreg$z.pref[xdata.aggreg$treat=='hete'&xdata.aggreg$sp=='LF'], na.rm = TRUE)
min.ho.hf <- min(xdata.aggreg$z.pref[xdata.aggreg$treat=='homo'&xdata.aggreg$sp=='HF'], na.rm = TRUE)
max.ho.hf <- max(xdata.aggreg$z.pref[xdata.aggreg$treat=='homo'&xdata.aggreg$sp=='HF'], na.rm = TRUE)
min.he.hf <- min(xdata.aggreg$z.pref[xdata.aggreg$treat=='hete'&xdata.aggreg$sp=='HF'], na.rm = TRUE)
max.he.hf <- max(xdata.aggreg$z.pref[xdata.aggreg$treat=='hete'&xdata.aggreg$sp=='HF'], na.rm = TRUE)

pred.data <- data.frame(z.pref=c(seq(from=min.ho.lf, to=max.ho.lf, length.out=50), seq(from=min.he.lf, to=max.he.lf, length.out=50),seq(from=min.ho.hf, to=max.ho.hf, length.out=50),seq(from=min.he.hf, to=max.he.hf, length.out=50)), treat=rep(rep(levels(xdata$treat), each=50), 2), year='2018', z.temp=0, sp=rep(levels(xdata$sp), each=100))

ci.plot=as.data.frame(predict.lm(object=res, newdata=pred.data, interval="confidence"))

#correlations----


bg <- ifelse(xdata.aggreg$treat=='homo', yes='blue', no='orange')
pch=16
lf.rows <- which(xdata.aggreg$sp=='LF')
hf.rows <- which(xdata.aggreg$sp=='HF')
  
jpeg("Figure2.jpg", width = 6000, height = 5000, units = 'px', res=1000)
layout(mat = matrix(c(1,2), nrow = 2, byrow = TRUE), heights = c(1,4))
par(mar=c(0, 0, 0, 0), xpd=TRUE)

plot(1, type="n", axes=FALSE, xlab="", ylab="")
legend(0.95,1.2, legend = c('own', 'mixed'), col=c('blue', 'orange'), pch=pch, bty='n', lty=1, inset = c(-1.2, 0), title='rearing treatment')

par(mar=c(4.1, 4.1, 1.1, 1.1), xpd=TRUE)

range(xdata.aggreg$peak_pref, na.rm=TRUE)
xlims <- c(160,340)
ylims <- c(150,330)
range(xdata.aggreg$pred.data[lf.rows], na.rm=TRUE)+c(-5,5)
plot(xdata.aggreg$peak_pref[lf.rows], xdata.aggreg$pred.data[lf.rows], col=bg[lf.rows], pch=pch, ylab="male signal frequency (Hz)", xlab="", frame=FALSE, xlim=xlims, ylim=ylims, xaxt='n', yaxt='n', cex=0.7)
xat=seq(160, 340, by=20)
yat=seq(150, 330, by=20)
axis(side = 1, at = xat, cex.axis=0.9)
axis(side = 2, at = yat, cex.axis=0.9)
mtext(text = "female peak preference (Hz)",side = 1, line = 2.5,at = 250)

pred <- predict(lm(pred.data~z.pref,data=xdata.aggreg[xdata.aggreg$sp=='LF'&xdata.aggreg$treat=='homo',]))
lines(xdata.aggreg$peak_pref[!is.na(xdata.aggreg$pred.data)&!is.na(xdata.aggreg$z.pref)&xdata.aggreg$sp=='LF'&xdata.aggreg$treat=='homo'], pred, col='blue')
cor.ho.lf <- cor.test(xdata.aggreg$z.pref[!is.na(xdata.aggreg$pred.data)&!is.na(xdata.aggreg$z.domf)&xdata.aggreg$sp=='LF'&xdata.aggreg$treat=='homo'], xdata.aggreg$pred.data[!is.na(xdata.aggreg$pred.data)&!is.na(xdata.aggreg$z.domf)&xdata.aggreg$sp=='LF'&xdata.aggreg$treat=='homo'])
text(x = 180, y=200, labels = paste('r=', round(cor.ho.lf$estimate, 2), sep=''), col='blue', cex = 0.9)


pred <- predict(lm(pred.data~z.pref,data=xdata.aggreg[xdata.aggreg$sp=='LF'&xdata.aggreg$treat=='hete',]))
lines(xdata.aggreg$peak_pref[!is.na(xdata.aggreg$pred.data)&!is.na(xdata.aggreg$z.pref)&xdata.aggreg$sp=='LF'&xdata.aggreg$treat=='hete'], pred, col='orange')
cor.he.lf <- cor.test(xdata.aggreg$z.pref[!is.na(xdata.aggreg$pred.data)&!is.na(xdata.aggreg$z.pref)&xdata.aggreg$sp=='LF'&xdata.aggreg$treat=='hete'], xdata.aggreg$pred.data[!is.na(xdata.aggreg$pred.data)&!is.na(xdata.aggreg$z.pref)&xdata.aggreg$sp=='LF'&xdata.aggreg$treat=='hete'])
text(x = 180, y=192, labels = paste('r=', round(cor.he.lf$estimate, digits = 2), sep=''), col='orange', cex = 0.9)


points(xdata.aggreg$peak_pref[hf.rows], xdata.aggreg$pred.data[hf.rows], col=bg[hf.rows], pch=pch, ylab="", xlab="", frame=FALSE, xlim=xlims, ylim=ylims, xaxt='n', yaxt='n', cex=0.7)

pred <- predict(lm(pred.data~z.pref,data=xdata.aggreg[xdata.aggreg$sp=='HF'&xdata.aggreg$treat=='homo',]))
lines(xdata.aggreg$peak_pref[!is.na(xdata.aggreg$pred.data)&!is.na(xdata.aggreg$z.pref)&xdata.aggreg$sp=='HF'&xdata.aggreg$treat=='homo'], pred, col='blue', lty=1)
cor.ho.hf <- cor.test(xdata.aggreg$z.pref[!is.na(xdata.aggreg$pred.data)&!is.na(xdata.aggreg$z.pref)&xdata.aggreg$sp=='HF'&xdata.aggreg$treat=='homo'], xdata.aggreg$pred.data[!is.na(xdata.aggreg$pred.data)&!is.na(xdata.aggreg$z.pref)&xdata.aggreg$sp=='HF'&xdata.aggreg$treat=='homo'])
text(x=320, y=330, labels = paste('r=', round(cor.ho.hf$estimate, 2), sep=''), col='blue', cex = 0.9)

pred <- predict(lm(pred.data~z.pref,data=xdata.aggreg[xdata.aggreg$sp=='HF'&xdata.aggreg$treat=='hete',]))
lines(xdata.aggreg$peak_pref[!is.na(xdata.aggreg$pred.data)&!is.na(xdata.aggreg$z.pref)&xdata.aggreg$sp=='HF'&xdata.aggreg$treat=='hete'], pred, col='orange', lty=1)
cor.he.hf <- cor.test(xdata.aggreg$z.pref[!is.na(xdata.aggreg$pred.data)&!is.na(xdata.aggreg$z.pref)&xdata.aggreg$sp=='HF'&xdata.aggreg$treat=='hete'], xdata.aggreg$pred.data[!is.na(xdata.aggreg$pred.data)&!is.na(xdata.aggreg$z.pref)&xdata.aggreg$sp=='HF'&xdata.aggreg$treat=='hete'])
text(x = 320, y=322, labels = paste('r=', round(cor.he.hf$estimate, 2), sep=''), col='orange', cex = 0.9)

#mtext(text = c('(a)', '(b)'), side=1, line=-16, at=c(-94,-25))
mtext(text = c(expression(sp[low]),expression(sp[high])), side=1, line=-16, at=c(180,305))
dev.off()


