rm(list=ls())

xdata <- data.frame(prefs=c(149, 185, 221, 229, 265, 301), signals=c(149, 185, 221, 229, 265, 301), col=c("#E7B800",  "#00AFBB", "#E7B800","#E7B800","#00AFBB", "#E7B800"))

col="#00AFBB"
norm1 <- rnorm(50, mean=0, sd=1)
norm2 <- rnorm(50, mean=0, sd=1)
norm3 <- rnorm(50, mean=0, sd=0.3)
norm4 <- rnorm(50, mean=0, sd=0.3)
norm5 <- rnorm(50, mean=0, sd=0.3)
norm6 <- rnorm(50, mean=0, sd=0.3)
norm7 <- rnorm(50, mean=0, sd=0.5)
norm8 <- rnorm(50, mean=0, sd=0.5)
norm9 <- rnorm(50, mean=0, sd=1)
norm10 <- rnorm(50, mean=0, sd=1)

norm11 <- rnorm(50, mean=0, sd=1)
norm12 <- rnorm(50, mean=0, sd=1)
norm13 <- rnorm(50, mean=0, sd=1)
norm14 <- rnorm(50, mean=0, sd=1)

norm18 <- rnorm(50, mean=0, sd=0.3)
norm17 <- rnorm(50, mean=0, sd=0.3)
norm15 <- rnorm(50, mean=0, sd=0.3)
norm16 <- rnorm(50, mean=0, sd=0.3)



cor <- seq(-2,2,length.out=10)
lim <- c(-10,4)

xdata$col <- as.character(xdata$col)

RES=300
tiff("Figure1.tiff", height=24/4*RES, width=24/4*RES, res=RES, compression = "lzw")
par(mfrow=c(2,2), mar=c(0.5,2,4,2), oma=c(4,2,0,0))

plot(norm1, norm2, xlim=lim, ylim=lim,xlab='', ylab='', xaxt='n', yaxt='n', cex=0.75, pch=16, col=col)
points(norm7+1.5, norm8+1.5, xlim=lim, ylim=lim,xlab='', ylab='', xaxt='n', yaxt='n', cex=0.75, pch=16, col=xdata$col[1])
points(norm11-7, norm12-7, xlim=lim, ylim=lim,xlab='', ylab='', xaxt='n', yaxt='n', cex=0.75, pch=16, col=xdata$col[2])
points(norm13-7, norm14-7, xlim=lim, ylim=lim,xlab='', ylab='', xaxt='n', yaxt='n', cex=0.75, pch=16, col=xdata$col[1])
title("both species", adj = 0.5, line = 1.5)
title("(a)", adj = 0, line = 0.5)
mtext(text = 'reinforcement', side = 2, line=2.5, font=2)
mtext(text = 'signal', side = 2, line=0.5)

plot(norm1, norm2, xlim=lim, ylim=lim,xlab='', ylab='', xaxt='n', yaxt='n', cex=0.75, pch=16, col=col)
points(norm7+1.5, norm8+1.5, xlim=lim, ylim=lim,xlab='', ylab='', xaxt='n', yaxt='n', cex=0.75, pch=16, col=xdata$col[1])
points(norm13-7, norm14-7, xlim=lim, ylim=lim,xlab='', ylab='', xaxt='n', yaxt='n', cex=0.75, pch=16, col=xdata$col[2])
points(norm7-8.5, norm8-8.5, xlim=lim, ylim=lim,xlab='', ylab='', xaxt='n', yaxt='n', cex=0.75, pch=16, col=xdata$col[1])
title("one species", adj = 0.5, line = 1.5)
title("(b)", adj = 0, line = 0.5)
mtext(text = 'preference', side = 1, line=0.5, at=0.25,outer=TRUE)
mtext(text = 'preference', side = 1, line=0.5, at=0.75,outer=TRUE)


plot(norm4+cor, norm3+cor, xlim=lim, ylim=lim,xlab='', ylab='', xaxt='n', yaxt='n', cex=0.75, pch=16, col=col)
points(norm5+cor, norm6+cor, xlim=lim, ylim=lim,xlab='', ylab='', xaxt='n', yaxt='n', cex=0.75, pch=16, col=xdata$col[1])
points(norm4-7+cor, norm3-7-cor, xlim=lim, ylim=lim,xlab='', ylab='', xaxt='n', yaxt='n', cex=0.75, pch=16, col=xdata$col[2])
points(norm5-7+cor, norm6-7-cor, xlim=lim, ylim=lim,xlab='', ylab='', xaxt='n', yaxt='n', cex=0.75, pch=16, col=xdata$col[1])
title("(c)", adj = 0, line = 0.5)
mtext(text = 'signal', side = 2, line=0.5)
mtext(text = 'pre-disposition', side = 2, line=2.5, font=2)

plot(norm4+cor, norm3+cor, xlim=lim, ylim=lim,xlab='', ylab='', xaxt='n', yaxt='n', cex=0.75, pch=16, col=col)
points(norm5+cor, norm6+cor, xlim=lim, ylim=lim,xlab='', ylab='', xaxt='n', yaxt='n', cex=0.75, pch=16, col=xdata$col[1])
points(norm11-7, norm12-7, xlim=lim, ylim=lim,xlab='', ylab='', xaxt='n', yaxt='n', cex=0.75, pch=16, col=xdata$col[2])
points(norm13-7, norm14-7, xlim=lim, ylim=lim,xlab='', ylab='', xaxt='n', yaxt='n', cex=0.75, pch=16, col=xdata$col[1])
title("(d)", adj = 0, line = 0.5)

par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), mar=c(0, 0, 0, 0), new=TRUE)
plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
legend("bottom", legend=c("own","mixed"), pch=16, col=xdata$col[2:1],bty='n',horiz=TRUE)
dev.off()

