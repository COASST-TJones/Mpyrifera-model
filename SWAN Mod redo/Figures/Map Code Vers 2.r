Vfind <- function(dat){
	Hmax <- 0.6541*dat$Hsig*(sqrt(log(dat$Tm_10 + 10800))+0.2886*(1/(sqrt(log(10800/dat$Tm_10)))))
	Hmax[Hmax > 0.78*dat$Depth] <- 0.78*dat$Depth[Hmax > 0.78*dat$Depth]
	k <- 2*pi/dat$Wlen
	Vmax <- (pi*Hmax/dat$Tm_10)*cosh(k)/sinh(k*dat$Depth)
	return(Vmax)
}

distfind <- function(dat, xrange, yrange){

	d1 <- abs(dat$Xt-xrange[1]) 
	d2 <- abs(dat$Xt-xrange[2]) 
	d3 <- abs(dat$Yt-yrange[1]) 
	d4 <- abs(dat$Yt-yrange[2])
	mindist <- pmin(d1, d2, d3, d4)

}


which.rowcol <- function(dat) {
	seq.col <- c(0,0.25,0.5,0.75,1,1.25,1.5,1.75,2.0,2.25,2.5,2.75,3)
	if(dat < 0) {
		col.ret <- rgb(0,0,0,1)
	} else {
		max.ind <- max(which(seq.col <= dat))
		
		colvec <- rainbow(n=12, end=0.75, start=0)
		colvec[4] <- rgb(1,1,0,1)
		colvec[5] <- rgb(0.65,1,0,1)
		col.ret <- colvec[13-max.ind]
	}
	return(col.ret)
}


###########
setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale")
n1.c21 <- read.csv(file='N1/Nest1_C21.csv',stringsAsFactors =FALSE)
n1.xran <- range(n1.c21$Xt)
n1.yran <- range(n1.c21$Yt)
n1.shore <- n1.c21[n1.c21$Depth<=0,]
n1.c21 <- n1.c21[n1.c21$Ubot != -10 & n1.c21$Depth > 1 & n1.c21$Tm_10 > 0,]
Vmax <- Vfind(n1.c21)
Dboun <- distfind(dat=n1.c21, xrange=n1.xran, yrange=n1.yran)
Aweight <- exp(-6.907 + 0.046*Dboun)/(1+exp(-6.907 + 0.046*Dboun))
n1.c21 <- data.frame(n1.c21[,c(14,15,16)], Vmax,Dboun, Aweight)

n2.c21 <- read.csv(file='N2/Nest2_C21.csv',stringsAsFactors =FALSE)
n2.xran <- range(n2.c21$Xt)
n2.yran <- range(n2.c21$Yt)
n2.shore <- n2.c21[n2.c21$Depth<=0,]
n2.c21 <- n2.c21[n2.c21$Ubot != -10 & n2.c21$Depth > 1 & n2.c21$Tm_10 > 0,]
Vmax <- Vfind(n2.c21)
Dboun <- distfind(dat=n2.c21, xrange=n2.xran, yrange=n2.yran)
Aweight <- exp(-6.907 + 0.046*Dboun)/(1+exp(-6.907 + 0.046*Dboun))
n2.c21 <- data.frame(n2.c21[,c(14,15,16)], Vmax,Dboun, Aweight)

n1.over <- n1.c21[n1.c21$uniqval %in% n2.c21$uniqval,]
n2.over <- n2.c21[n2.c21$uniqval %in% n1.c21$uniqval,]
sum(abs(n1.over$Xt-n2.over$Xt) + abs(n1.over$Yt-n2.over$Yt))

Vnew <- ((n1.over$Vmax*n1.over$Aweight) + (n2.over$Vmax*n2.over$Aweight))/(n1.over$Aweight + n2.over$Aweight) 

n1.c21$Vmax[n1.c21$uniqval %in% n2.c21$uniqval] <- Vnew
n2.c21$Vmax[n2.c21$uniqval %in% n1.c21$uniqval] <- Vnew

INDEXCOL <- 1:dim(n1.c21)[1]
colvec <- tapply(X=n1.c21$Vmax, INDEX=INDEXCOL, FUN=which.rowcol)

png(filename = "N1_C21.png", width = 2000, height = 2000, units = "px", pointsize = 12, bg = "white",  res = 300,type = "cairo-png")
par(mar=c(2,2,1,1))
plot(Yt ~ Xt, data=n1.c21, pch=15, col=colvec, asp=1, cex=0.17, xlab="", ylab="", bty="n", xaxt="n", yaxt="n", ylim=n1.yran, xlim=n1.xran)
lines(Yt ~ Xt, data=n1.shore, pch=15, col="grey", cex=0.17, type="p")
YCV <- c(5419000,5418600,5419360,5419360,5420200,5420200,5420600,5420600,5420550,5421800,5421800,5421000,5419650,5421600)
XCV <- c(309080,309080,309040,310460,311220,312640,314020,315150,316000,315100,316400,317100,317100,316500)
lines(YCV ~ XCV, pch=3, cex=1, col=2, type="p")
dev.off()

###

n3.c21 <- read.csv(file='N3/Nest3_C21.csv')
n3.xran <- range(n3.c21$Xt)
n3.yran <- range(n3.c21$Yt)
n3.shore <- n3.c21[n3.c21$Depth<=0,]
n3.c21 <- n3.c21[n3.c21$Ubot != -10 & n3.c21$Depth > 1 & n3.c21$Tm_10 > 0,]
Vmax <- Vfind(n3.c21)
Dboun <- distfind(dat=n3.c21, xrange=n3.xran, yrange=n3.yran)
Aweight <- exp(-6.907 + 0.046*Dboun)/(1+exp(-6.907 + 0.046*Dboun))
n3.c21 <- data.frame(n3.c21[,c(14,15,16)], Vmax,Dboun, Aweight)

n2.over <- n2.c21[n2.c21$uniqval %in% n3.c21$uniqval,]
n3.over <- n3.c21[n3.c21$uniqval %in% n2.c21$uniqval,]
sum(abs(n2.over$Xt-n3.over$Xt) + abs(n2.over$Yt-n3.over$Yt))

Vnew <- ((n2.over$Vmax*n2.over$Aweight) + (n3.over$Vmax*n3.over$Aweight))/(n2.over$Aweight + n3.over$Aweight) 

n2.c21$Vmax[n2.c21$uniqval %in% n3.c21$uniqval] <- Vnew
n3.c21$Vmax[n3.c21$uniqval %in% n2.c21$uniqval] <- Vnew

INDEXCOL <- 1:dim(n2.c21)[1]
colvec <- tapply(X=n2.c21$Vmax, INDEX=INDEXCOL, FUN=which.rowcol)

png(filename = "N2_C21.png", width = 2000, height = 2000, units = "px", pointsize = 12, bg = "white",  res = 300,type = "cairo-png")
par(mar=c(2,2,1,1))
plot(Yt ~ Xt, data=n2.c21, pch=15, col=colvec, asp=1, cex=0.17, xlab="", ylab="", bty="n", xaxt="n", yaxt="n", ylim=n2.yran, xlim=n2.xran)
lines(Yt ~ Xt, data=n2.shore, pch=15, col="grey", cex=0.17, type="p")
YCV <- c(5419000,5418600,5419360,5419360,5420200,5420200,5420600,5420600,5420550,5421800,5421800,5421000,5419650,5421600)
XCV <- c(309080,309080,309040,310460,311220,312640,314020,315150,316000,315100,316400,317100,317100,316500)
lines(YCV ~ XCV, pch=3, cex=1, col=2, type="p")
dev.off()

###################

n4.c21 <- read.csv(file='N4/Nest4_C21.csv')
n4.xran <- range(n4.c21$Xt)
n4.yran <- range(n4.c21$Yt)
n4.shore <- n4.c21[n4.c21$Depth<=0,]
n4.c21 <- n4.c21[n4.c21$Ubot != -10 & n4.c21$Depth > 1 & n4.c21$Tm_10 > 0,]
Vmax <- Vfind(n4.c21)
Dboun <- distfind(dat=n4.c21, xrange=n4.xran, yrange=n4.yran)
Aweight <- exp(-6.907 + 0.046*Dboun)/(1+exp(-6.907 + 0.046*Dboun))
n4.c21 <- data.frame(n4.c21[,c(14,15,16)], Vmax,Dboun, Aweight)

n3.over <- n3.c21[n3.c21$uniqval %in% n4.c21$uniqval,]
n4.over <- n4.c21[n4.c21$uniqval %in% n3.c21$uniqval,]
sum(abs(n3.over$Xt-n4.over$Xt) + abs(n3.over$Yt-n4.over$Yt))

Vnew <- ((n3.over$Vmax*n3.over$Aweight) + (n4.over$Vmax*n4.over$Aweight))/(n3.over$Aweight + n4.over$Aweight) 

n3.c21$Vmax[n3.c21$uniqval %in% n4.c21$uniqval] <- Vnew
n4.c21$Vmax[n4.c21$uniqval %in% n3.c21$uniqval] <- Vnew

INDEXCOL <- 1:dim(n3.c21)[1]
colvec <- tapply(X=n3.c21$Vmax, INDEX=INDEXCOL, FUN=which.rowcol)

png(filename = "N3_C21.png", width = 2000, height = 2000, units = "px", pointsize = 12, bg = "white",  res = 300,type = "cairo-png")
par(mar=c(2,2,1,1))
plot(Yt ~ Xt, data=n3.c21, pch=15, col=colvec, asp=1, cex=0.17, xlab="", ylab="", bty="n", xaxt="n", yaxt="n", ylim=n3.yran, xlim=n3.xran)
lines(Yt ~ Xt, data=n3.shore, pch=15, col="grey", cex=0.17, type="p")
YCV <- c(5419000,5418600,5419360,5419360,5420200,5420200,5420600,5420600,5420550,5421800,5421800,5421000,5419650,5421600)
XCV <- c(309080,309080,309040,310460,311220,312640,314020,315150,316000,315100,316400,317100,317100,316500)
lines(YCV ~ XCV, pch=3, cex=1, col=2, type="p")
dev.off()

########

n5.c21 <- read.csv(file='N5/Nest5_C21.csv')
n5.xran <- range(n5.c21$Xt)
n5.yran <- range(n5.c21$Yt)
n5.shore <- n5.c21[n5.c21$Depth<=0,]
n5.c21 <- n5.c21[n5.c21$Ubot != -10 & n5.c21$Depth > 1 & n5.c21$Tm_10 > 0,]
Vmax <- Vfind(n5.c21)
Dboun <- distfind(dat=n5.c21, xrange=n5.xran, yrange=n5.yran)
Aweight <- exp(-6.907 + 0.046*Dboun)/(1+exp(-6.907 + 0.046*Dboun))
n5.c21 <- data.frame(n5.c21[,c(14,15,16)], Vmax,Dboun, Aweight)

n4.over <- n4.c21[n4.c21$uniqval %in% n5.c21$uniqval,]
n5.over <- n5.c21[n5.c21$uniqval %in% n4.c21$uniqval,]
sum(abs(n4.over$Xt-n5.over$Xt) + abs(n4.over$Yt-n5.over$Yt))

Vnew <- ((n4.over$Vmax*n4.over$Aweight) + (n5.over$Vmax*n5.over$Aweight))/(n4.over$Aweight + n5.over$Aweight) 

n4.c21$Vmax[n4.c21$uniqval %in% n5.c21$uniqval] <- Vnew
n5.c21$Vmax[n5.c21$uniqval %in% n4.c21$uniqval] <- Vnew

INDEXCOL <- 1:dim(n4.c21)[1]
colvec <- tapply(X=n4.c21$Vmax, INDEX=INDEXCOL, FUN=which.rowcol)

png(filename = "N4_C21.png", width = 2000, height = 2000, units = "px", pointsize = 12, bg = "white",  res = 300,type = "cairo-png")
par(mar=c(2,2,1,1))
plot(Yt ~ Xt, data=n4.c21, pch=15, col=colvec, asp=1, cex=0.17, xlab="", ylab="", bty="n", xaxt="n", yaxt="n", ylim=n4.yran, xlim=n4.xran)
lines(Yt ~ Xt, data=n4.shore, pch=15, col="grey", cex=0.17, type="p")
YCV <- c(5419000,5418600,5419360,5419360,5420200,5420200,5420600,5420600,5420550,5421800,5421800,5421000,5419650,5421600)
XCV <- c(309080,309080,309040,310460,311220,312640,314020,315150,316000,315100,316400,317100,317100,316500)
lines(YCV ~ XCV, pch=3, cex=1, col=2, type="p")
dev.off()

###

n6.c21 <- read.csv(file='N6/Nest6_C21.csv')
n6.xran <- range(n6.c21$Xt)
n6.yran <- range(n6.c21$Yt)
n6.shore <- n6.c21[n6.c21$Depth<=0,]
n6.c21 <- n6.c21[n6.c21$Ubot != -10 & n6.c21$Depth > 1 & n6.c21$Tm_10 > 0,]
Vmax <- Vfind(n6.c21)
Dboun <- distfind(dat=n6.c21, xrange=n6.xran, yrange=n6.yran)
Aweight <- exp(-6.907 + 0.046*Dboun)/(1+exp(-6.907 + 0.046*Dboun))
n6.c21 <- data.frame(n6.c21[,c(14,15,16)], Vmax,Dboun, Aweight)

n5.over <- n5.c21[n5.c21$uniqval %in% n6.c21$uniqval,]
n6.over <- n6.c21[n6.c21$uniqval %in% n5.c21$uniqval,]
sum(abs(n5.over$Xt-n6.over$Xt) + abs(n5.over$Yt-n6.over$Yt))

Vnew <- ((n5.over$Vmax*n5.over$Aweight) + (n6.over$Vmax*n6.over$Aweight))/(n5.over$Aweight + n6.over$Aweight) 

n5.c21$Vmax[n5.c21$uniqval %in% n6.c21$uniqval] <- Vnew
n6.c21$Vmax[n6.c21$uniqval %in% n5.c21$uniqval] <- Vnew

INDEXCOL <- 1:dim(n5.c21)[1]
colvec <- tapply(X=n5.c21$Vmax, INDEX=INDEXCOL, FUN=which.rowcol)

png(filename = "N5_C21.png", width = 2000, height = 2000, units = "px", pointsize = 12, bg = "white",  res = 300,type = "cairo-png")
par(mar=c(2,2,1,1))
plot(Yt ~ Xt, data=n5.c21, pch=15, col=colvec, asp=1, cex=0.17, xlab="", ylab="", bty="n", xaxt="n", yaxt="n", ylim=n5.yran, xlim=n5.xran)
lines(Yt ~ Xt, data=n5.shore, pch=15, col="grey", cex=0.17, type="p")
YCV <- c(5419000,5418600,5419360,5419360,5420200,5420200,5420600,5420600,5420550,5421800,5421800,5421000,5419650,5421600)
XCV <- c(309080,309080,309040,310460,311220,312640,314020,315150,316000,315100,316400,317100,317100,316500)
lines(YCV ~ XCV, pch=3, cex=1, col=2, type="p")
dev.off()

###########
n7.c21 <- read.csv(file='N7/Nest7_C21.csv')
n7.xran <- range(n7.c21$Xt)
n7.yran <- range(n7.c21$Yt)
n7.shore <- n7.c21[n7.c21$Depth<=0,]
n7.c21 <- n7.c21[n7.c21$Ubot != -10 & n7.c21$Depth > 1 & n7.c21$Tm_10 > 0,]
Vmax <- Vfind(n7.c21)
Dboun <- distfind(dat=n7.c21, xrange=n7.xran, yrange=n7.yran)
Aweight <- exp(-6.907 + 0.046*Dboun)/(1+exp(-6.907 + 0.046*Dboun))
n7.c21 <- data.frame(n7.c21[,c(14,15,16)], Vmax,Dboun, Aweight)

n6.over <- n6.c21[n6.c21$uniqval %in% n7.c21$uniqval,]
n7.over <- n7.c21[n7.c21$uniqval %in% n6.c21$uniqval,]
sum(abs(n6.over$Xt-n7.over$Xt) + abs(n6.over$Yt-n7.over$Yt))

Vnew <- ((n6.over$Vmax*n6.over$Aweight) + (n7.over$Vmax*n7.over$Aweight))/(n6.over$Aweight + n7.over$Aweight) 

n6.c21$Vmax[n6.c21$uniqval %in% n7.c21$uniqval] <- Vnew
n7.c21$Vmax[n7.c21$uniqval %in% n6.c21$uniqval] <- Vnew

INDEXCOL <- 1:dim(n6.c21)[1]
colvec <- tapply(X=n6.c21$Vmax, INDEX=INDEXCOL, FUN=which.rowcol)

png(filename = "N6_C21.png", width = 2000, height = 2000, units = "px", pointsize = 12, bg = "white",  res = 300,type = "cairo-png")
par(mar=c(2,2,1,1))
plot(Yt ~ Xt, data=n6.c21, pch=15, col=colvec, asp=1, cex=0.17, xlab="", ylab="", bty="n", xaxt="n", yaxt="n", ylim=n6.yran, xlim=n6.xran)
lines(Yt ~ Xt, data=n6.shore, pch=15, col="grey", cex=0.17, type="p")
YCV <- c(5419000,5418600,5419360,5419360,5420200,5420200,5420600,5420600,5420550,5421800,5421800,5421000,5419650,5421600)
XCV <- c(309080,309080,309040,310460,311220,312640,314020,315150,316000,315100,316400,317100,317100,316500)
lines(YCV ~ XCV, pch=3, cex=1, col=2, type="p")
dev.off()

#####################
#####################
#####################

n7.c21 <- read.csv(file='N7/Nest7_C21.csv',stringsAsFactors =FALSE)
n7.xran <- range(n7.c21$Xt)
n7.yran <- range(n7.c21$Yt)
n7.shore <- n7.c21[n7.c21$Depth<=0,]
n7.c21 <- n7.c21[n7.c21$Ubot != -10 & n7.c21$Depth > 1 & n7.c21$Tm_10 > 0,]
Vmax <- Vfind(n7.c21)
Dboun <- distfind(dat=n7.c21, xrange=n7.xran, yrange=n7.yran)
Aweight <- exp(-6.907 + 0.046*Dboun)/(1+exp(-6.907 + 0.046*Dboun))
VmWeight <- Vmax*Aweight
n7.c21 <- data.frame(n7.c21[,c(14,15,16)], Vmax, Aweight, VmWeight)

n8.c21 <- read.csv(file='N8/Nest8_C21.csv',stringsAsFactors =FALSE)
n8.xran <- range(n8.c21$Xt)
n8.yran <- range(n8.c21$Yt)
n8.shore <- n8.c21[n8.c21$Depth<=0,]
n8.c21 <- n8.c21[n8.c21$Ubot != -10 & n8.c21$Depth > 1 & n8.c21$Tm_10 > 0,]
Vmax <- Vfind(n8.c21)
Dboun <- distfind(dat=n8.c21, xrange=n8.xran, yrange=n8.yran)
Aweight <- exp(-6.907 + 0.046*Dboun)/(1+exp(-6.907 + 0.046*Dboun))
VmWeight <- Vmax*Aweight
n8.c21 <- data.frame(n8.c21[,c(14,15,16)], Vmax, Aweight, VmWeight)

n9.c21 <- read.csv(file='N9/Nest9_C21.csv',stringsAsFactors =FALSE)
n9.xran <- range(n9.c21$Xt)
n9.yran <- range(n9.c21$Yt)
n9.shore <- n9.c21[n9.c21$Depth<=0,]
n9.c21 <- n9.c21[n9.c21$Ubot != -10 & n9.c21$Depth > 1 & n9.c21$Tm_10 > 0,]
Vmax <- Vfind(n9.c21)
Dboun <- distfind(dat=n9.c21, xrange=n9.xran, yrange=n9.yran)
Aweight <- exp(-6.907 + 0.046*Dboun)/(1+exp(-6.907 + 0.046*Dboun))
VmWeight <- Vmax*Aweight
n9.c21 <- data.frame(n9.c21[,c(14,15,16)], Vmax, Aweight, VmWeight)

n10.c21 <- read.csv(file='N10/Nest10_C21.csv',stringsAsFactors =FALSE)
n10.xran <- range(n10.c21$Xt)
n10.yran <- range(n10.c21$Yt)
n10.shore <- n10.c21[n10.c21$Depth<=0,]
n10.c21 <- n10.c21[n10.c21$Ubot != -10 & n10.c21$Depth > 1 & n10.c21$Tm_10 > 0,]
Vmax <- Vfind(n10.c21)
Dboun <- distfind(dat=n10.c21, xrange=n10.xran, yrange=n10.yran)
Aweight <- exp(-6.907 + 0.046*Dboun)/(1+exp(-6.907 + 0.046*Dboun))
VmWeight <- Vmax*Aweight
n10.c21 <- data.frame(n10.c21[,c(14,15,16)], Vmax, Aweight, VmWeight)

n11.c21 <- read.csv(file='N11/Nest11_C21.csv',stringsAsFactors =FALSE)
n11.xran <- range(n11.c21$Xt)
n11.yran <- range(n11.c21$Yt)
n11.shore <- n11.c21[n11.c21$Depth<=0,]
n11.c21 <- n11.c21[n11.c21$Ubot != -10 & n11.c21$Depth > 1 & n11.c21$Tm_10 > 0,]
Vmax <- Vfind(n11.c21)
Dboun <- distfind(dat=n11.c21, xrange=n11.xran, yrange=n11.yran)
Aweight <- exp(-6.907 + 0.046*Dboun)/(1+exp(-6.907 + 0.046*Dboun))
VmWeight <- Vmax*Aweight
n11.c21 <- data.frame(n11.c21[,c(14,15,16)], Vmax, Aweight, VmWeight)

n12.c21 <- read.csv(file='N12/Nest12_C21.csv',stringsAsFactors =FALSE)
n12.xran <- range(n12.c21$Xt)
n12.yran <- range(n12.c21$Yt)
n12.shore <- n12.c21[n12.c21$Depth<=0,]
n12.c21 <- n12.c21[n12.c21$Ubot != -10 & n12.c21$Depth > 1 & n12.c21$Tm_10 > 0,]
Vmax <- Vfind(n12.c21)
Dboun <- distfind(dat=n12.c21, xrange=n12.xran, yrange=n12.yran)
Aweight <- exp(-6.907 + 0.046*Dboun)/(1+exp(-6.907 + 0.046*Dboun))
VmWeight <- Vmax*Aweight
n12.c21 <- data.frame(n12.c21[,c(14,15,16)], Vmax, Aweight, VmWeight)

nlyall <- rbind(n7.c21, n8.c21, n9.c21, n10.c21, n11.c21, n12.c21)

vmprelim <- tapply(X=nlyall$VmWeight, INDEX=nlyall$uniqval, FUN=sum)
weightprelim <- tapply(X=nlyall$Aweight, INDEX=nlyall$uniqval, FUN=sum)

LyallVmax <- vmprelim/weightprelim
coordstring <- rownames(LyallVmax)
thy <- unlist(strsplit(coordstring, split="_"))
mat_coord <- matrix(as.numeric(thy), nrow=length(LyallVmax), ncol=2, byrow=TRUE)

LyallDat <- data.frame(mat_coord, LyallVmax)
names(LyallDat) <- c("Xt", "Yt", "Vmax")

n7.dat <- LyallDat[LyallDat$Xt >= n7.xran[1] & LyallDat$Xt <= n7.xran[2] & LyallDat$Yt >= n7.yran[1] & LyallDat$Yt <= n7.yran[2],]
n8.dat <- LyallDat[LyallDat$Xt >= n8.xran[1] & LyallDat$Xt <= n8.xran[2] & LyallDat$Yt >= n8.yran[1] & LyallDat$Yt <= n8.yran[2],]
n9.dat <- LyallDat[LyallDat$Xt >= n9.xran[1] & LyallDat$Xt <= n9.xran[2] & LyallDat$Yt >= n9.yran[1] & LyallDat$Yt <= n9.yran[2],]
n10.dat <- LyallDat[LyallDat$Xt >= n10.xran[1] & LyallDat$Xt <= n10.xran[2] & LyallDat$Yt >= n10.yran[1] & LyallDat$Yt <= n10.yran[2],]
n11.dat <- LyallDat[LyallDat$Xt >= n11.xran[1] & LyallDat$Xt <= n11.xran[2] & LyallDat$Yt >= n11.yran[1] & LyallDat$Yt <= n11.yran[2],]
n12.dat <- LyallDat[LyallDat$Xt >= n12.xran[1] & LyallDat$Xt <= n12.xran[2] & LyallDat$Yt >= n12.yran[1] & LyallDat$Yt <= n12.yran[2],]



INDEXCOL <- 1:dim(n7.dat)[1]
colvec <- tapply(X=n7.dat$Vmax, INDEX=INDEXCOL, FUN=which.rowcol)
png(filename = "N7_C21.png", width = 2000, height = 2000, units = "px", pointsize = 12, bg = "white",  res = 300,type = "cairo-png")
par(mar=c(2,2,1,1))
plot(Yt ~ Xt, data=n7.dat, pch=15, col=colvec, asp=1, cex=0.17, xlab="", ylab="", bty="n", xaxt="n", yaxt="n", ylim=n7.yran, xlim=n7.xran)
lines(Yt ~ Xt, data=n7.shore, pch=15, col="grey", cex=0.17, type="p")
YCV <- c(5419000,5418600,5419360,5419360,5420200,5420200,5420600,5420600,5420550,5421800,5421800,5421000,5419650,5421600)
XCV <- c(309080,309080,309040,310460,311220,312640,314020,315150,316000,315100,316400,317100,317100,316500)
lines(YCV ~ XCV, pch=3, cex=1, col=2, type="p")
dev.off()

INDEXCOL <- 1:dim(n8.dat)[1]
colvec <- tapply(X=n8.dat$Vmax, INDEX=INDEXCOL, FUN=which.rowcol)
png(filename = "N8_C21.png", width = 2000, height = 2000, units = "px", pointsize = 12, bg = "white",  res = 300,type = "cairo-png")
par(mar=c(2,2,1,1))
plot(Yt ~ Xt, data=n8.dat, pch=15, col=colvec, asp=1, cex=0.17, xlab="", ylab="", bty="n", xaxt="n", yaxt="n", ylim=n8.yran, xlim=n8.xran)
lines(Yt ~ Xt, data=n8.shore, pch=15, col="grey", cex=0.17, type="p")
YCV <- c(5419000,5418600,5419360,5419360,5420200,5420200,5420600,5420600,5420550,5421800,5421800,5421000,5419650,5421600)
XCV <- c(309080,309080,309040,310460,311220,312640,314020,315150,316000,315100,316400,317100,317100,316500)
lines(YCV ~ XCV, pch=3, cex=1, col=2, type="p")
dev.off()

INDEXCOL <- 1:dim(n9.dat)[1]
colvec <- tapply(X=n9.dat$Vmax, INDEX=INDEXCOL, FUN=which.rowcol)
png(filename = "N9_C21.png", width = 2000, height = 2000, units = "px", pointsize = 12, bg = "white",  res = 300,type = "cairo-png")
par(mar=c(2,2,1,1))
plot(Yt ~ Xt, data=n9.dat, pch=15, col=colvec, asp=1, cex=0.17, xlab="", ylab="", bty="n", xaxt="n", yaxt="n", ylim=n9.yran, xlim=n9.xran)
lines(Yt ~ Xt, data=n9.shore, pch=15, col="grey", cex=0.17, type="p")
YCV <- c(5419000,5418600,5419360,5419360,5420200,5420200,5420600,5420600,5420550,5421800,5421800,5421000,5419650,5421600)
XCV <- c(309080,309080,309040,310460,311220,312640,314020,315150,316000,315100,316400,317100,317100,316500)
lines(YCV ~ XCV, pch=3, cex=1, col=2, type="p")
dev.off()

INDEXCOL <- 1:dim(n10.dat)[1]
colvec <- tapply(X=n10.dat$Vmax, INDEX=INDEXCOL, FUN=which.rowcol)
png(filename = "N10_C21.png", width = 2000, height = 2000, units = "px", pointsize = 12, bg = "white",  res = 300,type = "cairo-png")
par(mar=c(2,2,1,1))
plot(Yt ~ Xt, data=n10.dat, pch=15, col=colvec, asp=1, cex=0.17, xlab="", ylab="", bty="n", xaxt="n", yaxt="n", ylim=n10.yran, xlim=n10.xran)
lines(Yt ~ Xt, data=n10.shore, pch=15, col="grey", cex=0.17, type="p")
YCV <- c(5419000,5418600,5419360,5419360,5420200,5420200,5420600,5420600,5420550,5421800,5421800,5421000,5419650,5421600)
XCV <- c(309080,309080,309040,310460,311220,312640,314020,315150,316000,315100,316400,317100,317100,316500)
lines(YCV ~ XCV, pch=3, cex=1, col=2, type="p")
dev.off()

INDEXCOL <- 1:dim(n11.dat)[1]
colvec <- tapply(X=n11.dat$Vmax, INDEX=INDEXCOL, FUN=which.rowcol)
png(filename = "N11_C21.png", width = 2000, height = 2000, units = "px", pointsize = 12, bg = "white",  res = 300,type = "cairo-png")
par(mar=c(2,2,1,1))
plot(Yt ~ Xt, data=n11.dat, pch=15, col=colvec, asp=1, cex=0.17, xlab="", ylab="", bty="n", xaxt="n", yaxt="n", ylim=n11.yran, xlim=n11.xran)
lines(Yt ~ Xt, data=n11.shore, pch=15, col="grey", cex=0.17, type="p")
YCV <- c(5419000,5418600,5419360,5419360,5420200,5420200,5420600,5420600,5420550,5421800,5421800,5421000,5419650,5421600)
XCV <- c(309080,309080,309040,310460,311220,312640,314020,315150,316000,315100,316400,317100,317100,316500)
lines(YCV ~ XCV, pch=3, cex=1, col=2, type="p")
dev.off()

INDEXCOL <- 1:dim(n12.dat)[1]
colvec <- tapply(X=n12.dat$Vmax, INDEX=INDEXCOL, FUN=which.rowcol)
png(filename = "N12_C21.png", width = 2000, height = 2000, units = "px", pointsize = 12, bg = "white",  res = 300,type = "cairo-png")
par(mar=c(2,2,1,1))
plot(Yt ~ Xt, data=n12.dat, pch=15, col=colvec, asp=1, cex=0.17, xlab="", ylab="", bty="n", xaxt="n", yaxt="n", ylim=n12.yran, xlim=n12.xran)
lines(Yt ~ Xt, data=n12.shore, pch=15, col="grey", cex=0.17, type="p")
YCV <- c(5419000,5418600,5419360,5419360,5420200,5420200,5420600,5420600,5420550,5421800,5421800,5421000,5419650,5421600)
XCV <- c(309080,309080,309040,310460,311220,312640,314020,315150,316000,315100,316400,317100,317100,316500)
lines(YCV ~ XCV, pch=3, cex=1, col=2, type="p")
dev.off()


#####################

n12.c21 <- read.csv(file='N12/Nest12_C21.csv',stringsAsFactors =FALSE)
n12.xran <- range(n12.c21$Xt)
n12.yran <- range(n12.c21$Yt)
n12.shore <- n12.c21[n12.c21$Depth<=0,]
n12.c21 <- n12.c21[n12.c21$Ubot != -10 & n12.c21$Depth > 1 & n12.c21$Tm_10 > 0,]
Vmax <- Vfind(n12.c21)
Dboun <- distfind(dat=n12.c21, xrange=n12.xran, yrange=n12.yran)
Aweight <- exp(-6.907 + 0.046*Dboun)/(1+exp(-6.907 + 0.046*Dboun))
n12.c21 <- data.frame(n12.c21[,c(14,15,16)], Vmax,Dboun, Aweight)

n13.c21 <- read.csv(file='N13/Nest13_C21.csv',stringsAsFactors =FALSE)
n13.xran <- range(n13.c21$Xt)
n13.yran <- range(n13.c21$Yt)
n13.shore <- n13.c21[n13.c21$Depth<=0,]
n13.c21 <- n13.c21[n13.c21$Ubot != -10 & n13.c21$Depth > 1 & n13.c21$Tm_10 > 0,]
Vmax <- Vfind(n13.c21)
Dboun <- distfind(dat=n13.c21, xrange=n13.xran, yrange=n13.yran)
Aweight <- exp(-6.907 + 0.046*Dboun)/(1+exp(-6.907 + 0.046*Dboun))
n13.c21 <- data.frame(n13.c21[,c(14,15,16)], Vmax,Dboun, Aweight)

n12.over <- n12.c21[n12.c21$uniqval %in% n13.c21$uniqval,]
n13.over <- n13.c21[n13.c21$uniqval %in% n12.c21$uniqval,]
sum(abs(n12.over$Xt-n13.over$Xt) + abs(n12.over$Yt-n13.over$Yt))

Vnew <- ((n12.over$Vmax*n12.over$Aweight) + (n13.over$Vmax*n13.over$Aweight))/(n12.over$Aweight + n13.over$Aweight) 

n12.c21$Vmax[n12.c21$uniqval %in% n13.c21$uniqval] <- Vnew
n13.c21$Vmax[n13.c21$uniqval %in% n12.c21$uniqval] <- Vnew

###

n14.c21 <- read.csv(file='N14/Nest14_C21.csv')
n14.xran <- range(n14.c21$Xt)
n14.yran <- range(n14.c21$Yt)
n14.shore <- n14.c21[n14.c21$Depth<=0,]
n14.c21 <- n14.c21[n14.c21$Ubot != -10 & n14.c21$Depth > 1 & n14.c21$Tm_10 > 0,]
Vmax <- Vfind(n14.c21)
Dboun <- distfind(dat=n14.c21, xrange=n14.xran, yrange=n14.yran)
Aweight <- exp(-6.907 + 0.046*Dboun)/(1+exp(-6.907 + 0.046*Dboun))
n14.c21 <- data.frame(n14.c21[,c(14,15,16)], Vmax,Dboun, Aweight)

n13.over <- n13.c21[n13.c21$uniqval %in% n14.c21$uniqval,]
n14.over <- n14.c21[n14.c21$uniqval %in% n13.c21$uniqval,]
sum(abs(n13.over$Xt-n14.over$Xt) + abs(n13.over$Yt-n14.over$Yt))

Vnew <- ((n13.over$Vmax*n13.over$Aweight) + (n14.over$Vmax*n14.over$Aweight))/(n13.over$Aweight + n14.over$Aweight) 

n13.c21$Vmax[n13.c21$uniqval %in% n14.c21$uniqval] <- Vnew
n14.c21$Vmax[n14.c21$uniqval %in% n13.c21$uniqval] <- Vnew

INDEXCOL <- 1:dim(n13.c21)[1]
colvec <- tapply(X=n13.c21$Vmax, INDEX=INDEXCOL, FUN=which.rowcol)

png(filename = "N13_C21.png", width = 2000, height = 2000, units = "px", pointsize = 12, bg = "white",  res = 300,type = "cairo-png")
par(mar=c(2,2,1,1))
plot(Yt ~ Xt, data=n13.c21, pch=15, col=colvec, asp=1, cex=0.17, xlab="", ylab="", bty="n", xaxt="n", yaxt="n", ylim=n13.yran, xlim=n13.xran)
lines(Yt ~ Xt, data=n13.shore, pch=15, col="grey", cex=0.17, type="p")
YCV <- c(5419000,5418600,5419360,5419360,5420200,5420200,5420600,5420600,5420550,5421800,5421800,5421000,5419650,5421600)
XCV <- c(309080,309080,309040,310460,311220,312640,314020,315150,316000,315100,316400,317100,317100,316500)
lines(YCV ~ XCV, pch=3, cex=1, col=2, type="p")
dev.off()


INDEXCOL <- 1:dim(n14.c21)[1]
colvec <- tapply(X=n14.c21$Vmax, INDEX=INDEXCOL, FUN=which.rowcol)

png(filename = "N14_C21.png", width = 2000, height = 2000, units = "px", pointsize = 12, bg = "white",  res = 300,type = "cairo-png")
par(mar=c(2,2,1,1))
plot(Yt ~ Xt, data=n14.c21, pch=15, col=colvec, asp=1, cex=0.17, xlab="", ylab="", bty="n", xaxt="n", yaxt="n", ylim=n14.yran, xlim=n14.xran)
lines(Yt ~ Xt, data=n14.shore, pch=15, col="grey", cex=0.17, type="p")
YCV <- c(5419000,5418600,5419360,5419360,5420200,5420200,5420600,5420600,5420550,5421800,5421800,5421000,5419650,5421600)
XCV <- c(309080,309080,309040,310460,311220,312640,314020,315150,316000,315100,316400,317100,317100,316500)
lines(YCV ~ XCV, pch=3, cex=1, col=2, type="p")
dev.off()


######################################################################
######################################################################
#### Summary statistics of orbital velocity #############

get.char <- function(dat){
	depth.ret <- dat[1]
	return(depth.ret)
}

profinder <- function(dat, subs.dat){
	denom <- length(dat)
	class1 <- length(dat[dat < 0.5])/denom
	class2 <- length(dat[dat >= 0.5 & dat < 1])/denom
	class3 <- length(dat[dat >=1 & dat < 1.5])/denom
	class4 <- length(dat[dat >= 1.5 & dat < 2])/denom
	class5 <- length(dat[dat >= 2 & dat < 2.5])/denom
	class6 <- length(dat[dat >= 2.5 & dat < 3])/denom
	class7 <- max(dat)
	class8 <- median(dat)
	class9 <- dat[rank(dat, ties.method="random")== floor(0.05*denom)]
	class10 <- dat[rank(dat, ties.method="random")== floor(0.95*denom)]

	dat2 <- dat[subs.dat=="Rock"]
	denom2 <- length(dat2)
	class21 <- length(dat2[dat2 < 0.5])/denom2
	class22 <- length(dat2[dat2 >= 0.5 & dat2 < 1])/denom2
	class23 <- length(dat2[dat2 >=1 & dat2 < 1.5])/denom2
	class24 <- length(dat2[dat2 >= 1.5 & dat2 < 2])/denom2
	class25 <- length(dat2[dat2 >= 2 & dat2 < 2.5])/denom2
	class26 <- length(dat2[dat2 >= 2.5 & dat2 < 3])/denom2
	class27 <- max(dat2)
	class28 <- median(dat2)
	class29 <- dat2[rank(dat2, ties.method="random")== floor(0.05*denom2)]
	class210 <- dat2[rank(dat2, ties.method="random")== floor(0.95*denom2)]
	
	xc1 <- c(class1, class2, class3, class4, class5, class6, class7, class8, class9, class10)
	xc2 <- c(class21, class22, class23, class24, class25, class26, class27,class28, class29, class210)
	
	ret.dat <- rbind(xc1, xc2)
	return(ret.dat)

}


setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale")
n1.c18 <- read.csv(file='N1/Nest1_C18.csv',stringsAsFactors=FALSE)
n1.c18 <- n1.c18[n1.c18$Ubot != -10 & n1.c18$Depth <= 20 & n1.c18$Depth >= 1 & n1.c18$Tm_10 > 0,c(3,7,8,12,13,16)]
Vmax <- Vfind(n1.c18)
n1.c18 <- data.frame(n1.c18[,c(3,5,6)], Vmax)
n2.c18 <- read.csv(file='N2/Nest2_C18.csv',stringsAsFactors=FALSE)
n2.c18 <- n2.c18[n2.c18$Ubot != -10 & n2.c18$Depth <= 20 & n2.c18$Depth >= 1 & n2.c18$Tm_10 > 0,c(3,7,8,12,13,16)]
Vmax <- Vfind(n2.c18)
n2.c18 <- data.frame(n2.c18[,c(3,5,6)], Vmax)
n3.c18 <- read.csv(file='N3/Nest3_C18.csv',stringsAsFactors=FALSE)
n3.c18 <- n3.c18[n3.c18$Ubot != -10 & n3.c18$Depth <= 20 & n3.c18$Depth >= 1 & n3.c18$Tm_10 > 0,c(3,7,8,12,13,16)]
Vmax <- Vfind(n3.c18)
n3.c18 <- data.frame(n3.c18[,c(3,5,6)], Vmax)
n4.c18 <- read.csv(file='N4/Nest4_C18.csv',stringsAsFactors=FALSE)
n4.c18 <- n4.c18[n4.c18$Ubot != -10 & n4.c18$Depth <= 20 & n4.c18$Depth >= 1 & n4.c18$Tm_10 > 0,c(3,7,8,12,13,16)]
Vmax <- Vfind(n4.c18)
n4.c18 <- data.frame(n4.c18[,c(3,5,6)], Vmax)
n5.c18 <- read.csv(file='N5/Nest5_C18.csv',stringsAsFactors=FALSE)
n5.c18 <- n5.c18[n5.c18$Ubot != -10 & n5.c18$Depth <= 20 & n5.c18$Depth >= 1 & n5.c18$Tm_10 > 0,c(3,7,8,12,13,16)]
Vmax <- Vfind(n5.c18)
n5.c18 <- data.frame(n5.c18[,c(3,5,6)], Vmax)
n6.c18 <- read.csv(file='N6/Nest6_C18.csv',stringsAsFactors=FALSE)
n6.c18 <- n6.c18[n6.c18$Ubot != -10 & n6.c18$Depth <= 20 & n6.c18$Depth >= 1 & n6.c18$Tm_10 > 0,c(3,7,8,12,13,16)]
Vmax <- Vfind(n6.c18)
n6.c18 <- data.frame(n6.c18[,c(3,5,6)], Vmax)
n7.c18 <- read.csv(file='N7/Nest7_C18.csv',stringsAsFactors=FALSE)
n7.c18 <- n7.c18[n7.c18$Ubot != -10 & n7.c18$Depth <= 20 & n7.c18$Depth >= 1 & n7.c18$Tm_10 > 0,c(3,7,8,12,13,16)]
Vmax <- Vfind(n7.c18)
n7.c18 <- data.frame(n7.c18[,c(3,5,6)], Vmax)
n8.c18 <- read.csv(file='N8/Nest8_C18.csv',stringsAsFactors=FALSE)
n8.c18 <- n8.c18[n8.c18$Ubot != -10 & n8.c18$Depth <= 20 & n8.c18$Depth >= 1 & n8.c18$Tm_10 > 0,c(3,7,8,12,13,16)]
Vmax <- Vfind(n8.c18)
n8.c18 <- data.frame(n8.c18[,c(3,5,6)], Vmax)
n9.c18 <- read.csv(file='N9/Nest9_C18.csv',stringsAsFactors=FALSE)
n9.c18 <- n9.c18[n9.c18$Ubot != -10 & n9.c18$Depth <= 20 & n9.c18$Depth >= 1 & n9.c18$Tm_10 > 0,c(3,7,8,12,13,16)]
Vmax <- Vfind(n9.c18)
n9.c18 <- data.frame(n9.c18[,c(3,5,6)], Vmax)
n10.c18 <- read.csv(file='N10/Nest10_C18.csv',stringsAsFactors=FALSE)
n10.c18 <- n10.c18[n10.c18$Ubot != -10 & n10.c18$Depth <= 20 & n10.c18$Depth >= 1 & n10.c18$Tm_10 > 0,c(3,7,8,12,13,16)]
Vmax <- Vfind(n10.c18)
n10.c18 <- data.frame(n10.c18[,c(3,5,6)], Vmax)
n11.c18 <- read.csv(file='N11/Nest11_C18.csv',stringsAsFactors=FALSE)
n11.c18 <- n11.c18[n11.c18$Ubot != -10 & n11.c18$Depth <= 20 & n11.c18$Depth >= 1 & n11.c18$Tm_10 > 0,c(3,7,8,12,13,16)]
Vmax <- Vfind(n11.c18)
n11.c18 <- data.frame(n11.c18[,c(3,5,6)], Vmax)
n12.c18 <- read.csv(file='N12/Nest12_C18.csv',stringsAsFactors=FALSE)
n12.c18 <- n12.c18[n12.c18$Ubot != -10 & n12.c18$Depth <= 20 & n12.c18$Depth >= 1 & n12.c18$Tm_10 > 0,c(3,7,8,12,13,16)]
Vmax <- Vfind(n12.c18)
n12.c18 <- data.frame(n12.c18[,c(3,5,6)], Vmax)
n13.c18 <- read.csv(file='N13/Nest13_C18.csv',stringsAsFactors=FALSE)
n13.c18 <- n13.c18[n13.c18$Ubot != -10 & n13.c18$Depth <= 20 & n13.c18$Depth >= 1 & n13.c18$Tm_10 > 0,c(3,7,8,12,13,16)]
Vmax <- Vfind(n13.c18)
n13.c18 <- data.frame(n13.c18[,c(3,5,6)], Vmax)
n14.c18 <- read.csv(file='N14/Nest14_C18.csv',stringsAsFactors=FALSE)
n14.c18 <- n14.c18[n14.c18$Ubot != -10 & n14.c18$Depth <= 20 & n14.c18$Depth >= 1 & n14.c18$Tm_10 > 0,c(3,7,8,12,13,16)]
Vmax <- Vfind(n14.c18)
n14.c18 <- data.frame(n14.c18[,c(3,5,6)], Vmax)

c18.dat <- rbind(n1.c18, n2.c18, n3.c18, n4.c18, n5.c18, n6.c18, n7.c18, n8.c18, n9.c18, n10.c18, n11.c18, n12.c18, n13.c18, n14.c18) 

c18.ubot <- tapply(X=c18.dat$Vmax, INDEX=c18.dat$uniqval, FUN=mean)
c18.depth <- tapply(X=c18.dat$Depth, INDEX=c18.dat$uniqval, FUN=get.char)
c18.substrate <- tapply(X=c18.dat$Substrate, INDEX=c18.dat$uniqval, FUN=get.char)

c18_data <- data.frame(uniq=names(c18.ubot), Ubot=c18.ubot, Depth=c18.depth, Substrate=c18.substrate)

vel.0to5_c18 <- c18_data[c18_data$Depth <= 5,]
vel.5to10_c18 <- c18_data[c18_data$Depth <= 10 & c18_data$Depth > 5,]
vel.10to15_c18 <- c18_data[c18_data$Depth <= 15 & c18_data$Depth > 10,]
vel.15to20_c18 <- c18_data[c18_data$Depth <= 20 & c18_data$Depth > 15,]

profinder(dat=vel.0to5_c18$Ubot, subs.dat=vel.0to5_c18$Substrate)

####################
#C21

setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale")
n1.c21 <- read.csv(file='N1/Nest1_C21.csv',stringsAsFactors=FALSE)
n1.c21 <- n1.c21[n1.c21$Ubot != -10 & n1.c21$Depth <= 20 & n1.c21$Depth >= 1 & n1.c21$Tm_10 > 0,c(3,7,8,12,13,16)]
Vmax <- Vfind(n1.c21)
n1.c21 <- data.frame(n1.c21[,c(3,5,6)], Vmax)
n2.c21 <- read.csv(file='N2/Nest2_C21.csv',stringsAsFactors=FALSE)
n2.c21 <- n2.c21[n2.c21$Ubot != -10 & n2.c21$Depth <= 20 & n2.c21$Depth >= 1 & n2.c21$Tm_10 > 0,c(3,7,8,12,13,16)]
Vmax <- Vfind(n2.c21)
n2.c21 <- data.frame(n2.c21[,c(3,5,6)], Vmax)
n3.c21 <- read.csv(file='N3/Nest3_C21.csv',stringsAsFactors=FALSE)
n3.c21 <- n3.c21[n3.c21$Ubot != -10 & n3.c21$Depth <= 20 & n3.c21$Depth >= 1 & n3.c21$Tm_10 > 0,c(3,7,8,12,13,16)]
Vmax <- Vfind(n3.c21)
n3.c21 <- data.frame(n3.c21[,c(3,5,6)], Vmax)
n4.c21 <- read.csv(file='N4/Nest4_C21.csv',stringsAsFactors=FALSE)
n4.c21 <- n4.c21[n4.c21$Ubot != -10 & n4.c21$Depth <= 20 & n4.c21$Depth >= 1 & n4.c21$Tm_10 > 0,c(3,7,8,12,13,16)]
Vmax <- Vfind(n4.c21)
n4.c21 <- data.frame(n4.c21[,c(3,5,6)], Vmax)
n5.c21 <- read.csv(file='N5/Nest5_C21.csv',stringsAsFactors=FALSE)
n5.c21 <- n5.c21[n5.c21$Ubot != -10 & n5.c21$Depth <= 20 & n5.c21$Depth >= 1 & n5.c21$Tm_10 > 0,c(3,7,8,12,13,16)]
Vmax <- Vfind(n5.c21)
n5.c21 <- data.frame(n5.c21[,c(3,5,6)], Vmax)
n6.c21 <- read.csv(file='N6/Nest6_C21.csv',stringsAsFactors=FALSE)
n6.c21 <- n6.c21[n6.c21$Ubot != -10 & n6.c21$Depth <= 20 & n6.c21$Depth >= 1 & n6.c21$Tm_10 > 0,c(3,7,8,12,13,16)]
Vmax <- Vfind(n6.c21)
n6.c21 <- data.frame(n6.c21[,c(3,5,6)], Vmax)
n7.c21 <- read.csv(file='N7/Nest7_C21.csv',stringsAsFactors=FALSE)
n7.c21 <- n7.c21[n7.c21$Ubot != -10 & n7.c21$Depth <= 20 & n7.c21$Depth >= 1 & n7.c21$Tm_10 > 0,c(3,7,8,12,13,16)]
Vmax <- Vfind(n7.c21)
n7.c21 <- data.frame(n7.c21[,c(3,5,6)], Vmax)
n8.c21 <- read.csv(file='N8/Nest8_C21.csv',stringsAsFactors=FALSE)
n8.c21 <- n8.c21[n8.c21$Ubot != -10 & n8.c21$Depth <= 20 & n8.c21$Depth >= 1 & n8.c21$Tm_10 > 0,c(3,7,8,12,13,16)]
Vmax <- Vfind(n8.c21)
n8.c21 <- data.frame(n8.c21[,c(3,5,6)], Vmax)
n9.c21 <- read.csv(file='N9/Nest9_C21.csv',stringsAsFactors=FALSE)
n9.c21 <- n9.c21[n9.c21$Ubot != -10 & n9.c21$Depth <= 20 & n9.c21$Depth >= 1 & n9.c21$Tm_10 > 0,c(3,7,8,12,13,16)]
Vmax <- Vfind(n9.c21)
n9.c21 <- data.frame(n9.c21[,c(3,5,6)], Vmax)
n10.c21 <- read.csv(file='N10/Nest10_C21.csv',stringsAsFactors=FALSE)
n10.c21 <- n10.c21[n10.c21$Ubot != -10 & n10.c21$Depth <= 20 & n10.c21$Depth >= 1 & n10.c21$Tm_10 > 0,c(3,7,8,12,13,16)]
Vmax <- Vfind(n10.c21)
n10.c21 <- data.frame(n10.c21[,c(3,5,6)], Vmax)
n11.c21 <- read.csv(file='N11/Nest11_C21.csv',stringsAsFactors=FALSE)
n11.c21 <- n11.c21[n11.c21$Ubot != -10 & n11.c21$Depth <= 20 & n11.c21$Depth >= 1 & n11.c21$Tm_10 > 0,c(3,7,8,12,13,16)]
Vmax <- Vfind(n11.c21)
n11.c21 <- data.frame(n11.c21[,c(3,5,6)], Vmax)
n12.c21 <- read.csv(file='N12/Nest12_C21.csv',stringsAsFactors=FALSE)
n12.c21 <- n12.c21[n12.c21$Ubot != -10 & n12.c21$Depth <= 20 & n12.c21$Depth >= 1 & n12.c21$Tm_10 > 0,c(3,7,8,12,13,16)]
Vmax <- Vfind(n12.c21)
n12.c21 <- data.frame(n12.c21[,c(3,5,6)], Vmax)
n13.c21 <- read.csv(file='N13/Nest13_C21.csv',stringsAsFactors=FALSE)
n13.c21 <- n13.c21[n13.c21$Ubot != -10 & n13.c21$Depth <= 20 & n13.c21$Depth >= 1 & n13.c21$Tm_10 > 0,c(3,7,8,12,13,16)]
Vmax <- Vfind(n13.c21)
n13.c21 <- data.frame(n13.c21[,c(3,5,6)], Vmax)
n14.c21 <- read.csv(file='N14/Nest14_C21.csv',stringsAsFactors=FALSE)
n14.c21 <- n14.c21[n14.c21$Ubot != -10 & n14.c21$Depth <= 20 & n14.c21$Depth >= 1 & n14.c21$Tm_10 > 0,c(3,7,8,12,13,16)]
Vmax <- Vfind(n14.c21)
n14.c21 <- data.frame(n14.c21[,c(3,5,6)], Vmax)

c21.dat <- rbind(n1.c21, n2.c21, n3.c21, n4.c21, n5.c21, n6.c21, n7.c21, n8.c21, n9.c21, n10.c21, n11.c21, n12.c21, n13.c21, n14.c21) 

c21.ubot <- tapply(X=c21.dat$Vmax, INDEX=c21.dat$uniqval, FUN=mean)
c21.depth <- tapply(X=c21.dat$Depth, INDEX=c21.dat$uniqval, FUN=get.char)
c21.substrate <- tapply(X=c21.dat$Substrate, INDEX=c21.dat$uniqval, FUN=get.char)

c21_data <- data.frame(uniq=names(c21.ubot), Ubot=c21.ubot, Depth=c21.depth, Substrate=c21.substrate)

vel.0to5_c21 <- c21_data[c21_data$Depth <= 5,]
vel.5to10_c21 <- c21_data[c21_data$Depth <= 10 & c21_data$Depth > 5,]
vel.10to15_c21 <- c21_data[c21_data$Depth <= 15 & c21_data$Depth > 10,]
vel.15to20_c21 <- c21_data[c21_data$Depth <= 20 & c21_data$Depth > 15,]

profinder(dat=vel.0to5_c21$Ubot, subs.dat=vel.0to5_c21$Substrate)




####################### Macrocystis breaking ########################################################
#####################################################################################################

setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo")
rkmod <- read.csv(file="Outputs_RKmodel.csv")
rkmod2 <- read.csv(file="RKMod Redo Output.csv")
rkmod1 <- rkmod[rkmod$error==0,]
rkmod <- rbind(rkmod1, rkmod2)
macact <- read.csv(file="Macrocystis_actual.csv")

setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale")
n2.c18 <- read.csv(file='N2/Nest2_C18.csv',stringsAsFactors=FALSE)
yran.n2 <- range(n2.c18$Yp)
xran.n2 <- range(n2.c18$Xp)
n2.shore <- n2.c18[n2.c18$Ubot == -10,c(1,2)]
n2.c18 <- n2.c18[n2.c18$Ubot != -10 & n2.c18$Depth > 3 & n2.c18$Depth < 20,]
###
uniqval <- array("P", dim=c(dim(n2.c18)[1]))
for(i in 1:dim(n2.c18)[1]){
	cat(i, "of ", dim(n2.c18)[1], "\r") 
	flush.console()
	DEP <- if(n2.c18$Depth[i] <= 15) round(n2.c18$Depth[i]*4,0)/4 else {round(n2.c18$Depth[i]*2,0)/2}
	SWH <- if(n2.c18$Hsig[i] <= 2) round(n2.c18$Hsig[i],1) else {round(n2.c18$Hsig[i]*5,0)/5}
	WPER <- if(n2.c18$Tm_10[i] <=5) round(n2.c18$Tm_10[i],1) else {round(n2.c18$Tm_10[i]*5,0)/5}
	WLEN <- if(n2.c18$Wlen[i] <=20) round(n2.c18$Wlen[i]*2,0)/2 else { if(n2.c18$Wlen[i] <= 40) round(n2.c18$Wlen[i],0) else {round(n2.c18$Wlen[i]/2,0)*2}}
	uniqval[i] <- paste(DEP,SWH,WPER,WLEN, sep="_")
}
uniqlist <- unique(uniqval)
rkmodn2.c18 <- rkmod[rkmod$unique.code %in% uniqlist,]
pmacbreak <- array(NA, dim=c(dim(n2.c18)[1]))
macstress <- array(NA, dim=c(dim(n2.c18)[1]))
for(i in 1:length(uniqlist)) {
	cat(i, "of ", length(uniqlist), "\r") 
	flush.console()
	pmacbreak[uniqval==uniqlist[i]] <- rkmodn2.c18$Pbrake[rkmodn2.c18$unique.code==uniqlist[i]]
	macstress[uniqval==uniqlist[i]] <- rkmodn2.c18$stress[rkmodn2.c18$unique.code==uniqlist[i]]
}
###
n2.c18 <- data.frame(n2.c18,pmacbreak, macstress)
write.csv(n2.c18, file="N2_C18_MAC.csv")
###

setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale")
n3.c18 <- read.csv(file='N3/Nest3_C18.csv',stringsAsFactors=FALSE)
yran.n3 <- range(n3.c18$Yp)
xran.n3 <- range(n3.c18$Xp)
n3.shore <- n3.c18[n3.c18$Ubot == -10,c(1,2)]
n3.c18 <- n3.c18[n3.c18$Ubot != -10 & n3.c18$Depth > 3 & n3.c18$Depth < 20,]
###
uniqval <- array("P", dim=c(dim(n3.c18)[1]))
for(i in 1:dim(n3.c18)[1]){
	cat(i, "of ", dim(n3.c18)[1], "\r") 
	flush.console()
	DEP <- if(n3.c18$Depth[i] <= 15) round(n3.c18$Depth[i]*4,0)/4 else {round(n3.c18$Depth[i]*2,0)/2}
	SWH <- if(n3.c18$Hsig[i] <= 2) round(n3.c18$Hsig[i],1) else {round(n3.c18$Hsig[i]*5,0)/5}
	WPER <- if(n3.c18$Tm_10[i] <=5) round(n3.c18$Tm_10[i],1) else {round(n3.c18$Tm_10[i]*5,0)/5}
	WLEN <- if(n3.c18$Wlen[i] <=20) round(n3.c18$Wlen[i]*2,0)/2 else { if(n3.c18$Wlen[i] <= 40) round(n3.c18$Wlen[i],0) else {round(n3.c18$Wlen[i]/2,0)*2}}
	uniqval[i] <- paste(DEP,SWH,WPER,WLEN, sep="_")
}
uniqlist <- unique(uniqval)
rkmodn3.c18 <- rkmod[rkmod$unique.code %in% uniqlist,]
pmacbreak <- array(NA, dim=c(dim(n3.c18)[1]))
macstress <- array(NA, dim=c(dim(n3.c18)[1]))
for(i in 1:length(uniqlist)) {
	cat(i, "of ", length(uniqlist), "\r") 
	flush.console()
	pmacbreak[uniqval==uniqlist[i]] <- rkmodn3.c18$Pbrake[rkmodn3.c18$unique.code==uniqlist[i]]
	macstress[uniqval==uniqlist[i]] <- rkmodn3.c18$stress[rkmodn3.c18$unique.code==uniqlist[i]]
}
###
n3.c18 <- data.frame(n3.c18,pmacbreak, macstress)
write.csv(n3.c18, file="N3_C18_MAC.csv")
###

setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale")
n4.c18 <- read.csv(file='N4/Nest4_C18.csv',stringsAsFactors=FALSE)
yran.n4 <- range(n4.c18$Yp)
xran.n4 <- range(n4.c18$Xp)
n4.shore <- n4.c18[n4.c18$Ubot == -10,c(1,2)]
n4.c18 <- n4.c18[n4.c18$Ubot != -10 & n4.c18$Depth > 3 & n4.c18$Depth < 20,]
###
uniqval <- array("P", dim=c(dim(n4.c18)[1]))
for(i in 1:dim(n4.c18)[1]){
	cat(i, "of ", dim(n4.c18)[1], "\r") 
	flush.console()
	DEP <- if(n4.c18$Depth[i] <= 15) round(n4.c18$Depth[i]*4,0)/4 else {round(n4.c18$Depth[i]*2,0)/2}
	SWH <- if(n4.c18$Hsig[i] <= 2) round(n4.c18$Hsig[i],1) else {round(n4.c18$Hsig[i]*5,0)/5}
	WPER <- if(n4.c18$Tm_10[i] <=5) round(n4.c18$Tm_10[i],1) else {round(n4.c18$Tm_10[i]*5,0)/5}
	WLEN <- if(n4.c18$Wlen[i] <=20) round(n4.c18$Wlen[i]*2,0)/2 else { if(n4.c18$Wlen[i] <= 40) round(n4.c18$Wlen[i],0) else {round(n4.c18$Wlen[i]/2,0)*2}}
	uniqval[i] <- paste(DEP,SWH,WPER,WLEN, sep="_")
}
uniqlist <- unique(uniqval)
rkmodn4.c18 <- rkmod[rkmod$unique.code %in% uniqlist,]
pmacbreak <- array(NA, dim=c(dim(n4.c18)[1]))
macstress <- array(NA, dim=c(dim(n4.c18)[1]))
for(i in 1:length(uniqlist)) {
	cat(i, "of ", length(uniqlist), "\r") 
	flush.console()
	pmacbreak[uniqval==uniqlist[i]] <- rkmodn4.c18$Pbrake[rkmodn4.c18$unique.code==uniqlist[i]]
	macstress[uniqval==uniqlist[i]] <- rkmodn4.c18$stress[rkmodn4.c18$unique.code==uniqlist[i]]
}
###
n4.c18 <- data.frame(n4.c18,pmacbreak, macstress)
write.csv(n4.c18, file="N4_C18_MAC.csv")
###

setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale")
n5.c18 <- read.csv(file='N5/Nest5_C18.csv',stringsAsFactors=FALSE)
yran.n5 <- range(n5.c18$Yp)
xran.n5 <- range(n5.c18$Xp)
n5.shore <- n5.c18[n5.c18$Ubot == -10,c(1,2)]
n5.c18 <- n5.c18[n5.c18$Ubot != -10 & n5.c18$Depth > 3 & n5.c18$Depth < 20,]
###
uniqval <- array("P", dim=c(dim(n5.c18)[1]))
for(i in 1:dim(n5.c18)[1]){
	cat(i, "of ", dim(n5.c18)[1], "\r") 
	flush.console()
	DEP <- if(n5.c18$Depth[i] <= 15) round(n5.c18$Depth[i]*4,0)/4 else {round(n5.c18$Depth[i]*2,0)/2}
	SWH <- if(n5.c18$Hsig[i] <= 2) round(n5.c18$Hsig[i],1) else {round(n5.c18$Hsig[i]*5,0)/5}
	WPER <- if(n5.c18$Tm_10[i] <=5) round(n5.c18$Tm_10[i],1) else {round(n5.c18$Tm_10[i]*5,0)/5}
	WLEN <- if(n5.c18$Wlen[i] <=20) round(n5.c18$Wlen[i]*2,0)/2 else { if(n5.c18$Wlen[i] <= 40) round(n5.c18$Wlen[i],0) else {round(n5.c18$Wlen[i]/2,0)*2}}
	uniqval[i] <- paste(DEP,SWH,WPER,WLEN, sep="_")
}
uniqlist <- unique(uniqval)
rkmodn5.c18 <- rkmod[rkmod$unique.code %in% uniqlist,]
pmacbreak <- array(NA, dim=c(dim(n5.c18)[1]))
macstress <- array(NA, dim=c(dim(n5.c18)[1]))
for(i in 1:length(uniqlist)) {
	cat(i, "of ", length(uniqlist), "\r") 
	flush.console()
	pmacbreak[uniqval==uniqlist[i]] <- rkmodn5.c18$Pbrake[rkmodn5.c18$unique.code==uniqlist[i]]
	macstress[uniqval==uniqlist[i]] <- rkmodn5.c18$stress[rkmodn5.c18$unique.code==uniqlist[i]]
}
###
n5.c18 <- data.frame(n5.c18,pmacbreak, macstress)
write.csv(n5.c18, file="N5_C18_MAC.csv")
###

setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale")
n6.c18 <- read.csv(file='N6/Nest6_C18.csv',stringsAsFactors=FALSE)
yran.n6 <- range(n6.c18$Yp)
xran.n6 <- range(n6.c18$Xp)
n6.shore <- n6.c18[n6.c18$Ubot == -10,c(1,2)]
n6.c18 <- n6.c18[n6.c18$Ubot != -10 & n6.c18$Depth > 3 & n6.c18$Depth < 20,]
###
uniqval <- array("P", dim=c(dim(n6.c18)[1]))
for(i in 1:dim(n6.c18)[1]){
	cat(i, "of ", dim(n6.c18)[1], "\r") 
	flush.console()
	DEP <- if(n6.c18$Depth[i] <= 15) round(n6.c18$Depth[i]*4,0)/4 else {round(n6.c18$Depth[i]*2,0)/2}
	SWH <- if(n6.c18$Hsig[i] <= 2) round(n6.c18$Hsig[i],1) else {round(n6.c18$Hsig[i]*5,0)/5}
	WPER <- if(n6.c18$Tm_10[i] <=5) round(n6.c18$Tm_10[i],1) else {round(n6.c18$Tm_10[i]*5,0)/5}
	WLEN <- if(n6.c18$Wlen[i] <=20) round(n6.c18$Wlen[i]*2,0)/2 else { if(n6.c18$Wlen[i] <= 40) round(n6.c18$Wlen[i],0) else {round(n6.c18$Wlen[i]/2,0)*2}}
	uniqval[i] <- paste(DEP,SWH,WPER,WLEN, sep="_")
}
uniqlist <- unique(uniqval)
rkmodn6.c18 <- rkmod[rkmod$unique.code %in% uniqlist,]
pmacbreak <- array(NA, dim=c(dim(n6.c18)[1]))
macstress <- array(NA, dim=c(dim(n6.c18)[1]))
for(i in 1:length(uniqlist)) {
	cat(i, "of ", length(uniqlist), "\r") 
	flush.console()
	pmacbreak[uniqval==uniqlist[i]] <- rkmodn6.c18$Pbrake[rkmodn6.c18$unique.code==uniqlist[i]]
	macstress[uniqval==uniqlist[i]] <- rkmodn6.c18$stress[rkmodn6.c18$unique.code==uniqlist[i]]
}
###
n6.c18 <- data.frame(n6.c18,pmacbreak, macstress)
write.csv(n6.c18, file="N6_C18_MAC.csv")
###

setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale")
n7.c18 <- read.csv(file='N7/Nest7_C18.csv',stringsAsFactors=FALSE)
yran.n7 <- range(n7.c18$Yp)
xran.n7 <- range(n7.c18$Xp)
n7.shore <- n7.c18[n7.c18$Ubot == -10,c(1,2)]
n7.c18 <- n7.c18[n7.c18$Ubot != -10 & n7.c18$Depth > 3 & n7.c18$Depth < 20,]
###
uniqval <- array("P", dim=c(dim(n7.c18)[1]))
for(i in 1:dim(n7.c18)[1]){
	cat(i, "of ", dim(n7.c18)[1], "\r") 
	flush.console()
	DEP <- if(n7.c18$Depth[i] <= 15) round(n7.c18$Depth[i]*4,0)/4 else {round(n7.c18$Depth[i]*2,0)/2}
	SWH <- if(n7.c18$Hsig[i] <= 2) round(n7.c18$Hsig[i],1) else {round(n7.c18$Hsig[i]*5,0)/5}
	WPER <- if(n7.c18$Tm_10[i] <=5) round(n7.c18$Tm_10[i],1) else {round(n7.c18$Tm_10[i]*5,0)/5}
	WLEN <- if(n7.c18$Wlen[i] <=20) round(n7.c18$Wlen[i]*2,0)/2 else { if(n7.c18$Wlen[i] <= 40) round(n7.c18$Wlen[i],0) else {round(n7.c18$Wlen[i]/2,0)*2}}
	uniqval[i] <- paste(DEP,SWH,WPER,WLEN, sep="_")
}
uniqlist <- unique(uniqval)
rkmodn7.c18 <- rkmod[rkmod$unique.code %in% uniqlist,]
pmacbreak <- array(NA, dim=c(dim(n7.c18)[1]))
macstress <- array(NA, dim=c(dim(n7.c18)[1]))
for(i in 1:length(uniqlist)) {
	cat(i, "of ", length(uniqlist), "\r") 
	flush.console()
	pmacbreak[uniqval==uniqlist[i]] <- rkmodn7.c18$Pbrake[rkmodn7.c18$unique.code==uniqlist[i]]
	macstress[uniqval==uniqlist[i]] <- rkmodn7.c18$stress[rkmodn7.c18$unique.code==uniqlist[i]]
}
###
n7.c18 <- data.frame(n7.c18,pmacbreak, macstress)
write.csv(n7.c18, file="N7_C18_MAC.csv")
###

setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale")
n8.c18 <- read.csv(file='N8/Nest8_C18.csv',stringsAsFactors=FALSE)
yran.n8 <- range(n8.c18$Yp)
xran.n8 <- range(n8.c18$Xp)
n8.shore <- n8.c18[n8.c18$Ubot == -10,c(1,2)]
n8.c18 <- n8.c18[n8.c18$Ubot != -10 & n8.c18$Depth > 3 & n8.c18$Depth < 20,]
###
uniqval <- array("P", dim=c(dim(n8.c18)[1]))
for(i in 1:dim(n8.c18)[1]){
	cat(i, "of ", dim(n8.c18)[1], "\r") 
	flush.console()
	DEP <- if(n8.c18$Depth[i] <= 15) round(n8.c18$Depth[i]*4,0)/4 else {round(n8.c18$Depth[i]*2,0)/2}
	SWH <- if(n8.c18$Hsig[i] <= 2) round(n8.c18$Hsig[i],1) else {round(n8.c18$Hsig[i]*5,0)/5}
	WPER <- if(n8.c18$Tm_10[i] <=5) round(n8.c18$Tm_10[i],1) else {round(n8.c18$Tm_10[i]*5,0)/5}
	WLEN <- if(n8.c18$Wlen[i] <=20) round(n8.c18$Wlen[i]*2,0)/2 else { if(n8.c18$Wlen[i] <= 40) round(n8.c18$Wlen[i],0) else {round(n8.c18$Wlen[i]/2,0)*2}}
	uniqval[i] <- paste(DEP,SWH,WPER,WLEN, sep="_")
}
uniqlist <- unique(uniqval)
rkmodn8.c18 <- rkmod[rkmod$unique.code %in% uniqlist,]
pmacbreak <- array(NA, dim=c(dim(n8.c18)[1]))
macstress <- array(NA, dim=c(dim(n8.c18)[1]))
for(i in 1:length(uniqlist)) {
	cat(i, "of ", length(uniqlist), "\r") 
	flush.console()
	pmacbreak[uniqval==uniqlist[i]] <- rkmodn8.c18$Pbrake[rkmodn8.c18$unique.code==uniqlist[i]]
	macstress[uniqval==uniqlist[i]] <- rkmodn8.c18$stress[rkmodn8.c18$unique.code==uniqlist[i]]
}
###
n8.c18 <- data.frame(n8.c18,pmacbreak, macstress)
write.csv(n8.c18, file="N8_C18_MAC.csv")
###

setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale")
n9.c18 <- read.csv(file='N9/Nest9_C18.csv',stringsAsFactors=FALSE)
yran.n9 <- range(n9.c18$Yp)
xran.n9 <- range(n9.c18$Xp)
n9.shore <- n9.c18[n9.c18$Ubot == -10,c(1,2)]
n9.c18 <- n9.c18[n9.c18$Ubot != -10 & n9.c18$Depth > 3 & n9.c18$Depth < 20,]
###
uniqval <- array("P", dim=c(dim(n9.c18)[1]))
for(i in 1:dim(n9.c18)[1]){
	cat(i, "of ", dim(n9.c18)[1], "\r") 
	flush.console()
	DEP <- if(n9.c18$Depth[i] <= 15) round(n9.c18$Depth[i]*4,0)/4 else {round(n9.c18$Depth[i]*2,0)/2}
	SWH <- if(n9.c18$Hsig[i] <= 2) round(n9.c18$Hsig[i],1) else {round(n9.c18$Hsig[i]*5,0)/5}
	WPER <- if(n9.c18$Tm_10[i] <=5) round(n9.c18$Tm_10[i],1) else {round(n9.c18$Tm_10[i]*5,0)/5}
	WLEN <- if(n9.c18$Wlen[i] <=20) round(n9.c18$Wlen[i]*2,0)/2 else { if(n9.c18$Wlen[i] <= 40) round(n9.c18$Wlen[i],0) else {round(n9.c18$Wlen[i]/2,0)*2}}
	uniqval[i] <- paste(DEP,SWH,WPER,WLEN, sep="_")
}
uniqlist <- unique(uniqval)
rkmodn9.c18 <- rkmod[rkmod$unique.code %in% uniqlist,]
pmacbreak <- array(NA, dim=c(dim(n9.c18)[1]))
macstress <- array(NA, dim=c(dim(n9.c18)[1]))
for(i in 1:length(uniqlist)) {
	cat(i, "of ", length(uniqlist), "\r") 
	flush.console()
	pmacbreak[uniqval==uniqlist[i]] <- rkmodn9.c18$Pbrake[rkmodn9.c18$unique.code==uniqlist[i]]
	macstress[uniqval==uniqlist[i]] <- rkmodn9.c18$stress[rkmodn9.c18$unique.code==uniqlist[i]]
}
###
n9.c18 <- data.frame(n9.c18,pmacbreak, macstress)
write.csv(n9.c18, file="N9_C18_MAC.csv")
###

setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale")
n10.c18 <- read.csv(file='N10/Nest10_C18.csv',stringsAsFactors=FALSE)
yran.n10 <- range(n10.c18$Yp)
xran.n10 <- range(n10.c18$Xp)
n10.shore <- n10.c18[n10.c18$Ubot == -10,c(1,2)]
n10.c18 <- n10.c18[n10.c18$Ubot != -10 & n10.c18$Depth > 3 & n10.c18$Depth < 20,]
###
uniqval <- array("P", dim=c(dim(n10.c18)[1]))
for(i in 1:dim(n10.c18)[1]){
	cat(i, "of ", dim(n10.c18)[1], "\r") 
	flush.console()
	DEP <- if(n10.c18$Depth[i] <= 15) round(n10.c18$Depth[i]*4,0)/4 else {round(n10.c18$Depth[i]*2,0)/2}
	SWH <- if(n10.c18$Hsig[i] <= 2) round(n10.c18$Hsig[i],1) else {round(n10.c18$Hsig[i]*5,0)/5}
	WPER <- if(n10.c18$Tm_10[i] <=5) round(n10.c18$Tm_10[i],1) else {round(n10.c18$Tm_10[i]*5,0)/5}
	WLEN <- if(n10.c18$Wlen[i] <=20) round(n10.c18$Wlen[i]*2,0)/2 else { if(n10.c18$Wlen[i] <= 40) round(n10.c18$Wlen[i],0) else {round(n10.c18$Wlen[i]/2,0)*2}}
	uniqval[i] <- paste(DEP,SWH,WPER,WLEN, sep="_")
}
uniqlist <- unique(uniqval)
rkmodn10.c18 <- rkmod[rkmod$unique.code %in% uniqlist,]
pmacbreak <- array(NA, dim=c(dim(n10.c18)[1]))
macstress <- array(NA, dim=c(dim(n10.c18)[1]))
for(i in 1:length(uniqlist)) {
	cat(i, "of ", length(uniqlist), "\r") 
	flush.console()
	pmacbreak[uniqval==uniqlist[i]] <- rkmodn10.c18$Pbrake[rkmodn10.c18$unique.code==uniqlist[i]]
	macstress[uniqval==uniqlist[i]] <- rkmodn10.c18$stress[rkmodn10.c18$unique.code==uniqlist[i]]
}
###
n10.c18 <- data.frame(n10.c18,pmacbreak, macstress)
write.csv(n10.c18, file="N10_C18_MAC.csv")
###

setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale")
n11.c18 <- read.csv(file='N11/Nest11_C18.csv',stringsAsFactors=FALSE)
yran.n11 <- range(n11.c18$Yp)
xran.n11 <- range(n11.c18$Xp)
n11.shore <- n11.c18[n11.c18$Ubot == -10,c(1,2)]
n11.c18 <- n11.c18[n11.c18$Ubot != -10 & n11.c18$Depth > 3 & n11.c18$Depth < 20,]
###
uniqval <- array("P", dim=c(dim(n11.c18)[1]))
for(i in 1:dim(n11.c18)[1]){
	cat(i, "of ", dim(n11.c18)[1], "\r") 
	flush.console()
	DEP <- if(n11.c18$Depth[i] <= 15) round(n11.c18$Depth[i]*4,0)/4 else {round(n11.c18$Depth[i]*2,0)/2}
	SWH <- if(n11.c18$Hsig[i] <= 2) round(n11.c18$Hsig[i],1) else {round(n11.c18$Hsig[i]*5,0)/5}
	WPER <- if(n11.c18$Tm_10[i] <=5) round(n11.c18$Tm_10[i],1) else {round(n11.c18$Tm_10[i]*5,0)/5}
	WLEN <- if(n11.c18$Wlen[i] <=20) round(n11.c18$Wlen[i]*2,0)/2 else { if(n11.c18$Wlen[i] <= 40) round(n11.c18$Wlen[i],0) else {round(n11.c18$Wlen[i]/2,0)*2}}
	uniqval[i] <- paste(DEP,SWH,WPER,WLEN, sep="_")
}
uniqlist <- unique(uniqval)
rkmodn11.c18 <- rkmod[rkmod$unique.code %in% uniqlist,]
pmacbreak <- array(NA, dim=c(dim(n11.c18)[1]))
macstress <- array(NA, dim=c(dim(n11.c18)[1]))
for(i in 1:length(uniqlist)) {
	cat(i, "of ", length(uniqlist), "\r") 
	flush.console()
	pmacbreak[uniqval==uniqlist[i]] <- rkmodn11.c18$Pbrake[rkmodn11.c18$unique.code==uniqlist[i]]
	macstress[uniqval==uniqlist[i]] <- rkmodn11.c18$stress[rkmodn11.c18$unique.code==uniqlist[i]]
}
###
n11.c18 <- data.frame(n11.c18,pmacbreak, macstress)
write.csv(n11.c18, file="N11_C18_MAC.csv")
###



setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale")
n12.c18 <- read.csv(file='N12/Nest12_C18.csv',stringsAsFactors=FALSE)
yran.n12 <- range(n12.c18$Yp)
xran.n12 <- range(n12.c18$Xp)
n12.shore <- n12.c18[n12.c18$Ubot == -10,c(1,2)]
n12.c18 <- n12.c18[n12.c18$Ubot != -10 & n12.c18$Depth > 3 & n12.c18$Depth < 20,]
###
uniqval <- array("P", dim=c(dim(n12.c18)[1]))
for(i in 1:dim(n12.c18)[1]){
	cat(i, "of ", dim(n12.c18)[1], "\r") 
	flush.console()
	DEP <- if(n12.c18$Depth[i] <= 15) round(n12.c18$Depth[i]*4,0)/4 else {round(n12.c18$Depth[i]*2,0)/2}
	SWH <- if(n12.c18$Hsig[i] <= 2) round(n12.c18$Hsig[i],1) else {round(n12.c18$Hsig[i]*5,0)/5}
	WPER <- if(n12.c18$Tm_10[i] <=5) round(n12.c18$Tm_10[i],1) else {round(n12.c18$Tm_10[i]*5,0)/5}
	WLEN <- if(n12.c18$Wlen[i] <=20) round(n12.c18$Wlen[i]*2,0)/2 else { if(n12.c18$Wlen[i] <= 40) round(n12.c18$Wlen[i],0) else {round(n12.c18$Wlen[i]/2,0)*2}}
	uniqval[i] <- paste(DEP,SWH,WPER,WLEN, sep="_")
}
uniqlist <- unique(uniqval)
rkmodn12.c18 <- rkmod[rkmod$unique.code %in% uniqlist,]
pmacbreak <- array(NA, dim=c(dim(n12.c18)[1]))
macstress <- array(NA, dim=c(dim(n12.c18)[1]))
for(i in 1:length(uniqlist)) {
	cat(i, "of ", length(uniqlist), "\r") 
	flush.console()
	pmacbreak[uniqval==uniqlist[i]] <- rkmodn12.c18$Pbrake[rkmodn12.c18$unique.code==uniqlist[i]]
	macstress[uniqval==uniqlist[i]] <- rkmodn12.c18$stress[rkmodn12.c18$unique.code==uniqlist[i]]
}
###
n12.c18 <- data.frame(n12.c18,pmacbreak, macstress)
write.csv(n12.c18, file="N12_C18_MAC.csv")
###

setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale")
n13.c18 <- read.csv(file='N13/Nest13_C18.csv',stringsAsFactors=FALSE)
yran.n13 <- range(n13.c18$Yp)
xran.n13 <- range(n13.c18$Xp)
n13.shore <- n13.c18[n13.c18$Ubot == -10,c(1,2)]
n13.c18 <- n13.c18[n13.c18$Ubot != -10 & n13.c18$Depth > 3 & n13.c18$Depth < 20,]
###
uniqval <- array("P", dim=c(dim(n13.c18)[1]))
for(i in 1:dim(n13.c18)[1]){
	cat(i, "of ", dim(n13.c18)[1], "\r") 
	flush.console()
	DEP <- if(n13.c18$Depth[i] <= 15) round(n13.c18$Depth[i]*4,0)/4 else {round(n13.c18$Depth[i]*2,0)/2}
	SWH <- if(n13.c18$Hsig[i] <= 2) round(n13.c18$Hsig[i],1) else {round(n13.c18$Hsig[i]*5,0)/5}
	WPER <- if(n13.c18$Tm_10[i] <=5) round(n13.c18$Tm_10[i],1) else {round(n13.c18$Tm_10[i]*5,0)/5}
	WLEN <- if(n13.c18$Wlen[i] <=20) round(n13.c18$Wlen[i]*2,0)/2 else { if(n13.c18$Wlen[i] <= 40) round(n13.c18$Wlen[i],0) else {round(n13.c18$Wlen[i]/2,0)*2}}
	uniqval[i] <- paste(DEP,SWH,WPER,WLEN, sep="_")
}
uniqlist <- unique(uniqval)
rkmodn13.c18 <- rkmod[rkmod$unique.code %in% uniqlist,]
pmacbreak <- array(NA, dim=c(dim(n13.c18)[1]))
macstress <- array(NA, dim=c(dim(n13.c18)[1]))
for(i in 1:length(uniqlist)) {
	cat(i, "of ", length(uniqlist), "\r") 
	flush.console()
	pmacbreak[uniqval==uniqlist[i]] <- rkmodn13.c18$Pbrake[rkmodn13.c18$unique.code==uniqlist[i]]
	macstress[uniqval==uniqlist[i]] <- rkmodn13.c18$stress[rkmodn13.c18$unique.code==uniqlist[i]]
}
###
n13.c18 <- data.frame(n13.c18,pmacbreak, macstress)
write.csv(n13.c18, file="N13_C18_MAC.csv")
###

setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale")
n14.c18 <- read.csv(file='N14/Nest14_C18.csv',stringsAsFactors=FALSE)
yran.n14 <- range(n14.c18$Yp)
xran.n14 <- range(n14.c18$Xp)
n14.shore <- n14.c18[n14.c18$Ubot == -10,c(1,2)]
n14.c18 <- n14.c18[n14.c18$Ubot != -10 & n14.c18$Depth > 3 & n14.c18$Depth < 20,]
###
uniqval <- array("P", dim=c(dim(n14.c18)[1]))
for(i in 1:dim(n14.c18)[1]){
	cat(i, "of ", dim(n14.c18)[1], "\r") 
	flush.console()
	DEP <- if(n14.c18$Depth[i] <= 15) round(n14.c18$Depth[i]*4,0)/4 else {round(n14.c18$Depth[i]*2,0)/2}
	SWH <- if(n14.c18$Hsig[i] <= 2) round(n14.c18$Hsig[i],1) else {round(n14.c18$Hsig[i]*5,0)/5}
	WPER <- if(n14.c18$Tm_10[i] <=5) round(n14.c18$Tm_10[i],1) else {round(n14.c18$Tm_10[i]*5,0)/5}
	WLEN <- if(n14.c18$Wlen[i] <=20) round(n14.c18$Wlen[i]*2,0)/2 else { if(n14.c18$Wlen[i] <= 40) round(n14.c18$Wlen[i],0) else {round(n14.c18$Wlen[i]/2,0)*2}}
	uniqval[i] <- paste(DEP,SWH,WPER,WLEN, sep="_")
}
uniqlist <- unique(uniqval)
rkmodn14.c18 <- rkmod[rkmod$unique.code %in% uniqlist,]
pmacbreak <- array(NA, dim=c(dim(n14.c18)[1]))
macstress <- array(NA, dim=c(dim(n14.c18)[1]))
for(i in 1:length(uniqlist)) {
	cat(i, "of ", length(uniqlist), "\r") 
	flush.console()
	pmacbreak[uniqval==uniqlist[i]] <- rkmodn14.c18$Pbrake[rkmodn14.c18$unique.code==uniqlist[i]]
	macstress[uniqval==uniqlist[i]] <- rkmodn14.c18$stress[rkmodn14.c18$unique.code==uniqlist[i]]
}
###
n14.c18 <- data.frame(n14.c18,pmacbreak, macstress)
write.csv(n14.c18, file="N14_C18_MAC.csv")
###


###########################################################################################################

setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale")
n1.c21 <- read.csv(file='N1/Nest1_C21.csv',stringsAsFactors=FALSE)
yran.n1 <- range(n1.c21$Yp)
xran.n1 <- range(n1.c21$Xp)
n1.shore <- n1.c21[n1.c21$Ubot == -10,c(1,2)]
n1.c21 <- n1.c21[n1.c21$Ubot != -10 & n1.c21$Depth > 3 & n1.c21$Depth < 20,]
###
uniqval <- array("P", dim=c(dim(n1.c21)[1]))
for(i in 1:dim(n1.c21)[1]){
	cat(i, "of ", dim(n1.c21)[1], "\r") 
	flush.console()
	DEP <- if(n1.c21$Depth[i] <= 15) round(n1.c21$Depth[i]*4,0)/4 else {round(n1.c21$Depth[i]*2,0)/2}
	SWH <- if(n1.c21$Hsig[i] <= 2) round(n1.c21$Hsig[i],1) else {round(n1.c21$Hsig[i]*5,0)/5}
	WPER <- if(n1.c21$Tm_10[i] <=5) round(n1.c21$Tm_10[i],1) else {round(n1.c21$Tm_10[i]*5,0)/5}
	WLEN <- if(n1.c21$Wlen[i] <=20) round(n1.c21$Wlen[i]*2,0)/2 else { if(n1.c21$Wlen[i] <= 40) round(n1.c21$Wlen[i],0) else {round(n1.c21$Wlen[i]/2,0)*2}}
	uniqval[i] <- paste(DEP,SWH,WPER,WLEN, sep="_")
}
uniqlist <- unique(uniqval)
rkmodn1.c21 <- rkmod[rkmod$unique.code %in% uniqlist,]
pmacbreak <- array(NA, dim=c(dim(n1.c21)[1]))
macstress <- array(NA, dim=c(dim(n1.c21)[1]))
for(i in 1:length(uniqlist)) {
	cat(i, "of ", length(uniqlist), "\r") 
	flush.console()
	pmacbreak[uniqval==uniqlist[i]] <- rkmodn1.c21$Pbrake[rkmodn1.c21$unique.code==uniqlist[i]]
	macstress[uniqval==uniqlist[i]] <- rkmodn1.c21$stress[rkmodn1.c21$unique.code==uniqlist[i]]
}
###
n1.c21 <- data.frame(n1.c21,pmacbreak, macstress)
write.csv(n1.c21, file="N1_C21_MAC.csv")
###

setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale")
n2.c21 <- read.csv(file='N2/Nest2_C21.csv',stringsAsFactors=FALSE)
yran.n2 <- range(n2.c21$Yp)
xran.n2 <- range(n2.c21$Xp)
n2.shore <- n2.c21[n2.c21$Ubot == -10,c(1,2)]
n2.c21 <- n2.c21[n2.c21$Ubot != -10 & n2.c21$Depth > 3 & n2.c21$Depth < 20,]
###
uniqval <- array("P", dim=c(dim(n2.c21)[1]))
for(i in 1:dim(n2.c21)[1]){
	cat(i, "of ", dim(n2.c21)[1], "\r") 
	flush.console()
	DEP <- if(n2.c21$Depth[i] <= 15) round(n2.c21$Depth[i]*4,0)/4 else {round(n2.c21$Depth[i]*2,0)/2}
	SWH <- if(n2.c21$Hsig[i] <= 2) round(n2.c21$Hsig[i],1) else {round(n2.c21$Hsig[i]*5,0)/5}
	WPER <- if(n2.c21$Tm_10[i] <=5) round(n2.c21$Tm_10[i],1) else {round(n2.c21$Tm_10[i]*5,0)/5}
	WLEN <- if(n2.c21$Wlen[i] <=20) round(n2.c21$Wlen[i]*2,0)/2 else { if(n2.c21$Wlen[i] <= 40) round(n2.c21$Wlen[i],0) else {round(n2.c21$Wlen[i]/2,0)*2}}
	uniqval[i] <- paste(DEP,SWH,WPER,WLEN, sep="_")
}
uniqlist <- unique(uniqval)
rkmodn2.c21 <- rkmod[rkmod$unique.code %in% uniqlist,]
pmacbreak <- array(NA, dim=c(dim(n2.c21)[1]))
macstress <- array(NA, dim=c(dim(n2.c21)[1]))
for(i in 1:length(uniqlist)) {
	cat(i, "of ", length(uniqlist), "\r") 
	flush.console()
	pmacbreak[uniqval==uniqlist[i]] <- rkmodn2.c21$Pbrake[rkmodn2.c21$unique.code==uniqlist[i]]
	macstress[uniqval==uniqlist[i]] <- rkmodn2.c21$stress[rkmodn2.c21$unique.code==uniqlist[i]]
}
###
n2.c21 <- data.frame(n2.c21,pmacbreak, macstress)
write.csv(n2.c21, file="N2_C21_MAC.csv")
###

setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale")
n3.c21 <- read.csv(file='N3/Nest3_C21.csv',stringsAsFactors=FALSE)
yran.n3 <- range(n3.c21$Yp)
xran.n3 <- range(n3.c21$Xp)
n3.shore <- n3.c21[n3.c21$Ubot == -10,c(1,2)]
n3.c21 <- n3.c21[n3.c21$Ubot != -10 & n3.c21$Depth > 3 & n3.c21$Depth < 20,]
###
uniqval <- array("P", dim=c(dim(n3.c21)[1]))
for(i in 1:dim(n3.c21)[1]){
	cat(i, "of ", dim(n3.c21)[1], "\r") 
	flush.console()
	DEP <- if(n3.c21$Depth[i] <= 15) round(n3.c21$Depth[i]*4,0)/4 else {round(n3.c21$Depth[i]*2,0)/2}
	SWH <- if(n3.c21$Hsig[i] <= 2) round(n3.c21$Hsig[i],1) else {round(n3.c21$Hsig[i]*5,0)/5}
	WPER <- if(n3.c21$Tm_10[i] <=5) round(n3.c21$Tm_10[i],1) else {round(n3.c21$Tm_10[i]*5,0)/5}
	WLEN <- if(n3.c21$Wlen[i] <=20) round(n3.c21$Wlen[i]*2,0)/2 else { if(n3.c21$Wlen[i] <= 40) round(n3.c21$Wlen[i],0) else {round(n3.c21$Wlen[i]/2,0)*2}}
	uniqval[i] <- paste(DEP,SWH,WPER,WLEN, sep="_")
}
uniqlist <- unique(uniqval)
rkmodn3.c21 <- rkmod[rkmod$unique.code %in% uniqlist,]
pmacbreak <- array(NA, dim=c(dim(n3.c21)[1]))
macstress <- array(NA, dim=c(dim(n3.c21)[1]))
for(i in 1:length(uniqlist)) {
	cat(i, "of ", length(uniqlist), "\r") 
	flush.console()
	pmacbreak[uniqval==uniqlist[i]] <- rkmodn3.c21$Pbrake[rkmodn3.c21$unique.code==uniqlist[i]]
	macstress[uniqval==uniqlist[i]] <- rkmodn3.c21$stress[rkmodn3.c21$unique.code==uniqlist[i]]
}
###
n3.c21 <- data.frame(n3.c21,pmacbreak, macstress)
write.csv(n3.c21, file="N3_C21_MAC.csv")
###

setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale")
n4.c21 <- read.csv(file='N4/Nest4_C21.csv',stringsAsFactors=FALSE)
yran.n4 <- range(n4.c21$Yp)
xran.n4 <- range(n4.c21$Xp)
n4.shore <- n4.c21[n4.c21$Ubot == -10,c(1,2)]
n4.c21 <- n4.c21[n4.c21$Ubot != -10 & n4.c21$Depth > 3 & n4.c21$Depth < 20,]
###
uniqval <- array("P", dim=c(dim(n4.c21)[1]))
for(i in 1:dim(n4.c21)[1]){
	cat(i, "of ", dim(n4.c21)[1], "\r") 
	flush.console()
	DEP <- if(n4.c21$Depth[i] <= 15) round(n4.c21$Depth[i]*4,0)/4 else {round(n4.c21$Depth[i]*2,0)/2}
	SWH <- if(n4.c21$Hsig[i] <= 2) round(n4.c21$Hsig[i],1) else {round(n4.c21$Hsig[i]*5,0)/5}
	WPER <- if(n4.c21$Tm_10[i] <=5) round(n4.c21$Tm_10[i],1) else {round(n4.c21$Tm_10[i]*5,0)/5}
	WLEN <- if(n4.c21$Wlen[i] <=20) round(n4.c21$Wlen[i]*2,0)/2 else { if(n4.c21$Wlen[i] <= 40) round(n4.c21$Wlen[i],0) else {round(n4.c21$Wlen[i]/2,0)*2}}
	uniqval[i] <- paste(DEP,SWH,WPER,WLEN, sep="_")
}
uniqlist <- unique(uniqval)
rkmodn4.c21 <- rkmod[rkmod$unique.code %in% uniqlist,]
pmacbreak <- array(NA, dim=c(dim(n4.c21)[1]))
macstress <- array(NA, dim=c(dim(n4.c21)[1]))
for(i in 1:length(uniqlist)) {
	cat(i, "of ", length(uniqlist), "\r") 
	flush.console()
	pmacbreak[uniqval==uniqlist[i]] <- rkmodn4.c21$Pbrake[rkmodn4.c21$unique.code==uniqlist[i]]
	macstress[uniqval==uniqlist[i]] <- rkmodn4.c21$stress[rkmodn4.c21$unique.code==uniqlist[i]]
}
###
n4.c21 <- data.frame(n4.c21,pmacbreak, macstress)
write.csv(n4.c21, file="N4_C21_MAC.csv")
###

setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale")
n5.c21 <- read.csv(file='N5/Nest5_C21.csv',stringsAsFactors=FALSE)
yran.n5 <- range(n5.c21$Yp)
xran.n5 <- range(n5.c21$Xp)
n5.shore <- n5.c21[n5.c21$Ubot == -10,c(1,2)]
n5.c21 <- n5.c21[n5.c21$Ubot != -10 & n5.c21$Depth > 3 & n5.c21$Depth < 20,]
###
uniqval <- array("P", dim=c(dim(n5.c21)[1]))
for(i in 1:dim(n5.c21)[1]){
	cat(i, "of ", dim(n5.c21)[1], "\r") 
	flush.console()
	DEP <- if(n5.c21$Depth[i] <= 15) round(n5.c21$Depth[i]*4,0)/4 else {round(n5.c21$Depth[i]*2,0)/2}
	SWH <- if(n5.c21$Hsig[i] <= 2) round(n5.c21$Hsig[i],1) else {round(n5.c21$Hsig[i]*5,0)/5}
	WPER <- if(n5.c21$Tm_10[i] <=5) round(n5.c21$Tm_10[i],1) else {round(n5.c21$Tm_10[i]*5,0)/5}
	WLEN <- if(n5.c21$Wlen[i] <=20) round(n5.c21$Wlen[i]*2,0)/2 else { if(n5.c21$Wlen[i] <= 40) round(n5.c21$Wlen[i],0) else {round(n5.c21$Wlen[i]/2,0)*2}}
	uniqval[i] <- paste(DEP,SWH,WPER,WLEN, sep="_")
}
uniqlist <- unique(uniqval)
rkmodn5.c21 <- rkmod[rkmod$unique.code %in% uniqlist,]
pmacbreak <- array(NA, dim=c(dim(n5.c21)[1]))
macstress <- array(NA, dim=c(dim(n5.c21)[1]))
for(i in 1:length(uniqlist)) {
	cat(i, "of ", length(uniqlist), "\r") 
	flush.console()
	pmacbreak[uniqval==uniqlist[i]] <- rkmodn5.c21$Pbrake[rkmodn5.c21$unique.code==uniqlist[i]]
	macstress[uniqval==uniqlist[i]] <- rkmodn5.c21$stress[rkmodn5.c21$unique.code==uniqlist[i]]
}
###
n5.c21 <- data.frame(n5.c21,pmacbreak, macstress)
write.csv(n5.c21, file="N5_C21_MAC.csv")
###

setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale")
n6.c21 <- read.csv(file='N6/Nest6_C21.csv',stringsAsFactors=FALSE)
yran.n6 <- range(n6.c21$Yp)
xran.n6 <- range(n6.c21$Xp)
n6.shore <- n6.c21[n6.c21$Ubot == -10,c(1,2)]
n6.c21 <- n6.c21[n6.c21$Ubot != -10 & n6.c21$Depth > 3 & n6.c21$Depth < 20,]
###
uniqval <- array("P", dim=c(dim(n6.c21)[1]))
for(i in 1:dim(n6.c21)[1]){
	cat(i, "of ", dim(n6.c21)[1], "\r") 
	flush.console()
	DEP <- if(n6.c21$Depth[i] <= 15) round(n6.c21$Depth[i]*4,0)/4 else {round(n6.c21$Depth[i]*2,0)/2}
	SWH <- if(n6.c21$Hsig[i] <= 2) round(n6.c21$Hsig[i],1) else {round(n6.c21$Hsig[i]*5,0)/5}
	WPER <- if(n6.c21$Tm_10[i] <=5) round(n6.c21$Tm_10[i],1) else {round(n6.c21$Tm_10[i]*5,0)/5}
	WLEN <- if(n6.c21$Wlen[i] <=20) round(n6.c21$Wlen[i]*2,0)/2 else { if(n6.c21$Wlen[i] <= 40) round(n6.c21$Wlen[i],0) else {round(n6.c21$Wlen[i]/2,0)*2}}
	uniqval[i] <- paste(DEP,SWH,WPER,WLEN, sep="_")
}
uniqlist <- unique(uniqval)
rkmodn6.c21 <- rkmod[rkmod$unique.code %in% uniqlist,]
pmacbreak <- array(NA, dim=c(dim(n6.c21)[1]))
macstress <- array(NA, dim=c(dim(n6.c21)[1]))
for(i in 1:length(uniqlist)) {
	cat(i, "of ", length(uniqlist), "\r") 
	flush.console()
	pmacbreak[uniqval==uniqlist[i]] <- rkmodn6.c21$Pbrake[rkmodn6.c21$unique.code==uniqlist[i]]
	macstress[uniqval==uniqlist[i]] <- rkmodn6.c21$stress[rkmodn6.c21$unique.code==uniqlist[i]]
}
###
n6.c21 <- data.frame(n6.c21,pmacbreak, macstress)
write.csv(n6.c21, file="N6_C21_MAC.csv")
###

setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale")
n7.c21 <- read.csv(file='N7/Nest7_C21.csv',stringsAsFactors=FALSE)
yran.n7 <- range(n7.c21$Yp)
xran.n7 <- range(n7.c21$Xp)
n7.shore <- n7.c21[n7.c21$Ubot == -10,c(1,2)]
n7.c21 <- n7.c21[n7.c21$Ubot != -10 & n7.c21$Depth > 3 & n7.c21$Depth < 20,]
###
uniqval <- array("P", dim=c(dim(n7.c21)[1]))
for(i in 1:dim(n7.c21)[1]){
	cat(i, "of ", dim(n7.c21)[1], "\r") 
	flush.console()
	DEP <- if(n7.c21$Depth[i] <= 15) round(n7.c21$Depth[i]*4,0)/4 else {round(n7.c21$Depth[i]*2,0)/2}
	SWH <- if(n7.c21$Hsig[i] <= 2) round(n7.c21$Hsig[i],1) else {round(n7.c21$Hsig[i]*5,0)/5}
	WPER <- if(n7.c21$Tm_10[i] <=5) round(n7.c21$Tm_10[i],1) else {round(n7.c21$Tm_10[i]*5,0)/5}
	WLEN <- if(n7.c21$Wlen[i] <=20) round(n7.c21$Wlen[i]*2,0)/2 else { if(n7.c21$Wlen[i] <= 40) round(n7.c21$Wlen[i],0) else {round(n7.c21$Wlen[i]/2,0)*2}}
	uniqval[i] <- paste(DEP,SWH,WPER,WLEN, sep="_")
}
uniqlist <- unique(uniqval)
rkmodn7.c21 <- rkmod[rkmod$unique.code %in% uniqlist,]
pmacbreak <- array(NA, dim=c(dim(n7.c21)[1]))
macstress <- array(NA, dim=c(dim(n7.c21)[1]))
for(i in 1:length(uniqlist)) {
	cat(i, "of ", length(uniqlist), "\r") 
	flush.console()
	pmacbreak[uniqval==uniqlist[i]] <- rkmodn7.c21$Pbrake[rkmodn7.c21$unique.code==uniqlist[i]]
	macstress[uniqval==uniqlist[i]] <- rkmodn7.c21$stress[rkmodn7.c21$unique.code==uniqlist[i]]
}
###
n7.c21 <- data.frame(n7.c21,pmacbreak, macstress)
write.csv(n7.c21, file="N7_C21_MAC.csv")
###

setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale")
n8.c21 <- read.csv(file='N8/Nest8_C21.csv',stringsAsFactors=FALSE)
yran.n8 <- range(n8.c21$Yp)
xran.n8 <- range(n8.c21$Xp)
n8.shore <- n8.c21[n8.c21$Ubot == -10,c(1,2)]
n8.c21 <- n8.c21[n8.c21$Ubot != -10 & n8.c21$Depth > 3 & n8.c21$Depth < 20,]
###
uniqval <- array("P", dim=c(dim(n8.c21)[1]))
for(i in 1:dim(n8.c21)[1]){
	cat(i, "of ", dim(n8.c21)[1], "\r") 
	flush.console()
	DEP <- if(n8.c21$Depth[i] <= 15) round(n8.c21$Depth[i]*4,0)/4 else {round(n8.c21$Depth[i]*2,0)/2}
	SWH <- if(n8.c21$Hsig[i] <= 2) round(n8.c21$Hsig[i],1) else {round(n8.c21$Hsig[i]*5,0)/5}
	WPER <- if(n8.c21$Tm_10[i] <=5) round(n8.c21$Tm_10[i],1) else {round(n8.c21$Tm_10[i]*5,0)/5}
	WLEN <- if(n8.c21$Wlen[i] <=20) round(n8.c21$Wlen[i]*2,0)/2 else { if(n8.c21$Wlen[i] <= 40) round(n8.c21$Wlen[i],0) else {round(n8.c21$Wlen[i]/2,0)*2}}
	uniqval[i] <- paste(DEP,SWH,WPER,WLEN, sep="_")
}
uniqlist <- unique(uniqval)
rkmodn8.c21 <- rkmod[rkmod$unique.code %in% uniqlist,]
pmacbreak <- array(NA, dim=c(dim(n8.c21)[1]))
macstress <- array(NA, dim=c(dim(n8.c21)[1]))
for(i in 1:length(uniqlist)) {
	cat(i, "of ", length(uniqlist), "\r") 
	flush.console()
	pmacbreak[uniqval==uniqlist[i]] <- rkmodn8.c21$Pbrake[rkmodn8.c21$unique.code==uniqlist[i]]
	macstress[uniqval==uniqlist[i]] <- rkmodn8.c21$stress[rkmodn8.c21$unique.code==uniqlist[i]]
}
###
n8.c21 <- data.frame(n8.c21,pmacbreak, macstress)
write.csv(n8.c21, file="N8_C21_MAC.csv")
###

setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale")
n9.c21 <- read.csv(file='N9/Nest9_C21.csv',stringsAsFactors=FALSE)
yran.n9 <- range(n9.c21$Yp)
xran.n9 <- range(n9.c21$Xp)
n9.shore <- n9.c21[n9.c21$Ubot == -10,c(1,2)]
n9.c21 <- n9.c21[n9.c21$Ubot != -10 & n9.c21$Depth > 3 & n9.c21$Depth < 20,]
###
uniqval <- array("P", dim=c(dim(n9.c21)[1]))
for(i in 1:dim(n9.c21)[1]){
	cat(i, "of ", dim(n9.c21)[1], "\r") 
	flush.console()
	DEP <- if(n9.c21$Depth[i] <= 15) round(n9.c21$Depth[i]*4,0)/4 else {round(n9.c21$Depth[i]*2,0)/2}
	SWH <- if(n9.c21$Hsig[i] <= 2) round(n9.c21$Hsig[i],1) else {round(n9.c21$Hsig[i]*5,0)/5}
	WPER <- if(n9.c21$Tm_10[i] <=5) round(n9.c21$Tm_10[i],1) else {round(n9.c21$Tm_10[i]*5,0)/5}
	WLEN <- if(n9.c21$Wlen[i] <=20) round(n9.c21$Wlen[i]*2,0)/2 else { if(n9.c21$Wlen[i] <= 40) round(n9.c21$Wlen[i],0) else {round(n9.c21$Wlen[i]/2,0)*2}}
	uniqval[i] <- paste(DEP,SWH,WPER,WLEN, sep="_")
}
uniqlist <- unique(uniqval)
rkmodn9.c21 <- rkmod[rkmod$unique.code %in% uniqlist,]
pmacbreak <- array(NA, dim=c(dim(n9.c21)[1]))
macstress <- array(NA, dim=c(dim(n9.c21)[1]))
for(i in 1:length(uniqlist)) {
	cat(i, "of ", length(uniqlist), "\r") 
	flush.console()
	pmacbreak[uniqval==uniqlist[i]] <- rkmodn9.c21$Pbrake[rkmodn9.c21$unique.code==uniqlist[i]]
	macstress[uniqval==uniqlist[i]] <- rkmodn9.c21$stress[rkmodn9.c21$unique.code==uniqlist[i]]
}
###
n9.c21 <- data.frame(n9.c21,pmacbreak, macstress)
write.csv(n9.c21, file="N9_C21_MAC.csv")
###

setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale")
n10.c21 <- read.csv(file='N10/Nest10_C21.csv',stringsAsFactors=FALSE)
yran.n10 <- range(n10.c21$Yp)
xran.n10 <- range(n10.c21$Xp)
n10.shore <- n10.c21[n10.c21$Ubot == -10,c(1,2)]
n10.c21 <- n10.c21[n10.c21$Ubot != -10 & n10.c21$Depth > 3 & n10.c21$Depth < 20,]
###
uniqval <- array("P", dim=c(dim(n10.c21)[1]))
for(i in 1:dim(n10.c21)[1]){
	cat(i, "of ", dim(n10.c21)[1], "\r") 
	flush.console()
	DEP <- if(n10.c21$Depth[i] <= 15) round(n10.c21$Depth[i]*4,0)/4 else {round(n10.c21$Depth[i]*2,0)/2}
	SWH <- if(n10.c21$Hsig[i] <= 2) round(n10.c21$Hsig[i],1) else {round(n10.c21$Hsig[i]*5,0)/5}
	WPER <- if(n10.c21$Tm_10[i] <=5) round(n10.c21$Tm_10[i],1) else {round(n10.c21$Tm_10[i]*5,0)/5}
	WLEN <- if(n10.c21$Wlen[i] <=20) round(n10.c21$Wlen[i]*2,0)/2 else { if(n10.c21$Wlen[i] <= 40) round(n10.c21$Wlen[i],0) else {round(n10.c21$Wlen[i]/2,0)*2}}
	uniqval[i] <- paste(DEP,SWH,WPER,WLEN, sep="_")
}
uniqlist <- unique(uniqval)
rkmodn10.c21 <- rkmod[rkmod$unique.code %in% uniqlist,]
pmacbreak <- array(NA, dim=c(dim(n10.c21)[1]))
macstress <- array(NA, dim=c(dim(n10.c21)[1]))
for(i in 1:length(uniqlist)) {
	cat(i, "of ", length(uniqlist), "\r") 
	flush.console()
	pmacbreak[uniqval==uniqlist[i]] <- rkmodn10.c21$Pbrake[rkmodn10.c21$unique.code==uniqlist[i]]
	macstress[uniqval==uniqlist[i]] <- rkmodn10.c21$stress[rkmodn10.c21$unique.code==uniqlist[i]]
}
###
n10.c21 <- data.frame(n10.c21,pmacbreak, macstress)
write.csv(n10.c21, file="N10_C21_MAC.csv")
###

setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale")
n11.c21 <- read.csv(file='N11/Nest11_C21.csv',stringsAsFactors=FALSE)
yran.n11 <- range(n11.c21$Yp)
xran.n11 <- range(n11.c21$Xp)
n11.shore <- n11.c21[n11.c21$Ubot == -10,c(1,2)]
n11.c21 <- n11.c21[n11.c21$Ubot != -10 & n11.c21$Depth > 3 & n11.c21$Depth < 20,]
###
uniqval <- array("P", dim=c(dim(n11.c21)[1]))
for(i in 1:dim(n11.c21)[1]){
	cat(i, "of ", dim(n11.c21)[1], "\r") 
	flush.console()
	DEP <- if(n11.c21$Depth[i] <= 15) round(n11.c21$Depth[i]*4,0)/4 else {round(n11.c21$Depth[i]*2,0)/2}
	SWH <- if(n11.c21$Hsig[i] <= 2) round(n11.c21$Hsig[i],1) else {round(n11.c21$Hsig[i]*5,0)/5}
	WPER <- if(n11.c21$Tm_10[i] <=5) round(n11.c21$Tm_10[i],1) else {round(n11.c21$Tm_10[i]*5,0)/5}
	WLEN <- if(n11.c21$Wlen[i] <=20) round(n11.c21$Wlen[i]*2,0)/2 else { if(n11.c21$Wlen[i] <= 40) round(n11.c21$Wlen[i],0) else {round(n11.c21$Wlen[i]/2,0)*2}}
	uniqval[i] <- paste(DEP,SWH,WPER,WLEN, sep="_")
}
uniqlist <- unique(uniqval)
rkmodn11.c21 <- rkmod[rkmod$unique.code %in% uniqlist,]
pmacbreak <- array(NA, dim=c(dim(n11.c21)[1]))
macstress <- array(NA, dim=c(dim(n11.c21)[1]))
for(i in 1:length(uniqlist)) {
	cat(i, "of ", length(uniqlist), "\r") 
	flush.console()
	pmacbreak[uniqval==uniqlist[i]] <- rkmodn11.c21$Pbrake[rkmodn11.c21$unique.code==uniqlist[i]]
	macstress[uniqval==uniqlist[i]] <- rkmodn11.c21$stress[rkmodn11.c21$unique.code==uniqlist[i]]
}
###
n11.c21 <- data.frame(n11.c21,pmacbreak, macstress)
write.csv(n11.c21, file="N11_C21_MAC.csv")
###



setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale")
n12.c21 <- read.csv(file='N12/Nest12_C21.csv',stringsAsFactors=FALSE)
yran.n12 <- range(n12.c21$Yp)
xran.n12 <- range(n12.c21$Xp)
n12.shore <- n12.c21[n12.c21$Ubot == -10,c(1,2)]
n12.c21 <- n12.c21[n12.c21$Ubot != -10 & n12.c21$Depth > 3 & n12.c21$Depth < 20,]
###
uniqval <- array("P", dim=c(dim(n12.c21)[1]))
for(i in 1:dim(n12.c21)[1]){
	cat(i, "of ", dim(n12.c21)[1], "\r") 
	flush.console()
	DEP <- if(n12.c21$Depth[i] <= 15) round(n12.c21$Depth[i]*4,0)/4 else {round(n12.c21$Depth[i]*2,0)/2}
	SWH <- if(n12.c21$Hsig[i] <= 2) round(n12.c21$Hsig[i],1) else {round(n12.c21$Hsig[i]*5,0)/5}
	WPER <- if(n12.c21$Tm_10[i] <=5) round(n12.c21$Tm_10[i],1) else {round(n12.c21$Tm_10[i]*5,0)/5}
	WLEN <- if(n12.c21$Wlen[i] <=20) round(n12.c21$Wlen[i]*2,0)/2 else { if(n12.c21$Wlen[i] <= 40) round(n12.c21$Wlen[i],0) else {round(n12.c21$Wlen[i]/2,0)*2}}
	uniqval[i] <- paste(DEP,SWH,WPER,WLEN, sep="_")
}
uniqlist <- unique(uniqval)
rkmodn12.c21 <- rkmod[rkmod$unique.code %in% uniqlist,]
pmacbreak <- array(NA, dim=c(dim(n12.c21)[1]))
macstress <- array(NA, dim=c(dim(n12.c21)[1]))
for(i in 1:length(uniqlist)) {
	cat(i, "of ", length(uniqlist), "\r") 
	flush.console()
	pmacbreak[uniqval==uniqlist[i]] <- rkmodn12.c21$Pbrake[rkmodn12.c21$unique.code==uniqlist[i]]
	macstress[uniqval==uniqlist[i]] <- rkmodn12.c21$stress[rkmodn12.c21$unique.code==uniqlist[i]]
}
###
n12.c21 <- data.frame(n12.c21,pmacbreak, macstress)
write.csv(n12.c21, file="N12_C21_MAC.csv")
###

setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale")
n13.c21 <- read.csv(file='N13/Nest13_C21.csv',stringsAsFactors=FALSE)
yran.n13 <- range(n13.c21$Yp)
xran.n13 <- range(n13.c21$Xp)
n13.shore <- n13.c21[n13.c21$Ubot == -10,c(1,2)]
n13.c21 <- n13.c21[n13.c21$Ubot != -10 & n13.c21$Depth > 3 & n13.c21$Depth < 20,]
###
uniqval <- array("P", dim=c(dim(n13.c21)[1]))
for(i in 1:dim(n13.c21)[1]){
	cat(i, "of ", dim(n13.c21)[1], "\r") 
	flush.console()
	DEP <- if(n13.c21$Depth[i] <= 15) round(n13.c21$Depth[i]*4,0)/4 else {round(n13.c21$Depth[i]*2,0)/2}
	SWH <- if(n13.c21$Hsig[i] <= 2) round(n13.c21$Hsig[i],1) else {round(n13.c21$Hsig[i]*5,0)/5}
	WPER <- if(n13.c21$Tm_10[i] <=5) round(n13.c21$Tm_10[i],1) else {round(n13.c21$Tm_10[i]*5,0)/5}
	WLEN <- if(n13.c21$Wlen[i] <=20) round(n13.c21$Wlen[i]*2,0)/2 else { if(n13.c21$Wlen[i] <= 40) round(n13.c21$Wlen[i],0) else {round(n13.c21$Wlen[i]/2,0)*2}}
	uniqval[i] <- paste(DEP,SWH,WPER,WLEN, sep="_")
}
uniqlist <- unique(uniqval)
rkmodn13.c21 <- rkmod[rkmod$unique.code %in% uniqlist,]
pmacbreak <- array(NA, dim=c(dim(n13.c21)[1]))
macstress <- array(NA, dim=c(dim(n13.c21)[1]))
for(i in 1:length(uniqlist)) {
	cat(i, "of ", length(uniqlist), "\r") 
	flush.console()
	pmacbreak[uniqval==uniqlist[i]] <- rkmodn13.c21$Pbrake[rkmodn13.c21$unique.code==uniqlist[i]]
	macstress[uniqval==uniqlist[i]] <- rkmodn13.c21$stress[rkmodn13.c21$unique.code==uniqlist[i]]
}
###
n13.c21 <- data.frame(n13.c21,pmacbreak, macstress)
write.csv(n13.c21, file="N13_C21_MAC.csv")
###

setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale")
n14.c21 <- read.csv(file='N14/Nest14_C21.csv',stringsAsFactors=FALSE)
yran.n14 <- range(n14.c21$Yp)
xran.n14 <- range(n14.c21$Xp)
n14.shore <- n14.c21[n14.c21$Ubot == -10,c(1,2)]
n14.c21 <- n14.c21[n14.c21$Ubot != -10 & n14.c21$Depth > 3 & n14.c21$Depth < 20,]
###
uniqval <- array("P", dim=c(dim(n14.c21)[1]))
for(i in 1:dim(n14.c21)[1]){
	cat(i, "of ", dim(n14.c21)[1], "\r") 
	flush.console()
	DEP <- if(n14.c21$Depth[i] <= 15) round(n14.c21$Depth[i]*4,0)/4 else {round(n14.c21$Depth[i]*2,0)/2}
	SWH <- if(n14.c21$Hsig[i] <= 2) round(n14.c21$Hsig[i],1) else {round(n14.c21$Hsig[i]*5,0)/5}
	WPER <- if(n14.c21$Tm_10[i] <=5) round(n14.c21$Tm_10[i],1) else {round(n14.c21$Tm_10[i]*5,0)/5}
	WLEN <- if(n14.c21$Wlen[i] <=20) round(n14.c21$Wlen[i]*2,0)/2 else { if(n14.c21$Wlen[i] <= 40) round(n14.c21$Wlen[i],0) else {round(n14.c21$Wlen[i]/2,0)*2}}
	uniqval[i] <- paste(DEP,SWH,WPER,WLEN, sep="_")
}
uniqlist <- unique(uniqval)
rkmodn14.c21 <- rkmod[rkmod$unique.code %in% uniqlist,]
pmacbreak <- array(NA, dim=c(dim(n14.c21)[1]))
macstress <- array(NA, dim=c(dim(n14.c21)[1]))
for(i in 1:length(uniqlist)) {
	cat(i, "of ", length(uniqlist), "\r") 
	flush.console()
	pmacbreak[uniqval==uniqlist[i]] <- rkmodn14.c21$Pbrake[rkmodn14.c21$unique.code==uniqlist[i]]
	macstress[uniqval==uniqlist[i]] <- rkmodn14.c21$stress[rkmodn14.c21$unique.code==uniqlist[i]]
}
###
n14.c21 <- data.frame(n14.c21,pmacbreak, macstress)
write.csv(n14.c21, file="N14_C21_MAC.csv")
###


######################################
######################################

which.rowcol <- function(dat) {
	seq.col <- c(0,0.001,0.005,0.01,0.02, 0.04,0.06,0.08,0.1,0.12,0.14,0.16,0.18)
	max.ind <- max(which(seq.col <= dat))
	colvec <- rainbow(n=12, end=0.7, start=0)
	colvec[4] <- rgb(1,1,0,1)
	colvec[5] <- rgb(0.65,1,0,1)
	col.ret <- colvec[13-max.ind]
	return(col.ret)
}

distfind <- function(dat, xrange, yrange){
	d1 <- abs(dat$Xt-xrange[1]) 
	d2 <- abs(dat$Xt-xrange[2]) 
	d3 <- abs(dat$Yt-yrange[1]) 
	d4 <- abs(dat$Yt-yrange[2])
	mindist <- pmin(d1, d2, d3, d4)
}

setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale")
n1.c18.mac <- read.csv(file='N1_C18_MAC.csv',stringsAsFactors =FALSE)
Dboun <- distfind(dat=n1.c18.mac, xrange=xran.n1, yrange=yran.n1)
Aweight <- exp(-6.907 + 0.046*Dboun)/(1+exp(-6.907 + 0.046*Dboun))
n1.c18.mac <- data.frame(n1.c18.mac[,c(15,16,17,18)], Aweight)

n2.c18.mac <- read.csv(file='N2_C18_MAC.csv',stringsAsFactors =FALSE)
Dboun <- distfind(dat=n2.c18.mac, xrange=xran.n2, yrange=yran.n2)
Aweight <- exp(-6.907 + 0.046*Dboun)/(1+exp(-6.907 + 0.046*Dboun))
n2.c18.mac <- data.frame(n2.c18.mac[,c(15,16,17,18)], Aweight)

n1.over <- n1.c18.mac[n1.c18.mac$uniqval %in% n2.c18.mac$uniqval,]
n2.over <- n2.c18.mac[n2.c18.mac$uniqval %in% n1.c18.mac$uniqval,]
sum(abs(n1.over$Xt-n2.over$Xt) + abs(n1.over$Yt-n2.over$Yt))

Vnew <- ((n1.over$pmacbreak*n1.over$Aweight) + (n2.over$pmacbreak*n2.over$Aweight))/(n1.over$Aweight + n2.over$Aweight) 
n1.c18.mac$pmacbreak[n1.c18.mac$uniqval %in% n2.c18.mac$uniqval] <- Vnew
n2.c18.mac$pmacbreak[n2.c18.mac$uniqval %in% n1.c18.mac$uniqval] <- Vnew

INDEXCOL <- 1:dim(n1.c18.mac)[1]
colvec <- tapply(X=n1.c18.mac$pmacbreak, INDEX=INDEXCOL, FUN=which.rowcol)

png(filename = "N1_C18_MACRO.png", width = 2000, height = 2000, units = "px", pointsize = 12, bg = "white",  res = 300,type = "cairo-png")
par(mar=c(2,2,1,1))
plot(Yt ~ Xt, data=n1.c18.mac, pch=15, col=colvec, asp=1, cex=0.17, xlab="", ylab="", bty="n", xaxt="n", yaxt="n", ylim=yran.n1, xlim=xran.n1)
lines(Yt ~ Xt, data=n1.shore, pch=15, col="grey", cex=0.17, type="p")
for(i in 1:63) {
	polygon(x=macact$lonNZ[macact$shape==i], y=macact$latNZ[macact$shape==i], border=1, lwd=2, col=NA)
}
YCV <- c(5419000,5418600,5419360,5419360,5420200,5420200,5420600,5420600,5420550,5421800,5421800,5421000,5419650,5421600)
XCV <- c(309080,309080,309040,310460,311220,312640,314020,315150,316000,315100,316400,317100,317100,316500)
lines(YCV ~ XCV, pch=3, cex=1, col=2, type="p")
dev.off()

#############

n3.c18.mac <- read.csv(file='N3_C18_MAC.csv',stringsAsFactors =FALSE)
Dboun <- distfind(dat=n3.c18.mac, xrange=xran.n3, yrange=yran.n3)
Aweight <- exp(-6.907 + 0.046*Dboun)/(1+exp(-6.907 + 0.046*Dboun))
n3.c18.mac <- data.frame(n3.c18.mac[,c(15,16,17,18)], Aweight)

n2.over <- n2.c18.mac[n2.c18.mac$uniqval %in% n3.c18.mac$uniqval,]
n3.over <- n3.c18.mac[n3.c18.mac$uniqval %in% n2.c18.mac$uniqval,]
sum(abs(n2.over$Xt-n3.over$Xt) + abs(n2.over$Yt-n3.over$Yt))

Vnew <- ((n2.over$pmacbreak*n2.over$Aweight) + (n3.over$pmacbreak*n3.over$Aweight))/(n2.over$Aweight + n3.over$Aweight) 
n2.c18.mac$pmacbreak[n2.c18.mac$uniqval %in% n3.c18.mac$uniqval] <- Vnew
n3.c18.mac$pmacbreak[n3.c18.mac$uniqval %in% n2.c18.mac$uniqval] <- Vnew

INDEXCOL <- 1:dim(n2.c18.mac)[1]
colvec <- tapply(X=n2.c18.mac$pmacbreak, INDEX=INDEXCOL, FUN=which.rowcol)

png(filename = "N2_C18_MACRO.png", width = 2000, height = 2000, units = "px", pointsize = 12, bg = "white",  res = 300,type = "cairo-png")
par(mar=c(2,2,1,1))
plot(Yt ~ Xt, data=n2.c18.mac, pch=15, col=colvec, asp=1, cex=0.17, xlab="", ylab="", bty="n", xaxt="n", yaxt="n", ylim=yran.n2, xlim=xran.n2)
lines(Yt ~ Xt, data=n2.shore, pch=15, col="grey", cex=0.17, type="p")
for(i in 1:63) {
	polygon(x=macact$lonNZ[macact$shape==i], y=macact$latNZ[macact$shape==i], border=1, lwd=2, col=NA)
}
YCV <- c(5419000,5418600,5419360,5419360,5420200,5420200,5420600,5420600,5420550,5421800,5421800,5421000,5419650,5421600)
XCV <- c(309080,309080,309040,310460,311220,312640,314020,315150,316000,315100,316400,317100,317100,316500)
lines(YCV ~ XCV, pch=3, cex=1, col=2, type="p")
dev.off()

#############

n4.c18.mac <- read.csv(file='N4_C18_MAC.csv',stringsAsFactors =FALSE)
Dboun <- distfind(dat=n4.c18.mac, xrange=xran.n4, yrange=yran.n4)
Aweight <- exp(-6.907 + 0.046*Dboun)/(1+exp(-6.907 + 0.046*Dboun))
n4.c18.mac <- data.frame(n4.c18.mac[,c(15,16,17,18)], Aweight)

n3.over <- n3.c18.mac[n3.c18.mac$uniqval %in% n4.c18.mac$uniqval,]
n4.over <- n4.c18.mac[n4.c18.mac$uniqval %in% n3.c18.mac$uniqval,]
sum(abs(n3.over$Xt-n4.over$Xt) + abs(n3.over$Yt-n4.over$Yt))

Vnew <- ((n3.over$pmacbreak*n3.over$Aweight) + (n4.over$pmacbreak*n4.over$Aweight))/(n3.over$Aweight + n4.over$Aweight) 
n3.c18.mac$pmacbreak[n3.c18.mac$uniqval %in% n4.c18.mac$uniqval] <- Vnew
n4.c18.mac$pmacbreak[n4.c18.mac$uniqval %in% n3.c18.mac$uniqval] <- Vnew

INDEXCOL <- 1:dim(n3.c18.mac)[1]
colvec <- tapply(X=n3.c18.mac$pmacbreak, INDEX=INDEXCOL, FUN=which.rowcol)

png(filename = "N3_C18_MACRO.png", width = 2000, height = 2000, units = "px", pointsize = 12, bg = "white",  res = 300,type = "cairo-png")
par(mar=c(2,2,1,1))
plot(Yt ~ Xt, data=n3.c18.mac, pch=15, col=colvec, asp=1, cex=0.17, xlab="", ylab="", bty="n", xaxt="n", yaxt="n", ylim=yran.n3, xlim=xran.n3)
lines(Yt ~ Xt, data=n3.shore, pch=15, col="grey", cex=0.17, type="p")
for(i in 1:63) {
	polygon(x=macact$lonNZ[macact$shape==i], y=macact$latNZ[macact$shape==i], border=1, lwd=2, col=NA)
}
YCV <- c(5419000,5418600,5419360,5419360,5420200,5420200,5420600,5420600,5420550,5421800,5421800,5421000,5419650,5421600)
XCV <- c(309080,309080,309040,310460,311220,312640,314020,315150,316000,315100,316400,317100,317100,316500)
lines(YCV ~ XCV, pch=3, cex=1, col=2, type="p")
dev.off()

#############

n5.c18.mac <- read.csv(file='N5_C18_MAC.csv',stringsAsFactors =FALSE)
Dboun <- distfind(dat=n5.c18.mac, xrange=xran.n5, yrange=yran.n5)
Aweight <- exp(-6.907 + 0.046*Dboun)/(1+exp(-6.907 + 0.046*Dboun))
n5.c18.mac <- data.frame(n5.c18.mac[,c(15,16,17,18)], Aweight)

n4.over <- n4.c18.mac[n4.c18.mac$uniqval %in% n5.c18.mac$uniqval,]
n5.over <- n5.c18.mac[n5.c18.mac$uniqval %in% n4.c18.mac$uniqval,]
sum(abs(n4.over$Xt-n5.over$Xt) + abs(n4.over$Yt-n5.over$Yt))

Vnew <- ((n4.over$pmacbreak*n4.over$Aweight) + (n5.over$pmacbreak*n5.over$Aweight))/(n4.over$Aweight + n5.over$Aweight) 
n4.c18.mac$pmacbreak[n4.c18.mac$uniqval %in% n5.c18.mac$uniqval] <- Vnew
n5.c18.mac$pmacbreak[n5.c18.mac$uniqval %in% n4.c18.mac$uniqval] <- Vnew

INDEXCOL <- 1:dim(n4.c18.mac)[1]
colvec <- tapply(X=n4.c18.mac$pmacbreak, INDEX=INDEXCOL, FUN=which.rowcol)

png(filename = "N4_C18_MACRO.png", width = 2000, height = 2000, units = "px", pointsize = 12, bg = "white",  res = 300,type = "cairo-png")
par(mar=c(2,2,1,1))
plot(Yt ~ Xt, data=n4.c18.mac, pch=15, col=colvec, asp=1, cex=0.17, xlab="", ylab="", bty="n", xaxt="n", yaxt="n", ylim=yran.n4, xlim=xran.n4)
lines(Yt ~ Xt, data=n4.shore, pch=15, col="grey", cex=0.17, type="p")
for(i in 1:63) {
	polygon(x=macact$lonNZ[macact$shape==i], y=macact$latNZ[macact$shape==i], border=1, lwd=2, col=NA)
}
YCV <- c(5419000,5418600,5419360,5419360,5420200,5420200,5420600,5420600,5420550,5421800,5421800,5421000,5419650,5421600)
XCV <- c(309080,309080,309040,310460,311220,312640,314020,315150,316000,315100,316400,317100,317100,316500)
lines(YCV ~ XCV, pch=3, cex=1, col=2, type="p")
dev.off()

#############

n6.c18.mac <- read.csv(file='N6_C18_MAC.csv',stringsAsFactors =FALSE)
Dboun <- distfind(dat=n6.c18.mac, xrange=xran.n6, yrange=yran.n6)
Aweight <- exp(-6.907 + 0.046*Dboun)/(1+exp(-6.907 + 0.046*Dboun))
n6.c18.mac <- data.frame(n6.c18.mac[,c(15,16,17,18)], Aweight)

n5.over <- n5.c18.mac[n5.c18.mac$uniqval %in% n6.c18.mac$uniqval,]
n6.over <- n6.c18.mac[n6.c18.mac$uniqval %in% n5.c18.mac$uniqval,]
sum(abs(n5.over$Xt-n6.over$Xt) + abs(n5.over$Yt-n6.over$Yt))

Vnew <- ((n5.over$pmacbreak*n5.over$Aweight) + (n6.over$pmacbreak*n6.over$Aweight))/(n5.over$Aweight + n6.over$Aweight) 
n5.c18.mac$pmacbreak[n5.c18.mac$uniqval %in% n6.c18.mac$uniqval] <- Vnew
n6.c18.mac$pmacbreak[n6.c18.mac$uniqval %in% n5.c18.mac$uniqval] <- Vnew

INDEXCOL <- 1:dim(n5.c18.mac)[1]
colvec <- tapply(X=n5.c18.mac$pmacbreak, INDEX=INDEXCOL, FUN=which.rowcol)

png(filename = "N5_C18_MACRO.png", width = 2000, height = 2000, units = "px", pointsize = 12, bg = "white",  res = 300,type = "cairo-png")
par(mar=c(2,2,1,1))
plot(Yt ~ Xt, data=n5.c18.mac, pch=15, col=colvec, asp=1, cex=0.17, xlab="", ylab="", bty="n", xaxt="n", yaxt="n", ylim=yran.n5, xlim=xran.n5)
lines(Yt ~ Xt, data=n5.shore, pch=15, col="grey", cex=0.17, type="p")
for(i in 1:63) {
	polygon(x=macact$lonNZ[macact$shape==i], y=macact$latNZ[macact$shape==i], border=1, lwd=2, col=NA)
}
YCV <- c(5419000,5418600,5419360,5419360,5420200,5420200,5420600,5420600,5420550,5421800,5421800,5421000,5419650,5421600)
XCV <- c(309080,309080,309040,310460,311220,312640,314020,315150,316000,315100,316400,317100,317100,316500)
lines(YCV ~ XCV, pch=3, cex=1, col=2, type="p")
dev.off()

#############

n7.c18.mac <- read.csv(file='N7_C18_MAC.csv',stringsAsFactors =FALSE)
Dboun <- distfind(dat=n7.c18.mac, xrange=xran.n7, yrange=yran.n7)
Aweight <- exp(-6.907 + 0.046*Dboun)/(1+exp(-6.907 + 0.046*Dboun))
n7.c18.mac <- data.frame(n7.c18.mac[,c(15,16,17,18)], Aweight)

n6.over <- n6.c18.mac[n6.c18.mac$uniqval %in% n7.c18.mac$uniqval,]
n7.over <- n7.c18.mac[n7.c18.mac$uniqval %in% n6.c18.mac$uniqval,]
sum(abs(n6.over$Xt-n7.over$Xt) + abs(n6.over$Yt-n7.over$Yt))

Vnew <- ((n6.over$pmacbreak*n6.over$Aweight) + (n7.over$pmacbreak*n7.over$Aweight))/(n6.over$Aweight + n7.over$Aweight) 
n6.c18.mac$pmacbreak[n6.c18.mac$uniqval %in% n7.c18.mac$uniqval] <- Vnew
n7.c18.mac$pmacbreak[n7.c18.mac$uniqval %in% n6.c18.mac$uniqval] <- Vnew

INDEXCOL <- 1:dim(n6.c18.mac)[1]
colvec <- tapply(X=n6.c18.mac$pmacbreak, INDEX=INDEXCOL, FUN=which.rowcol)

png(filename = "N6_C18_MACRO.png", width = 2000, height = 2000, units = "px", pointsize = 12, bg = "white",  res = 300,type = "cairo-png")
par(mar=c(2,2,1,1))
plot(Yt ~ Xt, data=n6.c18.mac, pch=15, col=colvec, asp=1, cex=0.17, xlab="", ylab="", bty="n", xaxt="n", yaxt="n", ylim=yran.n6, xlim=xran.n6)
lines(Yt ~ Xt, data=n6.shore, pch=15, col="grey", cex=0.17, type="p")
for(i in 1:63) {
	polygon(x=macact$lonNZ[macact$shape==i], y=macact$latNZ[macact$shape==i], border=1, lwd=2, col=NA)
}
YCV <- c(5419000,5418600,5419360,5419360,5420200,5420200,5420600,5420600,5420550,5421800,5421800,5421000,5419650,5421600)
XCV <- c(309080,309080,309040,310460,311220,312640,314020,315150,316000,315100,316400,317100,317100,316500)
lines(YCV ~ XCV, pch=3, cex=1, col=2, type="p")
dev.off()


##############
##############

n7.c18.mac <- read.csv(file='N7_C18_MAC.csv',stringsAsFactors =FALSE)
Dboun <- distfind(dat=n7.c18.mac, xrange=xran.n7, yrange=yran.n7)
Aweight <- exp(-6.907 + 0.046*Dboun)/(1+exp(-6.907 + 0.046*Dboun))
VmWeight <- n7.c18.mac$pmacbreak*Aweight
n7.c18.mac <- data.frame(n7.c18.mac[,c(15,16,17,18)], Aweight, VmWeight)

n8.c18.mac <- read.csv(file='N8_C18_MAC.csv',stringsAsFactors =FALSE)
Dboun <- distfind(dat=n8.c18.mac, xrange=xran.n8, yrange=yran.n8)
Aweight <- exp(-6.907 + 0.046*Dboun)/(1+exp(-6.907 + 0.046*Dboun))
VmWeight <- n8.c18.mac$pmacbreak*Aweight
n8.c18.mac <- data.frame(n8.c18.mac[,c(15,16,17,18)], Aweight, VmWeight)

n9.c18.mac <- read.csv(file='N9_C18_MAC.csv',stringsAsFactors =FALSE)
Dboun <- distfind(dat=n9.c18.mac, xrange=xran.n9, yrange=yran.n9)
Aweight <- exp(-6.907 + 0.046*Dboun)/(1+exp(-6.907 + 0.046*Dboun))
VmWeight <- n9.c18.mac$pmacbreak*Aweight
n9.c18.mac <- data.frame(n9.c18.mac[,c(15,16,17,18)], Aweight, VmWeight)

n10.c18.mac <- read.csv(file='N10_C18_MAC.csv',stringsAsFactors =FALSE)
Dboun <- distfind(dat=n10.c18.mac, xrange=xran.n10, yrange=yran.n10)
Aweight <- exp(-6.907 + 0.046*Dboun)/(1+exp(-6.907 + 0.046*Dboun))
VmWeight <- n10.c18.mac$pmacbreak*Aweight
n10.c18.mac <- data.frame(n10.c18.mac[,c(15,16,17,18)], Aweight, VmWeight)

n11.c18.mac <- read.csv(file='N11_C18_MAC.csv',stringsAsFactors =FALSE)
Dboun <- distfind(dat=n11.c18.mac, xrange=xran.n11, yrange=yran.n11)
Aweight <- exp(-6.907 + 0.046*Dboun)/(1+exp(-6.907 + 0.046*Dboun))
VmWeight <- n11.c18.mac$pmacbreak*Aweight
n11.c18.mac <- data.frame(n11.c18.mac[,c(15,16,17,18)], Aweight, VmWeight)

n12.c18.mac <- read.csv(file='N12_C18_MAC.csv',stringsAsFactors =FALSE)
Dboun <- distfind(dat=n12.c18.mac, xrange=xran.n12, yrange=yran.n12)
Aweight <- exp(-6.907 + 0.046*Dboun)/(1+exp(-6.907 + 0.046*Dboun))
VmWeight <- n12.c18.mac$pmacbreak*Aweight
n12.c18.mac <- data.frame(n12.c18.mac[,c(15,16,17,18)], Aweight, VmWeight)

nlyall <- rbind(n7.c18.mac, n8.c18.mac, n9.c18.mac, n10.c18.mac, n11.c18.mac, n12.c18.mac)

vmprelim <- tapply(X=nlyall$VmWeight, INDEX=nlyall$uniqval, FUN=sum)
weightprelim <- tapply(X=nlyall$Aweight, INDEX=nlyall$uniqval, FUN=sum)

LyallVmax <- vmprelim/weightprelim
coordstring <- rownames(LyallVmax)
thy <- unlist(strsplit(coordstring, split="_"))
mat_coord <- matrix(as.numeric(thy), nrow=length(LyallVmax), ncol=2, byrow=TRUE)

LyallDat <- data.frame(mat_coord, LyallVmax)
names(LyallDat) <- c("Xt", "Yt", "Pbreak")

n7.dat <- LyallDat[LyallDat$Xt >= xran.n7[1] & LyallDat$Xt <= xran.n7[2] & LyallDat$Yt >= yran.n7[1] & LyallDat$Yt <= yran.n7[2],]
n8.dat <- LyallDat[LyallDat$Xt >= xran.n8[1] & LyallDat$Xt <= xran.n8[2] & LyallDat$Yt >= yran.n8[1] & LyallDat$Yt <= yran.n8[2],]
n9.dat <- LyallDat[LyallDat$Xt >= xran.n9[1] & LyallDat$Xt <= xran.n9[2] & LyallDat$Yt >= yran.n9[1] & LyallDat$Yt <= yran.n9[2],]
n10.dat <- LyallDat[LyallDat$Xt >= xran.n10[1] & LyallDat$Xt <= xran.n10[2] & LyallDat$Yt >= yran.n10[1] & LyallDat$Yt <= yran.n10[2],]
n11.dat <- LyallDat[LyallDat$Xt >= xran.n11[1] & LyallDat$Xt <= xran.n11[2] & LyallDat$Yt >= yran.n11[1] & LyallDat$Yt <= yran.n11[2],]
n12.dat <- LyallDat[LyallDat$Xt >= xran.n12[1] & LyallDat$Xt <= xran.n12[2] & LyallDat$Yt >= yran.n12[1] & LyallDat$Yt <= yran.n12[2],]

INDEXCOL <- 1:dim(n7.dat)[1]
colvec <- tapply(X=n7.dat$Pbreak, INDEX=INDEXCOL, FUN=which.rowcol)
png(filename = "N7_C18_MACRO.png", width = 2000, height = 2000, units = "px", pointsize = 12, bg = "white",  res = 300,type = "cairo-png")
par(mar=c(2,2,1,1))
plot(Yt ~ Xt, data=n7.dat, pch=15, col=colvec, asp=1, cex=0.17, xlab="", ylab="", bty="n", xaxt="n", yaxt="n", ylim=yran.n7, xlim=xran.n7)
lines(Yt ~ Xt, data=n7.shore, pch=15, col="grey", cex=0.17, type="p")
for(i in 1:63) {
	polygon(x=macact$lonNZ[macact$shape==i], y=macact$latNZ[macact$shape==i], border=1, lwd=2, col=NA)
}
YCV <- c(5419000,5418600,5419360,5419360,5420200,5420200,5420600,5420600,5420550,5421800,5421800,5421000,5419650,5421600)
XCV <- c(309080,309080,309040,310460,311220,312640,314020,315150,316000,315100,316400,317100,317100,316500)
lines(YCV ~ XCV, pch=3, cex=1, col=2, type="p")
dev.off()

INDEXCOL <- 1:dim(n8.dat)[1]
colvec <- tapply(X=n8.dat$Pbreak, INDEX=INDEXCOL, FUN=which.rowcol)
png(filename = "N8_C18_MACRO.png", width = 2000, height = 2000, units = "px", pointsize = 12, bg = "white",  res = 300,type = "cairo-png")
par(mar=c(2,2,1,1))
plot(Yt ~ Xt, data=n8.dat, pch=15, col=colvec, asp=1, cex=0.17, xlab="", ylab="", bty="n", xaxt="n", yaxt="n", ylim=yran.n8, xlim=xran.n8)
lines(Yt ~ Xt, data=n8.shore, pch=15, col="grey", cex=0.17, type="p")
for(i in 1:63) {
	polygon(x=macact$lonNZ[macact$shape==i], y=macact$latNZ[macact$shape==i], border=1, lwd=2, col=NA)
}
YCV <- c(5419000,5418600,5419360,5419360,5420200,5420200,5420600,5420600,5420550,5421800,5421800,5421000,5419650,5421600)
XCV <- c(309080,309080,309040,310460,311220,312640,314020,315150,316000,315100,316400,317100,317100,316500)
lines(YCV ~ XCV, pch=3, cex=1, col=2, type="p")
dev.off()

INDEXCOL <- 1:dim(n9.dat)[1]
colvec <- tapply(X=n9.dat$Pbreak, INDEX=INDEXCOL, FUN=which.rowcol)
png(filename = "N9_C18_MACRO.png", width = 2000, height = 2000, units = "px", pointsize = 12, bg = "white",  res = 300,type = "cairo-png")
par(mar=c(2,2,1,1))
plot(Yt ~ Xt, data=n9.dat, pch=15, col=colvec, asp=1, cex=0.17, xlab="", ylab="", bty="n", xaxt="n", yaxt="n", ylim=yran.n9, xlim=xran.n9)
lines(Yt ~ Xt, data=n9.shore, pch=15, col="grey", cex=0.17, type="p")
for(i in 1:63) {
	polygon(x=macact$lonNZ[macact$shape==i], y=macact$latNZ[macact$shape==i], border=1, lwd=2, col=NA)
}
YCV <- c(5419000,5418600,5419360,5419360,5420200,5420200,5420600,5420600,5420550,5421800,5421800,5421000,5419650,5421600)
XCV <- c(309080,309080,309040,310460,311220,312640,314020,315150,316000,315100,316400,317100,317100,316500)
lines(YCV ~ XCV, pch=3, cex=1, col=2, type="p")
dev.off()

INDEXCOL <- 1:dim(n10.dat)[1]
colvec <- tapply(X=n10.dat$Pbreak, INDEX=INDEXCOL, FUN=which.rowcol)
png(filename = "N10_C18_MACRO.png", width = 2000, height = 2000, units = "px", pointsize = 12, bg = "white",  res = 300,type = "cairo-png")
par(mar=c(2,2,1,1))
plot(Yt ~ Xt, data=n10.dat, pch=15, col=colvec, asp=1, cex=0.17, xlab="", ylab="", bty="n", xaxt="n", yaxt="n", ylim=yran.n10, xlim=xran.n10)
lines(Yt ~ Xt, data=n10.shore, pch=15, col="grey", cex=0.17, type="p")
for(i in 1:63) {
	polygon(x=macact$lonNZ[macact$shape==i], y=macact$latNZ[macact$shape==i], border=1, lwd=2, col=NA)
}
YCV <- c(5419000,5418600,5419360,5419360,5420200,5420200,5420600,5420600,5420550,5421800,5421800,5421000,5419650,5421600)
XCV <- c(309080,309080,309040,310460,311220,312640,314020,315150,316000,315100,316400,317100,317100,316500)
lines(YCV ~ XCV, pch=3, cex=1, col=2, type="p")
dev.off()

INDEXCOL <- 1:dim(n11.dat)[1]
colvec <- tapply(X=n11.dat$Pbreak, INDEX=INDEXCOL, FUN=which.rowcol)
png(filename = "N11_C18_MACRO.png", width = 2000, height = 2000, units = "px", pointsize = 12, bg = "white",  res = 300,type = "cairo-png")
par(mar=c(2,2,1,1))
plot(Yt ~ Xt, data=n11.dat, pch=15, col=colvec, asp=1, cex=0.17, xlab="", ylab="", bty="n", xaxt="n", yaxt="n", ylim=yran.n11, xlim=xran.n11)
lines(Yt ~ Xt, data=n11.shore, pch=15, col="grey", cex=0.17, type="p")
for(i in 1:63) {
	polygon(x=macact$lonNZ[macact$shape==i], y=macact$latNZ[macact$shape==i], border=1, lwd=2, col=NA)
}
YCV <- c(5419000,5418600,5419360,5419360,5420200,5420200,5420600,5420600,5420550,5421800,5421800,5421000,5419650,5421600)
XCV <- c(309080,309080,309040,310460,311220,312640,314020,315150,316000,315100,316400,317100,317100,316500)
lines(YCV ~ XCV, pch=3, cex=1, col=2, type="p")
dev.off()

INDEXCOL <- 1:dim(n12.dat)[1]
colvec <- tapply(X=n12.dat$Pbreak, INDEX=INDEXCOL, FUN=which.rowcol)
png(filename = "N12_C18_MACRO.png", width = 2000, height = 2000, units = "px", pointsize = 12, bg = "white",  res = 300,type = "cairo-png")
par(mar=c(2,2,1,1))
plot(Yt ~ Xt, data=n12.dat, pch=15, col=colvec, asp=1, cex=0.17, xlab="", ylab="", bty="n", xaxt="n", yaxt="n", ylim=yran.n12, xlim=xran.n12)
lines(Yt ~ Xt, data=n12.shore, pch=15, col="grey", cex=0.17, type="p")
for(i in 1:63) {
	polygon(x=macact$lonNZ[macact$shape==i], y=macact$latNZ[macact$shape==i], border=1, lwd=2, col=NA)
}
YCV <- c(5419000,5418600,5419360,5419360,5420200,5420200,5420600,5420600,5420550,5421800,5421800,5421000,5419650,5421600)
XCV <- c(309080,309080,309040,310460,311220,312640,314020,315150,316000,315100,316400,317100,317100,316500)
lines(YCV ~ XCV, pch=3, cex=1, col=2, type="p")
dev.off()



#################################################
#################################################


n12.c18.mac <- read.csv(file='N12_C18_MAC.csv',stringsAsFactors =FALSE)
Dboun <- distfind(dat=n12.c18.mac, xrange=xran.n12, yrange=yran.n12)
Aweight <- exp(-6.907 + 0.046*Dboun)/(1+exp(-6.907 + 0.046*Dboun))
n12.c18.mac <- data.frame(n12.c18.mac[,c(15,16,17,18)], Aweight)

n13.c18.mac <- read.csv(file='N13_C18_MAC.csv',stringsAsFactors =FALSE)
Dboun <- distfind(dat=n13.c18.mac, xrange=xran.n13, yrange=yran.n13)
Aweight <- exp(-6.907 + 0.046*Dboun)/(1+exp(-6.907 + 0.046*Dboun))
n13.c18.mac <- data.frame(n13.c18.mac[,c(15,16,17,18)], Aweight)

n12.over <- n12.c18.mac[n12.c18.mac$uniqval %in% n13.c18.mac$uniqval,]
n13.over <- n13.c18.mac[n13.c18.mac$uniqval %in% n12.c18.mac$uniqval,]
sum(abs(n12.over$Xt-n13.over$Xt) + abs(n12.over$Yt-n13.over$Yt))

Vnew <- ((n12.over$pmacbreak*n12.over$Aweight) + (n13.over$pmacbreak*n13.over$Aweight))/(n12.over$Aweight + n13.over$Aweight) 

n12.c18.mac$pmacbreak[n12.c18.mac$uniqval %in% n13.c18.mac$uniqval] <- Vnew
n13.c18.mac$pmacbreak[n13.c18.mac$uniqval %in% n12.c18.mac$uniqval] <- Vnew

###

n14.c18.mac <- read.csv(file='N14_C18_MAC.csv')
Dboun <- distfind(dat=n14.c18.mac, xrange=xran.n14, yrange=yran.n14)
Aweight <- exp(-6.907 + 0.046*Dboun)/(1+exp(-6.907 + 0.046*Dboun))
n14.c18.mac <- data.frame(n14.c18.mac[,c(15,16,17,18)], Aweight)

n13.over <- n13.c18.mac[n13.c18.mac$uniqval %in% n14.c18.mac$uniqval,]
n14.over <- n14.c18.mac[n14.c18.mac$uniqval %in% n13.c18.mac$uniqval,]
sum(abs(n13.over$Xt-n14.over$Xt) + abs(n13.over$Yt-n14.over$Yt))

Vnew <- ((n13.over$pmacbreak*n13.over$Aweight) + (n14.over$pmacbreak*n14.over$Aweight))/(n13.over$Aweight + n14.over$Aweight) 

n13.c18.mac$pmacbreak[n13.c18.mac$uniqval %in% n14.c18.mac$uniqval] <- Vnew
n14.c18.mac$pmacbreak[n14.c18.mac$uniqval %in% n13.c18.mac$uniqval] <- Vnew

INDEXCOL <- 1:dim(n13.c18.mac)[1]
colvec <- tapply(X=n13.c18.mac$pmacbreak, INDEX=INDEXCOL, FUN=which.rowcol)
png(filename = "N13_C18_MACRO.png", width = 2000, height = 2000, units = "px", pointsize = 12, bg = "white",  res = 300,type = "cairo-png")
par(mar=c(2,2,1,1))
plot(Yt ~ Xt, data=n13.c18.mac, pch=15, col=colvec, asp=1, cex=0.17, xlab="", ylab="", bty="n", xaxt="n", yaxt="n", ylim=yran.n13, xlim=xran.n13)
lines(Yt ~ Xt, data=n13.shore, pch=15, col="grey", cex=0.17, type="p")
for(i in 1:63) {
	polygon(x=macact$lonNZ[macact$shape==i], y=macact$latNZ[macact$shape==i], border=1, lwd=2, col=NA)
}
YCV <- c(5419000,5418600,5419360,5419360,5420200,5420200,5420600,5420600,5420550,5421800,5421800,5421000,5419650,5421600)
XCV <- c(309080,309080,309040,310460,311220,312640,314020,315150,316000,315100,316400,317100,317100,316500)
lines(YCV ~ XCV, pch=3, cex=1, col=2, type="p")
dev.off()

INDEXCOL <- 1:dim(n14.c18.mac)[1]
colvec <- tapply(X=n14.c18.mac$pmacbreak, INDEX=INDEXCOL, FUN=which.rowcol)
png(filename = "N14_C18_MACRO.png", width = 2000, height = 2000, units = "px", pointsize = 12, bg = "white",  res = 300,type = "cairo-png")
par(mar=c(2,2,1,1))
plot(Yt ~ Xt, data=n14.c18.mac, pch=15, col=colvec, asp=1, cex=0.17, xlab="", ylab="", bty="n", xaxt="n", yaxt="n", ylim=yran.n14, xlim=xran.n14)
lines(Yt ~ Xt, data=n14.shore, pch=15, col="grey", cex=0.17, type="p")
for(i in 1:63) {
	polygon(x=macact$lonNZ[macact$shape==i], y=macact$latNZ[macact$shape==i], border=1, lwd=2, col=NA)
}
YCV <- c(5419000,5418600,5419360,5419360,5420200,5420200,5420600,5420600,5420550,5421800,5421800,5421000,5419650,5421600)
XCV <- c(309080,309080,309040,310460,311220,312640,314020,315150,316000,315100,316400,317100,317100,316500)
lines(YCV ~ XCV, pch=3, cex=1, col=2, type="p")
dev.off()

############################################################################################
############################################################################################
## C21

setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale")
n1.c21.mac <- read.csv(file='N1_C21_MAC.csv',stringsAsFactors =FALSE)
Dboun <- distfind(dat=n1.c21.mac, xrange=xran.n1, yrange=yran.n1)
Aweight <- exp(-6.907 + 0.046*Dboun)/(1+exp(-6.907 + 0.046*Dboun))
n1.c21.mac <- data.frame(n1.c21.mac[,c(15,16,17,18)], Aweight)

n2.c21.mac <- read.csv(file='N2_C21_MAC.csv',stringsAsFactors =FALSE)
Dboun <- distfind(dat=n2.c21.mac, xrange=xran.n2, yrange=yran.n2)
Aweight <- exp(-6.907 + 0.046*Dboun)/(1+exp(-6.907 + 0.046*Dboun))
n2.c21.mac <- data.frame(n2.c21.mac[,c(15,16,17,18)], Aweight)

n1.over <- n1.c21.mac[n1.c21.mac$uniqval %in% n2.c21.mac$uniqval,]
n2.over <- n2.c21.mac[n2.c21.mac$uniqval %in% n1.c21.mac$uniqval,]
sum(abs(n1.over$Xt-n2.over$Xt) + abs(n1.over$Yt-n2.over$Yt))

Vnew <- ((n1.over$pmacbreak*n1.over$Aweight) + (n2.over$pmacbreak*n2.over$Aweight))/(n1.over$Aweight + n2.over$Aweight) 
n1.c21.mac$pmacbreak[n1.c21.mac$uniqval %in% n2.c21.mac$uniqval] <- Vnew
n2.c21.mac$pmacbreak[n2.c21.mac$uniqval %in% n1.c21.mac$uniqval] <- Vnew

INDEXCOL <- 1:dim(n1.c21.mac)[1]
colvec <- tapply(X=n1.c21.mac$pmacbreak, INDEX=INDEXCOL, FUN=which.rowcol)

png(filename = "N1_C21_MACRO.png", width = 2000, height = 2000, units = "px", pointsize = 12, bg = "white",  res = 300,type = "cairo-png")
par(mar=c(2,2,1,1))
plot(Yt ~ Xt, data=n1.c21.mac, pch=15, col=colvec, asp=1, cex=0.17, xlab="", ylab="", bty="n", xaxt="n", yaxt="n", ylim=yran.n1, xlim=xran.n1)
lines(Yt ~ Xt, data=n1.shore, pch=15, col="grey", cex=0.17, type="p")
for(i in 1:63) {
	polygon(x=macact$lonNZ[macact$shape==i], y=macact$latNZ[macact$shape==i], border=1, lwd=2, col=NA)
}
YCV <- c(5419000,5418600,5419360,5419360,5420200,5420200,5420600,5420600,5420550,5421800,5421800,5421000,5419650,5421600)
XCV <- c(309080,309080,309040,310460,311220,312640,314020,315150,316000,315100,316400,317100,317100,316500)
lines(YCV ~ XCV, pch=3, cex=1, col=2, type="p")
dev.off()

#############

n3.c21.mac <- read.csv(file='N3_C21_MAC.csv',stringsAsFactors =FALSE)
Dboun <- distfind(dat=n3.c21.mac, xrange=xran.n3, yrange=yran.n3)
Aweight <- exp(-6.907 + 0.046*Dboun)/(1+exp(-6.907 + 0.046*Dboun))
n3.c21.mac <- data.frame(n3.c21.mac[,c(15,16,17,18)], Aweight)

n2.over <- n2.c21.mac[n2.c21.mac$uniqval %in% n3.c21.mac$uniqval,]
n3.over <- n3.c21.mac[n3.c21.mac$uniqval %in% n2.c21.mac$uniqval,]
sum(abs(n2.over$Xt-n3.over$Xt) + abs(n2.over$Yt-n3.over$Yt))

Vnew <- ((n2.over$pmacbreak*n2.over$Aweight) + (n3.over$pmacbreak*n3.over$Aweight))/(n2.over$Aweight + n3.over$Aweight) 
n2.c21.mac$pmacbreak[n2.c21.mac$uniqval %in% n3.c21.mac$uniqval] <- Vnew
n3.c21.mac$pmacbreak[n3.c21.mac$uniqval %in% n2.c21.mac$uniqval] <- Vnew

INDEXCOL <- 1:dim(n2.c21.mac)[1]
colvec <- tapply(X=n2.c21.mac$pmacbreak, INDEX=INDEXCOL, FUN=which.rowcol)

png(filename = "N2_C21_MACRO.png", width = 2000, height = 2000, units = "px", pointsize = 12, bg = "white",  res = 300,type = "cairo-png")
par(mar=c(2,2,1,1))
plot(Yt ~ Xt, data=n2.c21.mac, pch=15, col=colvec, asp=1, cex=0.17, xlab="", ylab="", bty="n", xaxt="n", yaxt="n", ylim=yran.n2, xlim=xran.n2)
lines(Yt ~ Xt, data=n2.shore, pch=15, col="grey", cex=0.17, type="p")
for(i in 1:63) {
	polygon(x=macact$lonNZ[macact$shape==i], y=macact$latNZ[macact$shape==i], border=1, lwd=2, col=NA)
}
YCV <- c(5419000,5418600,5419360,5419360,5420200,5420200,5420600,5420600,5420550,5421800,5421800,5421000,5419650,5421600)
XCV <- c(309080,309080,309040,310460,311220,312640,314020,315150,316000,315100,316400,317100,317100,316500)
lines(YCV ~ XCV, pch=3, cex=1, col=2, type="p")
dev.off()

#############

n4.c21.mac <- read.csv(file='N4_C21_MAC.csv',stringsAsFactors =FALSE)
Dboun <- distfind(dat=n4.c21.mac, xrange=xran.n4, yrange=yran.n4)
Aweight <- exp(-6.907 + 0.046*Dboun)/(1+exp(-6.907 + 0.046*Dboun))
n4.c21.mac <- data.frame(n4.c21.mac[,c(15,16,17,18)], Aweight)

n3.over <- n3.c21.mac[n3.c21.mac$uniqval %in% n4.c21.mac$uniqval,]
n4.over <- n4.c21.mac[n4.c21.mac$uniqval %in% n3.c21.mac$uniqval,]
sum(abs(n3.over$Xt-n4.over$Xt) + abs(n3.over$Yt-n4.over$Yt))

Vnew <- ((n3.over$pmacbreak*n3.over$Aweight) + (n4.over$pmacbreak*n4.over$Aweight))/(n3.over$Aweight + n4.over$Aweight) 
n3.c21.mac$pmacbreak[n3.c21.mac$uniqval %in% n4.c21.mac$uniqval] <- Vnew
n4.c21.mac$pmacbreak[n4.c21.mac$uniqval %in% n3.c21.mac$uniqval] <- Vnew

INDEXCOL <- 1:dim(n3.c21.mac)[1]
colvec <- tapply(X=n3.c21.mac$pmacbreak, INDEX=INDEXCOL, FUN=which.rowcol)

png(filename = "N3_C21_MACRO.png", width = 2000, height = 2000, units = "px", pointsize = 12, bg = "white",  res = 300,type = "cairo-png")
par(mar=c(2,2,1,1))
plot(Yt ~ Xt, data=n3.c21.mac, pch=15, col=colvec, asp=1, cex=0.17, xlab="", ylab="", bty="n", xaxt="n", yaxt="n", ylim=yran.n3, xlim=xran.n3)
lines(Yt ~ Xt, data=n3.shore, pch=15, col="grey", cex=0.17, type="p")
for(i in 1:63) {
	polygon(x=macact$lonNZ[macact$shape==i], y=macact$latNZ[macact$shape==i], border=1, lwd=2, col=NA)
}
YCV <- c(5419000,5418600,5419360,5419360,5420200,5420200,5420600,5420600,5420550,5421800,5421800,5421000,5419650,5421600)
XCV <- c(309080,309080,309040,310460,311220,312640,314020,315150,316000,315100,316400,317100,317100,316500)
lines(YCV ~ XCV, pch=3, cex=1, col=2, type="p")
dev.off()

#############

n5.c21.mac <- read.csv(file='N5_C21_MAC.csv',stringsAsFactors =FALSE)
Dboun <- distfind(dat=n5.c21.mac, xrange=xran.n5, yrange=yran.n5)
Aweight <- exp(-6.907 + 0.046*Dboun)/(1+exp(-6.907 + 0.046*Dboun))
n5.c21.mac <- data.frame(n5.c21.mac[,c(15,16,17,18)], Aweight)

n4.over <- n4.c21.mac[n4.c21.mac$uniqval %in% n5.c21.mac$uniqval,]
n5.over <- n5.c21.mac[n5.c21.mac$uniqval %in% n4.c21.mac$uniqval,]
sum(abs(n4.over$Xt-n5.over$Xt) + abs(n4.over$Yt-n5.over$Yt))

Vnew <- ((n4.over$pmacbreak*n4.over$Aweight) + (n5.over$pmacbreak*n5.over$Aweight))/(n4.over$Aweight + n5.over$Aweight) 
n4.c21.mac$pmacbreak[n4.c21.mac$uniqval %in% n5.c21.mac$uniqval] <- Vnew
n5.c21.mac$pmacbreak[n5.c21.mac$uniqval %in% n4.c21.mac$uniqval] <- Vnew

INDEXCOL <- 1:dim(n4.c21.mac)[1]
colvec <- tapply(X=n4.c21.mac$pmacbreak, INDEX=INDEXCOL, FUN=which.rowcol)

png(filename = "N4_C21_MACRO.png", width = 2000, height = 2000, units = "px", pointsize = 12, bg = "white",  res = 300,type = "cairo-png")
par(mar=c(2,2,1,1))
plot(Yt ~ Xt, data=n4.c21.mac, pch=15, col=colvec, asp=1, cex=0.17, xlab="", ylab="", bty="n", xaxt="n", yaxt="n", ylim=yran.n4, xlim=xran.n4)
lines(Yt ~ Xt, data=n4.shore, pch=15, col="grey", cex=0.17, type="p")
for(i in 1:63) {
	polygon(x=macact$lonNZ[macact$shape==i], y=macact$latNZ[macact$shape==i], border=1, lwd=2, col=NA)
}
YCV <- c(5419000,5418600,5419360,5419360,5420200,5420200,5420600,5420600,5420550,5421800,5421800,5421000,5419650,5421600)
XCV <- c(309080,309080,309040,310460,311220,312640,314020,315150,316000,315100,316400,317100,317100,316500)
lines(YCV ~ XCV, pch=3, cex=1, col=2, type="p")
dev.off()

#############

n6.c21.mac <- read.csv(file='N6_C21_MAC.csv',stringsAsFactors =FALSE)
Dboun <- distfind(dat=n6.c21.mac, xrange=xran.n6, yrange=yran.n6)
Aweight <- exp(-6.907 + 0.046*Dboun)/(1+exp(-6.907 + 0.046*Dboun))
n6.c21.mac <- data.frame(n6.c21.mac[,c(15,16,17,18)], Aweight)

n5.over <- n5.c21.mac[n5.c21.mac$uniqval %in% n6.c21.mac$uniqval,]
n6.over <- n6.c21.mac[n6.c21.mac$uniqval %in% n5.c21.mac$uniqval,]
sum(abs(n5.over$Xt-n6.over$Xt) + abs(n5.over$Yt-n6.over$Yt))

Vnew <- ((n5.over$pmacbreak*n5.over$Aweight) + (n6.over$pmacbreak*n6.over$Aweight))/(n5.over$Aweight + n6.over$Aweight) 
n5.c21.mac$pmacbreak[n5.c21.mac$uniqval %in% n6.c21.mac$uniqval] <- Vnew
n6.c21.mac$pmacbreak[n6.c21.mac$uniqval %in% n5.c21.mac$uniqval] <- Vnew

INDEXCOL <- 1:dim(n5.c21.mac)[1]
colvec <- tapply(X=n5.c21.mac$pmacbreak, INDEX=INDEXCOL, FUN=which.rowcol)

png(filename = "N5_C21_MACRO.png", width = 2000, height = 2000, units = "px", pointsize = 12, bg = "white",  res = 300,type = "cairo-png")
par(mar=c(2,2,1,1))
plot(Yt ~ Xt, data=n5.c21.mac, pch=15, col=colvec, asp=1, cex=0.17, xlab="", ylab="", bty="n", xaxt="n", yaxt="n", ylim=yran.n5, xlim=xran.n5)
lines(Yt ~ Xt, data=n5.shore, pch=15, col="grey", cex=0.17, type="p")
for(i in 1:63) {
	polygon(x=macact$lonNZ[macact$shape==i], y=macact$latNZ[macact$shape==i], border=1, lwd=2, col=NA)
}
YCV <- c(5419000,5418600,5419360,5419360,5420200,5420200,5420600,5420600,5420550,5421800,5421800,5421000,5419650,5421600)
XCV <- c(309080,309080,309040,310460,311220,312640,314020,315150,316000,315100,316400,317100,317100,316500)
lines(YCV ~ XCV, pch=3, cex=1, col=2, type="p")
dev.off()

#############

n7.c21.mac <- read.csv(file='N7_C21_MAC.csv',stringsAsFactors =FALSE)
Dboun <- distfind(dat=n7.c21.mac, xrange=xran.n7, yrange=yran.n7)
Aweight <- exp(-6.907 + 0.046*Dboun)/(1+exp(-6.907 + 0.046*Dboun))
n7.c21.mac <- data.frame(n7.c21.mac[,c(15,16,17,18)], Aweight)

n6.over <- n6.c21.mac[n6.c21.mac$uniqval %in% n7.c21.mac$uniqval,]
n7.over <- n7.c21.mac[n7.c21.mac$uniqval %in% n6.c21.mac$uniqval,]
sum(abs(n6.over$Xt-n7.over$Xt) + abs(n6.over$Yt-n7.over$Yt))

Vnew <- ((n6.over$pmacbreak*n6.over$Aweight) + (n7.over$pmacbreak*n7.over$Aweight))/(n6.over$Aweight + n7.over$Aweight) 
n6.c21.mac$pmacbreak[n6.c21.mac$uniqval %in% n7.c21.mac$uniqval] <- Vnew
n7.c21.mac$pmacbreak[n7.c21.mac$uniqval %in% n6.c21.mac$uniqval] <- Vnew

INDEXCOL <- 1:dim(n6.c21.mac)[1]
colvec <- tapply(X=n6.c21.mac$pmacbreak, INDEX=INDEXCOL, FUN=which.rowcol)

png(filename = "N6_C21_MACRO.png", width = 2000, height = 2000, units = "px", pointsize = 12, bg = "white",  res = 300,type = "cairo-png")
par(mar=c(2,2,1,1))
plot(Yt ~ Xt, data=n6.c21.mac, pch=15, col=colvec, asp=1, cex=0.17, xlab="", ylab="", bty="n", xaxt="n", yaxt="n", ylim=yran.n6, xlim=xran.n6)
lines(Yt ~ Xt, data=n6.shore, pch=15, col="grey", cex=0.17, type="p")
for(i in 1:63) {
	polygon(x=macact$lonNZ[macact$shape==i], y=macact$latNZ[macact$shape==i], border=1, lwd=2, col=NA)
}
YCV <- c(5419000,5418600,5419360,5419360,5420200,5420200,5420600,5420600,5420550,5421800,5421800,5421000,5419650,5421600)
XCV <- c(309080,309080,309040,310460,311220,312640,314020,315150,316000,315100,316400,317100,317100,316500)
lines(YCV ~ XCV, pch=3, cex=1, col=2, type="p")
dev.off()


##############
##############

n7.c21.mac <- read.csv(file='N7_C21_MAC.csv',stringsAsFactors =FALSE)
Dboun <- distfind(dat=n7.c21.mac, xrange=xran.n7, yrange=yran.n7)
Aweight <- exp(-6.907 + 0.046*Dboun)/(1+exp(-6.907 + 0.046*Dboun))
VmWeight <- n7.c21.mac$pmacbreak*Aweight
n7.c21.mac <- data.frame(n7.c21.mac[,c(15,16,17,18)], Aweight, VmWeight)

n8.c21.mac <- read.csv(file='N8_C21_MAC.csv',stringsAsFactors =FALSE)
Dboun <- distfind(dat=n8.c21.mac, xrange=xran.n8, yrange=yran.n8)
Aweight <- exp(-6.907 + 0.046*Dboun)/(1+exp(-6.907 + 0.046*Dboun))
VmWeight <- n8.c21.mac$pmacbreak*Aweight
n8.c21.mac <- data.frame(n8.c21.mac[,c(15,16,17,18)], Aweight, VmWeight)

n9.c21.mac <- read.csv(file='N9_C21_MAC.csv',stringsAsFactors =FALSE)
Dboun <- distfind(dat=n9.c21.mac, xrange=xran.n9, yrange=yran.n9)
Aweight <- exp(-6.907 + 0.046*Dboun)/(1+exp(-6.907 + 0.046*Dboun))
VmWeight <- n9.c21.mac$pmacbreak*Aweight
n9.c21.mac <- data.frame(n9.c21.mac[,c(15,16,17,18)], Aweight, VmWeight)

n10.c21.mac <- read.csv(file='N10_C21_MAC.csv',stringsAsFactors =FALSE)
Dboun <- distfind(dat=n10.c21.mac, xrange=xran.n10, yrange=yran.n10)
Aweight <- exp(-6.907 + 0.046*Dboun)/(1+exp(-6.907 + 0.046*Dboun))
VmWeight <- n10.c21.mac$pmacbreak*Aweight
n10.c21.mac <- data.frame(n10.c21.mac[,c(15,16,17,18)], Aweight, VmWeight)

n11.c21.mac <- read.csv(file='N11_C21_MAC.csv',stringsAsFactors =FALSE)
Dboun <- distfind(dat=n11.c21.mac, xrange=xran.n11, yrange=yran.n11)
Aweight <- exp(-6.907 + 0.046*Dboun)/(1+exp(-6.907 + 0.046*Dboun))
VmWeight <- n11.c21.mac$pmacbreak*Aweight
n11.c21.mac <- data.frame(n11.c21.mac[,c(15,16,17,18)], Aweight, VmWeight)

n12.c21.mac <- read.csv(file='N12_C21_MAC.csv',stringsAsFactors =FALSE)
Dboun <- distfind(dat=n12.c21.mac, xrange=xran.n12, yrange=yran.n12)
Aweight <- exp(-6.907 + 0.046*Dboun)/(1+exp(-6.907 + 0.046*Dboun))
VmWeight <- n12.c21.mac$pmacbreak*Aweight
n12.c21.mac <- data.frame(n12.c21.mac[,c(15,16,17,18)], Aweight, VmWeight)

nlyall <- rbind(n7.c21.mac, n8.c21.mac, n9.c21.mac, n10.c21.mac, n11.c21.mac, n12.c21.mac)

vmprelim <- tapply(X=nlyall$VmWeight, INDEX=nlyall$uniqval, FUN=sum)
weightprelim <- tapply(X=nlyall$Aweight, INDEX=nlyall$uniqval, FUN=sum)

LyallVmax <- vmprelim/weightprelim
coordstring <- rownames(LyallVmax)
thy <- unlist(strsplit(coordstring, split="_"))
mat_coord <- matrix(as.numeric(thy), nrow=length(LyallVmax), ncol=2, byrow=TRUE)

LyallDat <- data.frame(mat_coord, LyallVmax)
names(LyallDat) <- c("Xt", "Yt", "Pbreak")

n7.dat <- LyallDat[LyallDat$Xt >= xran.n7[1] & LyallDat$Xt <= xran.n7[2] & LyallDat$Yt >= yran.n7[1] & LyallDat$Yt <= yran.n7[2],]
n8.dat <- LyallDat[LyallDat$Xt >= xran.n8[1] & LyallDat$Xt <= xran.n8[2] & LyallDat$Yt >= yran.n8[1] & LyallDat$Yt <= yran.n8[2],]
n9.dat <- LyallDat[LyallDat$Xt >= xran.n9[1] & LyallDat$Xt <= xran.n9[2] & LyallDat$Yt >= yran.n9[1] & LyallDat$Yt <= yran.n9[2],]
n10.dat <- LyallDat[LyallDat$Xt >= xran.n10[1] & LyallDat$Xt <= xran.n10[2] & LyallDat$Yt >= yran.n10[1] & LyallDat$Yt <= yran.n10[2],]
n11.dat <- LyallDat[LyallDat$Xt >= xran.n11[1] & LyallDat$Xt <= xran.n11[2] & LyallDat$Yt >= yran.n11[1] & LyallDat$Yt <= yran.n11[2],]
n12.dat <- LyallDat[LyallDat$Xt >= xran.n12[1] & LyallDat$Xt <= xran.n12[2] & LyallDat$Yt >= yran.n12[1] & LyallDat$Yt <= yran.n12[2],]

INDEXCOL <- 1:dim(n7.dat)[1]
colvec <- tapply(X=n7.dat$Pbreak, INDEX=INDEXCOL, FUN=which.rowcol)
png(filename = "N7_C21_MACRO.png", width = 2000, height = 2000, units = "px", pointsize = 12, bg = "white",  res = 300,type = "cairo-png")
par(mar=c(2,2,1,1))
plot(Yt ~ Xt, data=n7.dat, pch=15, col=colvec, asp=1, cex=0.17, xlab="", ylab="", bty="n", xaxt="n", yaxt="n", ylim=yran.n7, xlim=xran.n7)
lines(Yt ~ Xt, data=n7.shore, pch=15, col="grey", cex=0.17, type="p")
for(i in 1:63) {
	polygon(x=macact$lonNZ[macact$shape==i], y=macact$latNZ[macact$shape==i], border=1, lwd=2, col=NA)
}
YCV <- c(5419000,5418600,5419360,5419360,5420200,5420200,5420600,5420600,5420550,5421800,5421800,5421000,5419650,5421600)
XCV <- c(309080,309080,309040,310460,311220,312640,314020,315150,316000,315100,316400,317100,317100,316500)
lines(YCV ~ XCV, pch=3, cex=1, col=2, type="p")
dev.off()

INDEXCOL <- 1:dim(n8.dat)[1]
colvec <- tapply(X=n8.dat$Pbreak, INDEX=INDEXCOL, FUN=which.rowcol)
png(filename = "N8_C21_MACRO.png", width = 2000, height = 2000, units = "px", pointsize = 12, bg = "white",  res = 300,type = "cairo-png")
par(mar=c(2,2,1,1))
plot(Yt ~ Xt, data=n8.dat, pch=15, col=colvec, asp=1, cex=0.17, xlab="", ylab="", bty="n", xaxt="n", yaxt="n", ylim=yran.n8, xlim=xran.n8)
lines(Yt ~ Xt, data=n8.shore, pch=15, col="grey", cex=0.17, type="p")
for(i in 1:63) {
	polygon(x=macact$lonNZ[macact$shape==i], y=macact$latNZ[macact$shape==i], border=1, lwd=2, col=NA)
}
YCV <- c(5419000,5418600,5419360,5419360,5420200,5420200,5420600,5420600,5420550,5421800,5421800,5421000,5419650,5421600)
XCV <- c(309080,309080,309040,310460,311220,312640,314020,315150,316000,315100,316400,317100,317100,316500)
lines(YCV ~ XCV, pch=3, cex=1, col=2, type="p")
dev.off()

INDEXCOL <- 1:dim(n9.dat)[1]
colvec <- tapply(X=n9.dat$Pbreak, INDEX=INDEXCOL, FUN=which.rowcol)
png(filename = "N9_C21_MACRO.png", width = 2000, height = 2000, units = "px", pointsize = 12, bg = "white",  res = 300,type = "cairo-png")
par(mar=c(2,2,1,1))
plot(Yt ~ Xt, data=n9.dat, pch=15, col=colvec, asp=1, cex=0.17, xlab="", ylab="", bty="n", xaxt="n", yaxt="n", ylim=yran.n9, xlim=xran.n9)
lines(Yt ~ Xt, data=n9.shore, pch=15, col="grey", cex=0.17, type="p")
for(i in 1:63) {
	polygon(x=macact$lonNZ[macact$shape==i], y=macact$latNZ[macact$shape==i], border=1, lwd=2, col=NA)
}
YCV <- c(5419000,5418600,5419360,5419360,5420200,5420200,5420600,5420600,5420550,5421800,5421800,5421000,5419650,5421600)
XCV <- c(309080,309080,309040,310460,311220,312640,314020,315150,316000,315100,316400,317100,317100,316500)
lines(YCV ~ XCV, pch=3, cex=1, col=2, type="p")
dev.off()

INDEXCOL <- 1:dim(n10.dat)[1]
colvec <- tapply(X=n10.dat$Pbreak, INDEX=INDEXCOL, FUN=which.rowcol)
png(filename = "N10_C21_MACRO.png", width = 2000, height = 2000, units = "px", pointsize = 12, bg = "white",  res = 300,type = "cairo-png")
par(mar=c(2,2,1,1))
plot(Yt ~ Xt, data=n10.dat, pch=15, col=colvec, asp=1, cex=0.17, xlab="", ylab="", bty="n", xaxt="n", yaxt="n", ylim=yran.n10, xlim=xran.n10)
lines(Yt ~ Xt, data=n10.shore, pch=15, col="grey", cex=0.17, type="p")
for(i in 1:63) {
	polygon(x=macact$lonNZ[macact$shape==i], y=macact$latNZ[macact$shape==i], border=1, lwd=2, col=NA)
}
YCV <- c(5419000,5418600,5419360,5419360,5420200,5420200,5420600,5420600,5420550,5421800,5421800,5421000,5419650,5421600)
XCV <- c(309080,309080,309040,310460,311220,312640,314020,315150,316000,315100,316400,317100,317100,316500)
lines(YCV ~ XCV, pch=3, cex=1, col=2, type="p")
dev.off()

INDEXCOL <- 1:dim(n11.dat)[1]
colvec <- tapply(X=n11.dat$Pbreak, INDEX=INDEXCOL, FUN=which.rowcol)
png(filename = "N11_C21_MACRO.png", width = 2000, height = 2000, units = "px", pointsize = 12, bg = "white",  res = 300,type = "cairo-png")
par(mar=c(2,2,1,1))
plot(Yt ~ Xt, data=n11.dat, pch=15, col=colvec, asp=1, cex=0.17, xlab="", ylab="", bty="n", xaxt="n", yaxt="n", ylim=yran.n11, xlim=xran.n11)
lines(Yt ~ Xt, data=n11.shore, pch=15, col="grey", cex=0.17, type="p")
for(i in 1:63) {
	polygon(x=macact$lonNZ[macact$shape==i], y=macact$latNZ[macact$shape==i], border=1, lwd=2, col=NA)
}
YCV <- c(5419000,5418600,5419360,5419360,5420200,5420200,5420600,5420600,5420550,5421800,5421800,5421000,5419650,5421600)
XCV <- c(309080,309080,309040,310460,311220,312640,314020,315150,316000,315100,316400,317100,317100,316500)
lines(YCV ~ XCV, pch=3, cex=1, col=2, type="p")
dev.off()

INDEXCOL <- 1:dim(n12.dat)[1]
colvec <- tapply(X=n12.dat$Pbreak, INDEX=INDEXCOL, FUN=which.rowcol)
png(filename = "N12_C21_MACRO.png", width = 2000, height = 2000, units = "px", pointsize = 12, bg = "white",  res = 300,type = "cairo-png")
par(mar=c(2,2,1,1))
plot(Yt ~ Xt, data=n12.dat, pch=15, col=colvec, asp=1, cex=0.17, xlab="", ylab="", bty="n", xaxt="n", yaxt="n", ylim=yran.n12, xlim=xran.n12)
lines(Yt ~ Xt, data=n12.shore, pch=15, col="grey", cex=0.17, type="p")
for(i in 1:63) {
	polygon(x=macact$lonNZ[macact$shape==i], y=macact$latNZ[macact$shape==i], border=1, lwd=2, col=NA)
}
YCV <- c(5419000,5418600,5419360,5419360,5420200,5420200,5420600,5420600,5420550,5421800,5421800,5421000,5419650,5421600)
XCV <- c(309080,309080,309040,310460,311220,312640,314020,315150,316000,315100,316400,317100,317100,316500)
lines(YCV ~ XCV, pch=3, cex=1, col=2, type="p")
dev.off()



#################################################
#################################################


n12.c21.mac <- read.csv(file='N12_C21_MAC.csv',stringsAsFactors =FALSE)
Dboun <- distfind(dat=n12.c21.mac, xrange=xran.n12, yrange=yran.n12)
Aweight <- exp(-6.907 + 0.046*Dboun)/(1+exp(-6.907 + 0.046*Dboun))
n12.c21.mac <- data.frame(n12.c21.mac[,c(15,16,17,18)], Aweight)

n13.c21.mac <- read.csv(file='N13_C21_MAC.csv',stringsAsFactors =FALSE)
Dboun <- distfind(dat=n13.c21.mac, xrange=xran.n13, yrange=yran.n13)
Aweight <- exp(-6.907 + 0.046*Dboun)/(1+exp(-6.907 + 0.046*Dboun))
n13.c21.mac <- data.frame(n13.c21.mac[,c(15,16,17,18)], Aweight)

n12.over <- n12.c21.mac[n12.c21.mac$uniqval %in% n13.c21.mac$uniqval,]
n13.over <- n13.c21.mac[n13.c21.mac$uniqval %in% n12.c21.mac$uniqval,]
sum(abs(n12.over$Xt-n13.over$Xt) + abs(n12.over$Yt-n13.over$Yt))

Vnew <- ((n12.over$pmacbreak*n12.over$Aweight) + (n13.over$pmacbreak*n13.over$Aweight))/(n12.over$Aweight + n13.over$Aweight) 

n12.c21.mac$pmacbreak[n12.c21.mac$uniqval %in% n13.c21.mac$uniqval] <- Vnew
n13.c21.mac$pmacbreak[n13.c21.mac$uniqval %in% n12.c21.mac$uniqval] <- Vnew

###

n14.c21.mac <- read.csv(file='N14_C21_MAC.csv')
Dboun <- distfind(dat=n14.c21.mac, xrange=xran.n14, yrange=yran.n14)
Aweight <- exp(-6.907 + 0.046*Dboun)/(1+exp(-6.907 + 0.046*Dboun))
n14.c21.mac <- data.frame(n14.c21.mac[,c(15,16,17,18)], Aweight)

n13.over <- n13.c21.mac[n13.c21.mac$uniqval %in% n14.c21.mac$uniqval,]
n14.over <- n14.c21.mac[n14.c21.mac$uniqval %in% n13.c21.mac$uniqval,]
sum(abs(n13.over$Xt-n14.over$Xt) + abs(n13.over$Yt-n14.over$Yt))

Vnew <- ((n13.over$pmacbreak*n13.over$Aweight) + (n14.over$pmacbreak*n14.over$Aweight))/(n13.over$Aweight + n14.over$Aweight) 

n13.c21.mac$pmacbreak[n13.c21.mac$uniqval %in% n14.c21.mac$uniqval] <- Vnew
n14.c21.mac$pmacbreak[n14.c21.mac$uniqval %in% n13.c21.mac$uniqval] <- Vnew

INDEXCOL <- 1:dim(n13.c21.mac)[1]
colvec <- tapply(X=n13.c21.mac$pmacbreak, INDEX=INDEXCOL, FUN=which.rowcol)
png(filename = "N13_C21_MACRO.png", width = 2000, height = 2000, units = "px", pointsize = 12, bg = "white",  res = 300,type = "cairo-png")
par(mar=c(2,2,1,1))
plot(Yt ~ Xt, data=n13.c21.mac, pch=15, col=colvec, asp=1, cex=0.17, xlab="", ylab="", bty="n", xaxt="n", yaxt="n", ylim=yran.n13, xlim=xran.n13)
lines(Yt ~ Xt, data=n13.shore, pch=15, col="grey", cex=0.17, type="p")
for(i in 1:63) {
	polygon(x=macact$lonNZ[macact$shape==i], y=macact$latNZ[macact$shape==i], border=1, lwd=2, col=NA)
}
YCV <- c(5419000,5418600,5419360,5419360,5420200,5420200,5420600,5420600,5420550,5421800,5421800,5421000,5419650,5421600)
XCV <- c(309080,309080,309040,310460,311220,312640,314020,315150,316000,315100,316400,317100,317100,316500)
lines(YCV ~ XCV, pch=3, cex=1, col=2, type="p")
dev.off()

INDEXCOL <- 1:dim(n14.c21.mac)[1]
colvec <- tapply(X=n14.c21.mac$pmacbreak, INDEX=INDEXCOL, FUN=which.rowcol)
png(filename = "N14_C21_MACRO.png", width = 2000, height = 2000, units = "px", pointsize = 12, bg = "white",  res = 300,type = "cairo-png")
par(mar=c(2,2,1,1))
plot(Yt ~ Xt, data=n14.c21.mac, pch=15, col=colvec, asp=1, cex=0.17, xlab="", ylab="", bty="n", xaxt="n", yaxt="n", ylim=yran.n14, xlim=xran.n14)
lines(Yt ~ Xt, data=n14.shore, pch=15, col="grey", cex=0.17, type="p")
for(i in 1:63) {
	polygon(x=macact$lonNZ[macact$shape==i], y=macact$latNZ[macact$shape==i], border=1, lwd=2, col=NA)
}
YCV <- c(5419000,5418600,5419360,5419360,5420200,5420200,5420600,5420600,5420550,5421800,5421800,5421000,5419650,5421600)
XCV <- c(309080,309080,309040,310460,311220,312640,314020,315150,316000,315100,316400,317100,317100,316500)
lines(YCV ~ XCV, pch=3, cex=1, col=2, type="p")
dev.off()





######################################################################
######################################################################
#### Summary statistics of breakage
 #############

get.char <- function(dat){
	depth.ret <- dat[1]
	return(depth.ret)
}

profinder <- function(dat){
	denom <- length(dat)
	class0 <- length(dat[dat < 0.001])/denom
	class1 <- length(dat[dat >= 0.001 & dat < 0.005])/denom
	class2 <- length(dat[dat >= 0.005 & dat < 0.01])/denom
	class3 <- length(dat[dat >=0.01 & dat < 0.05])/denom
	class4 <- length(dat[dat >= 0.05 & dat < 0.1])/denom
	class5 <- length(dat[dat >= 0.1 & dat < 0.15])/denom
	class6 <- length(dat[dat >= 0.15 & dat < 0.2])/denom
	class7 <- max(dat)
	class8 <- median(dat)
	class9 <- dat[rank(dat, ties.method="random")== floor(0.05*denom)]
	class10 <- dat[rank(dat, ties.method="random")== floor(0.95*denom)]

	xc1 <- c(class0,class1, class2, class3, class4, class5, class6, class7, class8, class9, class10)
	
	return(xc1)

}


setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale")
n1.c18 <- read.csv(file='N1_C18_MAC.csv',stringsAsFactors=FALSE)
n1.c18 <- n1.c18[n1.c18$Substrate!="Sand",c(9,12,14,15,16,17,18,19)]
n2.c18 <- read.csv(file='N2_C18_MAC.csv',stringsAsFactors=FALSE)
n2.c18 <- n2.c18[n2.c18$Substrate!="Sand",c(9,12,14,15,16,17,18,19)]
n3.c18 <- read.csv(file='N3_C18_MAC.csv',stringsAsFactors=FALSE)
n3.c18 <- n3.c18[n3.c18$Substrate!="Sand",c(9,12,14,15,16,17,18,19)]
n4.c18 <- read.csv(file='N4_C18_MAC.csv',stringsAsFactors=FALSE)
n4.c18 <- n4.c18[n4.c18$Substrate!="Sand",c(9,12,14,15,16,17,18,19)]
n5.c18 <- read.csv(file='N5_C18_MAC.csv',stringsAsFactors=FALSE)
n5.c18 <- n5.c18[n5.c18$Substrate!="Sand",c(9,12,14,15,16,17,18,19)]
n6.c18 <- read.csv(file='N6_C18_MAC.csv',stringsAsFactors=FALSE)
n6.c18 <- n6.c18[n6.c18$Substrate!="Sand",c(9,12,14,15,16,17,18,19)]
n7.c18 <- read.csv(file='N7_C18_MAC.csv',stringsAsFactors=FALSE)
n7.c18 <- n7.c18[n7.c18$Substrate!="Sand",c(9,12,14,15,16,17,18,19)]
n8.c18 <- read.csv(file='N8_C18_MAC.csv',stringsAsFactors=FALSE)
n8.c18 <- n8.c18[n8.c18$Substrate!="Sand",c(9,12,14,15,16,17,18,19)]
n9.c18 <- read.csv(file='N9_C18_MAC.csv',stringsAsFactors=FALSE)
n9.c18 <- n9.c18[n9.c18$Substrate!="Sand",c(9,12,14,15,16,17,18,19)]
n10.c18 <- read.csv(file='N10_C18_MAC.csv',stringsAsFactors=FALSE)
n10.c18 <- n10.c18[n10.c18$Substrate!="Sand",c(9,12,14,15,16,17,18,19)]
n11.c18 <- read.csv(file='N11_C18_MAC.csv',stringsAsFactors=FALSE)
n11.c18 <- n11.c18[n11.c18$Substrate!="Sand",c(9,12,14,15,16,17,18,19)]
n12.c18 <- read.csv(file='N12_C18_MAC.csv',stringsAsFactors=FALSE)
n12.c18 <- n12.c18[n12.c18$Substrate!="Sand",c(9,12,14,15,16,17,18,19)]
n13.c18 <- read.csv(file='N13_C18_MAC.csv',stringsAsFactors=FALSE)
n13.c18 <- n13.c18[n13.c18$Substrate!="Sand",c(9,12,14,15,16,17,18,19)]
n14.c18 <- read.csv(file='N14_C18_MAC.csv',stringsAsFactors=FALSE)
n14.c18 <- n14.c18[n14.c18$Substrate!="Sand",c(9,12,14,15,16,17,18,19)]

c18.dat <- rbind(n1.c18, n2.c18, n3.c18, n4.c18, n5.c18, n6.c18, n7.c18, n8.c18, n9.c18, n10.c18, n11.c18, n12.c18, n13.c18, n14.c18) 

c18.pmac <- tapply(X=c18.dat$pmacbreak, INDEX=c18.dat$uniqval, FUN=mean)
c18.depth <- tapply(X=c18.dat$Depth, INDEX=c18.dat$uniqval, FUN=get.char)
c18.qb <- tapply(X=c18.dat$Qb, INDEX=c18.dat$uniqval, FUN=mean)
c18.Xt <- tapply(X=c18.dat$Xt, INDEX=c18.dat$uniqval, FUN=get.char)
c18.Yt <- tapply(X=c18.dat$Yt, INDEX=c18.dat$uniqval, FUN=get.char)

c18_data <- data.frame(pmacbreak=c18.pmac, Depth=c18.depth, Qb=c18.qb, Xt=c18.Xt, Yt=c18.Yt)

pmac.0to5_c18 <- c18_data[c18_data$Depth <= 5,]
pmac.5to10_c18 <- c18_data[c18_data$Depth <= 10 & c18_data$Depth > 5,]
pmac.10to15_c18 <- c18_data[c18_data$Depth <= 15 & c18_data$Depth > 10,]
pmac.15to20_c18 <- c18_data[c18_data$Depth <= 20 & c18_data$Depth > 15,]

profinder(dat=pmac.0to5_c18$pmacbreak)

####################
#C21


setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale")
n1.c21 <- read.csv(file='N1_C21_MAC.csv',stringsAsFactors=FALSE)
n1.c21 <- n1.c21[n1.c21$Substrate!="Sand",c(9,12,14,15,16,17,18,19)]
n2.c21 <- read.csv(file='N2_C21_MAC.csv',stringsAsFactors=FALSE)
n2.c21 <- n2.c21[n2.c21$Substrate!="Sand",c(9,12,14,15,16,17,18,19)]
n3.c21 <- read.csv(file='N3_C21_MAC.csv',stringsAsFactors=FALSE)
n3.c21 <- n3.c21[n3.c21$Substrate!="Sand",c(9,12,14,15,16,17,18,19)]
n4.c21 <- read.csv(file='N4_C21_MAC.csv',stringsAsFactors=FALSE)
n4.c21 <- n4.c21[n4.c21$Substrate!="Sand",c(9,12,14,15,16,17,18,19)]
n5.c21 <- read.csv(file='N5_C21_MAC.csv',stringsAsFactors=FALSE)
n5.c21 <- n5.c21[n5.c21$Substrate!="Sand",c(9,12,14,15,16,17,18,19)]
n6.c21 <- read.csv(file='N6_C21_MAC.csv',stringsAsFactors=FALSE)
n6.c21 <- n6.c21[n6.c21$Substrate!="Sand",c(9,12,14,15,16,17,18,19)]
n7.c21 <- read.csv(file='N7_C21_MAC.csv',stringsAsFactors=FALSE)
n7.c21 <- n7.c21[n7.c21$Substrate!="Sand",c(9,12,14,15,16,17,18,19)]
n8.c21 <- read.csv(file='N8_C21_MAC.csv',stringsAsFactors=FALSE)
n8.c21 <- n8.c21[n8.c21$Substrate!="Sand",c(9,12,14,15,16,17,18,19)]
n9.c21 <- read.csv(file='N9_C21_MAC.csv',stringsAsFactors=FALSE)
n9.c21 <- n9.c21[n9.c21$Substrate!="Sand",c(9,12,14,15,16,17,18,19)]
n10.c21 <- read.csv(file='N10_C21_MAC.csv',stringsAsFactors=FALSE)
n10.c21 <- n10.c21[n10.c21$Substrate!="Sand",c(9,12,14,15,16,17,18,19)]
n11.c21 <- read.csv(file='N11_C21_MAC.csv',stringsAsFactors=FALSE)
n11.c21 <- n11.c21[n11.c21$Substrate!="Sand",c(9,12,14,15,16,17,18,19)]
n12.c21 <- read.csv(file='N12_C21_MAC.csv',stringsAsFactors=FALSE)
n12.c21 <- n12.c21[n12.c21$Substrate!="Sand",c(9,12,14,15,16,17,18,19)]
n13.c21 <- read.csv(file='N13_C21_MAC.csv',stringsAsFactors=FALSE)
n13.c21 <- n13.c21[n13.c21$Substrate!="Sand",c(9,12,14,15,16,17,18,19)]
n14.c21 <- read.csv(file='N14_C21_MAC.csv',stringsAsFactors=FALSE)
n14.c21 <- n14.c21[n14.c21$Substrate!="Sand",c(9,12,14,15,16,17,18,19)]

c21.dat <- rbind(n1.c21, n2.c21, n3.c21, n4.c21, n5.c21, n6.c21, n7.c21, n8.c21, n9.c21, n10.c21, n11.c21, n12.c21, n13.c21, n14.c21) 

c21.pmac <- tapply(X=c21.dat$pmacbreak, INDEX=c21.dat$uniqval, FUN=mean)
c21.depth <- tapply(X=c21.dat$Depth, INDEX=c21.dat$uniqval, FUN=get.char)
c21.qb <- tapply(X=c21.dat$Qb, INDEX=c21.dat$uniqval, FUN=mean)
c21.Xt <- tapply(X=c21.dat$Xt, INDEX=c21.dat$uniqval, FUN=get.char)
c21.Yt <- tapply(X=c21.dat$Yt, INDEX=c21.dat$uniqval, FUN=get.char)

c21_data <- data.frame(pmacbreak=c21.pmac, Depth=c21.depth, Qb=c21.qb, Xt=c21.Xt, Yt=c21.Yt)

pmac.0to5_c21 <- c21_data[c21_data$Depth <= 5,]
pmac.5to10_c21 <- c21_data[c21_data$Depth <= 10 & c21_data$Depth > 5,]
pmac.10to15_c21 <- c21_data[c21_data$Depth <= 15 & c21_data$Depth > 10,]
pmac.15to20_c21 <- c21_data[c21_data$Depth <= 20 & c21_data$Depth > 15,]

profinder(dat=pmac.0to5_c21$pmacbreak)


# Extract data for regression
library(sp)
library(maptools)
set.seed(5263)

sum(abs(c18_data$Xt - c21_data$Xt) + abs(c18_data$Yt - c21_data$Yt))

combi.dat <- data.frame(c18_data, c21_data[,c(1,3)])
names(combi.dat) <- c("pmacbreak.c18", "Depth", "Qb.c18", "Xt", "Yt", "pmacbreak.c21","Qb.c21")
combi.dat <- combi.dat[combi.dat$Xt >= 308450 & combi.dat$Xt <= 319050,]
combi.dat.d10 <- combi.dat[combi.dat$Depth <= 10,]

rownum1 <- 1:dim(combi.dat)[1]
rownum2 <- 1:dim(combi.dat.d10)[1]
sample1 <- sample(rownum1, size=20000, replace=FALSE)
sample2 <- sample(rownum2, size=20000, replace=FALSE)

samp.dat <- combi.dat[sample1,]
samp.dat.d10 <- combi.dat.d10[sample2,]

setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo")
macdat <- read.csv(file="Macrocystis_actual.csv")

in_out <- array(0, dim=c(20000))
for(i in 1:20000) {
	j <- 1
	while(j < 64){
		testpoly <- point.in.polygon(point.x=samp.dat$Xt[i], point.y=samp.dat$Yt[i], pol.x=macdat$lonNZ[macdat$shape==j], pol.y=macdat$latNZ[macdat$shape==j], mode.checked=FALSE)
		
		if(testpoly==1){
			in_out[i] <- 1
			j <- 200
		} else {
			j <- j+1
		}
		
	}
}

samp.dat <- data.frame(samp.dat, in_out)



in_out <- array(0, dim=c(20000))
for(i in 1:20000) {
	j <- 1
	while(j < 64){
		testpoly <- point.in.polygon(point.x=samp.dat.d10$Xt[i], point.y=samp.dat.d10$Yt[i], pol.x=macdat$lonNZ[macdat$shape==j], pol.y=macdat$latNZ[macdat$shape==j], mode.checked=FALSE)
		
		if(testpoly==1){
			in_out[i] <- 1
			j <- 200
		} else {
			j <- j+1
		}
		
	}
}

samp.dat.d10 <- data.frame(samp.dat.d10, in_out)

write.csv(samp.dat, file="Macro_regression.csv")
write.csv(samp.dat.d10, file="Macro_regression_d10.csv")

#######
#######################################
#######################################
# Analysing Macrocystis data
setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo")
macro.d20 <- read.csv(file="Macro_regression.csv")
macro.d10 <- read.csv(file="Macro_regression_d10.csv")
names(macro.d20) <- c("pmac.c18","Depth","Q.c18","Xt","Yt","pmac.c21","Q.c21","in_out")  
names(macro.d10) <- c("pmac.c18","Depth","Q.c18","Xt","Yt","pmac.c21","Q.c21","in_out")  
macro.d10 <- macro.d10[macro.d10$pmac.c18!=0,]

Dist <- sqrt((macro.d20$Xt - 319954.21)^2 + (macro.d20$Yt - 5422170.12)^2)
macro.d20 <- data.frame(macro.d20, Dist)

Dist <- sqrt((macro.d10$Xt - 319954.21)^2 + (macro.d10$Yt - 5422170.12)^2)
macro.d10 <- data.frame(macro.d10, Dist)


flogit <- function(dat){
	out <- log(dat/(1-dat))
	return(out)
}

### GLM models
# Macro d20
glm0 <- glm(in_out ~ 1, data=macro.d10, family=binomial(link="logit"))
glm1 <- glm(in_out ~ 1 + Depth, data=macro.d10, family=binomial(link="logit"))
glm2 <- glm(in_out ~ 1 + flogit(pmac.c18), data=macro.d10, family=binomial(link="logit"))
glm3 <- glm(in_out ~ 1 + flogit(pmac.c21), data=macro.d10, family=binomial(link="logit"))
glm4 <- glm(in_out ~ 1 + Q.c18, data=macro.d10, family=binomial(link="logit"))
glm5 <- glm(in_out ~ 1 + Q.c21, data=macro.d10, family=binomial(link="logit"))
glm6 <- glm(in_out ~ 1 + Depth + flogit(pmac.c18), data=macro.d10, family=binomial(link="logit"))
glm7 <- glm(in_out ~ 1 + Depth + flogit(pmac.c21), data=macro.d10, family=binomial(link="logit"))
glm8 <- glm(in_out ~ 1 + Depth + Q.c18, data=macro.d10, family=binomial(link="logit"))
glm9 <- glm(in_out ~ 1 + flogit(pmac.c18) + flogit(pmac.c21), data=macro.d10, family=binomial(link="logit"))
glm10 <- glm(in_out ~ 1 + flogit(pmac.c18) + Q.c18, data=macro.d10, family=binomial(link="logit"))
glm11 <- glm(in_out ~ 1 + flogit(pmac.c21) + Q.c18, data=macro.d10, family=binomial(link="logit"))
glm12 <- glm(in_out ~ 1 + Depth + Q.c21, data=macro.d10, family=binomial(link="logit"))
glm13 <- glm(in_out ~ 1 + flogit(pmac.c18) + Q.c21, data=macro.d10, family=binomial(link="logit"))
glm14 <- glm(in_out ~ 1 + flogit(pmac.c21) + Q.c21, data=macro.d10, family=binomial(link="logit"))
glm15 <- glm(in_out ~ 1 + Depth + flogit(pmac.c18) + flogit(pmac.c21), data=macro.d10, family=binomial(link="logit"))
glm16 <- glm(in_out ~ 1 + Depth + flogit(pmac.c18) + Q.c18, data=macro.d10, family=binomial(link="logit"))
glm17 <- glm(in_out ~ 1 + Depth + flogit(pmac.c21) + Q.c18, data=macro.d10, family=binomial(link="logit"))
glm18 <- glm(in_out ~ 1 + flogit(pmac.c18) + flogit(pmac.c21) + Q.c18, data=macro.d10, family=binomial(link="logit"))
glm19 <- glm(in_out ~ 1 + Depth + flogit(pmac.c18) + Q.c21, data=macro.d10, family=binomial(link="logit"))
glm20 <- glm(in_out ~ 1 + Depth + flogit(pmac.c21) + Q.c21, data=macro.d10, family=binomial(link="logit"))
glm21 <- glm(in_out ~ 1 + flogit(pmac.c18) + flogit(pmac.c21) + Q.c21, data=macro.d10, family=binomial(link="logit"))
glm22 <- glm(in_out ~ 1 + Depth + flogit(pmac.c18) + flogit(pmac.c21) + Q.c18, data=macro.d10, family=binomial(link="logit"))
glm23 <- glm(in_out ~ 1 + Depth + flogit(pmac.c18) + flogit(pmac.c21) + Q.c21, data=macro.d10, family=binomial(link="logit"))

glm.out <- c(round(AIC(glm0),2), round(AIC(glm1),2), round(AIC(glm2),2), round(AIC(glm3),2), round(AIC(glm4),2), round(AIC(glm5),2), round(AIC(glm6),2), round(AIC(glm7),2), round(AIC(glm8),2), round(AIC(glm9),2), round(AIC(glm10),2),  round(AIC(glm11),2), round(AIC(glm12),2), round(AIC(glm13),2), round(AIC(glm14),2), round(AIC(glm15),2), round(AIC(glm16),2), round(AIC(glm17),2), round(AIC(glm18),2), round(AIC(glm19),2), round(AIC(glm20),2), round(AIC(glm21),2), round(AIC(glm22),2), round(AIC(glm23),2))

# GAM models
library(mgcv)
# Macro d20
gamest <- log(dim(macro.d20)[1])/2

gam1 <- gam(in_out ~ 1 + s(Depth, k=20), data=macro.d10, family=binomial(link="logit"), gamma=2)
gam2 <- gam(in_out ~ 1 + s(pmac.c18, k=20), data=macro.d10, family=binomial(link="logit"), gamma=2)
gam3 <- gam(in_out ~ 1 + s(pmac.c21, k=20), data=macro.d10, family=binomial(link="logit"), gamma=2)
gam4 <- gam(in_out ~ 1 + s(sqrt(Q.c18), k=20), data=macro.d10, family=binomial(link="logit"), gamma=2)
gam5 <- gam(in_out ~ 1 + s(sqrt(Q.c21), k=20), data=macro.d10, family=binomial(link="logit"), gamma=2)
gam6 <- gam(in_out ~ 1 + s(Depth, k=20) + s(pmac.c18, k=20), data=macro.d10, family=binomial(link="logit"), gamma=2)
gam7 <- gam(in_out ~ 1 + s(Depth, k=20) + s(pmac.c21, k=20), data=macro.d10, family=binomial(link="logit"), gamma=2)
gam8 <- gam(in_out ~ 1 + s(Depth, k=20) + s(sqrt(Q.c18), k=20), data=macro.d10, family=binomial(link="logit"), gamma=2)
gam9 <- gam(in_out ~ 1 + s(pmac.c18, k=20) + s(pmac.c21, k=20), data=macro.d10, family=binomial(link="logit"), gamma=2)
gam10 <- gam(in_out ~ 1 + s(pmac.c18, k=20) + s(sqrt(Q.c18), k=20), data=macro.d10, family=binomial(link="logit"), gamma=2)
gam11 <- gam(in_out ~ 1 + s(pmac.c21, k=20) + s(sqrt(Q.c18), k=20), data=macro.d10, family=binomial(link="logit"), gamma=2)
gam12 <- gam(in_out ~ 1 + s(Depth, k=20) + s(sqrt(Q.c21), k=20), data=macro.d10, family=binomial(link="logit"), gamma=2)
gam13 <- gam(in_out ~ 1 + s(pmac.c18, k=20) + s(sqrt(Q.c21), k=20), data=macro.d10, family=binomial(link="logit"), gamma=2)
gam14 <- gam(in_out ~ 1 + s(pmac.c21, k=20) + s(sqrt(Q.c21), k=20), data=macro.d10, family=binomial(link="logit"), gamma=2)
gam15 <- gam(in_out ~ 1 + s(Depth, k=20) + s(pmac.c18, k=20) + s(pmac.c21, k=20), data=macro.d10, family=binomial(link="logit"), gamma=2)
gam16 <- gam(in_out ~ 1 + s(Depth, k=20) + s(pmac.c18, k=20) + s(sqrt(Q.c18), k=20), data=macro.d10, family=binomial(link="logit"), gamma=2)
gam17 <- gam(in_out ~ 1 + s(Depth, k=20) + s(pmac.c21, k=20) + s(sqrt(Q.c18), k=20), data=macro.d10, family=binomial(link="logit"), gamma=2)
gam18 <- gam(in_out ~ 1 + s(pmac.c18, k=20) + s(pmac.c21, k=20) + s(sqrt(Q.c18), k=20), data=macro.d10, family=binomial(link="logit"), gamma=2)
gam19 <- gam(in_out ~ 1 + s(Depth, k=20) + s(pmac.c18, k=20) + s(sqrt(Q.c21), k=20), data=macro.d10, family=binomial(link="logit"), gamma=2)
gam20 <- gam(in_out ~ 1 + s(Depth, k=20) + s(pmac.c21, k=20) + s(sqrt(Q.c21), k=20), data=macro.d10, family=binomial(link="logit"), gamma=2)
gam21 <- gam(in_out ~ 1 + s(pmac.c18, k=20) + s(pmac.c21, k=20) + s(sqrt(Q.c21), k=20), data=macro.d10, family=binomial(link="logit"), gamma=2)
gam22 <- gam(in_out ~ 1 + s(Depth, k=20) + s(pmac.c18, k=20) + s(pmac.c21, k=20) + s(sqrt(Q.c18), k=20), data=macro.d10, family=binomial(link="logit"), gamma=2)
gam23 <- gam(in_out ~ 1 + s(Depth, k=20) + s(pmac.c18, k=20) + s(pmac.c21, k=20) + s(sqrt(Q.c21), k=20), data=macro.d10, family=binomial(link="logit"), gamma=2)

gam.out <- c(round(AIC(gam1),2), round(AIC(gam2),2), round(AIC(gam3),2), round(AIC(gam4),2), round(AIC(gam5),2), round(AIC(gam6),2), round(AIC(gam7),2), round(AIC(gam8),2), round(AIC(gam9),2), round(AIC(gam10),2),  round(AIC(gam11),2), round(AIC(gam12),2), round(AIC(gam13),2), round(AIC(gam14),2), round(AIC(gam15),2), round(AIC(gam16),2), round(AIC(gam17),2), round(AIC(gam18),2), round(AIC(gam19),2), round(AIC(gam20),2), round(AIC(gam21),2), round(AIC(gam22),2), round(AIC(gam23),2))

# Best models

best.glm.d20 <- glm(in_out ~ 1 + Depth + pmac.c18 + pmac.c21 + sqrt(Q.c18), data=macro.d20, family=binomial(link="logit"))
best.glm.d10 <- glm(in_out ~ 1 + Depth + pmac.c18 + pmac.c21 + sqrt(Q.c18), data=macro.d10, family=binomial(link="logit"))
best.gam.d20 <- gam(in_out ~ 1 + s(Depth, k=20) + s(pmac.c18, k=20) + s(pmac.c21, k=20) + s(sqrt(Q.c18), k=20), data=macro.d20, family=binomial(link="logit"), gamma=2)
best.gam.d10 <- gam(in_out ~ 1 + s(Depth, k=20) + s(pmac.c18, k=20) + s(pmac.c21, k=20) + s(sqrt(Q.c18), k=20), data=macro.d10, family=binomial(link="logit"), gamma=2)

prev.find <- function(dat,in_out_dat, n.grp, xlabel, ylimit) {

	ran <- range(dat)
	split.vec <- seq(from=ran[1], to=ran[2]+0.0000001, length.out=n.grp+1)
	dim.vec <- array(NA, dim=c(n.grp))
	
	for(i in 1:n.grp){
		denom <- length(dat[dat >= split.vec[i] & dat < split.vec[i+1]])
		if(denom==0){
			dim.vec[i] <- 0
		} else {
			dim.vec[i] <- length(in_out_dat[in_out_dat==1 & dat >= split.vec[i] & dat < split.vec[i+1]])/denom
		}
	}
	
	plot(-400, ylim=ylimit, xlim=ran, ylab="Prevalence", xlab=xlabel)
	for(i in 1:n.grp) {
		polygon(x=c(split.vec[i], split.vec[i+1], split.vec[i+1], split.vec[i]), y=c(0,0,dim.vec[i], dim.vec[i]), col=rgb(0,0,0,0.3))
	}
}

pmac.c18.d20 <- seq(from=0.0000006, to=0.02, length.out=100)
depth.d20 <- seq(from=3, to=20, length.out=100)
Q.c18.d20 <- seq(from=0, to=0.36, length.out=100)
pmac.c21.d20 <- seq(from=0.0000005, to=0.15, length.out=100)

pmac.c18.d10 <- seq(from=0.0000005, to=0.02025, length.out=100)
depth.d10 <- seq(from=3, to=10, length.out=100)
Q.c18.d10 <- seq(from=0, to=0.5, length.out=100)
pmac.c21.d10 <- seq(from=0.0000004, to=0.125, length.out=100)


# d20 plots

par(mfrow=c(2,2))
pred.dat <- data.frame(Depth=depth.d10, pmac.c18=median(macro.d10$pmac.c18), pmac.c21=median(macro.d10$pmac.c21),Q.c18=median(macro.d10$Q.c18))
pred.gam <- predict(best.gam.d10, newdata=pred.dat, se=TRUE, type="response")
pred.glm <- predict(best.glm.d10, newdata=pred.dat, se=TRUE, type="response")
dep.dat <- data.frame(Depth=depth.d10, mu.gam=pred.gam$fit, upper.gam=pred.gam$fit+2*pred.gam$se.fit, lower.gam=pred.gam$fit-2*pred.gam$se.fit, mu.glm=pred.glm$fit, upper.glm=pred.glm$fit+2*pred.glm$se.fit, lower.glm=pred.glm$fit-2*pred.glm$se.fit)
prev.find(dat=macro.d10$Depth, in_out_dat=macro.d10$in_out, n.grp=20, xlabel="Depth", ylimit=c(0,0.05))
lines(mu.gam ~ Depth, data=dep.dat, lwd=2, col=2, type="l")
lines(upper.gam ~ Depth, data=dep.dat, lwd=2, col=2, type="l")
lines(lower.gam ~ Depth, data=dep.dat, lwd=2, col=2, type="l")
lines(mu.glm ~ Depth, data=dep.dat, lwd=2, col=1, type="l")
lines(upper.glm ~ Depth, data=dep.dat, lwd=2, col=1, type="l")
lines(lower.glm ~ Depth, data=dep.dat, lwd=2, col=1, type="l")

pred.dat <- data.frame(Depth=5, pmac.c18=pmac.c18.d10, pmac.c21=mean(macro.d10$pmac.c21),Q.c18=mean(macro.d10$Q.c18))
pred.gam <- predict(best.gam.d10, newdata=pred.dat, se=TRUE, type="response")
pred.glm <- predict(best.glm.d10, newdata=pred.dat, se=TRUE, type="response")
dep.dat <- data.frame(Depth=pmac.c18.d10, mu.gam=pred.gam$fit, upper.gam=pred.gam$fit+2*pred.gam$se.fit, lower.gam=pred.gam$fit-2*pred.gam$se.fit, mu.glm=pred.glm$fit, upper.glm=pred.glm$fit+2*pred.glm$se.fit, lower.glm=pred.glm$fit-2*pred.glm$se.fit)
prev.find(dat=macro.d10$pmac.c18, in_out_dat=macro.d10$in_out, n.grp=20, xlabel="P fail (C18)", ylimit=c(0,0.045))
lines(mu.gam ~ Depth, data=dep.dat, lwd=2, col=2, type="l")
lines(upper.gam ~ Depth, data=dep.dat, lwd=2, col=2, type="l")
lines(lower.gam ~ Depth, data=dep.dat, lwd=2, col=2, type="l")
lines(mu.glm ~ Depth, data=dep.dat, lwd=2, col=1, type="l")
lines(upper.glm ~ Depth, data=dep.dat, lwd=2, col=1, type="l")
lines(lower.glm ~ Depth, data=dep.dat, lwd=2, col=1, type="l")


pred.dat <- data.frame(Depth=5, pmac.c18=mean(macro.d10$pmac.c18), pmac.c21=pmac.c21.d10,Q.c18=mean(macro.d10$Q.c18))
pred.gam <- predict(best.gam.d10, newdata=pred.dat, se=TRUE, type="response")
pred.glm <- predict(best.glm.d10, newdata=pred.dat, se=TRUE, type="response")
dep.dat <- data.frame(Depth=pmac.c21.d10, mu.gam=pred.gam$fit, upper.gam=pred.gam$fit+2*pred.gam$se.fit, lower.gam=pred.gam$fit-2*pred.gam$se.fit, mu.glm=pred.glm$fit, upper.glm=pred.glm$fit+2*pred.glm$se.fit, lower.glm=pred.glm$fit-2*pred.glm$se.fit)
prev.find(dat=macro.d10$pmac.c21, in_out_dat=macro.d10$in_out, n.grp=20, xlabel="P fail (C21)", ylimit=c(0,0.035))
lines(mu.gam ~ Depth, data=dep.dat, lwd=2, col=2, type="l")
lines(upper.gam ~ Depth, data=dep.dat, lwd=2, col=2, type="l")
lines(lower.gam ~ Depth, data=dep.dat, lwd=2, col=2, type="l")
lines(mu.glm ~ Depth, data=dep.dat, lwd=2, col=1, type="l")
lines(upper.glm ~ Depth, data=dep.dat, lwd=2, col=1, type="l")
lines(lower.glm ~ Depth, data=dep.dat, lwd=2, col=1, type="l")


pred.dat <- data.frame(Depth=5, pmac.c18=mean(macro.d10$pmac.c18), pmac.c21=mean(macro.d10$pmac.c21),Q.c18=Q.c18.d10)
pred.gam <- predict(best.gam.d10, newdata=pred.dat, se=TRUE, type="response")
pred.glm <- predict(best.glm.d10, newdata=pred.dat, se=TRUE, type="response")
dep.dat <- data.frame(Depth=Q.c18.d10, mu.gam=pred.gam$fit, upper.gam=pred.gam$fit+2*pred.gam$se.fit, lower.gam=pred.gam$fit-2*pred.gam$se.fit, mu.glm=pred.glm$fit, upper.glm=pred.glm$fit+2*pred.glm$se.fit, lower.glm=pred.glm$fit-2*pred.glm$se.fit)
prev.find(dat=macro.d10$Q.c18, in_out_dat=macro.d10$in_out, n.grp=20, xlabel="Q (C18)", ylimit=c(0,0.02))
lines(mu.gam ~ Depth, data=dep.dat, lwd=2, col=2, type="l")
lines(upper.gam ~ Depth, data=dep.dat, lwd=2, col=2, type="l")
lines(lower.gam ~ Depth, data=dep.dat, lwd=2, col=2, type="l")
lines(mu.glm ~ Depth, data=dep.dat, lwd=2, col=1, type="l")
lines(upper.glm ~ Depth, data=dep.dat, lwd=2, col=1, type="l")
lines(lower.glm ~ Depth, data=dep.dat, lwd=2, col=1, type="l")





#


head(combi.dat)
setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo")
macdat <- read.csv(file="Macrocystis_actual.csv")


for( i in 1:63){
	xmin <- floor(range(macdat$lonNZ[macdat$shape==i])[1]/10)*10
	xmax <- ceiling(range(macdat$lonNZ[macdat$shape==i])[2]/10)*10

	ymin <- floor(range(macdat$latNZ[macdat$shape==i])[1]/10)*10
	ymax <- ceiling(range(macdat$latNZ[macdat$shape==i])[2]/10)*10

	yvec <- seq(from=ymin, to=ymax, by=2)
	xvec <- seq(from=xmin, to=xmax, by=2)

	grid.dat <- expand.grid(yvec, xvec)
	names(grid.dat) <- c("Y", "X")
	in_out <- array(NA, dim=c(dim(grid.dat)[1]))
	uniqval <- 0
	for(j in 1:dim(grid.dat)[1]){
		uniqval[j] <- paste(grid.dat$X[j], "_", grid.dat$Y[j], sep="")
		in_out[j] <- point.in.polygon(point.x=grid.dat$X[j], point.y=grid.dat$Y[j], pol.x=macdat$lonNZ[macdat$shape==i], pol.y=macdat$latNZ[macdat$shape==i], mode.checked=FALSE)
	}
	
	plot(Y ~ X, data=grid.dat, col=in_out+1, pch=15, cex=2.5, asp=1)
	
	if(i==1){
		uniqlist <- uniqval[in_out==1]
	} else {
		temp <- uniqval[in_out==1]
		uniqlist <- c(uniqlist, temp)
	}
}

mac.pres <- combi.dat[combi.dat$uniqval %in% uniqlist,]
mac.absent <- combi.dat[combi.dat$uniqval %in% uniqlist==FALSE,]
