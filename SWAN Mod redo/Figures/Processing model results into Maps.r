library(shapefiles)


which.rowcol <- function(dat) {
	seq.col <- c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.2,1.4,1.6,1.8,2.0,2.25,2.5,2.75,3)
	if(dat < 0) {
		col.ret <- rgb(0,0,0,1)
	} else {
		max.ind <- max(which(seq.col <= dat))
		col.ret <- rainbow(n=19)[max.ind]
	}
	return(col.ret)
}

setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale/N1")
n1.c21 <- read.csv(file='Nest1_C21.csv')
INDEXCOL <- 1:dim(n1.c21)[1]
col.vec <- tapply (X=n1.c21$Ubot, INDEX=INDEXCOL, FUN=which.rowcol)
png(filename = "N1_C21.png", width = 2000, height = 2000, units = "px", pointsize = 12, bg = "white",  res = 300,type = "cairo-png")
plot(Yp ~ Xp, data=n1.c21, pch=15, col=col.vec, asp=1, cex=0.12)
dev.off()

##################################################################################

setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale/N6")

n6.c18 <- read.csv(file='Nest6_C18.csv')

ele1 <- which(n5.c18$Xp %in% n6.c18$Xp & n5.c18$Yp %in% n6.c18$Yp)
ele2 <- which(n6.c18$Xp %in% n5.c18$Xp & n6.c18$Yp %in% n5.c18$Yp)

overlapn1 <- n5.c18[ele1,]
overlapn2 <- n6.c18[ele2,]

Xboun1 <- range(n5.c18$Xp)
Xboun2 <- range(n6.c18$Xp)
Yboun1 <- range(n5.c18$Yp)
Yboun2 <- range(n6.c18$Yp)

Ubot_replace <- array(NA, dim=c(dim(overlapn1)[1]))

for(i in 1:dim(overlapn1)[1]){
	cat(i, "of ", dim(overlapn1)[1], "\r") 
	flush.console()
	dboun1 <- sqrt(min((overlapn1$Xp[i] - Xboun1[1])^2, (overlapn1$Xp[i] - Xboun1[2])^2) +	min((overlapn1$Yp[i] - Yboun1[1])^2, (overlapn1$Yp[i] - Yboun1[2])^2))
	dboun2 <- sqrt(min((overlapn2$Xp[i] - Xboun2[1])^2, (overlapn2$Xp[i] - Xboun2[2])^2) +	min((overlapn2$Yp[i] - Yboun2[1])^2, (overlapn2$Yp[i] - Yboun2[2])^2))
	
	if(-10 %in% c(overlapn1$Ubot[i], overlapn2$Ubot[i])) {
		Ubot_replace[i] <- -10
	} else {
		Ubot_replace[i] <- ((dboun1*overlapn1$Ubot[i]) + (dboun2*overlapn2$Ubot[i]))/(dboun1+dboun2) 
	}
	
}

n5.c18$Ubot[ele1] <- Ubot_replace
n6.c18$Ubot[ele2] <- Ubot_replace


INDEXCOL <- 1:dim(n5.c18)[1]

col.vec <- tapply (X=n5.c18$Ubot, INDEX=INDEXCOL, FUN=which.rowcol)

png(filename = "N5_C18.png", width = 2000, height = 2000, units = "px", pointsize = 12, bg = "white",  res = 300,type = "cairo-png")
plot(Yp ~ Xp, data=n5.c18, pch=15, col=col.vec, asp=1, cex=0.12)
dev.off()


#############
# assigning substrates

setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale/N14")

n2.c18 <- read.csv(file='Nest14_C18.csv')
n2.subs <- read.csv(file="n14_subs.csv", header=FALSE)

subn2 <- as.matrix(n2.subs)
subs_n2 <- matrix(subn2, ncol = ncol(subn2), dimnames = NULL)
subs.vec <- subs_n2[dim(n2.subs)[1],]

for(i in 2:dim(n2.subs)[1]) {
	add.list <- subs_n2[dim(n2.subs)[1]-(i-1),]
	subs.vec <- c(subs.vec, add.list)
}

col.vec <- array(rainbow(n=3)[1], dim=c(length(subs.vec)))

col.vec[subs.vec > 0.07] <- rainbow(n=3)[1]
col.vec[subs.vec <= 0.07 & subs.vec > 0.03] <- rainbow(n=3)[2]
col.vec[subs.vec <= 0.03] <- rainbow(n=3)[3]

plot(Yp ~ Xp, data=n2.c18, col=col.vec, pch=15, cex=0.12, asp=1)

sub.vec <- array("S", dim=c(length(subs.vec)))
sub.vec[subs.vec > 0.07] <- "Rock"
sub.vec[subs.vec <= 0.07 & subs.vec > 0.03] <- "Rubble"
sub.vec[subs.vec <= 0.03] <- "Sand"

write.csv(sub.vec, file="SUBSTRATE_DELETE.csv")

##########################################################

# Histograms

get.char <- function(dat){
	depth.ret <- dat[1]
	return(depth.ret)
}

setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale/N1")
n1.c18 <- read.csv(file='Nest1_C18.csv',stringsAsFactors=FALSE)
n1.c18 <- n1.c18[n1.c18$Ubot != -10 & n1.c18$Depth <= 25,c(4,8,13,14)]
setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale/N2")
n2.c18 <- read.csv(file='Nest2_C18.csv',stringsAsFactors=FALSE)
n2.c18 <- n2.c18[n2.c18$Ubot != -10 & n2.c18$Depth <= 25,c(4,8,13,14)]
setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale/N3")
n3.c18 <- read.csv(file='Nest3_C18.csv',stringsAsFactors=FALSE)
n3.c18 <- n3.c18[n3.c18$Ubot != -10 & n3.c18$Depth <= 25,c(4,8,13,14)]
setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale/N4")
n4.c18 <- read.csv(file='Nest4_C18.csv',stringsAsFactors=FALSE)
n4.c18 <- n4.c18[n4.c18$Ubot != -10 & n4.c18$Depth <= 25,c(4,8,13,14)]
setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale/N5")
n5.c18 <- read.csv(file='Nest5_C18.csv',stringsAsFactors=FALSE)
n5.c18 <- n5.c18[n5.c18$Ubot != -10 & n5.c18$Depth <= 25,c(4,8,13,14)]
setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale/N6")
n6.c18 <- read.csv(file='Nest6_C18.csv',stringsAsFactors=FALSE)
n6.c18 <- n6.c18[n6.c18$Ubot != -10 & n6.c18$Depth <= 25,c(4,8,13,14)]
setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale/N7")
n7.c18 <- read.csv(file='Nest7_C18.csv',stringsAsFactors=FALSE)
n7.c18 <- n7.c18[n7.c18$Ubot != -10 & n7.c18$Depth <= 25,c(4,8,13,14)]
setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale/N8")
n8.c18 <- read.csv(file='Nest8_C18.csv',stringsAsFactors=FALSE)
n8.c18 <- n8.c18[n8.c18$Ubot != -10 & n8.c18$Depth <= 25,c(4,8,13,14)]
setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale/N9")
n9.c18 <- read.csv(file='Nest9_C18.csv',stringsAsFactors=FALSE)
n9.c18 <- n9.c18[n9.c18$Ubot != -10 & n9.c18$Depth <= 25,c(4,8,13,14)]
setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale/N10")
n10.c18 <- read.csv(file='Nest10_C18.csv',stringsAsFactors=FALSE)
n10.c18 <- n10.c18[n10.c18$Ubot != -10 & n10.c18$Depth <= 25,c(4,8,13,14)]
setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale/N11")
n11.c18 <- read.csv(file='Nest11_C18.csv',stringsAsFactors=FALSE)
n11.c18 <- n11.c18[n11.c18$Ubot != -10 & n11.c18$Depth <= 25,c(4,8,13,14)]
setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale/N12")
n12.c18 <- read.csv(file='Nest12_C18.csv',stringsAsFactors=FALSE)
n12.c18 <- n12.c18[n12.c18$Ubot != -10 & n12.c18$Depth <= 25,c(4,8,13,14)]
setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale/N13")
n13.c18 <- read.csv(file='Nest13_C18.csv',stringsAsFactors=FALSE)
n13.c18 <- n13.c18[n13.c18$Ubot != -10 & n13.c18$Depth <= 25,c(4,8,13,14)]
setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale/N14")
n14.c18 <- read.csv(file='Nest14_C18.csv',stringsAsFactors=FALSE)
n14.c18 <- n14.c18[n14.c18$Ubot != -10 & n14.c18$Depth <= 25,c(4,8,13,14)]

c18.dat <- rbind(n1.c18, n2.c18, n3.c18, n4.c18, n5.c18, n6.c18, n7.c18, n8.c18, n9.c18, n10.c18, n11.c18, n12.c18, n13.c18, n14.c18) 

c18.ubot <- tapply(X=c18.dat$Ubot, INDEX=c18.dat$uniqval, FUN=mean)
c18.depth <- tapply(X=c18.dat$Depth, INDEX=c18.dat$uniqval, FUN=get.char)
c18.substrate <- tapply(X=c18.dat$Substrate, INDEX=c18.dat$uniqval, FUN=get.char)

c18_data <- data.frame(uniq=names(c18.ubot), Ubot=c18.ubot, Depth=c18.depth, Substrate=c18.substrate)

vel.0to5 <- c18_data[c18_data$Depth <= 5,]
vel.5to10 <- c18_data[c18_data$Depth <= 10 & c18_data$Depth > 5,]
vel.10to15 <- c18_data[c18_data$Depth <= 15 & c18_data$Depth > 10,]
vel.15to20 <- c18_data[c18_data$Depth <= 20 & c18_data$Depth > 15,]


par(mfrow=c(1,2))
plot(-10,ylim=c(0,14), xlim=c(0,2.3), ylab="Frequency", xlab=expression("Orbital Speed - m.s" ^-1))
lines(density(vel.0to5$Ubot), col=1, lwd=2)
polygon(x=density(vel.0to5$Ubot)$x, y=density(vel.0to5$Ubot)$y, col=rgb(0,0,0,0.15))
lines(density(vel.0to5$Ubot[vel.0to5$Substrate=="Rock"]), col=1, lwd=2, lty=1)

lines(density(vel.5to10$Ubot), col=2, lwd=2)
polygon(x=density(vel.5to10$Ubot)$x, y=density(vel.5to10$Ubot)$y, col=rgb(1,0,0,0.15))
lines(density(vel.5to10$Ubot[vel.5to10$Substrate=="Rock"]), col=2, lwd=2, lty=1)

lines(density(vel.10to15$Ubot), col=3, lwd=2)
polygon(x=density(vel.10to15$Ubot)$x, y=density(vel.10to15$Ubot)$y, col=rgb(0,1,0,0.15))
lines(density(vel.10to15$Ubot[vel.10to15$Substrate=="Rock"]), col=3, lwd=2, lty=1)

lines(density(vel.15to20$Ubot), col=4, lwd=2)
polygon(x=density(vel.15to20$Ubot)$x, y=density(vel.15to20$Ubot)$y, col=rgb(0,0,1,0.15))
lines(density(vel.15to20$Ubot[vel.15to20$Substrate=="Rock"]), col=4, lwd=2, lty=1)

# C21


setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale/N1")
n1.c21 <- read.csv(file='Nest1_C21.csv',stringsAsFactors=FALSE)
n1.c21 <- n1.c21[n1.c21$Ubot != -10 & n1.c21$Depth <= 20,c(4,8,13,14)]
setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale/N2")
n2.c21 <- read.csv(file='Nest2_C21.csv',stringsAsFactors=FALSE)
n2.c21 <- n2.c21[n2.c21$Ubot != -10 & n2.c21$Depth <= 20,c(4,8,13,14)]
setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale/N3")
n3.c21 <- read.csv(file='Nest3_C21.csv',stringsAsFactors=FALSE)
n3.c21 <- n3.c21[n3.c21$Ubot != -10 & n3.c21$Depth <= 20,c(4,8,13,14)]
setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale/N4")
n4.c21 <- read.csv(file='Nest4_C21.csv',stringsAsFactors=FALSE)
n4.c21 <- n4.c21[n4.c21$Ubot != -10 & n4.c21$Depth <= 20,c(4,8,13,14)]
setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale/N5")
n5.c21 <- read.csv(file='Nest5_C21.csv',stringsAsFactors=FALSE)
n5.c21 <- n5.c21[n5.c21$Ubot != -10 & n5.c21$Depth <= 20,c(4,8,13,14)]
setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale/N6")
n6.c21 <- read.csv(file='Nest6_C21.csv',stringsAsFactors=FALSE)
n6.c21 <- n6.c21[n6.c21$Ubot != -10 & n6.c21$Depth <= 20,c(4,8,13,14)]
setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale/N7")
n7.c21 <- read.csv(file='Nest7_C21.csv',stringsAsFactors=FALSE)
n7.c21 <- n7.c21[n7.c21$Ubot != -10 & n7.c21$Depth <= 20,c(4,8,13,14)]
setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale/N8")
n8.c21 <- read.csv(file='Nest8_C21.csv',stringsAsFactors=FALSE)
n8.c21 <- n8.c21[n8.c21$Ubot != -10 & n8.c21$Depth <= 20,c(4,8,13,14)]
setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale/N9")
n9.c21 <- read.csv(file='Nest9_C21.csv',stringsAsFactors=FALSE)
n9.c21 <- n9.c21[n9.c21$Ubot != -10 & n9.c21$Depth <= 20,c(4,8,13,14)]
setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale/N10")
n10.c21 <- read.csv(file='Nest10_C21.csv',stringsAsFactors=FALSE)
n10.c21 <- n10.c21[n10.c21$Ubot != -10 & n10.c21$Depth <= 20,c(4,8,13,14)]
setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale/N11")
n11.c21 <- read.csv(file='Nest11_C21.csv',stringsAsFactors=FALSE)
n11.c21 <- n11.c21[n11.c21$Ubot != -10 & n11.c21$Depth <= 20,c(4,8,13,14)]
setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale/N12")
n12.c21 <- read.csv(file='Nest12_C21.csv',stringsAsFactors=FALSE)
n12.c21 <- n12.c21[n12.c21$Ubot != -10 & n12.c21$Depth <= 20,c(4,8,13,14)]
setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale/N13")
n13.c21 <- read.csv(file='Nest13_C21.csv',stringsAsFactors=FALSE)
n13.c21 <- n13.c21[n13.c21$Ubot != -10 & n13.c21$Depth <= 20,c(4,8,13,14)]
setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale/N14")
n14.c21 <- read.csv(file='Nest14_C21.csv',stringsAsFactors=FALSE)
n14.c21 <- n14.c21[n14.c21$Ubot != -10 & n14.c21$Depth <= 20,c(4,8,13,14)]

c21.dat <- rbind(n1.c21, n2.c21, n3.c21, n4.c21, n5.c21, n6.c21, n7.c21, n8.c21, n9.c21, n10.c21, n11.c21, n12.c21, n13.c21, n14.c21) 

c21.ubot <- tapply(X=c21.dat$Ubot, INDEX=c21.dat$uniqval, FUN=mean)
c21.depth <- tapply(X=c21.dat$Depth, INDEX=c21.dat$uniqval, FUN=get.char)
c21.substrate <- tapply(X=c21.dat$Substrate, INDEX=c21.dat$uniqval, FUN=get.char)

c21_data <- data.frame(uniq=names(c21.ubot), Ubot=c21.ubot, Depth=c21.depth, Substrate=c21.substrate)

vel.0to5_c21 <- c21_data[c21_data$Depth <= 5,]
vel.5to10_c21 <- c21_data[c21_data$Depth <= 10 & c21_data$Depth > 5,]
vel.10to15_c21 <- c21_data[c21_data$Depth <= 15 & c21_data$Depth > 10,]
vel.15to20_c21 <- c21_data[c21_data$Depth <= 20 & c21_data$Depth > 15,]



plot(-10,ylim=c(0,6), xlim=c(0,3), ylab="Frequency", xlab=expression("Orbital Speed - m.s" ^-1))
lines(density(vel.0to5_c21$Ubot), col=1, lwd=2)
polygon(x=density(vel.0to5_c21$Ubot)$x, y=density(vel.0to5_c21$Ubot)$y, col=rgb(0,0,0,0.15))
lines(density(vel.0to5_c21$Ubot[vel.0to5_c21$Substrate=="Rock"]), col=1, lwd=2, lty=1)

lines(density(vel.5to10_c21$Ubot), col=2, lwd=2)
polygon(x=density(vel.5to10_c21$Ubot)$x, y=density(vel.5to10_c21$Ubot)$y, col=rgb(1,0,0,0.15))
lines(density(vel.5to10_c21$Ubot[vel.5to10_c21$Substrate=="Rock"]), col=2, lwd=2, lty=1)

lines(density(vel.10to15_c21$Ubot), col=3, lwd=2)
polygon(x=density(vel.10to15_c21$Ubot)$x, y=density(vel.10to15_c21$Ubot)$y, col=rgb(0,1,0,0.15))
lines(density(vel.10to15_c21$Ubot[vel.10to15_c21$Substrate=="Rock"]), col=3, lwd=2, lty=1)

lines(density(vel.15to20_c21$Ubot), col=4, lwd=2)
polygon(x=density(vel.15to20_c21$Ubot)$x, y=density(vel.15to20_c21$Ubot)$y, col=rgb(0,0,1,0.15))
lines(density(vel.15to20_c21$Ubot[vel.15to20_c21$Substrate=="Rock"]), col=4, lwd=2, lty=1)


profinder <- function(dat, subs.dat){
	denom <- length(dat)
	class1 <- length(dat[dat < 0.5])/denom
	class2 <- length(dat[dat >= 0.5 & dat < 1])/denom
	class3 <- length(dat[dat >=1 & dat < 1.5])/denom
	class4 <- length(dat[dat >= 1.5 & dat < 2])/denom
	class5 <- length(dat[dat >= 2 & dat < 2.5])/denom
	class6 <- length(dat[dat >= 2.5 & dat < 3])/denom
	class7 <- max(dat)

	dat2 <- dat[subs.dat=="Rock"]
	denom2 <- length(dat2)
	class21 <- length(dat2[dat2 < 0.5])/denom2
	class22 <- length(dat2[dat2 >= 0.5 & dat2 < 1])/denom2
	class23 <- length(dat2[dat2 >=1 & dat2 < 1.5])/denom2
	class24 <- length(dat2[dat2 >= 1.5 & dat2 < 2])/denom2
	class25 <- length(dat2[dat2 >= 2 & dat2 < 2.5])/denom2
	class26 <- length(dat2[dat2 >= 2.5 & dat2 < 3])/denom2
	class27 <- max(dat2)
	
	xc1 <- c(class1, class2, class3, class4, class5, class6, class7)
	xc2 <- c(class21, class22, class23, class24, class25, class26, class27)
	
	ret.dat <- rbind(xc1, xc2)
	return(ret.dat)

}


profinder(dat=vel.0to5_c21$Ubot, subs.dat=vel.0to5_c21$Substrate)

profinder2 <- function (dat, subs.dat) {
	
	denom <- length(dat)
	class1 <- median(dat)
	class2 <- dat[rank(dat, ties.method="random")== floor(0.1*denom)]
	class3 <- dat[rank(dat, ties.method="random")== floor(0.9*denom)]
	class4 <- dat[rank(dat, ties.method="random")== floor(0.25*denom)]
	class5 <- dat[rank(dat, ties.method="random")== floor(0.75*denom)]
	class6 <- dat[rank(dat, ties.method="random")== floor(0.05*denom)]
	class7 <- dat[rank(dat, ties.method="random")== floor(0.95*denom)]
	
	dat2 <- dat[subs.dat=="Rock"]
	denom2 <- length(dat2)
	class21 <- median(dat2)
	class22 <- dat2[rank(dat2, ties.method="random")== floor(0.1*denom2)]
	class23 <- dat2[rank(dat2, ties.method="random")== floor(0.9*denom2)]
	class24 <- dat2[rank(dat2, ties.method="random")== floor(0.25*denom2)]
	class25 <- dat2[rank(dat2, ties.method="random")== floor(0.75*denom2)]
	class26 <- dat2[rank(dat2, ties.method="random")== floor(0.05*denom2)]
	class27 <- dat2[rank(dat2, ties.method="random")== floor(0.95*denom2)]

	xc1 <- c(class1, class2, class3, class4, class5, class6, class7)
	xc2 <- c(class21, class22, class23, class24, class25, class26, class27)
	
	ret.dat <- rbind(xc1, xc2)
	return(ret.dat)
}

profinder2(dat=vel.0to5_c21$Ubot, subs.dat=vel.0to5_c21$Substrate)




##########################################################
# Macrocystis

which.rowcol <- function(dat) {
	seq.col <- c(0,0.005,0.01,0.025,0.05,0.075,0.1,0.15,0.2,0.3,0.4,0.6,0.8,1)
	if(dat < 0) {
		col.ret <- rgb(0,0,0,1)
	} else {
		max.ind <- max(which(seq.col <= dat))
		col.ret <- rainbow(n=13)[max.ind]
	}
	return(col.ret)
}

setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo")
rkmod <- read.csv(file="Outputs_RKmodel.csv")
macact <- read.csv(file="Macrocystis_actual.csv")

####
###
setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale/N13")
n13.c18 <- read.csv(file='Nest13_C18.csv',stringsAsFactors=FALSE)
yran <- range(n13.c18$Yp)
xran <- range(n13.c18$Xp)
n13.shore <- n13.c18[n13.c18$Ubot == -10,c(1,2)]
n13.c18 <- n13.c18[n13.c18$Ubot != -10 & n13.c18$Depth > 3 & n13.c18$Depth < 25,]
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

###
INDEXCOL <- 1:dim(n13.c18)[1]
col.vec <- tapply (X=n13.c18$pmacbreak, INDEX=INDEXCOL, FUN=which.rowcol)
png(filename = "N13_C18_mac.png", width = 2000, height = 2000, units = "px", pointsize = 12, bg = "white",  res = 300,type = "cairo-png")
plot(Yp ~ Xp, data=n13.c18, pch=15, col=col.vec, asp=1, cex=0.12, ylim=yran, xlim=xran)
lines(Yp ~ Xp, data=n13.shore, pch=15, col="grey60", cex=0.12, type="p")
for(i in 1:63) {
	polygon(x=macact$lonNZ[macact$shape==i], y=macact$latNZ[macact$shape==i], border=1, lwd=2, col=NA)
}
YCV <- c(5420550,5421800,5421800,5421000,5419650,5421600)
XCV <- c(316000,315100,316400,317100,317100,316500)
lines(YCV ~ XCV, pch=3, cex=1, col=1, type="p")
dev.off()

#########################################################
#########################################################
#########################################################

setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale/N14")
n14.c18 <- read.csv(file='Nest14_C18.csv',stringsAsFactors=FALSE)
yran <- range(n14.c18$Yp)
xran <- range(n14.c18$Xp)
n14.shore <- n14.c18[n14.c18$Ubot == -10,c(1,2)]
n14.c18 <- n14.c18[n14.c18$Ubot != -10 & n14.c18$Depth > 3 & n14.c18$Depth < 25,]
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

###
INDEXCOL <- 1:dim(n14.c18)[1]
col.vec <- tapply (X=n14.c18$pmacbreak, INDEX=INDEXCOL, FUN=which.rowcol)
png(filename = "N14_C18_mac.png", width = 2000, height = 2000, units = "px", pointsize = 12, bg = "white",  res = 300,type = "cairo-png")
plot(Yp ~ Xp, data=n14.c18, pch=15, col=col.vec, asp=1, cex=0.12, ylim=yran, xlim=xran)
lines(Yp ~ Xp, data=n14.shore, pch=15, col="grey60", cex=0.12, type="p")
for(i in 1:63) {
	polygon(x=macact$lonNZ[macact$shape==i], y=macact$latNZ[macact$shape==i], border=1, lwd=2, col=NA)
}
YCV <- c(5420550,5421800,5421800,5421000,5419650,5421600)
XCV <- c(316000,315100,316400,317100,317100,316500)
lines(YCV ~ XCV, pch=3, cex=1, col=1, type="p")
dev.off()

##################################################
# C 21

setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale/N1")
n1.c21 <- read.csv(file='Nest1_C21.csv',stringsAsFactors=FALSE)
yran <- range(n1.c21$Yp)
xran <- range(n1.c21$Xp)
n1.shore <- n1.c21[n1.c21$Ubot == -10,c(1,2)]
n1.c21 <- n1.c21[n1.c21$Ubot != -10 & n1.c21$Depth > 3 & n1.c21$Depth < 25,]
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

###
INDEXCOL <- 1:dim(n1.c21)[1]
col.vec <- tapply (X=n1.c21$pmacbreak, INDEX=INDEXCOL, FUN=which.rowcol)
png(filename = "N1_C21_mac.png", width = 2000, height = 2000, units = "px", pointsize = 12, bg = "white",  res = 300,type = "cairo-png")
plot(Yp ~ Xp, data=n1.c21, pch=15, col=col.vec, asp=1, cex=0.12, ylim=yran, xlim=xran)
lines(Yp ~ Xp, data=n1.shore, pch=15, col="grey60", cex=0.12, type="p")
for(i in 1:63) {
	polygon(x=macact$lonNZ[macact$shape==i], y=macact$latNZ[macact$shape==i], border=1, lwd=2, col=NA)
}
YCV <- c(5419000,5418600,5419360,5419360,5420200,5420200,5420600,5420600,5420550,5421800,5421800,5421000,5419650,5421600)
XCV <- c(309080,309080,309040,310460,311220,312640,314020,315150,316000,315100,316400,317100,317100,316500)
lines(YCV ~ XCV, pch=3, cex=1, col=1, type="p")
dev.off()
###################

setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale/N2")
n2.c21 <- read.csv(file='Nest2_C21.csv',stringsAsFactors=FALSE)
yran <- range(n2.c21$Yp)
xran <- range(n2.c21$Xp)
n2.shore <- n2.c21[n2.c21$Ubot == -10,c(1,2)]
n2.c21 <- n2.c21[n2.c21$Ubot != -10 & n2.c21$Depth > 3 & n2.c21$Depth < 25,]
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

###
INDEXCOL <- 1:dim(n2.c21)[1]
col.vec <- tapply (X=n2.c21$pmacbreak, INDEX=INDEXCOL, FUN=which.rowcol)
png(filename = "N2_C21_mac.png", width = 2000, height = 2000, units = "px", pointsize = 12, bg = "white",  res = 300,type = "cairo-png")
plot(Yp ~ Xp, data=n2.c21, pch=15, col=col.vec, asp=1, cex=0.12, ylim=yran, xlim=xran)
lines(Yp ~ Xp, data=n2.shore, pch=15, col="grey60", cex=0.12, type="p")
for(i in 1:63) {
	polygon(x=macact$lonNZ[macact$shape==i], y=macact$latNZ[macact$shape==i], border=1, lwd=2, col=NA)
}
YCV <- c(5419000,5418600,5419360,5419360,5420200,5420200,5420600,5420600,5420550,5421800,5421800,5421000,5419650,5421600)
XCV <- c(309080,309080,309040,310460,311220,312640,314020,315150,316000,315100,316400,317100,317100,316500)
lines(YCV ~ XCV, pch=3, cex=1, col=1, type="p")
dev.off()
###################

setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale/N3")
n3.c21 <- read.csv(file='Nest3_C21.csv',stringsAsFactors=FALSE)
yran <- range(n3.c21$Yp)
xran <- range(n3.c21$Xp)
n3.shore <- n3.c21[n3.c21$Ubot == -10,c(1,2)]
n3.c21 <- n3.c21[n3.c21$Ubot != -10 & n3.c21$Depth > 3 & n3.c21$Depth < 25,]
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

###
INDEXCOL <- 1:dim(n3.c21)[1]
col.vec <- tapply (X=n3.c21$pmacbreak, INDEX=INDEXCOL, FUN=which.rowcol)
png(filename = "N3_C21_mac.png", width = 2000, height = 2000, units = "px", pointsize = 12, bg = "white",  res = 300,type = "cairo-png")
plot(Yp ~ Xp, data=n3.c21, pch=15, col=col.vec, asp=1, cex=0.12, ylim=yran, xlim=xran)
lines(Yp ~ Xp, data=n3.shore, pch=15, col="grey60", cex=0.12, type="p")
for(i in 1:63) {
	polygon(x=macact$lonNZ[macact$shape==i], y=macact$latNZ[macact$shape==i], border=1, lwd=2, col=NA)
}
YCV <- c(5419000,5418600,5419360,5419360,5420200,5420200,5420600,5420600,5420550,5421800,5421800,5421000,5419650,5421600)
XCV <- c(309080,309080,309040,310460,311220,312640,314020,315150,316000,315100,316400,317100,317100,316500)
lines(YCV ~ XCV, pch=3, cex=1, col=1, type="p")
dev.off()
###################

setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale/N4")
n4.c21 <- read.csv(file='Nest4_C21.csv',stringsAsFactors=FALSE)
yran <- range(n4.c21$Yp)
xran <- range(n4.c21$Xp)
n4.shore <- n4.c21[n4.c21$Ubot == -10,c(1,2)]
n4.c21 <- n4.c21[n4.c21$Ubot != -10 & n4.c21$Depth > 3 & n4.c21$Depth < 25,]
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

###
INDEXCOL <- 1:dim(n4.c21)[1]
col.vec <- tapply (X=n4.c21$pmacbreak, INDEX=INDEXCOL, FUN=which.rowcol)
png(filename = "N4_C21_mac.png", width = 2000, height = 2000, units = "px", pointsize = 12, bg = "white",  res = 300,type = "cairo-png")
plot(Yp ~ Xp, data=n4.c21, pch=15, col=col.vec, asp=1, cex=0.12, ylim=yran, xlim=xran)
lines(Yp ~ Xp, data=n4.shore, pch=15, col="grey60", cex=0.12, type="p")
for(i in 1:63) {
	polygon(x=macact$lonNZ[macact$shape==i], y=macact$latNZ[macact$shape==i], border=1, lwd=2, col=NA)
}
YCV <- c(5419000,5418600,5419360,5419360,5420200,5420200,5420600,5420600,5420550,5421800,5421800,5421000,5419650,5421600)
XCV <- c(309080,309080,309040,310460,311220,312640,314020,315150,316000,315100,316400,317100,317100,316500)
lines(YCV ~ XCV, pch=3, cex=1, col=1, type="p")
dev.off()
###################

setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale/N5")
n5.c21 <- read.csv(file='Nest5_C21.csv',stringsAsFactors=FALSE)
yran <- range(n5.c21$Yp)
xran <- range(n5.c21$Xp)
n5.shore <- n5.c21[n5.c21$Ubot == -10,c(1,2)]
n5.c21 <- n5.c21[n5.c21$Ubot != -10 & n5.c21$Depth > 3 & n5.c21$Depth < 25,]
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

###
INDEXCOL <- 1:dim(n5.c21)[1]
col.vec <- tapply (X=n5.c21$pmacbreak, INDEX=INDEXCOL, FUN=which.rowcol)
png(filename = "N5_C21_mac.png", width = 2000, height = 2000, units = "px", pointsize = 12, bg = "white",  res = 300,type = "cairo-png")
plot(Yp ~ Xp, data=n5.c21, pch=15, col=col.vec, asp=1, cex=0.12, ylim=yran, xlim=xran)
lines(Yp ~ Xp, data=n5.shore, pch=15, col="grey60", cex=0.12, type="p")
for(i in 1:63) {
	polygon(x=macact$lonNZ[macact$shape==i], y=macact$latNZ[macact$shape==i], border=1, lwd=2, col=NA)
}
YCV <- c(5419000,5418600,5419360,5419360,5420200,5420200,5420600,5420600,5420550,5421800,5421800,5421000,5419650,5421600)
XCV <- c(309080,309080,309040,310460,311220,312640,314020,315150,316000,315100,316400,317100,317100,316500)
lines(YCV ~ XCV, pch=3, cex=1, col=1, type="p")
dev.off()
###################

setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale/N6")
n6.c21 <- read.csv(file='Nest6_C21.csv',stringsAsFactors=FALSE)
yran <- range(n6.c21$Yp)
xran <- range(n6.c21$Xp)
n6.shore <- n6.c21[n6.c21$Ubot == -10,c(1,2)]
n6.c21 <- n6.c21[n6.c21$Ubot != -10 & n6.c21$Depth > 3 & n6.c21$Depth < 25,]
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

###
INDEXCOL <- 1:dim(n6.c21)[1]
col.vec <- tapply (X=n6.c21$pmacbreak, INDEX=INDEXCOL, FUN=which.rowcol)
png(filename = "N6_C21_mac.png", width = 2000, height = 2000, units = "px", pointsize = 12, bg = "white",  res = 300,type = "cairo-png")
plot(Yp ~ Xp, data=n6.c21, pch=15, col=col.vec, asp=1, cex=0.12, ylim=yran, xlim=xran)
lines(Yp ~ Xp, data=n6.shore, pch=15, col="grey60", cex=0.12, type="p")
for(i in 1:63) {
	polygon(x=macact$lonNZ[macact$shape==i], y=macact$latNZ[macact$shape==i], border=1, lwd=2, col=NA)
}
YCV <- c(5419000,5418600,5419360,5419360,5420200,5420200,5420600,5420600,5420550,5421800,5421800,5421000,5419650,5421600)
XCV <- c(309080,309080,309040,310460,311220,312640,314020,315150,316000,315100,316400,317100,317100,316500)
lines(YCV ~ XCV, pch=3, cex=1, col=1, type="p")
dev.off()
###################

setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale/N7")
n7.c21 <- read.csv(file='Nest7_C21.csv',stringsAsFactors=FALSE)
yran <- range(n7.c21$Yp)
xran <- range(n7.c21$Xp)
n7.shore <- n7.c21[n7.c21$Ubot == -10,c(1,2)]
n7.c21 <- n7.c21[n7.c21$Ubot != -10 & n7.c21$Depth > 3 & n7.c21$Depth < 25,]
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

###
INDEXCOL <- 1:dim(n7.c21)[1]
col.vec <- tapply (X=n7.c21$pmacbreak, INDEX=INDEXCOL, FUN=which.rowcol)
png(filename = "N7_C21_mac.png", width = 2000, height = 2000, units = "px", pointsize = 12, bg = "white",  res = 300,type = "cairo-png")
plot(Yp ~ Xp, data=n7.c21, pch=15, col=col.vec, asp=1, cex=0.12, ylim=yran, xlim=xran)
lines(Yp ~ Xp, data=n7.shore, pch=15, col="grey60", cex=0.12, type="p")
for(i in 1:63) {
	polygon(x=macact$lonNZ[macact$shape==i], y=macact$latNZ[macact$shape==i], border=1, lwd=2, col=NA)
}
YCV <- c(5419000,5418600,5419360,5419360,5420200,5420200,5420600,5420600,5420550,5421800,5421800,5421000,5419650,5421600)
XCV <- c(309080,309080,309040,310460,311220,312640,314020,315150,316000,315100,316400,317100,317100,316500)
lines(YCV ~ XCV, pch=3, cex=1, col=1, type="p")
dev.off()
###################

setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale/N8")
n8.c21 <- read.csv(file='Nest8_C21.csv',stringsAsFactors=FALSE)
yran <- range(n8.c21$Yp)
xran <- range(n8.c21$Xp)
n8.shore <- n8.c21[n8.c21$Ubot == -10,c(1,2)]
n8.c21 <- n8.c21[n8.c21$Ubot != -10 & n8.c21$Depth > 3 & n8.c21$Depth < 25,]
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

###
INDEXCOL <- 1:dim(n8.c21)[1]
col.vec <- tapply (X=n8.c21$pmacbreak, INDEX=INDEXCOL, FUN=which.rowcol)
png(filename = "N8_C21_mac.png", width = 2000, height = 2000, units = "px", pointsize = 12, bg = "white",  res = 300,type = "cairo-png")
plot(Yp ~ Xp, data=n8.c21, pch=15, col=col.vec, asp=1, cex=0.12, ylim=yran, xlim=xran)
lines(Yp ~ Xp, data=n8.shore, pch=15, col="grey60", cex=0.12, type="p")
for(i in 1:63) {
	polygon(x=macact$lonNZ[macact$shape==i], y=macact$latNZ[macact$shape==i], border=1, lwd=2, col=NA)
}
YCV <- c(5419000,5418600,5419360,5419360,5420200,5420200,5420600,5420600,5420550,5421800,5421800,5421000,5419650,5421600)
XCV <- c(309080,309080,309040,310460,311220,312640,314020,315150,316000,315100,316400,317100,317100,316500)
lines(YCV ~ XCV, pch=3, cex=1, col=1, type="p")
dev.off()
###################

setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale/N9")
n9.c21 <- read.csv(file='Nest9_C21.csv',stringsAsFactors=FALSE)
yran <- range(n9.c21$Yp)
xran <- range(n9.c21$Xp)
n9.shore <- n9.c21[n9.c21$Ubot == -10,c(1,2)]
n9.c21 <- n9.c21[n9.c21$Ubot != -10 & n9.c21$Depth > 3 & n9.c21$Depth < 25,]
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

###
INDEXCOL <- 1:dim(n9.c21)[1]
col.vec <- tapply (X=n9.c21$pmacbreak, INDEX=INDEXCOL, FUN=which.rowcol)
png(filename = "N9_C21_mac.png", width = 2000, height = 2000, units = "px", pointsize = 12, bg = "white",  res = 300,type = "cairo-png")
plot(Yp ~ Xp, data=n9.c21, pch=15, col=col.vec, asp=1, cex=0.12, ylim=yran, xlim=xran)
lines(Yp ~ Xp, data=n9.shore, pch=15, col="grey60", cex=0.12, type="p")
for(i in 1:63) {
	polygon(x=macact$lonNZ[macact$shape==i], y=macact$latNZ[macact$shape==i], border=1, lwd=2, col=NA)
}
YCV <- c(5419000,5418600,5419360,5419360,5420200,5420200,5420600,5420600,5420550,5421800,5421800,5421000,5419650,5421600)
XCV <- c(309080,309080,309040,310460,311220,312640,314020,315150,316000,315100,316400,317100,317100,316500)
lines(YCV ~ XCV, pch=3, cex=1, col=1, type="p")
dev.off()
###################

setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale/N10")
n10.c21 <- read.csv(file='Nest10_C21.csv',stringsAsFactors=FALSE)
yran <- range(n10.c21$Yp)
xran <- range(n10.c21$Xp)
n10.shore <- n10.c21[n10.c21$Ubot == -10,c(1,2)]
n10.c21 <- n10.c21[n10.c21$Ubot != -10 & n10.c21$Depth > 3 & n10.c21$Depth < 25,]
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

###
INDEXCOL <- 1:dim(n10.c21)[1]
col.vec <- tapply (X=n10.c21$pmacbreak, INDEX=INDEXCOL, FUN=which.rowcol)
png(filename = "N10_C21_mac.png", width = 2000, height = 2000, units = "px", pointsize = 12, bg = "white",  res = 300,type = "cairo-png")
plot(Yp ~ Xp, data=n10.c21, pch=15, col=col.vec, asp=1, cex=0.12, ylim=yran, xlim=xran)
lines(Yp ~ Xp, data=n10.shore, pch=15, col="grey60", cex=0.12, type="p")
for(i in 1:63) {
	polygon(x=macact$lonNZ[macact$shape==i], y=macact$latNZ[macact$shape==i], border=1, lwd=2, col=NA)
}
YCV <- c(5419000,5418600,5419360,5419360,5420200,5420200,5420600,5420600,5420550,5421800,5421800,5421000,5419650,5421600)
XCV <- c(309080,309080,309040,310460,311220,312640,314020,315150,316000,315100,316400,317100,317100,316500)
lines(YCV ~ XCV, pch=3, cex=1, col=1, type="p")
dev.off()
###################
setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale/N11")
n11.c21 <- read.csv(file='Nest11_C21.csv',stringsAsFactors=FALSE)
yran <- range(n11.c21$Yp)
xran <- range(n11.c21$Xp)
n11.shore <- n11.c21[n11.c21$Ubot == -10,c(1,2)]
n11.c21 <- n11.c21[n11.c21$Ubot != -10 & n11.c21$Depth > 3 & n11.c21$Depth < 25,]
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

###
INDEXCOL <- 1:dim(n11.c21)[1]
col.vec <- tapply (X=n11.c21$pmacbreak, INDEX=INDEXCOL, FUN=which.rowcol)
png(filename = "N11_C21_mac.png", width = 2000, height = 2000, units = "px", pointsize = 12, bg = "white",  res = 300,type = "cairo-png")
plot(Yp ~ Xp, data=n11.c21, pch=15, col=col.vec, asp=1, cex=0.12, ylim=yran, xlim=xran)
lines(Yp ~ Xp, data=n11.shore, pch=15, col="grey60", cex=0.12, type="p")
for(i in 1:63) {
	polygon(x=macact$lonNZ[macact$shape==i], y=macact$latNZ[macact$shape==i], border=1, lwd=2, col=NA)
}
YCV <- c(5419000,5418600,5419360,5419360,5420200,5420200,5420600,5420600,5420550,5421800,5421800,5421000,5419650,5421600)
XCV <- c(309080,309080,309040,310460,311220,312640,314020,315150,316000,315100,316400,317100,317100,316500)
lines(YCV ~ XCV, pch=3, cex=1, col=1, type="p")
dev.off()
###################

setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale/N12")
n12.c21 <- read.csv(file='Nest12_C21.csv',stringsAsFactors=FALSE)
yran <- range(n12.c21$Yp)
xran <- range(n12.c21$Xp)
n12.shore <- n12.c21[n12.c21$Ubot == -10,c(1,2)]
n12.c21 <- n12.c21[n12.c21$Ubot != -10 & n12.c21$Depth > 3 & n12.c21$Depth < 25,]
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

###
INDEXCOL <- 1:dim(n12.c21)[1]
col.vec <- tapply (X=n12.c21$pmacbreak, INDEX=INDEXCOL, FUN=which.rowcol)
png(filename = "N12_C21_mac.png", width = 2000, height = 2000, units = "px", pointsize = 12, bg = "white",  res = 300,type = "cairo-png")
plot(Yp ~ Xp, data=n12.c21, pch=15, col=col.vec, asp=1, cex=0.12, ylim=yran, xlim=xran)
lines(Yp ~ Xp, data=n12.shore, pch=15, col="grey60", cex=0.12, type="p")
for(i in 1:63) {
	polygon(x=macact$lonNZ[macact$shape==i], y=macact$latNZ[macact$shape==i], border=1, lwd=2, col=NA)
}
YCV <- c(5419000,5418600,5419360,5419360,5420200,5420200,5420600,5420600,5420550,5421800,5421800,5421000,5419650,5421600)
XCV <- c(309080,309080,309040,310460,311220,312640,314020,315150,316000,315100,316400,317100,317100,316500)
lines(YCV ~ XCV, pch=3, cex=1, col=1, type="p")
dev.off()
###################

setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale/N13")
n13.c21 <- read.csv(file='Nest13_C21.csv',stringsAsFactors=FALSE)
yran <- range(n13.c21$Yp)
xran <- range(n13.c21$Xp)
n13.shore <- n13.c21[n13.c21$Ubot == -10,c(1,2)]
n13.c21 <- n13.c21[n13.c21$Ubot != -10 & n13.c21$Depth > 3 & n13.c21$Depth < 25,]
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

###
INDEXCOL <- 1:dim(n13.c21)[1]
col.vec <- tapply (X=n13.c21$pmacbreak, INDEX=INDEXCOL, FUN=which.rowcol)
png(filename = "N13_C21_mac.png", width = 2000, height = 2000, units = "px", pointsize = 12, bg = "white",  res = 300,type = "cairo-png")
plot(Yp ~ Xp, data=n13.c21, pch=15, col=col.vec, asp=1, cex=0.12, ylim=yran, xlim=xran)
lines(Yp ~ Xp, data=n13.shore, pch=15, col="grey60", cex=0.12, type="p")
for(i in 1:63) {
	polygon(x=macact$lonNZ[macact$shape==i], y=macact$latNZ[macact$shape==i], border=1, lwd=2, col=NA)
}
YCV <- c(5419000,5418600,5419360,5419360,5420200,5420200,5420600,5420600,5420550,5421800,5421800,5421000,5419650,5421600)
XCV <- c(309080,309080,309040,310460,311220,312640,314020,315150,316000,315100,316400,317100,317100,316500)
lines(YCV ~ XCV, pch=3, cex=1, col=1, type="p")
dev.off()
###################

setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale/N14")
n14.c21 <- read.csv(file='Nest14_C21.csv',stringsAsFactors=FALSE)
yran <- range(n14.c21$Yp)
xran <- range(n14.c21$Xp)
n14.shore <- n14.c21[n14.c21$Ubot == -10,c(1,2)]
n14.c21 <- n14.c21[n14.c21$Ubot != -10 & n14.c21$Depth > 3 & n14.c21$Depth < 25,]
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

###
INDEXCOL <- 1:dim(n14.c21)[1]
col.vec <- tapply (X=n14.c21$pmacbreak, INDEX=INDEXCOL, FUN=which.rowcol)
png(filename = "N14_C21_mac.png", width = 2000, height = 2000, units = "px", pointsize = 12, bg = "white",  res = 300,type = "cairo-png")
plot(Yp ~ Xp, data=n14.c21, pch=15, col=col.vec, asp=1, cex=0.12, ylim=yran, xlim=xran)
lines(Yp ~ Xp, data=n14.shore, pch=15, col="grey60", cex=0.12, type="p")
for(i in 1:63) {
	polygon(x=macact$lonNZ[macact$shape==i], y=macact$latNZ[macact$shape==i], border=1, lwd=2, col=NA)
}
YCV <- c(5419000,5418600,5419360,5419360,5420200,5420200,5420600,5420600,5420550,5421800,5421800,5421000,5419650,5421600)
XCV <- c(309080,309080,309040,310460,311220,312640,314020,315150,316000,315100,316400,317100,317100,316500)
lines(YCV ~ XCV, pch=3, cex=1, col=1, type="p")
dev.off()
###################

setwd("C:/Users/Timothy/Desktop/Work Folder/Wave Model Paper/SWAN Mod redo/Nest Scale/N14")

n1.c21 <- read.csv(file='Nest14_C21.csv',stringsAsFactors=FALSE)
yran <- range(n1.c21$Yp)
xran <- range(n1.c21$Xp)
n1.c21 <- n1.c21[n1.c21$Depth <= 10,]

png(filename = "N14_Depth10.png", width = 2000, height = 2000, units = "px", pointsize = 12, bg = "white",  res = 300,type = "cairo-png")
plot(Yp ~ Xp, data=n1.c21, pch=15, col=1, asp=1, cex=0.12, ylim=yran, xlim=xran)
YCV <- c(5419000,5418600,5419360,5419360,5420200,5420200,5420600,5420600,5420550,5421800,5421800,5421000,5419650,5421600)
XCV <- c(309080,309080,309040,310460,311220,312640,314020,315150,316000,315100,316400,317100,317100,316500)
lines(YCV ~ XCV, pch=3, cex=1, col=2, type="p")
dev.off()


