###################################################################################
# Libraries
library(here)

# This codefile runs the numerical integration of a biomechanical model for
# macrocystis used to examine breaking force under different wave regimes
# as pasrt of a spatial analysis of wave forces given the presence/absence
# patterns of Macrocystis pyrifera along the wellington south coast

mac.rungekutta <- function(t.step, length.sim, mac.length, depth, 
                           sigWaveHeight, Wave.Period, wavelength, obs.period,long.short="L") {
	# This function performs a 4th order numerical integration of the force balance equations describing the 
	# Motion of an idealised macrocystis plant, incorporating Drag, Acceleration and Buoyancy forces
	# To predict the tension acting on the stipe
	
	###################################################
	# Macrocystis parameters
	mac.area <- 0.297*(mac.length^0.955)
	mac.efflength <- 0.8*mac.length
	mac.mass <- 0.774*mac.area
	mac.volume <- (mac.mass + (2.49/9.81))/1025
	mac.effmass <- mac.mass + (1025*3*mac.volume)
	orig.Y <- mac.efflength*(((2.49/(1.91e7*4.1e-5))^(1/1.4068))+1)
	###################################################
	
	###################################################
	# Wave Parameters
	Wave.Height <- 0.6541*sigWaveHeight * (sqrt(log(obs.period+Wave.Period)) + 
	                                         0.2886*(1/sqrt(log(obs.period/Wave.Period))))
	if(Wave.Height > 0.9*depth) {
		Wave.Height <- 0.9*depth
	}
	wavenum <- 2*pi/wavelength
	omega <- 2*pi/Wave.Period
	#################################################
	
	#################################################
	# Drag function
	drag.calc <- function(vel.mac, vel.water, Area) {
		vel.rel <- vel.water-vel.mac
		if(vel.rel < 0){
			multi <- -1
		} else {
			multi <- 1
		}
		vel.calc <- abs(vel.rel)
		drag <- multi*0.5*1025*0.0148*Area*(vel.calc)^1.596
		return(drag)
	}
	#################################################
	
	#################################################
	# Tension Function
	tension.calc <- function(X.pos, Y.pos, L0){
		theta <- atan2(Y.pos,X.pos)
		if(sqrt((X.pos^2)+(Y.pos^2)) > L0){
			tension.mag <- (1.91e7)*(((sqrt((X.pos^2)+(Y.pos^2))-L0)/L0)^1.407)*4.1e-5
		} else {
			tension.mag <- 0
		}
		tension.X <- -tension.mag*cos(theta)
		tension.Y <- -tension.mag*sin(theta)
		tension.Res <- c(tension.X, tension.Y, tension.mag)
		return(tension.Res)
	}
	################################################
	
	################################################
	# Water velocity function
	water.vel.acc <- function(Wave.Height, Wave.Period, depth, X.pos, Y.pos, 
	                          Time, omega, wavelength, wavenum) {
		
		vel.Y <- (pi*Wave.Height/Wave.Period)*sin((wavenum*X.pos)-(omega*Time))*sinh(wavenum*Y.pos)/sinh(wavenum*depth)
		vel.X <- (pi*Wave.Height/Wave.Period)*cos((wavenum*X.pos)-(omega*Time))*cosh(wavenum*Y.pos)/sinh(wavenum*depth)
		
		acc.X <- (2*(pi^2)*Wave.Height/(Wave.Period^2))*sin((wavenum*X.pos)-(omega*Time))*cosh(wavenum*Y.pos)/sinh(wavenum*depth)
		acc.Y <- -(2*(pi^2)*Wave.Height/(Wave.Period^2))*cos((wavenum*X.pos)-(omega*Time))*sinh(wavenum*Y.pos)/sinh(wavenum*depth)
		
		water.out <- c(vel.X, vel.Y, acc.X, acc.Y)
		return(water.out)
	}
	###########################################
	
	##############################################
	# Function describing the force balance in X and Y dimensions
	
	acc.calc <- function(X.pos,Y.pos,Time,vel.X,vel.Y, Wave.Height, Wave.Period,
	                     depth,L0, Area,omega, wavelength, wavenum){
		
		water.res <- water.vel.acc(Wave.Height,Wave.Period,depth, X.pos, Y.pos, Time,
		                           omega, wavelength, wavenum)
		
		tension.res <- tension.calc(X.pos,Y.pos,L0)

		Fdx <- drag.calc(vel.X, water.res[1], Area)
		Fdy <- drag.calc(vel.Y, water.res[2], Area)

		acc.compX <- 4*1025*mac.volume*water.res[3]
		acc.compY <- 4*1025*mac.volume*water.res[4]
		denom <- mac.effmass*(1+(3*1025*mac.volume/mac.effmass))
		B <- 2.49
		
		akx <- (Fdx + acc.compX + tension.res[1])/denom
		aky <- (B+Fdy + acc.compY + tension.res[2])/denom
		
		acc.resu <- c(akx,aky)
		
		return(acc.resu)	
	}
	###########################################################
	
	###########################################################
	# Initialisation of storage vectors
	time.vec <- seq(from=0, to=length.sim, by=t.step)
	posX.vec <- array(NA, dim=c(length(time.vec)))
	posY.vec <- array(NA, dim=c(length(time.vec)))
	velX.vec <- array(NA, dim=c(length(time.vec)))
	velY.vec <- array(NA, dim=c(length(time.vec)))
	accX.vec <- array(NA, dim=c(length(time.vec)))
	accY.vec <- array(NA, dim=c(length(time.vec)))
	watervelX.vec <- array(NA, dim=c(length(time.vec)))
	watervelY.vec <- array(NA, dim=c(length(time.vec)))
	wateraccX.vec <- array(NA, dim=c(length(time.vec)))
	wateraccY.vec <- array(NA, dim=c(length(time.vec)))
	dragX <- array(NA, dim=c(length(time.vec)))
	dragY <- array(NA, dim=c(length(time.vec)))
	tensionX <- array(NA, dim=c(length(time.vec)))
	tensionY <- array(NA, dim=c(length(time.vec)))
	accX <- array(NA, dim=c(length(time.vec)))
	accY <- array(NA, dim=c(length(time.vec)))
	#########################################################
	
	#########################################################
	# Loop performing the iterative runge-kutta numerical integration
	for(i in 1:length(time.vec)){
	
		if(i==1) {
			posX.vec[i] <- 0
			posY.vec[i] <- orig.Y
			velX.vec[i] <- 0
			velY.vec[i] <- 0
			accX.vec[i] <- acc.calc(X.pos=posX.vec[i],Y.pos=posY.vec[i],
			                        Time=time.vec[i],vel.X=velX.vec[i],vel.Y=velY.vec[i], Wave.Height, Wave.Period,depth,L0=mac.efflength, Area=mac.area,omega, wavelength, wavenum)[1]
			accY.vec[i] <- acc.calc(X.pos=posX.vec[i],Y.pos=posY.vec[i],
			                        Time=time.vec[i],vel.X=velX.vec[i],vel.Y=velY.vec[i], Wave.Height, Wave.Period,depth,L0=mac.efflength, Area=mac.area,omega, wavelength, wavenum)[2]
			water.anal <- water.vel.acc(Wave.Height, Wave.Period, depth, X.pos=0, Y.pos=posY.vec[i], Time=time.vec[i],omega, wavelength, wavenum)
			watervelX.vec[i] <- water.anal[1]
			watervelY.vec[i] <- water.anal[2]
			wateraccX.vec[i] <- water.anal[3]
			wateraccY.vec[i] <- water.anal[4]
			
			tension.anal <- tension.calc(X.pos=posX.vec[i], Y.pos=posY.vec[i], L0=mac.efflength)
			tensionX[i] <- tension.anal[1]
			tensionY[i] <- tension.anal[2]
			
			dragX[i] <- drag.calc(vel.mac=velX.vec[i], vel.water=watervelX.vec[i], Area=mac.area)
			dragY[i] <- drag.calc(vel.mac=velY.vec[i], vel.water=watervelY.vec[i], Area=mac.area)
			
			accX[i] <- (3*1025*mac.volume*(wateraccX.vec[i]-accX.vec[i]))+(1025*mac.volume*wateraccX.vec[i])
			accY[i] <- (3*1025*mac.volume*(wateraccY.vec[i]-accY.vec[i]))+(1025*mac.volume*wateraccY.vec[i])
			
		} else {
			
			X1np1 <- posX.vec[i-1]
			Y1np1 <- posY.vec[i-1]
			
			V1np1 <- velX.vec[i-1]
			U1np1 <- velY.vec[i-1]
			
			Acc1 <- acc.calc(X.pos=X1np1,Y.pos=Y1np1,Time=time.vec[i-1],vel.X=V1np1,
			                 vel.Y=U1np1, Wave.Height, Wave.Period,depth,L0=mac.efflength,
			                 Area=mac.area,omega, wavelength, wavenum)
			
			AX1np1 <- Acc1[1]
			AY1np1 <- Acc1[2]
			
			X2np1 <- posX.vec[i-1] + (t.step*V1np1/2)
			Y2np1 <- posY.vec[i-1] + (t.step*U1np1/2)
			
			V2np1 <- velX.vec[i-1] + (t.step*AX1np1/2)
			U2np1 <- velY.vec[i-1] + (t.step*AY1np1/2)
			
			Acc2 <- acc.calc(X.pos=X2np1,Y.pos=Y2np1,Time=time.vec[i-1]+(t.step/2),
			                 vel.X=V2np1,vel.Y=U2np1, Wave.Height, Wave.Period,depth,
			                 L0=mac.efflength, Area=mac.area,omega, wavelength, wavenum)
			
			AX2np1 <- Acc2[1]
			AY2np1 <- Acc2[2]
			
			X3np1 <- posX.vec[i-1] + (t.step*V2np1/2)
			Y3np1 <- posY.vec[i-1] + (t.step*U2np1/2)
			
			V3np1 <- velX.vec[i-1] + (t.step*AX2np1/2)
			U3np1 <- velY.vec[i-1] + (t.step*AY2np1/2)
			
			Acc3 <- acc.calc(X.pos=X3np1,Y.pos=Y3np1,Time=time.vec[i-1]+(t.step/2),
			                 vel.X=V3np1,vel.Y=U3np1, Wave.Height, Wave.Period,depth,
			                 L0=mac.efflength, Area=mac.area,omega, wavelength, wavenum)
			
			AX3np1 <- Acc3[1]
			AY3np1 <- Acc3[2]
			
			X4np1 <- posX.vec[i-1] + (t.step*V3np1)
			Y4np1 <- posY.vec[i-1] + (t.step*U3np1)
			
			V4np1 <- velX.vec[i-1] + (t.step*AX3np1)
			U4np1 <- velY.vec[i-1] + (t.step*AY3np1)
			
			Acc4 <- acc.calc(X.pos=X4np1,Y.pos=Y4np1,Time=time.vec[i-1]+t.step,vel.X=V4np1,vel.Y=U4np1, Wave.Height, Wave.Period,depth,L0=mac.efflength, Area=mac.area,omega, wavelength, wavenum)
			
			AX4np1 <- Acc4[1]
			AY4np1 <- Acc4[2]
			
			posX.vec[i] <- posX.vec[i-1] + ((t.step/6)*(V1np1 + 2*V2np1 + 2*V3np1 + V4np1))
			posY.vec[i] <- posY.vec[i-1] + ((t.step/6)*(U1np1 + 2*U2np1 + 2*U3np1 + U4np1))
			
			velX.vec[i] <- velX.vec[i-1] + ((t.step/6)*(AX1np1 + 2*AX2np1 + 2*AX3np1 + AX4np1))
			velY.vec[i] <- velY.vec[i-1] + ((t.step/6)*(AY1np1 + 2*AY2np1 + 2*AY3np1 + AY4np1))
			
			water.anal <- water.vel.acc(Wave.Height, Wave.Period, depth, X.pos=posX.vec[i], Y.pos=posY.vec[i], Time=time.vec[i],omega, wavelength, wavenum)
			watervelX.vec[i] <- water.anal[1]
			watervelY.vec[i] <- water.anal[2]
			wateraccX.vec[i] <- water.anal[3]
			wateraccY.vec[i] <- water.anal[4]
			
			tension.anal <- tension.calc(X.pos=posX.vec[i], Y.pos=posY.vec[i], L0=mac.efflength)
			tensionX[i] <- tension.anal[1]
			tensionY[i] <- tension.anal[2]
			
			dragX[i] <- drag.calc(vel.mac=velX.vec[i], vel.water=watervelX.vec[i], Area=mac.area)
			dragY[i] <- drag.calc(vel.mac=velY.vec[i], vel.water=watervelY.vec[i], Area=mac.area)
			
			accX.vec[i] <- acc.calc(X.pos=posX.vec[i],Y.pos=posY.vec[i],Time=time.vec[i],vel.X=velX.vec[i],vel.Y=velY.vec[i], Wave.Height, Wave.Period,depth,L0=mac.efflength, Area=mac.area,omega, wavelength, wavenum)[1]
			accY.vec[i] <- acc.calc(X.pos=posX.vec[i],Y.pos=posY.vec[i],Time=time.vec[i],vel.X=velX.vec[i],vel.Y=velY.vec[i], Wave.Height, Wave.Period,depth,L0=mac.efflength, Area=mac.area,omega, wavelength, wavenum)[2]
			
			accX[i] <- (3*1025*mac.volume*(wateraccX.vec[i]-accX.vec[i]))+(1025*mac.volume*wateraccX.vec[i])
			accY[i] <- (3*1025*mac.volume*(wateraccY.vec[i]-accY.vec[i]))+(1025*mac.volume*wateraccY.vec[i])
		
		}
	}
	##########################################################################
	
	##########################################################################
	# Output data
	if(long.short=="L") {
		outdat <- data.frame(time.vec, posX.vec, posY.vec, velX.vec, velY.vec, watervelX.vec, watervelY.vec, wateraccX.vec, wateraccY.vec, tensionX, tensionY, dragX, dragY, accX, accY)
		return(outdat)
	} else {
		
		tension <- sqrt((tensionX^2) + (tensionY^2))
		maxtension <- max(tension[time.vec > 10*Wave.Period])
		stress <- maxtension/4.1e-5
		Pbreak <- 1 - exp(-(((stress-1)/3.16e6)^3.75))
		outdat <- c(maxtension, Pbreak)
		return(outdat)
	}
	
	##########################################################################
}

####################################################################################

####################################################################################
# Run a timing test

test <- mac.rungekutta(t.step=0.01, length.sim=300, mac.length=10, 
                           depth=10, sigWaveHeight=1.8, Wave.Period=7.6, 
                           wavelength=42, obs.period=3*60*60, long.short="L")

# Create some plots 
plot(posY.vec ~ time.vec, data=test, type="l", col=1,
     ylab="Y position", xlab="Time")
plot(posX.vec ~ time.vec, data=test, type="l", col=1,
     ylab="X position", xlab="Time")

plot(posY.vec ~ posX.vec, data=test, type="l", col=1,
     ylab="Y position", xlab="X position", ylim=c(0,10), xlim=c(-10,10), asp=1)

plot(tensionY ~ time.vec, data=test, type="l")

####################################################################################

####################################################################################
# Now run the code for each set of parameters
params <- read.csv(here("data","Simulation_parameters.csv"))

Pbrake <- array(NA, dim=c(dim(params)[1]))
stress <- array(NA, dim=c(dim(params)[1]))

for(i in 1:dim(params)[1]){
	cat(i, "of ", dim(params)[1], "\r") 
	flush.console()
	
	if(params$Hsig[i]==0){
		Pbrake[i] <- 0
		stress[i] <- 0
	} else {
		ode <- mac.rungekutta(t.step=0.02, length.sim=ceiling(20*params$Wper[i]), 
		                      mac.length=params$Depth[i], depth=params$Depth[i], 
		                      sigWaveHeight=params$Hsig[i], Wave.Period=params$Wper[i], 
		                      wavelength=params$Wlen[i], obs.period=6*60*60, long.short="S")
		Pbrake[i] <- ode[2]
		stress[i] <- ode[1]
	}
}

sim.out <- data.frame(params, Pbrake, stress)

####################################################################################

#################################################################################################
# Processing the results
n1.c21 <- read.csv(here::here("data","Nested results","N1_C21_MAC.csv"))
n1.c21 <- n1.c21[n1.c21$Hsig!=-9 & n1.c21$Depth <= 30 & n1.c21$Depth >= 3,]

# variables of interest: 
# depth: 3-15 round to 0.25, 15-30 round to 0.5 () 
# sigWaveHeight: 0-2 round to 0.1, > 2 round to 0.2 ()
# Wave.Period: 0-5 round to 0.1, > 5 round to 0.2 ()
# wavelength: 0-20 round to 0.5, 20-40 round to 1, > 40 round to 2 ()
uniqval <- array("P", dim=c(dim(n1.c21)[1]))
DEP <- array(NA, dim=c(dim(n1.c21)[1]))
SWH <- array(NA, dim=c(dim(n1.c21)[1]))
WPER <- array(NA, dim=c(dim(n1.c21)[1]))
WLEN <- array(NA, dim=c(dim(n1.c21)[1]))

for(i in 1:dim(n1.c21)[1]){
	DEP[i] <- if(n1.c21$Depth[i] <= 15) round(n1.c21$Depth[i]*4,0)/4 else {round(n1.c21$Depth[i]*2,0)/2}
	SWH[i] <- if(n1.c21$Hsig[i] <= 2) round(n1.c21$Hsig[i],1) else {round(n1.c21$Hsig[i]*5,0)/5}
	WPER[i] <- if(n1.c21$Tm_10[i] <=5) round(n1.c21$Tm_10[i],1) else {round(n1.c21$Tm_10[i]*5,0)/5}
	WLEN[i] <- if(n1.c21$Wlen[i] <=20) round(n1.c21$Wlen[i]*2,0)/2 else { if(n1.c21$Wlen[i] <= 40) round(n1.c21$Wlen[i],0) else {round(n1.c21$Wlen[i]/2,0)*2}}
	uniqval[i] <- paste(DEP[i],SWH[i],WPER[i],WLEN[i], sep="_")
}

list.params.c21 <- unique(uniqval)
length(list.params.c21)

# Plotting bottom orbital velocity
colvec <- floor(n1.c21$Ubot*5)+1
col.vec <- rainbow(n=12)[1]
for(i in 1:dim(n1.c21)[1]){
	col.vec[i] <- rev(rainbow(n=12))[colvec[i]]
}
plot(Yp ~ Xp, data=n1.c21, pch=15, col=col.vec, asp=1, cex=0.12,
     ylab="Latitude (NZTM)", xlab="Longitude (NZTM)")

legend("bottomleft",col=rev(rainbow(n=12)), pch=15,
       legend=c("0-20","20-40","40-60","60-80","80-100",
                "100-120","120-140","140-160","160-180","180-200",
                "200-220","220-240","240-260"), 
       title="Orbital Velocity (cm.s)", bty="n")

#################################################################################

#################################################################################
# Plot the results of the simulation

mactest1 <- mac.rungekutta(t.step=0.01, length.sim=300, mac.length=8, depth=10, sigWaveHeight=2, Wave.Period=10, wavelength=42, obs.period=3*60*60, long.short="L")
mactest2 <- mactest1[mactest1$time.vec > 200,]
tension.tot <- sqrt((mactest2$tensionX^2)+ (mactest2$tensionY^2))
mactest2 <- data.frame(mactest2, tension.tot)
macmax <- mactest2[mactest2$tension.tot==max(mactest2$tension.tot),] 

macmin <- mactest2[mactest2$posY.vec==min(mactest2$posY.vec),]

# time.vec posX.vec posY.vec  velX.vec    velY.vec watervelX.vec watervelY.vec wateraccX.vec wateraccY.vec  tensionX tensionY dragX    dragY     accX     accY tension.tot

# Trajectory
plot(0, ylim=c(0,10), xlim=c(-5,5), asp=1, cex=0, ylab="Depth",xlab="X-position (relative to holdfast)")
theta <- seq(from=0, to=2*pi, length.out=500)
xcirc <- 0.8*8*cos(theta)
ycirc <- 0.8*8*sin(theta)
lines(ycirc ~ xcirc, type="l", lty=1, lwd=1)
abline(h=0)
abline(h=10)
lines(posY.vec ~ posX.vec, data= mactest2, type="l", lty=1, lwd=1)
segments(x0=0, y0=0, x1 = macmax$posX.vec, y1 = macmax$posY.vec, col=2)

# velocities
segments(x0=macmax$posX.vec, y0=macmax$posY.vec,x1= macmax$posX.vec + (macmax$velX.vec/0.3), y1= macmax$posY.vec + (macmax$velY.vec/0.3), col=3)
segments(x0=macmax$posX.vec, y0=macmax$posY.vec,x1= macmax$posX.vec + (macmax$watervelX.vec/0.3), y1= macmax$posY.vec + (macmax$watervelY.vec/0.3), col=3)
# forces
segments(x0=macmax$posX.vec, y0=macmax$posY.vec,x1= macmax$posX.vec + (macmax$tensionX/4), y1= macmax$posY.vec + (macmax$tensionY/4), col=4)
segments(x0=macmax$posX.vec, y0=macmax$posY.vec,x1= macmax$posX.vec + (macmax$dragX/4), y1= macmax$posY.vec + (macmax$dragY/4), col=4)
segments(x0=macmax$posX.vec, y0=macmax$posY.vec,x1= macmax$posX.vec + (macmax$accX/4), y1= macmax$posY.vec + (macmax$accY/4), col=4)
segments(x0=macmax$posX.vec, y0=macmax$posY.vec,x1= macmax$posX.vec, y1= macmax$posY.vec + (2.49/4), col=4)

# set 2
segments(x0=0, y0=0, x1 = macmin$posX.vec, y1 = macmin$posY.vec, col=2)

# velocities
segments(x0=macmin$posX.vec, y0=macmin$posY.vec,x1= macmin$posX.vec + (macmin$velX.vec/0.3), y1= macmin$posY.vec + (macmin$velY.vec/0.3), col=3)
segments(x0=macmin$posX.vec, y0=macmin$posY.vec,x1= macmin$posX.vec + (macmin$watervelX.vec/0.3), y1= macmin$posY.vec + (macmin$watervelY.vec/0.3), col=3)
# forces
#segments(x0=macmin$posX.vec, y0=macmin$posY.vec,x1= macmin$posX.vec + (macmin$tensionX/5), y1= macmin$posY.vec + (macmin$tensionY/5), col=4)
segments(x0=macmin$posX.vec, y0=macmin$posY.vec,x1= macmin$posX.vec + (macmin$dragX/4), y1= macmin$posY.vec + (macmin$dragY/4), col=4)
segments(x0=macmin$posX.vec, y0=macmin$posY.vec,x1= macmin$posX.vec + (macmin$accX/4), y1= macmin$posY.vec + (macmin$accY/4), col=4)
segments(x0=macmin$posX.vec, y0=macmin$posY.vec,x1= macmin$posX.vec, y1= macmin$posY.vec + (2.49/4), col=4)

