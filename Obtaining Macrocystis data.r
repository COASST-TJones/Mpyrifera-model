#################################
# This file overlays the macrocystis distribution onto maps of orbital velocity
library(sp)
library(maptools)
library(here)

# Read in data
macdat <- read.csv(here::here("data","Macrocystis_distribution_poly.csv"))

# Read in nested model results for comparison
n14.c18 <- read.csv(here::here("data","Nested results","N14","Nest14_C18.csv"))

plot(Yp ~ Xp, data=n14.c18[n14.c18$Ubot==-10,], col=1, pch=15, cex=0.12, asp=1)

for(i in 1:63) {
	polygon(x=macdat$lonNZ[macdat$shape==i], y=macdat$latNZ[macdat$shape==i], col=2)
}

#########################
