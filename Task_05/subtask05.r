setwd("C:\\Users\\yogur\\Desktop\\Evolution\\Tasks\\Task_05")
results <- read.csv("http://jonsmitchell.com/data/biol112labresults.csv", stringsAsFactors = F)

length(results)
nrow(results)
ncol(results)
colnames(results)
colnames(results)
#[1] "ï..Year"                     
#[2] "Group.Number"                
#[3] "Habitat.Color"               
#[4] "yellow"                      
#[5] "red"                         
#[6] "green"                       
#[7] "blue"                        
#[8] "black"                       
#[9] "tan"                         
#[10] "Did.Natural.Selection.occur."
#[11] "Total"                       
#[12] "Notes"      

counts <- results[,c("yellow", "red", "green", "blue", "black", "tan")]

results <- read.csv("http://jonsmitchell.com/data/biol112labresults.csv", stringsAsFactors = F)

counts <- results[,c("yellow", "red", "green", "blue", "black", "tan")]

backgrounds <- c("White", "Red", "Yellow", "Green", "Blue", "Black")

backgroundCol <- c("white", "#d53e4f", "#fee08b",
                   "#abdda4", "#3288bd", "black")

calcChi(counts[1,])

Chisqs <- apply(counts, 1, calcChi)

plotChis(counts)
# A lot closer to even when the Chi-squared value
# is low. A higher output from the Chi-squared test
# indicates that the differential survival is higher
# in that case than a blind predation.

Avg <- mean(Chisqs)
# 60.99081, much higher than the critical value for
# the exercise.

backgroundAvgs <- tapply(Chisqs, results[,3], mean)
# Definitely differs by background, but never dips
# below the critical value.

propSig <- length( which( Chisqs > 11.70))/length(Chisqs)
percSig <- round(100*propSig)
# ~92% are significant, probably because students
# like things to look profound or like they've done
# a great job of mimicing natural forces.

par(las=1, mar=c(4,4,1,1), mgp=c(2,0.5,0), tck=-0.01, cex.axis=1)

hist(Chisqs, main="", xlab="chi-squared values", ylab="frequency")

par(las=1, mar=c(4,4,1,1), mgp=c(2,0.5,0), tck=-0.01, cex.axis=1)

plot(1, 1, xlim=c(0,400), ylim=c(1,8.5), xlab="", ylab="", type="n", yaxt="n")

axis(2, at=1:length(backgrounds), labels=backgrounds)
mtext(side=1, expression(chi^2), cex=1.75, line=2.5)

counter <- 1
for(i in backgrounds){
  Data <- Chisqs[which(results[,3]==i)]
  addHist(Y=counter, Dat=Data, Color=backgroundCol[counter])
  counter <- counter+1
}

abline(v=11.70, lty=2, lwd=2, col="black")
# There don't seem to be any differences, which kinda
# seems to be in line with the plotChis(counts) thing
# from earlier.

Simulation <- simDraws(1000)

addHist(Y=7, Dat=Simulation, Color="lightgray")

mtext(side=2, at=7, line=0, "simulated")

abline(v=11.70, lty=2, lwd=)
# roughly 80% of the trials were above the critical
# value

Fit <- c(1,1,1,1,1,1)
names(Fit) <- 1:6
Simulation2 <- simDraws(1e4, w=Fit)
addHist(Y=8, Dat=Simulation2, Color=rgb(0,0,0,0,25))

Fit <- c(0.1,1,1,1,1,1)
names(Fit) <- 1:6
Simulation3 <- simDraws(1e4, w=Fit)
addHist(Y=8, Dat=Simulation3, Color=rgb(0,0,0,0,0.25))

Fit <- c(0.5,0.6,0.7,1,1,1)
names(Fit) <- 1:6
Simulation4 <- simDraws(1e4, w=Fit)
addHist(Y=8, Dat=Simulation4, Color=rgb(0,0,0,0,0.25))

Fit <- c(0.1,0.2,0.3,0.4,0.5,1)
names(Fit) <- 1:6
Simulation5 <- simDraws(1e4, w=Fit)
addHist(Y=8, Dat=Simulation5, Color=rgb(0,0,0,0,0.25))

Fit <- c(0.1, 0.1, 0.1, 0.1, 0.1, 1)
names(Fit) <- 1:6
Simulation6 <- simDraws(1e4, w=Fit)
addHist(Y=8, Dat=Simulation6, Color=rgb(0,0,0,0.25))
mtext(side=2, at=8, line=0, "sel.sim.")

Simulation7 <- c(Simulation2, Simulation2, Simulation3, Simulation4, Simulation5, Simulation6)
addHist(Y=8, Dat=Simulation7, Color=rgb(0,0,1,1))

