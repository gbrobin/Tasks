setwd("C:\\Users\\yogur\\Desktop\\Evolution\\Tasks\\Task_02")
Data <- read.csv("http://jonsmitchell.com/data/beren.csv")

# Ways to look at the object
length(Data)
nrow(Data)
ncol(Data)
colnames(Data)
head(Data)

Data[1,]
Data[2,]
Data[1:3]
Data[1:3, 4]
Data[1:5, 1:3]
Data[257, 1:3]

beren <- Data

#
Feeds <-which(Data[,9] == "bottle")
# Takes the info from column nine, asks which elements
# are "bottle, then stores those in a variable called
# "Feeds"
berenMilk <- beren[Feeds,]
#stores the "Feeds" query in "berenMilk"
head(berenMilk)

Feeds <- which(beren[, "event"] == "bottle")
# searches for a column called "event" and then
# displays those elements of "event" for which
# "bottle" is true, storing them in "Feeds"

Feeds <- which(beren$event == "bottle")
# same as before, but we use a dollar sign as our
# bracket-and-comma situation

dayID <- apply(beren, 1, function(x) paste (x[1:3], collapse="-"))
dateID <- sapply(dayID, as.Date, format = "%Y-%m-%d", origin = "2019-04-18")

beren$age <- dateID - dateID[beren$event == "birth"]
# searches the "event" column for "birth", then finds
# the date of that day, which it subtracts from the
# date of any given other event, thereby converting
# that difference into an age in days. This whole
# shebang gets stored in "beren$age".

head(beren)

beren2 <- beren
beren3 <- beren2[order(beren$age),]
# beren3 gets all the information from beren2, but
# arranged into an order; that is, the x (rows) are 
# sorted by how old he was at the time of the row's
# occurrence. From what I can tell, that is.
head(beren)
head(beren2)
head(beren3)

write.csv(beren3, "beren_new.csv", quote=F, row.names=FALSE)
# makes a new .csv file out of our beren3 variable,
# saves it without quotation marks on the row names



# PART B

# Question 1:
# Hypothesis 1 doesn't work because we don't have his
# weight.
# Hypothesis 2 doesn't work because "a relationship"
# is meaningless.
setwd()
beren3 <- read.csv("beren_new.csv", stringsAsFactors=F)

Feeds <- which(beren3$event == "bottle")
avgMilk <- mean(beren3$value[Feeds])

avgFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], mean)
varFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], var)
numFeeds<- tapply(beren3$value[Feeds], beren3$age[Feeds], length)
totalFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], sum)

cor(beren3$value[Feeds], beren3$age[Feeds])
cor.test(beren3$value[Feeds], beren3$age[Feeds])

berenCor <- cor.test(beren3$value[Feeds], beren3$age[Feeds])
summary(berenCor)

berenANOVA <- aov(beren3$value[Feeds] ~ beren3$caregiver[Feeds])
boxplot(beren3$value[Feeds] ~ beren$caregiver[Feeds], xlab="who gave the bottle", ylab = "amount of milk consumed (oz)")

par(las=1, mar=c(5, 5, 1, 1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab="age in days", ylab="ounces of milk")
abline(h=mean(totalFeed), lty=2, col='red')
pdf("r02b-totalMilkByDay.pdf", height=4, width=4)
par(las=1, mar=c(5,5,1,1), mgp=c(2,0.5,0), tck=-0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab="age in days", ylab="ounces of milk")
abline(h=mean(totalFeed), lty=2, col='red')
dev.off()
# Question 2: There's no information given about
# actual consumption, just consumption while Beren
# is at daycare.
