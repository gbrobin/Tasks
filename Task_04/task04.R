trueMean1 <- 5
trueSD1 <- 5
population1 <- rnorm(1e6, trueMean1, trueSD1)

trueMean2 <- 4
trueSD2 <- 5
population2 <- rnorm(1e6, trueMean2, trueSD2)
# makes two populations that use variables in the 
# arguments to make it easier to shift values around
# later

Size <- 50
Sample1 <- sample(population1, Size)
Sample2 <- sample(population2, Size)
# once again, setting the numeric input to an 
# easily-changeable variable

boxplot(Sample1, Sample2)

source("http://jonsmitchell.com/code/simFxn04.R")

MatGrandma <- makeFounder("grandma_mom")
MatGrandpa <- makeFounder("grandpa_mom")
PatGrandma <- makeFounder("grandma_da")
PatGrandpa <- makeFounder("grandpa_da")

Alan <- makeBaby(PatGrandma, PatGrandpa)
Brenda <- makeBaby(MatGrandma, MatGrandpa)

Focus <- makeBaby(Brenda, Alan)

ToMom <- length(grep("mom", Focus)) / length(Focus)
# Looks through everything with "mom" in it for 
# things that match with Focus. Should be the full
# complement of what Focus shares with Brenda

ToMomMom <- length(grep( "grandma_mom", Focus))/length(Focus)
ToMomDad <- length(grep( "grandpa_mom", Focus))/length(Focus)
# Much more closely related to MatGrandma than
# MatGrandpa. At first, there was no relation at all
# to MatGrandpa, and then I realized on looking back
# that I accidentally made Brenda and Alan 
# half-siblings, which explained things.

Sibling_01 <- makeBaby(Brenda, Alan)
# Compiles another kid from Brenda and Alan
ToSib <- length(intersect(Focus, Sibling_01))/length(Focus)
# takes all the places where genes in Focus and her
# sibling intersect, and then compares that to the 
# total number of genes in Focus

ManySiblings <- replicate(1e3, length(intersect(
  Focus, makeBaby(Brenda, Alan)))/length(Focus))
# We've achieved a sort of Too Many Daves situation
# here

quantile(ManySiblings)
mean(ManySiblings)
plot(density(ManySiblings), main="", xlab="
     proportion shared genes")
# Independent assortment should yield a fairly normal
# distribution of similarity; there appear to be some
# who are almost completely unrelated to Focus; they 
# must have gotten essentially the opposite of her 
# genes in the dice toss of meiosis

HWE <- function(p) {
  aa <- p^2
  ab <- 2*p*(1-p)
  bb <- (1-p)^2
  return(c(aa=aa, ab=ab, bb=bb))
}

HWE(0.05)

plot(1, 1, type="n", xlim=c(0,1), ylim=c(0,1),
     xlab="freq. allele a", ylab="geno. freq")
p <- seq(from=0, to=1, by=0.01)
GenoFreq <- t(sapply(p, HWE))

lines(p, GenoFreq[, "aa"], lwd=2, col="red")
# genotype "aa" frequency increases toward one as
# the frequency of the allele approaches one

lines(p, GenoFreq[, "ab"], lwd=2, col="purple")
lines(p, GenoFreq[, "bb"], lwd=2, col="blue")
legend("top", legend=c("aa", "ab", "bb"),
       col=c("red", "purple", "blue"), lty=1, lwd=2,
       bty="n")

Pop <- simPop(500)
points(Pop[, "freqa"], Pop[,"Genotypes.aa"]/500,
       pch=21, bg="red")

Pop <- simPop(50)
points(Pop[,"freqa"], Pop[,"Genotypes.aa"]/50, pch=22,
       bg="red")
# We've got a much wider spread with this version,
# though I don't understand the whole process well
# enough to speak intelligently on why that would be.

install.packages(learnPopGen)

X <- genetic.drift(Ne=200, nrep=5, pause=0.01)
# runs genetic drift over a hundred generations

PopSizes <- 5:50
Samples <- rep(PopSizes, 5)
tExt <- sapply(Samples, function(x) nrow(simPop(x, 500)))
Line <- lm(tExt ~ Samples)
summary(Line)
Line$coef

plot(Samples, tExt)
abline(Line)

Line2 <- lm(tExt~Samples + 0)
# gotta get rid of outliers I think?
abline(Line2)
# The line has a lot more points below than above
# now, which means to me that the line is an upper
# limit to something, though it beats me what.