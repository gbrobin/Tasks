library(learnPopGen)

setwd("C:\\Users\\yogur\\Desktop\\Evolution\\Tasks\\Task_06")
pdf("Coalescent N10Ngen20.pdf", height=4, width=4)
coalescent.plot(n=10, ngen=50, colors=NULL)
dev.off()

pdf("coalescent N20Ngen25.pdf", height=4, width=4)
coalescent.plot(n=20, ngen=25, colors=NULL)


coalescent.plot(n=30, ngen=40, colors=NULL)
coalescent.plot()

# Changing the "n" argument changes the number of alleles,
# or, more accurately, the number of lines of descent. There
# doesn't seem to be much of an average under the default
# parameters. Averages seem to break down for number of
# offspring as well. Fitness doesn't have a role as far as
# I can tell. Usually there's a fluctuation that takes the
# eventual final trait back down to one member after an
# initial growth, meaning that the most recent common
# ancestor is more recent than the origin.

install.packages("coala")
install.packages("phytools")

library(coala)

model <- coal_model(sample_size=5, loci_number=10, 
  loci_length=500, ploidy=2)+
  feat_mutation(10)+
  feat_recombination(10)+
  sumstat_trees()+
  sumstat_nucleotide_div()

model

stats <- simulate(model, nsim=1)
stats

Diversity <- stats$pi
Diversity
# Numbers are different because different simulations will
# yield different results. Given enough simulations, we
# could figure out a good mean for the diversity, but for
# the time being, we don't have enough data to make
# any inferences.

Nloci <- length(stats$trees)

library(phytools)
t1 <- read.tree(text=stats$trees[[1]][1])
plot(t1)
axisPhylo()

# We're getting alleles of the one locus, and it's only
# displaying extant versions. Drift got rid of a bunch.

Age1 <- max(nodeHeights(t1))

t2 <- read.tree(text=stats$trees[[2]][1])
plot(t2)
axisPhylo()

# 