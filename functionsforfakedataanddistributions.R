### Making Fake Data ###
### Help plan plotting, analyses, programs, scripts, etc.

## Sequence
sequence1 <- seq(from=1, to=10, by=1)
sequence2 <- seq(1,10,length.out=100)
sequence1
sequence2

dfseq <- data.frame(xcoord=sequence1, ycoord=seq(2,3,length.out=10))
head(dfseq, 10)
plot(ycoord~xcoord, data=dfseq)

## uniform distribution
randuni <- runif(10, min=2, max=3)
hist(randuni)
plot(density(randuni))

dfuni <- data.frame(xcoord=dfseq$xcoord, ycoord=randuni)
plot(ycoord~xcoord, data=dfuni)


## Normal Data
randnorm1 <- rnorm(10, mean=5, sd=1)
randnorm1
hist(randnorm1)
plot(density(randnorm1))

dfnorm <- data.frame(xcoord=dfseq$xcoord, ycoord=randnorm1)

plot(ycoord~xcoord, data=dfnorm)

## Log Normal Data
randlnorm <- rlnorm(1000, meanlog=5, sdlog=1)
plot(density(randlnorm))
plot(density(log(randlnorm)))

## Sampling
randnormbig <- rnorm(10000, mean=5, sd=1)
randnormsampF <- sample(randnormbig, size=100, replace=F)
randnormsampT <- sample(randnormbig, size=100, replace=T)
randnormsamphigh <- sample(randnormbig[randnormbig>=quantile(randnormbig,probs=0.75)], size=5, replace=F)
randnormsampprobT <- sample(c(1,2,3,4,5), size=100, replace=T, prob=c(0.125,0.25,0.25,0.25,0.125))
randnormsampprobF <- sample(c(1,2,3,4,5), size=100, replace=F, prob=c(0.125,0.25,0.25,0.25,0.125))
randnormsampprobF <- sample(c(1,2,3,4,5), size=length(c(1,2,3,4,5)), replace=F, prob=c(0.125,0.25,0.25,0.25,0.125))


plot(density(randnormbig), ylim=c(0,1))
points(density(randnormsampF), type="l", col="red")
points(density(randnormsampT), type="l", col="blue")
points(density(randnormsamphigh), type="l", col="darkgreen")
rug(randnormsamphigh,side=1, col="darkgreen", ticksize=0.1)

hist(randnormsampprobT)
hist(randnormsampprobF)

## Find Quantile (percentile)

quantile(randnorm1, probs=0.1)
quantile(randnorm1, probs=0.5)
median(randnorm1)
mean(randnorm1)
quantile(randnorm1, probs=c(0.025,0.25,0.5,0.75,0.975))
quantobj <- quantile(randnorm1, probs=c(0.025,0.25,0.5,0.75,0.975))
quantobj <- matrix(quantobj)
quantobj

boxplot(randnorm1)
rug(randnorm1, ticksize=0.1,side=4)
boxplot(randnorm1, plot=F)
bxpobj <- boxplot(randnorm1, plot=F)
bxpobj$stats

all.equal(bxpobj$stats, quantobj)
plot(bxpobj$stats, quantobj)
summary(lm(quantobj~bxpobj$stats))

IQR(randnorm1)
median(randnorm1)+(IQR(randnorm1)/2)
bxpobj$stats[4,]
# see ?boxplot.stats to explain difference.  Note how quartiles are calculated and values picked.
# "mod" modulo ?"%%"




