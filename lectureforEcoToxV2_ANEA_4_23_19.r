library(readr)
data <- read_csv("popdatabytreats_nometadata_ANEA_skinny.csv")  # make sure that script+file are in same directory and the wd of R is set there

library(tidyverse)

library(ggridges)

## what are the groups of data like?
length(unique(data$rep))
length(data$rep)
length(unique(data$treatnum))
length(data$treatnum)


## Use dplyr strategy to group and summarise data
datasumm <- data %>%
  group_by(treatnum,day) %>%
  summarise(maxc = max(count),
            minc = min(count),
            meanall = mean(count),
            upperc = quantile(count,probs=c(0.95)),
            lowerc = quantile(count,probs=c(0.05)),
            varall = var(count),
            sdall = sd(count),
            medianall = median(count)
  )
head(datasumm,20)  # look at top of dataframe
tail(datasumm,20)  # look at end of dataframe

## to add proportion of mean column to dataframe
datasumm$contmean <- datasumm$meanall[datasumm$treatnum==0]  # repeat control mean data
head(datasumm, 20)
datasumm$propdiff <- datasumm$meanall/datasumm$contmean  # do vector math
head(datasumm, 20)

## to add day fudging for plotting tricks
datasumm$dayplus <- datasumm$day+0.5
datasumm$dayminus <- datasumm$day-0.5
head(datasumm,20)

## simple just view the data plots
plot(meanall~day, data=subset(datasumm, treatnum==0), type="l", ylim=c(0,300))
points(meanall~day, data=subset(datasumm, treatnum==10), type="l", col="blue")
points(meanall~day, data=subset(datasumm, treatnum==100), type="l", col="red")

plot(count~day, data=subset(data, treatnum==0), pch=16, ylim=c(0,300))
points(count~day, data=subset(data, treatnum==10), pch=16, col="blue")
points(count~day, data=subset(data, treatnum==100), pch=16, col="red")

boxplot(count~treatnum, data=subset(data, day==70))
boxplot(count~treatnum, data=data[data$day==70,])  # how to do subsetting?
boxplot(count~day, data=subset(data, treatnum==10))
boxplot(count~day*treatfact, data=data)
boxplot(count~treatfact*day, data=data)
boxplot(count~treatfact*day, data=data, col=c("darkgrey","blue","red"))


# more subsetting ideas...
data[data$day==70,]
data["day"]
data[["day"]]
data$day
head(data.frame(data$day),20)

# are the treatments different?
plot(data$count[data$treatnum==10]~data$count[data$treatnum==0], pch=16)
lm1 <- lm(data$count[data$treatnum==10]~data$count[data$treatnum==0])
summary(lm1)
abline(lm1)
plot(lm1)

# are the treatments different?
plot(density(data$count[data$treatnum==10]))
points(density(data$count[data$treatnum==0]), col="red", type="l")


# simple plot of proportional difference from control line plot
plot(propdiff~day, data=subset(datasumm, treatnum==0), type="l", ylim=c(0,2))
points(propdiff~day, data=subset(datasumm, treatnum==10), type="l", col="blue")
points(propdiff~day, data=subset(datasumm, treatnum==100), type="l", col="red")


# pretty plot of same using base methods
par(mai=c(1,1,0.1,0.1))
plot(propdiff~day, data=subset(datasumm, treatnum==0), type="l", 
     ylim=c(min(datasumm$propdiff),max(datasumm$propdiff)), 
     xlim=c(0,70),
     ylab="Proportional Difference to Control", xlab="Day", 
     font=2, font.axis=2, font.lab=2, cex.lab=1.5, cex.axis=1.25,
     bty="l", lwd=2, las=1)
points(propdiff~day, data=subset(datasumm, treatnum==10), type="l", col="blue", lwd=2)
points(propdiff~day, data=subset(datasumm, treatnum==100), type="l", col="red", lwd=2)
legend("topleft",c("Control","10 ppb","100 ppb"), col=c("black","blue","red"), 
       lty=1, pch=NA, text.font=2, bty="n", lwd=2)
text(50,0.6,bquote(paste("Mean, 10 ppb = ",.(round(mean(datasumm$propdiff[datasumm$treatnum==10]),2)))), col="blue")
text(50,0.55,bquote(paste("Mean, 100 ppb = ",.(round(mean(datasumm$propdiff[datasumm$treatnum==100]),2)))), col="red")
par(mai=c(1,1,0.5,0.5))


# simple ggplot of propdiff~time
ggplot(datasumm, aes(x=day,y=propdiff, group=factor(treatnum), colour=factor(treatnum))) + 
  geom_line()

ggplot(datasumm, aes(x=day,y=propdiff, group=factor(treatnum), colour=factor(treatnum))) + 
  geom_line() +
  theme_bw()  # add a theme

# pretty ggplot of propdiff
ggplot(datasumm, aes(x=day,y=propdiff, group=factor(treatnum), colour=factor(treatnum))) + 
  geom_line(lwd=1.25) +
  scale_colour_manual(values=c("black","blue","red")) +
  annotate("text", x=50,y=0.6,
           label=paste("Mean, 10 ppb = ", round(mean(datasumm$propdiff[datasumm$treatnum==10]),2)), colour="blue") +
  annotate("text", x=50,y=0.55,
           label=paste("Mean, 100 ppb = ", round(mean(datasumm$propdiff[datasumm$treatnum==100]),2)), colour="red") + 
  theme_classic() +
  theme(legend.position=c(0,1), legend.justification=c(0,1), 
        legend.background=element_blank(),axis.title=element_text(face="bold", size=14), 
        axis.text=element_text(face="bold", size=12, colour="black")) +
  labs(colour="Treatment", x="Day", y="Proportional Difference to Control")









plot(meanall~day, data=subset(datasumm, treatnum==0), type="l", ylim=c(min(datasumm$minc),max(datasumm$maxc)))
points(meanall~day, data=subset(datasumm, treatnum==10), type="l", col="blue")
points(meanall~day, data=subset(datasumm, treatnum==100), type="l", col="red")
points(maxc~day, data=subset(datasumm, treatnum==0), type="l", lty=2)
points(maxc~day, data=subset(datasumm, treatnum==10), type="l", lty=2, col="blue")
points(maxc~day, data=subset(datasumm, treatnum==100), type="l", lty=2, col="red")
points(minc~day, data=subset(datasumm, treatnum==0), type="l", lty=2)
points(minc~day, data=subset(datasumm, treatnum==10), type="l", lty=2, col="blue")
points(minc~day, data=subset(datasumm, treatnum==100), type="l", lty=2, col="red")

plot(meanall~day, data=subset(datasumm, treatnum==0), pch=16, ylim=c(min(datasumm$minc),max(datasumm$maxc)))
points(meanall~dayplus, data=subset(datasumm, treatnum==10), pch=16, col="blue")
points(meanall~dayminus, data=subset(datasumm, treatnum==100), pch=16, col="red")
arrows(datasumm$day[datasumm$treatnum==0],datasumm$lowerc[datasumm$treatnum==0],datasumm$day[datasumm$treatnum==0],datasumm$upperc[datasumm$treatnum==0], length=0.05, code=3, angle=90)
arrows(datasumm$dayplus[datasumm$treatnum==10],datasumm$lowerc[datasumm$treatnum==10],datasumm$dayplus[datasumm$treatnum==10],datasumm$upperc[datasumm$treatnum==10], length=0.05, code=3, angle=90, col="blue")
arrows(datasumm$dayminus[datasumm$treatnum==100],datasumm$lowerc[datasumm$treatnum==100],datasumm$dayminus[datasumm$treatnum==100],datasumm$upperc[datasumm$treatnum==100], length=0.05, code=3, angle=90, col="red")

plot(meanall~day, data=subset(datasumm, treatnum==0), pch="C", ylim=c(min(datasumm$minc[datasumm$day>=25&datasumm$day<=40]),max(datasumm$maxc[datasumm$day>=25&datasumm$day<=40])), xlim=c(23.5,40.25))
points(meanall~dayplus, data=subset(datasumm, treatnum==10), pch="L", col="blue")
points(meanall~dayminus, data=subset(datasumm, treatnum==100), pch="H", col="red")
arrows(datasumm$day[datasumm$treatnum==0],datasumm$lowerc[datasumm$treatnum==0],datasumm$day[datasumm$treatnum==0],datasumm$upperc[datasumm$treatnum==0], length=0.05, code=3, angle=90)
arrows(datasumm$dayplus[datasumm$treatnum==10],datasumm$lowerc[datasumm$treatnum==10],datasumm$dayplus[datasumm$treatnum==10],datasumm$upperc[datasumm$treatnum==10], length=0.05, code=3, angle=90, col="blue")
arrows(datasumm$dayminus[datasumm$treatnum==100],datasumm$lowerc[datasumm$treatnum==100],datasumm$dayminus[datasumm$treatnum==100],datasumm$upperc[datasumm$treatnum==100], length=0.05, code=3, angle=90, col="red")



plot(meanall~day, data=subset(datasumm, treatnum==0), pch=16, ylim=c(min(datasumm$minc),max(datasumm$maxc)), type="b")
points(meanall~dayplus, data=subset(datasumm, treatnum==10), pch=16, col="blue", type="b")
points(meanall~dayminus, data=subset(datasumm, treatnum==100), pch=16, col="red", type="b")
arrows(datasumm$day[datasumm$treatnum==0],datasumm$lowerc[datasumm$treatnum==0],datasumm$day[datasumm$treatnum==0],datasumm$upperc[datasumm$treatnum==0], length=0.05, code=3, angle=90)
arrows(datasumm$dayplus[datasumm$treatnum==10],datasumm$lowerc[datasumm$treatnum==10],datasumm$dayplus[datasumm$treatnum==10],datasumm$upperc[datasumm$treatnum==10], length=0.05, code=3, angle=90, col="blue")
arrows(datasumm$dayminus[datasumm$treatnum==100],datasumm$lowerc[datasumm$treatnum==100],datasumm$dayminus[datasumm$treatnum==100],datasumm$upperc[datasumm$treatnum==100], length=0.05, code=3, angle=90, col="red")




ggplot(data,aes(x=count, y=factor(treatnum))) +
  geom_density_ridges2(scale=2.5, rel_min_height=0.005) +
  facet_wrap(~day) +
  theme_ridges()
  
ggplot(data,aes(x=count, y=factor(day))) +
  geom_density_ridges2(scale=2.5, rel_min_height=0.005) +
  facet_wrap(~treatnum) +
  theme_ridges()

ggplot(datasumm, aes(x=day, y=meanall, group=factor(treatnum), fill=factor(treatnum))) +
  geom_ribbon(aes(ymin=minc, ymax=maxc), alpha=0.3) +
  geom_line(aes(x=day, y=meanall, group=factor(treatnum), colour=factor(treatnum)))






data$ID2 <- paste(data$treatfact,data$rep,sep="")

ggplot(data, aes(x=day, y=count, colour=factor(treatnum), group=ID2)) +
  geom_line()

ggplot(data, aes(x=day, y=count, colour=factor(treatnum), group=ID2)) +
  geom_line() +
  facet_wrap(~factor(treatnum))

ggplot() +
  geom_line(data=data, mapping=aes(x=day, y=count, colour=factor(treatnum), group=ID2)) +
  geom_ribbon(data=datasumm,mapping=aes(ymin=minc, ymax=maxc, x=day, fill=factor(treatnum)), alpha=0.3) +
  facet_wrap(~treatnum, ncol=1) +
  theme(legend.position="top")


plot(1,1,ylim=c(0,2),xlim=c(0,200), pch=NA)
text(datasumm$meanall[datasumm$day>=25], datasumm$propdiff[datasumm$day>=25], datasumm$treatnum[datasumm$day>=25])

plot(1,1,ylim=c(0,70),xlim=c(0,200), pch=NA)
text(data$count, data$day, data$ID2)

plot(count~day, data=data, pch=c("C","L","H")[unclass(factor(data$treatfact))], col=c("black","blue","red")[unclass(factor(treatnum))], xlim=c(23.5,40.25))
plot(count~jitter(day,1), data=data, pch=c("C","L","H")[unclass(factor(data$treatfact))], col=c("black","blue","red")[unclass(factor(treatnum))], xlim=c(23.5,40.25))









cont1 <- subset(data, ID2=="C1")
cont2 <- subset(data, ID2=="C2")
cont3 <- subset(data, ID2=="C3")
cont4 <- subset(data, ID2=="C4")
cont5 <- subset(data, ID2=="C5")

plot(count~day, data=cont1, type="l", ylim=c(0,250))
points(count~day, data=cont2, type="l")
points(count~day, data=cont3, type="l")
points(count~day, data=cont4, type="l")
points(count~day, data=cont5, type="l")





