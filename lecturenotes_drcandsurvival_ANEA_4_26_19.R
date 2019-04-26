

library(drc)

head(selenium, 20)

plot(dead/total~conc, data=selenium, pch=16)
plot(dead/total~conc, data=selenium, pch=16, log="x")

selenium$propmort <- selenium$dead/selenium$total

head(selenium, 20)

drm1 <- drm(propmort~conc, data=selenium, fct=LL.2(), type="binomial")
plot(drm1)
?plot.drc
plot(drm1, type="all", pch=16)
plot(drm1, type="confidence")
plot(drm1, type="obs", pch=16, add=T)


getMeanFunctions()

?mselect
mselect(drm1, list(LL.3(),LL.4(),LL.5(),W1.2(),W2.2(),W1.4(),W2.4()))
drm12 <- drm(propmort~conc, data=selenium, fct=W2.2(),type="binomial")
plot(drm12, pch=16, col="red")
plot(drm1, type="none", add=T)

ED(drm12, 0.5, type="absolute", interval="delta")
ED(drm12, c(0.5,0.1,0.9), type="absolute", interval="delta")

drmall <- drm(propmort~conc, curveid=type, data=selenium, fct=LL.2(), type="binomial")
plot(drmall)
mselect(drmall, list(LL.3(),LL.4(),LL.5(),W1.2(),W2.2(),W1.4(),W2.4()))
ED(drmall, 0.5, type="absolute", interval="delta")
compParm(drmall, "e", "-")

backfit(drm1)




library(survival)
?survfit
fit <- survfit(Surv(time,status)~x, data=aml)
plot(fit, col=c(1,2), conf.int=T, mark.time=T)
fitcox <- coxph(Surv(time,status)~x, data=aml)
summary(fitcox)


fit <- coxph(Surv(futime, fustat) ~ age, data = ovarian) 
plot(survfit(fit, newdata=data.frame(age=60)),
     xscale=365.25, xlab = "Years", ylab="Survival") 

fit2 <- survfit(Surv(futime,fustat)~age, data=ovarian)
plot(fit2)
