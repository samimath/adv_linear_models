## sleep study example with lmer:
library(lme4)
library(lattice)
data(sleepstudy)
#View(sleepstudy)


xyplot(Reaction ~ Days | Subject, 
       data=sleepstudy,
       type = c("g","p","r"),
       xlab = "Days of sleep deprivation",
       ylab = "Average reaction time (ms)", 
       aspect = "xy",layout=c(9,2))


fm1 <- lmer(Reaction ~ Days + (Days | Subject),
            sleepstudy,REML=FALSE)

fm1


fm2 <- lmer(Reaction ~ Days + (Days | Subject),
            sleepstudy,REML=TRUE)

fm2