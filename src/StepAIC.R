#copier=read.csv("http://www.stat.ufl.edu/~athienit/STA6166/copiers.csv",skip=1,header=TRUE)
psadata=read.table("http://www.stat.ufl.edu/~athienit/STA6166/assignment3_1.txt",
                   col.names=c("ID","Y","Volume","Weight","Age","BPH","SVI","CP","GS"))

#print(psadata)     
reg=lm(Y ~ Volume + Weight + Age + BPH + CP + GS + Volume:SVI + Weight:SVI + Age:SVI + BPH:SVI + CP:SVI + GS:SVI, data=psadata)
summary(reg)
anova(reg)
abline(reg)

library(MASS)
fit1 <- lm(Y ~ Volume + Weight + Age + BPH + CP + GS + Volume:SVI + Weight:SVI + Age:SVI + BPH:SVI + CP:SVI + GS:SVI, data=psadata)
fit2 <- lm(Y ~ 1, data=psadata)
fit3=stepAIC(fit1,direction="backward")
stepAIC(fit2,direction="forward",scope=list(upper=fit1,lower=fit2))
stepAIC(fit2,direction="both",scope=list(upper=fit1,lower=fit2))
