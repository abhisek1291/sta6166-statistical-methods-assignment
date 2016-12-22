psadata=read.table("http://www.stat.ufl.edu/~athienit/STA6166/assignment3_1.txt",
                   col.names=c("ID","Y","Volume","Weight","Age","BPH","SVI","CP","GS"))

#regm=lm(Y ~ Volume + CP + Volume:SVI + SVI:Weight + SVI:Age + Volume:GS + CP:GS, data = psadata)
regm=lm(Y ~ Volume + CP + Volume:SVI + Volume:GS + CP:GS, data = psadata)
summary(regm)
source("http://www.stat.ufl.edu/~athienit/check.R")
check(regm,tests=TRUE)
plot(Y ~ Volume + CP + Volume:SVI + SVI:Weight + SVI:Age + Volume:GS + CP:GS, data=psadata)
abline(regm)


re=rstandard(regm) #standardized residuals
### BOX-COX: 2 methods in R
# Method 1
# library(MASS)
# bc1=boxcox(regm)
# lambda=bc1$x[which.max(bc1$y)];lambda
# axis(1,at=round(lambda,2),cex.axis=0.7)

# Method 2
library(car)
#bc2=powerTransform(Y ~ Volume + CP + Volume:SVI + SVI:Weight + SVI:Age + Volume:GS + CP:GS, data=psadata)
bc2=powerTransform(Volume + SVI, data=psadata)
summary(bc2)
yT=bcPower(psadata$Y,0.1)

#reg2=lm(yT~Volume + CP + Volume:SVI + SVI:Weight + SVI:Age + Volume:GS + CP:GS, data = psadata)
reg2=lm(yT~Volume + CP + Volume:SVI + Volume:GS + CP:GS, data = psadata)
summary(reg2)
AIC(reg2)
anova(reg2)

source("http://www.stat.ufl.edu/~athienit/check.R")
check(reg2,tests=TRUE)

predict.lm(reg2,se.fit=TRUE,newdata=data.frame(Volume=4.2633,CP=0,GS=6,SVI=0),interval="prediction",level=0.95)
