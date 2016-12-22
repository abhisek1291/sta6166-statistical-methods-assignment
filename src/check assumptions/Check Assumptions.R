psadata=read.table("http://www.stat.ufl.edu/~athienit/STA6166/assignment3_1.txt",
                   col.names=c("ID","Y","Volume","Weight","Age","BPH","SVI","CP","GS"))


#reg=lm(Y ~ Volume + Weight + Age + BPH + CP + GS, data=psadata)
reg=lm(Y ~ Volume + CP + GS + Volume:SVI, data=psadata)
#reg=lm(Y ~ Volume + GS + Volume:SVI, data=psadata)
summary(reg)
anova(reg)
# abline(reg)

confint(reg,level=0.95,method="Wald")

# predict.lm(reg,se.fit=TRUE,newdata=data.frame(Copiers=67),interval="confidence",level=0.95)
# predict.lm(reg,se.fit=TRUE,newdata=data.frame(Copiers=123),interval="prediction",level=0.95)

source("http://www.stat.ufl.edu/~athienit/check.R")
check(reg,tests=TRUE)

### Checking model assumptions graphically, basically what the previous function does
re=rstandard(reg) #standardized residuals
# Normality
windows(6.5,5.4)
hist(re,,xlab="std. residuals",main="Histogram of std residuals")

windows(6.5,5.4)
qqnorm(re,datax=TRUE)
qqline(re,datax=TRUE)

# Independence
windows(6.5,5.4)
plot(re,type="o",pch=22,xlab="Order",ylab="std res",main="Independence")
abline(h=0)

# Homogeneity of variance/Model Fit
windows(6.5,5.4)
plot(re~fitted.values(reg),xlab=expression(hat(y)),ylab="std res",main="Homogeneity / Fit")
abline(h=0)