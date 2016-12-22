assignmentdata=read.csv("/Users/abhisekmohanty/Documents/Assignment.csv",header=TRUE)
assignmentdata
#l <- vector('list', log(recoverydata$recovery)) 
#l
plot(assignmentdata$Production,assignmentdata$ElectricUsage,pch=16,xlab="Production",ylab="Usage",main="Scatterplot")

sd(assignmentdata$ElectricUsage)
reg=lm(ElectricUsage~Production,data=assignmentdata)
summary(reg)
anova(reg)
abline(reg)

confint(reg,level=0.95,method="Wald")

predict.lm(reg,se.fit=TRUE,newdata=data.frame(Copiers=67),interval="confidence",level=0.95)
predict.lm(reg,se.fit=TRUE,newdata=data.frame(Copiers=123),interval="prediction",level=0.95)

qt(0.025, 11)