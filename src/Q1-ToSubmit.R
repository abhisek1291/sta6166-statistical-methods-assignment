psadata=read.table("http://www.stat.ufl.edu/~athienit/STA6166/assignment3_1.txt",
                   col.names=c("ID","Y","Volume","Weight","Age","BPH","SVI","CP","GS"))

volume_trans=psadata$Volume
y=psadata$Y

regm=lm(y ~ volume_trans + Weight + Age + BPH + CP + volume_trans:SVI + Weight:SVI + Age:SVI + BPH:SVI + CP:SVI + volume_trans:GS + Weight:GS + Age:GS + BPH:GS + CP:GS, data=psadata)

source("http://www.stat.ufl.edu/~athienit/check.R")
check(regm,tests=TRUE) # Assumptions Fail

# Use powerTrans on volume. Some of the variables throw an error, probably because of log 0
# Volume transformations almost fixes the failed assumtions
library(car)
bc2=powerTransform(y~volume_trans)
summary(bc2)
yT=bcPower(y,0)

psadata.model <- lm(yT ~ volume_trans + Weight + Age + BPH + CP + volume_trans:SVI + Weight:SVI + Age:SVI + BPH:SVI + CP:SVI + volume_trans:GS + Weight:GS + Age:GS + BPH:GS + CP:GS, data=psadata)

summary(psadata.model)
anova(psadata.model)
summary(psadata.model)$adj.r.squared
AIC(psadata.model)

updatedpsamodel <- update(psadata.model, . ~ . -volume_trans:SVI)
summary(updatedpsamodel)
anova(updatedpsamodel)
summary(updatedpsamodel)$adj.r.squared
AIC(updatedpsamodel) 

updatedpsamodel <- update(updatedpsamodel, . ~ . -BPH:GS)
summary(updatedpsamodel)
anova(updatedpsamodel)
summary(updatedpsamodel)$adj.r.squared
AIC(updatedpsamodel)

updatedpsamodel <- update(updatedpsamodel, . ~ . -Age:GS)
summary(updatedpsamodel)
anova(updatedpsamodel)
summary(updatedpsamodel)$adj.r.squared
AIC(updatedpsamodel)

updatedpsamodel <- update(updatedpsamodel, . ~ . -volume_trans:GS)
summary(updatedpsamodel)
anova(updatedpsamodel)
summary(updatedpsamodel)$adj.r.squared
AIC(updatedpsamodel) 

updatedpsamodel <- update(updatedpsamodel, . ~ . -CP:GS)
summary(updatedpsamodel)
anova(updatedpsamodel)
summary(updatedpsamodel)$adj.r.squared
AIC(updatedpsamodel)

updatedpsamodel <- update(updatedpsamodel, . ~ . -CP)
summary(updatedpsamodel)
anova(updatedpsamodel)
summary(updatedpsamodel)$adj.r.squared
AIC(updatedpsamodel)

updatedpsamodel <- update(updatedpsamodel, . ~ . -SVI:CP)
summary(updatedpsamodel)
anova(updatedpsamodel)
summary(updatedpsamodel)$adj.r.squared
AIC(updatedpsamodel)

updatedpsamodel <- update(updatedpsamodel, . ~ . -Weight)
summary(updatedpsamodel)
anova(updatedpsamodel)
summary(updatedpsamodel)$adj.r.squared
AIC(updatedpsamodel)


updatedpsamodel <- update(updatedpsamodel, . ~ . -Weight:GS)
summary(updatedpsamodel)
anova(updatedpsamodel)
summary(updatedpsamodel)$adj.r.squared
AIC(updatedpsamodel)


updatedpsamodel <- update(updatedpsamodel, . ~ . -BPH:SVI)
summary(updatedpsamodel)
anova(updatedpsamodel)
summary(updatedpsamodel)$adj.r.squared
AIC(updatedpsamodel)

updatedpsamodel <- update(updatedpsamodel, . ~ . -Age:SVI)
summary(updatedpsamodel)
anova(updatedpsamodel)
summary(updatedpsamodel)$adj.r.squared
AIC(updatedpsamodel)

################Testing later##############
reg2=lm(yT~volume_trans + Age + BPH + Weight:SVI, data=psadata)
summary(reg2)

check(reg2,tests=TRUE) #Assumptions seem to hold true for the final model

############### Final Model ###############
#
# log y = 1.274287 + 0.082334*log(Volume) + 0.004201*Age + 0.093149*BPH + 0.012624*Weight*SVI
#
###########################################

predict.lm(reg2,se.fit=TRUE,newdata=data.frame(volume_trans = log(4.2633), Age = 68, BPH = 1.3500, Weight = 22.783, SVI = 0),interval="prediction",level=0.90)

