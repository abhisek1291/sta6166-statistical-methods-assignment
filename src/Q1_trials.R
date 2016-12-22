psadata=read.table("http://www.stat.ufl.edu/~athienit/STA6166/assignment3_1.txt",
                   col.names=c("ID","Y","Volume","Weight","Age","BPH","SVI","CP","GS"))

print(psadata)

#Adding all the parameters. Will reduce in the next steps
psadata.model <- lm(Y ~ Volume + Weight + Age + BPH + CP + Volume:SVI + Weight:SVI + Age:SVI + BPH:SVI + CP:SVI + Volume:GS + Weight:GS + Age:GS + BPH:GS + CP:GS, data=psadata)

summary(psadata.model)
AIC(psadata.model)
anova(psadata.model)

#Removing Age - highest p-value = 0.883442

updatedpsamodel <- update(psadata.model, . ~ . -Age)
summary(updatedpsamodel)
AIC(updatedpsamodel)
anova(updatedpsamodel)

#Next Highest is Weight:SVI, but removing it increases the AIC, hence skipping it for now

#Removing Weight:GS, Age:GS - next highest p-values = 0.7766404, 0.7551538

updatedpsamodel <- update(updatedpsamodel, . ~ . -Weight:GS -Age:GS)
summary(updatedpsamodel)
AIC(updatedpsamodel)
anova(updatedpsamodel)

#Removing Weight - next highest p-values = 0.7509112

updatedpsamodel <- update(updatedpsamodel, . ~ . -Weight)
summary(updatedpsamodel)
AIC(updatedpsamodel) 
anova(updatedpsamodel)

#Removing SVI:Weight - highest p-value = 0.7718984
# This increases the AIC
# Lets skip and check till the end

#Removing CP:SVI - highest p-value = 0.7064485

updatedpsamodel <- update(updatedpsamodel, . ~ . -CP:SVI)
summary(updatedpsamodel)
AIC(updatedpsamodel) 
anova(updatedpsamodel)

#Removing SVI:Age - highest p-value = 0.6539774
# This increases the AIC
# Lets skip and check till the end
# updatedpsamodel <- update(updatedpsamodel, . ~ . -SVI:Age)
# summary(updatedpsamodel)
# AIC(updatedpsamodel) 
# anova(updatedpsamodel)


#Removing BPH:SVI - highest p-value = 0.6307026

updatedpsamodel <- update(updatedpsamodel, . ~ . -BPH:SVI)
summary(updatedpsamodel)
AIC(updatedpsamodel)
anova(updatedpsamodel)

#Removing BPH:GS - highest p-value = 0.4695622

updatedpsamodel <- update(updatedpsamodel, . ~ . -BPH:GS)
summary(updatedpsamodel)
AIC(updatedpsamodel)
anova(updatedpsamodel)


#Removing BPH - highest p-value = 0.352512

updatedpsamodel <- update(updatedpsamodel, . ~ . -BPH)
summary(updatedpsamodel)
AIC(updatedpsamodel) 
anova(updatedpsamodel)
summary(updatedpsamodel)$adj.r.squared


# #Try removing SVI:Weight
# Still increases the AIC, R-squared decreases
# Dont think its would be a good idea to remove this parameter
updatedpsamodel <- update(updatedpsamodel, . ~ . -SVI:Weight)
summary(updatedpsamodel)
AIC(updatedpsamodel) 
anova(updatedpsamodel)
summary(updatedpsamodel)$adj.r.squared

# #Try removing SVI:Age
# Still increases the AIC, R-squared decreases
# Dont think its would be a good idea to remove this parameter
updatedpsamodel <- update(updatedpsamodel, . ~ . -SVI:Age)
summary(updatedpsamodel)
AIC(updatedpsamodel) 
anova(updatedpsamodel)
summary(updatedpsamodel)$adj.r.squared

################################
#########  FINAL MODEL  #########
#
#
# Final Model : 6.3982 + 4.6098*Volume - 39.5617*CP + 1.6402*Volume*SVI - 0.7187*SVI*Weight + 0.5120*SVI*Age - 0.4663*Volume*GS + 5.4943*CP*GS
#
#
# Without Weight and Age : 6.9739 + 4.5194*Volume - 34.1294*CP + 1.7483*Volume*SVI - 0.4615*Volume*GS + 4.7445*CP*GS
#