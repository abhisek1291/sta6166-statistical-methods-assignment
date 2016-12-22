# Create data frame
machines_data=data.frame(time=c(42.5, 39.3, 39.6, 39.9, 42.9, 43.6, 39.8, 40.1, 40.5, 42.3, 42.5, 43.1,40.2, 40.5, 41.3, 43.4, 44.9, 45.1, 41.3, 42.2, 43.5, 44.2, 45.9, 42.3),
                         machine=factor(rep(c("1","2","3","4"),each=6 )), operator=factor(c("1","2","3","4","5","6")))

foperator=factor(machines_data$operator, levels=1:6)
levels(foperator)=c("Operator1", "Operator2", "Operator3","Operator4","Operator5","Operator6")

machines=data.frame(time=machines_data$time, foperator, machine=factor(machines_data$machine))
attach(machines)
machines

# means=tapply(machines$time,machines$treat,mean) # obtain means by trt
# means
# tapply(machines$time,machines$treat,sd) # obtain st.dev. by trt

# create easy to view table
table=xtabs(time~machine + foperator);table
round(addmargins(table,c(1,2),FUN=mean),2)

# Fit the ANOVA for the RBD with subject and interacting agent as independent variables
machines.rbd=aov(time~foperator+machine)
anova(machines.rbd)

machines.Tukey=TukeyHSD(machines.rbd,"foperator",conf.level=0.90)
print(machines.Tukey)
#windows(width=5,height=5,pointsize=10)
plot(machines.Tukey, sub="Machines Data", adj=0)
mtext("Tukey Honest Significant Differences",side=3,line=0.5)