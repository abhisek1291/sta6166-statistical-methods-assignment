# Create data frame
machines_data=data.frame(time=c(42.5, 39.3, 39.6, 39.9, 42.9, 43.6, 39.8, 40.1, 40.5, 42.3, 42.5, 43.1,40.2, 40.5, 41.3, 43.4, 44.9, 45.1, 41.3, 42.2, 43.5, 44.2, 45.9, 42.3),
                 machine=factor(rep(c("1","2","3","4"),each=6 )), operator=factor(c("1","2","3","4","5","6")))

fmachine=factor(machines_data$machine, levels=1:4)
levels(fmachine)=c("Machine1", "Machine2", "Machine3","Machine4")

machines=data.frame(time=machines_data$time, fmachine, operator=factor(machines_data$operator))
attach(machines)
machines

# create easy to view table
table=xtabs(time~operator+fmachine);table
round(addmargins(table,c(1,2),FUN=mean),2)

# Fit the ANOVA for the RBD with subject and interacting agent as independent variables
machines.rbd=aov(time~fmachine+operator)
anova(machines.rbd)

machines.Tukey=TukeyHSD(machines.rbd,"fmachine",conf.level=0.95)
print(machines.Tukey)
#windows(width=5,height=5,pointsize=10)
plot(machines.Tukey, sub="Machines Data", adj=0)
mtext("Tukey Honest Significant Differences",side=3,line=0.5)