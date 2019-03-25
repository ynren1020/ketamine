##Plot cadss_observer change during the treatment##
##read dataset## NULL is replaced by NA in excel dataset
setwd("D:/Biostatistics/2016FallRA/")
ketamine<-read.csv("dataset/ketna.csv", header=T, sep=',')
attach(ketamine)
nrow(ketamine)
ncol(ketamine)

##cadss_observer subset ##
cadss_observer<-c("grid","pre_inf1_cadss_observer_total","post_inf1_cadss_observer_total","X1hr_inf1_cadss_observer_total","X2hr_inf1_cadss_observer_total","pre_inf2_cadss_observer_total","post_inf2_cadss_observer_total","X1hr_inf2_cadss_observer_total","X2hr_inf2_cadss_observer_total","pre_inf3_cadss_observer_total","post_inf3_cadss_observer_total","X1hr_inf3_cadss_observer_total","X2hr_inf3_cadss_observer_total","pre_inf4_cadss_observer_total","post_inf4_cadss_observer_total","X1hr_inf4_cadss_observer_total","X2hr_inf4_cadss_observer_total","pre_inf5_cadss_observer_total","post_inf5_cadss_observer_total","X1hr_inf5_cadss_observer_total","X2hr_inf5_cadss_observer_total","pre_inf6_cadss_observer_total","post_inf6_cadss_observer_total","X1hr_inf6_cadss_observer_total","X2hr_inf6_cadss_observer_total")
ketaminecadss_observer<-ketamine[cadss_observer]
ncol(ketaminecadss_observer)
nrow(ketaminecadss_observer)


## wide to long dataset##

library(reshape2)
ketaminecadss_observer_long <- melt(ketaminecadss_observer,
                           id.vars = "grid",
                           measure.vars = c("pre_inf1_cadss_observer_total","post_inf1_cadss_observer_total","X1hr_inf1_cadss_observer_total","X2hr_inf1_cadss_observer_total","pre_inf2_cadss_observer_total","post_inf2_cadss_observer_total","X1hr_inf2_cadss_observer_total","X2hr_inf2_cadss_observer_total","pre_inf3_cadss_observer_total","post_inf3_cadss_observer_total","X1hr_inf3_cadss_observer_total","X2hr_inf3_cadss_observer_total","pre_inf4_cadss_observer_total","post_inf4_cadss_observer_total","X1hr_inf4_cadss_observer_total","X2hr_inf4_cadss_observer_total","pre_inf5_cadss_observer_total","post_inf5_cadss_observer_total","X1hr_inf5_cadss_observer_total","X2hr_inf5_cadss_observer_total","pre_inf6_cadss_observer_total","post_inf6_cadss_observer_total","X1hr_inf6_cadss_observer_total","X2hr_inf6_cadss_observer_total"),
                           variable.name = "condition")

ketaminecadss_observer_long
as.numeric(ketaminecadss_observer_long$value)
View(ketaminecadss_observer_long)

##call functions ##
source("D:/Biostatistics/2016FallRA/R/summarySE.r")
source("D:/Biostatistics/2016FallRA/R/normDataWithin.r")
source("D:/Biostatistics/2016FallRA/R/summarySEwithin.r")

##plot##
cadss_observerc <- summarySE(ketaminecadss_observer_long, measurevar="value", groupvars="condition",na.rm=T)
cadss_observerc$condition<-rep(c("pre","post","1hr","2hr"),6)
cadss_observerc$infusion<-c(rep(1,4),rep(2,4),rep(3,4),rep(4,4),rep(5,4),rep(6,4))
cadss_observerc

library(ggplot2)
pd <- position_dodge(0.1)
ggplot(cadss_observerc, aes(x=condition, y=value, group=infusion)) + 
  geom_errorbar(aes(ymin=value-ci, ymax=value+ci), width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd)+
  facet_wrap(~infusion)+
  scale_x_discrete(limits=c("pre","post","1hr","2hr"))+
  labs(x = "Condition",
       y = "CADSS_observer",
       title = "Mean and CI for CADSS_observer")


library(ggplot2)
# Make the graph with the 95% confidence interval
bp<-ggplot(cadss_observerc, aes(x=condition, y=value, group=1)) +
  geom_line() +
  geom_errorbar(width=.1, aes(ymin=value-ci, ymax=value+ci)) +
  geom_point(shape=21, size=3, fill="white") +
  ylim(0,2)+
  labs(x = "Infusion",
       y = "CADSS_observer",
       title = "Mean for CADSS_observer Across Infusions")
bp+scale_x_discrete(limits=c("pre.1","post.1","pre.2","post.2","pre.3","post.3","pre.4","post.4","pre.5","post.5","pre.6","post.6","ptx"))

