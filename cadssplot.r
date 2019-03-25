##Plot cadss change during the treatment##
##read dataset## NULL is replaced by NA in excel dataset
setwd("D:/Biostatistics/2016FallRA/")
ketamine<-read.csv("dataset/ketna.csv", header=T, sep=',')
attach(ketamine)
nrow(ketamine)
ncol(ketamine)

##cadss subset ##

cadss<-c("grid","pre_inf1_cadss_total","post_inf1_cadss_total","X1hr_inf1_cadss_total","X2hr_inf1_cadss_total","pre_inf2_cadss_total","post_inf2_cadss_total","X1hr_inf2_cadss_total","X2hr_inf2_cadss_total","pre_inf3_cadss_total","post_inf3_cadss_total","X1hr_inf3_cadss_total","X2hr_inf3_cadss_total","pre_inf4_cadss_total","post_inf4_cadss_total","X1hr_inf4_cadss_total","X2hr_inf4_cadss_total","pre_inf5_cadss_total","post_inf5_cadss_total","X1hr_inf5_cadss_total","X2hr_inf5_cadss_total","pre_inf6_cadss_total","post_inf6_cadss_total","X1hr_inf6_cadss_total","X2hr_inf6_cadss_total")
#cadss<-c("grid","pre_inf1_cadss_total","post_inf1_cadss_total","pre_inf2_cadss_total","post_inf2_cadss_total","pre_inf3_cadss_total","post_inf3_cadss_total","pre_inf4_cadss_total","post_inf4_cadss_total","pre_inf5_cadss_total","post_inf5_cadss_total","pre_inf6_cadss_total","post_inf6_cadss_total")

ketaminecadss<-ketamine[cadss]
ncol(ketaminecadss)
nrow(ketaminecadss)
View(ketaminecadss)

## wide to long dataset##

library(reshape2)
ketaminecadss_long <- melt(ketaminecadss,
                                       id.vars = "grid",
                                       measure.vars = c("pre_inf1_cadss_total","post_inf1_cadss_total","X1hr_inf1_cadss_total","X2hr_inf1_cadss_total","pre_inf2_cadss_total","post_inf2_cadss_total","X1hr_inf2_cadss_total","X2hr_inf2_cadss_total","pre_inf3_cadss_total","post_inf3_cadss_total","X1hr_inf3_cadss_total","X2hr_inf3_cadss_total","pre_inf4_cadss_total","post_inf4_cadss_total","X1hr_inf4_cadss_total","X2hr_inf4_cadss_total","pre_inf5_cadss_total","post_inf5_cadss_total","X1hr_inf5_cadss_total","X2hr_inf5_cadss_total","pre_inf6_cadss_total","post_inf6_cadss_total","X1hr_inf6_cadss_total","X2hr_inf6_cadss_total"),
                                       variable.name = "condition")

ketaminecadss_long
as.numeric(ketaminecadss_long$value)
View(ketaminecadss_long)

##call functions ##
source("D:/Biostatistics/2016FallRA/R/summarySE.r")
source("D:/Biostatistics/2016FallRA/R/normDataWithin.r")
source("D:/Biostatistics/2016FallRA/R/summarySEwithin.r")

##plot##
cadssc <- summarySE(ketaminecadss_long, measurevar="value", groupvars="condition",na.rm=T)
cadssc$condition<-rep(c("pre","post","1hr","2hr"),6)
cadssc$infusion<-c(rep(1,4),rep(2,4),rep(3,4),rep(4,4),rep(5,4),rep(6,4))
cadssc


library(ggplot2)
pd <- position_dodge(0.1)
ggplot(cadssc, aes(x=condition, y=value, group=infusion)) + 
  geom_errorbar(aes(ymin=value-ci, ymax=value+ci), width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd)+
  facet_wrap(~infusion)+
  scale_x_discrete(limits=c("pre","post","1hr","2hr"))+
  labs(x = "Condition",
       y = "CADSS",
       title = "Mean and CI for CADSS")


# Make the graph with the 95% confidence interval
#bp<-ggplot(cadssc, aes(x=condition, y=value)) +
#  geom_line() +
#  geom_errorbar(width=.1, aes(ymin=value-ci, ymax=value+ci)) +
# geom_point(shape=21, size=3, fill="white") +
#  ylim(0,10)+
#  labs(x = "Time",
#       y = "CADSS",
#       title = "Mean for CADSS")
#bp+scale_x_discrete(limits=c("pre-inf","post-inf","post-1hr","post-2hr"))




