##Plot shaps change during the treatment##
##read dataset## NULL is replaced by NA in excel dataset
setwd("D:/Biostatistics/2016FallRA/")
ketamine<-read.csv("dataset/ketna.csv", header=T, sep=',')
attach(ketamine)
nrow(ketamine)
ncol(ketamine)

##shaps subset ##

shaps<-c("grid","bl_shaps_total","pre_inf1_shaps_total","X2hr_inf1_shaps_total","pre_inf2_shaps_total","X2hr_inf2_shaps_total","pre_inf3_shaps_total","X2hr_inf3_shaps_total","pre_inf4_shaps_total","X2hr_inf4_shaps_total","pre_inf5_shaps_total","X2hr_inf5_shaps_total","pre_inf6_shaps_total","X2hr_inf6_shaps_total","ptx_shaps_total")
ketamineshaps<-ketamine[shaps]
ncol(ketamineshaps)
nrow(ketamineshaps)


## long dataset##

library(reshape2)
ketamineshaps_long <- melt(ketamineshaps,
                           id.vars = "grid",
                           measure.vars = c("bl_shaps_total","pre_inf1_shaps_total","X2hr_inf1_shaps_total","pre_inf2_shaps_total","X2hr_inf2_shaps_total","pre_inf3_shaps_total","X2hr_inf3_shaps_total","pre_inf4_shaps_total","X2hr_inf4_shaps_total","pre_inf5_shaps_total","X2hr_inf5_shaps_total","pre_inf6_shaps_total","X2hr_inf6_shaps_total","ptx_shaps_total"),
                           variable.name = "condition")

ketamineshaps_long
as.numeric(ketamineshaps_long$value)
View(ketamineshaps_long)



##call function ##
source("D:/Biostatistics/2016FallRA/R/summarySE.r")
source("D:/Biostatistics/2016FallRA/R/normDataWithin.r")
source("D:/Biostatistics/2016FallRA/R/summarySEwithin.r")

##plot##
shapsc <- summarySE(ketamineshaps_long, measurevar="value", groupvars="condition",na.rm=T)
shapsc$condition<-c("bl","pre.1","post.1","pre.2","post.2","pre.3","post.3","pre.4","post.4","pre.5","post.5","pre.6","post.6","ptx")
shapsc


library(ggplot2)
# Make the graph with the 95% confidence interval
bp<-ggplot(shapsc, aes(x=condition, y=value, group=1)) +
  geom_line() +
  geom_errorbar(width=.1, aes(ymin=value-ci, ymax=value+ci)) +
  geom_point(shape=21, size=3, fill="white") +
  ylim(0,15)+
  labs(x = "Infusion",
       y = "SHAPS",
       title = "Mean and CI for SHAPS Across Infusions")
bp+scale_x_discrete(limits=c("bl","pre.1","post.1","pre.2","post.2","pre.3","post.3","pre.4","post.4","pre.5","post.5","pre.6","post.6","ptx"))

