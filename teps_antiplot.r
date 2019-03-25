##Plot teps_anticipatory change during the treatment##
##read dataset## NULL is replaced by NA in excel dataset
setwd("D:/Biostatistics/2016FallRA/")
ketamine<-read.csv("dataset/ketna.csv", header=T, sep=',')
attach(ketamine)
nrow(ketamine)
ncol(ketamine)

##teps_anticipatory subset ##

teps_anticipatory<-c("grid","bl_teps_anticipatory_total","pre_inf1_teps_anticipatory_total","X2hr_inf1_teps_anticipatory_total","pre_inf2_teps_anticipatory_total","X2hr_inf2_teps_anticipatory_total","pre_inf3_teps_anticipatory_total","X2hr_inf3_teps_anticipatory_total","pre_inf4_teps_anticipatory_total","X2hr_inf4_teps_anticipatory_total","pre_inf5_teps_anticipatory_total","X2hr_inf5_teps_anticipatory_total","pre_inf6_teps_anticipatory_total","X2hr_inf6_teps_anticipatory_total","ptx_teps_anticipatory_total")
ketamineteps_anticipatory<-ketamine[teps_anticipatory]
ncol(ketamineteps_anticipatory)
nrow(ketamineteps_anticipatory)


## long dataset##

library(reshape2)
ketamineteps_anticipatory_long <- melt(ketamineteps_anticipatory,
                                       id.vars = "grid",
                                       measure.vars = c("bl_teps_anticipatory_total","pre_inf1_teps_anticipatory_total","X2hr_inf1_teps_anticipatory_total","pre_inf2_teps_anticipatory_total","X2hr_inf2_teps_anticipatory_total","pre_inf3_teps_anticipatory_total","X2hr_inf3_teps_anticipatory_total","pre_inf4_teps_anticipatory_total","X2hr_inf4_teps_anticipatory_total","pre_inf5_teps_anticipatory_total","X2hr_inf5_teps_anticipatory_total","pre_inf6_teps_anticipatory_total","X2hr_inf6_teps_anticipatory_total","ptx_teps_anticipatory_total"),
                                       variable.name = "condition")

ketamineteps_anticipatory_long
as.numeric(ketamineteps_anticipatory_long$value)
View(ketamineteps_anticipatory_long)

##call function ##
source("D:/Biostatistics/2016FallRA/R/summarySE.r")
source("D:/Biostatistics/2016FallRA/R/normDataWithin.r")
source("D:/Biostatistics/2016FallRA/R/summarySEwithin.r")

##plot##
teps_anticipatoryc <- summarySE(ketamineteps_anticipatory_long, measurevar="value", groupvars="condition",na.rm=T)
teps_anticipatoryc$condition<-c("bl","pre.1","post.1","pre.2","post.2","pre.3","post.3","pre.4","post.4","pre.5","post.5","pre.6","post.6","ptx")
teps_anticipatoryc


library(ggplot2)
# Make the graph with the 95% confidence interval
bp<-ggplot(teps_anticipatoryc, aes(x=condition, y=value, group=1)) +
  geom_line() +
  geom_errorbar(width=.1, aes(ymin=value-ci, ymax=value+ci)) +
  geom_point(shape=21, size=3, fill="white") +
  ylim(5,45)+
  labs(x = "Infusion",
       y = "TEPS_anticipatory",
       title = "Mean and CI for TEPS_anticipatory Across Infusions")
bp+scale_x_discrete(limits=c("bl","pre.1","post.1","pre.2","post.2","pre.3","post.3","pre.4","post.4","pre.5","post.5","pre.6","post.6","ptx"))

