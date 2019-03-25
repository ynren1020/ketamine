##Plot teps_consummatory change during the treatment##
##read dataset## NULL is replaced by NA in excel dataset
setwd("D:/Biostatistics/2016FallRA/")
ketamine<-read.csv("dataset/ketna.csv", header=T, sep=',')
attach(ketamine)
nrow(ketamine)
ncol(ketamine)

##teps_consummatory subset ##

teps_consummatory<-c("grid","bl_teps_consummatory_total","pre_inf1_teps_consummatory_total","X2hr_inf1_teps_consummatory_total","pre_inf2_teps_consummatory_total","X2hr_inf2_teps_consummatory_total","pre_inf3_teps_consummatory_total","X2hr_inf3_teps_consummatory_total","pre_inf4_teps_consummatory_total","X2hr_inf4_teps_consummatory_total","pre_inf5_teps_consummatory_total","X2hr_inf5_teps_consummatory_total","pre_inf6_teps_consummatory_total","X2hr_inf6_teps_consummatory_total","ptx_teps_consummatory_total")
ketamineteps_consummatory<-ketamine[teps_consummatory]
ncol(ketamineteps_consummatory)
nrow(ketamineteps_consummatory)


## long dataset##

library(reshape2)
ketamineteps_consummatory_long <- melt(ketamineteps_consummatory,
                           id.vars = "grid",
                           measure.vars = c("bl_teps_consummatory_total","pre_inf1_teps_consummatory_total","X2hr_inf1_teps_consummatory_total","pre_inf2_teps_consummatory_total","X2hr_inf2_teps_consummatory_total","pre_inf3_teps_consummatory_total","X2hr_inf3_teps_consummatory_total","pre_inf4_teps_consummatory_total","X2hr_inf4_teps_consummatory_total","pre_inf5_teps_consummatory_total","X2hr_inf5_teps_consummatory_total","pre_inf6_teps_consummatory_total","X2hr_inf6_teps_consummatory_total","ptx_teps_consummatory_total"),
                           variable.name = "condition")

ketamineteps_consummatory_long
as.numeric(ketamineteps_consummatory_long$value)
View(ketamineteps_consummatory_long)

##call function ##
source("D:/Biostatistics/2016FallRA/R/summarySE.r")
source("D:/Biostatistics/2016FallRA/R/normDataWithin.r")
source("D:/Biostatistics/2016FallRA/R/summarySEwithin.r")

##plot##
teps_consummatoryc <- summarySE(ketamineteps_consummatory_long, measurevar="value", groupvars="condition",na.rm=T)
teps_consummatoryc$condition<-c("bl","pre.1","post.1","pre.2","post.2","pre.3","post.3","pre.4","post.4","pre.5","post.5","pre.6","post.6","ptx")
teps_consummatoryc


library(ggplot2)
# Make the graph with the 95% confidence interval
bp<-ggplot(teps_consummatoryc, aes(x=condition, y=value, group=1)) +
  geom_line() +
  geom_errorbar(width=.1, aes(ymin=value-ci, ymax=value+ci)) +
  geom_point(shape=21, size=3, fill="white") +
  ylim(5,45)+
  labs(x = "Infusion",
       y = "TEPS_consummatory",
       title = "Mean and CI for TEPS_consummatory Across Infusions")
bp+scale_x_discrete(limits=c("bl","pre.1","post.1","pre.2","post.2","pre.3","post.3","pre.4","post.4","pre.5","post.5","pre.6","post.6","ptx"))

