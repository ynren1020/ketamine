##Plot madrs change during the treatment##
##read dataset## NULL is replaced by NA in excel dataset
setwd("D:/Biostatistics/2016FallRA/")
ketamine<-read.csv("dataset/ketna.csv", header=T, sep=',')
attach(ketamine)
nrow(ketamine)
ncol(ketamine)

##madrs subset ##

madrs<-c("grid","bl_madrs_total","pre_inf1_madrs_total","post_inf1_madrs_total","pre_inf2_madrs_total","post_inf2_madrs_total","pre_inf3_madrs_total","post_inf3_madrs_total","pre_inf4_madrs_total","post_inf4_madrs_total","pre_inf5_madrs_total","post_inf5_madrs_total","pre_inf6_madrs_total","post_inf6_madrs_total","ptx_madrs_total")
ketaminemadrs<-ketamine[madrs]
ncol(ketaminemadrs)
nrow(ketaminemadrs)


## long dataset##

library(reshape2)
ketaminemadrs_long <- melt(ketaminemadrs,
                         id.vars = "grid",
                         measure.vars = c("bl_madrs_total","pre_inf1_madrs_total","post_inf1_madrs_total","pre_inf2_madrs_total","post_inf2_madrs_total","pre_inf3_madrs_total","post_inf3_madrs_total","pre_inf4_madrs_total","post_inf4_madrs_total","pre_inf5_madrs_total","post_inf5_madrs_total","pre_inf6_madrs_total","post_inf6_madrs_total","ptx_madrs_total"),
                         variable.name = "condition")

ketaminemadrs_long
as.numeric(ketaminemadrs_long$value)
View(ketaminemadrs_long)



##call function ##
source("D:/Biostatistics/2016FallRA/R/summarySE.r")
source("D:/Biostatistics/2016FallRA/R/normDataWithin.r")
source("D:/Biostatistics/2016FallRA/R/summarySEwithin.r")

##plot##
madrsc <- summarySE(ketaminemadrs_long, measurevar="value", groupvars="condition",na.rm=T)
madrsc$condition<-c("bl","pre.1","post.1","pre.2","post.2","pre.3","post.3","pre.4","post.4","pre.5","post.5","pre.6","post.6","ptx")
madrsc


library(ggplot2)
# Make the graph with the 95% confidence interval
bp<-ggplot(madrsc, aes(x=condition, y=value, group=1)) +
  geom_line() +
  geom_errorbar(width=.1, aes(ymin=value-ci, ymax=value+ci)) +
  geom_point(shape=21, size=3, fill="white") +
  ylim(0,40)+
  labs(x = "Infusion",
       y = "MADRS",
       title = "Mean and CI for MADRS Across Infusions")
bp+scale_x_discrete(limits=c("bl","pre.1","post.1","pre.2","post.2","pre.3","post.3","pre.4","post.4","pre.5","post.5","pre.6","post.6","ptx"))

