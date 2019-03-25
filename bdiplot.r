##Plot BDI change during the treatment##
##read dataset## NULL is replaced by NA in excel dataset
setwd("D:/Biostatistics/2016FallRA/")
ketamine<-read.csv("dataset/ketna.csv", header=T, sep=',')
attach(ketamine)
nrow(ketamine)
ncol(ketamine)

##BDI subset ##

bdi<-c("grid","bl_bdi_total_score","pre_inf1_bdi_total_score","post_inf1_bdi_total_score","pre_inf2_bdi_total_score","post_inf2_bdi_total_score","pre_inf3_bdi_total_score","post_inf3_bdi_total_score","pre_inf4_bdi_total_score","post_inf4_bdi_total_score","pre_inf5_bdi_total_score","post_inf5_bdi_total_score","pre_inf6_bdi_total_score","post_inf6_bdi_total_score","ptx_bdi_total_score")
ketaminebdi<-ketamine[bdi]
ncol(ketaminebdi)
nrow(ketaminebdi)


## wide to long dataset##

library(reshape2)
ketaminebdi_long <- melt(ketaminebdi,
                         id.vars = "grid",
                         measure.vars = c("bl_bdi_total_score","pre_inf1_bdi_total_score","post_inf1_bdi_total_score","pre_inf2_bdi_total_score","post_inf2_bdi_total_score","pre_inf3_bdi_total_score","post_inf3_bdi_total_score","pre_inf4_bdi_total_score","post_inf4_bdi_total_score","pre_inf5_bdi_total_score","post_inf5_bdi_total_score","pre_inf6_bdi_total_score","post_inf6_bdi_total_score","ptx_bdi_total_score"),
                         variable.name = "condition")

ketaminebdi_long
as.numeric(ketaminebdi_long$value)
View(ketaminebdi_long)



##call functions ##
source("D:/Biostatistics/2016FallRA/R/summarySE.r")
source("D:/Biostatistics/2016FallRA/R/normDataWithin.r")
source("D:/Biostatistics/2016FallRA/R/summarySEwithin.r")

##plot##
bdic <- summarySE(ketaminebdi_long, measurevar="value", groupvars="condition",na.rm=T)
bdic$condition<-c("bl","pre.1","post.1","pre.2","post.2","pre.3","post.3","pre.4","post.4","pre.5","post.5","pre.6","post.6","ptx")
bdic
class(bdic$condition)

library(ggplot2)
# Make the graph with the 95% confidence interval
bp<-ggplot(bdic, aes(x=condition, y=value, group=1)) +
  geom_line() +
  geom_errorbar(width=.1, aes(ymin=value-ci, ymax=value+ci)) +
  geom_point(shape=21, size=3, fill="white") +
  ylim(0,40)+
  labs(x = "Infusion",
       y = "BDI",
       title = "Mean and CI for BDI Across Infusions")
bp+scale_x_discrete(limits=c("bl","pre.1","post.1","pre.2","post.2","pre.3","post.3","pre.4","post.4","pre.5","post.5","pre.6","post.6","ptx"))

