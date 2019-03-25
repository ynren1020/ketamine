##Confidence Interval for phat(response rate)##
N<-c(rep(20,6),rep(50,6),rep(80,6),rep(100,6))
p.hat<-rep(seq(0.2,0.7,0.1),4)
confdf<-data.frame(N,p.hat)
confdf$ci<-1.96*sqrt(p.hat*(1-p.hat)/N)
confdf

library(ggplot2)
pd <- position_dodge(0.1)
ggplot(confdf, aes(x=p.hat, y=p.hat, group=N)) + 
  geom_errorbar(aes(ymin=p.hat-ci, ymax=p.hat+ci), width=.1, position=pd) +
  geom_point(position=pd)+
  facet_wrap(~N)+
#  scale_x_discrete(limits=c("0.2","0.3","0.4","0.5","0.6","0.7"))+
  labs(x = "Response Rate",
       y = "P",
       title = "Mean and CI for response rate")+

theme_bw()
