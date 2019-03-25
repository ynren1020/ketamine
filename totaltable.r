##merge bdi, madrs, shaps ...output together ##

bdi <- read.csv('D:/Biostatistics/2016FallRA/output/bdi.csv',header=T, sep=',')
madrs <- read.csv('D:/Biostatistics/2016FallRA/output/madrs.csv',header=T, sep=',')
shaps <- read.csv('D:/Biostatistics/2016FallRA/output/shaps.csv',header=T, sep=',')
tepscon <- read.csv('D:/Biostatistics/2016FallRA/output/tepscon.csv',header=T, sep=',')
tepsanti<- read.csv('D:/Biostatistics/2016FallRA/output/tepsanti.csv',header=T, sep=',')
cadss <- read.csv('D:/Biostatistics/2016FallRA/output/cadss.csv',header=T, sep=',')
cadssob <- read.csv('D:/Biostatistics/2016FallRA/output/cadssob.csv',header=T, sep=',')

##rbind##
total<-rbind(bdi,madrs)
total<-rbind(total,shaps)
total<-rbind(total,tepscon)
total<-rbind(total,tepsanti)
total<-rbind(total,cadss)
total<-rbind(total,cadssob)
total

##write table##
setwd("D:/Biostatistics/2016FallRA/output")
write.csv(total, file = "total.csv")
