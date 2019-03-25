##merge data##

ket1 <- read.csv('D:/Biostatistics/2016FallRA/dataset/ket1.csv',header=T, sep=',')
ket2 <- read.csv('D:/Biostatistics/2016FallRA/dataset/ket2.csv',header=T, sep=',')
ket3 <- read.csv('D:/Biostatistics/2016FallRA/dataset/ket3.csv',header=T, sep=',')
ket4 <- read.csv('D:/Biostatistics/2016FallRA/dataset/ket4.csv',header=T, sep=',')
ket5 <- read.csv('D:/Biostatistics/2016FallRA/dataset/ket5.csv',header=T, sep=',')

##reorder ##
ket1<-ket1[order(ket1$grid),]
ket2<-ket2[order(ket2$grid),]
ket3<-ket3[order(ket3$grid),]
ket4<-ket4[order(ket4$grid),]
ket5<-ket5[order(ket5$grid),]

ncol(ket)
##combine##
ket<-cbind(ket1,ket2)
ket<-cbind(ket,ket3)
ket<-cbind(ket,ket4)
ket<-cbind(ket,ket5)

##write table##
setwd("D:/Biostatistics/2016FallRA/dataset")
write.csv(ket, file = "ket.csv")


