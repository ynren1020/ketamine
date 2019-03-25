---
title: "fMRI"
author: "Yanan Ren"
date: "2016Äê11ÔÂ9ÈÕ"
output: word_document
---

```{r setup, include=FALSE}
setwd("D:/Biostatistics/2016FallRA/")
fMRI<-read.csv("dataset/ketamine_clusters_r.csv", header=T, sep=',')
attach(fMRI)
View(fMRI)

##mean##
mean_fMRI<-apply(fMRI[,2:ncol(fMRI)], 2, mean)
mean_fMRI
length(mean_fMRI)
sd_fMRI<-apply(fMRI[,2:ncol(fMRI)], 2, sd)
sd_fMRI
length(sd_fMRI)
baseline_mean<-mean_fMRI[]




```

