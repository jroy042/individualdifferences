# Analysis Script for the 
# In Progress Cleaning. 

makeGam <- function(labuslong,labuslong2, saveDir = "/data/Big Gamms/"){
#Creates the gam objects and saves everything to a file
require(mgcv)
require(visreg)
require(parallel)
require(irr)
  
totList = readRPT(dirR="/data/Dropbox/current papers/Active/Cole-Mahrt-Roy/Cole-Mahrt-Roy prosody modeling/data")
#totList = readRPT(dirR="/data/Dropbox/current papers/Active/Lab Phonology/Cole-Mahrt-Roy prosody modeling/data",cutT=FALSE)
  
  
list2env(totList ,.GlobalEnv)
  
nc <- 12   ## cluster size, set for example portability
  if (detectCores()>1) { ## no point otherwise
    cl <- makeCluster(nc) 
    ## could also use makeForkCluster, but read warnings first
  } else cl <- NULL
  
boundGAM = bam(rating ~ s(word.phonerate)  + pos +  s(log.f0.sv.max.norm) + s(int.sv.norm) + 
                 s(log.wordfreq.switchboard)+ s(durpostpause) +
                 s(word,bs="re") + 
                 s(subject, word.phonerate,bs="fs",m=1)+
                 s(subject, log.f0.sv.max.norm, bs="fs",m=1)+  
                 s(subject, int.sv.norm, bs="fs",m=1)+  
                 s(subject,log.wordfreq.switchboard,bs="fs",m=1)+
                 s(subject,durpostpause,bs="fs",m=1)+
                 s(subject,pos,bs="re"), 
               family="binomial", data=labuslong, cluster=cl)

promGAM = bam(rating ~ s(word.phonerate) + s(durpostpause)+
                s(log.f0.sv.max.norm) +  s(log.wordfreq.switchboard)+ s(int.sv.norm) + pos + boundarym +
                s(word,bs="re") +  
                s(subject,durpostpause,bs="fs",m=1)+
                s(subject, pos, bs="re") + 
                s(subject, boundarym,bs="re")+   
                s(subject, word.phonerate,bs="fs",m=1)+
                s(subject, log.wordfreq.switchboard, bs="fs",m=1)+
                s(subject, log.f0.sv.max.norm, bs="fs",m=1)+  
                s(subject, int.sv.norm, bs="fs",m=1), 
              family="binomial", data=labuslong2, cluster=cl)

save(boundGAM,promGAM,labuslong,labuslong2, file="gammsNEW.rda", compress='xz')

}

plotIndividualDiffs <- function(boundGam,promGam) {
  require(mgcv)
  require(visreg)
  require(ggplot2)
  
  plotGAMSSubject(gam1=boundGAM,type="Boundary")
  plotGAMSSubject(gam1=boundGAM,type="Boundary_all_in_one",ovt=TRUE)
  plotGAMSSubject(gam1=boundGAM,type="Boundary",subj=FALSE)
  plotGAMSSubject(gam1=boundGAM,type="ToBI_Boundary",subj=FALSE, condL = list(subject="33"))


  load("gammsNEW.rda")


 plotGAMSSubject(gam1=boundGAM,type="Boundary")
 plotGAMSSubject(gam1=boundGAM,type="Boundary_all_in_one",ovt=TRUE)
 plotGAMSSubject(gam1=promGAM,type="Prominence")
 plotGAMSSubject(gam1=promGAM,type="Prominence_all_in_one",ovt=TRUE)
 plotGAMSSubject(gam1=boundGAM,type="Boundary",subj=FALSE)
 plotGAMSSubject(gam1=promGAM,type="Prominence",subj=FALSE)

  plotGAMSSubject(gam1=boundGAM,type="ToBI_Boundary",subj=FALSE, condL = list(subject="33"))
  plotGAMSSubject(gam1=promGAM,type="ToBI_Prominence",subj=FALSE, condL=list(subject="33"))

  plotGAMSSubject(gam1=boundGAM,type="Part_22",subj=FALSE, condL = list(subject="22"))
  plotGAMSSubject(gam1=boundGAM,type="Part_23",subj=FALSE, condL = list(subject="23"))


  load("gammsNEW.rda")

  summary(boundGAM)
  summary(promGAM)

  png(file = "frequnecyPlotBound.png", bg = "white",units="in",width = 8, height = 11, res = 72)
  plot = visreg(boundGAM,type="conditional",xvar ="log.wordfreq.switchboard",line=list(col="black"),ps = 12, cex = 1, cex.main = 1, 
              main="Boundary",xlab="log.wordfreq.switchboard",ylab="P(Boundary)",scale="response")
  dev.off()


  png(file = "frequnecyPlotProm.png", bg = "white",units="in",width = 8, height = 11, res = 72)
  plot = visreg(promGAM,type="conditional",xvar ="log.wordfreq.switchboard",line=list(col="black"),ps = 12, cex = 1, cex.main = 1, 
             main="Prominence",xlab="log.wordfreq.switchboard",ylab="P(Prominence)",scale="response")
 dev.off()

  png(file = "wordPhoneBound.png", bg = "white",units="in",width = 8, height = 11, res = 72)
  plot = visreg(boundGAM,type="conditional",xvar ="word.phonerate",line=list(col="black"),ps = 12, cex = 1, cex.main = 1, 
              main="Boundary",xlab="word.phonerate",ylab="P(Boundary)",scale="response")
  dev.off()


  png(file = "WordPhoneProm.png", bg = "white",units="in",width = 8, height = 11, res = 72)
  plot = visreg(promGAM,type="conditional",xvar ="word.phonerate",line=list(col="black"),ps = 12, cex = 1, cex.main = 1, 
              main="Prominence",xlab="word.phonerate",ylab="P(Prominence)",scale="response")
  dev.off()


 png(file = "posBoundProm.png", bg = "white",units="in",width = 8, height = 11, res = 72)
  plot = visreg(boundGAM,type="conditional",xvar ="pos",line=list(col="black"),ps = 12, cex = 1, cex.main = 1, ylim =c(0,.5),
              main="Boundary",xlab="pos",ylab="P(Boundary)",rug=FALSE, partial=FALSE,scale="response")
 dev.off()

}