# Analysis Script for the 
# In Progress Cleaning. 

makeGam <- function(){
#Creates the gam objects and saves everything to a file
require(mgcv)
require(visreg)
require(parallel)
require(irr)
  
totList = readRPT(dirR="/data/Dropbox/current papers/Active/Cole-Mahrt-Roy/Cole-Mahrt-Roy prosody modeling/data")
#totList = readRPT(dirR="C:/Users/Joe/Dropbox/current papers/Active/Cole-Mahrt-Roy/Cole-Mahrt-Roy prosody modeling/data",cutT=FALSE)
  
  
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

save(boundGAM,promGAM,labuslong,labuslong2, file="/data/ETAP Feb 2017/gammsNEW.rda", compress='xz')

}

makeETAPIndiv <- function() {
  require(mgcv)
  require(visreg)
  require(ggplot2)
  

  load("/data/ETAP Feb 2017/gammsNEW.rda")
  
  boundGAM$data = labuslong
  promGAM$data=labuslong2
  
  setwd("/data/Dropbox/current papers/Active/Cole-Mahrt-Roy/LabPhon special issue (ETAP)/new output/")

  # capture.output(summary(boundGAM),file="boundaryGAM.txt")
  # capture.output(summary(promGAM),file="prominenceGAM.txt")
  # 
  # capture.output(concurvity(boundGAM),file="checks.txt", append=TRUE)
  # capture.output(concurvity(promGAM),file="checks.txt", append=TRUE)
  # 

  plotGAMSSubject(gam1=boundGAM,type="Boundary")
  plotGAMSSubject(gam1=boundGAM,type="Boundary_all_in_one",ovt=TRUE)
  plotGAMSSubject(gam1=boundGAM,type="Boundary",subj=FALSE)

  plotGAMSSubject(gam1=promGAM,type="Prominence")
  plotGAMSSubject(gam1=promGAM,type="Prominence_all_in_one",ovt=TRUE)
  plotGAMSSubject(gam1=promGAM,type="Prominence",subj=FALSE)


  plotGAMSSubject(gam1=boundGAM,type="ToBI_Boundary",subj=FALSE, condL = list(subject="33"))
  plotGAMSSubject(gam1=promGAM,type="ToBI_Prominence",subj=FALSE, condL=list(subject="33"))


  plotGAMSSubject(gam1=boundGAM,type="Part_22",subj=FALSE, condL = list(subject="22"))
  plotGAMSSubject(gam1=boundGAM,type="Part_23",subj=FALSE, condL = list(subject="23"))
}

makeETAPOverall <- function(){
  
  require(mgcv)
  require(visreg)
  require(ggplot2)
  
  load("/data/ETAP Feb 2017/gammsNEW.rda")
  
  boundGAM$data = labuslong
  promGAM$data=labuslong2
  
  setwd("/data/Dropbox/current papers/Active/Cole-Mahrt-Roy/LabPhon special issue (ETAP)/new output/")
  
  
  #Local Intensity
  png(file = "IntensityBoundary.png", bg = "white",units="in",width = 8, height = 11, res = 300)
  plot = visreg(boundGAM,type="conditional",xvar ="int.sv.norm",line=list(col="black"),ps = 16, cex = 1, cex.main = 1, ylim =c(0,.5),
                main="Boundary",xlab="Local Intensity",ylab="P(Boundary)",scale="response")
  dev.off()
  
  png(file = "IntensityProminence.png", bg = "white",units="in",width = 8, height = 11, res = 300)
  plot = visreg(promGAM,type="conditional",xvar ="int.sv.norm",line=list(col="black"),ps = 16, cex = 1, cex.main = 1, ylim =c(0,.5),
                main="Prominence",xlab="Local Intensity",ylab="P(Prominence)",scale="response")
  dev.off()
  
  #Max Local F0
  png(file = "maxF0Boundary.png", bg = "white",units="in",width = 8, height = 11, res = 300)
  plot = visreg(boundGAM,type="conditional",xvar ="log.f0.sv.max.norm",line=list(col="black"),ps = 16, cex = 1, cex.main = 1, ylim =c(0,.5),
                main="Boundary",xlab="Max Local f0",ylab="P(Boundary)",scale="response")
  dev.off()
  
  png(file = "maxF0Prominence.png", bg = "white",units="in",width = 8, height = 11, res = 300)
  plot = visreg(promGAM,type="conditional",xvar ="log.f0.sv.max.norm",line=list(col="black"),ps = 16, cex = 1, cex.main = 1, ylim =c(0,.5),
                main="Prominence",xlab="Max Local f0",ylab="P(Prominence)",scale="response")
  dev.off()
  
  #Word Frequency
  png(file = "WordFrequencyBoundary.png", bg = "white",units="in",width = 8, height = 11, res = 300)
  plot = visreg(boundGAM,type="conditional",xvar ="log.wordfreq.switchboard",line=list(col="black"),ps = 16, cex = 1, cex.main = 1, ylim =c(0,.5),
                main="Boundary",xlab="Word Frequency",ylab="P(Boundary)",scale="response")
  dev.off()
  
  png(file = "WordFrequencyProminence.png", bg = "white",units="in",width = 8, height = 11, res = 300)
  plot = visreg(promGAM,type="conditional",xvar ="log.wordfreq.switchboard",line=list(col="black"),ps = 16, cex = 1, cex.main = 1, ylim =c(0,.5),
                main="Prominence",xlab="Word Frequency",ylab="P(Prominence)",scale="response")
  dev.off()
  
  
  # Word Phone Rate
  png(file = "WordPhoneRateBoundary.png", bg = "white",units="in",width = 8, height = 11, res = 300)
  plot = visreg(boundGAM,type="conditional",xvar ="word.phonerate",line=list(col="black"),ps = 16, cex = 1, cex.main = 1, ylim =c(0,.5),
                main="Boundary",xlab="Word Phone Rate",ylab="P(Boundary)",scale="response")
  dev.off()
  
  png(file = "WordPhoneRatProminence.png", bg = "white",units="in",width = 8, height = 11, res = 300)
  plot = visreg(promGAM,type="conditional",xvar ="word.phonerate",line=list(col="black"),ps = 16, cex = 1, cex.main = 1, ylim =c(0,.5),
                main="Prominence",xlab="Word Phone Rate",ylab="P(Prominence)",scale="response")
  dev.off()
  
 #Post Pause Duration
  png(file = "ppdBoundary.png", bg = "white",units="in",width = 8, height = 11, res = 300)
  plot = visreg(boundGAM,type="conditional",xvar ="durpostpause",line=list(col="black"),ps = 16, cex = 1, cex.main = 1, ylim =c(0,1),
                main="Boundary",xlab="Post-Pause Duration",ylab="P(Boundary)",scale="response")
  dev.off()
  
  png(file = "ppdProminence.png", bg = "white",units="in",width = 8, height = 11, res = 300)
  plot = visreg(promGAM,type="conditional",xvar ="durpostpause",line=list(col="black"),ps = 16, cex = 1, cex.main = 1, ylim =c(0,.5),
                main="Prominence",xlab="Post-Pause Duration",ylab="P(Prominence)",scale="response")
  dev.off()
  
  
  #Boundary ARM
  png(file = "boundarymProminence.png", bg = "white",units="in",width = 8, height = 11, res = 300)
  plot = visreg(promGAM,type="conditional",xvar ="boundarym",line=list(col="black"),ps = 16, cex = 1, cex.main = 1, ylim =c(0,.5),
                main="Prominence",xlab="Marked for Boundary",ylab="P(Prominence)",scale="response", rug=FALSE, partial=FALSE)
  dev.off()
  
  #POS 
  png(file = "posBound.png", bg = "white",units="in",width = 8, height = 11, res = 300)
  plot = visreg(boundGAM,type="conditional",xvar ="pos",line=list(col="black"),ps = 12, cex = 1, cex.main = 1, ylim =c(0,.5),
                main="Boundary",xlab="Part of Speech",ylab="P(Boundary)",rug=FALSE, partial=FALSE,scale="response")
  dev.off()
  
  png(file = "posProm.png", bg = "white",units="in",width = 8, height = 11, res = 300)
  plot = visreg(promGAM,type="conditional",xvar ="pos",line=list(col="black"),ps = 12, cex = 1, cex.main = 1, ylim =c(0,.5),
                main="Prominence",xlab="Part of Speech",ylab="P(Prominence)",rug=FALSE, partial=FALSE,scale="response")
  dev.off()
  
}



makeBootKappa <- function (ratings, pType, idir){
  ksSE = seq(2:31)
  ksMean = seq(2:31)
  ksDF = data.frame(ksSE=ksSE,ksMean=ksMean)
  for (i in 2:31){
   stepk = bootRating(ratings,numSel = i, numRaters = 32, numSims = 10000, prosodicType=pType, idir=idir)  
   ksDF[i-1,1] = stepk[[2]] 
   ksDF[i-1,2] = stepk[[1]] 
  }
  setwd(idir)
  write.csv(ksDF,file="kappaBootStrap.csv")
}
