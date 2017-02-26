bootRating = function(ratings,numSel=2, numRaters=32,prosodicType,numSims=10000, idir="/data/Dropbox/current papers/Active/Cole-Mahrt-Roy/LabPhon special issue (ETAP)/new output/"){
  require(irr)
  require(ggplot2)
  cols = seq(1:numRaters)
  kfSamp = seq(1:numSims)
  for(i in 1:numSims){
    ids = sample(cols,size=numSel,replace=TRUE)
    raters = ratings[,ids]
    kfSamp[i] = kappam.fleiss(raters)$value     
  }
  df = data.frame(kappaS = kfSamp)
  
  ttl = paste0(prosodicType,"Number of Raters: ", numSel)
  
  p = ggplot(df, aes(x=kappaS)) + 
    geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                   binwidth=.05,
                   colour="black", fill="white") + ggtitle(ttl)+
    geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot
   
  setwd(idir)
  ftitle = paste0(prosodicType,"_",numSel,"_raters.png")
  png(file = ftitle, bg = "white",units="in",width = 8, height = 11, res = 600)
  print(p)
  dev.off()
  kfMean = mean(kfSamp)
  kfSE = sd(kfSamp)
  return(list(kfMean,kfSE))
}



plotGAMSSubject <- function(gam1, type, pDir= "/data/Dropbox/current papers/Active/Cole-Mahrt-Roy/LabPhon special issue (ETAP)/new output/", ovt=FALSE,subj=TRUE, condL=NULL){
  require(stringr)
  require(reshape2)
  formula = as.character(gam1$formula)[3]
  terms = strsplit(formula, "\\+")
  if(subj){
  t2 = grep("subject", terms[[1]],value=TRUE)
  outm=colsplit(string=t2, pattern=",", names=c("Subject", "term","fit","m"))
  outm2 = trimws(outm$term)
  setwd(pDir)
  
  
  for(i in 1:length(outm2)){ 
    title = paste0(outm2[i],type,"_bySubject.png")
    png(file = title, bg = "white",units="in",width = 8, height = 11, res = 600)

    plot = visreg(gam1,type="conditional",xvar = outm2[i], by="subject",line=list(col="black"),ps = 12, cex = 2,cex.lab=1.5, cex.main = 2, 
                  main=type,xlab=outm2[i], ylab=paste0("P(",type,")"),cond=condL, scale="response",overlay=ovt, legend=FALSE,partial=FALSE, rug=FALSE)
    dev.off()
  }
  }else{
    
    t1 = terms[[1]]
    t2 = t1[!grepl("subject", t1)]
    t2 = t2[!grepl("word,", t2)]
    outm2 = trimws(t2)
    setwd(pDir)
    outm2 = sub('s\\(', '', outm2)
    outm2 = sub('\\)', '', outm2)
    
    for(i in 1:length(outm2)){ 
      title = paste0(outm2[i],type,"_overall.png")
      png(file = title, bg = "white",units="in",width = 8, height = 11, res = 600)
     
      plot = visreg(gam1,type="conditional",xvar = outm2[i],line=list(col="black"),cond = condL,  ps = 16, cex = 2,cex.lab=1.5, cex.main = 2, 
                    main=type,xlab=outm2[i],ylab=paste0("P(",type,")"), scale="response",ylim = c(0,0.5),partial=FALSE, rug=FALSE)
      dev.off()
  }
 }
}


