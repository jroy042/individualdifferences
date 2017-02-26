bootRating = function(ratings,numSel=2, numRaters=32,numSims=10000, idir="/data/Dropbox/current papers/Active/Cole-Mahrt-Roy/LabPhon special issue (ETAP)/new output/"){
  require(irr)
  require(ggplot2)
  cols = sequence(1:numRaters)
  kfSamp = seq(1:numSims)
  for(i in 1:numSims){
    ids = sample(cols,size=numSel,replace=TRUE)
    raters = ratings[,ids]
    kfSamp[i] = kappam.fleiss(raters)$value     
  }
  df = data.frame(kappaS = kfSamp)
  
  ttl = paste0("Number of Raters: ", numSel)
  
  p = ggplot(df, aes(x=kappaS)) + 
    geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                   binwidth=.05,
                   colour="black", fill="white") +
    geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot
  setwd(idir)
  ftitle = paste0(numSel,"_raters.png")
  png(file = ftitle, bg = "white",units="in",width = 8, height = 11, res = 600)
  print(p)
  dev.off()
  kfMean = mean(kfSamp)
  kfSE = sd(kfSamp)
  return(list(kfMean,kfSE))
}

ratings = as.matrix(labusn[,1:32])


outk = bootRating(ratings,numSel = 2, numSims=1000, idir="C:/Users/Joe/Desktop")
