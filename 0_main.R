
setwd("/data/Dropbox/current papers/Active/Cole-Mahrt-Roy/LabPhon special issue (ETAP)/github/individualdifferences/")

source("1_readRPT.r")
source("analysis.R")
source("supportFuns.R")
#makeGam()


makeETAPOverall()

# #Read in the data and required libraries
# totList = readRPT(dirR="/data/Dropbox/current papers/Active/Cole-Mahrt-Roy/Cole-Mahrt-Roy prosody modeling/data")
# #totList = readRPT(dirR="/data/Dropbox/current papers/Active/Lab Phonology/Cole-Mahrt-Roy prosody modeling/data",cutT=FALSE)
# 
# 
# #post the data frames to main environ ment
# list2env(totList ,.GlobalEnv)
# 


# setwd("/home/joe/Desktop/Prosody Collaboration/")
# save(boundDat,promDat,file="prosData.RData")
# #FUNCTIONS
# source("2_IndivRegress.R")
# 
# source("plotallR.r")
# #PRODUCES OUTPUT @@@
# source("2_gamPlot.R")
# 
# 
# #Run individual regressions by Cohort
# 
# labus.ba.res = IndivRegress(depusba,labusn)
# labus.pa.res = IndivRegress(depuspa,labusn)
# 
# mtus.ba.res = IndivRegress(depmtusba,mtusn)
# mtus.pa.res = IndivRegress(depmtuspa,mtusn)
# 
# mtin.ba.res = IndivRegress(depmtinba,mtinn)
# mtin.pa.res = IndivRegress(depmtinpa,mtinn)
# #
# 
# 
# labusn$pos = relevel(labusn$pos, ref = "Noun")
# glm1 = glm(boundary ~  pos + log.wordfreq.buckeye+norm.wp + int.sv.norm*norm.wp + log.f0.sv.max.norm*norm.wp,family="binomial",  data=labusn)
# glm2 = glm(accented ~  pos + log.wordfreq.buckeye+norm.wp + int.sv.norm*norm.wp + log.f0.sv.max.norm*norm.wp,family="binomial",  data=labusn)
# 
# 
# setwd("C:/Users/jroy042/Box Sync/projects/Jennifer Cole/RPT data/Results/individuals")
# 
# write.csv(labus.ba.res,"labus.ba.res.csv")
# write.csv(labus.pa.res,"labus.pa.res.csv")
# 
# write.csv(mtus.ba.res,"mtus.ba.res.csv") 
# write.csv(mtus.pa.res,"mtus.pa.res.csv") 
# 
# write.csv(mtin.ba.res,"mtin.ba.res.csv")
# write.csv(mtin.pa.res,"mtin.pa.res.csv")
# 
# resultsba=labus.ba.res
# regPlot(resultsba,tobii=glm1,plotFile="labusb",titleP="Lab Boundary")
# 
# resultsba=labus.pa.res
# regPlot(resultsba,tobii=glm2,plotFile="labusp",titleP="Lab Prominence")
# instal
# resultsba=mtus.ba.res
# regPlot(resultsba,tobii=glm1,plotFile="mtusb",titleP="US Mechanical Turk Boundary")
# resultsba=mtus.pa.res
# regPlot(resultsba,tobii=glm2,plotFile="mtusp",titleP="US Mechanical Turk Prominence")
# 
# resultsba=mtin.ba.res
# regPlot(resultsba,tobii=glm1,plotFile="mtinb",titleP="India Mechanical Turk Boundary")
# resultsba=mtin.pa.res
# regPlot(resultsba,tobii=glm2,plotFile="mtinp",titleP="India Mechanical Turk Prominence")
# 
# 
# 
# ##Plot everything## 
# 
# 
# #Prominence and boundary#
# 
# 
# 
# glme1b = glmer(rating ~ poly(tsl,3) + norm.wp + int.sv.norm*norm.wp + log.f0.sv.max.norm*norm.wp +log.wordfreq.buckeye+ (1+norm.wp + int.sv.norm*norm.wp + log.f0.sv.max.norm*norm.wp +log.wordfreq.buckeye|subject),family="binomial", data=labuslong)
# 
# glme1p = glmer(rating ~ poly(tsl,3) + norm.wp + int.sv.norm*norm.wp + log.f0.sv.max.norm*norm.wp +log.wordfreq.buckeye+ (1|subject),family="binomial", data=labuslong2)
# 
# 
# ###### Tobii Prom and Boundary
# 
# 
# glm3 = glm(boundary ~  pos +norm.wp + int.sv.norm*norm.wp + log.f0.sv.max.norm*norm.wp,family="binomial",  data=mtusn)
# glm4 = glm(accented ~ pos + norm.wp + int.sv.norm*norm.wp + log.f0.sv.max.norm*norm.wp,family="binomial",  data=mtusn)
# 
# glm5 = glm(boundary ~  pos +norm.wp + int.sv.norm*norm.wp + log.f0.sv.max.norm*norm.wp,family="binomial",  data=mtinn)
# glm6 = glm(accented ~  pos +norm.wp + int.sv.norm*norm.wp + log.f0.sv.max.norm*norm.wp,family="binomial",  data=mtinn)
# 
# stargazer(glm1,glm2)
# 
# ########################### CORELATIONS, Etc. 
# 
# library(ggplot2)
# library(reshape2)
# 
# myvars = c("norm.wp","int.sv.norm","log.f0.sv.max.norm")
# sub.lab.ba = labus.ba.res[myvars]
# sub.lab.pa =labus.pa.res[myvars]
# cor1 = cor(sub.lab.ba)
# cor2 = cor(sub.lab.pa)
# 
# qplot(x=Var1, y=Var2, data=melt(cor1), fill=value, geom="tile")
# qplot(x=Var1, y=Var2, data=melt(cor2),fill=value, geom="tile")
# 
# devexp = read.csv(file.choose(),header=TRUE)
# 
# png(file = "devExp.png", bg = "transparent",units="in",width = 4.65, height = 4, res = 72)
# ggplot(devexp, aes(x=Prominence,y=Boundary)) + 
#   geom_point() + xlab("Prominence by Individual") +
#   ylab("Boundary by Individual")+ theme_bw()
# dev.off()
# 
# source("plotallR.r")
# resultsba=labus.ba.res
# regPlot(resultsba,tobii=glm1,plotFile="pub_labusb",titleP="Boundary")
# 
# resultsba=labus.pa.res
# regPlot(resultsba,tobii=glm2,plotFile="pub_labusp",titleP="Prominence")
# 
# #1:33,136:168cit
# kappam.fleiss(labusn[,1:32])
# kappam.fleiss(labusn[,136:167])

   