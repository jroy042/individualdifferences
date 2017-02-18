readRPT = function(dirR="C:/Users/jroy042/Box Sync/projects/Jennifer Cole/RPT data/data/", cutT=FALSE, refPOS="Noun"){
# December 17 2016: Verified and reconfigured for time analysis by subject.
#setwd("C:/Users/joe/Dropbox/current papers/Active/Cole-Mahrt-Roy/time analysis/data")
## ALL THE LIBRARIES NEEDED
require(lme4)
require(ggplot2)
require(plyr)
require(binomTools)
require(reshape2)
require(mgcv)
require(gridExtra)
require(stargazer)
require(visreg)
#require(BiodiversityR) 
require(reshape2)
##Read in the data files
#create long files (for overall modeling)
#dirR = directory with data files in it. 
cur = getwd()
setwd(dirR)

##READ THE DATA IN####
##LAB US ### 
labus=read.csv("labusOmni.csv",header=TRUE, na.strings=c("NA","NaN", "--undefined--"))
timeus = read.csv("lab_us_timing.csv",header=TRUE,na.strings=c("NA","NaN", "--undefined--"))
labus = cbind(labus,timeus)
labusn =labus[labus$contfunc=="Content",,drop=TRUE]
labusn$pos = factor(labusn$pos)
labusn$norm.wp = scale(labusn$word.phonerate)
labusn$norm.wp = as.numeric(labusn$norm.wp)

### Turkers US
mtus=read.csv("mtusOmni.csv",header=TRUE,na.strings=c("NA","NaN", "--undefined--"))
timesd = read.csv("mt_us_timing.csv",header=TRUE,na.strings=c("NA","NaN", "--undefined--"))
mtus = cbind(mtus,timesd)
mtusn = mtus[mtus$contfunc=="Content",,drop=TRUE]
 mtusn$norm.wp = scale(mtusn$word.phonerate)
 mtusn$norm.wp = as.numeric(mtusn$norm.wp)

##Tukers Indian
mtin=read.csv("mtinOmni.csv",header=TRUE,na.strings=c("NA","NaN", "--undefined--"))
timesd = read.csv("mt_in_timing.csv",header=TRUE,na.strings=c("NA","NaN", "--undefined--"))
mtin = cbind(mtin,timesd)
mtinn = mtin[mtin$contfunc=="Content",,drop=TRUE]
 mtinn$norm.wp = scale(mtinn$word.phonerate)
 mtinn$norm.wp = as.numeric(mtinn$norm.wp)

###MAKE THE DATA LONG###

######################LAB US #############
##Bound##
labuslong = reshape(labusn,varying=list(1:33,136:168),v.names=c("rating","tsl"),times=1:33,direction="long")
labuslong$subject = paste0("LabUS",labuslong$time)

keeps = c("rating","tsl", "subject","norm.wp","pos","int.sv.norm","word.phonerate","log.f0.sv.max.norm", "log.wordfreq.switchboard", "filename","durword","durstress",
         "dursv",	"durphonword",	"durprepause",	"durpostpause","word","token","filename")
labuslong = labuslong[,keeps,drop=FALSE]
levels(labuslong$pos) = c("Noun", "Adjective","Adverb","Adverb","Verb","Verb")

  labuslong$pos = relevel(labuslong$pos, ref = "Noun")

##Prom##

labuslong2 = reshape(labusn,varying=list(34:66,169:201),v.names=c("rating","tsl"),times=1:33,direction="long")
labuslong2$subject=paste0("LabUS",labuslong2$time)
levels(labuslong2$pos) = c("Noun", "Adjective","Adverb","Adverb","Verb","Verb")
  labuslong2$pos = relevel(labuslong2$pos, ref = "Noun")

  #keeps = c("rating","tsl", "subject","norm.wp","pos","int.sv.norm","log.f0.sv.max.norm", "log.wordfreq.buckeye", "filename","durword","durstress",
   #         "dursv",	"durphonword",	"durprepause",	"durpostpause")
  labuslong2 = labuslong2[,keeps,drop=FALSE]


#Extract ToBI  
  
toBIbound = labuslong[labuslong$subject == "LabUS33",]
toBIProm = labuslong2[labuslong2$subject == "LabUS33",]
labuslong = labuslong[labuslong$subject != "LabUS33",]
labuslong2 = labuslong2[labuslong2$subject != "LabUS33",]

toBIbound$group = "toBI"
toBIProm$group = "toBI"
toBIProm$boundarym = toBIbound$rating
###DV all possible regressions#####
depusba = names(labusn[,1:32])
depuspa =names(labusn[,33:64])


###############MT US###########

##Bound##
mtuslong = reshape(mtusn,varying=list(1:32,134:165),v.names=c("rating","tsl"),times=1:32,direction="long")
mtuslong$subject= paste0("MTUS",mtuslong$time)
mtuslong$pos = factor(mtuslong$pos)

levels(mtuslong$pos) = c("Noun", "Adjective","Adverb","Adverb","Verb","Verb")

  mtuslong$pos = relevel(mtuslong$pos, ref = "Noun")

 # keeps = c("rating","tsl", "subject","norm.wp","pos","int.sv.norm","log.f0.sv.max.norm", "log.wordfreq.buckeye", "filename","durword","durstress",
  #          "dursv",	"durphonword",	"durprepause",	"durpostpause")
  mtuslong = mtuslong[,keeps,drop=FALSE]

##Prom##

mtuslong2 = reshape(mtusn,varying=list(33:64,166:197),v.names=c("rating","tsl"),times=1:32,direction="long")
mtuslong2$subject = paste0("MTUS",mtuslong2$time)
mtuslong2$pos = factor(mtuslong2$pos)
levels(mtuslong2$pos) = c("Noun", "Adjective","Adverb","Adverb","Verb","Verb")

mtuslong2$pos = relevel(mtuslong2$pos, ref = "Noun")

#keeps = c("rating","tsl", "subject","norm.wp","pos","int.sv.norm","log.f0.sv.max.norm", "log.wordfreq.buckeye", "filename","durword","durstress",
 #         "dursv",	"durphonword",	"durprepause",	"durpostpause")

mtuslong2 = mtuslong2[,keeps,drop=FALSE]

###DV all possible regressions#####
depmtusba = names(mtusn[,1:32])
depmtuspa = names(mtusn[,33:64])

##############MT IN ####################

mtinlong = reshape(mtinn,varying=list(1:32,134:165),v.names=c("rating","tsl"),times=1:32,direction="long")
mtinlong$subject= paste0("MTIN",mtinlong$time)
mtinlong$pos = factor(mtinlong$pos)


mtinlong = mtinlong[,keeps,drop=FALSE]
levels(mtinlong$pos) = c("Noun", "Adjective","Adverb","Adverb","Verb","Verb")

  mtinlong$pos = relevel(mtinlong$pos, ref = "Noun")

##Prom##

mtinlong2 = reshape(mtinn,varying=list(33:64,166:197),v.names=c("rating","tsl"),times=1:32,direction="long")
mtinlong2$subject= paste0("MTIN",mtinlong2$time)
mtinlong2$pos = factor(mtinlong2$pos)

levels(mtinlong2$pos) = c("Noun", "Adjective","Adverb","Adverb","Verb","Verb")

  mtinlong2$pos = relevel(mtinlong2$pos, ref = "Noun")

#  keeps = c("rating","tsl", "subject","norm.wp","pos","int.sv.norm","log.f0.sv.max.norm", "log.wordfreq.buckeye", "filename","durword","durstress",
 #           "dursv",	"durphonword",	"durprepause",	"durpostpause")
  mtinlong2 = mtinlong2[,keeps,drop=FALSE]

###DV all possible regressions#####
depmtinba= names(mtinn[,1:32])
depmtinpa = names(mtinn[,33:64])

setwd(cur)



labuslong$pos = relevel(labuslong$pos, ref = refPOS)
labuslong2$pos = relevel(labuslong2$pos, ref = refPOS)
mtuslong$pos = relevel(mtuslong$pos, ref = refPOS)
mtuslong2$pos = relevel(mtuslong2$pos, ref = refPOS)
mtinlong$pos = relevel(mtinlong$pos, ref = refPOS)
mtinlong2$pos = relevel(mtinlong2$pos, ref = refPOS)

labuslong$rating = as.factor(labuslong$rating)
labuslong2$rating = as.factor(labuslong2$rating)
labuslong$subject = as.factor(labuslong$subject)
labuslong2$subject = as.factor(labuslong2$subject)
labuslong2$boundarym = labuslong$rating


mtuslong$rating = as.factor(mtuslong$rating)
mtuslong2$rating = as.factor(mtuslong2$rating)
mtuslong$subject = as.factor(mtuslong$subject)
mtuslong2$subject = as.factor(mtuslong2$subject)
mtuslong2$boundarym = mtuslong$rating


mtinlong$rating = as.factor(mtinlong$rating)
mtinlong2$rating = as.factor(mtinlong2$rating)
mtinlong$subject = as.factor(mtinlong$subject)
mtinlong2$subject = as.factor(mtinlong2$subject)
mtinlong2$boundarym = mtinlong$rating

#scaling all 
#s(word.phonerate)  + pos +  s(log.f0.sv.max.norm) + s(int.sv.norm) + 
# s(log.wordfreq.switchboard)+ s(durpostpause)

labuslong$word.phonerate = scale(labuslong$word.phonerate)
labuslong$log.f0.sv.max.norm = scale(labuslong$log.f0.sv.max.norm)
labuslong$int.sv.norm = scale(labuslong$int.sv.norm)
labuslong$log.wordfreq.switchboard = scale(labuslong$log.wordfreq.switchboard)
labuslong$durpostpause = scale(labuslong$durpostpause)

labuslong2$word.phonerate = scale(labuslong2$word.phonerate)
labuslong2$log.f0.sv.max.norm = scale(labuslong2$log.f0.sv.max.norm)
labuslong2$int.sv.norm = scale(labuslong2$int.sv.norm)
labuslong2$log.wordfreq.switchboard = scale(labuslong2$log.wordfreq.switchboard)
labuslong2$durpostpause = scale(labuslong2$durpostpause)



##CREATE GROUP VARIABLES AND OMNI SEt
mtinlong$group = "MT_Indian"
mtinlong2$group = "MT_Indian"
mtuslong$group= "MT_US"
mtuslong2$group = "MT_US"
labuslong$group = "Lab_US"
labuslong2$group = "Lab_US"
if(cutT){
  labuslong = subset(labuslong,tsl<3.0)
  labuslong2 = subset(labuslong2,tsl<3.0)
  
  
  mtuslong = subset(mtuslong,tsl<3.0)
  
  mtuslong2 = subset(mtuslong2,tsl<3.0)
  
  mtinlong = subset(mtinlong,tsl<3.0)
  mtinlong2 = subset(mtinlong2,tsl<3.0)
  
  
}
boundDat = rbind(mtuslong,mtinlong,labuslong,toBIbound)
promDat = rbind(mtuslong2,mtinlong2,labuslong2,toBIProm)




retList = list(promDat=promDat,boundDat=boundDat,labusn=labusn,mtusn=mtusn,mtinn=mtinn,labuslong=labuslong,labuslong2=labuslong2,mtuslong=mtuslong,mtuslong2=mtuslong2,mtinlong=mtinlong,mtinlong2=mtinlong2,depusba=depusba,depuspa=depuspa,depmtusba=depmtusba,depmtuspa=depmtuspa,depmtinba=depmtinba,depmtinpa=depmtinpa)
return(retList)


}

