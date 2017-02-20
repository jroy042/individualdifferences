
setwd("/data/Dropbox/current papers/Active/Cole-Mahrt-Roy/LabPhon special issue (ETAP)/github/individualdifferences/")

source("1_readRPT.r")

totList = readRPT(dirR="/data/Dropbox/current papers/Active/Cole-Mahrt-Roy/Cole-Mahrt-Roy prosody modeling/data")
list2env(totList ,.GlobalEnv)

setwd("/data/Dropbox/current papers/Active/Cole-Mahrt-Roy/LabPhon special issue (ETAP)/new output/")


bscore = rowSums(labusn[,1:32])/32
pscore = rowSums(labusn[,34:63])/32


desDat = data.frame(pscore=pscore, bscore=bscore, pos=labusn$pos, WordPhonerate=labusn$word.phonerate,intensity=labusn$int.sv.norm,pitch=labusn$log.f0.sv.max.norm, postPauseDur=labusn$durpostpause, frequency=labusn$log.wordfreq.buckeye)

# Intensity 
# Max F0
# Part of Speech
# Word Frequency
# Word Phonerate
# Post-pause Duration

plot1a = ggplot(desDat, aes(x=bscore)) +
  geom_bar() + theme_bw() + ggtitle("Boundary Scores")

plot2a= ggplot(desDat, aes(x=pscore)) +
  geom_bar() + theme_bw() + ggtitle("Prominence Scores")

png(file="histograms.png", bg = "white",units="in",width = 10, height = 7.5, res = 72)
grid.arrange(plot1a,plot2a, ncol=2, top="Histograms of Prosodic Scores")
dev.off()
# 
# plot1 = ggplot(desDat, aes(y=bscore, x=intensity)) +
#   geom_point() + stat_smooth(method="lm", colour="black") + theme_bw()
# 
# plot2= ggplot(desDat, aes(y=bscore, x=pitch)) +
#   geom_point() + stat_smooth(method="lm", colour="black") + xlab("max F0")+ theme_bw() + ylim(0,1)
# 
# plot3=ggplot(desDat, aes(y=bscore, x=frequency)) +
#   geom_point() + stat_smooth(method="lm", colour="black") + xlab("frequency") + theme_bw()+ ylim(0,1)
# 
# plot4 = ggplot(desDat, aes(y=bscore, x=WordPhonerate)) +
#   geom_point() + stat_smooth(method="lm", colour="black") + xlab("word phonerate") + theme_bw()+ ylim(0,1)
# 
# plot5 = ggplot(desDat, aes(y=bscore, x=postPauseDur)) +
#   geom_point() + stat_smooth(method="lm", colour="black") + xlab("post-pause duration") + theme_bw()+ ylim(0,1)
# 
# 
# plot6 = ggplot(desDat, aes(y=pscore, x=intensity)) +
#   geom_point() + stat_smooth(method="lm", colour="black") + theme_bw()+ ylim(0,1)
# 
# plot7= ggplot(desDat, aes(y=pscore, x=pitch)) +
#   geom_point() + stat_smooth(method="lm", colour="black") + xlab("max F0")+ theme_bw()+ ylim(0,1)
# 
# plot8=ggplot(desDat, aes(y=pscore, x=frequency)) +
#   geom_point() + stat_smooth(method="lm", colour="black") + xlab("frequency") + theme_bw()+ ylim(0,1)
# 
# plot9 = ggplot(desDat, aes(y=pscore, x=WordPhonerate)) +
#   geom_point() + stat_smooth(method="lm", colour="black") + xlab("word phonerate") + theme_bw()+ ylim(0,1)
# 
# plot10 = ggplot(desDat, aes(y=pscore, x=postPauseDur)) +
#   geom_point() + stat_smooth(method="lm", colour="black") + xlab("post-pause duration") + theme_bw()+ ylim(0,1)

plot1 = ggplot(desDat, aes(y=bscore, x=intensity)) +
  geom_violin() + theme_bw()

plot2= ggplot(desDat, aes(y=bscore, x=pitch)) +
  geom_violin() + xlab("max F0")+ theme_bw() + ylim(0,1)

plot3=ggplot(desDat, aes(y=bscore, x=frequency)) +
  geom_violin() + xlab("frequency") + theme_bw()+ ylim(0,1)

plot4 = ggplot(desDat, aes(y=bscore, x=WordPhonerate)) +
  geom_violin() +xlab("word phonerate") + theme_bw()+ ylim(0,1)

plot5 = ggplot(desDat, aes(y=bscore, x=postPauseDur)) +
  geom_violin() + xlab("post-pause duration") + theme_bw()+ ylim(0,1)


plot6 = ggplot(desDat, aes(y=pscore, x=intensity)) +
  geom_violin() + theme_bw()+ ylim(0,1)

plot7= ggplot(desDat, aes(y=pscore, x=pitch)) +
  geom_violin() + xlab("max F0")+ theme_bw()+ ylim(0,1)

plot8=ggplot(desDat, aes(y=pscore, x=frequency)) +
  geom_violin() + xlab("frequency") + theme_bw()+ ylim(0,1)

plot9 = ggplot(desDat, aes(y=pscore, x=WordPhonerate)) +
  geom_violin() + xlab("word phonerate") + theme_bw()+ ylim(0,1)

plot10 = ggplot(desDat, aes(y=pscore, x=postPauseDur)) +
  geom_violin() + xlab("post-pause duration") + theme_bw()+ ylim(0,1)


png(file="descriptive.png", bg = "white",units="in",width = 10, height = 7.5, res = 72)
grid.arrange(plot1,plot6,plot2,plot7,plot3,plot8,plot4,plot9,plot5,plot10, ncol=2, top="Descriptive Distribution of Acoustic and Frequency Cues by Prosodic Score")
dev.off()


#POS PLOTS ####

desDat$pos = factor(desDat$pos)

levels(desDat$pos) = c("Adjective", "Adverb","Adverb","Noun","Verb","Verb")

plot10 = ggplot(desDat, aes(x=factor(pos),y=bscore)) + geom_violin()+ theme_bw() + xlab("")

plot11 = ggplot(desDat, aes(x=factor(pos),y=pscore)) + geom_violin()+ theme_bw()+ xlab("")

png(file="descriptivePOS.png", bg = "transparent",units="in",width = 10, height = 6, res = 72)
grid.arrange(plot10,plot11, ncol=2, top="Descriptive Distribution of Part of Speech by Prosodic Score")
dev.off()
