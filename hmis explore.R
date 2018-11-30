#bh1<-read.csv('C:/Users/dmw63/Desktop/My documents h/GATES/india/hmis/Bihar series by district.csv')
bh1<-read.csv('C:/Users/dmw63/Desktop/My documents h/GATES/india/hmis/hmis/Uttar Pradesh series by district.csv')

str(bh1)
bh1$uri<-bh1$X_10_13
bh1$pneu_death<-bh1$X_16_3_1
bh1$diar_death<-bh1$X_16_3_2
bh1$measles_death<-bh1$X_16_3_4
bh1$asphyxia_death<-bh1$X_16_2_2
bh1$sepsis_death<-bh1$X_16_2_1
bh1$neonatal_death<-bh1$X_16_1
bh1$year<-substr(bh1$Date,2,5)
bh1$month<-substr(bh1$Date,6,7)
bh1$monthdate<-as.Date(paste(bh1$year,bh1$month,'01', sep='-'))

unique(bh1$District)

strat1<-factor(bh1$monthdate)
ds.sub<-bh1[,c('uri', 'diar_death', 'pneu_death','sepsis_death','asphyxia_death', 'measles_death', 'neonatal_death')]
bh2<-aggregate(x=ds.sub, by=list( strat1) , FUN='sum', na.rm=TRUE)
names(bh2)<-c('monthdate',names(ds.sub))
bh2$monthdate<-as.Date(bh2$monthdate)
bh2$neonatal_death[nrow(bh2)]<-NA
par(mfrow=c(3,2), mar=c(1,3,1,1))
plot(bh2$monthdate,bh2$diar_death, type='l', bty='l')
plot(bh2$monthdate,bh2$pneu_death, type='l', bty='l')
plot(bh2$monthdate,bh2$sepsis_death, type='l', bty='l')
plot(bh2$monthdate,bh2$asphyxia_death, type='l', bty='l')
plot(bh2$monthdate,bh2$measles_death, type='l', bty='l')
plot(bh2$monthdate, bh2$neonatal_death, type='l', bty='l')

plot(bh2$uri, type='l')


#heatmap of reporting of URI
library(reshape2)
ds.sub<-bh1[,c('District','monthdate','uri')]
ds.m<-melt(ds.sub, id=c('District','monthdate'))
ds.c<-dcast(ds.m, monthdate~District)
par(mfrow=c(1,1), mar=c(1,1,1,1))
hm1<-heatmap(t(as.matrix(ds.c[,-1])), scale='row', Rowv=NA, Colv=NA)