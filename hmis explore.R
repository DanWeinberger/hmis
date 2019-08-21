#PCV was launched by the Union Health Minister, Shri JP Nadda on May 13, 2017 at Mandi,
#Himachal Pradesh [1]. With this phased introduction, nearly 2.1 million children in 
#Himachal Pradesh (all 12 districts), parts of Bihar (17 out of 38 districts) 
#and Uttar Pradesh (6 out of 75 districts) will be vaccinated with PCV in the first year [2]. 
#This will be followed by introduction in Madhya Pradesh and Rajasthan next year, and eventually coverage will be expanded across the entire country in a phased manner, in the coming years. " 
#In uttar Pradesh it is: Lakhimpur Kheri, Sitapur, Siddharth Nagar, Bahraich, Balrampur, Shrawasti;
#In Bihar:  The 17 high-priority districts are  Araria, Begusarai, Darbhanga, Kishanganj, Khagaria, Katihar, 
#Muzaffarpur, Munger, Vaishali, Madhepura, Madhubani, Purnea, Samastipur, Saran, Sitamarhi, Sheohar and Supaul
#bh1<-read.csv('C:/Users/dmw63/Desktop/My documents h/GATES/india/hmis/Bihar series by district.csv')
library(lme4)
bh1<-read.csv('C:/Users/dmw63/Desktop/My documents h/GATES/india/hmis/hmis/uttarPradesh series to Mar 2019.csv')

str(bh1)
bh1$month<-as.numeric(substr(bh1$DATE,6,7))
bh1$year<-as.numeric(substr(bh1$DATE,2,5))
bh1$uri<-bh1$X_10_13
bh1$pneu_death<-bh1$X_16_3_1
bh1$diar_death<-bh1$X_16_3_2
bh1$measles_death<-bh1$X_16_3_4
bh1$asphyxia_death<-bh1$X_16_2_2
bh1$sepsis_death<-bh1$X_16_2_1
bh1$neonatal_death<-bh1$X_16_1
bh1$monthdate<-as.Date(paste(bh1$year,bh1$month,'01', sep='-'))
up.intro.districts<-c('Lakhimpur Kheri', 'Sitapur', 'Siddharth Nagar', 'Bahraich', 'Balrampur', 'Shrawasti')
bh1$pcv.status<-0
bh1$pcv.status[bh1$DISTRICT %in% up.intro.districts] <-1

unique(bh1$DISTRICT)

strat1<-factor(bh1$monthdate)
ds.sub<-bh1[,c('uri', 'diar_death', 'pneu_death','sepsis_death','asphyxia_death', 'measles_death', 'neonatal_death')]
bh2<-aggregate(x=ds.sub, by=list( strat1) , FUN='sum', na.rm=TRUE)
names(bh2)<-c('monthdate',names(ds.sub))
bh2$monthdate<-as.Date(bh2$monthdate)
bh2$neonatal_death[nrow(bh2)]<-NA
par(mfrow=c(3,2), mar=c(3,3,1,1))
plot(bh2$monthdate,bh2$pneu_death,main='Pneumonia deaths', type='l', bty='l')
plot(bh2$monthdate,bh2$diar_death,main='Diarrhea deaths', type='l', bty='l')
plot(bh2$monthdate,bh2$sepsis_death,main='Sepsis Deaths', type='l', bty='l')
plot(bh2$monthdate,bh2$asphyxia_death,main='Asphyxia Deaths', type='l', bty='l')
plot(bh2$monthdate,bh2$measles_death,main='Measles Deaths', type='l', bty='l')
plot(bh2$monthdate, bh2$neonatal_death, main='Neonatal Deaths',type='l', bty='l')

par(mfrow=c(1,1), mar=c(2,3,1,1))
plot(bh2$uri, type='l',bty='l', main='URI cases')

#heatmap of reporting of URI
#seems to be incomplete before April 2017
library(reshape2)
ds.sub<-bh1[,c('DISTRICT','monthdate','uri')]
ds.m<-melt(ds.sub, id=c('DISTRICT','monthdate'))
ds.c<-dcast(ds.m, monthdate~DISTRICT)
par(mfrow=c(1,1), mar=c(1,1,1,1))
hm1<-heatmap(t(as.matrix(ds.c[,-1])), scale='row', Rowv=NA, Colv=NA,cexRow =0.5)
hm1<-heatmap(t(as.matrix(ds.c[ds.c$monthdate>=as.Date('2017-04-01'),-1])), scale='row', Rowv=NA, Colv=NA,cexRow =0.5)
hm1<-heatmap(t(as.matrix(ds.c[ds.c$monthdate>=as.Date('2017-04-01'),-1])), scale='row',  Colv=NA,cexRow =0.5)


#Pneumonia deaths--seems to be incomplete before April 2017
ds.sub<-bh1[,c('DISTRICT','monthdate','pneu_death')]
ds.m<-melt(ds.sub, id=c('DISTRICT','monthdate'))
ds.c<-dcast(ds.m, monthdate~DISTRICT)
par(mfrow=c(1,1), mar=c(1,1,1,1))
hm1<-heatmap(t(as.matrix(ds.c[,-1])), scale='row', Rowv=NA, Colv=NA,cexRow =0.5)
hm1<-heatmap(t(as.matrix(ds.c[ds.c$monthdate>=as.Date('2017-04-01'),-1])), scale='row', Rowv=NA, Colv=NA,cexRow =0.5)
hm1<-heatmap(t(as.matrix(ds.c[ds.c$monthdate>=as.Date('2017-04-01'),-1])), scale='row',  Colv=NA,cexRow =0.5)


##Simple model#
#Consider Apr 2018-Sep 2018 as roll out perios and Oct 2018-Mar 2019 as eval period
bh3<-bh1[bh1$monthdate>=as.Date('2017-04-01'),]
bh3$post.pcv<-0
bh3$post.pcv[bh3$pcv.status==1 & bh3$monthdate>=as.Date('2018-04-01')& bh3$monthdate<=as.Date('2018-09-01')] <- 1
bh3$post.pcv[bh3$pcv.status==1 & bh3$monthdate>=as.Date('2018-10-01')& bh3$monthdate<=as.Date('2019-03-01')] <- 2
bh3$post.pcv<-as.factor(bh3$post.pcv)
bh3$date.factor<-as.factor(bh3$monthdate)
bh3$obs<-as.factor(1:nrow(bh3))
mod1<-glmer(pneu_death ~ (1|date.factor) + (1|DISTRICT) + (1|obs)+ post.pcv, data=bh3, family='poisson' )
summary(mod1)

mod1<-glmer(uri ~  (1|DISTRICT) + (1|obs)+ post.pcv, data=bh3, family='poisson' )
summary(mod1)

#Gamm with time smooth
library(mgcv)
bh3$date.cont<-bh3$year+ bh3$month/12-1/12
mod2<-gamm(uri ~   s(date.cont) + post.pcv, data=bh3, family='poisson' ,random=list(DISTRICT=~1))
summary(mod2$lme)

#mod2<-gamm(pneu_death ~   s(date.cont) + post.pcv, data=bh3, family='poisson' ,random=list(DISTRICT=~1), niterPQL=500)
#summary(mod2$lme)

#statewide
ds.c.state<-dcast(ds.m, monthdate~1, fun.aggregate = sum)
par(mar=c(3,2,1,1))
plot(ds.c.state[ds.c.state$monthdate>=as.Date('2017-04-01'),1],ds.c.state[ds.c.state$monthdate>=as.Date('2017-04-01'),2], type='l')

#Brian: analyze by region vs district
#Need a crosswalk file. Western/Central/Eastern in UP; district-> division
#District hospitals--probably kids coming from district; bigger hospitals, kids coming from other places
# Mortality data: is location based on residence or place of death?
#--does death registry include out of hospital deaths
#There is an official death registry
