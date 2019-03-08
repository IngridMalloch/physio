rm(list=ls())
setwd("~/Desktop/physio/advice")
setwd("~/Desktop/physio/advice/numbered")
setwd("~/Desktop/physio/advice/advice questionniare data")

##########packages loading##########
library(psych)
library(car)

library(dplyr)
library(tidyr)
library(reshape2)
library(data.table)
library(lme4)
library(emmeans)
library(ggplot2)

library(lavaan)
library(nlme)
library(lsr)

if(!require(nlme)){install.packages("nlme")}
if(!require(multcomp)){install.packages("multcomp")}
if(!require(multcompView)){install.packages("multcompView")}

if(!require(lmerTest)){install.packages("lmerTest")}
if(!require(TukeyC)){install.packages("TukeyC")}

#this is pre-liminary analysis data for ICA abstract,ID 1- 24
load('advice-pre-ica.Rda')
save(list=c('d','d1','d2','d3','d4',
            'edabind','heartlong','cibind','oobind'),file='advice-pre-ica.Rda')
write.csv(d,file='advice-pre-ica.csv')

#this is full ICA data
load('advice-full.Rda')
save(list=c('d','d1','d2','d3','d4','text','message','d_physio'
            ,'person'),file='advice-full.Rda')
#d: contains physio,liwc, message rating, personality and demographics
#d1 - d4: physio only in four conditions
#text: Liwc results only
#message: compiled message rating of attribution, emotion and identification
#d_physio: contains physio and liwc
#peson: personality data, with demographics

#this is physio with liwc results csv
d=read.csv('advice-physio-with-text.csv',header=T)

###export d, with physio, liwc, personality, message raing and demo
write.csv(d,'advice-full-physio-liwc-personality-messagerating-demo.csv')


###get rid of duplicated cases
dunique=read.csv('advice-full-physio-liwc-personality-messagerating-demo.csv',header = T)
d=dunique
#####transfer time to factor######
edabind$time=as.factor(edabind$time)
heartlong$time=as.factor(heartlong$time)
cibind$time=as.factor(cibind$time)
oobind$time=as.factor(oobind$time)

###split self-report data####
d$identF=ifelse(d$ident<mean(d$ident,na.rm=T),'Low',"high")
d$attriF=ifelse(d$attribution<mean(d$attribution,na.rm=T),'Low',"high")

#d, compiled prelim data
#d1-d4: 1-4 condition
#bind data: long data, transferred from wide data

###for reference
#spread eda
#d111001_eda=acast(d111001, topic~timeF,value.var='eda')
#colnames(d111001_eda) <- paste("eda", colnames(d111001_eda), sep = "_")
#d111001_eda=as.data.frame(d111001_eda)
#d111001_eda=setDT(d111001_eda, keep.rownames = TRUE)[]
#colnames(d111001_eda)[1] <- "topic"

##d111001=d111001[-which(d111001$timepoint>12),]
#d111001=unite(d111001, timeF, c(material, timepoint), remove=FALSE)
#d111001$heartT=paste("heart", d111001$timeF, sep = "_")
#d111001$edaT=paste("eda", d111001$timeF, sep = "_")
#d111001$ooT=paste("oo", d111001$timeF, sep = "_")
#d111001$ciT=paste("ci", d111001$timeF, sep = "_")

#m <- aov(h ~ (arousal*valence*time) + Error(topic/(arousal*valence*time)), 
#         contrasts = contr.sum,data=oobind)
#summary(m)
#summary(aov(h~arousal*valence*time+topic+Error(id/(arousal*valence*time*topic)), oobind))

#summary(aov(oo~arousal*valence*topic+Error(id/(arousal*valence*topic)), d))
#m = lme(h ~ arousal*valence*time, random=list(topic=~1, id=~1),data=oobind, method="REML",na.action=na.omit)
#Anova(m,type=c(2))
#summary(glht(m, linfct=mcp(Material = "Tukey")), test = adjusted(type = "bonferroni"))



#function: cleaning
clean=function(x){
  x=x[-which(x$timepoint>24),]
  x=unite(x,timeF, c(material, timepoint), remove=FALSE)
  x$heartT=paste("heart", x$timeF, sep = "_")
  x$edaT=paste("eda", x$timeF, sep = "_")
  x$ooT=paste("oo", x$timeF, sep = "_")
  x$ciT=paste("ci", x$timeF, sep = "_")
  x=as.data.frame(x)
  print(x)
}
#function: spread all four colums of data
spreadheart=function(x){setDT(as.data.frame(acast(x, topic~heartT,value.var='heart')),keep.rownames = TRUE)[]}
spreadoo=function(x){setDT(as.data.frame(acast(x, topic~ooT,value.var='oo')),keep.rownames = TRUE)[]}
spreadci=function(x){setDT(as.data.frame(acast(x, topic~ciT,value.var='ci')),keep.rownames = TRUE)[]}
spreadeda=function(x){setDT(as.data.frame(acast(x, topic~edaT,value.var='eda')),keep.rownames = TRUE)[]}
#function: merge four flat data into one data set
spread4=function(x){heart=spreadheart(x)
oo=spreadoo(x)
ci=spreadci(x)
eda=spreadeda(x) 
d=merge(heart,oo,by='rn')
d=merge(d,ci,by='rn')
d=merge(d,eda,by='rn')
d=as.data.frame(d)
colnames(d)[1]='topic'
print(d)
}


#d111001
d111001=read.table('111001-advice-numbered.csv',sep=',',header=T)
d111001=clean(d111001)
d111001=spread4(d111001)
d111001$id=c(111001)
d111001$condition=c(1)

#121002
d121001=read.table('121002-advice-numbered.csv',sep=',',header=T)
d121001=clean(d121001)
d121001=spread4(d121001)
d121001$id=c(121001)
d121001$condition=c(1)

#131003
d131003=read.table('131003-advice-numbered.csv',sep=',',header=T)
d131003=clean(d131003)
d131003=spread4(d131003)
d131003$id=c(131003)
d131003$condition=c(1)

#142004
d142004=read.table('142004-advice-numbered.csv',sep=',',header=T)
d142004=clean(d142004)
d142004=spread4(d142004)
d142004$id=c(142004)
d142004$condition=c(1)

#152005
d152005=read.table('152005-advice-numbered-grad10.5s.csv',sep=',',header=T)
d152005=clean(d152005)
d152005=spread4(d152005)
d152005$id=c(152005)
d152005$condition=c(1)

#162006
d162006=read.table('162006-advice-numbered.csv',sep=',',header=T)
d162006=clean(d162006)
d162006=spread4(d162006)
d162006$id=c(162006)
d162006$condition=c(1)

#111025
d111025=read.table('w_111025_advice-numbered.csv',sep=',',header=T)
d111025=clean(d111025)
d111025=spread4(d111025)
d111025$id=c(111025)
d111025$condition=c(1)

#121026
d121026=read.table('121026-advice-numbered.csv',sep=',',header=T)
d121026=clean(d121026)
d121026=d121026[-c(which(is.na(d121026))),]
d121026=spread4(d121026)
d121026$id=c(121026)
d121026$condition=c(1)

#131027 is crashed files

#142028
d142028=read.table('142028-advice-numbered.csv',sep=',',header=T)
d142028=clean(d142028)
d142028=spread4(d142028)
d142028$id=c(142028)
d142028$condition=c(1)

#152029
d152029=read.table('152029-advice-numbered.csv',sep=',',header=T)
d152029=clean(d152029)
d152029=spread4(d152029)
d152029$id=c(152029)
d152029$condition=c(1)

#162030 is crashed file

#111049
d111049=read.table('111049_advice-numbered.csv',sep=',',header=T)
d111049=clean(d111049)
d111049=spread4(d111049)
d111049$id=c(111049)
d111049$condition=c(1)

#121050
d121050=read.table('121050_advice-numbered.csv',sep=',',header=T)
d121050=clean(d121050)
d121050=spread4(d121050)
d121050$id=c(121050)
d121050$condition=c(1)
#131051
d131051=read.table('131051_advice-numbered.csv',sep=',',header=T)
d131051=clean(d131051)
d131051=spread4(d131051)
d131051$id=c(131051)
d131051$condition=c(1)
#142052
d142052=read.table('142052_advice-numbered.csv',sep=',',header=T)
d142052=clean(d142052)
d142052=spread4(d142052)
d142052$id=c(142052)
d142052$condition=c(1)
#152053
d152053=read.table('152053_advice-numbered.csv',sep=',',header=T)
d152053=clean(d152053)
d152053=spread4(d152053)
d152053$id=c(152053)
d152053$condition=c(1)
#162054
d162054=read.table('162054_advice-numbered.csv',sep=',',header=T)
d162054=clean(d162054)
d162054=spread4(d162054)
d162054$id=c(162054)
d162054$condition=c(1)
#111073
d111073=read.table('w_111073_advice-numbered.csv',sep=',',header=T)
d111073=clean(d111073)
d111073=spread4(d111073)
d111073$id=c(111073)
d111073$condition=c(1)
#121074
d121074=read.table('121074-advice-numbered.csv',sep=',',header=T)
d121074=clean(d121074)
d121074=spread4(d121074)
d121074$id=c(121074)
d121074$condition=c(1)
#131075
d131075=read.table('131075-advice-numbered.csv',sep=',',header=T)
d131075=clean(d131075)
d131075=spread4(d131075)
d131075$id=c(131075)
d131075$condition=c(1)
#142076
d142076=read.table('142076-advice-numbered.csv',sep=',',header=T)
d142076=clean(d142076)
d142076=spread4(d142076)
d142076$id=c(142076)
d142076$condition=c(1)
#152077
d152077=read.table('w_152077_advice-numbered.csv',sep=',',header=T)
d152077=clean(d152077)
d152077=spread4(d152077)
d152077$id=c(152077)
d152077$condition=c(1)
#111097
d111097=read.table('w_111097_advice-numbered.csv',sep=',',header=T)
d111097=clean(d111097)
d111097=spread4(d111097)
d111097$id=c(111097)
d111097$condition=c(1)
#121098
d121098=read.table('w_121098_advice-numbered.csv',sep=',',header=T)
d121098=clean(d121098)
d121098=spread4(d121098)
d121098$id=c(121098)
d121098$condition=c(1)
#131099
d131099=read.table('w_131099_advice-numbered.csv',sep=',',header=T)
d131099=clean(d131099)
d131099=spread4(d131099)
d131099$id=c(131099)
d131099$condition=c(1)
#142100
d142100=read.table('w_142100_advice-numbered.csv',sep=',',header=T)
d142100=clean(d142100)
d142100=spread4(d142100)
d142100$id=c(142100)
d142100$condition=c(1)
#152101
d152101=read.table('w_152101_advice-numbered.csv',sep=',',header=T)
d152101=clean(d152101)
d152101=spread4(d152101)
d152101$id=c(152101)
d152101$condition=c(1)
#162102
d162102=read.table('w_162102_advice-numbered.csv',sep=',',header=T)
d162102=clean(d162102)
d162102=spread4(d162102)
d162102$id=c(162102)
d162102$condition=c(1)

#111149
d111149=read.table('111149-advice-numbered.csv',sep=',',header=T)
d111149=clean(d111149)
d111149=spread4(d111149)
d111149$id=c(111149)
d111149$condition=c(1)
#152153
d152153=read.table('152153-advice-numbered.csv',sep=',',header=T)
d152153=clean(d152153)
d152153=spread4(d152153)
d152153$id=c(152153)
d152153$condition=c(1)
#131157
d131157=read.table('131157-advice-numbered.csv',sep=',',header=T)
d131157=clean(d131157)
d131157=spread4(d131157)
d131157$id=c(131157)
d131157$condition=c(1)

#rbind condition 1, combined to id 157,labeled
d1=rbind(d111001,d121001,d131003,d142004,d152005,d162006)
d1[,c('valence','arousal')]=list(NULL)
d1=rbind(d1,d111025,d121026,d142028,d152029)
d1=rbind(d1,d111049,d121050,d131051,d142052,d152053,d162054)
d1=rbind(d1,d111073,d121074,d131075,d142076,d152077,d131099,d111097,d121098,d142100,
         d152101,d162102)
d1=rbind(d1,d111149,d152153,d131157)

d1$valence=ifelse(d1$topic=='sport'|d1$topic=='preg','N','P')
d1$arousal=ifelse(d1$topic=='sport'|d1$topic=='boyf','A','C')

dnew1=d1
d1=dnew1

#211007
d211007=read.table('211007-advice-numbered-sport9s.csv',sep=',',header=T)
d211007=clean(d211007)
d211007=spread4(d211007)
d211007$id=c(211007)
d211007$condition=c(2)
#221008
d221008=read.table('221008-advice-numbered.csv',sep=',',header=T)
d221008=clean(d221008)
d221008=spread4(d221008)
d221008$id=c(221008)
d221008$condition=c(2)
#231009
d231009=read.table('231009-advice-numbered.csv',sep=',',header=T)
d231009=clean(d231009)
d231009=spread4(d231009)
d231009$id=c(231009)
d231009$condition=c(2)
#242010
d242010=read.table('242010-advice-numbered.csv',sep=',',header=T)
d242010=clean(d242010)
d242010=spread4(d242010)
d242010$id=c(242010)
d242010$condition=c(2)
#252011
d252011=read.table('w_252011_advice-numbered.csv',sep=',',header=T)
d252011=clean(d252011)
d252011=spread4(d252011)
d252011$id=c(252011)
d252011$condition=c(2)
#262012
d262012=read.table('w_262012_advice-numbered-sport6s.csv',sep=',',header=T)
d262012=clean(d262012)
d262012=spread4(d262012)
d262012$id=c(262012)
d262012$condition=c(2)

#211119
d211119=read.table('211119-advice-numbered.csv',sep=',',header=T)
d211119=clean(d211119)
d211119=spread4(d211119)
d211119$id=c(211119)
d211119$condition=c(2)

#221120
d221120=read.table('221120-advice-numbered.csv',sep=',',header=T)
d221120=clean(d221120)
d221120=spread4(d221120)
d221120$id=c(221120)
d221120$condition=c(2)
#231121
d231121=read.table('231121-advice-numbered.csv',sep=',',header=T)
d231121=clean(d231121)
d231121=spread4(d231121)
d231121$id=c(231121)
d231121$condition=c(2)
#242122
d242122=read.table('242122-advice-numbered.csv',sep=',',header=T)
d242122=clean(d242122)
d242122=spread4(d242122)
d242122$id=c(242122)
d242122$condition=c(2)
#252123
d252123=read.table('252123-advice-numbered.csv',sep=',',header=T)
d252123=clean(d252123)
d252123=spread4(d252123)
d252123$id=c(252123)
d252123$condition=c(2)
#262124
d262124=read.table('262124-advice-numbered.csv',sep=',',header=T)
d262124=clean(d262124)
d262124=spread4(d262124)
d262124$id=c(262124)
d262124$condition=c(2)

#211125
d211125=read.table('211125-advice-numbered.csv',sep=',',header=T)
d211125=clean(d211125)
d211125=spread4(d211125)
d211125$id=c(211125)
d211125$condition=c(2)


#221126
d221126=read.table('221126-advice-numbered.csv',sep=',',header=T)
d221126=clean(d221126)
d221126=spread4(d221126)
d221126$id=c(221126)
d221126$condition=c(2)
#231127
d231127=read.table('231127-advice-1Bincomplete-numbered.csv',sep=',',header=T)
d231127=clean(d231127)
d231127=spread4(d231127)
d231127$id=c(231127)
d231127$condition=c(2)
#242128
d242128=read.table('242128-advice-numbered.csv',sep=',',header=T)
d242128=clean(d242128)
d242128=spread4(d242128)
d242128$id=c(242128)
d242128$condition=c(2)
#252129
d252129=read.table('w_252129_advice-numbered.csv',sep=',',header=T)
d252129=clean(d252129)
d252129=spread4(d252129)
d252129$id=c(252129)
d252129$condition=c(2)
#262130
d262130=read.table('w_262130_advic-numbered.csv',sep=',',header=T)
d262130=clean(d262130)
d262130=spread4(d262130)
d262130$id=c(262130)
d262130$condition=c(2)

#211131 data missing, no sensor data
#221132 needs to be extracted

#231133
d231133=read.table('231133-advice-numbered.csv',sep=',',header=T)
d231133=clean(d231133)
d231133=spread4(d231133)
d231133$id=c(231133)
d231133$condition=c(2)
#242134
d242134=read.table('242134-advice-numbered.csv',sep=',',header=T)
d242134=clean(d242134)
d242134=spread4(d242134)
d242134$id=c(242134)
d242134$condition=c(2)
#252135
d252135=read.table('252135-advice-numbered.csv',sep=',',header=T)
d252135=clean(d252135)
d252135=spread4(d252135)
d252135$id=c(252135)
d252135$condition=c(2)
#262136
d262136=read.table('262136-advice-numbered.csv',sep=',',header=T)
d262136=clean(d262136)
d262136=spread4(d262136)
d262136$id=c(262136)
d262136$condition=c(2)

#211137
d211137=read.table('211137-advice-numbered.csv',sep=',',header=T)
d211137=clean(d211137)
d211137=spread4(d211137)
d211137$id=c(211137)
d211137$condition=c(2)

#221138 needs to be extracted

#231139
d231139=read.table('231139-advice-numbered.csv',sep=',',header=T)
d231139=clean(d231139)
d231139=spread4(d231139)
d231139$id=c(231139)
d231139$condition=c(2)
#242140
d242140=read.table('242140-advice-numbered.csv',sep=',',header=T)
d242140=clean(d242140)
d242140=spread4(d242140)
d242140$id=c(242140)
d242140$condition=c(2)
#252141
d252141=read.table('252141-advice-numbered.csv',sep=',',header=T)
d252141=clean(d252141)
d252141=spread4(d252141)
d252141$id=c(252141)
d252141$condition=c(2)
#262142
d262142=read.table('262142-advice-numbered.csv',sep=',',header=T)
d262142=clean(d262142)
d262142=spread4(d262142)
d262142$id=c(262142)
d262142$condition=c(2)

#211143 data missing, no sensor data

#221144
d221144=read.table('221144-advice-numbdered.csv',sep=',',header=T)
d221144=clean(d221144)
d221144=spread4(d221144)
d221144$id=c(221144)
d221144$condition=c(2)
#231145
d231145=read.table('231145-advice-numbered.csv',sep=',',header=T)
d231145=clean(d231145)
d231145=spread4(d231145)
d231145$id=c(231145)
d231145$condition=c(2)
#242146
d242146=read.table('242146-advice-numbered.csv',sep=',',header=T)
d242146=clean(d242146)
d242146=spread4(d242146)
d242146$id=c(242146)
d242146$condition=c(2)
#252147
d252147=read.table('252147-advice-numbered.csv',sep=',',header=T)
d252147=clean(d252147)
d252147=spread4(d252147)
d252147$id=c(252147)
d252147$condition=c(2)
#262148
d262148=read.table('262148-advice-numbered.csv',sep=',',header=T)
d262148=clean(d262148)
d262148=spread4(d262148)
d262148$id=c(262148)
d262148$condition=c(2)
#221150
d221150=read.table('221150-advice-numbered.csv',sep=',',header=T)
d221150=clean(d221150)
d221150=spread4(d221150)
d221150$id=c(221150)
d221150$condition=c(2)
#262154
d262154=read.table('262154-advice-numbered.csv',sep=',',header=T)
d262154=clean(d262154)
d262154=spread4(d262154)
d262154$id=c(262154)
d262154$condition=c(2)
#242158
d242158=read.table('242158-advice-numbered.csv',sep=',',header=T)
d242158=clean(d242158)
d242158=spread4(d242158)
d242158$id=c(242158)
d242158$condition=c(2)

#rbind condition 2, combined to id 158, labeled
d2=rbind(d221120,d231121,d242122,d252123,d262124,d221126,d231127,d242128,
         d252129,d262130,d231133,d242134,d252135,d262136,d262012,d231139,d242140,
         d252141,d262142,d221144,d231145,d242146,d252147,d262148,d221150,
         d262154,d242158,d211119,d211125,d211137)

d2$valence=ifelse(d2$topic=='grad'|d2$topic=='boyf','N','P')
d2$arousal=ifelse(d2$topic=='grad'|d2$topic=='sport','A','C')


#311013
d311013=read.table('w_311013_advice-numbered.csv',sep=',',header=T)
d311013=clean(d311013)
d311013=spread4(d311013)
d311013$id=c(311013)
d311013$condition=c(3)
#321014
d321014=read.table('w_321014_advice-numbered.csv',sep=',',header=T)
d321014=clean(d321014)
d321014=spread4(d321014)
d321014$id=c(321014)
d321014$condition=c(3)
#331015
d331015=read.table('w_331015_advice-numbered.csv',sep=',',header=T)
d331015=clean(d331015)
d331015=spread4(d331015)
d331015$id=c(331015)
d331015$condition=c(3)
#342016
d342016=read.table('w_342016_advice-numbered.csv',sep=',',header=T)
d342016=clean(d342016)
d342016=spread4(d342016)
d342016$id=c(342016)
d342016$condition=c(3)
#352017
d352017=read.table('w_352017-advice-numbered.csv',sep=',',header=T)
d352017=clean(d352017)
d352017=spread4(d352017)
d352017$id=c(352017)
d352017$condition=c(3)
#362018
d362018=read.table('w_362018-advice-numbered.csv',sep=',',header=T)
d362018=clean(d362018)
d362018=spread4(d362018)
d362018$id=c(362018)
d362018$condition=c(3)
#311037
d311037=read.table('311037_advice-numbered.csv',sep=',',header=T)
d311037=clean(d311037)
d311037=spread4(d311037)
d311037$id=c(311037)
d311037$condition=c(3)
#321038
d321038=read.table('321038_advice-numbered.csv',sep=',',header=T)
d321038=clean(d321038)
d321038=spread4(d321038)
d321038$id=c(321038)
d321038$condition=c(3)
#331039
d331039=read.table('331039_advice-numbered.csv',sep=',',header=T)
d331039=clean(d331039)
d331039=spread4(d331039)
d331039$id=c(331039)
d331039$condition=c(3)
#342040
d342040=read.table('342040_advice-numbered.csv',sep=',',header=T)
d342040=clean(d342040)
d342040=spread4(d342040)
d342040$id=c(342040)
d342040$condition=c(3)
#352041
d352041=read.table('352041_advice-numbered.csv',sep=',',header=T)
d352041=clean(d352041)
d352041=spread4(d352041)
d352041$id=c(352041)
d352041$condition=c(3)
#362042
d362042=read.table('362042_advice-numbered.csv',header=T)
d362042=clean(d362042)
d362042=spread4(d362042)
d362042$id=c(362042)
d362042$condition=c(3)
#311061
d311061=read.table('w_311061_advice-numbered.csv',sep=',',header=T)
d311061=clean(d311061)
d311061=spread4(d311061)
d311061$id=c(311061)
d311061$condition=c(3)
#321062
d321062=read.table('w_321062_advice-numbered.csv',sep=',',header=T)
d321062=clean(d321062)
d321062=spread4(d321062)
d321062$id=c(321062)
d321062$condition=c(3)
#331063
d331063=read.table('w_331063_advice-numbered.csv',sep=',',header=T)
d331063=clean(d331063)
d331063=spread4(d331063)
d331063$id=c(331063)
d331063$condition=c(3)
#342064
d342064=read.table('342064-advice-numbered.csv',sep=',',header=T)
d342064=clean(d342064)
d342064=spread4(d342064)
d342064$id=c(342064)
d342064$condition=c(3)
#352065
d352065=read.table('352065-advice-numbered.csv',sep=',',header=T)
d352065=clean(d352065)
d352065=spread4(d352065)
d352065$id=c(352065)
d352065$condition=c(3)
#362066
d362066=read.table('362066-advice-numbered.csv',sep=',',header=T)
d362066=clean(d362066)
d362066=spread4(d362066)
d362066$id=c(362066)
d362066$condition=c(3)
#311085
d311085=read.table('w_311085_advice-numbered.csv',sep=',',header=T)
d311085=clean(d311085)
d311085=spread4(d311085)
d311085$id=c(311085)
d311085$condition=c(3)
#321086
d321086=read.table('321086-advice-numbered.csv',sep=',',header=T)
d321086=clean(d321086)
d321086=spread4(d321086)
d321086$id=c(321086)
d321086$condition=c(3)
#331087
d331087=read.table('331087-advice-numbered.csv',sep=',',header=T)
d331087=clean(d331087)
d331087=spread4(d331087)
d331087$id=c(331087)
d331087$condition=c(3)
#342088
d342088=read.table('w_342088_advice-numbered.csv',sep=',',header=T)
d342088=clean(d342088)
d342088=spread4(d342088)
d342088$id=c(342088)
d342088$condition=c(3)
#352089
d352089=read.table('w_352089_advice-numbered.csv',sep=',',header=T)
d352089=clean(d352089)
d352089=spread4(d352089)
d352089$id=c(352089)
d352089$condition=c(3)
#362090
d362090=read.table('w_362090_advice-numbered.csv',sep=',',header=T)
d362090=clean(d362090)
d362090=spread4(d362090)
d362090$id=c(362090)
d362090$condition=c(3)
#311107
d311107=read.table('w_311107_advice-numbered.csv',sep=',',header=T)
d311107=clean(d311107)
d311107=spread4(d311107)
d311107$id=c(311107)
d311107$condition=c(3)
#321108
d321108=read.table('w_321108_advice-numbered.csv',sep=',',header=T)
d321108=clean(d321108)
d321108=spread4(d321108)
d321108$id=c(321108)
d321108$condition=c(3)
#331109
d331109=read.table('331109-advice-numbered.csv',sep=',',header=T)
d331109=clean(d331109)
d331109=spread4(d331109)
d331109$id=c(331109)
d331109$condition=c(3)
#342110
d342110=read.table('342110-advice-numbered.csv',sep=',',header=T)
d342110=clean(d342110)
d342110=spread4(d342110)
d342110$id=c(342110)
d342110$condition=c(3)
#352111
d352111=read.table('352111-advice-numbered.csv',sep=',',header=T)
d352111=clean(d352111)
d352111=spread4(d352111)
d352111$id=c(352111)
d352111$condition=c(3)
#362112
d362112=read.table('362112-advice-numbered.csv',sep=',',header=T)
d362112=clean(d362112)
d362112=spread4(d362112)
d362112$id=c(362112)
d362112$condition=c(3)

#331151
d331151=read.table('331151-advice-numbered.csv',sep=',',header=T)
d331151=clean(d331151)
d331151=spread4(d331151)
d331151$id=c(331151)
d331151$condition=c(3)
#311155
d311155=read.table('311155-advice-numbered.csv',sep=',',header=T)
d311155=clean(d311155)
d311155=spread4(d311155)
d311155$id=c(311155)
d311155$condition=c(3)
#352159
d352159=read.table('352159-advice-numnbered.csv',sep=',',header=T)
d352159=clean(d352159)
d352159=spread4(d352159)
d352159$id=c(352159)
d352159$condition=c(3)

#rbind condition 3, combined to id 159, labeled
d3=rbind(d311013,d321014,d331015,d342016,d352017,d362018)
d3[,c('valence','arousal')]=list(NULL)
d3=rbind(d3,d311037,d321038,d331039,d342040,d352041,d362042)
d3=rbind(d3,d311061,d321062, d331063, d342064,d352065, d362066,d311085,d321086,d331087,
         d342088,d352089,d362090,d311107,d321108,d331109,d342110,d352111,d362112,
         d331151,d311155,d352159)

d3$valence=ifelse(d3$topic=='preg'|d2$topic=='sport','N','P')
d3$arousal=ifelse(d3$topic=='grad'|d3$topic=='preg','A','C')


#411019
d411019=read.table('w_411019_advice-numbered.csv',sep=',',header=T)
d411019=clean(d411019)
d411019=spread4(d411019)
d411019$id=c(411019)
d411019$condition=c(4)
#421020
d421020=read.table('w_421020_advice-numbered.csv',sep=',',header=T)
d421020=clean(d421020)
d421020=spread4(d421020)
d421020$id=c(421020)
d421020$condition=c(4)
#431021
d431021=read.table('w_431021_advice-numbered.csv',sep=',',header=T)
d431021=clean(d431021)
d431021=spread4(d431021)
d431021$id=c(431021)
d431021$condition=c(4)
#442022
d442022=read.table('w_442022_advice-numbered.csv',sep=',',header=T)
d442022=clean(d442022)
d442022=spread4(d442022)
d442022$id=c(442022)
d442022$condition=c(4)
#452023
d452023=read.table('w_452023_advice-numbered.csv',sep=',',header=T)
d452023=clean(d452023)
d452023=spread4(d452023)
d452023$id=c(452023)
d452023$condition=c(4)
#462024
d462024=read.table('w_462024_advice-numbered.csv',sep=',',header=T)
d462024=clean(d462024)
d462024=spread4(d462024)
d462024$id=c(462024)
d462024$condition=c(4)
#411043
d411043=read.table('411043_advice-numbered.csv',sep=',',header=T)
d411043=clean(d411043)
d411043=spread4(d411043)
d411043$id=c(411043)
d411043$condition=c(4)
#421044
d421044=read.table('421044_advice-numbered.csv',sep=',',header=T)
d421044=clean(d421044)
d421044=spread4(d421044)
d421044$id=c(421044)
d421044$condition=c(4)

#431045 needs to be extracted

#442046
d442046=read.table('442046_advice-numbered.csv',sep=',',header=T)
d442046=clean(d442046)
d442046=spread4(d442046)
d442046$id=c(442046)
d442046$condition=c(4)
#452047
d453047=read.table('452047_advice-numbered.csv',sep=',',header=T)
d453047=clean(d453047)
d453047=spread4(d453047)
d453047$id=c(453047)
d453047$condition=c(4)

#462048
d462048=read.table('462048-advice-numbered.csv',sep=',',header=T)
d462048=clean(d462048)
d462048=spread4(d462048)
d462048$id=c(462048)
d462048$condition=c(4)

#411067
d411067=read.table('411067_advice-numbered.csv',sep=',',header=T)
d411067=clean(d411067)
d411067=spread4(d411067)
d411067$id=c(411067)
d411067$condition=c(4)
#421068
d421068=read.table('421068_advice-numbered.csv',sep=',',header=T)
d421068=clean(d421068)
d421068=spread4(d421068)
d421068$id=c(421068)
d421068$condition=c(4)
#431069
d431069=read.table('w_431069_advice-numbered.csv',sep=',',header=T)
d431069=clean(d431069)
d431069=spread4(d431069)
d431069$id=c(431069)
d431069$condition=c(4)

#442070 needs to be extracted

#452071
d452071=read.table('452071_advice-numbered.csv',sep=',',header=T)
d452071=clean(d452071)
d452071=spread4(d452071)
d452071$id=c(452071)
d452071$condition=c(4)
#462072
d462072=read.table('w_462072_advice-numbered.csv',sep=',',header=T)
d462072=clean(d462072)
d462072=spread4(d462072)
d462072$id=c(462072)
d462072$condition=c(4)
#411091
d411091=read.table('w_411091_advice-numbered.csv',sep=',',header=T)
d411091=clean(d411091)
d411091=spread4(d411091)
d411091$id=c(411091)
d411091$condition=c(4)
#421092
d421092=read.table('w_421092_advice-numbered.csv',sep=',',header=T)
d421092=clean(d421092)
d421092=spread4(d421092)
d421092$id=c(421092)
d421092$condition=c(4)
#431093
d431093=read.table('431093_advice-numbered.csv',sep=',',header=T)
d431093=clean(d431093)
d431093=spread4(d431093)
d431093$id=c(431093)
d431093$condition=c(4)
#442094
d442094=read.table('w_442094_advice-numbered.csv',sep=',',header=T)
d442094=clean(d442094)
d442094=spread4(d442094)
d442094$id=c(442094)
d442094$condition=c(4)
#452095
d452095=read.table('w_452095_advice-numbered.csv',sep=',',header=T)
d452095=clean(d452095)
d452095=spread4(d452095)
d452095$id=c(452095)
d452095$condition=c(4)
#462096
d462096=read.table('w_462096_advice-numbered.csv',sep=',',header=T)
d462096=clean(d462096)
d462096=spread4(d462096)
d462096$id=c(462096)
d462096$condition=c(4)
#411113
d411113=read.table('w_411113_advice-numbered.csv',sep=',',header=T)
d411113=clean(d411113)
d411113=spread4(d411113)
d411113$id=c(411113)
d411113$condition=c(4)
#421114
d421114=read.table('421114_advice-numbered.csv',sep=',',header=T)
d421114=clean(d421114)
d421114=spread4(d421114)
d421114$id=c(421114)
d421114$condition=c(4)
#431115
d431115=read.table('431115_advice-numbered.csv',sep=',',header=T)
d431115=clean(d431115)
d431115=spread4(d431115)
d431115$id=c(431115)
d431115$condition=c(4)
#442116
d442116=read.table('w_442116_advice-numbered.csv',sep=',',header=T)
d442116=clean(d442116)
d442116=spread4(d442116)
d442116$id=c(442116)
d442116$condition=c(4)
#452117
d452117=read.table('w_452117_advice-numbered.csv',sep=',',header=T)
d452117=clean(d452117)
d452117=spread4(d452117)
d452117$id=c(452117)
d452117$condition=c(4)
#462118
d462118=read.table('w_462118_advice-numbered.csv',sep=',',header=T)
d462118=clean(d462118)
d462118=spread4(d462118)
d462118$id=c(462118)
d462118$condition=c(4)

#442152
d442152=read.table('442152-advice-numbered.csv',sep=',',header=T)
d442152=clean(d442152)
d442152=spread4(d442152)
d442152$id=c(442152)
d442152$condition=c(4)
#421156
d421156=read.table('421156-advice-numbered.csv',sep=',',header=T)
d421156=clean(d421156)
d421156=spread4(d421156)
d421156$id=c(421156)
d421156$condition=c(4)
#462160
d462160=read.table('462160-advice-numbered.csv',sep=',',header=T)
d462160=clean(d462160)
d462160=spread4(d462160)
d462160$id=c(462160)
d462160$condition=c(4)

#rbind condition 4, combined to id 160, labeled
d4=rbind(d411019,d421020,d431021,d442022,d452023,d462024)
d4[,c('valence','arousal')]=list(NULL)
d4=rbind(d4,d411043,d421044,d442046,d453047)
d4=rbind(d4,d411067, d421068, d431069, d452071, d462072,d411091,d431093,d442094,d452095,d462096,
         d411113,d421114,d431115,d442116,d452117,d462118,d442152,d421156,d462160)
d4=rbind(d4,d462048)

d4$valence=ifelse(d4$topic=='grad'|d4$topic=='boyf','N','P')
d4$arousal=ifelse(d4$topic=='boyf'|d4$topic=='preg','A','C')

#create overall dataset
d=rbind(d1,d2,d3,d4)

d4=unique(d4)

#######bind advice texts####

#clean text data
setwd("~/Desktop/physio/advice/advice questionniare data")

#condition 1
lines <- readLines('response_C1_NA_sport.txt')
dd <- read.csv(text=lines[seq(1, length(lines), by=2)], header=F)
dd$comments = lines[seq(2, length(lines), by=2)]
dd$condition=c(1)
dd$valence=c('N')
dd$arousal=c('A')
dd$topic=c('sport')
C1_NA_sport=dd

lines <- readLines('response_C1_NC_preg.txt')
dd <- read.csv(text=lines[seq(1, length(lines), by=2)], header=F)
dd$comments = lines[seq(2, length(lines), by=2)]
dd$condition=c(1)
dd$valence=c('N')
dd$arousal=c('C')
dd$topic=c('preg')
C1_NC_preg=dd

lines <- readLines('response_C1_PA_boyf.txt')
dd <- read.csv(text=lines[seq(1, length(lines), by=2)], header=F)
dd$comments = lines[seq(2, length(lines), by=2)]
dd$condition=c(1)
dd$valence=c('P')
dd$arousal=c('A')
dd$topic=c('boyf')
C1_PA_boyf=dd

lines <- readLines('response_C1_PC_grad.txt')
dd <- read.csv(text=lines[seq(1, length(lines), by=2)], header=F)
dd$comments = lines[seq(2, length(lines), by=2)]
dd$condition=c(1)
dd$valence=c('P')
dd$arousal=c('C')
dd$topic=c('grad')
C1_PC_grad=dd

text_C1=rbind(C1_NA_sport,C1_NC_preg,C1_PA_boyf,C1_PC_grad)

#condition 2 
lines <- readLines('response_C2_NA_grad.txt')
dd <- read.csv(text=lines[seq(1, length(lines), by=2)], header=F)
dd$comments = lines[seq(2, length(lines), by=2)]
dd$condition=c(2)
dd$valence=c('N')
dd$arousal=c('A')
dd$topic=c('grad')
C2_NA_grad=dd

lines <- readLines('response_C2_NC_boyf.txt')
dd <- read.csv(text=lines[seq(1, length(lines), by=2)], header=F)
dd$comments = lines[seq(2, length(lines), by=2)]
dd$condition=c(2)
dd$valence=c('N')
dd$arousal=c('C')
dd$topic=c('boyf')
C2_NC_boyf=dd


lines <- readLines('response_C2_PA_sport.txt')
dd <- read.csv(text=lines[seq(1, length(lines), by=2)], header=F)
dd$comments = lines[seq(2, length(lines), by=2)]
dd$condition=c(2)
dd$valence=c('P')
dd$arousal=c('A')
dd$topic=c('sport')
C2_PA_sport=dd

lines <- readLines('response_C2_PC_preg.txt')
dd <- read.csv(text=lines[seq(1, length(lines), by=2)], header=F)
dd$comments = lines[seq(2, length(lines), by=2)]
dd$condition=c(2)
dd$valence=c('P')
dd$arousal=c('C')
dd$topic=c('preg')
C2_PC_preg=dd

text_C2=rbind(C2_NA_grad,C2_NC_boyf,C2_PA_sport,C2_PC_preg)

#condition 3
lines <- readLines('response_C3_NA_preg.txt')
dd <- read.csv(text=lines[seq(1, length(lines), by=2)], header=F)
dd$comments = lines[seq(2, length(lines), by=2)]
dd$condition=c(3)
dd$valence=c('N')
dd$arousal=c('A')
dd$topic=c('preg')
C3_NA_preg=dd

lines <- readLines('response_C3_NC_sport.txt')
dd <- read.csv(text=lines[seq(1, length(lines), by=2)], header=F)
dd$comments = lines[seq(2, length(lines), by=2)]
dd$condition=c(3)
dd$valence=c('N')
dd$arousal=c('C')
dd$topic=c('sport')
C3_NC_sport=dd

lines <- readLines('response_C3_PA_grad.txt')
dd <- read.csv(text=lines[seq(1, length(lines), by=2)], header=F)
dd$comments = lines[seq(2, length(lines), by=2)]
dd$condition=c(3)
dd$valence=c('P')
dd$arousal=c('A')
dd$topic=c('grad')
C3_PA_grad=dd

lines <- readLines('response_C3_PC_boyf.txt')
dd <- read.csv(text=lines[seq(1, length(lines), by=2)], header=F)
dd$comments = lines[seq(2, length(lines), by=2)]
dd$condition=c(3)
dd$valence=c('P')
dd$arousal=c('C')
dd$topic=c('boyf')
C3_PC_boyf=dd

text_C3=rbind(C3_NA_preg,C3_NC_sport,C3_PA_grad,C3_PC_boyf)

#condition 4
lines <- readLines('response_C4_NA_boyf.txt')
dd <- read.csv(text=lines[seq(1, length(lines), by=2)], header=F)
dd$comments = lines[seq(2, length(lines), by=2)]
dd$condition=c(4)
dd$valence=c('N')
dd$arousal=c('A')
dd$topic=c('boyf')
C4_NA_boyf=dd

lines <- readLines('response_C4_NC_grad.txt')
dd <- read.csv(text=lines[seq(1, length(lines), by=2)], header=F)
dd$comments = lines[seq(2, length(lines), by=2)]
dd$condition=c(4)
dd$valence=c('N')
dd$arousal=c('C')
dd$topic=c('grad')
C4_NC_grad=dd

lines <- readLines('response_C4_PA_preg.txt')
dd <- read.csv(text=lines[seq(1, length(lines), by=2)], header=F)
dd$comments = lines[seq(2, length(lines), by=2)]
dd$condition=c(4)
dd$valence=c('P')
dd$arousal=c('A')
dd$topic=c('preg')
C4_PA_preg=dd

lines <- readLines('response_C4_PC_sport.txt')
dd <- read.csv(text=lines[seq(1, length(lines), by=2)], header=F)
dd$comments = lines[seq(2, length(lines), by=2)]
dd$condition=c(4)
dd$valence=c('P')
dd$arousal=c('C')
dd$topic=c('sport')
C4_PC_sport=dd

text_C4=rbind(C4_NA_boyf,C4_NC_grad,C4_PA_preg,C4_PC_sport)

text=rbind(text_C1,text_C2,text_C3,text_C4)

#export clean text data
write.csv(text,'advice-response-cleaned.csv')


######prelim text analysis
#t=read.table('advice-text-prelim.csv',sep=',')
#library(stringr)
#t2=t
#t2=str_split_fixed(t$V2, "',", 4)
#t2=gsub("'",replacement='',t2)
#t2=as.data.frame(t2)
#t2$topic=t$V1
#t2[which(t2$V3=='NA'),3]=NA
#write.csv(t2,file='advice-text-prelim-cleaned.csv')

liwc=read.csv('LIWC2015 Results (advice-response-cleaned).csv',header=T)
liwc=liwc[-c(1),]
liwc$Source..B.=gsub("'", '', liwc$Source..B.)
liwc$Source..B.=as.numeric(liwc$Source..B.)

d=merge(d,liwc,by.x=c('topic','id'), by.y=c('Source..K.','Source..B.'))

write.csv(d,'advice-physio-with-text.csv')

############create composite scores of physio##########
d$heart=rowMeans(select(d,num_range("heart_S_", 1:24)),na.rm=T)
d$oo=rowMeans(select(d,num_range("oo_S_", 1:24)),na.rm=T)
d$ci=rowMeans(select(d,num_range("ci_S_", 1:24)),na.rm=T)
d$eda=rowMeans(select(d,num_range("eda_S_", 1:24)),na.rm=T)



########prelim bind ratings of emotion
#r1=read.csv('ExPart1_C1.csv',header=T)
#r_e=data.frame(id=r1$Subject, condition=c(1),
 #            arousal_sport=r1$arouse_C1_NA_sport,arousal_preg=r1$arouse_C1_NC_preg,arousal_boyf=r1$arouse_C1_PA_boyf,arousal_grad=r1$arouse_C1_PC_grad,
  #          age=r1$age,sex=r1$sex)
#positive_sport=r1$positive_C1_NA_sport,positive_preg=r1$positive_C1_NC_preg,positive_boyf=r1$positive_C1_PA_boyf,positive_grad=r1$positive_C1_PC_grad,
#negative_sport=r1$negative_C1_NA_sport,negative_preg=r1$negative_C1_NC_preg,negative_boyf=r1$negative_C1_PA_boyf,negative_grad=r1$negative_C1_PC_grad,

#r_e=gather(r_e,key='topic',value='arousal',r_e[,3:6])

#r2=read.csv('ExPart1_C2.csv',header=T)
#r3=read.csv('ExPart1_C3.csv',header=T)
#r4=read.csv('ExPart1_C4.csv',header=T)
#r=rbind(r1,r2,r3,r4)


######bind message rating data (emotions, attribution, identification)#######
message=read.csv('advice-questionnaire-message rating-compiled.csv',header=T)
d=merge(d,message,by.x=c('id','topic'),by.y=c('Subject','topic'))

######bind personality data, demo#########
person1=read.csv('advice-questionnaire-personality-compiled.csv',header=T)
person1$topic=c('grad')
person2=read.csv('advice-questionnaire-personality-compiled.csv',header=T)
person2$topic=c('sport')
person3=read.csv('advice-questionnaire-personality-compiled.csv',header=T)
person3$topic=c('preg')
person4=read.csv('advice-questionnaire-personality-compiled.csv',header=T)
person4$topic=c('boyf')
person=rbind(person1,person2,person3,person4)
d=merge(d,person,by.x=c('id','topic'),by.y=c('Subject','topic'))

##########create data without NAs###########
dc=d
k=which(is.na(select(dc,num_range("heart_S_", 1:24))),arr.ind=TRUE)
dc[k] <- rowMeans(select(dc,num_range("heart_S_", 1:24)), na.rm=TRUE)[k[,1]]
dc[k]=dc$heart[k[,1]]

##############reliability test of personlaity and rating data##############
#attribution
alpha(d[,c('attri_fault.x','attri_resp.x','attri_blame.x')])
d$attribution=rowMeans(d[,c('attri_fault.x','attri_resp.x','attri_blame.x')])

#identification
alpha(d[,c('ident_under.x','ident_same.x','ident_felt.x','ident_inside.x','ident_did.x')])
d$ident=rowMeans(d[,c('ident_under.x','ident_same.x','ident_felt.x','ident_inside.x','ident_did.x')])

#empathy
alpha(select(d,starts_with('emp_')))
d$empathy=rowMeans(select(d,starts_with('emp_')))


#############running start by heart#########
#discriptives
sapply(select(d,starts_with('heart_S_')), aggregate(d$heart_S_1,by=list(d$valence,d$arousal),mean,na.rm=T))
lapply(10:33,function(x){aggregate(d[,x],by=list(d$valence,d$arousal),mean,na.rm=T)})
lapply(1:24,function(x){aggregate(select(d,num_range('heart_S_',x)),by=list(d$valence,d$arousal),mean,na.rm=T)})


heartlong=data.frame(select(d,num_range("heart_S_", 1:24)),condition=d$condition,id=d$id,
                     valence=d$valence,arousal=d$arousal,topic=d$topic)
colnames(heartlong)[1:24]=c('h1','h2','h3','h4','h5','h6','h7','h8','h9',
                            'h10','h11','h12','h13','h14','h15','h16','h17','h18',
                            'h19','h20','h21','h22','h23','h24')
heartlong=reshape(heartlong,varying=1:24,idvar = c("id",'topic','condition','valence','arousal'), direction = "long",v.names='h')

#positive higher heart
Anova(lmer(h ~ arousal*valence*time+(1|id)+(1|topic),data=heartlong,na.action=na.omit))
m=lmer(h ~ arousal*valence*time+(1|id)+(1|topic),data=heartlong,na.action=na.omit)
summary(m)

m = lme(h ~ valence*arousal*time,  random=list(id=~1, topic=~1), data=heartlong,method="REML",na.action=na.omit)
summary(m)
anova(m)

summary(lmer(h ~ arousal*valence*time+(1|id)+(1|topic),data=heartlong,na.action=na.omit,REML=FALSE))

summary(lmer(h ~ arousal*valence*time+(time|id/topic),data=heartlong,na.action=na.omit))
summary(lmer(h ~ arousal*valence*time+(time|topic:id)+
               (arousal*valence*time||topic),data=heartlong,na.action=na.omit))

summary(lmer(heart ~ arousal*valence+(1|id)+(1|topic),data=d,na.action=na.omit))

m = lme(h ~ arousal*valence*time*topic, random=list(~1|id),data=heartlong,method="REML",na.action=na.omit)
summary(m)
anova(m)
aggregate(heartlong$h,by=list(heartlong$valence),mean,na.rm=T)
aggregate(heartlong$h,by=list(heartlong$arousal),mean,na.rm=T)

m = lme(h ~ arousal*valence*time*topic, random=~1|id,data=heartlong,method="REML",na.action=na.omit)
summary(m)
anova(m)
lsmeans(m, list(pairwise ~ valence*time), adjust = "tukey")

#ploting
dNP=aggregate(heartlong$h,by=list(heartlong$valence,heartlong$time),mean,na.rm=T)
colnames(dNP)=c('valence','time','heart')
dNP$time=as.numeric(dNP$time)
ggplot(data = dNP, aes(x=time, y=heart)) + geom_line(aes(colour=valence))

dAC=aggregate(heartlong$h,by=list(heartlong$arousal,heartlong$time),mean,na.rm=T)
colnames(dAC)=c('arousal','time','heart')
dAC$time=as.numeric(dAC$time)
ggplot(data = dAC, aes(x=time, y=heart)) + geom_line(aes(colour=arousal))

dx=aggregate(heartlong$h,by=list(heartlong$arousal,heartlong$valence,heartlong$time),mean,na.rm=T)
colnames(dx)=c('arousal','valence','time','heart')
dx$time=as.numeric(dx$time)
ggplot(dx, aes(x = time, y = heart,colour = factor(valence))) +geom_line() +facet_grid(. ~ arousal) + theme_classic()

dx=aggregate(heartlong$h,by=list(heartlong$arousal,heartlong$valence,heartlong$time),mean,na.rm=T)
colnames(dx)=c('arousal','valence','time','OO')
dx$time=as.numeric(dx$time)
dx$arousal=ifelse(dx$arousal=='A','Arousing content','Calm content')
dx$valence=ifelse(dx$valence=='P','Positive content','Negative content')
ggplot(dx, aes(x = time, y = OO,colour = valence)) +geom_line() +facet_grid(. ~ arousal) + 
  theme_classic()+  xlab("Time (seconds)") + ylab("Heart Rate (IBI)")




#####oo, indicates positivity#########
oobind=cbind(select(d,num_range("oo_S_", 1:24)),condition=d$condition,id=d$id,
             valence=d$valence,arousal=d$arousal,topic=d$topic)
colnames(oobind)[1:24]=c('oo1','oo2','oo3','oo4','oo5','oo6','oo7','oo8','oo9',
                            'oo10','oo11','oo12','oo13','oo14','oo15','oo16','oo17','oo18',
                            'oo19','oo20','oo21','oo22','oo23','oo24')
oobind=reshape(oobind,varying=1:24,idvar = c("id",'topic','condition','valence','arousal'), direction = "long",v.names='h')

oobind$time=as.factor(oobind$time)
m = lme(h ~ valence*arousal*time,  random=list(id=~1, topic=~1), data=oobind,method="REML",na.action=na.omit)
summary(m)
anova(m)
lsmeans(m, list(pairwise ~ valence*arousal*time), adjust = "tukey")


summary(lmer(h ~ arousal*valence*time+(1|id)+(1|topic),data=oobind,na.action=na.omit))
summary(lmer(h ~ arousal*valence*time+(time|id/topic),data=oobind,na.action=na.omit))
aggregate(oobind$h,by=list(oobind$valence),mean,na.rm=T)
aggregate(oobind$h,by=list(oobind$topic),mean,na.rm=T)

summary(lmer(oo ~ arousal*valence*time+(1|id)+(1|topic),data=d,na.action=na.omit))

m = lme(h ~ arousal*valence*time*topic, random=list(~1|id,~1|topic),data=oobind,method="REML",na.action=na.omit)
summary(m)
anova(m)
lsmeans(m, list(pairwise ~ arousal*valence), adjust = "tukey")

#ploting
dNP=aggregate(oobind$h,by=list(oobind$valence,oobind$time),mean,na.rm=T)
colnames(dNP)=c('valence','time','OO')
dNP$time=as.numeric(dNP$time)
ggplot(data = dNP, aes(x=time, y=OO)) + geom_line(aes(colour=valence))+
  xlab("Time") + ylab("Zygomatic activity (μ Volts)")

dAC=aggregate(oobind$h,by=list(oobind$arousal,oobind$time),mean,na.rm=T)
colnames(dAC)=c('arousal','time','OO')
dAC$time=as.numeric(dAC$time)
ggplot(data = dAC, aes(x=time, y=OO)) + geom_line(aes(colour=arousal))

dx=aggregate(oobind$h,by=list(oobind$arousal,oobind$valence,oobind$time),mean,na.rm=T)
colnames(dx)=c('arousal','valence','time','OO')
dx$time=as.numeric(dx$time)
dx$arousal=ifelse(dx$arousal=='A','Arousing content','Calm content')
dx$valence=ifelse(dx$valence=='P','Positive content','Negative content')
ggplot(dx, aes(x = time, y = OO,colour = valence)) +geom_line() +facet_grid(. ~ arousal) + 
  theme_classic()+  xlab("Time (seconds)") + ylab("Orbicularis Oculi (μ Volts)")

##########running OO with covariates###############
oobind=cbind(select(d,num_range("oo_S_", 1:24)),condition=d$condition,id=d$id,
             valence=d$valence,arousal=d$arousal,topic=d$topic,identification=d$identF,attribution=d$attriF)
colnames(oobind)[1:24]=c('oo1','oo2','oo3','oo4','oo5','oo6','oo7','oo8','oo9',
                         'oo10','oo11','oo12','oo13','oo14','oo15','oo16','oo17','oo18',
                         'oo19','oo20','oo21','oo22','oo23','oo24')
oobind=reshape(oobind,varying=1:24,idvar = c("id",'topic','condition','valence','arousal'), direction = "long",v.names='h')


m = lme(h ~ valence*arousal*time*attribution,  random=list(id=~1, topic=~1), data=oobind,method="REML",na.action=na.omit)
anova(m)
summary(m)
oobind$time=as.factor(oobind$time)
lsmeans(m, list(pairwise ~ valence*attribution), adjust = "tukey")
lsmeans(m, list(pairwise ~ arousal*attribution), adjust = "tukey")
lsmeans(m, list(pairwise ~ arousal), adjust = "tukey")
lsmeans(m, list(pairwise ~ valence), adjust = "tukey")


summary(lmer(h ~ arousal*valence*time+(1|id)+(1|topic),data=oobind,na.action=na.omit))
summary(lmer(h ~ arousal*valence*time+(time|id/topic),data=oobind,na.action=na.omit))
aggregate(oobind$h,by=list(oobind$valence),mean,na.rm=T)
aggregate(oobind$h,by=list(oobind$topic),mean,na.rm=T)

summary(lmer(oo ~ arousal*valence*time+(1|id)+(1|topic),data=d,na.action=na.omit))

m = lme(h ~ arousal*valence*time*topic, random=list(~1|id,~1|topic),data=oobind,method="REML",na.action=na.omit)
summary(m)
anova(m)
lsmeans(m, list(pairwise ~ arousal*valence), adjust = "tukey")


#####ci, indicates negativity#########
cibind=cbind(select(d,num_range("ci_S_", 1:24)),condition=d$condition,id=d$id,
             valence=d$valence,arousal=d$arousal,topic=d$topic)
colnames(cibind)[1:24]=c('ci1','ci2','ci3','ci4','ci5','ci6','ci7','ci8','ci9',
                         'ci10','ci11','ci12','ci13','ci14','ci15','ci16','ci17','ci18',
                         'ci19','ci20','ci21','ci22','ci23','ci24')
cibind=reshape(cibind,varying=1:24,idvar = c("id",'topic','condition','valence','arousal'), direction = "long",v.names='h')

m = lme(h ~ valence*arousal*time,  random=list(id=~1, topic=~1), data=cibind,method="REML",na.action=na.omit)
summary(m)
anova(m)
cibind$time=as.factor(cibind$time)
lsmeans(m, list(pairwise ~ valence*arousal*time), adjust = "tukey")


summary(lmer(h ~ arousal*valence*time+(1|id)+(1|topic),data=cibind,na.action=na.omit))
m=lmer(h ~ arousal*valence*time+(1|id)+(1|topic),data=cibind,na.action=na.omit)
summary(glht(m, linfct=mcp(valence = "Tukey")), test = adjusted(type = "bonferroni"))
aggregate(cibind$h,by=list(cibind$valence,cibind$time),mean,na.rm=T)

summary(lmer(h ~ arousal*valence*time+(time|topic/id),data=cibind,na.action=na.omit))

######ci with covariates#########
cibind=cbind(select(d,num_range("ci_S_", 1:24)),condition=d$condition,id=d$id,
             valence=d$valence,arousal=d$arousal,topic=d$topic, attribution=d$attriF,
             identification=d$identF)
colnames(cibind)[1:24]=c('ci1','ci2','ci3','ci4','ci5','ci6','ci7','ci8','ci9',
                         'ci10','ci11','ci12','ci13','ci14','ci15','ci16','ci17','ci18',
                         'ci19','ci20','ci21','ci22','ci23','ci24')
cibind=reshape(cibind,varying=1:24,idvar = c("id",'topic','condition','valence','arousal'), direction = "long",v.names='h')

m = lme(h ~ attribution*time,  random=list(id=~1, topic=~1), data=cibind,method="REML",na.action=na.omit)
cibind$time=as.factor(cibind$time)
summary(m)
anova(m)
lsmeans(m, list(pairwise ~ valence*arousal*time), adjust = "tukey")

#NA>PC negativity
#m = lme(h ~ arousal*valence*time, random=list(~1|id,~1|topic),data=cibind,method="REML",na.action=na.omit)
m = lme(h ~ arousal*valence*time*topic, random=list(~1|id,~1|id),data=cibind,method="REML",na.action=na.omit)
summary(m)
anova(m)
lsmeans(m, list(pairwise ~ arousal*valence), adjust = "tukey")
aggregate(cibind$h,by=list(cibind$valence),mean,na.rm=T)
aggregate(cibind$h,by=list(cibind$arousal),mean,na.rm=T)

#ploting
dNP=aggregate(cibind$h,by=list(cibind$valence,cibind$time),mean,na.rm=T)
colnames(dNP)=c('valence','time','Corrugator')
dNP$time=as.numeric(dNP$time)
ggplot(data = dNP, aes(x=time, y=Corrugator)) + geom_line(aes(colour=valence))

dAC=aggregate(cibind$h,by=list(cibind$arousal,cibind$time),mean,na.rm=T)
colnames(dAC)=c('arousal','time','Corrugator')
dAC$time=as.numeric(dAC$time)
ggplot(data = dAC, aes(x=time, y=Corrugator)) + geom_line(aes(colour=arousal))

dx=aggregate(cibind$h,by=list(cibind$arousal,cibind$valence,cibind$time),mean,na.rm=T)
colnames(dx)=c('arousal','valence','time','Corrugator')
dx$time=as.numeric(dx$time)
dx$arousal=ifelse(dx$arousal=='A','Arousing content','Calm content')
dx$valence=ifelse(dx$valence=='P','Positive content','Negative content')
ggplot(dx, aes(x = time, y = Corrugator,colour = valence)) +geom_line() +facet_grid(. ~ arousal) + 
  theme_classic()+  xlab("Time (seconds)") + ylab("Corrugator activity (μ Volts)")

#####eda, indicates arousal#########
#descriptive
lapply(1:24,function(x){aggregate(select(d,num_range('eda_S_',x)),by=list(d$valence),mean,na.rm=T)})
lapply(1:24,function(x){aggregate(select(d,num_range('eda_S_',x)),by=list(d$arousal),mean,na.rm=T)})

pos=subset(d,d$valence=='P')
lapply(1:24,function(x){mean(select(pos,num_range('eda_S_',x)),na.rm=T)})
mean(pos[,100:123],na.rm=T)

colMeans(pos[sapply(pos, is.numeric)],na.rm=T)



edabind=cbind(select(d,num_range("eda_S_", 1:24)),condition=d$condition,id=d$id,
             valence=d$valence,arousal=d$arousal,topic=d$topic)
colnames(edabind)[1:24]=c('eda1','eda2','eda3','eda4','eda5','eda6','eda7','eda8','eda9',
                         'eda10','eda11','eda12','eda13','eda14','eda15','eda16','eda17','eda18',
                         'eda19','eda20','eda21','eda22','eda23','eda24')
edabind=reshape(edabind,varying=1:24,idvar = c("id",'topic','condition','valence','arousal'), direction = "long",v.names='h')



m = lme(h ~ valence*arousal*time,  random=list(id=~1, topic=~1), data=edabind,method="REML",na.action=na.omit)
summary(m)
anova(m)
edabind$time=as.factor(edabind$time)
lsmeans(m, list(pairwise ~ valence*arousal*time), adjust = "tukey")


summary(lmer(h ~ arousal*valence*time+(1|id)+(1|topic),data=edabind,na.action=na.omit))
m=lmer(h ~ arousal*valence*time+(1|id)+(1|topic),data=edabind,na.action=na.omit)
summary(glht(m, linfct=mcp(arousal = "Tukey")), test = adjusted(type = "bonferroni"))
summary(glht(m, linfct=mcp(valence = "Tukey")), test = adjusted(type = "bonferroni"))

m = lme(h ~ arousal*valence*time*topic, random=list(~1|id),data=edabind,method="REML",na.action=na.omit)
anova(m)
summary(m)

aggregate(edabind$h,by=list(edabind$valence),mean,na.rm=T)
aggregate(edabind$h,by=list(edabind$arousal),mean,na.rm=T)
aggregate(edabind$h,by=list(edabind$topic),mean,na.rm=T)


#ploting
dNP=aggregate(edabind$h,by=list(edabind$valence,edabind$time),mean,na.rm=T)
colnames(dNP)=c('valence','time','eda')
dNP$time=as.numeric(dNP$time)
ggplot(data = dNP, aes(x=time, y=eda)) + geom_line(aes(colour=valence))

dAC=aggregate(edabind$h,by=list(edabind$arousal,edabind$time),mean,na.rm=T)
colnames(dAC)=c('arousal','time','eda')
dAC$time=as.numeric(dAC$time)
ggplot(data = dAC, aes(x=time, y=eda)) + geom_line(aes(colour=arousal))

dx=aggregate(edabind$h,by=list(edabind$arousal,edabind$valence,edabind$time),mean,na.rm=T)
colnames(dx)=c('arousal','valence','time','eda')
dx$time=as.numeric(dx$time)
dx$arousal=ifelse(dx$arousal=='A','Arousing content','Calm content')
dx$valence=ifelse(dx$valence=='P','Positive content','Negative content')
ggplot(dx, aes(x = time, y = eda,colour = valence)) +geom_line() +facet_grid(. ~ arousal) + 
  theme_classic()+  xlab("Time (seconds)") + ylab("EDA (μ sem)")

dx=aggregate(edabind$h,by=list(edabind$time),mean,na.rm=T)
colnames(dx)=c('arousal','valence','time','eda')
dx$time=as.numeric(dx$time)
dx$arousal=ifelse(dx$arousal=='A','Arousing content','Calm content')
dx$valence=ifelse(dx$valence=='P','Positive content','Negative content')
ggplot(dx, aes(x = time, y = eda,colour = valence)) +geom_line() +facet_grid(. ~ arousal) + 
  theme_classic()+  xlab("Time (seconds)") + ylab("EDA (μ sem)")

#####eda by covariates####
edabind=cbind(select(d,num_range("eda_S_", 1:24)),condition=d$condition,id=d$id,
              valence=d$valence,arousal=d$arousal,topic=d$topic,attribution=d$attriF,
              identification=d$identF)
colnames(edabind)[1:24]=c('eda1','eda2','eda3','eda4','eda5','eda6','eda7','eda8','eda9',
                          'eda10','eda11','eda12','eda13','eda14','eda15','eda16','eda17','eda18',
                          'eda19','eda20','eda21','eda22','eda23','eda24')
edabind=reshape(edabind,varying=1:24,idvar = c("id",'topic','condition','valence','arousal'), direction = "long",v.names='h')



m = lme(h ~ valence *arousal* attribution*time,  random=list(id=~1, topic=~1), data=edabind,method="REML",na.action=na.omit)
summary(m)
anova(m)
edabind$time=as.factor(edabind$time)
lsmeans(m, list(pairwise ~ arousal), adjust = "tukey")

edabind$time=as.factor(edabind$time)
lsmeans(m, list(pairwise ~ time), adjust = "tukey")
lsmeans(m, list(pairwise ~ arousal), adjust = "tukey")
lsmeans(m, list(pairwise ~ valence*attribution*time), adjust = "tukey")
lsmeans(m, list(pairwise ~ arousal*attribution*time), adjust = "tukey")

lsmeans(m, list(pairwise ~ valence*time), adjust = "tukey")
lsmeans(m, list(pairwise ~ arousal*time), adjust = "tukey")

#####liwc output as DVs#####

#####LIWC: word count#####
m = lme(WC ~ valence*arousal,  random=list(id=~1, topic=~1), data=d,method="REML",na.action=na.omit)
summary(m)
anova(m)
lsmeans(m, list(pairwise ~ valence*arousal), adjust = "tukey")

m = lme(WC ~ valence*arousal*identF,  random=list(id=~1, topic=~1), data=d,method="REML",na.action=na.omit)
summary(m)
anova(m)
lsmeans(m, list(pairwise ~ valence*arousal), adjust = "tukey")

m = lme(WC ~ identF,  random=list(id=~1, topic=~1), data=d,method="REML",na.action=na.omit)
anova(m)
lsmeans(m, list(pairwise ~ identF), adjust = "tukey")

m = lme(WC ~ valence*arousal+empathy,  random=list(id=~1, topic=~1), data=d,method="REML",na.action=na.omit)
anova(m)

m = lme(WC ~ valence * arousal*attriF,  random=list(id=~1, topic=~1), data=d,method="REML",na.action=na.omit)
anova(m)
lsmeans(m, list(pairwise ~ valence*arousal), adjust = "tukey")

summary(lmer(WC ~ arousal*valence+(1|id)+(1|topic),data=d,na.action=na.omit))
m=lmer(WC ~ arousal*valence+(1|id)+(1|topic),data=d,na.action=na.omit)
summary(glht(m, linfct=mcp(arousal = "Tukey")), test = adjusted(type = "bonferroni"))

m = lme(WC ~ arousal*valence*topic, random=list(~1|id),data=d,method="REML",na.action=na.omit)
summary(m)
anova(m)
lsmeans(m, list(pairwise ~ arousal), adjust = "tukey")

summary(lmer(WC ~ arousal*valence+heart+oo+ci+eda+(1|id)+(1|topic),data=d,na.action=na.omit))

##ploting
dNP=aggregate(d$WC,by=list(d$valence,d$arousal),mean,na.rm=T)
colnames(dNP)=c('valence','arousal','word_count')
sd=aggregate(d$WC,by=list(d$valence,d$arousal),sd,na.rm=T)
dNP$sd=sd$x
dNP$arousal=ifelse(dNP$arousal=='A','Arousing content','Calm content')
dNP$valence=ifelse(dNP$valence=='P','Positive content','Negative content')

dodge <- position_dodge(width = 0.9)
ggplot(dNP, aes(x = interaction(valence, arousal), y = word_count, fill = arousal)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymax = word_count + sd, ymin = word_count - sd), position = dodge, width = 0.2)+
  xlab("Interaction of Valence by Arousal") + ylab("Word count")+
  scale_x_discrete(labels=c("Negative-Arousing","Positive-Arousing","Negative-Calm","Positive-Calm"))

##########LIWC: cognitive processing########
m = lme(cogproc ~ valence*arousal,  random=list(id=~1, topic=~1), data=d,method="REML",na.action=na.omit)
summary(m)
anova(m)
lsmeans(m, list(pairwise ~  valence), adjust = "tukey")


m = lme(cogproc ~ identF,  random=list(id=~1, topic=~1), data=d,method="REML",na.action=na.omit)
anova(m)
m = lme(cogproc ~ valence*arousal*attriF,  random=list(id=~1, topic=~1), data=d,method="REML",na.action=na.omit)
anova(m)
lsmeans(m, list(pairwise ~  valence), adjust = "tukey")

m = lme(cogproc ~ valence*arousal*identF,  random=list(id=~1, topic=~1), data=d,method="REML",na.action=na.omit)
anova(m)
lsmeans(m, list(pairwise ~  valence), adjust = "tukey")

summary(lmer(cogproc ~ arousal*valence+(1|id)+(1|topic),data=d,na.action=na.omit))
m=lmer(cogproc ~ arousal*valence+(1|id)+(1|topic),data=d,na.action=na.omit)
summary(glht(m, linfct=mcp(valence = "Tukey")), test = adjusted(type = "bonferroni"))
lsmeans(m, list(pairwise ~ arousal*valence), adjust = "tukey")

m = lme(cogproc ~ arousal*valence*topic, random=list(~1|id),data=d,method="REML",na.action=na.omit)
summary(m)
anova(m)
lsmeans(m, list(pairwise ~ valence*arousal), adjust = "tukey")

summary(lmer(cogproc ~ arousal*valence+heart+oo+ci+eda+(1|id)+(1|topic),data=d,na.action=na.omit))

##ploting
dNP=aggregate(d$cogproc,by=list(d$valence,d$arousal),mean,na.rm=T)
colnames(dNP)=c('valence','arousal','cognitive_processing')
sd=aggregate(d$cogproc,by=list(d$valence,d$arousal),sd,na.rm=T)
dNP$sd=sd$x
dNP$arousal=ifelse(dNP$arousal=='A','Arousing content','Calm content')
dNP$valence=ifelse(dNP$valence=='P','Positive content','Negative content')

dodge <- position_dodge(width = 0.9)
ggplot(dNP, aes(x = interaction(valence, arousal), y = cognitive_processing, fill = arousal)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymax = cognitive_processing + sd, ymin = cognitive_processing - sd), position = dodge, width = 0.2)+
  xlab("Interaction of Valence by Arousal") + ylab("Cognitive Processing Words")+
  scale_x_discrete(labels=c("Negative-Arousing","Positive-Arousing","Negative-Calm","Positive-Calm"))

###########LIWC: self-disclosure###########
m = lme(i ~ valence*arousal,  random=list(id=~1, topic=~1), data=d,method="REML",na.action=na.omit)
summary(m)
anova(m)
lsmeans(m, list(pairwise ~ arousal), adjust = "tukey")



m = lme(i ~ valence*arousal*attriF,  random=list(id=~1, topic=~1), data=d,method="REML",na.action=na.omit)
anova(m)
lsmeans(m, list(pairwise ~ arousal), adjust = "tukey")

m = lme(i ~ valence*arousal*identF,  random=list(id=~1, topic=~1), data=d,method="REML",na.action=na.omit)
anova(m)
lsmeans(m, list(pairwise ~ arousal), adjust = "tukey")

##ploting
dNP=aggregate(d$i,by=list(d$valence,d$arousal),mean,na.rm=T)
colnames(dNP)=c('valence','arousal','SelfDisclosure')
sd=aggregate(d$i,by=list(d$valence,d$arousal),sd,na.rm=T)
dNP$sd=sd$x
dNP$arousal=ifelse(dNP$arousal=='A','Arousing content','Calm content')
dNP$valence=ifelse(dNP$valence=='P','Positive content','Negative content')

dodge <- position_dodge(width = 0.9)
ggplot(dNP, aes(x = interaction(valence, arousal), y = SelfDisclosure, fill = arousal)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymax = SelfDisclosure + sd, ymin = SelfDisclosure - sd), position = dodge, width = 0.2)+
  xlab("Interaction of Valence by Arousal") + ylab("self-disclosure")+
  scale_x_discrete(labels=c("Negative-Arousing","Positive-Arousing","Negative-Calm","Positive-Calm"))

#############LIWC: complex words###########
m = lme(Sixltr ~ valence*arousal,  random=list(id=~1, topic=~1), data=d,method="REML",na.action=na.omit)
summary(m)
anova(m)
##ploting
dNP=aggregate(d$i,by=list(d$valence,d$arousal),mean,na.rm=T)
colnames(dNP)=c('valence','arousal','SelfDisclosure')
sd=aggregate(d$i,by=list(d$valence,d$arousal),sd,na.rm=T)
dNP$sd=sd$x
dNP$arousal=ifelse(dNP$arousal=='A','Arousing content','Calm content')
dNP$valence=ifelse(dNP$valence=='P','Positive content','Negative content')

dodge <- position_dodge(width = 0.9)
ggplot(dNP, aes(x = interaction(valence, arousal), y = SelfDisclosure, fill = arousal)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymax = SelfDisclosure + sd, ymin = SelfDisclosure - sd), position = dodge, width = 0.2)+
  xlab("Interaction of Valence by Arousal") + ylab("self-disclosure")+
  scale_x_discrete(labels=c("Negative-Arousing","Positive-Arousing","Negative-Calm","Positive-Calm"))

#########LIWC: positive emotion words##########
m = lme(posemo ~ valence*arousal,  random=list(id=~1, topic=~1), data=d,method="REML",na.action=na.omit)
summary(m)
anova(m)
aggregate(d$posemo,by=list(d$valence),mean)
lsmeans(m, list(pairwise ~ valence), adjust = "tukey")

m = lme(posemo ~ valence*arousal*attriF,  random=list(id=~1, topic=~1), data=d,method="REML",na.action=na.omit)
summary(m)
anova(m)
lsmeans(m, list(pairwise ~ valence*attriF), adjust = "tukey")

#########LIWC: negative emotion words##########
m = lme(negemo ~ valence*arousal,  random=list(id=~1, topic=~1), data=d,method="REML",na.action=na.omit)
summary(m)
anova(m)
lsmeans(m, list(pairwise ~ valence), adjust = "tukey")

m = lme(negemo ~ valence*arousal*attriF,  random=list(id=~1, topic=~1), data=d,method="REML",na.action=na.omit)
summary(m)
anova(m)
lsmeans(m, list(pairwise ~ arousal*attriF), adjust = "tukey")

#########LIWC: they words##########
m = lme(informal ~ valence*arousal,  random=list(id=~1, topic=~1), data=d,method="REML",na.action=na.omit)
summary(m)
anova(m)
aggregate(d$shehe,by=list(d$valence),mean)


######message rating analysis##########
m = lme(arouse.x ~ valence*arousal,  random=list(id=~1, topic=~1), data=d,method="REML",na.action=na.omit)
anova(m)
aggregate(d$arouse.y,by=list(d$valence,d$arousal),mean)
lsmeans(m, list(pairwise ~ valence), adjust = "tukey")

m = lme(arouse.x ~ valence*arousal*attriF,  random=list(id=~1, topic=~1), data=d,method="REML",na.action=na.omit)
anova(m)
aggregate(d$arouse.y,by=list(d$valence,d$arousal),mean)
lsmeans(m, list(pairwise ~ valence*attriF), adjust = "tukey")


m = lme(positive.y ~ valence*arousal,  random=list(id=~1, topic=~1), data=d,method="REML",na.action=na.omit)
anova(m)
summary(m)
aggregate(d$positive.x,by=list(d$valence,d$arousal),mean)
lsmeans(m, list(pairwise ~ valence*arousal), adjust = "tukey")

m = lme(positive.y ~ valence*arousal*attriF,  random=list(id=~1, topic=~1), data=d,method="REML",na.action=na.omit)
anova(m)
summary(m)
aggregate(d$positive.x,by=list(d$valence,d$arousal),mean)
lsmeans(m, list(pairwise ~ valence*arousal), adjust = "tukey")


m = lme(negative.y ~ valence*arousal,  random=list(id=~1, topic=~1), data=d,method="REML",na.action=na.omit)
anova(m)
aggregate(d$negative.x,by=list(d$valence,d$arousal),mean)
lsmeans(m, list(pairwise ~ valence*arousal), adjust = "tukey")

m = lme(negative.y ~ valence*arousal*attriF,  random=list(id=~1, topic=~1), data=d,method="REML",na.action=na.omit)
anova(m)
aggregate(d$negative.x,by=list(d$valence,d$arousal),mean)
lsmeans(m, list(pairwise ~ valence*arousal), adjust = "tukey")

##########attribution as DV##########
m = lme(attribution ~ valence*arousal,  random=list(id=~1, topic=~1), data=d,method="REML",na.action=na.omit)
anova(m)
summary(m)
aggregate(d$attribution,by=list(d$valence,d$arousal),mean)

m = lme(ident ~ valence*arousal,  random=list(id=~1, topic=~1), data=d,method="REML",na.action=na.omit)
anova(m)
summary(m)
aggregate(d$ident,by=list(d$arousal),mean)


