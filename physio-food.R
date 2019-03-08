rm(list=ls())
setwd("~/Desktop/physio/food")
library(dplyr)
library(tidyr)
library(reshape2)
library(data.table)
library(car)
library(lme4)
library(lsmeans)
library(ggplot2)

library(lavaan)
library(nlme)
library(lsr)
library(Publish)
library(lava)


##########
if(!require(multcomp)){install.packages("multcomp")}
if(!require(multcompView)){install.packages("multcompView")}

if(!require(lmerTest)){install.packages("lmerTest")}
if(!require(TukeyC)){install.packages("TukeyC")}

load('food.Rda')
save(list=c('d','dp'),file='food.Rda')
write.csv(d,file='food.csv')
write.csv(dp,file='food_with_personality.csv')

#d: food physio data with 12 seconds of reading
#dp: d data with personality in the food study

############create composite scores##########
d$heart1=rowMeans (select(d,num_range("heart_S1_", 1:24)),na.rm=T)
d$heart2=rowMeans(select(d,num_range("heart_S2_", 1:24)),na.rm=T)

d$oo1=rowMeans(select(d,num_range("oo_S1_", 1:24)),na.rm=T)
d$oo2=rowMeans(select(d,num_range("oo_S2_", 1:24)),na.rm=T)

d$ci1=rowMeans(select(d,num_range("ci_S1_", 1:24)),na.rm=T)
d$ci2=rowMeans(select(d,num_range("ci_S2_", 1:24)),na.rm=T)

d$eda1=rowMeans(select(d,num_range("eda_S1_", 1:24)),na.rm=T)
d$eda2=rowMeans(select(d,num_range("eda_S2_", 1:24)),na.rm=T)

d$valence=ifelse(d$condition==1|d$condition==2,'negative','positive')
d$support=ifelse(d$condition==1|d$condition==4,'supportive','critical')


Anova(lm(heart1~valence,data=d),type=c(2))
Anova(lm(heart2~valence*support,data=d),type=c(2))
Anova(lm(oo1~valence,data=d),type=c(2))
Anova(lm(oo2~valence*support,data=d),type=c(2))
Anova(lm(ci1~valence*support,data=d),type=c(2))
Anova(lm(ci2~valence*support,data=d),type=c(2))
Anova(lm(eda1~valence,data=d),type=c(2))
Anova(lm(eda2~valence*support,data=d),type=c(2))

########posts heart######

heartlong=data.frame(select(d,num_range("heart_S1_", 1:24)),condition=d$condition,id=d$id,
                     valence=d$valence,support=d$support)
colnames(heartlong)[1:24]=c('h1','h2','h3','h4','h5','h6','h7','h8','h9',
                            'h10','h11','h12','h13','h14','h15','h16','h17','h18',
                            'h19','h20','h21','h22','h23','h24')
heartlong=reshape(heartlong,varying=1:24,idvar = c("id",'condition','valence','support'), direction = "long",v.names='h')

m = lme(h ~ valence, random=~1|time, data=heartlong,method="REML",na.action=na.omit)
summary(m)
anova(m)

dNP=aggregate(heartlong$h,by=list(heartlong$valence,heartlong$time),mean,na.rm=T)
colnames(dNP)=c('valence','time','heart')
dNP$time=as.numeric(dNP$time)
ggplot(data = dNP, aes(x=time, y=heart)) + geom_line(aes(colour=valence))

########comments heart######
heartlong=data.frame(select(d,num_range("heart_S2_", 1:24)),condition=d$condition,id=d$id,
                     valence=d$valence,support=d$support)
colnames(heartlong)[1:24]=c('h1','h2','h3','h4','h5','h6','h7','h8','h9',
                            'h10','h11','h12','h13','h14','h15','h16','h17','h18',
                            'h19','h20','h21','h22','h23','h24')
heartlong=reshape(heartlong,varying=1:24,idvar = c("id",'condition','valence','support'), direction = "long",v.names='h')

m = lme(h ~ support*valence*time, random=~1|id,data=heartlong,method="REML",na.action=na.omit)
anova(m)
heartlong$time=as.factor(heartlong$time)
lsmeans(m, list(pairwise ~ valence*time*support), adjust = "tukey")

summary(aov(h ~ valence + Error(id/(valence)), data=heartlong))
summary(ci.mean(h ~ support*valence*time,data=heartlong),format="(u,l)",digits=9,se=TRUE)



dNP=aggregate(heartlong$h,by=list(heartlong$support,heartlong$time),mean,na.rm=T)
colnames(dNP)=c('support','time','heart')
dNP$time=as.numeric(dNP$time)
ggplot(data = dNP, aes(x=time, y=heart)) + geom_line(aes(colour=support))
#plot interaction
dx=aggregate(heartlong$h,by=list(heartlong$support,heartlong$valence,heartlong$time),mean,na.rm=T)
colnames(dx)=c('support','valence','time','heart')
dx$time=as.numeric(dx$time)
ggplot(dx, aes(x = time, y = heart,colour = support)) +geom_line() +facet_grid(. ~ valence) + 
  theme_classic()+  xlab("Time (seconds)") + ylab("heart")+theme(legend.position="bottom")
  
ggplot(dx, aes(x = time, y = heart,colour = support)) +geom_line() +facet_grid(. ~ valence) + 
  theme_classic()+  xlab("Time") + ylab("Heart")+theme(legend.position="bottom")+
  scale_color_manual(values=c("red", "blue"))+
  labs(color = "Comment valence")

########posts oo######
oobind=data.frame(select(d,num_range("oo_S1_", 1:24)),condition=d$condition,id=d$id,
                  valence=d$valence,support=d$support)
colnames(oobind)[1:24]=c('h1','h2','h3','h4','h5','h6','h7','h8','h9',
                         'h10','h11','h12','h13','h14','h15','h16','h17','h18',
                         'h19','h20','h21','h22','h23','h24')
oobind=reshape(oobind,varying=1:24,idvar = c("id",'condition','valence','support'), direction = "long",v.names='h')

m = lme(h ~ valence*time, random=~1|id,data=oobind,method="REML",na.action=na.omit)
anova(m)
dNP=aggregate(oobind$h,by=list(oobind$valence,oobind$time),mean,na.rm=T)
colnames(dNP)=c('valence','time','oo')
dNP$time=as.numeric(dNP$time)
ggplot(data = dNP, aes(x=time, y=oo)) + 
  geom_line(aes(colour=valence))+theme_bw()+
  scale_color_manual(values=c("red", "blue"))

##getting confidence interval
ci.mean(h ~ valence*time,data=oobind)
summary(ci.mean(h ~ valence*time,data=oobind),format="(u,l)",digits=9,se=TRUE)

#adding se to the graph
summary(m)
oobind$time=as.factor(oobind$time)
se=lsmeans(m, list(pairwise ~ valence*time), adjust = "tukey")
#graph=se$`lsmeans of valence, time`
graph=read.csv('OO of story.csv',header=T)
dNP$min=graph$lower.CL
dNP$max=graph$upper.CL
dNP$se=graph$SE
pd <- position_dodge(0.1)
ggplot(data = dNP, aes(x=time, y=oo)) + 
  geom_line(aes(colour=valence))+
  geom_errorbar(aes(ymin=oo-se, ymax=oo+se), width=.1, position=pd) 

########comments oo######
oobind=data.frame(select(d,num_range("oo_S2_", 1:24)),condition=d$condition,id=d$id,
                  valence=d$valence,support=d$support)
colnames(oobind)[1:24]=c('h1','h2','h3','h4','h5','h6','h7','h8','h9',
                         'h10','h11','h12','h13','h14','h15','h16','h17','h18',
                         'h19','h20','h21','h22','h23','h24')
oobind=reshape(oobind,varying=1:24,idvar = c("id",'condition','valence','support'), direction = "long",v.names='h')

m = lme(h ~ support*time*valence, random=~1|id,data=oobind,method="REML",na.action=na.omit)
anova(m)
oobind$time=as.factor(oobind$time)
lsmeans(m, list(pairwise ~ valence*time*support), adjust = "tukey")
summary(ci.mean(h ~ support*valence*time,data=oobind),format="(u,l)",digits=9,se=TRUE)


##comment oo with demographics
oobind=data.frame(select(dp,num_range("oo_S2_", 1:24)),condition=dp$condition,id=dp$id,
                  valence=dp$valence,support=dp$support,similarity=dp$similarity_health)
colnames(oobind)[1:24]=c('h1','h2','h3','h4','h5','h6','h7','h8','h9',
                         'h10','h11','h12','h13','h14','h15','h16','h17','h18',
                         'h19','h20','h21','h22','h23','h24')
oobind=reshape(oobind,varying=1:24,idvar = c("id",'condition','valence','support','similarity'), direction = "long",v.names='h')

m = lme(h ~ support*time*valence*similarity, random=~1|id,data=oobind,method="REML",na.action=na.omit)
anova(m)


#experimenting with new methods start#####
summary(aov(h~support*valence+Error(factor(time)),oobind))

m <- aov(h ~ support*valence*time + Error(id/(time)) + support*valence, data=oobind)
summary(m)
model.tables(m)
summary(aov(h ~ support*valence*time + Error(id/time), data=oobind))

library(ez)
m=ezANOVA(oobind, dv=.(h), wid=(id),within=.(time), between=.(support,valence),detailed = T)

ezANOVA(oobind, dv=h, wid=id, within=.(time, id), between=.(support,valence), detailed=T)

print(m)
summary(m)

options(contrasts = c("contr.sum","contr.poly"))
Anv3 = lm(cbind(oo_S2_1, oo_S2_2, oo_S2_3, oo_S2_4,oo_S2_5, oo_S2_6, 
                oo_S2_7, oo_S2_8,oo_S2_9, oo_S2_10, oo_S2_11, oo_S2_12,
                oo_S2_13, oo_S2_14,oo_S2_15, oo_S2_16, oo_S2_17, oo_S2_18,
                oo_S2_19, oo_S2_20,oo_S2_21, oo_S2_22, oo_S2_23, oo_S2_24)~valence*support,data=d)
within_time=factor(c('oo_S2_1', 'oo_S2_2', 'oo_S2_3', 'oo_S2_4','oo_S2_5', 'oo_S2_6', 
                'oo_S2_7', 'oo_S2_8','oo_S2_9', 'oo_S2_10', 'oo_S2_11', 'oo_S2_12',
                'oo_S2_13', 'oo_S2_14','oo_S2_15', 'oo_S2_16', 'oo_S2_17', 'oo_S2_18',
                'oo_S2_19', 'oo_S2_20','oo_S2_21', 'oo_S2_22', 'oo_S2_23', 'oo_S2_24' ))
Anv4 = Anova(Anv3, idata=data.frame(within_time), idesign=~within_time, type=3,data=d)
summary(Anv4, multivariate=T)

library(lme4)
library(lmerTest)
lmeModel = lmer(h ~ support*valence*time + (1|id), data=oobind)
anova(lmeModel)
summary(lmeModel)

lm1 <- lme( h ~ support*valence*time, random=~1|id, method="REML", data=oobind )
Anova( lm1, type=3 )
##experimenting with new methods end####



dNP=aggregate(oobind$h,by=list(oobind$support,oobind$time),mean,na.rm=T)
colnames(dNP)=c('support','time','oo')
dNP$time=as.numeric(dNP$time)
ggplot(data = dNP, aes(x=time, y=oo)) + geom_line(aes(colour=support))
#plot interaction
dx=aggregate(oobind$h,by=list(oobind$support,oobind$valence,oobind$time),mean,na.rm=T)
colnames(dx)=c('support','valence','time','oo')
dx$time=as.numeric(dx$time)
ggplot(dx, aes(x = time, y = oo,colour = support)) +geom_line() +facet_grid(. ~ valence) + 
  theme_bw()+  xlab("Time") + ylab("OO")+theme(legend.position="bottom")+
  scale_color_manual(values=c("red", "blue"))+
  labs(color = "Comment valence")


oobind$time=as.factor(oobind$time)
lsmeans(m, list(pairwise ~ support*valence*time), adjust = "tukey")


########posts eda######
edabind=data.frame(select(d,num_range("eda_S1_", 1:24)),condition=d$condition,id=d$id,
                   valence=d$valence,support=d$support)
colnames(edabind)[1:24]=c('h1','h2','h3','h4','h5','h6','h7','h8','h9',
                          'h10','h11','h12','h13','h14','h15','h16','h17','h18',
                          'h19','h20','h21','h22','h23','h24')
edabind=reshape(edabind,varying=1:24,idvar = c("id",'condition','valence','support'), direction = "long",v.names='h')

m = lme(h ~ valence*time, random=~1|id,data=edabind,method="REML",na.action=na.omit)
anova(m)
edabind$time=as.factor(edabind$time)
lsmeans(m, list(pairwise ~ valence*time), adjust = "tukey")
dNP=aggregate(edabind$h,by=list(edabind$valence,edabind$time),mean,na.rm=T)
colnames(dNP)=c('valence','time','eda')
dNP$time=as.numeric(dNP$time)
ggplot(data = dNP, aes(x=time, y=eda)) + geom_line(aes(colour=valence))+
  theme_bw()+
  scale_color_manual(values=c("red", "blue"))

#se and ci
summary(ci.mean(h ~ valence*time,data=edabind),format="(u,l)",digits=9,se=TRUE)


########comments eda######
edabind=data.frame(select(d,num_range("eda_S2_", 1:24)),condition=d$condition,id=d$id,
                   valence=d$valence,support=d$support)
colnames(edabind)[1:24]=c('h1','h2','h3','h4','h5','h6','h7','h8','h9',
                          'h10','h11','h12','h13','h14','h15','h16','h17','h18',
                          'h19','h20','h21','h22','h23','h24')
edabind=reshape(edabind,varying=1:24,idvar = c("id",'condition','valence','support'), direction = "long",v.names='h')

m = lme(h ~ support*time*valence, random=~1|id,data=edabind,method="REML",na.action=na.omit)
anova(m)
edabind$time=as.factor(edabind$time)
lsmeans(m, list(pairwise ~ valence*time*support), adjust = "tukey")
summary(ci.mean(h ~ support*valence*time,data=edabind),format="(u,l)",digits=9,se=TRUE)

dNP=aggregate(edabind$h,by=list(edabind$support,edabind$time),mean,na.rm=T)
colnames(dNP)=c('support','time','eda')
dNP$time=as.numeric(dNP$time)
ggplot(data = dNP, aes(x=time, y=eda)) + geom_line(aes(colour=support))

#plot interaction
dx=aggregate(edabind$h,by=list(edabind$support,edabind$valence,edabind$time),mean,na.rm=T)
colnames(dx)=c('support','valence','time','eda')
dx$time=as.numeric(dx$time)
ggplot(dx, aes(x = time, y = eda,colour = support)) +geom_line() +facet_grid(. ~ valence) + 
  theme_bw()+  xlab("Time (seconds)") + ylab("EDA (μ sem)")+theme(legend.position="bottom")+
  scale_color_manual(values=c("red", "blue"))+
  labs(color = "Comment valence")

ggplot(dx, aes(x = time, y = eda,colour = support)) +geom_line() +facet_grid(. ~ valence) + 
  theme_bw()+  xlab("Time") + ylab("EDA")+theme(legend.position="bottom")+
  scale_color_manual(values=c("red", "blue"))+
  labs(color = "Comment valence")

lsmeans(m, list(pairwise ~ support*valence), adjust = "tukey")

##comment eda with demographics
edabind=data.frame(select(dp,num_range("eda_S2_", 1:24)),condition=dp$condition,id=dp$id,
                   valence=dp$valence,support=dp$support,similarity=dp$similarity_health)
colnames(edabind)[1:24]=c('h1','h2','h3','h4','h5','h6','h7','h8','h9',
                          'h10','h11','h12','h13','h14','h15','h16','h17','h18',
                          'h19','h20','h21','h22','h23','h24')
edabind=reshape(edabind,varying=1:24,idvar = c("id",'condition','valence','support','similarity'), direction = "long",v.names='h')

m = lme(h ~ support*time*valence+similarity, random=~1|id,data=edabind,method="REML",na.action=na.omit)
anova(m)

########posts ci######
cibind=data.frame(select(d,num_range("ci_S1_", 1:24)),condition=d$condition,id=d$id,
                  valence=d$valence,support=d$support)
colnames(cibind)[1:24]=c('h1','h2','h3','h4','h5','h6','h7','h8','h9',
                         'h10','h11','h12','h13','h14','h15','h16','h17','h18',
                         'h19','h20','h21','h22','h23','h24')
cibind=reshape(cibind,varying=1:24,idvar = c("id",'condition','valence','support'), direction = "long",v.names='h')

m = lme(h ~ valence*time, random=~1|id,data=cibind,method="REML",na.action=na.omit)
anova(m)
cibind$time=as.factor(cibind$time)
lsmeans(m, list(pairwise ~ valence*time), adjust = "tukey")
dNP=aggregate(cibind$h,by=list(cibind$valence,cibind$time),mean,na.rm=T)
colnames(dNP)=c('valence','time','ci')
dNP$time=as.numeric(dNP$time)
ggplot(data = dNP, aes(x=time, y=ci)) + geom_line(aes(colour=valence))+theme_bw()+
  scale_color_manual(values=c("red", "blue"))

##se and ci
summary(ci.mean(h ~ valence*time,data=cibind),format="(u,l)",digits=9,se=TRUE)


########comments ci######
cibind=data.frame(select(d,num_range("ci_S2_", 1:24)),condition=d$condition,id=d$id,
                  valence=d$valence,support=d$support)
colnames(cibind)[1:24]=c('h1','h2','h3','h4','h5','h6','h7','h8','h9',
                         'h10','h11','h12','h13','h14','h15','h16','h17','h18',
                         'h19','h20','h21','h22','h23','h24')
cibind=reshape(cibind,varying=1:24,idvar = c("id",'condition','valence','support'), direction = "long",v.names='h')

m = lme(h ~ support*valence*time, random=~1|id,data=cibind,method="REML",na.action=na.omit)
anova(m)
summary(m)
dNP=aggregate(cibind$h,by=list(cibind$support,cibind$time),mean,na.rm=T)
colnames(dNP)=c('support','time','ci')
dNP$time=as.numeric(dNP$time)
ggplot(data = dNP, aes(x=time, y=ci)) + geom_line(aes(colour=support))

#plot interaction
dx=aggregate(cibind$h,by=list(cibind$support,cibind$valence,cibind$time),mean,na.rm=T)
colnames(dx)=c('support','valence','time','ci')
dx$time=as.numeric(dx$time)
ggplot(dx, aes(x = time, y = ci,colour = support)) +geom_line() +facet_grid(. ~ valence) + 
  theme_classic()+  xlab("Time (seconds)") + ylab("corrugator")

#comment ci with demographics
cibind=data.frame(select(dp,num_range("ci_S2_", 1:24)),condition=dp$condition,id=dp$id,
                  valence=dp$valence,support=dp$support, similarity=dp$similarity_health)
colnames(cibind)[1:24]=c('h1','h2','h3','h4','h5','h6','h7','h8','h9',
                         'h10','h11','h12','h13','h14','h15','h16','h17','h18',
                         'h19','h20','h21','h22','h23','h24')
cibind=reshape(cibind,varying=1:24,idvar = c("id",'condition','valence','support','similarity'), direction = "long",v.names='h')

m = lme(h ~ support*time*valence+similarity, random=~1|id,data=cibind,method="REML",na.action=na.omit)
anova(m)

#### merge fit intentions####
intention=read.csv('food-diet-intention.csv',header=T)

d=merge(d,intention, by.x='id',by.y='Subject',incomparables = NA)
d$diet=rowMeans(select(d,num_range("intent_diet_", 1:3)),na.rm=T)
d$exer=rowMeans(select(d,num_range("intent_exer_", 1:3)),na.rm=T)
alpha(select(intention,starts_with('intent_diet')))
alpha(select(intention,starts_with('intent_exer')))

Anova(lm(diet~valence*support,data=d))
summary(lm(diet~valence*support,data=d))
TukeyHSD(aov(diet~valence*support,data=d))
aggregate(d$diet,by=list(d$valence,d$support),mean,na.rm=T)
aggregate(d$diet,by=list(d$valence,d$support),sd,na.rm=T)
summary(ci.mean(diet~valence*support,data=d),format="(u,l)",digits=9,se=TRUE)


#plot diet
dNP=aggregate(d$diet,by=list(d$valence,d$support),mean,na.rm=T)
colnames(dNP)=c('valence','support','diet_intention')
sd=aggregate(d$diet,by=list(d$valence,d$support),sd,na.rm=T)
dNP$sd=sd$x

dodge <- position_dodge(width = 0.9)
ggplot(dNP, aes(x = interaction(valence, support), y = diet_intention, fill = support)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymax = diet_intention + sd, ymin = diet_intention - sd), position = dodge, width = 0.2)+
  xlab("Interaction of Valence by Support") + ylab("Diet_Intention")+theme(legend.position = 'bottom')

ggplot(dNP, aes(x = interaction(valence, support), y = diet_intention, fill = support)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  xlab("Interaction of Valence by Support") + ylab("Diet_Intention")+theme(legend.position = 'bottom')+
  theme_bw()+
  scale_fill_manual("Comment valence", values = c("critical" = "red", "supportive" = "blue"))+
  theme(legend.position = 'bottom')

#plot exercise
dNP=aggregate(d$exer,by=list(d$valence,d$support),mean,na.rm=T)
colnames(dNP)=c('valence','support','exercise_intention')
sd=aggregate(d$exer,by=list(d$valence,d$support),sd,na.rm=T)
dNP$sd=sd$x

dodge <- position_dodge(width = 0.9)
ggplot(dNP, aes(x = interaction(valence, support), y = exercise_intention, fill = support)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymax = exercise_intention + sd, ymin = exercise_intention - sd), position = dodge, width = 0.2)+
  xlab("Interaction of Valence by Support") + ylab("exercise_intention")



#####clean food personality compiled data####
setwd("~/Desktop/physio/food")
fp=read.csv('food-personality-clean-compiled.csv',header=T)
fp$attribution=rowMeans(select(fp,starts_with('attri')))
alpha(select(fp,starts_with('attri')))

fp$envy=rowMeans(select(fp,starts_with('envy')))
alpha(select(fp,starts_with('envy')))

fp$similarity_general=rowMeans(select(fp,starts_with('similar')))
alpha(select(fp,starts_with('similar')))

fp$similarity_health=rowMeans(select(fp,starts_with('sim')))
alpha(select(fp,starts_with('sim')))

fp$locus_internal_pre=rowMeans(select(fp,ends_with('I_pre')))
alpha(select(fp,ends_with('I_pre')))
alpha(fp[,c('locus_10_I_pre','locus_11_I_pre','locus_2_I_pre','locus_8_I_pre')])#drop 1_I

fp$locus_external_pre=rowMeans(select(fp,ends_with('E_pre')))
alpha(select(fp,ends_with('E_pre')))
alpha(fp[,c('locus_4_E_pre','locus_5_E_pre','locus_6_E_pre','locus_7_E_pre','locus_9_E_pre')])#drop 3_E

fp$locus_internal_post=rowMeans(select(fp,ends_with('I_post')))
alpha(select(fp,ends_with('I_post')))
alpha(fp[,c('locus_10_I_post','locus_11_I_post','locus_2_I_post','locus_8_I_post')])#drop 1_I

fp$locus_external_post=rowMeans(select(fp,ends_with('E_post')))
alpha(select(fp,ends_with('E_post')))
alpha(fp[,c('locus_4_E_post','locus_5_E_post','locus_6_E_post','locus_7_E_post','locus_9_E_post')])#drop 3_E

fa(select(fp,ends_with('_pre')),nfactors=2)
fa(select(fp,ends_with('post')),nfactors=2)

library(car)
fp$sto_rt=recode(fp$sto_rt,"'1'=6;'2'=5;'3'=4;'4'=3;'5'=2;'6'=1")
fp$bod_rt=recode(fp$bod_rt,"'1'=6;'2'=5;'3'=4;'4'=3;'5'=2;'6'=1")
fp$but_rt=recode(fp$but_rt,"'1'=6;'2'=5;'3'=4;'4'=3;'5'=2;'6'=1")
fp$thi_rt=recode(fp$thi_rt,"'1'=6;'2'=5;'3'=4;'4'=3;'5'=2;'6'=1")
fp$hip_rt=recode(fp$hip_rt,"'1'=6;'2'=5;'3'=4;'4'=3;'5'=2;'6'=1")
fp$body_sat=rowMeans(fp[,c('sto_lg','thi_lg','sto_rt','bod_rt','but_rt','hip_lg','thi_rt','but_lg','hip_rt')])
alpha(fp[,c('sto_lg','thi_lg','sto_rt','bod_rt','but_rt','hip_lg','thi_rt','but_lg','hip_rt')])

d=merge(d,fp, by.x='id',by.y='Subject')

##merge demographics
demo=read.csv('food-demo-compiled.csv',header=T)
demo$age=as.numeric(demo$age)
demo$class=as.factor(demo$class)
demo$ethnicity=as.factor(demo$ethnicity)
demo$sex=as.factor(demo$sex)

d=merge(d,demo,by.x='id',by.y='Subject',incomparables = NA)

dp=d


#create pre post test difference for locus of control
dp$locus_internal_diff=dp$locus_internal_post-dp$locus_internal_pre
dp$locus_external_diff=dp$locus_external_post-dp$locus_external_pre
aggregate(dp$locus_internal_diff,by=list(dp$support,dp$valence),mean)
aggregate(dp$locus_external_diff,by=list(dp$support,dp$valence),mean)

Anova(lm(locus_external_diff~support*valence,data=dp),type=c(2))
Anova(lm(locus_internal_diff~support*valence,data=dp),type=c(2))#valence sig 
by(dp$locus_internal_diff,dp$valence,mean)

Anova(lm(dp$locus_internal_post~valence*support,data=dp),type=c(2))#valence sig
by(dp$locus_internal_post,dp$valence,mean)

Anova(lm(dp$locus_external_post~valence*support,data=dp),type=c(2))

#effects on diet intentions by demographics
Anova(lm(diet~support+locus_external_pre,data=dp),type=c(2))#support and locus external sig
by(dp$diet,dp$support,mean)
Anova(lm(diet~support+locus_internal_pre,data=dp),type=c(2))
by(dp$diet,dp$support,mean)

Anova(lm(diet~valence*support+locus_internal_pre,data=dp),type=c(2))
aggregate(d$diet,by=list(d$support,d$valence),mean)

Anova(lm(diet~valence*support,data=dp),type=c(2))

Anova(lm(diet~valence*support+similarity_health,data=dp),type=c(2))
Anova(lm(diet~valence*support+envy,data=dp),type=c(2))
Anova(lm(dp$diet~valence*support,data=dp),type=c(2))
Anova(lm(dp$exer~valence*support,data=dp),type=c(2))

Anova(lm(dp$diet~valence*support+attribution+envy+similarity_health+similarity_general,data=dp),type=c(2))
Anova(lm(dp$exer~valence*support+attribution+envy+similarity_health,data=dp),type=c(2))

Anova(lm(dp$diet~valence*support*similarity_health,data=dp),type=c(2))
aggregate(dp$diet,by=list(dp$valence),mean)
aggregate(dp$diet,by=list(dp$support),mean)

Anova(lm(dp$exer~valence*support*similarity_health,data=dp),type=c(2))
aggregate(dp$exer,by=list(dp$valence),mean)


####effects based on column differences###
d[3,8]=(d[3,4]+d[3,5]+d[3,6]+d[3,7])/4

diff_heart=function(x){output=x-d$heart_B1_6
print(output)}
diff_h=apply(select(d,num_range("heart_S1_", 1:24)),2,diff)

diff_ci=function(x){output=x-d$ci_B1_6
print(output)}
diff_ci=apply(select(d,num_range("ci_S1_", 1:24)),2,diff)

diff_eda=function(x){output=x-d$eda_B1_6
print(output)}
diff_eda=apply(select(d,num_range("eda_S1_", 1:24)),2,diff)

diff_oo=function(x){output=x-d$oo_B1_6
print(output)}
diff_oo=apply(select(d,num_range("oo_S1_", 1:24)),2,diff)


d$heart_D1_1 = d$heart_S1_1 - d$heart_B1_6
d$heart_D1_2 = d$heart_S1_2 - d$heart_B1_6
d$heart_D1_3 = d$heart_S1_3 - d$heart_B1_6
d$heart_D1_4 = d$heart_S1_4 - d$heart_B1_6
d$heart_D1_5 = d$heart_S1_5 - d$heart_B1_6
d$heart_D1_6 = d$heart_S1_6 - d$heart_B1_6
d$heart_D1_7 = d$heart_S1_7 - d$heart_B1_6
d$heart_D1_8 = d$heart_S1_8 - d$heart_B1_6
d$heart_D1_9 = d$heart_S1_9 - d$heart_B1_6
d$heart_D1_10 = d$heart_S1_10 - d$heart_B1_6
d$heart_D1_11 = d$heart_S1_11 - d$heart_B1_6
d$heart_D1_12 = d$heart_S1_12 - d$heart_B1_6
d$heart_D1_13 = d$heart_S1_13 - d$heart_B1_6
d$heart_D1_14 = d$heart_S1_14 - d$heart_B1_6
d$heart_D1_15 = d$heart_S1_15 - d$heart_B1_6
d$heart_D1_16 = d$heart_S1_16 - d$heart_B1_6
d$heart_D1_17 = d$heart_S1_17 - d$heart_B1_6
d$heart_D1_18 = d$heart_S1_18 - d$heart_B1_6
d$heart_D1_19 = d$heart_S1_19 - d$heart_B1_6
d$heart_D1_20 = d$heart_S1_20 - d$heart_B1_6
d$heart_D1_21 = d$heart_S1_21 - d$heart_B1_6
d$heart_D1_22 = d$heart_S1_22 - d$heart_B1_6
d$heart_D1_23 = d$heart_S1_23 - d$heart_B1_6
d$heart_D1_24 = d$heart_S1_24 - d$heart_B1_6



d$oo_D1_1 = d$oo_S1_1 - d$oo_B1_6
d$oo_D1_2 = d$oo_S1_2 - d$oo_B1_6
d$oo_D1_3 = d$oo_S1_3 - d$oo_B1_6
d$oo_D1_4 = d$oo_S1_4 - d$oo_B1_6
d$oo_D1_5 = d$oo_S1_5 - d$oo_B1_6
d$oo_D1_6 = d$oo_S1_6 - d$oo_B1_6
d$oo_D1_7 = d$oo_S1_7 - d$oo_B1_6
d$oo_D1_8 = d$oo_S1_8 - d$oo_B1_6
d$oo_D1_9 = d$oo_S1_9 - d$oo_B1_6
d$oo_D1_10 = d$oo_S1_10 - d$oo_B1_6
d$oo_D1_11 = d$oo_S1_11 - d$oo_B1_6
d$oo_D1_12 = d$oo_S1_12 - d$oo_B1_6
d$oo_D1_13 = d$oo_S1_13 - d$oo_B1_6
d$oo_D1_14 = d$oo_S1_14 - d$oo_B1_6
d$oo_D1_15 = d$oo_S1_15 - d$oo_B1_6
d$oo_D1_16 = d$oo_S1_16 - d$oo_B1_6
d$oo_D1_17 = d$oo_S1_17 - d$oo_B1_6
d$oo_D1_18 = d$oo_S1_18 - d$oo_B1_6
d$oo_D1_19 = d$oo_S1_19 - d$oo_B1_6
d$oo_D1_20 = d$oo_S1_20 - d$oo_B1_6
d$oo_D1_21 = d$oo_S1_21 - d$oo_B1_6
d$oo_D1_22 = d$oo_S1_22 - d$oo_B1_6
d$oo_D1_23 = d$oo_S1_23 - d$oo_B1_6
d$oo_D1_24 = d$oo_S1_24 - d$oo_B1_6

d$ci_D1_1 = d$ci_S1_1 - d$ci_B1_6
d$ci_D1_2 = d$ci_S1_2 - d$ci_B1_6
d$ci_D1_3 = d$ci_S1_3 - d$ci_B1_6
d$ci_D1_4 = d$ci_S1_4 - d$ci_B1_6
d$ci_D1_5 = d$ci_S1_5 - d$ci_B1_6
d$ci_D1_6 = d$ci_S1_6 - d$ci_B1_6
d$ci_D1_7 = d$ci_S1_7 - d$ci_B1_6
d$ci_D1_8 = d$ci_S1_8 - d$ci_B1_6
d$ci_D1_9 = d$ci_S1_9 - d$ci_B1_6
d$ci_D1_10 = d$ci_S1_10 - d$ci_B1_6
d$ci_D1_11 = d$ci_S1_11 - d$ci_B1_6
d$ci_D1_12 = d$ci_S1_12 - d$ci_B1_6
d$ci_D1_13 = d$ci_S1_13 - d$ci_B1_6
d$ci_D1_14 = d$ci_S1_14 - d$ci_B1_6
d$ci_D1_15 = d$ci_S1_15 - d$ci_B1_6
d$ci_D1_16 = d$ci_S1_16 - d$ci_B1_6
d$ci_D1_17 = d$ci_S1_17 - d$ci_B1_6
d$ci_D1_18 = d$ci_S1_18 - d$ci_B1_6
d$ci_D1_19 = d$ci_S1_19 - d$ci_B1_6
d$ci_D1_20 = d$ci_S1_20 - d$ci_B1_6
d$ci_D1_21 = d$ci_S1_21 - d$ci_B1_6
d$ci_D1_22 = d$ci_S1_22 - d$ci_B1_6
d$ci_D1_23 = d$ci_S1_23 - d$ci_B1_6
d$ci_D1_24 = d$ci_S1_24 - d$ci_B1_6

d$eda_D1_1 = d$eda_S1_1 - d$eda_B1_6
d$eda_D1_2 = d$eda_S1_2 - d$eda_B1_6
d$eda_D1_3 = d$eda_S1_3 - d$eda_B1_6
d$eda_D1_4 = d$eda_S1_4 - d$eda_B1_6
d$eda_D1_5 = d$eda_S1_5 - d$eda_B1_6
d$eda_D1_6 = d$eda_S1_6 - d$eda_B1_6
d$eda_D1_7 = d$eda_S1_7 - d$eda_B1_6
d$eda_D1_8 = d$eda_S1_8 - d$eda_B1_6
d$eda_D1_9 = d$eda_S1_9 - d$eda_B1_6
d$eda_D1_10 = d$eda_S1_10 - d$eda_B1_6
d$eda_D1_11 = d$eda_S1_11 - d$eda_B1_6
d$eda_D1_12 = d$eda_S1_12 - d$eda_B1_6
d$eda_D1_13 = d$eda_S1_13 - d$eda_B1_6
d$eda_D1_14 = d$eda_S1_14 - d$eda_B1_6
d$eda_D1_15 = d$eda_S1_15 - d$eda_B1_6
d$eda_D1_16 = d$eda_S1_16 - d$eda_B1_6
d$eda_D1_17 = d$eda_S1_17 - d$eda_B1_6
d$eda_D1_18 = d$eda_S1_18 - d$eda_B1_6
d$eda_D1_19 = d$eda_S1_19 - d$eda_B1_6
d$eda_D1_20 = d$eda_S1_20 - d$eda_B1_6
d$eda_D1_21 = d$eda_S1_21 - d$eda_B1_6
d$eda_D1_22 = d$eda_S1_22 - d$eda_B1_6
d$eda_D1_23 = d$eda_S1_23 - d$eda_B1_6
d$eda_D1_24 = d$eda_S1_24 - d$eda_B1_6


##recode comments



d$heart_D2_1 = d$heart_S2_1 - d$heart_B1_2
d$heart_D2_2 = d$heart_S2_2 - d$heart_B1_2
d$heart_D2_3 = d$heart_S2_3 - d$heart_B1_2
d$heart_D2_4 = d$heart_S2_4 - d$heart_B1_2
d$heart_D2_5 = d$heart_S2_5 - d$heart_B1_2
d$heart_D2_6 = d$heart_S2_6 - d$heart_B1_2
d$heart_D2_7 = d$heart_S2_7 - d$heart_B1_2
d$heart_D2_8 = d$heart_S2_8 - d$heart_B1_2
d$heart_D2_9 = d$heart_S2_9 - d$heart_B1_2
d$heart_D2_10 = d$heart_S2_10 - d$heart_B1_2
d$heart_D2_11 = d$heart_S2_11 - d$heart_B1_2
d$heart_D2_12 = d$heart_S2_12 - d$heart_B1_2
d$heart_D2_13 = d$heart_S2_13 - d$heart_B1_2
d$heart_D2_14 = d$heart_S2_14 - d$heart_B1_2
d$heart_D2_15 = d$heart_S2_15 - d$heart_B1_2
d$heart_D2_16 = d$heart_S2_16 - d$heart_B1_2
d$heart_D2_17 = d$heart_S2_17 - d$heart_B1_2
d$heart_D2_18 = d$heart_S2_18 - d$heart_B1_2
d$heart_D2_19 = d$heart_S2_19 - d$heart_B1_2
d$heart_D2_20 = d$heart_S2_20 - d$heart_B1_2
d$heart_D2_21 = d$heart_S2_21 - d$heart_B1_2
d$heart_D2_22 = d$heart_S2_22 - d$heart_B1_2
d$heart_D2_23 = d$heart_S2_23 - d$heart_B1_2
d$heart_D2_24 = d$heart_S2_24 - d$heart_B1_2



d$oo_D2_1 = d$oo_S2_1 - d$oo_B1_2
d$oo_D2_2 = d$oo_S2_2 - d$oo_B1_2
d$oo_D2_3 = d$oo_S2_3 - d$oo_B1_2
d$oo_D2_4 = d$oo_S2_4 - d$oo_B1_2
d$oo_D2_5 = d$oo_S2_5 - d$oo_B1_2
d$oo_D2_6 = d$oo_S2_6 - d$oo_B1_2
d$oo_D2_7 = d$oo_S2_7 - d$oo_B1_2
d$oo_D2_8 = d$oo_S2_8 - d$oo_B1_2
d$oo_D2_9 = d$oo_S2_9 - d$oo_B1_2
d$oo_D2_10 = d$oo_S2_10 - d$oo_B1_2
d$oo_D2_11 = d$oo_S2_11 - d$oo_B1_2
d$oo_D2_12 = d$oo_S2_12 - d$oo_B1_2
d$oo_D2_13 = d$oo_S2_13 - d$oo_B1_2
d$oo_D2_14 = d$oo_S2_14 - d$oo_B1_2
d$oo_D2_15 = d$oo_S2_15 - d$oo_B1_2
d$oo_D2_16 = d$oo_S2_16 - d$oo_B1_2
d$oo_D2_17 = d$oo_S2_17 - d$oo_B1_2
d$oo_D2_18 = d$oo_S2_18 - d$oo_B1_2
d$oo_D2_19 = d$oo_S2_19 - d$oo_B1_2
d$oo_D2_20 = d$oo_S2_20 - d$oo_B1_2
d$oo_D2_21 = d$oo_S2_21 - d$oo_B1_2
d$oo_D2_22 = d$oo_S2_22 - d$oo_B1_2
d$oo_D2_23 = d$oo_S2_23 - d$oo_B1_2
d$oo_D2_24 = d$oo_S2_24 - d$oo_B1_2

d$ci_D2_1 = d$ci_S2_1 - d$ci_B1_2
d$ci_D2_2 = d$ci_S2_2 - d$ci_B1_2
d$ci_D2_3 = d$ci_S2_3 - d$ci_B1_2
d$ci_D2_4 = d$ci_S2_4 - d$ci_B1_2
d$ci_D2_5 = d$ci_S2_5 - d$ci_B1_2
d$ci_D2_6 = d$ci_S2_6 - d$ci_B1_2
d$ci_D2_7 = d$ci_S2_7 - d$ci_B1_2
d$ci_D2_8 = d$ci_S2_8 - d$ci_B1_2
d$ci_D2_9 = d$ci_S2_9 - d$ci_B1_2
d$ci_D2_10 = d$ci_S2_10 - d$ci_B1_2
d$ci_D2_11 = d$ci_S2_11 - d$ci_B1_2
d$ci_D2_12 = d$ci_S2_12 - d$ci_B1_2
d$ci_D2_13 = d$ci_S2_13 - d$ci_B1_2
d$ci_D2_14 = d$ci_S2_14 - d$ci_B1_2
d$ci_D2_15 = d$ci_S2_15 - d$ci_B1_2
d$ci_D2_16 = d$ci_S2_16 - d$ci_B1_2
d$ci_D2_17 = d$ci_S2_17 - d$ci_B1_2
d$ci_D2_18 = d$ci_S2_18 - d$ci_B1_2
d$ci_D2_19 = d$ci_S2_19 - d$ci_B1_2
d$ci_D2_20 = d$ci_S2_20 - d$ci_B1_2
d$ci_D2_21 = d$ci_S2_21 - d$ci_B1_2
d$ci_D2_22 = d$ci_S2_22 - d$ci_B1_2
d$ci_D2_23 = d$ci_S2_23 - d$ci_B1_2
d$ci_D2_24 = d$ci_S2_24 - d$ci_B1_2

d$eda_D2_1 = d$eda_S2_1 - d$eda_B1_2
d$eda_D2_2 = d$eda_S2_2 - d$eda_B1_2
d$eda_D2_3 = d$eda_S2_3 - d$eda_B1_2
d$eda_D2_4 = d$eda_S2_4 - d$eda_B1_2
d$eda_D2_5 = d$eda_S2_5 - d$eda_B1_2
d$eda_D2_6 = d$eda_S2_6 - d$eda_B1_2
d$eda_D2_7 = d$eda_S2_7 - d$eda_B1_2
d$eda_D2_8 = d$eda_S2_8 - d$eda_B1_2
d$eda_D2_9 = d$eda_S2_9 - d$eda_B1_2
d$eda_D2_10 = d$eda_S2_10 - d$eda_B1_2
d$eda_D2_11 = d$eda_S2_11 - d$eda_B1_2
d$eda_D2_12 = d$eda_S2_12 - d$eda_B1_2
d$eda_D2_13 = d$eda_S2_13 - d$eda_B1_2
d$eda_D2_14 = d$eda_S2_14 - d$eda_B1_2
d$eda_D2_15 = d$eda_S2_15 - d$eda_B1_2
d$eda_D2_16 = d$eda_S2_16 - d$eda_B1_2
d$eda_D2_17 = d$eda_S2_17 - d$eda_B1_2
d$eda_D2_18 = d$eda_S2_18 - d$eda_B1_2
d$eda_D2_19 = d$eda_S2_19 - d$eda_B1_2
d$eda_D2_20 = d$eda_S2_20 - d$eda_B1_2
d$eda_D2_21 = d$eda_S2_21 - d$eda_B1_2
d$eda_D2_22 = d$eda_S2_22 - d$eda_B1_2
d$eda_D2_23 = d$eda_S2_23 - d$eda_B1_2
d$eda_D2_24 = d$eda_S2_24 - d$eda_B1_2

heartlong=data.frame(select(d,num_range("heart_D1_", 1:24)),condition=d$condition,id=d$id,
                     valence=d$valence,support=d$support)
colnames(heartlong)[1:24]=c('h1','h2','h3','h4','h5','h6','h7','h8','h9',
                            'h10','h11','h12','h13','h14','h15','h16','h17','h18',
                            'h19','h20','h21','h22','h23','h24')
heartlong=reshape(heartlong,varying=1:24,idvar = c("id",'condition','valence','support'), direction = "long",v.names='h')

m = lme(h ~ valence*time, random=~1|id,data=heartlong,method="REML",na.action=na.omit)
anova(m)
dNP=aggregate(heartlong$h,by=list(heartlong$valence,heartlong$time),mean,na.rm=T)
colnames(dNP)=c('valence','time','heart')
dNP$time=as.numeric(dNP$time)
ggplot(data = dNP, aes(x=time, y=heart)) + geom_line(aes(colour=valence))

########comments heart######
heartlong=data.frame(select(d,num_range("heart_D2_", 1:24)),condition=d$condition,id=d$id,
                     valence=d$valence,support=d$support)
colnames(heartlong)[1:24]=c('h1','h2','h3','h4','h5','h6','h7','h8','h9',
                            'h10','h11','h12','h13','h14','h15','h16','h17','h18',
                            'h19','h20','h21','h22','h23','h24')
heartlong=reshape(heartlong,varying=1:24,idvar = c("id",'condition','valence','support'), direction = "long",v.names='h')

m = lme(h ~ support*time*valence, random=~1|id,data=heartlong,method="REML",na.action=na.omit)
anova(m)
dNP=aggregate(heartlong$h,by=list(heartlong$support,heartlong$time),mean,na.rm=T)
colnames(dNP)=c('support','time','heart')
dNP$time=as.numeric(dNP$time)
ggplot(data = dNP, aes(x=time, y=heart)) + geom_line(aes(colour=support))
#plot interaction
dx=aggregate(heartlong$h,by=list(heartlong$support,heartlong$valence,heartlong$time),mean,na.rm=T)
colnames(dx)=c('support','valence','time','heart')
dx$time=as.numeric(dx$time)
ggplot(dx, aes(x = time, y = heart,colour = support)) +geom_line() +facet_grid(. ~ valence) + 
  theme_classic()+  xlab("Time (seconds)") + ylab("heart")




########posts oo######
oobind=data.frame(select(d,num_range("oo_D1_", 1:24)),condition=d$condition,id=d$id,
                  valence=d$valence,support=d$support)
colnames(oobind)[1:24]=c('h1','h2','h3','h4','h5','h6','h7','h8','h9',
                         'h10','h11','h12','h13','h14','h15','h16','h17','h18',
                         'h19','h20','h21','h22','h23','h24')
oobind=reshape(oobind,varying=1:24,idvar = c("id",'condition','valence','support'), direction = "long",v.names='h')

m = lme(h ~ valence*time, random=~1|id,data=oobind,method="REML",na.action=na.omit)
anova(m)
dNP=aggregate(oobind$h,by=list(oobind$valence,oobind$time),mean,na.rm=T)
colnames(dNP)=c('valence','time','oo')
dNP$time=as.numeric(dNP$time)
ggplot(data = dNP, aes(x=time, y=oo)) + geom_line(aes(colour=valence))


########comments oo######
oobind=data.frame(select(d,num_range("oo_D2_", 1:24)),condition=d$condition,id=d$id,
                  valence=d$valence,support=d$support)
colnames(oobind)[1:24]=c('h1','h2','h3','h4','h5','h6','h7','h8','h9',
                         'h10','h11','h12','h13','h14','h15','h16','h17','h18',
                         'h19','h20','h21','h22','h23','h24')
oobind=reshape(oobind,varying=1:24,idvar = c("id",'condition','valence','support'), direction = "long",v.names='h')

m = lme(h ~ support*time*valence, random=~1|id,data=oobind,method="REML",na.action=na.omit)
anova(m)
dNP=aggregate(oobind$h,by=list(oobind$support,oobind$time),mean,na.rm=T)
colnames(dNP)=c('support','time','oo')
dNP$time=as.numeric(dNP$time)
ggplot(data = dNP, aes(x=time, y=oo)) + geom_line(aes(colour=support))
#plot interaction
dx=aggregate(oobind$h,by=list(oobind$support,oobind$valence,oobind$time),mean,na.rm=T)
colnames(dx)=c('support','valence','time','oo')
dx$time=as.numeric(dx$time)
ggplot(dx, aes(x = time, y = oo,colour = support)) +geom_line() +facet_grid(. ~ valence) + 
  theme_classic()+  xlab("Time (seconds)") + ylab("OO")

oobind$time=as.factor(oobind$time)
lsmeans(m, list(pairwise ~ support*valence*time), adjust = "tukey")


########posts eda######
edabind=data.frame(select(d,num_range("eda_D1_", 1:24)),condition=d$condition,id=d$id,
                   valence=d$valence,support=d$support)
colnames(edabind)[1:24]=c('h1','h2','h3','h4','h5','h6','h7','h8','h9',
                          'h10','h11','h12','h13','h14','h15','h16','h17','h18',
                          'h19','h20','h21','h22','h23','h24')
edabind=reshape(edabind,varying=1:24,idvar = c("id",'condition','valence','support'), direction = "long",v.names='h')

m = lme(h ~ valence*time, random=~1|id,data=edabind,method="REML",na.action=na.omit)
anova(m)
dNP=aggregate(edabind$h,by=list(edabind$valence,edabind$time),mean,na.rm=T)
colnames(dNP)=c('valence','time','eda')
dNP$time=as.numeric(dNP$time)
ggplot(data = dNP, aes(x=time, y=eda)) + geom_line(aes(colour=valence))


########comments eda######
edabind=data.frame(select(d,num_range("eda_D2_", 1:24)),condition=d$condition,id=d$id,
                   valence=d$valence,support=d$support)
colnames(edabind)[1:24]=c('h1','h2','h3','h4','h5','h6','h7','h8','h9',
                          'h10','h11','h12','h13','h14','h15','h16','h17','h18',
                          'h19','h20','h21','h22','h23','h24')
edabind=reshape(edabind,varying=1:24,idvar = c("id",'condition','valence','support'), direction = "long",v.names='h')

m = lme(h ~ support*time*valence, random=~1|id,data=edabind,method="REML",na.action=na.omit)
anova(m)
dNP=aggregate(edabind$h,by=list(edabind$support,edabind$time),mean,na.rm=T)
colnames(dNP)=c('support','time','eda')
dNP$time=as.numeric(dNP$time)
ggplot(data = dNP, aes(x=time, y=eda)) + geom_line(aes(colour=support))

#plot interaction
dx=aggregate(edabind$h,by=list(edabind$support,edabind$valence,edabind$time),mean,na.rm=T)
colnames(dx)=c('support','valence','time','eda')
dx$time=as.numeric(dx$time)
ggplot(dx, aes(x = time, y = eda,colour = support)) +geom_line() +facet_grid(. ~ valence) + 
  theme_classic()+  xlab("Time (seconds)") + ylab("EDA (μ sem)")

lsmeans(m, list(pairwise ~ support*valence), adjust = "tukey")


########posts ci######
cibind=data.frame(select(d,num_range("ci_D1_", 1:24)),condition=d$condition,id=d$id,
                  valence=d$valence,support=d$support)
colnames(cibind)[1:24]=c('h1','h2','h3','h4','h5','h6','h7','h8','h9',
                         'h10','h11','h12','h13','h14','h15','h16','h17','h18',
                         'h19','h20','h21','h22','h23','h24')
cibind=reshape(cibind,varying=1:24,idvar = c("id",'condition','valence','support'), direction = "long",v.names='h')

m = lme(h ~ valence*time, random=~1|id,data=cibind,method="REML",na.action=na.omit)
anova(m)
dNP=aggregate(cibind$h,by=list(cibind$valence,cibind$time),mean,na.rm=T)
colnames(dNP)=c('valence','time','ci')
dNP$time=as.numeric(dNP$time)
ggplot(data = dNP, aes(x=time, y=ci)) + geom_line(aes(colour=valence))

########comments ci######
cibind=data.frame(select(d,num_range("ci_D2_", 1:24)),condition=d$condition,id=d$id,
                  valence=d$valence,support=d$support)
colnames(cibind)[1:24]=c('h1','h2','h3','h4','h5','h6','h7','h8','h9',
                         'h10','h11','h12','h13','h14','h15','h16','h17','h18',
                         'h19','h20','h21','h22','h23','h24')
cibind=reshape(cibind,varying=1:24,idvar = c("id",'condition','valence','support'), direction = "long",v.names='h')

m = lme(h ~ support*time*valence, random=~1|id,data=cibind,method="REML",na.action=na.omit)
anova(m)
dNP=aggregate(cibind$h,by=list(cibind$support,cibind$time),mean,na.rm=T)
colnames(dNP)=c('support','time','ci')
dNP$time=as.numeric(dNP$time)
ggplot(data = dNP, aes(x=time, y=ci)) + geom_line(aes(colour=support))

#plot interaction
dx=aggregate(cibind$h,by=list(cibind$support,cibind$valence,cibind$time),mean,na.rm=T)
colnames(dx)=c('support','valence','time','ci')
dx$time=as.numeric(dx$time)
ggplot(dx, aes(x = time, y = ci,colour = support)) +geom_line() +facet_grid(. ~ valence) + 
  theme_classic()+  xlab("Time (seconds)") + ylab("corrugator")



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
spreadheart=function(x){setDT(as.data.frame(acast(x, condition~heartT,value.var='heart')),keep.rownames = TRUE)[]}
spreadoo=function(x){setDT(as.data.frame(acast(x, condition~ooT,value.var='oo')),keep.rownames = TRUE)[]}
spreadci=function(x){setDT(as.data.frame(acast(x, condition~ciT,value.var='ci')),keep.rownames = TRUE)[]}
spreadeda=function(x){setDT(as.data.frame(acast(x, condition~edaT,value.var='eda')),keep.rownames = TRUE)[]}
#function: merge four flat data into one data set
spread4=function(x){heart=spreadheart(x)
oo=spreadoo(x)
ci=spreadci(x)
eda=spreadeda(x) 
d=merge(heart,oo,by='rn')
d=merge(d,ci,by='rn')
d=merge(d,eda,by='rn')
d=as.data.frame(d)
colnames(d)[1]='condition'
print(d)
}


#d111001
d111001=read.table('111001_food-numbered.xls.csv',sep=',',header=T)
d111001=clean(d111001)
d111001$condition=c(1)
d111001=d111001[-c(which(is.na(d111001)==T)),]
d111001=spread4(d111001)
d111001$id=c(111001)

#121002
d121002=read.table('121002-food-numbered.csv',sep=',',header=T)
d121002=clean(d121002)
d121002$condition=c(2)
d121002=spread4(d121002)
d121002$id=c(121002)

#131003
d131003=read.table('131003_food-numbered.xls.csv',sep=',',header=T)
d131003=clean(d131003)
d131003=d131003[-c(which(is.na(d131003)==T)),]
d131003$condition=c(3)
d131003=spread4(d131003)
d131003$id=c(131003)

#d142004
d142004=read.table('142004_food-numbered.xls.csv',sep=',',header=T)
d142004=clean(d142004)
d142004$condition=c(4)
d142004=spread4(d142004)
d142004$id=c(142004)

#152005
d152005=read.table('152005_food-numbered.xls.csv',sep=',',header=T)
d152005=clean(d152005)
d152005=d152005[-c(which(is.na(d152005)==T)),]
d152005$condition=c(5)
d152005=spread4(d152005)
d152005$id=c(152005)

#162006
d162006=read.table('162006-food-numbered.csv',sep=',',header=T)
d162006=clean(d162006)
d162006$condition=c(6)
d162006=spread4(d162006)
d162006$id=c(162006)

#211007
d211007=read.table('211007_food-numbered.xls.csv',sep=',',header=T)
d211007=clean(d211007)
d211007=d211007[-c(which(is.na(d211007)==T)),]
d211007$condition=c(1)
d211007=spread4(d211007)
d211007$id=c(211007)

#221008
d221008=read.table('221008_food-numbered.xls.csv',sep=',',header=T)
d221008=clean(d221008)
d221008$condition=c(2)
d221008=spread4(d221008)
d221008$id=c(221008)

#231009
d231009=read.table('231009_food-numbered.xls.csv',sep=',',header=T)
d231009=clean(d231009)
d231009$condition=c(3)
d231009=spread4(d231009)
d231009$id=c(231009)


#242010
d242010=read.table('242010_food-numbered.xls.csv',sep=',',header=T)
d242010=clean(d242010)
d242010=d242010[-c(which(is.na(d242010)==T)),]
d242010$condition=c(4)
d242010=spread4(d242010)
d242010$id=c(242010)

#252011
d252011=read.table('252011_food-numbered.xls.csv',sep=',',header=T)
d252011=clean(d252011)
d252011$condition=c(5)
d252011=spread4(d252011)
d252011$id=c(252011)

#262012
d262012=read.table('w_262012_food-numbered.csv',sep=',',header=T)
d262012=clean(d262012)
d262012$condition=c(6)
d262012=spread4(d262012)
d262012$id=c(262012)

#311013
d311013=read.table('311013_food-numbered.xls.csv',sep=',',header=T)
d311013=clean(d311013)
d311013$condition=c(1)
d311013=spread4(d311013)
d311013$id=c(311013)

#321014
d321014=read.table('321014_food-numbered.xls.csv',sep=',',header=T)
d321014=clean(d321014)
d321014$condition=c(2)
d321014=spread4(d321014)
d321014$id=c(321014)

#331015
d331015=read.table('w_331015_food-numbered.csv',sep=',',header=T)
d331015=clean(d331015)
d331015$condition=c(3)
d331015=spread4(d331015)
d331015$id=c(331015)


#342016
d342016=read.table('342016_food-numbered.xls.csv',sep=',',header=T)
d342016=clean(d342016)
d342016=d342016[-c(which(is.na(d342016)==T)),]
d342016$condition=c(4)
d342016=spread4(d342016)
d342016$id=c(342016)

#352017
d352017=read.table('352017_food-numbered.xls.csv',sep=',',header=T)
d352017=clean(d352017)
d352017$condition=c(5)
d352017=spread4(d352017)
d352017$id=c(352017)

#362018
d362018=read.table('w_362018-food-numbered.csv',sep=',',header=T)
d362018=clean(d362018)
d362018$condition=c(6)
d362018=spread4(d362018)
d362018$id=c(362018)


#411019
d411019=read.table('411019_food-numbered.xls.csv',sep=',',header=T)
d411019=clean(d411019)
d411019$condition=c(1)
d411019=spread4(d411019)
d411019$id=c(411019)

#421020
d421020=read.table('421020_food-numbered.xls.csv',sep=',',header=T)
d421020=clean(d421020)
d421020$condition=c(2)
d421020=spread4(d421020)
d421020$id=c(421020)

#431021
d431021=read.table('431021_food-numbered.xls.csv',sep=',',header=T)
d431021=clean(d431021)
d431021$condition=c(3)
d431021=spread4(d431021)
d431021$id=c(431021)

#442022
d442022=read.table('442022_food-numbered.xls.csv',sep=',',header=T)
d442022=clean(d442022)
d442022$condition=c(4)
d442022=spread4(d442022)
d442022$id=c(442022)


#452023
d452023=read.table('452023_food-numbered.xls.csv',sep=',',header=T)
d452023=clean(d452023)
d452023$condition=c(5)
d452023=spread4(d452023)
d452023$id=c(452023)

#462024
d462024=read.table('462024_food-numbered.xls.csv',sep=',',header=T)
d462024=clean(d462024)
d462024$condition=c(6)
d462024=spread4(d462024)
d462024$id=c(462024)



#111025
d111025=read.table('111025_food-numbered.xls.csv',sep=',',header=T)
d111025=clean(d111025)
d111025$condition=c(1)
d111025=spread4(d111025)
d111025$id=c(111025)

#131027
d131027=read.table('131027_food-numbered.xls.csv',sep=',',header=T)
d131027=clean(d131027)
d131027$condition=c(3)
d131027=spread4(d131027)
d131027$id=c(131027)

#142028
d142028=read.table('142028_food-numbered.xls.csv',sep=',',header=T)
d142028=clean(d142028)
d142028=d142028[-c(which(is.na(d142028)==T)),]
d142028$condition=c(4)
d142028=spread4(d142028)
d142028$id=c(142028)

#152029
d152029=read.table('152029_food-numbered.xls.csv',sep=',',header=T)
d152029=clean(d152029)
d152029$condition=c(5)
d152029=spread4(d152029)
d152029$id=c(152029)

#162030
d162030=read.table('162030_food-numbered.xls.csv',sep=',',header=T)
d162030=clean(d162030)
d162030$condition=c(6)
d162030=spread4(d162030)
d162030$id=c(162030)

#211031
d211031=read.csv('211031_food-numbered.csv',sep='',header=T)
d211031=clean(d211031)
d211031$condition=c(1)
d211031=spread4(d211031)
d211031$id=c(211031)
#221032
d221032=read.csv('221032_food-numbered.csv',sep='',header=T)
d221032=clean(d221032)
d221032$condition=c(2)
d221032=spread4(d221032)
d221032$id=c(221032)
#231033
d231033=read.csv('231033_food-numbered.csv',sep='',header=T)
d231033=clean(d231033)
d231033$condition=c(3)
d231033=spread4(d231033)
d231033$id=c(231033)
#242034
d242034=read.csv('242034_food-numbered.csv',sep='',header=T)
d242034=clean(d242034)
d242034$condition=c(4)
d242034=spread4(d242034)
d242034$id=c(242034)
#252035
d252035=read.csv('252035_food-numbered.csv',sep='',header=T)
d252035=clean(d252035)
d252035$condition=c(5)
d252035=spread4(d252035)
d252035$id=c(252035)
#262036
d262036=read.csv('262036_food-numbered.csv',sep='',header=T)
d262036=clean(d262036)
d262036$condition=c(6)
d262036=spread4(d262036)
d262036$id=c(262036)
#311037
d311037=read.csv('311037_food-numbered.csv',sep='',header=T)
d311037=clean(d311037)
d311037$condition=c(1)
d311037=spread4(d311037)
d311037$id=c(311037)
#321038
d321038=read.csv('321038_food-numbered.csv',sep='',header=T)
d321038=clean(d321038)
d321038$condition=c(2)
d321038=spread4(d321038)
d321038$id=c(321038)
#331039
d331039=read.csv('331039_food-numbered.csv',sep='',header=T)
d331039=clean(d331039)
d331039$condition=c(3)
d331039=spread4(d331039)
d331039$id=c(331039)
#342040
d342040=read.csv('342040_food-numbered.csv',sep='',header=T)
d342040=clean(d342040)
d342040$condition=c(4)
d342040=spread4(d342040)
d342040$id=c(342040)
#352041
d352041=read.csv('352041_food-numbered.csv',sep='',header=T)
d352041=clean(d352041)
d352041$condition=c(5)
d352041=spread4(d352041)
d352041$id=c(352041)
#362042
d362042=read.csv('362042_food-numbered.csv',sep='',header=T)
d362042=clean(d362042)
d362042$condition=c(6)
d362042=spread4(d362042)
d362042$id=c(362042)
#411043
d411043=read.csv('411043_food-numbered.csv',sep='',header=T)
d411043=clean(d411043)
d411043$condition=c(1)
d411043=spread4(d411043)
d411043$id=c(411043)
#421044
d421044=read.csv('421044_food-numbered.csv',sep='',header=T)
d421044=clean(d421044)
d421044$condition=c(2)
d421044=spread4(d421044)
d421044$id=c(421044)
#442046
d442046=read.csv('442046_food-numbered.csv',sep='',header=T)
d442046=clean(d442046)
d442046$condition=c(4)
d442046=spread4(d442046)
d442046$id=c(442046)
#452047
d452047=read.csv('452047_food-numbered.csv',sep='',header=T)
d452047=clean(d452047)
d452047$condition=c(5)
d452047=spread4(d452047)
d452047$id=c(452047)
#111049
d111049=read.csv('111049_food-numbered.csv',sep='',header=T)
d111049=clean(d111049)
d111049$condition=c(1)
d111049=spread4(d111049)
d111049$id=c(111049)
#121050
d121050=read.csv('121050_food-numbered.xls.csv',sep=',',header=T)
d121050=clean(d121050)
d121050$condition=c(2)
d121050=spread4(d121050)
d121050$id=c(121050)
#131051
d131051=read.csv('131051_food-numbered.xls.csv',sep=',',header=T)
d131051=clean(d131051)
d131051$condition=c(3)
d131051=spread4(d131051)
d131051$id=c(131051)
#142052
d142052=read.csv('142052_food-numbered.xls.csv',sep=',',header=T)
d142052=clean(d142052)
d142052$condition=c(4)
d142052=spread4(d142052)
d142052$id=c(142052)
#152053
d152053=read.csv('152053_food-numbered.xls.csv',sep=',',header=T)
d152053=clean(d152053)
d152053$condition=c(5)
d152053=spread4(d152053)
d152053$id=c(152053)
#162054
d162054=read.csv('162054_food-numbered.xls.csv',sep=',',header=T)
d162054=clean(d162054)
d162054$condition=c(6)
d162054=spread4(d162054)
d162054$id=c(162054)
#211055
d211055=read.csv('211055_food-numbered.xls.csv',sep=',',header=T)
d211055=clean(d211055)
d211055$condition=c(1)
d211055=spread4(d211055)
d211055$id=c(211055)
#221056
d221056=read.csv('221056_food-numbered.xls.csv',sep=',',header=T)
d221056=clean(d221056)
d221056$condition=c(2)
d221056=spread4(d221056)
d221056$id=c(221056)
#231057
d231057=read.csv('231057-food-numbered.csv',sep=',',header=T)
d231057=clean(d231057)
d231057$condition=c(3)
d231057=spread4(d231057)
d231057$id=c(231057)
#242058
d242058=read.csv('242058-food-numbered.csv',sep=',',header=T)
d242058=clean(d242058)
d242058$condition=c(4)
d242058=spread4(d242058)
d242058$id=c(242058)
#d252059
d252059=read.csv('252059_food-numbered.xls.csv',sep=',',header=T)
d252059=clean(d252059)
d252059$condition=c(5)
d252059=spread4(d252059)
d252059$id=c(252059)
#262060 -- problem
d262060=read.csv('262060_food-numbered.xls.csv',sep=',',header=T)
d262060=clean(d262060)
d262060$condition=c(6)
d262060=spread4(d262060)
d262060$id=c(262060)
#311061
d311061=read.csv('311061_food-numbered.xls.csv',sep=',',header=T)
d311061=clean(d311061)
d311061$condition=c(1)
d311061=spread4(d311061)
d311061$id=c(311061)
#321062
d321062=read.csv('321062_food-numbered.xls.csv',sep=',',header=T)
d321062=clean(d321062)
d321062$condition=c(2)
d321062=spread4(d321062)
d321062$id=c(321062)
#331063
d331063=read.csv('331063_food-numbered.xls.csv',sep=',',header=T)
d331063=clean(d331063)
d331063$condition=c(3)
d331063=spread4(d331063)
d331063$id=c(331063)
#342064
d342064=read.csv('342064_food-numbered.xls.csv',sep=',',header=T)
d342064=clean(d342064)
d342064$condition=c(4)
d342064=spread4(d342064)
d342064$id=c(342064)
#352065
d352065=read.csv('352065_food-numbered.xls.csv',sep=',',header=T)
d352065=clean(d352065)
d352065$condition=c(5)
d352065=spread4(d352065)
d352065$id=c(352065)
#362066
d362066=read.csv('362066_food-numbered.xls.csv',sep=',',header=T)
d362066=clean(d362066)
d362066$condition=c(6)
d362066=spread4(d362066)
d362066$id=c(362066)
#411067
d411067=read.csv('411067_food-numbered.csv',sep='',header=T)
d411067=clean(d411067)
d411067$condition=c(1)
d411067=spread4(d411067)
d411067$id=c(411067)
#421068
d421068=read.csv('421068_food-numbered.xls.csv',sep=',',header=T)
d421068=clean(d421068)
d421068$condition=c(2)
d421068=spread4(d421068)
d421068$id=c(421068)
#431069
d431069=read.csv('431069_food-numbered.xls.csv',sep=',',header=T)
d431069=clean(d431069)
d431069$condition=c(3)
d431069=spread4(d431069)
d431069$id=c(431069)
#431069
d431069=read.csv('431069_food-numbered.xls.csv',sep=',',header=T)
d431069=clean(d431069)
d431069$condition=c(3)
d431069=spread4(d431069)
d431069$id=c(431069)
#452071
d452071=read.csv('452071_food-numbered.csv',sep='',header=T)
d452071=clean(d452071)
d452071$condition=c(5)
d452071=spread4(d452071)
d452071$id=c(452071)
#462072
d462072=read.csv('462072_food-numbered.xls.csv',sep=',',header=T)
d462072=clean(d462072)
d462072$condition=c(6)
d462072=spread4(d462072)
d462072$id=c(462072)
#121074
d121074=read.csv('121074_food-numbered.xls.csv',sep=',',header=T)
d121074=clean(d121074)
d121074$condition=c(2)
d121074=spread4(d121074)
d121074$id=c(121074)
#131075
d131075=read.csv('131075_food-numbered.csv',sep=',',header=T)
d131075=clean(d131075)
d131075$condition=c(3)
d131075=spread4(d131075)
d131075$id=c(131075)
#142076
d142076=read.csv('142076_food-numbered.xls.csv',sep=',',header=T)
d142076=clean(d142076)
d142076$condition=c(4)
d142076=spread4(d142076)
d142076$id=c(142076)
#152077
d152077=read.csv('152077_food-numbered.csv',sep=',',header=T)
d152077=clean(d152077)
d152077$condition=c(5)
d152077=spread4(d152077)
d152077$id=c(152077)
#162078
d162078=read.csv('162078_food-numbered.csv',sep=',',header=T)
d162078=clean(d162078)
d162078$condition=c(6)
d162078=spread4(d162078)
d162078$id=c(162078)
#211079
d211079=read.csv('211079_food-numbered.csv',sep=',',header=T)
d211079=clean(d211079)
d211079$condition=c(1)
d211079=spread4(d211079)
d211079$id=c(211079)
#221080
d221080=read.csv('221080_food-numbered.csv',sep=',',header=T)
d221080=clean(d221080)
d221080$condition=c(2)
d221080=spread4(d221080)
d221080$id=c(221080)
#231081
d231081=read.csv('231081_food-numbered.csv',sep=',',header=T)
d231081=clean(d231081)
d231081$condition=c(3)
d231081=spread4(d231081)
d231081$id=c(231081)
#242082
d242082=read.csv('242082_food-numbered.csv',sep=',',header=T)
d242082=clean(d242082)
d242082$condition=c(4)
d242082=spread4(d242082)
d242082$id=c(242082)
#252083
d252083=read.csv('252083_food-numbered.csv',sep='',header=T)
d252083=clean(d252083)
d252083$condition=c(5)
d252083=spread4(d252083)
d252083$id=c(252083)
#311085
d311085=read.csv('311085_food-numbered.csv',sep=',',header=T)
d311085=clean(d311085)
d311085$condition=c(1)
d311085=spread4(d311085)
d311085$id=c(311085)
#331087
d331087=read.csv('331087_food-numbered.csv',sep=',',header=T)
d331087=clean(d331087)
d331087$condition=c(3)
d331087=spread4(d331087)
d331087$id=c(331087)



d=bind_rows(d111001,d121002,d142004,d152005,d211007,d221008,d242010,
            d252011,d311013,d321014,d342016,d352017,d411019,d421020,
            d442022,d452023,d111025,d142028,d152029,
            d211031,d221032,d242034,d252035,d311037,d321038,
            d342040,d352041, d411043,d421044,d442046,d452047,
            d121050,d142052,d152053,d211055,d221056,d242058,d252059,
            d311061,d321062,d342064,d352065,d411067,d421068,
            d452071,d121074,d142076,d152077,d211079,
            d221080,d242082,d252083,d311085)

#342088
d342088=read.csv('342088-food-numbered.csv',sep=',',header=T)
d342088=clean(d342088)
d342088$condition=c(4)
d342088=spread4(d342088)
d342088$id=c(342088)

#352089
d352089=read.csv('352089-food-numbered.csv',sep=',',header=T)
d352089=clean(d352089)
d352089$condition=c(5)
d352089=spread4(d352089)
d352089$id=c(352089)

#411091
d411091=read.csv('411091_food-numbered.csv',sep=',',header=T)
d411091=clean(d411091)
d411091$condition=c(1)
d411091=spread4(d411091)
d411091$id=c(411091)

#421092
d421092=read.csv('421092-food-numbered.csv',sep=',',header=T)
d421092=clean(d421092)
d421092$condition=c(2)
d421092=spread4(d421092)
d421092$id=c(421092)

#442094
d442094=read.csv('442094-food-numbered.csv',sep=',',header=T)
d442094=clean(d442094)
d442094$condition=c(4)
d442094=spread4(d442094)
d442094$id=c(442094)

#452095
d452095=read.csv('452095-food-numbered.csv',sep=',',header=T)
d452095=clean(d452095)
d452095$condition=c(5)
d452095=spread4(d452095)
d452095$id=c(452095)

#111097
d111097=read.csv('w_111097_food-numbered.csv',sep=',',header=T)
d111097=clean(d111097)
d111097$condition=c(1)
d111097=spread4(d111097)
d111097$id=c(111097)

#142100
d142100=read.csv('w_142100_food-numbered.csv',sep=',',header=T)
d142100=clean(d142100)
d142100$condition=c(4)
d142100=spread4(d142100)
d142100$id=c(142100)

#152101
d152101=read.csv('w_152101_food-numbered.csv',sep=',',header=T)
d152101=clean(d152101)
d152101$condition=c(5)
d152101=spread4(d152101)
d152101$id=c(152101)

#211103
d211103=read.csv('w_211103_food-numbered.csv',sep=',',header=T)
d211103=clean(d211103)
d211103$condition=c(1)
d211103=spread4(d211103)
d211103$id=c(211103)

#221104
d221104=read.csv('221104-food-numbered.csv',sep=',',header=T)
d221104=clean(d221104)
d221104$condition=c(2)
d221104=spread4(d221104)
d221104$id=c(221104)

#311107
d311107=read.csv('311107-food-numbered.csv',sep=',',header=T)
d311107=clean(d311107)
d311107$condition=c(1)
d311107=spread4(d311107)
d311107$id=c(311107)

#321108
d321108=read.csv('321108-food-numbered.csv',sep=',',header=T)
d321108=clean(d321108)
d321108$condition=c(2)
d321108=spread4(d321108)
d321108$id=c(321108)

#342110
d342110=read.csv('342110-food-numbered.csv',sep=',',header=T)
d342110=clean(d342110)
d342110$condition=c(4)
d342110=spread4(d342110)
d342110$id=c(342110)

#352111
d352111=read.csv('352111-food-numbered.csv',sep=',',header=T)
d352111=clean(d352111)
d352111$condition=c(5)
d352111=spread4(d352111)
d352111$id=c(352111)

#411113
d411113=read.csv('411113-food-numbered.csv',sep=',',header=T)
d411113=clean(d411113)
d411113$condition=c(1)
d411113=spread4(d411113)
d411113$id=c(411113)

#421114
d421114=read.csv('421114-food-numbered.csv',sep=',',header=T)
d421114=clean(d421114)
d421114$condition=c(2)
d421114=spread4(d421114)
d421114$id=c(421114)

#442116
d442116=read.csv('442116-food-numbered.csv',sep=',',header=T)
d442116=clean(d442116)
d442116$condition=c(4)
d442116=spread4(d442116)
d442116$id=c(442116)

#452117
d452117=read.csv('452117-food-numbered.csv',sep=',',header=T)
d452117=clean(d452117)
d452117$condition=c(5)
d452117=spread4(d452117)
d452117$id=c(452117)

#211119 problem
d211119=read.csv('211119-food-numbered.csv',sep=',',header=T)

d211119=unite(d211119,timeF, c(material, timepoint), remove=FALSE)
d211119$heartT=paste("heart", d211119$timeF, sep = "_")
  d211119$edaT=paste("eda", d211119$timeF, sep = "_")
  d211119$ooT=paste("oo", d211119$timeF, sep = "_")
  d211119$ciT=paste("ci", d211119$timeF, sep = "_")
  d211119=as.data.frame(d211119)
d211119$condition=c(1)
d211119=spread4(d211119)
d211119$id=c(211119)

#221120
d221120=read.csv('221120-food-numbered.csv',sep=',',header=T)
d221120=clean(d221120)
d221120$condition=c(2)
d221120=spread4(d221120)
d221120$id=c(221120)

#242122
d242122=read.csv('242122-food-numbered.csv',sep=',',header=T)
d242122=clean(d242122)
d242122$condition=c(4)
d242122=spread4(d242122)
d242122$id=c(242122)

#252123
d252123=read.csv('252123-food-numbered.csv',sep=',',header=T)
d252123=clean(d252123)
d252123$condition=c(5)
d252123=spread4(d252123)
d252123$id=c(252123)

#211125
d211125=read.csv('211125-food-numbered.csv',sep=',',header=T)
d211125=clean(d211125)
d211125$condition=c(1)
d211125=spread4(d211125)
d211125$id=c(211125)

#221126
d221126=read.csv('221126-food-numbered.csv',sep=',',header=T)
d221126=clean(d221126)
d221126$condition=c(2)
d221126=spread4(d221126)
d221126$id=c(221126)

#242128
d242128=read.csv('242128-food-numbered.csv',sep=',',header=T)
d242128=clean(d242128)
d242128$condition=c(4)
d242128=spread4(d242128)
d242128$id=c(242128)

#252129
d252129=read.csv('w_252129-food-numbered.csv',sep=',',header=T)
d252129=clean(d252129)
d252129$condition=c(5)
d252129=spread4(d252129)
d252129$id=c(252129)

#221132
d221132=read.csv('221132-food-numbered.csv',sep=',',header=T)
d221132=clean(d221132)
d221132$condition=c(2)
d221132=spread4(d221132)
d221132$id=c(221132)

#242134
d242134=read.csv('242134-food-numbered.csv',sep=',',header=T)
d242134=clean(d242134)
d242134$condition=c(4)
d242134=spread4(d242134)
d242134$id=c(242134)

#252135
d252135=read.csv('252135-food-numbered.csv',sep=',',header=T)
d252135=clean(d252135)
d252135$condition=c(5)
d252135=spread4(d252135)
d252135$id=c(252135)

#211137
d211137=read.csv('211137-food-numbered.csv',sep=',',header=T)
d211137=clean(d211137)
d211137$condition=c(1)
d211137=spread4(d211137)
d211137$id=c(211137)

#221138
d221138=read.csv('221138-food-numbered.csv',sep=',',header=T)
d221138=clean(d221138)
d221138$condition=c(2)
d221138=spread4(d221138)
d221138$id=c(221138)

#242140
d242140=read.csv('242140-food-numbered.csv',sep=',',header=T)
d242140=clean(d242140)
d242140$condition=c(4)
d242140=spread4(d242140)
d242140$id=c(242140)

#252141
d252141=read.csv('252141-food-numbered.csv',sep=',',header=T)
d252141=clean(d252141)
d252141$condition=c(5)
d252141=spread4(d252141)
d252141$id=c(252141)

#221144
d221144=read.csv('221144-food-numbered.csv',sep=',',header=T)
d221144=clean(d221144)
d221144$condition=c(2)
d221144=spread4(d221144)
d221144$id=c(221144)

#242146
d242146=read.csv('242146-food-numbered.csv',sep=',',header=T)
d242146=clean(d242146)
d242146$condition=c(4)
d242146=spread4(d242146)
d242146$id=c(242146)

#252147
d252147=read.csv('252147-food-numbered.csv',sep=',',header=T)
d252147=clean(d252147)
d252147$condition=c(5)
d252147=spread4(d252147)
d252147$id=c(252147)

#111149
d111149=read.csv('111149-food-numbered.csv',sep=',',header=T)
d111149=clean(d111149)
d111149$condition=c(1)
d111149=spread4(d111149)
d111149$id=c(111149)

#221150
d221150=read.csv('221150-food-numbered.csv',sep=',',header=T)
d221150=clean(d221150)
d221150$condition=c(2)
d221150=spread4(d221150)
d221150$id=c(221150)

#442152
d442152=read.csv('442152-food-numbered.csv',sep=',',header=T)
d442152=clean(d442152)
d442152$condition=c(4)
d442152=spread4(d442152)
d442152$id=c(442152)

#152153
d152153=read.csv('152153-food-numbered.csv',sep=',',header=T)
d152153=clean(d152153)
d152153$condition=c(5)
d152153=spread4(d152153)
d152153$id=c(152153)

#311155
d311155=read.csv('311155-food-numbered.csv',sep=',',header=T)
d311155=clean(d311155)
d311155$condition=c(1)
d311155=spread4(d311155)
d311155$id=c(311155)

#421156
d421156=read.csv('421156-food-numbered.csv',sep=',',header=T)
d421156=clean(d421156)
d421156$condition=c(2)
d421156=spread4(d421156)
d421156$id=c(421156)

#242158
d242158=read.csv('242158-food-numbered.csv',sep=',',header=T)
d242158=clean(d242158)
d242158$condition=c(4)
d242158=spread4(d242158)
d242158$id=c(242158)

#352159
d352159=read.csv('352159-food-numbered.csv',sep=',',header=T)
d352159=clean(d352159)
d352159$condition=c(5)
d352159=spread4(d352159)
d352159$id=c(352159)



d=bind_rows(d,d342088,d352089,
            d411091,d421092,d442094, d452095, d111097,
            d142100, d152101,d211103,d221104,d311107,d321108,
            d342110,d352111,d411113,d421114,d442116,d452117,
            d221120,d242122,d252123,d211125,d221126,d242128,d252129,
            d221132,d242134,d252135,d211137,d221138,
            d242140,d252141,d221144,d242146,d252147,d111149,
            d221150,d442152,d152153,d311155,d421156,d242158,d352159)

#121026
d121026=read.csv('121026-food-numbered.csv',sep=',',header=T)
d121026=clean(d121026)
d121026$condition=c(2)
d121026=spread4(d121026)
d121026$id=c(121026)

#242106
d242106=read.csv('242106-food-numbered.csv',sep=',',header=T)
d242106=clean(d242106)
d242106$condition=c(4)
d242106=spread4(d242106)
d242106$id=c(242106)

#321086
d321086=read.csv('321086-food-numbered.csv',sep=',',header=T)
d321086=clean(d321086)
d321086$condition=c(2)
d321086=spread4(d321086)
d321086$id=c(321086)

#111073
d111073=read.csv('111073-food-numbered.csv',sep=',',header=T)
d111073=clean(d111073)
d111073$condition=c(1)
d111073=spread4(d111073)
d111073$id=c(111073)

d=bind_rows(d,d121026,d242106,d321086,d111073)
