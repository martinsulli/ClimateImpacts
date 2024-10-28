################
# Code For running mixed effects model and creating Extended Data Fig. 4

# Load lme4 package
library(lme4)

# Read data (from data package save first sheet as a csv)
dat<-read.csv("Cuni_Sanchez_RawData.csv")

# Create derived variables - proportion of possible changes
dat$clim.score<-apply(dat[,11:21],1,function(x)sum(x,na.rm=T)/length(x[!is.na(x)]))
dat$impact.score<-apply(dat[,22:29],1,function(x)sum(x,na.rm=T)/length(x[!is.na(x)]))
dat$adapt.score<-apply(dat[,30:42],1,function(x)sum(x,na.rm=T)/length(x[!is.na(x)]))
dat$adaptDone<-apply(dat[,30:42],1,function(x)sum(x,na.rm=T))
dat$adaptAvail<-apply(dat[,30:42],1,function(x)length(x[!is.na(x)]))

# Basic exploration of pairwise relationships
pairs(cbind(dat$clim.score,dat$impact.score,dat$adapt.score))
cor.test(dat$clim.score,dat$impact.score)#Sig pos
cor.test(dat$clim.score,dat$adapt.score)#Sig pos
cor.test(dat$impact.score,dat$adapt.score) #NS

# Fit model
mf<-lmer(adapt.score ~ clim.score + wealth + (wealth + clim.score|site),data=dat)
plot(mf)
hist(residuals(mf))
summary(mf)

# Confidence intervals of fixed effects
confint(mf,method="boot")


# Plots of residuals look OK, but comment in review suggested checking using binomial GLM 
mf2<-glmer(adapt.score ~ clim.score + wealth + (wealth + clim.score|site),data=dat,family=binomial,weights=dat$adaptAvail)
plot(mf2)
summary(mf2)# Same general trends

# Predict model for wealth-site combintations
pred.dat<-expand.grid(unique(dat$site),unique(dat$wealth))
names(pred.dat)<-c("site","wealth")
pred.dat$clim.score<-mean(dat$clim.score)
preds.fun<-function(x){
predict(x,newdata=pred.dat,re.form=NULL)
}

preds<-preds.fun(mf)
boxplot(preds~pred.dat$site)

# Bootstrap of predictions
pmat<-bootMer(mf,preds.fun,999,re.form=NULL)

# CIs from bootstrap
cfs<-apply(pmat$t,2,function(x)quantile(x,c(0.025,0.5,0.975)))
# Extract CIs
pred.dat$Pmed<-cfs[2,]
pred.dat$Plcl<-cfs[1,]
pred.dat$Pucl<-cfs[3,]

# Set positions on x axis (called y here, but reversing)
y1<-seq(1:10)
y2<-y1-0.2
y3<-y1+0.2


# Sort order of sites on x axis
pred.dat$site <- factor(pred.dat$site, levels=sort(levels(pred.dat$site)))
pred.dat<-pred.dat[order(pred.dat$wealth,pred.dat$site),]

# Extended data 4 start
par(mfrow=c(1,2))
par(mar=c(4,4,1,1))
par(mgp=c(2.5,0.5,0))
library(RColorBrewer)
cols<-brewer.pal(8,"Dark2")
cols<-colorRampPalette(cols)(10)
plot(y1,pred.dat$Pmed[11:20],ylim=c(0.25,0.85),xaxt="n",col=cols,bg=cols,pch=21,xlab="",ylab="Proportion of adaptations")
axis(1,at=y1,labels=pred.dat$site[1:10],las=2)
arrows(y1,pred.dat$Plcl[11:20],y1,pred.dat$Pucl[11:20],length=0,col=cols)
points(y3,pred.dat$Pmed[1:10],col=cols,bg=cols,pch=25)
arrows(y3,pred.dat$Plcl[1:10],y3,pred.dat$Pucl[1:10],length=0,col=cols)
points(y2,pred.dat$Pmed[21:30],col=cols,bg=cols,pch=24)
arrows(y2,pred.dat$Plcl[21:30],y2,pred.dat$Pucl[21:30],length=0,col=cols)
legend("bottomleft",pch=c(24,21,25),legend=c("Rich","Average","Poor"))
mtext("(a)",at=0.25)

# Relationships with clim score for part b
preds2<-predict(m3_1,re.form=NULL)

sts<-sort(unique(dat$site))
plot(1,type="n",ylim=c(0.2,0.8),xlim=c(0,1),ylab="Proportion of adaptations",xlab="Proportion of changes")
for(i in 1:length(sts)){
#with(dat[dat$site==sts[i],],points(adapt.score~clim.score,pch=16,col=cols[i]))
tmp<-data.frame("Preds"=preds2[dat$site==sts[i]],"clim.score"=dat$clim.score[dat$site==sts[i]],"wealth"=dat$wealth[dat$site==sts[i]])
t2<-tmp[tmp$wealth=="average",]
t2<-t2[order(t2$clim.score),]
points(t2$Preds ~ t2$clim.score,type="l",col=cols[i],lty=1)
text(mean(t2$clim.score),mean(t2$Preds),sts[i],cex=0.8)
}
mtext("(b)",at=0)






