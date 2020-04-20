# metal analysis for occup.health by JHY 
# (update, 20190324)

install.packages('metafor')
library(metafor)


#quick-R (google)
mm<-read.table("stomach.csv", header=TRUE, sep=",")

fix(mm)
mm$pesr<-format(round(mm$esR,2))
mm$pll<-format(round(mm$ll,2))
mm$pul<-format(round(mm$ul,2))
View(mm)

res <-rma(yi=lnesR, sei=se, measure="RR", digits=1, data=mm, method="FE")
summary(res)
#exp(res$b)
forest(res, digits=2, atransf=exp)

res1 <-rma(yi=lnesR, sei=se, measure="RR", digits=1, data=mm)
res1
forest(res1, digits=2, atransf=exp)
res$beta
res$pval
res1$pval

funnel(res1)
regtest(res1, model="lm")
rtf <-trimfill(res)
rtf
funnel(rtf)
head(mm)

# mixed effect model#

res3 <-rma(yi=lnesR, sei=se, measure="RR",  data=mm)
res3
forest(res3, digits=2, atransf=exp)


res3 <-rma(yi=lnesR, sei=se, measure="RR",mods=cbind(jt),  data=mm)
res3
forest(res3, digits=2, atransf=exp)
table(mm$jt)
predict(res3, newmods=cbind(seq(from=1, to=5, by=1)), transf=exp, addx=TRUE)


preds <- predict(res3, newmods = cbind(0:5), transf = exp)
preds
wi<-1/(mm$se^2)
size <- 5 + 10 * (wi - min(wi))/(max(wi) - min(wi))
plot(mm$jt, exp(mm$lnesR), pch = 19 ,col = alpha(blues9, 0.4), cex = size/2, xlab = "Occupation", ylab = "Relative Risk",
     las = 1, bty = "l", log = "y", xlim=c(1,5.5))
lines(0:5, preds$pred, lwd=2, col='grey')
lines(0:5, preds$ci.lb, lty = "dashed")
lines(0:5, preds$ci.ub, lty = "dashed")
abline(h = 1, lty = "dotted")
preds
funnel(res3)
rtf3 <-trimfill(res3)

#######ggplot
install.packages('ggplot2')
library('ggplot2')
preds
preds <- predict(res3, newmods = cbind(1:5), transf = exp)

ggplot(mm, aes(x = jt, y = exp(lnesR)))+
  geom_point(data = mm, alpha=1/3, col="blue", shape = 16, size =(log(wi)*2+2))

jtp<-c(1:5)
pp<-cbind(jtp, data.frame(preds))
fix(pp)


ggplot(pp, aes(jtp, y=pred)) +   
         geom_point(data=mm, aes(x=jt, y=exp(lnesR)), colour="blue", alpha=0.5, shape=16, size=(log(wi)*2+2)) +
 stat_smooth(method="loess", formula=y~x, fullrange=T, se=FALSE, size=1, colour="red")+
 stat_smooth(data=pp,aes(x=jtp,y=ci.lb),method="loess",formula=y~x,fullrange=T,  
            se=FALSE,size=1,linetype=3,colour="red")+
  stat_smooth(data=pp,aes(x=jtp,y=ci.ub),method="loess",formula=y~x,fullrange=T,
              se=FALSE,size=1,linetype=3,colour="red")



  
  
#####ceramic

mmceramic<-mm[mm$jt==1,]
rescer <-rma(yi=lnesR, sei=se, measure="RR", digits=1, data=mmceramic)
rescer
forest(rescer, digits=2, atransf=exp)
rtfcer <-trimfill(rescer)
rtfcer
funnel(rtfcer)



