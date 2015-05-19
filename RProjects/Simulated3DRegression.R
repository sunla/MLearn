library(mvtnorm)
library(rgl)

n <- 2000

KM <- matrix(c(202.11710,  128.10062, 152.69674,
        128.10062, 172.23024,  124.15861,
        152.69674,  124.15861, 221.97948),nrow = 3) # Covariance Matrix
X<-rmvnorm(n,mean = c(101.3862,98.50172,101.324),sigma = KM) #Means
colnames(X)<-c("Gc","PA","Decoding")
d<-data.frame(X)

fit <- lm(Decoding ~ Gc + PA,data=d)
coefs <- coef(fit)
yhat<-cbind(rep(1,n),as.matrix(d[,1:2]))%*%coefs
resid<-d$Decoding-yhat

open3d(windowRect = c(100,100, 900, 900),family="serif" )
rain<-rainbow(length(resid))
plot3d(d$Gc,d$PA,d$Decoding,col=rain[rank(resid)],type="s",size=0.5,xlim=c(55,145),ylim=c(55,145),zlim=c(55,145),box=F,axes=F,xlab="",ylab="",zlab="")
            
planes3d(a=coefs["Gc"], b=coefs["PA"],-1,  coefs["(Intercept)"], alpha=0.50, col="plum2")
axis3d('x',pos=c(NA, 40, 40),at=seq(55,145,15))
axis3d('y',pos=c(40, NA, 40),at=seq(55,145,15))
axis3d('z',pos=c(40, 40, NA),at=seq(55,145,15))
mtext3d(text='Gc',edge="x",col="red")
mtext3d(text='Phonemic Awareness',edge="y",col="red")
mtext3d(text='Decoding',edge="z",col="red")
# spin1 <- spin3d(rpm = 1)
# play3d(spin1,duration=20)


points3d(cbind(d$Gc,d$PA,yhat),cex=20)
