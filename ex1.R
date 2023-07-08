n=200
a=5
b=2
S=3
i=1:200
step=0.1
start=1
x=start+step*i
e=rlnorm(200,0,1)
y=a*(x^b)*e
plot(x,y)

lx=log(x)
ly=log(y)
plot(lx,ly)


xyl=data.frame(cbind(lx,ly))
reg=lm(ly~lx,data=xyl)
summary(reg)
abline(reg,col="red") 



int=predict(reg,interval='conf',level=0.95)
matlines(xyl$lx,cbind(int),lty=c(1,2,2),col=c(2,4,4))

intpred=predict(reg,interval='pred',level=0.95)
matlines(xyl$lx,cbind(intpred),lty=c(1,2,2),col=c(2,3,3))

x1=seq(min(xyl[,"xl"]),max(xyl[,"xl"]),length=n)
grid=data.frame(x1)


# ------------------------------------------------------------------------------

n=190
a=5
b=2
S=3
i=1:190
step=0.1
start=1
x=start+step*i
e=rlnorm(190,0,1)
y=a*(exp(b*x))*e
plot(x,y)


ly=log(y)
plot(x,ly)


xyl=data.frame(cbind(x,ly))
reg=lm(ly~x,data=xyl)
summary(reg)
abline(reg,col="red") 

int=predict(reg,interval='conf',level=0.95)
matlines(xyl$x,cbind(int),lty=c(1,2,2),col=c(2,4,4))

intpred=predict(reg,interval='pred',level=0.95)
matlines(xyl$x,cbind(intpred),lty=c(1,2,2),col=c(2,3,3))

x1=seq(min(xyl[,"x"]),max(xyl[,"x"]),length=n)
grid=data.frame(x1)


# -----------------------------------------------------------------------------

n=180
a=5
b=2
S=3
i=1:180
step=0.1
start=1
x=start+step*i
e=rnorm(180,0,1)
y=a+b*log(x)+e
plot(x,y)

lx=log(x)
plot(lx,y)


xyl=data.frame(cbind(lx,y))
reg=lm(y~lx,data=xyl)
summary(reg)
abline(reg,col="red") 

int=predict(reg,interval='conf',level=0.95)
matlines(xyl$lx,cbind(int),lty=c(1,2,2),col=c(2,4,4))

intpred=predict(reg,interval='pred',level=0.95)
matlines(xyl$lx,cbind(intpred),lty=c(1,2,2),col=c(2,3,3))

x1=seq(min(xyl[,"lx"]),max(xyl[,"lx"]),length=n)
grid=data.frame(x1)

# -----------------------------------------------------------------------------
n=165
a=5
b=2
S=3
i=1:165
step=0.1
start=1
x=start+step*i
e=rnorm(165,0,0.1)
y=a+(b/x)+e
plot(x,y)

x1=(1/x)
plot(x1,y)


xy=data.frame(cbind(x1,y))
reg=lm(y~x1,data=xy)
summary(reg)
abline(reg,col="red") 


int=predict(reg,interval='conf',level=0.95)
matlines(xy$x1,cbind(int),lty=c(1,2,2),col=c(2,4,4))

intpred=predict(reg,interval='pred',level=0.95)
matlines(xy$x1,cbind(intpred),lty=c(1,2,2),col=c(2,3,3))

x1=seq(min(xy[,"x1"]),max(xy[,"x1"]),length=n)
grid=data.frame(x1)
