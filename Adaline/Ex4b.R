rm(list=ls())
library("corpcor") #pacote para achar a matriz H'->pseudoinversa
source('trainadaline.R')
source('yadaline.R')

#Funcao a minimizar
np<-20
x00<-seq((2*pi/np),2*pi,(2*pi/np))
x01<-scale(sin(x00))
x02<-scale(cos(x00))
x03<-scale(x00)
a<- pi/2
b<- 1
c<- 2
d<- 3
y<-a+b*x01+c*x02+d*x03
plot(x00,x01,type='b', col = 'red',xlim=c(0,2*pi),ylim=c(min(x01),max(x01)),xlab = 't',ylab='x1')
plot(x00,x02,type='b', col = 'green',xlim=c(0,2*pi),ylim=c(min(x02),max(x02)),xlab = 't',ylab='x2')
plot(x00,x03,type='b', col = 'blue',xlim=c(0,2*pi),ylim=c(min(x03),max(x03)),xlab = 't',ylab='x3')
plot(x00,y,type='b', col = 'orange',xlim=c(0,2*pi),c(min(y),max(y)),xlab = 't',ylab='y')
x0<-cbind(x01,x02,x03)
grau<-dim(x0)[2]
mse = matrix(0,ncol = 1, nrow = 1)
train<-sample((np))
yt<-y[train[1:(0.7*np)]]

yv<-y[train[((0.7*np)+1):np]]
par(new=T)
plot(x00[train[1:(0.7*np)]],yt,type='p',pch = 21,bg = 'red', col = 'black',xlim=c(0,2*pi),
     ylim=c(min(y),max(y)),xlab = 'x',ylab='')

plot(x00,y,type='b', col = 'black',xlim=c(0,2*pi),ylim=c(min(y),max(y)),xlab = 'x')
#Escolhendo o grau do polinomio para aproximacao o qual sera a primeira camada
x<-matrix(1, nrow = length(y),ncol = (grau))
x<-cbind(1, x0)
xt<-matrix(1, nrow = (0.7*np), ncol = (grau))
xt<-x[train[1:(0.7*np)],]
xv<-matrix(1,nrow = length(yv),ncol = (grau))
xv<-x[train[((0.7*np)+1):(np)],]

#   #No caso da Adaline pode-se calcular os pesos usando-se a Regra Delta
#   theta<-pseudoinverse(xt) %*% yt
eta<-0.1
tol<-0.001
maxepocas<-1000
par<-0
xin<-xt
yd<-yt
result<-trainadaline(as.matrix(xt),yt,eta,0.01,maxepocas,0)
theta<-result[[1]]
print(theta)
y_hat<-yadaline(xv,theta,0)
y_hat2<-yadaline(as.matrix(x),theta,0)

#Realizando a previsao
par(new=T)
plot(x00,y_hat2,type='b', col = 'green',xlim=c(0,2*pi),ylim=c(min(y),max(y)),xlab = '',ylab='')
par(new=T)
plot(x00[train[((0.7*np)+1):(np)]],y_hat,type='p',pch = 21,bg = 'green',xlim=c(0,2*pi),ylim=c(min(y),max(y)),xlab = 'x',ylab='sinc(x)')
par(new=T)
mse = sum(((yv - y_hat)^2)/length(y))
print(mse)
graus<- seq(1,1,1)
legend(4.5,-0.35, c('Grau','Original',graus),lty=c(1,1), lwd=c(2.5,2.5),col=c('white','black','green'))

