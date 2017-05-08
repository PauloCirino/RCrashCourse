rm(list=ls())
library("corpcor") #pacote para achar a matriz H'->pseudoinversa
source('trainadaline.R')
source('yadaline.R')

#Funcao a minimizar
# x00<-seq(0.3,6,0.3)
# x0<-sin(x00)
# a<-0.3
# b<-0.5
# y<-x0*a+b
x00<-as.matrix(read.table('Ex1_t'))
x0<-as.matrix(read.table('Ex1_x'))
y<-as.matrix(read.table('Ex1_y'))
plot(x00,x0,type='b', col = 'blue',xlim=c(0,6),ylim=c(-1,1),xlab = 'x',ylab='')
par(new=T)
plot(x00,y,type='b', col = 'red',xlim=c(0,6),ylim=c(-1,1),xlab = 'x',ylab='')
legend(4.5,-0.35, c('Entrada','Saída'),lty=c(1,1), lwd=c(2.5,2.5),col=c('blue','red'))
total_graus<-1
corlin = rainbow(total_graus)

mse = matrix(0,ncol = total_graus, nrow = 1)
train<-sample((20))
yt<-y[train[1:(0.7*20)]]
yv<-y[train[((0.7*20)+1):20]]
par(new=T)
plot(x00[train[1:(0.7*20)]],yt,type='p',pch = 21,bg = 'red', col = 'black',xlim=c(0,6),
     ylim=c(-1,1),xlab = 'x',ylab='')

plot(x00,y,type='b', col = 'black',xlim=c(0,6),ylim=c(-1,1),xlab = 'x',ylab='')

#Escolhendo o grau do polinomio para aproximacao o qual sera a primeira camada
i <- 1
grau <- i
x <- matrix(1, nrow = length(x0), ncol = (grau + 1))
for (j in 1:(grau + 1))
{
  x[, j] <- x0 ^ (j - 1)
  if (j > 1)
  {
    x[, j] <- scale(x[, j])
  }
}
xt <- matrix(1, nrow = (0.7 * 20), ncol = (grau + 1))
xt <- x[train[1:(0.7 * 20)], ]
xv <- matrix(1, nrow = length(yv), ncol = (grau + 1))
xv <- x[train[((0.7 * 20) + 1):(20)], ]

#   #No caso da Adaline pode-se calcular os pesos usando-se a Regra Delta
#   theta<-pseudoinverse(xt) %*% yt
eta <- 1e-1
tol <- 0.001
maxepocas <- 10000
par <- 0
xin <- xt
yd <- yt
result <- trainadaline(as.matrix(xt), yt, eta, 0.01, 100, 0)
theta <- result[[1]]
print(theta)
y_hat <- yadaline(xv, theta, 0)
y_hat2 <- yadaline(as.matrix(x), theta, 0)

#Realizando a previsao
par(new = T)
plot(
  x00,
  y_hat2,
  type = 'b',
  col = 'green',
  xlim = c(0, 6),
  ylim = c(-1, 1),
  xlab = '',
  ylab = ''
)
par(new = T)
plot(
  x00[train[((0.7 * 20) + 1):(20)]],
  y_hat,
  type = 'p',
  pch = 21,
  bg = 'green',
  xlim = c(0, 6),
  ylim = c(-1, 1),
  xlab = 'x',
  ylab = ''
)
par(new = T)
mse[i] = sum(((yv - y_hat) ^ 2) / length(y))
graus<- seq(1,total_graus,1)
legend(4.5,-0.35, c('Original','Previsto'),lty=c(1,1), lwd=c(2.5,2.5),col=c('black','green'))
