rm(list=ls())

#Funcao de custo
J<-function(x,y,theta,lambda,m)
{
  J<-(1/(2*m))*sum(((x%*%theta -y)^2),2)
  #adicionando regularizacao
  # J<-J+sum((lambda/(2*m))*tail(theta,-1)^2)
  return(J)
}

theta_grad<-function(x,theta,lambda,m)
{
  dJ<-matrix(1,nrow = grau+1,ncol = 1)
  for (i in 1:(grau+1))
  {
    dJ[i]<-(x%*%theta -y)*x[,i]/m
  }
  # theta_grad<-t(x)%*%(x%*%theta -y)/m
  # theta_grad<-rbind(theta_grad[1],theta_grad[2:(grau+1)]+tail(theta_grad,-1)*lambda/m)
  return(dJ)
}

#Atualizando parametros
atualiza<-function(theta,dJ,alpha)
{
  theta_new<-theta - alpha*dJ
  return(theta_new)
}

#treinando a rede
train_adaline<-function(x,y,alpha,epoc,lambda)
{
  #Criando a rede Adeline
  epsilon<-sqrt(6)/(grau+1)
  m <- length(x0)
  theta <- matrix(1,nrow = grau+1,ncol = 1)
  theta<- matrix(runif(grau+1,min = -epsilon,max = epsilon),nrow = grau+1,ncol = 1) 
  custo<-matrix(0,nrow = epoc,ncol = 1)
  mat_theta<-vector('list',epoc)
  mat_theta[[1]]<-theta
  for (i in 1:epoc-1)
  {
    custo[i]<-J(x,y,mat_theta[[i]],lambda,m)
    dJ<-theta_grad(x,mat_theta[[i]],lambda,m)
    mat_theta[[i+1]]<-atualiza(mat_theta[[i]],dJ,alpha)
    # print(theta)
  }
  return(list(custo,mat_theta))
}

x0<-seq(-5,5,0.1)
y<-x0^2

# x0<-runif(n = 1000, min=-15,max=10)
# y<-(0.5*(x0^2)+3*x0+10) + 10*rnorm(length(x0))

plot(x0,y,type='p')

#Escolhendo o grau do polinomio para aproximacao o qual sera a primeira camada
grau <- 4
x<-matrix(-1,nrow = length(x0),ncol = (grau+1))
for (i in 1:(grau+1))
{
  x[,i]<-x0^(i-1)
}

lambda<-1
alpha<-0.01


epoc<-200
adaline<-train_adaline(x,y,alpha,epoc,lambda)

# plot(x,x%*%adaline[[2]])
plot(adaline[[1]],type='l')
theta<-adaline[[2]]
plot(x%*%mat_theta[[epoc]])


