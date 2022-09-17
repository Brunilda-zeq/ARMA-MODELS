setwd("C:/Users/Bru/Desktop/1i ergasia dedrami")
source("norm_pdf1.R")
source("autocorr_1.R")
source("likeI_ar1.R")
source("likel_ar2.R")
source("likel_ar3.R")
source("log_norm_pdf1.R")
source("ols_1.R")

       

# an MA1 model: v(t=)  c + u(t) + theta1*u(t=1), u(t)=N(0,1)
yy=matrix(rep(0,2000),2000)
c=0.1
theta1=0.7 # theta2=0.6 
e_i_lag1=0
for  (i  in  2:2000)
  
{

  e_i=rnorm(1, mean=0, sd=1)

  yy[i]=c+e_i+ theta1*e_i_lag1
  e_i_lag1 = e_i
}
plot(yy,type='l',col="#377eb8",main="MA(1) model",xlab="Time",ylab="y")
legend("topleft",lty=1,col=c("#377eb8"),
       legend=c("y(t)"),cex = 0.6)




rho=matrix( rep(0,20)  ,  20  )                
for (l in 1:20)
{
  w=autocorr_1(yy,l)
  ff=unlist(w)
  rho[l]=ff[1]
}


plot(rho,type='l',col="#e41a1c",main="autocorrelation function MA(1)",xlab="Time",ylab="r")
legend("topleft",lty=1,col=c("#e41a1c"),
       legend=c("r"),cex = 0.6)
#an AR2 model: v(t)= c + phi"v(t=1) +u(t), u(t)  N(0,sigma)         
yy=matrix(rep(0,2000),2000)
c=0.1
phi=0.5

phi2= 0.1

for  (i  in  3:2000)
{
  yy[i]=c+phi*yy[i-1]+rnorm(1,mean=0,sd=2)
  
}
plot(yy,type='l')
plot(yy,type='l',col="#377eb8",main="AR(2) model",xlab="Time",ylab="y")
legend("topleft",lty=1,col=c("#377eb8"),
       legend=c("y(t)"),cex = 0.6)
for (l in 1:20)
{
  w=autocorr_1(yy,l)
  ff=unlist(w)
  rho[l]=ff[1]
}
plot(rho,type='l',col="#e41a1c",main="autocorrelation function AR(2)",xlab="Time",ylab="r")
legend("topleft",lty=1,col=c("#e41a1c"),
       legend=c("r"),cex = 0.6)
# estimate an ARE , PACF
T=length(yy)
y=matrix( yy[2:T],T-1)
xo=matrix(yy[1:T-1],T-1)
c=matrix( rep(1,T-1),T-1)
y1=y
x1=cbind(c,xo)
q1=ols_1(y1,x1)
q1
y2=matrix(yy[3:T],T-2)
xo1=matrix(yy[2:(T-1)],T-2)
xo2=matrix(yy[1:(T-2)],T-2)
c=matrix( rep(1,T-2),T-2)
x2=cbind(c,xo1,xo2)
q2=ols_1(y2,x2)
q2
y3=matrix(yy[4:T],T-3)
xo1=matrix(yy[3:(T-1)],T-3)
xo2=matrix(yy[2:(T-2)],T-3)
xo3=matrix(yy[1:(T-3)],T-3)
c=matrix( rep(1,T-3),T-3)
x3=cbind(c,xo1,xo2,xo3)
q3=ols_1(y3,x3)
q3
# AIC
theta0=c(0.5,0.9,5)
test=likel_ar1(theta0, yy)
low=c(0,0,0)
h=c(1,0.99,50)
theta_opt<-optim(theta0,likel_ar1, gr=NULL,y=yy,
                 method=  c( "L-BFGS-B"),
                 lower=low, upper= h,
                 hessian=  TRUE)

log_likel_1=  -theta_opt$value
aic=-(2/length(T)*log_likel_1)+2/length(T)*3
aic
theta0=c(0.5,0.9,0.1,0.9,var(yy))
test=likel_ar2(theta0, yy)
low=c(0,0,0)
h=c(1,0.97,0.97,5)
theta_opt<-optim(theta0,likel_ar2, gr=NULL,y=yy,
                 method=  c( "L-BFGS-B"),
                 lower=low, upper=h,
                 hessian= TRUE)

log_likel_2=-theta_opt$value
aic=-(2/length(T)*log_likel_2)+2/length(T)*3
aic
theta0<- c(0.5,0.9,0.1,0.8,0.4,var(yy))
test<- likel_ar3(theta0,yy)
low<-c(0,0,0)
h<- c(1,0.97,0.97,50)
theta_opt<- optim(theta0,likel_ar3,gr=NULL,y=yy,
                  method = c('L-BFGS-B'),
                  lower = low, upper = h,
                  hessian = T)
log_likel_3<- -theta_opt$value
aic=-(2/length(T)*log_likel_3)+2/length(T)*3
aic



