############## Q1-2
set.seed(124);
theta=2;
x=runif(1000,0,1);
y=-1/theta*log(x);
#density<- function(y) {density = theta*exp(-theta*y)
           #return(y)}
hist(y, freq=FALSE)
lines(density(y))

############### Q2-2,3
set.seed(124)
x=runif(300,0,1);
y1=3*exp(-3*x);
y2=exp(-x);
y=0.6*y1+0.4*y2;
par(mfrow=c(3, 1))
hist(y1, freq=FALSE, xlab='y=3*exp(-3*x)')
hist(y2, freq=FALSE,xlab = 'y=exp(-x)' )
hist(y, freq=FALSE,xlab = 'y=0.6*3*exp(-3*x)+0.4*exp(-x)')

################# Q3 
set.seed(100)
G <- 600;
M <- dbeta(1.875,3,3);
draws <- 0; 
tdraws <- 0;

while (length(draws) < G + 1)  {
  U <- runif(2)
  if (U[2] <- (dbeta(U[1],3,3)/M*U[1]))
    draws <- c (draws,U[1])

  tdraws <- tdraws +1 
}
target <- draws[2:(G+1)]
arate <- G/tdraws ;
print("mean = "); mean(target)
print("sd = "); sd(target)

# The ture mean is 1/(1+beta/alpha)=0.5  
# The true variance is alpah*beta/square(alpha+beta)(alpha+beta+1)=0.0357

##############################  Q4 what's wrong here?
set.seed(123)
G <- 10000;
a <- exp(-1);
b <- 1;
c = b/(1-a);
draws <- 0; 
tdraws <- 0;

while (length(draws) < G + 1){
  U <- runif(1,a,b)
    draws <- c(draws,1/(1+U^2)*c*exp(-U)/dbeta(U,2,3))
  
  tdraws <- tdraws +1   
  
}
target <- draws[2:(G+1)]
arate <- G/tdraws ; 
print("mean = "); mean(target)
print("sd = "); sd(target)
############################## Q4
set.seed(123)
G <- 10000;
a <- exp(-1);
b <- 1;
c = b/(1-a);
xvalues=runif(10000,a,b);
g=1/(1+xvalues^2);
f=c*exp(-xvalues);
h=dbeta(xvalues,2,3);
w=f/h;
weighted.mean(g,w)

############################## Q5
n <- 200;
rho=0.1;
x1 <- rep(0,n);
x2 <- rep(0,n);
x0 <- -1.5;
x1[1] <- x0;
for (i in 2:n) {
  x2[i] <- rnorm(1,rho*x1[i-1],1-rho^2)
  x1[i] <- rnorm(1,rho*x2[i],1-rho^2) }  
y=c(x1[-150:-1],x2[-150:-1])
par(mfrow=c(3, 1))
hist(y,freq=FALSE)
lines(density(y))
##############################
n <- 500;
rho=0.1;
x1 <- rep(0,n);
x2 <- rep(0,n);
x0 <- -1.5;
x1[1] <- x0;
for (i in 2:n) {
  x2[i] <- rnorm(1,rho*x1[i-1],1-rho^2)
  x1[i] <- rnorm(1,rho*x2[i],1-rho^2) }  
y=c(x1[-150:-1],x2[-150:-1])
hist(y,freq=FALSE)
lines(density(y))
##############################
n <- 5000;
rho=0.1;
x1 <- rep(0,n);
x2 <- rep(0,n);
x0 <- -1.5;
x1[1] <- x0;
for (i in 2:n) {
  x2[i] <- rnorm(1,rho*x1[i-1],1-rho^2)
  x1[i] <- rnorm(1,rho*x2[i],1-rho^2) }  
y=c(x1[-150:-1],x2[-150:-1])
hist(y,freq=FALSE)
lines(density(y))
##############################
n <- 10000;
rho=0.1;
x1 <- rep(0,n);
x2 <- rep(0,n);
x0 <- -1.5;
x1[1] <- x0;
for (i in 2:n) {
  x2[i] <- rnorm(1,rho*x1[i-1],1-rho^2)
  x1[i] <- rnorm(1,rho*x2[i],1-rho^2) }  
y=c(x1[-150:-1],x2[-150:-1])
hist(y,freq=FALSE)
lines(density(y),type='b')

library(MASS)
bivn <- mvrnorm(10000, mu = c(0, 0), Sigma = matrix(c(1, 0.1, 0.1, 1), 2))
# now we do a kernel density estimate
bivn.kde <- kde2d(bivn[,1], bivn[,2], n = 100)
lines(density(bivn))
# now plot your results
contour(bivn.kde)
image(bivn.kde)
persp(bivn.kde, phi = 45, theta = 30)

##############################
n <- 200;
rho=0.97;
x1 <- rep(0,n);
x2 <- rep(0,n);
x0 <- 1.5;
x1[1] <- x0;
for (i in 2:n) {
  x2[i] <- rnorm(1,rho*x1[i-1],1-rho^2)
  x1[i] <- rnorm(1,rho*x2[i],1-rho^2) }  
y=c(x1[151:200],x2[151:200])
par(mfrow=c(3, 1))
hist(y,freq=FALSE)
lines(density(y))
##############################
n <- 500;
rho=0.97;
x1 <- rep(0,n);
x2 <- rep(0,n);
x0 <- 1.5;
x1[1] <- x0;
for (i in 2:n) {
  x2[i] <- rnorm(1,rho*x1[i-1],1-rho^2)
  x1[i] <- rnorm(1,rho*x2[i],1-rho^2) }  
y=c(x1[151:200],x2[151:200])
hist(y,freq=FALSE)
lines(density(y))
##############################
n <- 5000;
rho=0.97;
x1 <- rep(0,n);
x2 <- rep(0,n);
x0 <- 1.5;
x1[1] <- x0;
for (i in 2:n) {
  x2[i] <- rnorm(1,rho*x1[i-1],1-rho^2)
  x1[i] <- rnorm(1,rho*x2[i],1-rho^2) }  
y=c(x1[151:200],x2[151:200])
hist(y,freq=FALSE)
lines(density(y))
library(MASS)
bivn <- mvrnorm(10000, mu = c(0, 0), Sigma = matrix(c(1, 0.97, 0.97, 1), 2))
lines(density(bivn),type='b')
##############################  Q6
# since Laplace distribution is symmetric around 0, for the RW kernel with N(0,1), q(x,y)=q(y,x)
x.start= 0 # initial values
Nsim = 5000;
mu = 0;
sigma = 1;
set.seed(123);
X=rep(x.start,Nsim) # initialize the chain
for (i in 2:Nsim){
  Y=X[i-1] + rnorm(1,mean=mu,sd=sigma) # note that rnorm is symmetric around 0
  ratio=min(0.5*exp(-abs(Y))/0.5*exp(-abs(X[i-1])),1) 
  X[i]=X[i-1] + (Y-X[i-1])*(runif(1)<ratio)
}
hist(X,freq=F,col="wheat2",main="")
#install.packages(LaplacesDemon)
#y=dlaplace(10000,0, 1)
#lines(density(y))
print("mean = "); mean(X)
print("sd = "); sd(X)