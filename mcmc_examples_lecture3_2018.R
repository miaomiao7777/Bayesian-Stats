# Markov chain AR1 example
#####################################################################################

set.seed(1)
nsim <- 10^4
x[1]=rnorm(1)
r=0.9     #
for (i in 2:nsim){
  x[i]=r*x[i-1]+rnorm(1) }
hist(x,freq=F,col="wheat2",main="")
curve(dnorm(x,sd=1/sqrt(1-r^2)),add=T,col="tomato")

########################################################################################
# independence M-H: simulate Beta(3,4) using MH with U(0,1) as the proposal: q(x,y)=q(y)=1
#########################################################################################
alpha =3; beta=4; x.start= 0 # initial values
Nsim = 5000;
set.seed(1001);
X=rep(x.start,Nsim) # initialize the chain
 for (i in 2:Nsim){
  Y=runif(1)
  ratio= min(dbeta(Y,alpha,beta)/dbeta(X[i-1],alpha,beta),1)
 # ratio= min((dbeta(Y,alpha,beta)*q(Y,X[i-1]))/(dbeta(X[i-1],alpha,beta)*q(X[i-1,Y])),1) 
 #  q(Y,X[i-1])=1 for uniform dist.
   X[i]=X[i-1] + (Y-X[i-1])*(runif(1)<ratio)
 }

#################################################################################################
# RW M-H: simulate Beta(3,4) using MH with N(0,1) as the proposal
# since N(0,1) is symmetric around 0, for the RW kernel with N(0,1), q(x,y)=q(y,x)
###############################################################################################
alpha =3; beta=4; x.start= 0 # initial values
Nsim = 5000;
mu = 0;
sigma = 1;
set.seed(1001);
X=rep(x.start,Nsim) # initialize the chain
for (i in 2:Nsim){
  Y=X[i-1] + rnorm(1,mean=mu,sd=sigma) # note that rnorm is symmetric around 0
  ratio=min(dbeta(Y,alpha,beta)/dbeta(X[i-1],alpha,beta),1) #ratio=(Y^2*(1-Y)^3)/(X[i-1])^2*(1-X[i-1])^3
  X[i]=X[i-1] + (Y-X[i-1])*(runif(1)<ratio)
}


# MH - Bivariate normal using bivariate t-distributions. 
library("BaM")
library("MASS")
nsim <- 1000
sigma <- matrix(c(1,-.9,-.9,1),2,2)
theta.matrix <- matrix(0,nsim,2)
set.seed(990)
metropolis <- function(theta.matrix,reps,I.mat) {
  for (i in 2:reps) {
    theta.star <- mvrnorm(1,theta.matrix[(i-1),],I.mat)/ (sqrt(rchisq(2,5)/5))
    r <- dmultinorm(theta.star[1],theta.star[2],c(0,0),I.mat)/
      dmultinorm(theta.matrix[(i-1),1],theta.matrix[(i-1),2],c(0,0),I.mat)
  
    if (r > runif(1)) theta.matrix[i,] <<- theta.star
    else theta.matrix[i,] <<- theta.matrix[(i-1),]
        }
   theta.matrix
}
metropolis(theta.matrix,nsim,sigma)
theta.matrix[801:1000,]
par(mfrow=c(1,2),mar=c(2,2,2,2),oma=c(1,1,3,1))
plot(theta.matrix[801:1000,],pch=".",xlab="",ylab="",xlim=c(-3,3),ylim=c(-3,3),cex=3)
plot_walk_MH(theta.matrix[801:1000,])
mtext(outer=TRUE,side=3,cex=1.2,"Metropolis-Hastings Demonstartion,Bivariate Normal")