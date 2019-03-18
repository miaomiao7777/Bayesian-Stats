# plotting normal likelihood: Q2-1
n <-100; beta<- 0.7; tau <- 1;
x <- runif(n,10,20);
y <- rnorm(n,beta*x,1/sqrt(tau));
plot(x,y);
b <- sum(x*y)/sum(x*x);
abline(0,b)
sdb <-1/sqrt(tau*sum(x*x))
betavalues <- seq(-1,2.5, length=100) ;
#betavalues <- seq(b-4*sdb,b+4*sdb,length=200);
plot(betavalues, dnorm(betavalues, b, sdb), type='l');
###################################################;
# Qb2-b3
##########################################

set.seed(50)
n <- 100; beta <- 0.5; nbeta <- 100;
x <- runif(n,1,2);
y <- rexp(n,exp(-beta*x));
plot(x,y);
betavalues <- seq(0.2,0.8, length=100);
explikel <- function(b){exp(-b*sum(x))*exp(-sum(y*exp(-b*x)))};
LV <- rep(0,nbeta);
for (i in 1:100){ LV[i] <- explikel(betavalues[i])};
plot(betavalues,LV,type='l');

#plot(betavalues,exp(-betavalues*sum(x)) * exp(-sum(y*exp(-betavalues*x))),type='l');


########################################
# Q3
#######################################
n <- 10; theta <- 3; 
y <- rpois(n,3); ybar <- mean(y);
thetahat <- (n*ybar-1)/n;
tau <- n^2/(n*ybar-1);
sigma <- sqrt(1/tau);
tv <- seq(0.1,5,length=30);
plot(tv,dgamma(tv,sum(y)-1,n),type="l",ylim=c(0, 1)); 
#the 2nd parameter seems like the exponent of theta,n is the obseervation number 
points(tv,dnorm(tv,thetahat,sigma),pch=1) #pch stands for the shape of the points 


