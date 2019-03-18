
# Question 2
set.seed(123)
ndraws <- 300
x1 <- -1/3*log(runif(ndraws)) # generate exp(3)
x2 <- -log(runif(ndraws))     # generate exp(1) 
x3 <- runif(ndraws)
mixedx <- rep(ndraws,0)

#generate mxed exponential
for (i in 1:ndraws) { if (x3[i] <= 0.6) mixedx[i] <- x1[i] 
                     else mixedx[i] <- x2[i] }
plot(density(x1))
plot(density(x2))
plot(density(mixedx))
print (mean(x1))
print (mean(x2))
print (mean(mixedx))

#############################################################
# Question 3
set.seed(124)

# get the maximum value of the density for beta(3,3): 1.875 at 0.5
M <- optimize(f=function(x){dbeta(x,3,3)},interval=c(0,1),maximum=T)$objective
nsim =600;
x=NULL
while (length(x)<nsim){
  y=runif(nsim*M)
  x=c(x,y[runif(nsim*M)*M<dbeta(y,a,b)])}
x=x[1:nsim]
hist(x, freq=FALSE);
lines(density(dbeta(x,3,4)));
ar_mean=mean(x);
ar_var = var(x);

# Q4.b
set.seed(100)
nsim=10^4
y=rbeta(nsim,2,3)    # generate beta(2,3) 
weit=dexp(y,1)/(dbeta(y,2,3)*(1-exp^(-1)))
EV = 1/nsim*sum(weit/(1+y^2))                
EV

# Q5
set.seed(100)
n <- 500; #200,500,5000,10000
r <- 0.1; # 0.97
x <- rep(0,n);
y <- rep(0,n);
y0 <- 1.5; #1.5
y[1] <- y0;
for (i in 2:n) {
  x[i] <- rnorm(1,r*y[i-1],sqrt(1-r^2))
  y[i] <- rnorm(1,r*x[i],sqrt(1-r^2)) }  
hist(x,freq=F,col="wheat2",main="")
curve(dnorm(x,mean=0,sd=1),add=T,col="tomato")
print (mean(x))
print (var(y))



