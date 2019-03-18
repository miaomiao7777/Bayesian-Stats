#################################################################
## Model checking using prior predictive distribution 
##################################################################


#################################################################;
# 1. Generate AR1 data first: yt = rho*y(t-1) + et                        ;
#################################################################;



set.seed(7)
n <- 51;  #total number of obs including initial value y1
rho <- 0.7; # AR coeff 
tau <- 1;   # precision
y <- rep(0,n); # initialize y vector
y[1] <- 0; # set the initial value
for (i in 2:n){ y[i] <- rho*y[i-1]+ rnorm(1,0,1/sqrt(tau))}; # generate 50 obs from the AR model
y0 <- y[2:51];  # dependent var y[-1]
ylag <- y[1:50]; # Lagged dependent var y[-n]

# ols estimate of rho: r  <- sum(ylag*y0)/sum(ylag*ylag);
# e <- rep(0,n-1);
# for (i in 1:n-1){ e[i] <- y0[i]-r*ylag[i]}; residual
# e0 <- e[-1]; residual
# elag <- e[-(n-1)]; lagged residual
# eacf <- cor(e0,elag); auto correlation of residual


#rvalues <- seq(0.5,0.9,length=200);
#sdr <- 1/sqrt(tau*sum(ylag*ylag))
# plot(rvalues, dnorm(rvalues, r, sdr), type='l');

################################################################
# 2. generate predictive distribution of residual autocorrelation
################################################################

set.seed(5)
nrep <- 1000; # number of simulations for predictive distribution
rhovalues <- runif(nrep,0,1);   #generate rho from uniform prior  
yb <- matrix(0,nrow=nrep,ncol=n);# initialize  yb matrix
e  <- matrix(0,nrow=nrep,ncol=n-1); # initialize error term
edata <- matrix(0,nrow=nrep,ncol=n-1); # initialize error term for data  
e0 <- matrix(0,nrow=nrep,ncol=n-2);  
e1 <- matrix(0,nrow=nrep,ncol=n-2); 
edata0 <- matrix(0,nrow=nrep,ncol=n-2);
edata1 <- matrix(0,nrow=nrep,ncol=n-2);
eacf <- matrix(0,nrow=nrep,ncol=1); 

set.seed(3)

for (i in 1:nrep) {
  for (j in 2:n) {
    yb[i,j] <- rhovalues[i]*yb[i,j-1] + rnorm(1,0,1/sqrt(tau)) ;
    e[i,j-1] <- yb[i,j]- rhovalues[i]*yb[i,j-1]; 
    edata[i,j-1] <- y[j] -rhovalues[i]*y[j-1];}
   e0[i,] <- e[i,-(n-1)];
   e1[i,] <- e[i,-1];
   edata0[i,] <- edata[i,-(n-1)];
   edata1[i,] <- edata[i,-1];   
   eacf[i] <- cor(e0[i,],e1[i,]) - cor(edata0[i,],edata1[i,]); 
   
}
hist(eacf);


