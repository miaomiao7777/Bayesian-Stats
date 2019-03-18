# sampling from the truncated normal with mean=m and sd=s
# and defined over the interval (a,b)
set.seed(1);
n <-1000;
m <- 0;
s <- 1;
a <- 0.5;
b <- 1.5;
y <- qnorm(pnorm(a,m,s)+ runif(1000)*(pnorm(b,m,s)-pnorm(a,m,s)),m,s);
hist(y, freq=FALSE)
lines(denisty(y))

#####################################################################################
# accept-Reject sampling example 1
# target density: f(x) standard normal 
# proposal density: g(x) Laplace distribution =1/2exp(-abs(x))
# algorithm:
# 1. Generate 3 unifrom r.v. U1-u3
# 2. since both normal and laplace are symmetric, use exponential e^(-x)
# for laplace, accept or rejct, and if accept, assign a positive
# or negative values with .5 prob
# 3. use u1 to generate exponential
# 4. max of f(x)/g(x) occurs at x=1 and M=sqrt(e/(2*pi)) 
# 5. if u2 <= f(x)/(M.g(x)) and u3 <=0.5 then return negative x
# u2 <= f(x)/(M.g(x)) and u3  > 0.5 then return positive x
#####################################################################################

set.seed(103);
G <- 500;
draws <- 0;
tdraws <- 0;

while (length(draws) < G + 1){
  U <- runif(3)
  if (U[2] <= exp(-log(U[1]) - ((-log(U[1]))^2)/2 - 1/2) & U[3] <= 1/2)
    draws <- c(draws, -log(U[1]))
  if (U[2] <= exp(-log(U[1]) - ((-log(U[1]))^2)/2 - 1/2) & U[3] >  1/2)
    draws <- c(draws,  log(U[1]))
 tdraws <- tdraws +1 ;
  }
target <- draws[2:(G+1)];
arate <- G/tdraws ;
print("mean =" ); mean(target)
print("sd =" ); sd(target)

#################################################################
# A-R example 2
# # target density= 1-abs(x),x(-1,1)
# use accept-reject to draw from truncated 
# use u(a,b)=a+(b-a)*u(0,1) as the proposal density
# if u2 <= f(x)/(M)u(a,b) return x=a+(b-a)*U(0,1)
######################################################################
set.seed(100)
G <- 500;
a <- -1;
b <- 1;
M <- 1;
draws <- 0; 
tdraws <- 0;

while (length(draws) < G + 1){
  U <- runif(2)
  if (U[2] <= (1-abs(a+(b-a)*U[1]))/M)
      draws <- c(draws, a+(b-a)*U[1])
    
  tdraws <- tdraws +1   
    
}
target <- draws[2:(G+1)]
arate <- G/tdraws ;
#print("mean = "); mean(target)
#print("sd = "); sd(target)


hist(target, freq=FALSE);
lines(density(target));
################################################################################
# importance sampling
# example from Robert and Casella 
################################################################################
x=rnorm(10^8) #whole sample
bound=qnorm(c(.5,.75,.8,.9,.95,.99,.999,.9999)) # t=0, 0.67, 0.84, 1.28 1.65, 2.32, 2,58, 3.09, 3.72
 res=matrix(0,ncol=8,nrow=7)
 for (i in 2:8) 
   for (j in 1:8)
     res[i-1,j]=mean(x[1:10^i]<bound[j])
 matrix(as.numeric(format(res,digi=4)),ncol=8)

#variance around 0 is 1/(4n). To achieve a precision less than 4 decimal point requires 10^8 simulations
# at tail end, requires even more simulations
# Calculate P(x>4.5) using importance sampling
# consider a distribution with support restricted to (4.5, infinity): g(x) = exp^(-x)/int_(4.5)^(inf) exp^(-x)=exp^(-(y-4.5))
# use truncated exponential distribution
  
#pnorm(-4.5,log=T);
Nsim=10^3
y=rexp(Nsim)+4.5
weit=dnorm(y)/dexp(y-4.5)
plot(cumsum(weit)/1:Nsim,type="l")
abline(a=pnorm(-4.5),b=0,col="red")

###############################################################
# other importance sampling example
##############################################################


#############################################################
# Gibbs sampling
########################################################


install.packages("ellipse")
library(ellipse)
n <- 50;
r <- 0.3;
y1 <- rep(0,n);
y2 <- rep(0,n);
y0 <- 0;
y1[1] <- y0;
for (i in 2:n) {
    y2[i] <- rnorm(1,r*y1[i-1],sqrt(1-r^2))
    y1[i] <- rnorm(1,r*y2[i],sqrt(1-r^2)) }  
plot(y1, y2, type="l") ;
lines(ellipse(r, level=.95))

  

