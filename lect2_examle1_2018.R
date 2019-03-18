# plotting normal likelihood for a regression model
# y(i) = beta*x(i) + epsilon(i) epsilon(i)-N(0,1/tau)

n <-50   # the number of obserations I want to generate
beta<- 0.7 # parameter value for beta  
tau <- 1   # parameter value for tau (precsion)

# generate explanatory variable from uniform dist random
# if you want replicate your experiment you can set the seed
# set.seed(1)
x <- runif(n,10,20)  

# geerate y given x
y <- rnorm(n,beta*x,1/sqrt(tau))

plot(x,y)

# calculate OLS estimate of beta, b=betahat 
b <- sum(x*y)/sum(x*x);

abline(0,b)  #draw line with no intercep and slope =b

# derive standard error of b
sdb <-1/sqrt(tau*sum(x*x))

# set the betavalues over which the likelihood will be plotted
# to be 
betavalues <- seq(0.64,0.74, length=100) ;

# You can also set the betavalues interval base on standard error of b
#betavalues <- seq(b-4*sdb,b+4*sdb,length=200);

# from pp20 of lecture note 20 Ieq 7), the likelihood
# of the model is normal with mean=xb and variance:sdb^2
# on page 20, there are typos:
# the term before (b-beta)^2 should be sum of squares of x, not y.

plot(betavalues, dnorm(betavalues, b, sdb), type='l');


##############################################################;
# plotting likelihood for Bernoulli Trial
# Note that Bernoulli Trial is binomal random variale with 1 trial
########################################################
n <-50        # No of random var generated
theta <-0.2   # parameter of Binomial Random alraible


# generate n Bernoulli trail r.v (or n binomial r.v. with 1 trial)
# with parameter value theta
z <- rbinom(n,1,theta) 

s <- sum(z)  # same s on pp22 of lecture note.
thetavalues <- seq(0,1,length=100)  # set the range for likelihood plot

# the likelihood of Bernoulli trial is Beta dist family
# with alpha=s+1 and beta=n-s+1 (see pp22-23 of the note)

plot(thetavalues,dbeta(thetavalues,s+1,n-s+1),type='l');

############################################################
