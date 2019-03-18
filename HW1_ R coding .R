# Q2(1)
beta = 0.7
tau = 1
x = runif(100, 0 ,20)
y = rnorm(100, beta*x, 1/sqrt(tau))
b = -sum(x*y)/sum(x*x)
sdb = 1/sqrt(tau*sum(x*x))
betavalues = seq(-1,2.5, length = 100)
plot(betavalues, dnorm(betavalues, b, sdb), type='l')
#Q2(2)
