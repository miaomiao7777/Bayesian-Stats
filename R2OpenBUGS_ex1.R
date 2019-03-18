install.packages("R2OpenBUGS")
library(R2OpenBUGS)

bugsex1 <- function() {
    pi ~ dbeta(1,1)%_%I(0.2,0.45)
    r ~ dbin(pi,N) 
}
N <- 14
r <- 4
filename <- file.path("C:/Users/wangsaja/Desktop/openbugs/","bugsex1.txt")
write.model(bugsex1,filename)
filename
data <- list("N","r")
#inits <- function() {list(pi=0.5)}
parameters <- c("pi")
#bugsex1.sim <- bugs(data,inits=NULL,parameters,filename,codaPkg=TRUE,n.iter=1000)
bugsex1.sim <- bugs(data,inits=NULL,parameters,filename,n.iter=1000)
print(bugsex1.sim)
plot(bugsex1.sim)
#print(mean(pi))
#print(quantile(pi, c(.25, .75)))
#library("coda")
#codaex1 <- read.bugs(bugsex1.sim)
#plot(codaex1)