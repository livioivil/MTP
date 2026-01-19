x <- -300:300/100
par(mai=c(1,1,.1,.1))
plot(x,exp(2*x)/(1+exp(2*x)),type="l",ylim=0:1, xlab="direction of complete separation", ylab="fitted probability")
lines(x,exp(3*x)/(1+exp(3*x)))
lines(x,exp(10*x)/(1+exp(10*x)))
lines(x,exp(100*x)/(1+exp(100*x)))
rug(rexp(2), side=3, col=2)
rug(-rexp(2), col=2)