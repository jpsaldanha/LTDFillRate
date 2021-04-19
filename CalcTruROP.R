# Calculate the True ROP

#Uniform Distribution
P2<-0.99
Q<-94
mu<-100
sig<-20
a<-66
b<-134

sU<-P2*(b-a)+a-(Q/2)

# The error function
erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1

#Truncated Normal
library(truncnorm)

#tn<-rtruncnorm(10^6,0,Inf,100,20)
#mean(tn)
#sd(tn)

#curve(dnorm(x,100,20),from = 0,to=300)

P2<-0.85
Q<-25
mu<-100
sig<-20

#Erf1<-erf(mu/(sig*sqrt(2)))
#Erf2<-erf((Q+s-mu)/(sig*sqrt(2)))
#Erf3<-erf((s-mu)/(sig*sqrt(2)))
#Expon<-sig*sqrt(2)*(exp(-(Q+s-mu)^2/(2*sig^2))-exp(-(s-mu)^2/(2*sig^2)))
#Den<-1/sqrt(pi)*(1-(1/2)*(1+erf(-mu/(sig*sqrt(2)))))
#VQ<-Den*(Expon+sqrt(pi)*((mu-s)*Erf3+(Q+s-mu)*Erf2+Q*Erf1))

VQ<-function(z){(1/(2*sqrt(pi)*(1-(1/2)*(1+erf(-mu/(sig*sqrt(2)))))))*
                         (sig*sqrt(2)*(exp(-(Q+z-mu)^2/(2*sig^2))-
                                         exp(-(z-mu)^2/(2*sig^2)))+
                            sqrt(pi)*((mu-z)*erf((z-mu)/(sig*sqrt(2)))+
                                        (Q+z-mu)*erf((Q+z-mu)/(sig*sqrt(2)))+
                                        Q*erf(mu/(sig*sqrt(2)))))}

sTN<-optimize(function(z){abs(P2-
      ((1/(2*sqrt(pi)*(1-(1/2)*(1+erf(-mu/(sig*sqrt(2)))))))*
      (sig*sqrt(2)*(exp(-(Q+z-mu)^2/(2*sig^2))-exp(-(z-mu)^2/(2*sig^2)))+
      sqrt(pi)*((mu-z)*erf((z-mu)/(sig*sqrt(2)))+
                  (Q+z-mu)*erf((Q+z-mu)/(sig*sqrt(2)))+
                  Q*erf(mu/(sig*sqrt(2))))))/
      Q)},lower=0,upper=(mu+6*sig))$minimum


#curve(VQ(x),from = 0,to = 300)

#sN<-optimize(function(z){abs(((Q/sig)*(1-P2))-((dnorm(z)-z*(1-pnorm(z)))-
#                        (dnorm(z+Q/sig)-(z+Q/sig)*(1-pnorm(z+Q/sig)))))},
#             lower = 0,upper = 6)$minimum*sig+mu

#normalLoss<-function(z){Q-sig*((dnorm(z)-z*(1-pnorm(z)))-
#                        (dnorm(z+Q/sig)-(z+Q/sig)*(1-pnorm(z+Q/sig))))}

# curve(normalLoss(x),from = -6,to=6)

# Lognormal

curve(dlnorm(x,4.585559829,0.1980422),from = 0,to=300)

P2<-0.8
Q<-19
mu<-4.585559829
sig<-0.1980422
CV<-0.2

sLN<-optimize(function(z){abs(P2-
          (Q/2*(1-erf((mu-log(z))/(sig*sqrt(2))))+
          (z+Q)/2*(erf((mu-log(z))/(sig*sqrt(2)))-erf((mu-log(z+Q))/
                                                         (sig*sqrt(2))))-
          exp(mu+sig^2/2)/2*(erf((mu+sig^2-log(z))/(sig*sqrt(2)))-
                               erf((mu+sig^2-log(z+Q))/(sig*sqrt(2)))))/
            Q)},lower=0,upper=(100+6*CV*100))$minimum

