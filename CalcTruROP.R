# Calculate the True ROP FPR  0.9 < P_2 <= 0.9

# The error function
erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1


# UNIFORM DISTRIBUTION

# Case 1: P2 > 0.9
P2<-0.98
Q<-94
mu<-100
sig<-20
a<-66
b<-134

sU1<-Q*(1-P2)*(b-a)+b

# Case 2: P2 <= 0.9
P2<-0.8
Q<-19
mu<-100
sig<-20
a<-66
b<-134

sU2<-P2*(b-a)+a-(Q/2)

# TRUNCATED NORMAL DISTRIBUTION
library(truncnorm)
#curve(dnorm(x,100,20),from = 0,to=300)

# Case 1: P2 > 0.9

P2<-0.80
Q<-19
mu<-100
sig<-20

#Truncated Normal Expected Shorts
TNES<-function(z)
{
  ((mu-z)*sqrt(pi/2)+sig*exp(-(1/2)*((z-mu)/sig)^2)+
     sig*sqrt(pi/2)*((z-mu)/sig)*erf((z-mu)/(sig*sqrt(2))))/
    (sqrt(2*pi)*(1-0.5*(1+erf(-mu/(sig*sqrt(2))))))
}

sTN1<-optimize(function(z){abs(Q*(1-P2)-(((mu-z)*sqrt(pi/2)+sig*exp(-(1/2)*((z-mu)/sig)^2)+
                                      sig*sqrt(pi/2)*((z-mu)/sig)*erf((z-mu)/(sig*sqrt(2))))/
                                      (sqrt(2*pi)*(1-0.5*(1+erf(-mu/
                                      (sig*sqrt(2))))))))},lower=0,upper=(mu+6*sig))$minimum

#sN1<-optimize(function(z){abs(((Q/sig)*(1-P2))-((dnorm(z)-
#                                z*(1-pnorm(z)))))},lower = 0,upper = 6)$minimum*sig+mu
curve(TNES(x),from = 0,to=mu+6*sig)

# Case 2: P2 <= 0.9

P2<-0.85
Q<-25
mu<-100
sig<-20

TNVQ<-function(z){(1/(2*sqrt(pi)*(1-(1/2)*(1+erf(-mu/(sig*sqrt(2)))))))*
                         (sig*sqrt(2)*(exp(-(Q+z-mu)^2/(2*sig^2))-
                                         exp(-(z-mu)^2/(2*sig^2)))+
                            sqrt(pi)*((mu-z)*erf((z-mu)/(sig*sqrt(2)))+
                                        (Q+z-mu)*erf((Q+z-mu)/(sig*sqrt(2)))+
                                        Q*erf(mu/(sig*sqrt(2)))))}

sTN2<-optimize(function(z){abs(P2-
      ((1/(2*sqrt(pi)*(1-(1/2)*(1+erf(-mu/(sig*sqrt(2)))))))*
      (sig*sqrt(2)*(exp(-(Q+z-mu)^2/(2*sig^2))-exp(-(z-mu)^2/(2*sig^2)))+
      sqrt(pi)*((mu-z)*erf((z-mu)/(sig*sqrt(2)))+
                  (Q+z-mu)*erf((Q+z-mu)/(sig*sqrt(2)))+
                  Q*erf(mu/(sig*sqrt(2))))))/
      Q)},lower=0,upper=(mu+6*sig))$minimum


curve(VQ(x),from = 0,to = 300)

#sN2<-optimize(function(z){abs(((Q/sig)*(1-P2))-((dnorm(z)-z*(1-pnorm(z)))-
#                        (dnorm(z+Q/sig)-(z+Q/sig)*(1-pnorm(z+Q/sig)))))},
#             lower = 0,upper = 6)$minimum*sig+mu

# LOGNORMAL DISTRIBUTION
# The comparison of the lognormal and normal graphs below shows the marginal difference in s
#   for expected shorts calculated by the lognormal and normal distributions below
curve(dlnorm(x,4.585559829,0.1980422),from = 0,to=300)
curve(dnorm(x,100,20),from = 0,to=300)

# Case 1: > 0.9

P2<-0.95
Q<-44
mu<-4.585559829
sig<-0.1980422
CV<-0.2

#Truncated Normal Expected Shorts
LnES<-function(z)
{
  1/2*(exp(mu+sig^2/2)-z-z*erf((mu-log(z))/(sig*sqrt(2)))+
         exp(mu+sig^2/2)*erf((mu+sig^2-log(z))/(sig*sqrt(2))))
}

sLN1<-optimize(function(z){abs(abs(Q*(1-P2))-
                                 (1/2*(exp(mu+sig^2/2)-z-z*erf((mu-log(z))/(sig*sqrt(2)))+
                                 exp(mu+sig^2/2)*erf((mu+sig^2-log(z))/
                                 (sig*sqrt(2))))))},lower=0,upper=(100+6*CV*100))$minimum

curve(LnES(x),from = 0,to=100+6*20)

muX<-100
sigX<-20
sN1<-optimize(function(z){abs(((Q/sigX)*(1-P2))-((dnorm(z)-
                                    z*(1-pnorm(z)))))},lower = 0,upper = 6)$minimum*sigX+muX

# Case 2: P2 <= 0.9
P2<-0.8
Q<-19
mu<-4.585559829
sig<-0.1980422
CV<-0.2



sLN2<-optimize(function(z){abs(P2-
          (Q/2*(1-erf((mu-log(z))/(sig*sqrt(2))))+
          (z+Q)/2*(erf((mu-log(z))/(sig*sqrt(2)))-erf((mu-log(z+Q))/
                                                         (sig*sqrt(2))))-
          exp(mu+sig^2/2)/2*(erf((mu+sig^2-log(z))/(sig*sqrt(2)))-
                               erf((mu+sig^2-log(z+Q))/(sig*sqrt(2)))))/
            Q)},lower=0,upper=(100+6*CV*100))$minimum

muX<-100
sigX<-20
sN1<-optimize(function(z){abs(((Q/sigX)*(1-P2))-((dnorm(z)-z*(1-pnorm(z)))-
                        (dnorm(z+Q/sigX)-(z+Q/sigX)*(1-pnorm(z+Q/sigX)))))},
                        lower = 0,upper = 6)$minimum*sigX+muX

# Bimodal Distribution

# Case 1: P2 > 0.9
P2<-0.95
Q<-44
mu1<-85.9
sig1<-8.6
mu2<-121.1
sig2<-12.1
p<-0.6
q<-0.4 # q = 1-p
CV<-0.2

BMES<-function(z){p*z/2*(erf((z-mu1)/(sig1*sqrt(2)))-1)-mu1*p/2*(erf((z-mu1)/(sig1*sqrt(2)))-1)+
    p/sqrt(2*pi)*sig1*exp(-(z-mu1)^2/(2*sig1^2))+
    q*z/2*(erf((z-mu2)/(sig2*sqrt(2)))-1)-mu2*q/2*(erf((z-mu2)/(sig2*sqrt(2)))-1)+
    q/sqrt(2*pi)*sig2*exp(-(z-mu2)^2/(2*sig2^2))}

curve(BMES(x),from = 0,to=100+6*20)

sBM1<-optimize(function(z)
  {abs(abs(Q*(1-P2))-(p*z/2*(erf((z-mu1)/(sig1*sqrt(2)))-1)-mu1*p/2*(erf((z-mu1)/(sig1*sqrt(2)))-1)+
        p/sqrt(2*pi)*sig1*exp(-(z-mu1)^2/(2*sig1^2))+
        q*z/2*(erf((z-mu2)/(sig2*sqrt(2)))-1)-mu2*q/2*(erf((z-mu2)/(sig2*sqrt(2)))-1)+
        q/sqrt(2*pi)*sig2*exp(-(z-mu2)^2/(2*sig2^2))))},
  lower=0,upper=(100+6*CV*100))$minimum


# Case 2: P2 <= 0.9

P2<-0.8
Q<-19
mu1<-85.9
sig1<-8.6
mu2<-121.1
sig2<-12.1
p<-0.6
q<-0.4 # q = 1-p
CV<-0.2

BMVQ<-function(z)
  {1/sqrt(2*pi)*((z+Q)*sqrt(pi/2)*(p*erf((z+Q-mu1)/(sig1*sqrt(2)))-
   p*erf((z-mu1)/(sig1*sqrt(2)))+(p-1)*(erf((z-mu2)/(sig2*sqrt(2)))-erf((Q+z-mu2)/
   (sig2*sqrt(2)))))+Q*sqrt(pi/2)*(p*erf((z-mu1)/(sig1*sqrt(2)))+p*erf(mu1/(sig1*sqrt(2)))-
   (p-1)*(erf((z-mu2)/(sig2*sqrt(2)))+erf(mu2/(sig2*sqrt(2)))))+mu1*p*sqrt(pi/2)*
   (erf((z-mu1)/(sig1*sqrt(2)))-erf((z+Q-mu1)/(sig1*sqrt(2))))+mu2*(p-1)*sqrt(pi/2)*
   (erf((Q+z-mu2)/(sig2*sqrt(2)))-erf((z-mu2)/(sig2*sqrt(2))))+
   p*sig1*(exp(-(z+Q-mu1)^2/(2*sig1^2))-exp(-(z-mu1)^2/(2*sig1^2)))+
  (sig2-p*sig2)*exp(-(z+Q-mu2)^2/(2*sig2^2))+(p*sig2*-sig2)*exp(-(z-mu2)^2/(2*sig2^2)))}

curve(BMVQ(x),from=0, to=100+6*100*CV)

sBM2<-optimize(function(z)
  {abs(P2-(1/sqrt(2*pi)*(sqrt(pi/2)*(p*(z+Q)*erf((z+Q-mu1)/(sig1*sqrt(2)))-
  p*z*erf((z-mu1)/(sig1*sqrt(2)))+p*Q*erf(mu1/(sig1*sqrt(2)))+(p*z-z)*
  erf((z-mu2)/(sig2*sqrt(2)))+(Q-p*Q+z-p*z)*erf((z+Q-mu2)/(sig2*sqrt(2)))-
  (p-1)*Q*erf(mu2/(sig2*sqrt(2))))+mu1*p*sqrt(pi/2)*(erf((z-mu1)/(sig1*sqrt(2)))-
  erf((z+Q-mu1)/(sig1*sqrt(2))))+mu2*(p-1)*sqrt(pi/2)*(erf((Q+z-mu2)/(sig2*sqrt(2)))-
  erf((z-mu2)/(sig2*sqrt(2))))+p*sig1*(exp(-(z+Q-mu1)^2/(2*sig1^2))-
  exp(-(z-mu1)^2/(2*sig1^2)))+(sig2-p*sig2)*exp(-(z+Q-mu2)^2/(2*sig2^2))+
  (p*sig2*-sig2)*exp(-(z-mu2)^2/(2*sig2^2))))/Q)},
  lower=0,upper=(100+6*CV*100))$minimum

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Gamma distribution All P2 no double counting To be used in calculating the Silver (1970)
#   modification for P2<= 0.9.

#curve(dlnorm(x,4.585559829,0.1980422),from = 0,to=300)

P2<-0.8
Q<-19
mu<-100
sig<-20
alpha<-mu^2/sig^2
beta<-mu/sig^2
CV<-0.2

EVQGam<-function(z){Q*pgamma(z,alpha,beta)+(z+Q)*
    (pgamma(z+Q,alpha,beta)-pgamma(z,alpha,beta))-(alpha/beta)*(pgamma(z+Q,alpha+1,beta)-
                                                                pgamma(z,alpha+1,beta))}

#curve(EVQGam(x),from = 0,to=mu+6*sig)
#curve(dgamma(x,alpha,beta),from = 0,to=mu+6*sig)
#curve(dnorm(x,mu,sig),from = 0,to=mu+6*sig)

sGam<-optimize(function(z){abs(P2-
                                (Q*pgamma(z,alpha,beta)+(z+Q)*(pgamma(z+Q,alpha,beta)-
                                pgamma(z,alpha,beta))-(alpha/beta)*(pgamma(z+Q,alpha+1,beta)-
                                pgamma(z,alpha+1,beta)))/
                                 Q)},lower=0,upper=(100+6*CV*100))$minimum

#sN2<-optimize(function(z){abs(((Q/sig)*(1-P2))-((dnorm(z)-z*(1-pnorm(z)))-
#                        (dnorm(z+Q/sig)-(z+Q/sig)*(1-pnorm(z+Q/sig)))))},
#              lower = 0,upper = 6)$minimum*sig+mu

