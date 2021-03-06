# Calculate the True ROP FOR  0.9 < P_2 <= 0.9 Using Mathematica Generated Analytical Formula
#   File updated April 27, 2021 without the bimodal True ROP expressions
#   For details and other supporting results see the FRBootstrapNotes.RMD markdown file

# The error function
erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1

#INITIALIZE INPUTS
P2<-0.80 #Fill rate target
Q<-19 #Fixed order quantity
Dist<-22 #no. of distributions (NOT INCLUDING BIMODAL)
#read in distribution parameter and other experimental inputs
ExpInputs<-read.csv("ExpInputs.csv",header = FALSE)
Qexps<-read.csv("QExps.csv",header = FALSE)
U<-cbind(c(1:6),c(0.8,0.85,0.9,0.95,0.98,0.99))

# OUTPUT
TruROP<-matrix(0,nrow=Dist,ncol=6) # TRUE ROP TRADITIONAL EXP SHORTS FUNCTION
SMTruROP<-matrix(0,nrow=Dist,ncol=6) # TRUE ROP Silver (1970) MODIFIED FUNCTION

#Distributional inputs
for (d in 1:Dist){ # Cycle through the distributions in ExpInputs (NOT INCLUDING BIMODAL)

  # Identifies the CV of the distribution to select correct Q
  if (d==1|d==4|d==7|d==12|d==17|d==20) {
    CV<-1
    cv<-1/5
  } else if (d==2|d==5|d==8|d==13|d==18|d==21) {
    CV<-2
    cv<-1/3
  } else if (d==3|d==6|d==9|d==14|d==19|d==22) {
    CV<-3
    cv<-1/2
  } else if (d==10|d==15) {
    CV<-4
    cv<-1
  } else {
    CV<-5
    cv<-2
  }
  
  for (c in U[,1]) {
    
    P2<-U[c,2] # The P2 experimental level
    Q<-Qexps[c,CV] # Use the correct Q
  
  if (d<=3){
    # UNIFORM DISTRIBUTION
    a<-ExpInputs[d,1]
    b<-ExpInputs[d,2]
    # Case 1: P2 > 0.9
    
    TruROP[d,c]<-b-sqrt(2*Q*(1-P2)*(b-a))
    
    # Case 2: P2 <= 0.9
    
    SMTruROP[d,c]<-P2*(b-a)+a-(Q/2)
    }
    
    if (d>=4 && d<=6){
    # TRUNCATED NORMAL DISTRIBUTION
    mu<-ExpInputs[d,1]
    sig<-ExpInputs[d,2]
    # Case 1: P2 > 0.9
    
    TruROP[d,c]<-optimize(function(z){abs(Q*(1-P2)-(((mu-z)*sqrt(pi/2)+sig*exp(-(1/2)*((z-mu)/
                  sig)^2)+sig*sqrt(pi/2)*((z-mu)/sig)*erf((z-mu)/(sig*sqrt(2))))/
                  (sqrt(2*pi)*(1-0.5*(1+erf(-mu/
                  (sig*sqrt(2))))))))},lower=0,upper=(mu+6*sig))$minimum
    
    # Case 2: P2 <= 0.9
    
    SMTruROP[d,c]<-optimize(function(z){abs(P2-
          ((1/(2*sqrt(pi)*(1-(1/2)*(1+erf(-mu/(sig*sqrt(2)))))))*
          (sig*sqrt(2)*(exp(-(Q+z-mu)^2/(2*sig^2))-exp(-(z-mu)^2/(2*sig^2)))+
          sqrt(pi)*((mu-z)*erf((z-mu)/(sig*sqrt(2)))+
          (Q+z-mu)*erf((Q+z-mu)/(sig*sqrt(2)))+Q*erf(mu/(sig*sqrt(2))))))/Q)},
          lower=0,upper=(mu+6*sig))$minimum
    }
    
    if (d>=7 && d<=11){
    # LOGNORMAL DISTRIBUTION
    mu<-ExpInputs[d,1]
    sig<-ExpInputs[d,2]
    # Case 1: > 0.9
    
    TruROP[d,c]<-optimize(function(z){abs(abs(Q*(1-P2))-(1/2*(exp(mu+sig^2/2)-z-z*
          erf((mu-log(z))/(sig*sqrt(2)))+exp(mu+sig^2/2)*erf((mu+sig^2-log(z))/
          (sig*sqrt(2))))))},lower=0,upper=(100+6*cv*100))$minimum
    
    # Case 2: P2 <= 0.9
    
    SMTruROP[d,c]<-optimize(function(z){abs(P2-
        (Q/2*(1-erf((mu-log(z))/(sig*sqrt(2))))+(z+Q)/2*(erf((mu-log(z))/
        (sig*sqrt(2)))-erf((mu-log(z+Q))/(sig*sqrt(2))))-exp(mu+sig^2/2)/2*
        (erf((mu+sig^2-log(z))/(sig*sqrt(2)))-erf((mu+sig^2-log(z+Q))/(sig*sqrt(2)))))/Q)},
        lower=0,upper=(100+6*cv*100))$minimum
    }

    # Bimodal Distribution
    if (d>=12){
    p<-ExpInputs[d,1]
    q<-(1-p)
    mu1<-ExpInputs[d,2]
    sig1<-ExpInputs[d,3]
    mu2<-ExpInputs[d,4]
    sig2<-ExpInputs[d,5]
    # Case 1: P2 > 0.9
    
    TruROP[d,c]<-optimize(function(z)
    {abs(abs(Q*(1-P2))-(p*z/2*(erf((z-mu1)/(sig1*sqrt(2)))-1)-mu1*p/2*(erf((z-mu1)/
    (sig1*sqrt(2)))-1)+p/sqrt(2*pi)*sig1*exp(-(z-mu1)^2/(2*sig1^2))+q*z/2*(erf((z-mu2)/
    (sig2*sqrt(2)))-1)-mu2*q/2*(erf((z-mu2)/(sig2*sqrt(2)))-1)+q/sqrt(2*pi)*sig2*
    exp(-(z-mu2)^2/(2*sig2^2))))},lower=0,upper=(100+6*CV*100))$minimum
    # 
    # 
    # # Case 2: P2 <= 0.9
    # 
    # P2<-0.8
    # Q<-19
    # mu1<-85.9
    # sig1<-8.6
    # mu2<-121.1
    # sig2<-12.1
    # p<-0.6
    # q<-0.4 # q = 1-p
    # CV<-0.2
    # 
    # BMVQ<-function(z)
    # {1/sqrt(2*pi)*((z+Q)*sqrt(pi/2)*(p*erf((z+Q-mu1)/(sig1*sqrt(2)))-
    #  p*erf((z-mu1)/(sig1*sqrt(2)))+(p-1)*(erf((z-mu2)/(sig2*sqrt(2)))-erf((Q+z-mu2)/
    #                                                                                                           (sig2*sqrt(2)))))+Q*sqrt(pi/2)*(p*erf((z-mu1)/(sig1*sqrt(2)))+p*erf(mu1/(sig1*sqrt(2)))-
    #                                                                                                                                             (p-1)*(erf((z-mu2)/(sig2*sqrt(2)))+erf(mu2/(sig2*sqrt(2)))))+mu1*p*sqrt(pi/2)*
    #                  (erf((z-mu1)/(sig1*sqrt(2)))-erf((z+Q-mu1)/(sig1*sqrt(2))))+mu2*(p-1)*sqrt(pi/2)*
    #                  (erf((Q+z-mu2)/(sig2*sqrt(2)))-erf((z-mu2)/(sig2*sqrt(2))))+
    #                  p*sig1*(exp(-(z+Q-mu1)^2/(2*sig1^2))-exp(-(z-mu1)^2/(2*sig1^2)))+
    #                  (sig2-p*sig2)*exp(-(z+Q-mu2)^2/(2*sig2^2))+(p*sig2*-sig2)*exp(-(z-mu2)^2/(2*sig2^2)))}
    # 
    # curve(BMVQ(x),from=0, to=100+6*100*CV)
    # 
    # sBM2<-optimize(function(z)
    # {abs(P2-(1/sqrt(2*pi)*((z+Q)*sqrt(pi/2)*(p*erf((z+Q-mu1)/(sig1*sqrt(2)))-
    #                                            p*erf((z-mu1)/(sig1*sqrt(2)))+(p-1)*(erf((z-mu2)/(sig2*sqrt(2)))-erf((Q+z-mu2)/
    #                                                                                                                   (sig2*sqrt(2)))))+Q*sqrt(pi/2)*(p*erf((z-mu1)/(sig1*sqrt(2)))+p*erf(mu1/(sig1*sqrt(2)))-
    #                                                                                                                                                     (p-1)*(erf((z-mu2)/(sig2*sqrt(2)))+erf(mu2/(sig2*sqrt(2)))))+mu1*p*sqrt(pi/2)*
    #                          (erf((z-mu1)/(sig1*sqrt(2)))-erf((z+Q-mu1)/(sig1*sqrt(2))))+mu2*(p-1)*sqrt(pi/2)*
    #                          (erf((Q+z-mu2)/(sig2*sqrt(2)))-erf((z-mu2)/(sig2*sqrt(2))))+
    #                          p*sig1*(exp(-(z+Q-mu1)^2/(2*sig1^2))-exp(-(z-mu1)^2/(2*sig1^2)))+
    #                          (sig2-p*sig2)*exp(-(z+Q-mu2)^2/(2*sig2^2))+(p*sig2*-sig2)*exp(-(z-mu2)^2/(2*sig2^2))))/Q)},
    # lower=0,upper=(100+6*CV*100))$minimum
    }
  }  
}

# OUTPUT THE TRUE CALCULATED ROP
write.table(TruROP,file = "TrueCalcROP.csv",sep=",",row.names = FALSE,col.names=FALSE)
write.table(SMTruROP,file = "SMTrueCalcROP.csv",sep=",",row.names = FALSE,col.names=FALSE)
