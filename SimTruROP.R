# True ROP and SS for the FR experiments

#setwd("/Users/beuser/Documents/Work/Research/Solo Research/Bootstrap Research/Bootstrap Fill Rate/LTDFillRate/") 

library(truncnorm) # package for generating truncated normal variates

# Define a function to create a bimodal distribution
bimodDistFunc <- function (sz,modsplt, cpar1, cpar2, vpar1, vpar2) {
  #  y0 <- rgamma(sz,cpar1^2/vpar1^2, cpar1/vpar1^2) # USE WHEN FIRST DIST IS GAMMA
  y0 <- rnorm(sz,cpar1,vpar1) # USE WHEN FIRST DIST IS NORMAL
  y1 <- rnorm(sz,cpar2,vpar2)
  
  pct <- rbinom(sz,size=1,prob=modsplt)
  y <- y0*pct + y1*(1-pct) 
}

# Define a function to estimate the expected shorts for every bootstrap sample
ExpShort<-function(x,n)
{
  short<-1:n
  for (i in 1:n)
  {
    short[i]<-sum(x[i:n]-x[i])/n
  }
  return(short)
}

# Use a fixed random seed to replicate the experiments
set.seed(931971)

#Inputs
P2<-0.98 #Fill rate target
U<-cbind(c(1:6),c(0.8,0.85,0.9,0.95,0.98,0.99))
ExpInputs<-read.csv("ExpInputs.csv",header = FALSE)
Qexps<-read.csv("QExps.csv",header = FALSE)

# Outputs
BTruROP<-matrix(0,nrow=22,ncol=6) # Bootstrap ROP
BTruSS<-matrix(0,nrow=22,ncol=6) # Bootstrap SS
NTruROP<-matrix(0,nrow=22,ncol=6) # Normal ROP
NTruSS<-matrix(0,nrow=22,ncol=6) # Normal SS
GTruROP<-matrix(0,nrow=22,ncol=6) # Gamma ROP
GTruSS<-matrix(0,nrow=22,ncol=6) # Gamma SS

#Distributional inputs
for (d in 1:22){ # Cycle through all the distributions in ExpInputs

    #Uniform 1-3
    if (d<=3){
      TruLTD<-runif(10^6,ExpInputs[d,1],ExpInputs[d,2])}
    
    #Truncated normal 4-6
    if (d>=4 && d<=6){
      TruLTD<-rtruncnorm(10^6,0,mean=ExpInputs[d,1],sd=ExpInputs[d,2])}
    
    #Lognormal distribution draw 7-11
    if (d>=7 && d<=11){
      TruLTD<-rlnorm(10^6,ExpInputs[d,1],ExpInputs[d,2])}
    
    #Two point distribution 12-16
    #if  (d>=12 && d<=16){
    #TruLTD<-rdiscrete(10^6,c(ExpInputs[d,1],
    #                                    ExpInputs[d,2]),c(ExpInputs[d,3],ExpInputs[d,4]))}
    
    #Bimodal distribution draw 12-22
    if (d>=12){
      TruLTD<-bimodDistFunc(10^6,ExpInputs[d,1],ExpInputs[d,2],ExpInputs[d,3],
                                         ExpInputs[d,4],ExpInputs[d,5])}
  # Collects a record of the True LTD samples
  assign(paste("TruX",d,sep = ""),TruLTD)
  
  # Identifies the CV of the distribution to select correct Q
  if (d==1|d==4|d==7|d==12|d==17|d==20) {
    CV<-1
  } else if (d==2|d==5|d==8|d==13|d==18|d==21) {
    CV<-2
  } else if (d==3|d==6|d==9|d==14|d==19|d==22) {
    CV<-3
  } else if (d==10|d==15) {
    CV<-4
  } else {
    CV<-5
  }
  
  # Calculates the shape (alpha) and scale (1/beta) parameters of the gamma distribution
  MuX<-mean(TruLTD) # Mean of LTD
#  SigmaX<-sd(TruLTD) # SD of LTD
#  Gshape<-MuX^2/SigmaX^2 # the alpha parameter of the gamma distribution
#  Gscale<-MuX/SigmaX^2 # the 1/beta paramter of the gamma distribution
  TruLTD<-sort(TruLTD) # Sorts the true LTD vector
  ES<-ExpShort(TruLTD,10^6) # Calculate the expected shorts (ES)
  assign(paste("ES",d,sep = ""),ES)
  
  for (c in U[,1]) {
    
# IGNORE NORMAL & GAMMA, they don't give true estimates for the distributions in our test bed
    
    P2<-U[c,2] # The P2 experimental level
    Q<-Qexps[c,CV] # Use the correct Q
#    Qratio<-Q/SigmaX # Calculates the Q/sigma for the normal ROP and SS calculation
    TS<-Q*(1-P2)+Qratio # Calculate the target shorts (TS)
    diff<-TS-ES # Take the difference between the target and expected shorts
    # The next two values are the indices for the ES vector on either side of the TS
    lo<-max(which(diff==max(diff[diff<0]))) # Smallest ES<TS
    hi<-min(which(diff==min(diff[diff>0]))) # Smallest ES>TS
    
    # Calculate the bootstrap ROP and SS
    BTruROP[d,c]<-TruLTD[hi]-(((TS-ES[hi])*(TruLTD[hi]-TruLTD[lo]))/
                                     (ES[lo]-ES[hi]))
    BTruSS[d,c]<-BTruROP[d,c]-MuX
    
    # Calculate the normal ROP and SS
#   normloss<-Qratio*(1-P2) # Calculate the value of the normal loss function
    # The Silver (1970) adjustment G(k) - G(k+Q/Sigma_X) = Q/Sigma_X*(1-P_2) to find k
    #Then used in ROP = k*SigmaX + MuX
    # The optimize function finds the "z" value that minimizes (zeroes) the function finds the closest value to true "z"
#    NTruSS[d,c]<-optimize(function(z){abs(((dnorm(z)-z*(1-pnorm(z)))-(dnorm(z+Qratio)-
#                                  (z+Qratio)*(1-pnorm(z+Qratio))))-
#                                  normloss)},lower=0,upper = 6)$minimum*SigmaX
#    if (NTruROP[d,c]<1)browser() # **** FOR DEBUGGING
#    NTruROP[d,c]<-NTruSS[d,c]+MuX # Calculate the ROP
    
    # Calculate the gamma ROP and SS
#    GTruROP[d,c]<-optimize(function(s){abs((Gshape/Gscale*(1-pgamma(s,Gshape+1,
#                         Gscale))-s*(1-pgamma(s,Gshape,Gscale)))-TS)},
#                         lower=0,upper=qgamma(0.99,Gshape,Gscale))$minimum
#    GTruSS[d,c]<-GTruROP[d,c]-MuX # Calculate the SS
  }
}

write.table(TruROP,file = "TrueROP.csv",sep=",",row.names = FALSE,col.names=FALSE)
write.table(TruSS,file = "TrueSS.csv",sep=",",row.names = FALSE,col.names=FALSE)
# write.table(NTruROP,file = "NTrueROP.csv",sep=",",row.names = FALSE,col.names=FALSE)
# write.table(NTruSS,file = "NTrueSS.csv",sep=",",row.names = FALSE,col.names=FALSE)
# write.table(GTruROP,file = "GTrueROP.csv",sep=",",row.names = FALSE,col.names=FALSE)
# write.table(GTruSS,file = "GTrueSS.csv",sep=",",row.names = FALSE,col.names=FALSE)
