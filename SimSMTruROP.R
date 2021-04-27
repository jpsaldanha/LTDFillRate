# Simulated True ROP Using teh Silver (1970) modified Expected Shorts Expression
library(truncnorm) # package for generating truncated normal variates
library(stats) #package for generating uniform variates

# Estimate the Silver (1970) modified expected shorts for every bootstrap sample
#   Note that the n-th ES value will always be zero, hence, n-th P2 value is always 1
#   Must only apply the ExpShort function on data sorted in ascending order
SMExpShort<-function(x,n)
{
  modshort<-(1:n)*0
  for (i in 1:(n-1))
  {
    j<-max(which((x[i]+Q-x)>0))
    modshort[i]<-Q-Q*i/n-(sum(x[i]+Q-x[(i+1):j])/n)
  }
  return(modshort)
}

#INITIALIZE INPUTS
P2<-0.80 #Fill rate target
P2lvls<-6 #no. of levels of P2
Q<-19 #Fixed order quantity
Dist<-22 #no. of distributions 
#read in distribution parameter and other experimental inputs
ExpInputs<-read.csv("ExpInputs.csv",header = FALSE)
Qexps<-read.csv("QExps.csv",header = FALSE)
U<-cbind(c(1:6),c(0.8,0.85,0.9,0.95,0.98,0.99))
nX<-10^3

# OUTPUT
SMTruROP<-matrix(0,nrow=22,ncol=6)
SMTruSS<-matrix(0,nrow=22,ncol=6)

# Use a fixed random seed to replicate the experiments
set.seed(931971)

#Distributional inputs
for (d in 1:22){ # Cycle through all the distributions in ExpInputs
  
  #Uniform 1-3
  if (d<=3){
    TruLTD<-runif(nX,ExpInputs[d,1],ExpInputs[d,2])}
  
  #Truncated normal 4-6
  if (d>=4 && d<=6){
    TruLTD<-rtruncnorm(nX,0,mean=ExpInputs[d,1],sd=ExpInputs[d,2])}
  
  #Lognormal distribution draw 7-11
  if (d>=7 && d<=11){
    TruLTD<-rlnorm(nX,ExpInputs[d,1],ExpInputs[d,2])}
  
  #Two point distribution 12-16
  #if  (d>=12 && d<=16){
  #TruLTD<-rdiscrete(nX,c(ExpInputs[d,1],
  #                                    ExpInputs[d,2]),c(ExpInputs[d,3],ExpInputs[d,4]))}
  
  #Bimodal distribution draw 12-22
  if (d>=12){
    TruLTD<-bimodDistFunc(nX,ExpInputs[d,1],ExpInputs[d,2],ExpInputs[d,3],
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
  TruLTD<-sort(TruLTD) # Sorts the true LTD vector
  SMES<-SMExpShort(TruLTD,nX) # Calculate the Silver (1970) modified expected shorts (ES)
  assign(paste("SMES",d,sep = ""),SMES)
  
  for (c in U[,1]) {
    
    P2<-U[c,2] # The P2 experimental level
    Q<-Qexps[c,CV] # Use the correct Q
    TS<-Q*(1-P2)+Qratio # Calculate the target shorts (TS)
    diff<-TS-SMES # Take the difference between the target and expected shorts
    # The next two values are the indices for the ES vector on either side of the TS
    lo<-max(which(diff==max(diff[diff<0]))) # Smallest ES<TS
    hi<-min(which(diff==min(diff[diff>0]))) # Smallest ES>TS
    
    # Calculate the bootstrap ROP and SS
    SMTruROP[d,c]<-TruLTD[hi]-(((TS-SMES[hi])*(TruLTD[hi]-TruLTD[lo]))/
                                (SMES[lo]-SMES[hi]))
    SMTruSS[d,c]<-SMTruROP[d,c]-MuX
    
  }
}

write.table(SMTruROP,file = "SMTrueROP.csv",sep=",",row.names = FALSE,col.names=FALSE)
write.table(SMTruSS,file = "SMTrueSS.csv",sep=",",row.names = FALSE,col.names=FALSE)
