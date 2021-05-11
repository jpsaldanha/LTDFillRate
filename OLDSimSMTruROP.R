# Simulated True ROP Using teh Silver (1970) modified Expected Shorts Expression
library(truncnorm) # package for generating truncated normal variates
library(stats) #package for generating uniform variates

# Define a function to create a bimodal distribution
bimodDistFunc <- function (sz,modsplt, cpar1, cpar2, vpar1, vpar2) {
  #  y0 <- rgamma(sz,cpar1^2/vpar1^2, cpar1/vpar1^2) # USE WHEN FIRST DIST IS GAMMA
  y0 <- rnorm(sz,cpar1,vpar1) # USE WHEN FIRST DIST IS NORMAL
  y1 <- rnorm(sz,cpar2,vpar2)
  
  pct <- rbinom(sz,size=1,prob=modsplt)
  y <- y0*pct + y1*(1-pct) 
}

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
#Q<-19 #Fixed order quantity
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

# Variables for the experiments
TruLTD<-matrix(0,nrow = nX,ncol = 22)
Q<-rep(0,22)
system.time(
  #Distributional inputs
  for (d in 1:22){ # Cycle through all the distributions in ExpInputs
    
    #Uniform 1-3
    if (d<=3){
      TruLTD[,d]<-sort(runif(nX,ExpInputs[d,1],ExpInputs[d,2]))}
    
    #Truncated normal 4-6
    if (d>=4 && d<=6){
      TruLTD[,d]<-sort(rtruncnorm(nX,0,mean=ExpInputs[d,1],sd=ExpInputs[d,2]))}
    
    #Lognormal distribution draw 7-11
    if (d>=7 && d<=11){
      TruLTD[,d]<-sort(rlnorm(nX,ExpInputs[d,1],ExpInputs[d,2]))}
    
    #Two point distribution 12-16
    #if  (d>=12 && d<=16){
    #TruLTD<-rdiscrete(nX,c(ExpInputs[d,1],
    #                                    ExpInputs[d,2]),c(ExpInputs[d,3],ExpInputs[d,4]))}
    
    #Bimodal distribution draw 12-22
    if (d>=12){
      TruLTD[,d]<-sort(bimodDistFunc(nX,ExpInputs[d,1],ExpInputs[d,2],ExpInputs[d,3],
                                     ExpInputs[d,4],ExpInputs[d,5]))}
    
    # Identifies the CV of the distribution to select correct Q
    if (d==1|d==4|d==7|d==12|d==17|d==20) {
      CV[d]<-1
    } else if (d==2|d==5|d==8|d==13|d==18|d==21) {
      CV[d]<-2
    } else if (d==3|d==6|d==9|d==14|d==19|d==22) {
      CV[d]<-3
    } else if (d==10|d==15) {
      CV[d]<-4
    } else {
      CV[d]<-5
    }
  }
)

system.time(
  MuX<-apply(TruLTD,2,mean)) # Mean of LTD
# Calculate the Silver (1970) modified expected shorts (ES)
system.time(
  # The SMExpShort function executes in 2h21m for each 10^6 LTD or 2d 5h for all 22 dist
  SMES<-apply(TruLTD,2,SMExpShort,n = nX))

for(d in 1:22) {
  for (c in U[,1]) {
    P2<-U[c,2] # The P2 experimental level
    Q<-Qexps[c,CV[d]] # Use the correct Q
    TS<-Q*(1-P2) # Calculate the target shorts (TS)
    diff<-TS-SMES[,d] # Take the difference between the target and expected shorts
    
    # 1. The first condition is when all SMES<TS  
    if(length(which(diff<0))==0) # This accomodates the case where SMES[1,d]=TS or diff[1]=0
    {
      # Calculates the index of the second smallest TruLTD sample considering duplicates
      m0<-max(which(TruLTD[,d]==min(TruLTD[,d])))+1
      SMTruROP[d,c]<-TruLTD[1,d]-(((TS-SMES[1,d])*(TruLTD[m0,d]-TruLTD[1,d]))/
                                (SMES[1,d]-SMES[m0,d]))
    }
    # 2. The second condition is when resample ES values are such that ES(1)<TS<ES(nX)
    else 
    {
    # The next two values are the indices for the ES vector on either side of the TS
    lo<-max(which(diff==max(diff[diff<0]))) # Smallest ES<TS
    hi<-min(which(diff==min(diff[diff>=0]))) # Smallest ES>=TS
    
    # Calculate the bootstrap ROP and SS
    SMTruROP[d,c]<-TruLTD[hi,d]-(((TS-SMES[hi,d])*(TruLTD[hi,d]-TruLTD[lo,d]))/
                                   (SMES[lo,d]-SMES[hi,d]))
    SMTruSS[d,c]<-SMTruROP[d,c]-MuX[d]
    }
  }
}

write.table(SMTruROP,file = "SMTrueROP.csv",sep=",",row.names = FALSE,col.names=FALSE)
write.table(SMTruSS,file = "SMTrueSS.csv",sep=",",row.names = FALSE,col.names=FALSE)
