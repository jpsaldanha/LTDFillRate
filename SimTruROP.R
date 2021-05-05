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

# # Define a function to estimate the expected shorts for every bootstrap sample
# ExpShort<-function(x,n)
# {
#   short<-1:n
#   for (i in 1:n)
#   {
#     short[i]<-sum(x[i:n]-x[i])/n
#   }
#   return(short)
# }

# Define a (new lapply) function to estimate the expected shorts for every bootstrap sample
ExpShort<-function(x,n)
{
  mclapply(1:n,
    function(z)
    {
      sum(x[z:n]-x[z])/n
    }
  ,mc.cores = ncores)
}

# Use a fixed random seed to replicate the experiments
set.seed(931971)

#INITIALIZE INPUTS
P2<-0.80 #Fill rate target %%%%%%%%%%%%%%% CHANGE THIS VALUE FOR EACH P2 FILE
U<-cbind(c(1:3),c(0.95,0.98,0.99))

#read in distribution parameter and other experimental inputs
ExpInputs<-read.csv("ExpInputs.csv",header = FALSE)
Qexps<-read.csv("QExps.csv",header = FALSE)
nX<-10^5
D<-22 # Number of distributions
ncores<-16

# Outputs
TruROP<-matrix(0,nrow=22,ncol=6) # Bootstrap ROP
TruSS<-matrix(0,nrow=22,ncol=6) # Bootstrap SS

# Variables for the experiments
TruLTD<-matrix(0,nrow = nX,ncol = 22)
Q<-matrix(0,nrow = D,ncol = 3)
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
      Q[d,]<-c(44,78,94)  # ENTER THE Q FOR THE P2 VALUE
    } else if (d==2|d==5|d==8|d==13|d==18|d==21) {
      Q[d,]<-c(73,130,157) # ENTER THE Q FOR THE P2 VALUE
    } else if (d==3|d==6|d==9|d==14|d==19|d==22) {
      Q[d,]<-c(110,195,235) # ENTER THE Q FOR THE P2 VALUE
    } else if (d==10|d==15) {
      Q[d,]<-c(220,390,470) # ENTER THE Q FOR THE P2 VALUE
    } else {
      Q[d,]<-c(440,780,940) # ENTER THE Q FOR THE P2 VALUE
    }
  }
)
  
# Calculates the shape (alpha) and scale (1/beta) parameters of the gamma distribution
MuX<-apply(TruLTD,2,mean) # Mean of LTD
#  Calculate the expected shorts (ES)
es0<-mclapply(1:D,function(z){ExpShort(TruLTD[,z],n = nX)},mc.cores = ncores)
ES<-matrix(0,nrow = nX,ncol = 22)
for(m in 1:D){for(f in 1:(nX-1)){ES2[f,m]<-es0[[m]][[f]]}}

for(b in 1:D) {
  for (c in U[,1]) {
    P2<-U[c,2] # The P2 experimental level
    TS<-Q[b,c]*(1-P2) # Calculate the target shorts (TS)
    diff<-TS-ES[,b] # Take the difference between the target and expected shorts
    # The next two values are the indices for the ES vector on either side of the TS
    lo<-max(which(diff==max(diff[diff<0]))) # Smallest ES<TS
    hi<-min(which(diff==min(diff[diff>0]))) # Smallest ES>TS
    
    # Calculate the bootstrap ROP and SS
    TruROP[b,c]<-TruLTD[hi]-(((TS-ES[hi,b])*(TruLTD[hi,b]-TruLTD[lo,b]))/
                                     (ES[lo,b]-ES[hi,b]))
    TruSS[b,c]<-TruROP[b,c]-MuX[b]
  }
}

write.table(TruROP,file = "TrueROP.csv",sep=",",row.names = FALSE,col.names=FALSE)
write.table(TruSS,file = "TrueSS.csv",sep=",",row.names = FALSE,col.names=FALSE)
