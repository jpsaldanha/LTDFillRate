# Simulated True ROP Using the Silver (1970) modified Expected Shorts Expression
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
SMExpShort<-function(x,n,qt)
{
  modshort<-(1:n)*0
  for (i in 1:(n-1))
  {
    j<-max(which((x[i]+qt-x)>0))
    modshort[i]<-qt-qt*i/n-(sum(x[i]+qt-x[(i+1):j])/n)
  }
  return(modshort)
}
# SMExpShort<-function(x,n,qt)
# {
#   Reduce("rbind",lapply(1:n,function(z)
#     {
#      qt-qt*z/n-sum(x[z]+qt-(x[(z+1):max(which((x[z]+qt-x)>0))]))/n
#     }))
#   # lapply(1:n,function(z)
#   #  {
#   #   qt-qt*z/n-sum(x[z]+qt-(x[(z+1):max(which((x[z]+qt-x)>0))]))/n
#   #  })
# }


#INITIALIZE INPUTS
P2<-0.80 #Fill rate target %%%%%%%%%%%%%%% CHANGE THIS VALUE FOR EACH P2 FILE
U<-cbind(c(1:3),c(0.8,0.85,0.9))

#read in distribution parameter and other experimental inputs
ExpInputs<-read.csv("ExpInputs.csv",header = FALSE)
Qexps<-read.csv("QExps.csv",header = FALSE)
nX<-10^2
D<-22 # Number of distributions
ncores<-22

# OUTPUT
SMTruROPfrp2<-matrix(0,nrow=22,ncol=6)
SMTruSSfrp2<-matrix(0,nrow=22,ncol=6)

# Use a fixed random seed to replicate the experiments
set.seed(931971)

# Variables for the experiments
TruLTD<-matrix(0,nrow = nX,ncol = D)
Q<-matrix(0,nrow = D,ncol = 3)
system.time(
  #Distributional inputs
  for (d in 1:D){ # Cycle through all the distributions in ExpInputs
    
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
      Q[d,]<-c(19,25,38)  # ENTER THE Q FOR THE P2 VALUE
    } else if (d==2|d==5|d==8|d==13|d==18|d==21) {
      Q[d,]<-c(31,42,63) # ENTER THE Q FOR THE P2 VALUE
    } else if (d==3|d==6|d==9|d==14|d==19|d==22) {
      Q[d,]<-c(47,63,95) # ENTER THE Q FOR THE P2 VALUE
    } else if (d==10|d==15) {
      Q[d,]<-c(94,127,190) # ENTER THE Q FOR THE P2 VALUE
    } else {
      Q[d,]<-c(187,253,380) # ENTER THE Q FOR THE P2 VALUE
    }
  }
)

MuX<-apply(TruLTD,2,mean) # Mean of LTD
# Calculate the Silver (1970) modified expected shorts (ES)


# The SMExpShort function executes in 2h21m for each 10^6 LTD or 2d 5h for all 22 dist
system.time(
for (c in U[,1]) {
  P2<-U[c,2]
  smes0<-lapply(1:D,function(z){SMExpShort(TruLTD[,z],n = nX, qt = Q[z,c])})
  SMES<-matrix(0,nrow = nX,ncol = 22)
  for(m in 1:D){for(f in 1:(nX-1)){SMES[f,m]<-smes0[[m]][[f]]}}
    
    #system.time(
    for(b in 1:D) {
    TS<-Q[b,c]*(1-P2) # Calculate the target shorts (TS)
    diff<-TS-SMES[,b] # Take the difference between the target and expected shorts
    
    # 1. The first condition is when all SMES<TS  
      if(length(which(diff<0))==0) # This accomodates the case where SMES[1,b]=TS or diff[1]=0
      {
          # Calculates the index of the second smallest TruLTD sample considering duplicates
          m0<-max(which(TruLTD[,b]==min(TruLTD[,b])))+1
          SMTruROPfrp2[b,c]<-TruLTD[1,b]-(((TS-SMES[1,b])*(TruLTD[m0,b]-TruLTD[1,b]))/
                                    (SMES[1,b]-SMES[m0,b]))
      }
      # 2. The second condition is when resample ES values are such that ES(1)<TS<ES(nX)
      else 
      {
      # The next two values are the indices for the ES vector on either side of the TS
      lo<-max(which(diff==max(diff[diff<0]))) # Smallest ES<TS
      hi<-min(which(diff==min(diff[diff>=0]))) # Smallest ES>=TS
      
      # Calculate the bootstrap ROP and SS
      SMTruROPfrp2[b,c]<-TruLTD[hi,b]-(((TS-SMES[hi,b])*(TruLTD[hi,b]-TruLTD[lo,b]))/
                                     (SMES[lo,b]-SMES[hi,b]))
      SMTruSSfrp2[b,c]<-SMTruROPfrp2[b,c]-MuX[b]
      }
  }
}
#)
)
write.table(SMTruROPfrp2,file = "SMTrueROPfrp2.csv",sep=",",row.names = FALSE,col.names=FALSE)
write.table(SMTruSSfrp2,file = "SMTrueSSfrp2.csv",sep=",",row.names = FALSE,col.names=FALSE)
