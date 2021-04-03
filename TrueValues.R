# True ROP and SS for the FR experiments

# Set the working directory
#setwd("/Users/beuser/Documents/Work/Research/Solo Research/Bootstrap Research/Bootstrap Fill Rate/LTDFillRate/") 

# Use a fixed random seed to replicate the experiments
set.seed(931971)

#Inputs
P2<-0.98 #Fill rate target
U<-cbind(c(1:6),c(0.8,0.85,0.9,0.95,0.98,0.99))
ExpInputs<-read.csv("ExpInputs.csv",header = FALSE)

# Outputs
TruROP<-matrix(0,nrow=22,ncol=6)
TruSS<-matrix(0,nrow=22,ncol=6)

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
  
  for (c in U[,1]) {
    P2<-U[c,2] # The P2 experimental level
    TruROP[d,c]<-quantile(TruLTD,P2)
    TruSS[d,c]<-quantile(TruLTD,P2)-mean(TruLTD)
  }
}

#write.table(TruROP,file = "TrueROP.csv",sep=",",row.names = FALSE,col.names=FALSE)
#write.table(TruSS,file = "TrueSS.csv",sep=",",row.names = FALSE,col.names=FALSE)
