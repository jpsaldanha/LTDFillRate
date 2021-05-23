library(truncnorm) # package for generating truncated normal variates
library(stats) #package for generating uniform variates

#Inputs
P2<-0.98 #Fill rate target
B3<-1
P2lvls<-6 #no. of levels of P2
nX<-10^6 #no. of LTD data
nXlvls<-4 #no. of nX levels
R<-100 #no. of experimental replications
Dist<-22 #no. of distributions
TruROP<-read.csv("TruROP.csv",header = FALSE)
ExpInputs<-read.csv("ExpInputs.csv",header = FALSE)
ExpROP<-read.csv("AllStkdRslts.csv",header = TRUE)
U<-cbind(c(1:6),c(0.8,0.85,0.9,0.95,0.98,0.99))
V<-cbind(c(1:4),c(7,12,24,30))

TTC<-matrix(0,nrow = Dist,ncol = P2lvls)
ETC<-matrix(0,nrow = P2lvls*nXlvls*Dist*R*3,ncol = 4)

# Use a fixed random seed to replicate the experiments
set.seed(931971)

#Distributional inputs
for (d in 1:Dist){ # Cycle through all the distributions in ExpInputs
  
  #Uniform 1-3
  if (d<=3){
    TruLTD<-sort(runif(nX,ExpInputs[d,1],ExpInputs[d,2]))}
  
  #Truncated normal 4-6
  if (d>=4 && d<=6){
    TruLTD<-sort(rtruncnorm(nX,0,mean=ExpInputs[d,1],sd=ExpInputs[d,2]))}
  
  #Lognormal distribution draw 7-11
  if (d>=7 && d<=11){
    TruLTD<-sort(rlnorm(nX,ExpInputs[d,1],ExpInputs[d,2]))}
  
  #Two point distribution 12-16
  #if  (d>=12 && d<=16){
  #TruLTD<-rdiscrete(nX,c(ExpInputs[d,1],
  #                                    ExpInputs[d,2]),c(ExpInputs[d,3],ExpInputs[d,4]))}
  
  #Bimodal distribution draw 12-22
  if (d>=12){
    TruLTD<-sort(bimodDistFunc(nX,ExpInputs[d,1],ExpInputs[d,2],ExpInputs[d,3],
                                   ExpInputs[d,4],ExpInputs[d,5]))}
  
  # CALCULATE TRUE TOTAL COST (TTC)
  for (p in U[,1]) {
    P2<-U[p,2]
    r<-B3*(1-P2)/P2
    s0<-TruROP[d,p]-TruLTD
    e0<-TruLTD-TruROP[d,p]
    TTC[d,p]<-sum(s0[which(s0>0)])*r/nX+sum(e0[which(e0>0)])*B3/nX
  }
}

# CALCULATE THE TOTAL COST FOR ALL EXPERIMENTSxR
for(a in 1:3){
  for(l in 1:R){
    for (d in 1:Dist){
      
      #Uniform 1-3
      if (d<=3){
        TruLTD<-sort(runif(nX,ExpInputs[d,1],ExpInputs[d,2]))}
      
      #Truncated normal 4-6
      if (d>=4 && d<=6){
        TruLTD<-sort(rtruncnorm(nX,0,mean=ExpInputs[d,1],sd=ExpInputs[d,2]))}
      
      #Lognormal distribution draw 7-11
      if (d>=7 && d<=11){
        TruLTD<-sort(rlnorm(nX,ExpInputs[d,1],ExpInputs[d,2]))}
      
      #Two point distribution 12-16
      #if  (d>=12 && d<=16){
      #TruLTD<-rdiscrete(nX,c(ExpInputs[d,1],
      #                                    ExpInputs[d,2]),c(ExpInputs[d,3],ExpInputs[d,4]))}
      
      #Bimodal distribution draw 12-22
      if (d>=12){
        TruLTD<-sort(bimodDistFunc(nX,ExpInputs[d,1],ExpInputs[d,2],ExpInputs[d,3],
                                   ExpInputs[d,4],ExpInputs[d,5]))}
      
      for (b in V[,1]) {
        for (c in U[,1]) {
          P2<-U[c,2]
          r<-B3*(1-P2)/P2
          # TRUE COST
          ETC[(a-1)*(R*Dist*P2lvls*nXlvls)+(l-1)*(Dist*P2lvls*nXlvls)+(d-1)*
                       (P2lvls*nXlvls)+(b-1)*P2lvls+c,1]<-TTC[d,c]
          # VECTOR OF SAFETY STOCKS
          s0<-ExpROP[(a-1)*(R*Dist*P2lvls*nXlvls)+(l-1)*(Dist*P2lvls*nXlvls)+(d-1)*
                       (P2lvls*nXlvls)+(b-1)*P2lvls+c,9]-TruLTD
          # VECTOR OF EXPECTED SHORTS
          e0<-TruLTD-ExpROP[(a-1)*(R*Dist*P2lvls*nXlvls)+(l-1)*(Dist*P2lvls*nXlvls)+(d-1)*
                              (P2lvls*nXlvls)+(b-1)*P2lvls+c,9]
          # ESTIMATED SAFETY STOCKS
          ETC[(a-1)*(R*Dist*P2lvls*nXlvls)+(l-1)*(Dist*P2lvls*nXlvls)+(d-1)*(P2lvls*nXlvls)+
                (b-1)*P2lvls+c,2]<-sum(s0[which(s0>0)])/nX
          # ESTIMATED EXPECTED SHORTS
          ETC[(a-1)*(R*Dist*P2lvls*nXlvls)+(l-1)*(Dist*P2lvls*nXlvls)+(d-1)*(P2lvls*nXlvls)+
                (b-1)*P2lvls+c,3]<-sum(e0[which(e0>0)])/nX
          # TOTAL ESTIMATED COSTS
          ETC[(a-1)*(R*Dist*P2lvls*nXlvls)+(l-1)*(Dist*P2lvls*nXlvls)+(d-1)*(P2lvls*nXlvls)+
                (b-1)*P2lvls+c,4]<-sum(s0[which(s0>0)])*r/nX+sum(e0[which(e0>0)])*B3/nX
        }
      }
    }
  }
}

AllCostRslts<-as.data.frame(ETC)
names(AllCostRslts)[1]<-"TruCost"
names(AllCostRslts)[2]<-"SS"
names(AllCostRslts)[3]<-"ES"
names(AllCostRslts)[4]<-"EstCost"

AllStkdCostRslts<-cbind(ExpROP,AllCostRslts)

write.csv(AllStkdCostRslts, 'AllStkdCostRslts.csv')