# Run the bootstrap Fill Rate Experiments

# Define a function to create a bimodal distribution
bimodDistFunc <- function (sz,modsplt, cpar1, cpar2, vpar1, vpar2) {
#  y0 <- rgamma(sz,cpar1^2/vpar1^2, cpar1/vpar1^2) # USE WHEN FIRST DIST IS GAMMA
  y0 <- rnorm(sz,cpar1,vpar1) # USE WHEN FIRST DIST IS NORMAL
  y1 <- rnorm(sz,cpar2,vpar2)
  
  pct <- rbinom(sz,size=1,prob=modsplt)
  y <- y0*pct + y1*(1-pct) 
}

######   BOOTSTRAP ALGORITHM FOR FILL RATE WITH FIXED Q   ######
FRBoot<-function(tildeX){
  
# Define a function to estimate the expected shorts for every bootstrap sample
#   Note that the n-th ES value will always be zero, hence, n-th P2 value is always 1
ExpShort<-function(x,n)
{
  short<-1:n
  for (i in 1:n)
  {
    #short[i]<-((sum(x[i:n])-x[i]*(n-i+1))/n)*sd(x)
    short[i]<-(sum(x[i:n]-x[i])/n)*sd(x)
  }
  return(short)
}

#B Bootstrap sorted resamples of LTD data in tilde X
BtSmplX<-replicate(B,sort(sample(tildeX,size=nX,replace = TRUE)))

#Use the ExpShort() function to calc exp. shorts for all B resamples (in cols)
esMat<-apply(BtSmplX,2,ExpShort,n=nX)

#calculate the P2 for all B resamples of LTD from tildeX
P2lst<-1-(esMat/Q)

#calculate the diff of each P2 from target fill rate for all B resamples
# we take the difference of the target P2 from P2lst so that 
# values <target P2 are negative and values >target P2 are positive
# we create two tables one to find the nearest neg value and one to find nearest pos value
P2NegDifflst<-P2lst-P2
P2PosDifflst<-P2lst-P2

# This table is used to find the minimum P2 value where resample b's P2 > target P2
P2Pos<-1-(esMat/Q)
P2Pos[P2Pos<0]<-99
P2PosMin<-1:B
for (q in 1:B) {
  P2PosMin[q]<-max(which(P2Pos[,q]==min(P2Pos[,q])))
}

# Prepare each table to find the position of the P2 value closest to the P2 target
# Convert all positive values to a v.small neg value to find the largest (neg) number
P2NegDifflst[P2NegDifflst>0]<--99
# Convert all negative values to a v.large pos value to find the smallest (pos) number
P2PosDifflst[P2PosDifflst<0]<-99


#Finds the position of the P2 value closest to the P2 target in each b resample
# Finds the largest negative value (closest to tgt P2) even with duplicates
NearNeg<-1:B
for (t in 1:B) {
  NearNeg[t]<-max(which(P2NegDifflst[,t]==max(P2NegDifflst[,t])))
}
#Remove this next line once confirmed the above code works
#NearNeg<-apply(P2NegDifflst,2,which.max)
NearPos<-apply(P2PosDifflst,2,which.min) # Finds the minimum positive value (first in the vector)

#  uses linear interpolation to compute the ROP from the tildeX with P2 closest to target P2
#  returns a B length vector of the ROP estimate s_hat for each resample
s_hat<-1:B # vector to store the estimates ROP for each bootstrap resample
for (k in 1:B) #iterate over all B resamples
{
  #Find the ROP for each bootstrap resample
  # If all LTD data in the nX sample are identical return the LTD value
  if (sd(BtSmplX[,k])==0){
    s_hat[k]<-BtSmplX[1,k]
  }
  #   Note that ALL b resamples cannot have P2 < target P2 as the nX-th value is always ==1.0
  # If the smallest P2 value in resample b are greater than the target P2 value
  else if (P2Pos[P2PosMin[k],k]>P2){
    # If from resample b a P2 == target P2
    if (P2PosMin[k]>1 && P2Pos[P2PosMin[k]-1,k]==0){
      s_hat[k]<-BtSmplX[P2PosMin[k]-1,k]
    }
    # If from resample b there are duplicates of the smallest P2 > target P2
    else if (length(which(P2Pos[,k]==min(P2Pos[,k])))>1)
    {
      dupl<-length(which(P2Pos[,k]==min(P2Pos[,k])))-1
      # If duplicates of smallest P2 > target P2 such that one occupies 1st place in vector
      if (P2PosMin[k]==length(which(P2Pos[,k]==min(P2Pos[,k]))))
      {
        s_hat[k]<-BtSmplX[1,k]-(((P2lst[1,k]-P2)*(BtSmplX[P2PosMin[k]+1,k]-
                  BtSmplX[P2PosMin[k],k]))/(P2lst[P2PosMin[k]+1,k]-P2lst[P2PosMin[k],k]))
      }
      # All other duplicates where one does not occupy the 1st place in the vector
      else 
      {
#        if (P2PosMin[k]-dupl-1<1 | P2PosMin[k]-dupl-1>nX) browser() # *** USED FOR DEBUGGIN-REMOVE 
        s_hat[k]<-BtSmplX[P2PosMin[k],k]-(((BtSmplX[P2PosMin[k],k]-BtSmplX[P2PosMin[k]-dupl-1,k])*
                  ((P2Pos[P2PosMin[k],k]-P2)*Q))/(esMat[P2PosMin[k]-dupl-1,k]-esMat[P2PosMin[k],k]))
      }
    }
    # If from resample b the smallest P2 > target P2
    else if (length(which(P2Pos[,k]==min(P2Pos[,k])))==1 && P2PosMin[k]>1) {
#      if (P2PosMin[k]-1<1 | P2PosMin[k]-1>nX) browser() # *** USED FOR DEBUGGIN-REMOVE 
      s_hat[k]<-BtSmplX[P2PosMin[k],k]-(((BtSmplX[P2PosMin[k],k]-BtSmplX[P2PosMin[k]-1,k])*
                ((P2Pos[P2PosMin[k],k]-P2)*Q))/(esMat[P2PosMin[k]-1,k]-esMat[P2PosMin[k],k]))
    }
    # If from resample b the smallest P2 > target P2 and is in the 1st position of LTD vector
    else {
      s_hat[k]<-BtSmplX[1,k]-(((P2lst[1,k]-P2)*(BtSmplX[2,k]-BtSmplX[1,k]))/
                                (P2lst[2,k]-P2lst[1,k]))
    }
  }
  # For all other occassions where ROP is interpolated from tgt P2 values within the vector 
  else{
    s_hat[k]<-(((P2-P2lst[NearNeg[k],k])*(BtSmplX[NearPos[k],k]-BtSmplX[NearNeg[k],k]))/
                 (P2lst[NearPos[k],k]-P2lst[NearNeg[k],k]))+BtSmplX[NearNeg[k],k]
  }
  if (is.nan(s_hat[k])) browser() # used for tracing the origin of the NaN values
}

# Calculate the mean ROP of all the bootstrap resamples' ROP
s_boot<-mean(s_hat) 
}
######        END BOOTSTRAP ALGORITHM       ######


#<<<<<<<<<<<<<<<<<   START EXPERIMENTS  >>>>>>>>>>>>>>>>>>
library(truncnorm) # package for generating truncated normal variates
#library(e1071) #package for generating two-point variates
library(stats) #package for generating uniform variates

# Set the working directory
#setwd("/Users/beuser/Documents/Work/Research/Solo Research/Bootstrap Research/Bootstrap Fill Rate/LTDFillRate/") 

# Use a fixed random seed to replicate the experiments
set.seed(931971)

#Inputs
P2<-0.98 #Fill rate target
P2lvls<-6 #no. of levels of P2
Q<-100 #Fixed order quantity
nX<-7 #no. of LTD data
nXlvls<-4 #no. of nX levels
B<-50 #no. of bootstrap resamples
R<-10 #no. of experimental replications
Dist<-22 #no. of distributions

#read in distribution parameter and other experimental inputs
ExpInputs<-read.csv("ExpInputs.csv",header = FALSE)
Qexps<-read.csv("QExps.csv",header = FALSE)
TruROP<-read.csv("TrueROP.csv",header = FALSE)


#Function to generate a vector of the std normal loss function for k=0.00-4.00
#nlsfun<-vapply(seq(0,4,by=0.0001), function(k){dnorm(k)-k*(1-pnorm(k))},numeric(1))
#This has been repalced by an aproximation to find the root of the function using optimize


#Setup the results matrices for 22 distributions * 6 P2 values * 4 nX values (rows) 
# and R replication (columns)
sboot<-matrix(0,(Dist*P2lvls*nXlvls),R)
sNorm<-matrix(0,(Dist*P2lvls*nXlvls),R)
sGamm<-matrix(0,(Dist*P2lvls*nXlvls),R)
labels<-matrix(0,(Dist*P2lvls*nXlvls),6)
resultN<-1:R # temporary list used to estimate normal ROP
resultG<-1:R # temporary list used to estimate gamma ROP

# Matrices to loop through all experiments.
U<-cbind(c(1:6),c(0.8,0.85,0.9,0.95,0.98,0.99))
V<-cbind(c(1:4),c(7,12,24,30))

#Distributional inputs
for (d in 1:22){ # Cycle through all the distributions in ExpInputs
  for (b in V[,1]) {
    nX<-V[b,2]
    
    #Uniform 1-3
    if (d<=3){
    LTDsmpl<-replicate(R,runif(nX,ExpInputs[d,1],ExpInputs[d,2]))}
    
    #Truncated normal 4-6
    if (d>=4 && d<=6){
    LTDsmpl<-replicate(R,rtruncnorm(nX,0,mean=ExpInputs[d,1],sd=ExpInputs[d,2]))}
    
    #Lognormal distribution draw 7-11
    if (d>=7 && d<=11){
    LTDsmpl<-replicate(R,rlnorm(nX,ExpInputs[d,1],ExpInputs[d,2]))}
    
    #Two point distribution 12-16
    #if  (d>=12 && d<=16){
    #LTDsmpl<-replicate(R,rdiscrete(nX,c(ExpInputs[d,1],
    #                                    ExpInputs[d,2]),c(ExpInputs[d,3],ExpInputs[d,4])))}
    
    #Bimodal distribution draw 12-22
    if (d>=12){
    LTDsmpl<-replicate(R,bimodDistFunc(nX,ExpInputs[d,1],ExpInputs[d,2],ExpInputs[d,3],
                                       ExpInputs[d,4],ExpInputs[d,5]))}
    
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
    
    # Collects a record of the LTD samples
    assign(paste("X",d,nX,sep = "-"),LTDsmpl)
    
    # Calculates LTD sample Mu_X, Sigma_X & Q/Sigma_X ratio to estimate normal approach ROP
    MuX<-apply(LTDsmpl,2,mean)
    SigmaX<-apply(LTDsmpl,2,sd)
    Qratio<-Q/SigmaX
    # Calcukates the shape (alpha) and scale (1/beta) parameters of the gamma distribution
    Gshape<-MuX^2/SigmaX^2 # the alpha parameter of the gamma distribution
    Gscale<-MuX/SigmaX^2 # the 1/beta paramter of the gamma distribution
    
    for (c in U[,1]) {
        P2<-U[c,2] # The P2 experimental level
        
        # Use the correct Q
        Q<-Qexps[c,CV]
        
        # Returns the Bootstrap ROP
        sboot[(d-1)*(P2lvls*nXlvls)+(b-1)*P2lvls+c,]<-t(apply(LTDsmpl,2,FRBoot)) 
        
        # Returns the normal ROP
        # *******  In the small chance all x are identical Qratio (Q/SigmaX) is undefined *****
        normloss<-vapply(Qratio,function(x){x*(1-P2)},numeric(1))
        # The Silver (1970) adjustment G(k) - G(k+Q/Sigma_X) = Q/Sigma_X*(1-P_2) to find k
          #Then used in ROP = k*SigmaX + MuX
        for (l in 1:R) {
          # The optimize function finds the "z" value that 
          resultN[l]<-optimize(function(z){abs(((dnorm(z)-z*(1-pnorm(z)))-(dnorm(z+Qratio[l])-
                   (z+Qratio[l])*(1-pnorm(z+Qratio[l]))))-
                     normloss[l])},lower=0,upper = 6)$minimum*SigmaX[l]+MuX[l]
          if (resultN[l]<1)browser()
        }
        sNorm[(d-1)*(P2lvls*nXlvls)+(b-1)*P2lvls+c,]<-t(resultN)
        
        # Returns the gamma ROP
        gESC<-Q*(1-P2) # Calculates the ESC
        for (g in 1:R) {
          resultG[g]<-optimize(function(s){abs((Gshape[g]/Gscale[g]*(1-pgamma(s,Gshape[g]+1,
                      Gscale[g]))-s*(1-pgamma(s,Gshape[g],Gscale[g])))-gESC)},
                      lower=0,upper=qgamma(0.99,Gshape[g],Gscale[g]))$minimum
#          if (resultG[g]<1)browser()
        }
        sGamm[(d-1)*(P2lvls*nXlvls)+(b-1)*P2lvls+c,]<-t(resultG)
        
        # Returns the experimental factor levels to output dataframe
        labels[(d-1)*(P2lvls*nXlvls)+(b-1)*P2lvls+c,1]=d # distribution level 
        labels[(d-1)*(P2lvls*nXlvls)+(b-1)*P2lvls+c,2]=nX # nX level
        labels[(d-1)*(P2lvls*nXlvls)+(b-1)*P2lvls+c,3]=U[c,2] # P2 level
        labels[(d-1)*(P2lvls*nXlvls)+(b-1)*P2lvls+c,4]=Q # The level of quantity
        labels[(d-1)*(P2lvls*nXlvls)+(b-1)*P2lvls+c,5]=TruROP[d,c] # The level of quantity
      }
  }
}

# Creates bootstrap output dataframe with named columns
labels[,6]<-"boot"
btROPrslts<-as.data.frame(cbind(labels,sboot))

# Creates normal output dataframe with named columns
labels[,6]<-"normal"
normROPrslts<-as.data.frame(cbind(labels,sNorm))

# Creates gamma output dataframe with named columns
labels[,6]<-"gamma"
gamROPrslts<-as.data.frame(cbind(labels,sGamm))

AllResults<-as.data.frame(rbind(btROPrslts,normROPrslts,gamROPrslts))
names(AllResults)[1]<-"DistNo"
names(AllResults)[2]<-"nX"
names(AllResults)[3]<-"P2"
names(AllResults)[4]<-"Q"
names(AllResults)[5]<-"TruROP"
names(AllResults)[6]<-"Method"
for (i in 1:R) {
  names(AllResults)[i+6]<-i
}

# Compute the means of all repitions
reps<-rbind(sboot,sNorm,sGamm)
repsmeans<-rowMeans(reps)
stkdlbls<-rbind(labels,labels,labels)
stkdlbls[1:528,6]<-"boot"
stkdlbls[529:1056,6]<-"normal"
stkdlbls[1057:1584,6]<-"gamma"
AllResultsMeans<-as.data.frame(cbind(stkdlbls,repsmeans))
names(AllResultsMeans)[1]<-"DistNo."
names(AllResultsMeans)[2]<-"nX"
names(AllResultsMeans)[3]<-"P2"
names(AllResultsMeans)[4]<-"Q"
names(AllResultsMeans)[5]<-"TruROP"
names(AllResultsMeans)[6]<-"Method"
names(AllResultsMeans)[7]<-"Mean"

# Return results in stacked format
library(reshape)
AllStkdRslts <- reshape(AllResults, 
             varying = c(7:106), 
             v.names = "ROP",
             timevar = "Reps", 
             times = c(1:100), 
             new.row.names = 1:158400,
             direction = "long")

# If the data need to be sorted
# new<-AllStkdRslts[order(AllStkdRslts$`Dist No.`,AllStkdRslts$P2,AllStkdRslts$nX),]

# Drop the id column
AllStkdRslts<-AllStkdRslts[-c(9)]

write.csv(AllStkdRslts, 'AllStkdRslts.csv')
write.csv(AllResultsMeans,'AllResultsMeans.csv')
