# Run the bootstrap Fill Rate Experiments

# Define a function to create a bimodal distribution
bimodDistFunc <- function (sz,modsplt, cpar1, cpar2, vpar1, vpar2) {
#  y0 <- rgamma(sz,cpar1^2/vpar1^2, cpar1/vpar1^2) # USE WHEN FIRST DIST IS GAMMA
  y0 <- rnorm(sz,cpar1,vpar1) # USE WHEN FIRST DIST IS NORMAL
  y1 <- rnorm(sz,cpar2,vpar2)
  
  pct <- rbinom(sz,size=1,prob=modsplt)
  y <- y0*pct + y1*(1-pct) 
}

######   BOOTSTRAP ALGORITHM FOR FILL RATE WITH FIXED Q P_2 > 0.9  ######
FRBoot<-function(tildeX){
  
# Define a function to estimate the expected shorts for every bootstrap sample
#   Note that the n-th ES value will always be zero, hence, n-th P2 value is always 1
#   Must only apply the ExpShort function on data sorted in ascending order
ExpShort<-function(x,n)
{
  short<-1:n
  for (i in 1:n)
  {
    short[i]<-(sum(x[i:n]-x[i])/n)
  }
  return(short)
}

#B Bootstrap sorted resamples of LTD data in tilde X
BtSmplX<-replicate(B,sort(sample(tildeX,size=nX,replace = TRUE)))

#Use the ExpShort() function to calc exp. shorts for all B resamples (in cols)
esMat<-apply(BtSmplX,2,ExpShort,n=nX)

# Calculate the target shorts (TS)
TS<-Q*(1-P2)

# Calculate this difference before the for loop to calculate the s_hat vector
diff<-TS-esMat

# Function to find the largest negative value in a resample
maxneg<-function(vec)
{
  # Test that at least one ES > TS
  if(length(which(vec<0))>0){
    out<-max(which(vec==max(vec[vec<0])))
  }
  # If all ES < TS then flag for debugging
  else {out<-99}
  return(out)
}

# Function to find the smallest positive value in a resample
minpos<-function(vect)
{
  out<-min(which(vect==min(vect[vect>0])))
  return(out)
}

# Vector of resample indices with smallest ES>TS
lo<-apply(diff,2,maxneg)
# Vector of resample indices with smallest ES<TS
hi<-apply(diff,2,minpos)

#  returns a B length vector of the ROP estimate s_hat for each resample
s_hat<-1:B # vector to store the estimates ROP for each bootstrap resample

for (k in 1:B) # Iterate over all B resamples
{
  # 1. The first condition is if at least one of the resample values ES = TS
  if(length(which(diff[,k]==0))>0)
  {
    if (which(diff[, k] == 0)) browser()
    s_hat[k]<-BtSmplX[which(diff[,k]==0),k]
  }
  # 2. The second condition is if all resample values are equal 
  if(length(which(diff[,k]==TS))==nX)
  {
    s_hat[k]<-BtSmplX[1,k]
  }
  # 3. The third condition is when all the resample values ES<TS  
  else if(length(which(diff[,k]<0))==0 && length(which(diff[,k]==TS))<nX)
  {
    # This calculates the index of the second smallest bootstrap resample
    m0<-max(which(BtSmplX[,k]==min(BtSmplX[,k])))+1
    s_hat[k]<-BtSmplX[1,k]-(((TS-esMat[1,k])*(BtSmplX[m0,k]-BtSmplX[1,k]))/
                                  (esMat[1,k]-esMat[m0,k]))
  }
  # 4. The fourth condition is when resample ES values are such that ES(1)<TS<ES(nX)
  else 
  {
    s_hat[k]<-BtSmplX[hi[k],k]-(((TS-esMat[hi[k],k])*(BtSmplX[hi[k],k]-BtSmplX[lo[k],k]))/
                                      (esMat[lo[k],k]-esMat[hi[k],k]))
  }
  if (is.nan(s_hat[k])) browser() # used for tracing the origin of the NaN values
}

# Calculate the mean ROP of all the bootstrap resamples' ROP
s_boot<-mean(s_hat) 
}

######        END CONVENTIONAL BOOTSTRAP ALGORITHM       ######


######   SILVER MODIFIED FILL RATE BOOTSTRAP ALGORITHM FOR FIXED Q & P_2 <= 0.9  ######
SMFRBoot<-function(tildeX){
  
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
  
  #B Bootstrap sorted resamples of LTD data in tilde X
  BtSmplX<-replicate(B,sort(sample(tildeX,size=nX,replace = TRUE)))
  
  #Use the ExpShort() function to calc exp. shorts for all B resamples (in cols)
  SMesMat<-apply(BtSmplX,2,SMExpShort,n=nX)
  
  # Calculate the target shorts (TS)
  TS<-Q*(1-P2)
  
  # Calculate this difference before the for loop to calculate the s_hat vector
  diff<-TS-SMesMat
  
  # Function to find the largest negative value in a resample
  maxneg<-function(vec)
  {
    # Test that at least one ES > TS
    if(length(which(vec<0))>0){
      out<-max(which(vec==max(vec[vec<0])))
    }
    # If all ES < TS then flag for debugging
    else {out<-99}
    return(out)
  }
  
  # Function to find the smallest positive value in a resample
  minpos<-function(vect)
  {
    out<-min(which(vect==min(vect[vect>0])))
    return(out)
  }
  
  # Vector of resample indices with smallest ES>TS
  lo<-apply(diff,2,maxneg)
  # Vector of resample indices with smallest ES<TS
  hi<-apply(diff,2,minpos)
  
  #  returns a B length vector of the ROP estimate s_hat for each resample
  SMs_hat<-1:B # vector to store the estimates ROP for each bootstrap resample
  
  for (k in 1:B) # Iterate over all B resamples
  {
    # 1. The first condition is if at least one of the resample values ES = TS
    if(length(which(diff[,k]==0))>0)
    {
      if (which(diff[,k] == 0)) browser()
      SMs_hat[k]<-BtSmplX[which(diff[,k]==0),k]
    }
    # 2. The second condition is if all resample values are equal 
    if(length(which(BtSmplX[,k]==BtSmplX[1,k]))==nX)
    {
      SMs_hat[k]<-BtSmplX[1,k]
    }
    # 3. The third condition is when all the resample values ES<TS  
    else if(length(which(SMesMat[,k]<TS))==0 && length(which(BtSmplX[,k]==BtSmplX[1,k]))<nX)
    {
      # This calculates the index of the second smallest bootstrap resample
      m0<-max(which(BtSmplX[,k]==min(BtSmplX[,k])))+1
      SMs_hat[k]<-BtSmplX[1,k]-(((TS-esMat[1,k])*(BtSmplX[m0,k]-BtSmplX[1,k]))/
                                (esMat[1,k]-esMat[m0,k]))
    }
    # 4. The fourth condition is when resample ES values are such that ES(1)<TS<ES(nX)
    else 
    {
      SMs_hat[k]<-BtSmplX[hi[k],k]-(((TS-esMat[hi[k],k])*(BtSmplX[hi[k],k]-BtSmplX[lo[k],k]))/
                                    (esMat[lo[k],k]-esMat[hi[k],k]))
    }
    if (is.nan(SMs_hat[k])) browser() # used for tracing the origin of the NaN values
  }
  
  # Calculate the mean ROP of all the bootstrap resamples' ROP
  SMs_boot<-mean(SMs_hat) 
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
B<-500#no. of bootstrap resamples
R<-100 #no. of experimental replications
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
          # The optimize function finds the "z" value that minimizes (zeroes) the function finds the closest value to true "z"
          resultN[l]<-optimize(function(z){abs(((dnorm(z)-z*(1-pnorm(z)))-(dnorm(z+Qratio[l])-
                   (z+Qratio[l])*(1-pnorm(z+Qratio[l]))))-
                     normloss[l])},lower=0,upper = 6)$minimum*SigmaX[l]+MuX[l]
          if (resultN[l]<1)browser() # **** FOR DEBUGGING
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
