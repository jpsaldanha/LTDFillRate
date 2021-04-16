R<-100
B<-500
P2<-0.98
Q<-46

nX<-30
LTDsmpl<-`X-3-7`
sboot30_98<-apply(LTDsmpl,2,FRBoot)
hist(sboot30_98)
abline(v = 182.57, col = "red", lwd=2, lty=2);
mean(sboot30_98)
sd(sboot30_98)

esMat<-apply(BtSmplX,2,ExpShort,n=nX)

TS<-Q*(1-P2)

# Calculate this difference before the for loop to calculate the s_hat vector
diff<-TS-esMat

# Function to find the largest negative value
maxneg<-function(vec)
{
  if(length(which(vec<0))){
  out<-max(which(vec==max(vec[vec<0])))
  }
  else {out<-0}
  return(out)
}

# Function to find the smallest positive value
minpos<-function(vect)
{
  out<-min(which(vect==min(vect[vect>0])))
  return(out)
}
# Vector of resample indices with smallest ES>TS
lo<-apply(diff,2,maxneg)
# Vector of resample indices with smallest ES<TS
hi<-apply(diff,2,minpos)

s_hat<-1:B # vector to store the estimates ROP for each bootstrap resample

for (k in 1:B) # Iterate over all B resamples
{
  # 1. The first condition is if at least one of the resample values ES = TS
  # 2. If all resample values are equal
  if(length(which(diff[,k]==0))>0)
  {
    if (which(diff[, k] == 0)) browser()
    s_hat[k]<-BtSmplX[which(diff[,k]==0),k]
  }
  # 2. The second ondition is if all resample values are equal 
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
  # 4. The fourth condition is when resample ES values are such that ES(i)<TS<ES(j)
  else 
  {
    s_hat[k]<-BtSmplX[hi[k],k]-(((TS-esMat[hi[k],k])*(BtSmplX[hi[k],k]-BtSmplX[lo[k],k]))/
                                  (esMat[lo[k],k]-esMat[hi[k],k]))
  }
  if (is.nan(s_hat[k])) browser() # used for tracing the origin of the NaN values
}

BtSmplX7<-BtSmplX
diff7<-diff
esMat7<-esMat
s_hat7<-s_hat

sdes7<-apply(esMat7,2,sd)
sdes12<-apply(esMat12,2,sd)
sdes7<-apply(esMat7,2,sd)
sdes7<-apply(esMat7,2,sd)

sdesMat<-cbind(sdes7,sdes12,sdes7,sdes70)
               
NTruSS<-optimize(function(z){abs(((dnorm(z)-z*(1-pnorm(z)))-(dnorm(z+Qratio)-
                                                                    (z+Qratio)*(1-pnorm(z+Qratio))))-
                                        normloss)},lower=0,upper = 6)$minimum*SigmaX

BTruROP<-TruLTD[hi]-(((TS-ES[hi])*(TruLTD[hi]-TruLTD[lo]))/
                            (ES[lo]-ES[hi]))
               
