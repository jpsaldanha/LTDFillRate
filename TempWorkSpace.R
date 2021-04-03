P2<-0.8
nX<-7
Q<-18

LTDsmpl<-replicate(R,runif(nX,ExpInputs[1,1],ExpInputs[1,2]))

BtSmplX<-replicate(B,sort(sample(LTDsmpl[,1],size=nX,replace = TRUE)))

esMat<-apply(BtSmplX,2,ExpShort,n=nX)

TS<-Q*(1-P2)

# Calculate this difference before the for loop to calculate the s_hat vector
diff<-TS-esMat

# Function to find the largest negative value
maxneg<-function(vec)
{
  out<-max(which(vec==max(vec[vec<0])))
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

s_hat_new<-1:B
for (k in 1:B) 
  {
  # 1. The first condition is if at least one of the resample values ES = TS
    if(length(which(diff[,k]==0))>0)
    {
      s_hat_new[k]<-BtSmplX[which(diff[,k]==0),k]
    }
  # 2. The second condition is when all the resample values ES<TS  
    else if(length(which(diff[,k]<0))==0)
    {
      # This calculates the index of the second smallest bootstrap resample
      m0<-max(which(BtSmplX[,k]==min(BtSmplX[,k])))+1
      s_hat_new[k]<-BtSmplX[1,k]-(((TS-esMat[1,k])*(BtSmplX[m0,k]-BtSmplX[1,k]))/
                                 (esMat[1,k]-esMat[m0,k]))
    }
  # 3. The third condition is when resample ES values are such that ES(i)<TS<ES(j)
    else 
    {
      s_hat_new[k]<-BtSmplX[hi[k],k]-(((TS-esMat[hi[k],k])*(BtSmplX[hi[k],k]-BtSmplX[lo[k],k]))/
                                 (esMat[lo[k],k]-esMat[hi[k],k]))
    }
  }





#########$$$$$$$$$$$$$$$$%%%%%%%%%%%%%%%%

dupl<-length(which(P2Pos[,1]==min(P2Pos[,1])))-1

s_hat[1]<-BtSmplX[P2PosMin[1],1]-(((BtSmplX[P2PosMin[1],1]-BtSmplX[P2PosMin[1]-dupl-1,1])*
                                     ((P2Pos[P2PosMin[1],1]-P2)*Q))/
                                    (esMat[P2PosMin[1]-dupl-1,1]-esMat[P2PosMin[1],1]))
