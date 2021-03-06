bootldfr<-function(x,y,qty,p2,B=500,roptru = TRUE,seed = as.numeric(Sys.time())){
  
  set.seed(seed)
  nL<-length(x)
  
  ltd<-function(p,q){
    X0<-1:nL
    for(i in 1:nL)
    {
      X0[i]<-sum(sample(q,floor(p[i]),TRUE))+((p[i]-floor(p[i]))*sample(q,1))
    }
    return(X0)
  }
  
  ExpShort<-function(x0,n)
  {
    short<-1:n
    for (i in 1:n)
    {
      short[i]<-(sum(x0[i:n]-x0[i])/n)
    }
    return(short)
  }
  
  SMExpShort<-function(x,n)
  {
    modshort<-(1:n)*0
    for (i in 1:(n-1))
    {
      j<-max(which((x[i]+qty-x)>0))
      modshort[i]<-qty-qty*i/n-(sum(x[i]+qty-x[(i+1):j])/n)
    }
    return(modshort)
  }
  
  maxneg<-function(vec)
  {
    if(length(which(vec<0))>0){
      out<-max(which(vec==max(vec[vec<0])))
    }else{
      out<-99
    }
    return(out)
  }
  
  minpos<-function(vect)
  {
    out<-min(which(vect==min(vect[vect>=0])))
    return(out)
  }
  
  BtSmplX<-replicate(B,sort(ltd(x,y)))
  BtSmplMu<-apply(BtSmplX,2,mean)
  
  TS<-qty*(1-p2)
  
  if(p2>0.9){
    
    esMat<-apply(BtSmplX,2,ExpShort,n=nL)
    
    diff<-TS-esMat
    
    lo<-apply(diff,2,maxneg)
    
    hi<-apply(diff,2,minpos)
    
    s_hat<-1:B
    
    for (k in 1:B)
    {
      
      if(length(which(diff[,k]==TS))==nL)
      {
        s_hat[k]<-BtSmplX[1,k]
      }
      
      else if(length(which(diff[,k]<0))==0 && length(which(diff[,k]==TS))<nL)
      {
        
        m0<-max(which(BtSmplX[,k]==min(BtSmplX[,k])))+1
        s_hat[k]<-BtSmplX[1,k]-(((TS-esMat[1,k])*(BtSmplX[m0,k]-BtSmplX[1,k]))/
                                  (esMat[1,k]-esMat[m0,k]))
      }
      
      else
      {
        s_hat[k]<-BtSmplX[hi[k],k]-(((TS-esMat[hi[k],k])*(BtSmplX[hi[k],k]-BtSmplX[lo[k],k]))/
                                      (esMat[lo[k],k]-esMat[hi[k],k]))
      }
      #      if (is.nan(s_hat[k])) browser() # used for tracing the origin of the NaN values
    }
    
    if(roptru == TRUE){
      return(mean(s_hat))
    }else{
      return(mean(s_hat-BtSmplMu))
    }
    
    ######                     END BOOTSTRAP ALGORITHM FOR P2>0.9                      ######
    
  }else{
    
    ######   SILVER MODIFIED FILL RATE BOOTSTRAP ALGORITHM FOR FIXED qty & P_2 <= 0.9  ######
    
    SMesMat<-apply(BtSmplX,2,SMExpShort,n=nL)
    
    TS<-qty*(1-p2)
    
    diff<-TS-SMesMat
    
    lo<-apply(diff,2,maxneg)
    
    hi<-apply(diff,2,minpos)
    
    SMs_hat<-1:B
    
    for (k in 1:B) # Iterate over all B resamples
    {
      
      if(length(which(diff[,k]==0))>0)
      {
        
        SMs_hat[k]<-BtSmplX[which(diff[,k]==0),k]
      }
      
      if(length(which(BtSmplX[,k]==BtSmplX[1,k]))==nL)
      {
        SMs_hat[k]<-BtSmplX[1,k]
      }
      
      else if(length(which(SMesMat[,k]>TS))==0 && length(which(BtSmplX[,k]==BtSmplX[1,k]))<nL)
      {
        
        m0<-max(which(BtSmplX[,k]==min(BtSmplX[,k])))+1
        SMs_hat[k]<-BtSmplX[1,k]-(((TS-SMesMat[1,k])*(BtSmplX[m0,k]-BtSmplX[1,k]))/
                                    (SMesMat[1,k]-SMesMat[m0,k]))
      }
      
      else
      {
        SMs_hat[k]<-BtSmplX[hi[k],k]-(((TS-SMesMat[hi[k],k])*(BtSmplX[hi[k],k]-
                                                                BtSmplX[lo[k],k]))/(SMesMat[lo[k],k]-SMesMat[hi[k],k]))
      }
      #      if (is.nan(SMs_hat[k])) browser()
    }
    
    if(roptru == TRUE){
      return(mean(SMs_hat))
    }else{
      return(mean(SMs_hat-BtSmplMu))
    }
  }
}