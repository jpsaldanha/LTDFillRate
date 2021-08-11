# BIMODAL LTD EXAMPLE
library(extraDistr)

# Define a function to create a bimodal distribution
bimodDistFunc <- function (sz,modsplt, minpar1, modpar1, maxpar1, minpar2, modpar2, maxpar2) {
  y0 <- rtriang(sz,minpar1,maxpar1,modpar1)
  y1 <- rtriang(sz,minpar2,maxpar2,modpar2)
  
  pct <- rbinom(sz,size=1,prob=modsplt)
  y <- y0*pct + y1*(1-pct) 
}

ltd<-function(lt,par1,par2,draws){
  fun<-function(l0){
    # return(sum(rnorm(floor(l0),par1,par2)+
    # (l0-floor(l0))*rnorm(1,par1,par2))) # Used for normal demand
    return(sum(rgamma(floor(l0),par1^2/par2^2,par1/par2^2)+
                  (l0-floor(l0))*rnorm(1,par1^2/par2^2,par1/par2^2))) # Used for gamma demand  
  }
  return(sapply(lt,fun))
}

# Define a function to estimate the expected shorts for every bootstrap sample
ExpShort<-function(x,n)
{
  short<-1:n
  for (i in 1:n)
  {
    short[i]<-sum(x[i:n]-x[i])/n
  }
  return(short)
}

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

# SKU17 LTD example

a17<-read.csv("SKU17LTD.csv")
sku17<-sort(sample(a17$VAR1,10^6))
Q<-238
P2<-0.99
TS<-Q*(1-P2)

mu17<-mean(sku17)
sigma17<-sd(sku17)
Gshape<-mu17^2/sigma17^2 
Gscale<-mu17/sigma17^2 

if(P2>0.9){
# THE TRADITIONAL METHOD
# system.time(es17<-ExpShort(sku17,10^6))
  diff<-TS-es17
  # The next two values are the indices for the ES vector on either side of the TS
  lo<-max(which(diff==max(diff[diff<0]))) # Smallest ES<TS
  hi<-min(which(diff==min(diff[diff>=0]))) # Smallest ES>=TS
  # Calculate the bootstrap ROP and SS
  TruROP<-sku17[hi]-(((TS-es17[hi])*(sku17[hi]-sku17[lo]))/
                               (es17[lo]-es17[hi]))
  TruSS<-TruROP-mean(sku17)
}else{
# SILVER MODIFICATION TRUE ROP & SS
# system.time(smes17<-SMExpShort(sku17,10^6,Q))
  diff<-TS-smes17
    # 1. The first condition is when all SMES<TS  
    if(length(which(diff<0))==0) # This accomodates the case where SMES[1,b]=TS or diff[1]=0
    {
      # Calculates the index of the second smallest sku17 sample considering duplicates
      m0<-max(which(sku17==min(sku17)))+1
      TruROP<-sku17[1]-(((TS-smes17[1])*(sku17[m0]-sku17[1]))/
                                        (smes17[1]-smes17[m0]))
    }
    # 2. The second condition is when resample ES values are such that ES(1)<TS<ES(nX)
    else 
    {
      # The next two values are the indices for the ES vector on either side of the TS
      lo<-max(which(diff==max(diff[diff<0]))) # Smallest ES<TS
      hi<-min(which(diff==min(diff[diff>=0]))) # Smallest ES>=TS
      
      # Calculate the bootstrap ROP and SS
      smTruROP<-sku17[hi]-(((TS-smes17[hi])*(sku17[hi]-sku17[lo]))/
                                         (smes17[lo]-smes17[hi]))
      smTruSS<-smTruROP-mean(sku17)
    }
}
# NORMAL ROP & SS

if(P2>0.9){
    # The optimize function finds the "z" value that minimizes (zeroes) 
    #  the function finds the closest value to true "z"
    normloss<-Q/sd(sku17)*(1-P2)
    NormROP<-optimize(function(z){abs((dnorm(z)-z*(1-pnorm(z)))-
                                normloss)},lower=0,upper = 6)$minimum*sd(sku17)+mean(sku17)
#    if (resultN[l]<1)browser() # **** FOR DEBUGGING
}else{
    # The optimize function finds the "z" value that minimizes (zeroes) 
    #  the function finds the closest value to true "z"
    normloss<-Q/sd(sku17)*(1-P2)
    smNormROP<-optimize(function(z){abs(((dnorm(z)-z*(1-pnorm(z)))-
             (dnorm(z+Q/sd(sku17))-(z+Q/sd(sku17))*(1-pnorm(z+Q/sd(sku17)))))-
             normloss)},lower=0,upper = 6)$minimum*sd(sku17)+mean(sku17)
#    if (resultN[l]<1)browser() # **** FOR DEBUGGING
}

# Returns the gamma ROP from Tyworth Guo and Ganeshan (1996) for P2 > 0.9
gESC<-Q*(1-P2) # Calculates the ESC
if(P2>0.9){
  
    GammROP<-optimize(function(s){abs((Gshape/Gscale*(1-pgamma(s,Gshape+1,
              Gscale))-s*(1-pgamma(s,Gshape,Gscale)))-gESC)},
                         lower=0,upper=qgamma(0.99,Gshape,Gscale))$minimum
    #          if (resultG[g]<1)browser()
} else {
  # Returns the gamma ROP from Silver (1970) for P2 <= 0.9
    smGammROP<-optimize(function(s)
    {abs(Q-(Q*pgamma(s,Gshape,Gscale)+(s+Q)*(pgamma(s+Q,Gshape,Gscale)-
            pgamma(s,Gshape,Gscale))-Gshape/Gscale*
              (pgamma(s+Q,Gshape+1,Gscale)-pgamma(s,Gshape+1,Gscale)))-gESC)
    },lower=0,upper=qgamma(0.99,Gshape,Gscale))$minimum
    #          if (resultG[g]<1)browser()

}

hist(sku17,freq = FALSE,xlab = "SKU17 LTD",col = "lightblue",main = NULL)
if(P2>0.9){
  abline(v=c(TruROP,NormROP,GammROP), col=c("black","red","blue"),lwd=c(3,2,2))
  # Change the SKU and the P2 level in the tilte within the ShinyApp
  title(paste("SKU 17", " at P_2=0.9"))
} else{
  abline(v=c(smTruROP,smNormROP,smGammROP,NormROP,GammROP), 
         col=c("black","red","blue","red","blue"),lwd=c(3,2,2,1,1),lty=c(1,1,1,2,2))
  # Change the SKU and the P2 level in the tilte within the ShinyApp
title(main = paste("SKU17","at P2=0.9"))
  
}

# SKU3 LTD example

a0<-read.csv("SKU3LTD.csv")
sku3<-sort(sample(a0$VAR1,10^6))
Q<-350
P2<-0.99
TS<-Q*(1-P2)

if(P2>0.9){
  # THE TRADITIONAL METHOD
  #system.time(es3<-ExpShort(sku3,10^6))
  diff<-TS-es3
  # The next two values are the indices for the ES vector on either side of the TS
  lo<-max(which(diff==max(diff[diff<0]))) # Smallest ES<TS
  hi<-min(which(diff==min(diff[diff>=0]))) # Smallest ES>=TS
  # Calculate the bootstrap ROP and SS
  TruROP<-sku3[hi]-(((TS-es3[hi])*(sku3[hi]-sku3[lo]))/
                       (es3[lo]-es3[hi]))
  TruSS<-TruROP-mean(sku3)
}else{
  # SILVER MODIFICATION TRUE ROP & SS
  # system.time(smes3<-SMExpShort(sku3,10^6,Q))
  diff<-TS-smes3
  # 1. The first condition is when all SMES<TS  
  if(length(which(diff<0))==0) # This accomodates the case where SMES[1,b]=TS or diff[1]=0
  {
    # Calculates the index of the second smallest sku3 sample considering duplicates
    m0<-max(which(sku3==min(sku3)))+1
    TruROP<-sku3[1]-(((TS-smes3[1])*(sku3[m0]-sku3[1]))/
                        (smes3[1]-smes3[m0]))
  }
  # 2. The second condition is when resample ES values are such that ES(1)<TS<ES(nX)
  else 
  {
    # The next two values are the indices for the ES vector on either side of the TS
    lo<-max(which(diff==max(diff[diff<0]))) # Smallest ES<TS
    hi<-min(which(diff==min(diff[diff>=0]))) # Smallest ES>=TS
    
    # Calculate the bootstrap ROP and SS
    smTruROP<-sku3[hi]-(((TS-smes3[hi])*(sku3[hi]-sku3[lo]))/
                           (smes3[lo]-smes3[hi]))
    smTruSS<-smTruROP-mean(sku3)
  }
}

# SKU6 LTD example

a6<-read.csv("SKU6LTD.csv")
sku6<-sort(sample(a6$VAR1,10^6))
Q<-3380
P2<-0.99
TS<-Q*(1-P2)

if(P2>0.9){
  # THE TRADITIONAL METHOD
  #system.time(es6<-ExpShort(sku6,10^6))
  diff<-TS-es6
  # The next two values are the indices for the ES vector on either side of the TS
  lo<-max(which(diff==max(diff[diff<0]))) # Smallest ES<TS
  hi<-min(which(diff==min(diff[diff>=0]))) # Smallest ES>=TS
  # Calculate the bootstrap ROP and SS
  TruROP<-sku6[hi]-(((TS-es6[hi])*(sku6[hi]-sku6[lo]))/
                      (es6[lo]-es6[hi]))
  TruSS<-TruROP-mean(sku6)
}else{
  # SILVER MODIFICATION TRUE ROP & SS
  # system.time(smes6<-SMExpShort(sku6,10^6,Q))
  diff<-TS-smes6
  # 1. The first condition is when all SMES<TS  
  if(length(which(diff<0))==0) # This accomodates the case where SMES[1,b]=TS or diff[1]=0
  {
    # Calculates the index of the second smallest sku6 sample considering duplicates
    m0<-max(which(sku6==min(sku6)))+1
    TruROP<-sku6[1]-(((TS-smes6[1])*(sku6[m0]-sku6[1]))/
                       (smes6[1]-smes6[m0]))
  }
  # 2. The second condition is when resample ES values are such that ES(1)<TS<ES(nX)
  else 
  {
    # The next two values are the indices for the ES vector on either side of the TS
    lo<-max(which(diff==max(diff[diff<0]))) # Smallest ES<TS
    hi<-min(which(diff==min(diff[diff>=0]))) # Smallest ES>=TS
    
    # Calculate the bootstrap ROP and SS
    smTruROP<-sku6[hi]-(((TS-smes6[hi])*(sku6[hi]-sku6[lo]))/
                          (smes6[lo]-smes6[hi]))
    smTruSS<-smTruROP-mean(sku6)
  }
}

# SKU7 LTD example

a7<-read.csv("SKU7LTD.csv")
sku7<-sort(sample(a7$VAR1,10^6))
Q<-219
P2<-0.99
TS<-Q*(1-P2)

if(P2>0.9){
  # THE TRADITIONAL METHOD
  # system.time(es7<-ExpShort(sku7,10^6))
  diff<-TS-es7
  # The next two values are the indices for the ES vector on either side of the TS
  lo<-max(which(diff==max(diff[diff<0]))) # Smallest ES<TS
  hi<-min(which(diff==min(diff[diff>=0]))) # Smallest ES>=TS
  # Calculate the bootstrap ROP and SS
  TruROP<-sku7[hi]-(((TS-es7[hi])*(sku7[hi]-sku7[lo]))/
                      (es7[lo]-es7[hi]))
  TruSS<-TruROP-mean(sku7)
}else{
  # SILVER MODIFICATION TRUE ROP & SS
  #system.time(smes7<-SMExpShort(sku7,10^6,Q))
  diff<-TS-smes7
  # 1. The first condition is when all SMES<TS  
  if(length(which(diff<0))==0) # This accomodates the case where SMES[1,b]=TS or diff[1]=0
  {
    # Calculates the index of the second smallest sku7 sample considering duplicates
    m0<-max(which(sku7==min(sku7)))+1
    TruROP<-sku7[1]-(((TS-smes7[1])*(sku7[m0]-sku7[1]))/
                       (smes7[1]-smes7[m0]))
  }
  # 2. The second condition is when resample ES values are such that ES(1)<TS<ES(nX)
  else 
  {
    # The next two values are the indices for the ES vector on either side of the TS
    lo<-max(which(diff==max(diff[diff<0]))) # Smallest ES<TS
    hi<-min(which(diff==min(diff[diff>=0]))) # Smallest ES>=TS
    
    # Calculate the bootstrap ROP and SS
    smTruROP<-sku7[hi]-(((TS-smes7[hi])*(sku7[hi]-sku7[lo]))/
                          (smes7[lo]-smes7[hi]))
    smTruSS<-smTruROP-mean(sku7)
  }
}

# SKU9 LTD example

a9<-read.csv("SKU9LTD.csv")
sku9<-sort(sample(a9$VAR1,10^6))
Q<-441
P2<-0.99
TS<-Q*(1-P2)

if(P2>0.9){
  # THE TRADITIONAL METHOD
  # system.time(es9<-ExpShort(sku9,10^6))
  diff<-TS-es9
  # The next two values are the indices for the ES vector on either side of the TS
  lo<-max(which(diff==max(diff[diff<0]))) # Smallest ES<TS
  hi<-min(which(diff==min(diff[diff>=0]))) # Smallest ES>=TS
  # Calculate the bootstrap ROP and SS
  TruROP<-sku9[hi]-(((TS-es9[hi])*(sku9[hi]-sku9[lo]))/
                      (es9[lo]-es9[hi]))
  TruSS<-TruROP-mean(sku9)
}else{
  # SILVER MODIFICATION TRUE ROP & SS
  # system.time(smes9<-SMExpShort(sku9,10^6,Q))
  diff<-TS-smes9
  # 1. The first condition is when all SMES<TS  
  if(length(which(diff<0))==0) # This accomodates the case where SMES[1,b]=TS or diff[1]=0
  {
    # Calculates the index of the second smallest sku9 sample considering duplicates
    m0<-max(which(sku9==min(sku9)))+1
    TruROP<-sku9[1]-(((TS-smes9[1])*(sku9[m0]-sku9[1]))/
                       (smes9[1]-smes9[m0]))
  }
  # 2. The second condition is when resample ES values are such that ES(1)<TS<ES(nX)
  else 
  {
    # The next two values are the indices for the ES vector on either side of the TS
    lo<-max(which(diff==max(diff[diff<0]))) # Smallest ES<TS
    hi<-min(which(diff==min(diff[diff>=0]))) # Smallest ES>=TS
    
    # Calculate the bootstrap ROP and SS
    smTruROP<-sku9[hi]-(((TS-smes9[hi])*(sku9[hi]-sku9[lo]))/
                          (smes9[lo]-smes9[hi]))
    smTruSS<-smTruROP-mean(sku9)
  }
}

# SKU10 LTD example

a10<-read.csv("SKU10LTD.csv")
sku10<-sort(sample(a10$VAR1,10^6))
Q<-1760
P2<-0.99
TS<-Q*(1-P2)

if(P2>0.9){
  # THE TRADITIONAL METHOD
  # system.time(es10<-ExpShort(sku10,10^6))
  diff<-TS-es10
  # The next two values are the indices for the ES vector on either side of the TS
  lo<-max(which(diff==max(diff[diff<0]))) # Smallest ES<TS
  hi<-min(which(diff==min(diff[diff>=0]))) # Smallest ES>=TS
  # Calculate the bootstrap ROP and SS
  TruROP<-sku10[hi]-(((TS-es10[hi])*(sku10[hi]-sku10[lo]))/
                      (es10[lo]-es10[hi]))
  TruSS<-TruROP-mean(sku10)
}else{
  # SILVER MODIFICATION TRUE ROP & SS
  # system.time(smes10<-SMExpShort(sku10,10^6,Q))
  diff<-TS-smes10
  # 1. The first condition is when all SMES<TS  
  if(length(which(diff<0))==0) # This accomodates the case where SMES[1,b]=TS or diff[1]=0
  {
    # Calculates the index of the second smallest sku10 sample considering duplicates
    m0<-max(which(sku10==min(sku10)))+1
    TruROP<-sku10[1]-(((TS-smes10[1])*(sku10[m0]-sku10[1]))/
                       (smes10[1]-smes10[m0]))
  }
  # 2. The second condition is when resample ES values are such that ES(1)<TS<ES(nX)
  else 
  {
    # The next two values are the indices for the ES vector on either side of the TS
    lo<-max(which(diff==max(diff[diff<0]))) # Smallest ES<TS
    hi<-min(which(diff==min(diff[diff>=0]))) # Smallest ES>=TS
    
    # Calculate the bootstrap ROP and SS
    smTruROP<-sku10[hi]-(((TS-smes10[hi])*(sku10[hi]-sku10[lo]))/
                          (smes10[lo]-smes10[hi]))
    smTruSS<-smTruROP-mean(sku10)
  }
}

# SKU13 LTD example

a13<-read.csv("SKU13LTD.csv")
sku13<-sort(sample(a13$VAR1,10^6))
Q<-2310
P2<-0.99
TS<-Q*(1-P2)

if(P2>0.9){
  # THE TRADITIONAL METHOD
  # system.time(es13<-ExpShort(sku13,10^6))
  diff<-TS-es13
  # The next two values are the indices for the ES vector on either side of the TS
  lo<-max(which(diff==max(diff[diff<0]))) # Smallest ES<TS
  hi<-min(which(diff==min(diff[diff>=0]))) # Smallest ES>=TS
  # Calculate the bootstrap ROP and SS
  TruROP<-sku13[hi]-(((TS-es13[hi])*(sku13[hi]-sku13[lo]))/
                       (es13[lo]-es13[hi]))
  TruSS<-TruROP-mean(sku13)
}else{
  # SILVER MODIFICATION TRUE ROP & SS
  # system.time(smes13<-SMExpShort(sku13,10^6,Q))
  diff<-TS-smes13
  # 1. The first condition is when all SMES<TS  
  if(length(which(diff<0))==0) # This accomodates the case where SMES[1,b]=TS or diff[1]=0
  {
    # Calculates the index of the second smallest sku13 sample considering duplicates
    m0<-max(which(sku13==min(sku13)))+1
    TruROP<-sku13[1]-(((TS-smes13[1])*(sku13[m0]-sku13[1]))/
                        (smes13[1]-smes13[m0]))
  }
  # 2. The second condition is when resample ES values are such that ES(1)<TS<ES(nX)
  else 
  {
    # The next two values are the indices for the ES vector on either side of the TS
    lo<-max(which(diff==max(diff[diff<0]))) # Smallest ES<TS
    hi<-min(which(diff==min(diff[diff>=0]))) # Smallest ES>=TS
    
    # Calculate the bootstrap ROP and SS
    smTruROP<-sku13[hi]-(((TS-smes13[hi])*(sku13[hi]-sku13[lo]))/
                           (smes13[lo]-smes13[hi]))
    smTruSS<-smTruROP-mean(sku13)
  }
}

# SKU14 LTD example

a14<-read.csv("SKU14LTD.csv")
sku14<-sort(sample(a14$VAR1,10^6))
Q<-20600
P2<-0.99
TS<-Q*(1-P2)

if(P2>0.9){
  # THE TRADITIONAL METHOD
  # system.time(es14<-ExpShort(sku14,10^6))
  diff<-TS-es14
  # The next two values are the indices for the ES vector on either side of the TS
  lo<-max(which(diff==max(diff[diff<0]))) # Smallest ES<TS
  hi<-min(which(diff==min(diff[diff>=0]))) # Smallest ES>=TS
  # Calculate the bootstrap ROP and SS
  TruROP<-sku14[hi]-(((TS-es14[hi])*(sku14[hi]-sku14[lo]))/
                       (es14[lo]-es14[hi]))
  TruSS<-TruROP-mean(sku14)
}else{
  # SILVER MODIFICATION TRUE ROP & SS
  # system.time(smes14<-SMExpShort(sku14,10^6,Q))
  diff<-TS-smes14
  # 1. The first condition is when all SMES<TS  
  if(length(which(diff<0))==0) # This accomodates the case where SMES[1,b]=TS or diff[1]=0
  {
    # Calculates the index of the second smallest sku14 sample considering duplicates
    m0<-max(which(sku14==min(sku14)))+1
    TruROP<-sku14[1]-(((TS-smes14[1])*(sku14[m0]-sku14[1]))/
                        (smes14[1]-smes14[m0]))
  }
  # 2. The second condition is when resample ES values are such that ES(1)<TS<ES(nX)
  else 
  {
    # The next two values are the indices for the ES vector on either side of the TS
    lo<-max(which(diff==max(diff[diff<0]))) # Smallest ES<TS
    hi<-min(which(diff==min(diff[diff>=0]))) # Smallest ES>=TS
    
    # Calculate the bootstrap ROP and SS
    smTruROP<-sku14[hi]-(((TS-smes14[hi])*(sku14[hi]-sku14[lo]))/
                           (smes14[lo]-smes14[hi]))
    smTruSS<-smTruROP-mean(sku14)
  }
}

# SKU16 LTD example

a16<-read.csv("SKU16LTD.csv")
sku16<-sort(sample(a16$VAR1,10^6))
Q<-709
P2<-0.99
TS<-Q*(1-P2)

if(P2>0.9){
  # THE TRADITIONAL METHOD
  # system.time(es16<-ExpShort(sku16,10^6))
  diff<-TS-es16
  # The next two values are the indices for the ES vector on either side of the TS
  lo<-max(which(diff==max(diff[diff<0]))) # Smallest ES<TS
  hi<-min(which(diff==min(diff[diff>=0]))) # Smallest ES>=TS
  # Calculate the bootstrap ROP and SS
  TruROP<-sku16[hi]-(((TS-es16[hi])*(sku16[hi]-sku16[lo]))/
                       (es16[lo]-es16[hi]))
  TruSS<-TruROP-mean(sku16)
}else{
  # SILVER MODIFICATION TRUE ROP & SS
  # system.time(smes16<-SMExpShort(sku16,10^6,Q))
  diff<-TS-smes16
  # 1. The first condition is when all SMES<TS  
  if(length(which(diff<0))==0) # This accomodates the case where SMES[1,b]=TS or diff[1]=0
  {
    # Calculates the index of the second smallest sku16 sample considering duplicates
    m0<-max(which(sku16==min(sku16)))+1
    TruROP<-sku16[1]-(((TS-smes16[1])*(sku16[m0]-sku16[1]))/
                        (smes16[1]-smes16[m0]))
  }
  # 2. The second condition is when resample ES values are such that ES(1)<TS<ES(nX)
  else 
  {
    # The next two values are the indices for the ES vector on either side of the TS
    lo<-max(which(diff==max(diff[diff<0]))) # Smallest ES<TS
    hi<-min(which(diff==min(diff[diff>=0]))) # Smallest ES>=TS
    
    # Calculate the bootstrap ROP and SS
    smTruROP<-sku16[hi]-(((TS-smes16[hi])*(sku16[hi]-sku16[lo]))/
                           (smes16[lo]-smes16[hi]))
    smTruSS<-smTruROP-mean(sku16)
  }
}

# SKU17 LTD example

a17<-read.csv("SKU17LTD.csv")
sku17<-sort(sample(a17$VAR1,10^6))
Q<-238
P2<-0.8
TS<-Q*(1-P2)

if(P2>0.9){
  # THE TRADITIONAL METHOD
  system.time(es17<-ExpShort(sku17,10^6))
  diff<-TS-es17
  # The next two values are the indices for the ES vector on either side of the TS
  lo<-max(which(diff==max(diff[diff<0]))) # Smallest ES<TS
  hi<-min(which(diff==min(diff[diff>=0]))) # Smallest ES>=TS
  # Calculate the bootstrap ROP and SS
  TruROP<-sku17[hi]-(((TS-es17[hi])*(sku17[hi]-sku17[lo]))/
                       (es17[lo]-es17[hi]))
  TruSS<-TruROP-mean(sku17)
}else{
  # SILVER MODIFICATION TRUE ROP & SS
  # system.time(smes17<-SMExpShort(sku17,10^6,Q))
  diff<-TS-smes17
  # 1. The first condition is when all SMES<TS  
  if(length(which(diff<0))==0) # This accomodates the case where SMES[1,b]=TS or diff[1]=0
  {
    # Calculates the index of the second smallest sku17 sample considering duplicates
    m0<-max(which(sku17==min(sku17)))+1
    TruROP<-sku17[1]-(((TS-smes17[1])*(sku17[m0]-sku17[1]))/
                        (smes17[1]-smes17[m0]))
  }
  # 2. The second condition is when resample ES values are such that ES(1)<TS<ES(nX)
  else 
  {
    # The next two values are the indices for the ES vector on either side of the TS
    lo<-max(which(diff==max(diff[diff<0]))) # Smallest ES<TS
    hi<-min(which(diff==min(diff[diff>=0]))) # Smallest ES>=TS
    
    # Calculate the bootstrap ROP and SS
    smTruROP<-sku17[hi]-(((TS-smes17[hi])*(sku17[hi]-sku17[lo]))/
                           (smes17[lo]-smes17[hi]))
    smTruSS<-smTruROP-mean(sku17)
  }
}

write.csv(smes3, 'smes3.csv')
