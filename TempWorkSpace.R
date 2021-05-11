

Bimodal<-sort(bimodDistFunc(nX,ExpInputs[d,1],ExpInputs[d,2],ExpInputs[d,3],
                             ExpInputs[d,4],ExpInputs[d,5]))
system.time(
BMmodES<-SMExpShort(Bimodal,nX)
)

# %%%%%%%%%%%%%%%%% NEW PARALLEIZATION OF CODE %%%%%%%%%%%%%%%%%
# Use a fixed random seed to replicate the experiments
set.seed(931971)

nX<-10^3
TruLTD<-matrix(0,nrow = nX,ncol = 22)
CV<-rep(0,22)
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
      CV[d]<-1
    } else if (d==2|d==5|d==8|d==13|d==18|d==21) {
      CV[d]<-2
    } else if (d==3|d==6|d==9|d==14|d==19|d==22) {
      CV[d]<-3
    } else if (d==10|d==15) {
      CV[d]<-4
    } else {
      CV[d]<-5
    }
  }
)

system.time(
MuX<-apply(TruLTD,2,mean)) # Mean of LTD
# Calculate the Silver (1970) modified expected shorts (ES)
system.time(
# The SMExpShort function executes in 2h21m for each 10^6 LTD or 2d 5h for all 22 dist
SMES<-apply(TruLTD,2,SMExpShort,n = nX))

system.time(
for(d in 1:22) {
  for (c in U[,1]) {
    P2<-U[c,2] # The P2 experimental level
    Q<-Qexps[c,CV[d]] # Use the correct Q
    TS<-Q*(1-P2) # Calculate the target shorts (TS)
    diff<-TS-SMES # Take the difference between the target and expected shorts
    # The next two values are the indices for the ES vector on either side of the TS
    lo<-max(which(diff[,d]==max(diff[,d][diff[,d]<0]))) # Smallest ES<TS
    hi<-min(which(diff[,d]==min(diff[,d][diff[,d]>=0]))) # Smallest ES>TS
    
    # Calculate the bootstrap ROP and SS
    SMTruROP[d,c]<-TruLTD[hi,d]-(((TS-SMES[hi,d])*(TruLTD[hi,d]-TruLTD[lo,d]))/
                                 (SMES[lo,d]-SMES[hi,d]))
    SMTruSS[d,c]<-SMTruROP[d,c]-MuX[d]
    
  }
}
)


tempfun<-function(x,a,b){sum(x)+a+a*b}

h<-matrix(1:10,nrow = 5,ncol = 2)

apply(h,2,tempfun,a=1,b=2)

lapply(1:3,function(z){apply(h,2,tempfun,a=z,b=2)})
a<-1:22
smes1<-mapply(SMExpShort,TruLTD,n = nX, qt = Q[z])


testSMExpShort<-function(x,n,d,qt)
{
  modshort<-matrix(0,nrow = n,ncol = d)
  for (k in 1:d)
  {
    for (i in 1:(n-1))
    {
      nu0<-x[i,k]
      j<-max(which((nu0+qt[k]-x[,k])>0))
      modshort[i,k]<-qt[k]-qt[k]*i/n-(sum(x[i,k]+qt[k]-x[(i+1):j,k])/n)
    }
  }
  return(modshort)
}

testSMExpShort<-function(x,n,qt){lapply(1:n,function(z){qt-qt*z/n-
    (sum(x[z]+qt-x[(z+1):max(which((x[z]+qt-x)>0))])/n)})}
system.time(
smes1<-lapply(1:22,function(t){testSMExpShort(TruLTD[,t],nX,Q[t])}))
SMES2<-matrix(0,nrow = nX,ncol = 22)
system.time(
for(m in 1:22){for(f in 1:(nX-1)){SMES2[f,m]<-smes1[[m]][[f]]}})



TruLTD<-sort(runif(10^4,ExpInputs[d,1],ExpInputs[d,2]))
system.time(ES<-ExpShort(TruLTD,10^4))

# Define a function to estimate the expected shorts for every bootstrap sample
testExpShort<-function(x,n)
{
  short<-1:n
  es0<-lapply(1:n,
     function(z)
      {
        sum(x[z:n]-x[z])/n
     }
  )
  for(e in 1:n){short[e]<-es0[[e]]}
  return(short)
}

system.time(testES<-testExpShort(TruLTD,10^4))

test<-ExpShort(TruLTD[,16],nX)
