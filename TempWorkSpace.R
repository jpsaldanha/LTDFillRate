R<-5
B<-10
P2<-0.80
Q<-19
nX<-24
ExpInputs<-read.csv("ExpInputs.csv",header = FALSE)

set.seed(931971)

LTDSmpl<-replicate(R,sort(runif(nX,ExpInputs[1,1],ExpInputs[1,2])))
which((LTDSmpl[1,1]+Q-LTDSmpl[,1])>0)
j<-max(which((LTDSmpl[24,1]+Q-LTDSmpl[,1])>0))

modExpShort<-function(x,n)
  {
    modshort<-(1:n)*0
    for (i in 1:(n-1))
      {
        j<-max(which((x[i]+Q-x)>0))
        modshort[i]<-Q-Q*i/n-(sum(x[i]+Q-x[(i+1):j])/n)
        if(modshort[i]<0) browser()
      }
  return(modshort)
  }

test<-rep(80,24)

test1<-modExpShort(test,24)

length(which(test1<10^-6))

bimod<-bimodDistFunc(10^4,ExpInputs[12,1],ExpInputs[12,2],ExpInputs[12,3],
                                   ExpInputs[12,4],ExpInputs[12,5])

hist(bimod)
bimod<-sort(bimod)
modES<-modExpShort(bimod,10^4)