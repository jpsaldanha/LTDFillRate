# 06-navlist.R

library(shiny)
library(shinybusy)


ui <- navbarPage(title = "Fill Rate Calculator",
                 add_busy_spinner(spin = "fading-circle"),
                 tabPanel(title = "Experimental Distributions",
                          sidebarLayout(
                            sidebarPanel(
                              tags$h2("Inputs"),
                              tags$p("Select a distribution and P2 proportion",
                                     "fill rate from the drop down menus below.",
                                     "Then click",tags$strong("Run"),
                                     "to produce a 1 million draw histogram",
                                     "of the selected distribution with the",
                                     "true, normal and gamma reorder points (ROPs)",
                                     "marked at the fill rate selected.",
                                     "The explanation below the plot provides",
                                     "additional information including ROP values."),
                              selectInput("dist1",
                                          "Select a Distribution:",
                                          c("Uniform CV=1/5" = 1,
                                            "Uniform CV=1/3" = 2,
                                            "Uniform CV=1/2" = 3,
                                            "Truncated Normal CV=1/5" = 4,
                                            "Truncated Normal CV=1/5" = 5,
                                            "Truncated Normal CV=1/5" = 6,
                                            "Lognormal CV=1/5" = 7,
                                            "Lognormal CV=1/3" = 8,
                                            "Lognormal CV=1/2" = 9,
                                            "Lognormal CV=1" = 10,
                                            "Lognormal CV=2" = 11,
                                            "Right Skew Bimodal CV=1/5" = 12,
                                            "Right Skew Bimodal CV=1/3" = 13,
                                            "Right Skew Bimodal CV=1/2" = 14,
                                            "Right Skew Bimodal CV=1" = 15,
                                            "Right Skew Bimodal CV=2" = 16,
                                            "Symmetric Bimodal CV=1/5" = 17,
                                            "Symmetric Bimodal CV=1/3" = 18,
                                            "Symmetric Bimodal CV=1/2" = 19,
                                            "Left Skew Bimodal CV=1/5" = 20,
                                            "Left Skew Bimodal CV=1/3" = 21,
                                            "Left Skew Bimodal CV=1/2" = 22)),
                                          selectInput("P2exps1","Select a P2 Fill Rate:",
                                                      c(0.8,0.85,0.9,0.95,0.98,0.99)),
                              actionButton(inputId = "run1",label = "Run")
                  ),
                            mainPanel(
                              tags$h2("Histogram Plot"),
                              plotOutput("tab1plot"),
                              hr(),
                              textOutput("tab1text")))
                 ),
                 tabPanel(title = "Benchmarking Estimators",
                          sidebarLayout(
                            sidebarPanel(
                              tags$h2("Inputs"),
                              tags$p("Select a distribution and P2 proportion",
                                     "fill rate from the drop down menus.",
                                     "These inputs will result in a 1 million",
                                     "draw histogram of the selected distribution."),
                              tags$p("Next, set lead time demand data sample size n",
                                     "and the number of bootstrap resamples B",
                                     "using the sliders. A preset order quantity Q",
                                     "is then used to compute the bootstrap and",
                                     "normal or gamma estimated reorder points (ROPs)",
                                     "for 100 n size samples displayed as histograms",
                                     "overlayed on the parent distribution's histogram",
                                     "plot."),
                              tags$p("You may select to display either the normal",
                                     " or gamma estimated ROP using the radio button",
                                     "selector. The explanation below the plot provides",
                                     "additional information including mean ROP values."),
                              selectInput("dist2",
                                          "Select a distribution:",
                                          c("Uniform CV=1/5" = 1,
                                            "Uniform CV=1/3" = 2,
                                            "Uniform CV=1/2" = 3,
                                            "Truncated Normal CV=1/5" = 4,
                                            "Truncated Normal CV=1/5" = 5,
                                            "Truncated Normal CV=1/5" = 6,
                                            "Lognormal CV=1/5" = 7,
                                            "Lognormal CV=1/3" = 8,
                                            "Lognormal CV=1/2" = 9,
                                            "Lognormal CV=1" = 10,
                                            "Lognormal CV=2" = 11,
                                            "Right Skew Bimodal CV=1/5" = 12,
                                            "Right Skew Bimodal CV=1/3" = 13,
                                            "Right Skew Bimodal CV=1/2" = 14,
                                            "Right Skew Bimodal CV=1" = 15,
                                            "Right Skew Bimodal CV=2" = 16,
                                            "Symmetric Bimodal CV=1/5" = 17,
                                            "Symmetric Bimodal CV=1/3" = 18,
                                            "Symmetric Bimodal CV=1/2" = 19,
                                            "Left Skew Bimodal CV=1/5" = 20,
                                            "Left Skew Bimodal CV=1/3" = 21,
                                            "Left Skew Bimodal CV=1/2" = 22),
                                          selected = "21"),
                              selectInput("P2exps2","Select a P2 Fill Rate",
                                          c(0.8,0.85,0.9,0.95,0.98,0.99),
                                          selected = "0.95"),
                              sliderInput("nX2","Sample Size",6,50,24,step = 1
                                          ),
                              sliderInput("B2","Bootstrap Resamples",
                                          100,1000,500,step = 100
                              ),
                              tags$em("High values of sample size and bootstrap sample size",
                                      "will result in longer refresh times and may overload",
                                      "the server."),
                              radioButtons("ropEst2", "ROP Estimator",
                                           c("Normal" = 1,
                                             "Gamma" = 2),
                                           inline = TRUE),
                              actionButton(inputId = "run2",label = "Run")
                            ),
                            mainPanel(
                              tags$h2("Histogram Plot"),
                              textOutput("tab2text0"),
                              plotOutput("tab2plot"),
                              hr(),
                              tags$h3("Plot Explanation"),
                              #tableOutput("tab2testtable"),
                              #textOutput("tab2test"),
                              tags$p(textOutput("tab2text1")),
                              tags$p("Note: The mean normal or gamma ROP marks ",
                              "may not be visible if they are outside the range ",
                              "of the plot.")))
                 ),
                 tabPanel(title = "Industry Application",
                          sidebarLayout(
                            sidebarPanel(
                              tags$h2("Inputs"),
                              tags$p("Select a SKU LTD distribution and P2 proportion",
                                     "fill rate from the drop down menus.",
                                     "These inputs will result in a 500,000 randomly drawn",
                                     "histogram of the selected SKU's LTD distribution."),
                              tags$p("Next, set lead time demand data sample size n",
                                     "and the number of bootstrap resamples B",
                                     "using the sliders. The order quantity Q for the",
                                     "corresponding SKU is then used to compute the bootstrap",
                                     "and normal or gamma estimated reorder points (ROPs)",
                                     "for 100 n size samples displayed as histograms",
                                     "overlayed on the parent distribution's histogram",
                                     "plot."),
                              tags$p("You may select to display either the normal",
                                     " or gamma estimated ROP using the radio button",
                                     "selector. The explanation below the plot provides",
                                     "additional information including mean ROP values."),
                              selectInput("skutab3",
                                          "Select a SKU LTD distribution:",
                                          c("SKU 3" = 1,
                                            #"SKU 6" = 2,
                                            #"SKU 7" = 3,
                                            #"SKU 9" = 4,
                                            #"SKU 10" = 5,
                                            #"SKU 13" = 6,
                                            #"SKU 14" = 7,
                                            #"SKU 16" = 8,
                                            "SKU 17" = 9)),
                              selectInput("P2exps3","Select a P2 Fill Rate",
                                          c(0.8,0.85,0.9,0.95,0.98,0.99),
                                          selected = "0.95"),
                              sliderInput("nX3","Sample Size",6,50,24,step = 1
                              ),
                              sliderInput("B3","Bootstrap Resamples",
                                          100,1000,500,step = 100
                              ),
                              tags$em("High values of sample size and bootstrap sample size",
                                      "will result in longer refresh times and may overload",
                                      "the server."),
                              radioButtons("ropEst3", "ROP Estimator",
                                           c("Normal" = 1,
                                             "Gamma" = 2),
                                           inline = TRUE),
                              actionButton(inputId = "run3",label = "Run")
                            ),
                            mainPanel(
                              tags$h2("SKU Histogram Plot"),
                              textOutput("tab3text0"),
                              plotOutput("tab3plot"),
                              hr(),
                              tags$h3("Plot Explanation"),
                              #tableOutput("tab3testtable"),
                              textOutput("tab2test"),
                              tags$p(textOutput("tab3text1")),
                              tags$p("Note: The mean normal or gamma ROP marks ",
                                     "may not be visible if they are outside the range ",
                                     "of the plot.")))
                 )
                 
)

server <- function(input, output) {
  
  #ADD ALL CUSTOM FUNCTIONS HERE
  #Bimodal Distribution
  
  bimodDistFunc <- function (sz,modsplt, cpar1, cpar2, vpar1, vpar2) {
    #  y0 <- rgamma(sz,cpar1^2/vpar1^2, cpar1/vpar1^2) # USE WHEN FIRST DIST IS GAMMA
    y0 <- rnorm(sz,cpar1,vpar1) # USE WHEN FIRST DIST IS NORMAL
    y1 <- rnorm(sz,cpar2,vpar2)
    
    pct <- rbinom(sz,size=1,prob=modsplt)
    y <- y0*pct + y1*(1-pct) 
  }
  bootxfr<-function(x,qty,P2,B=500,rop = TRUE,seed = as.numeric(Sys.time())){
    
    set.seed(seed)
    nX<-length(x)
    
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
    
    BtSmplX<-replicate(B,sort(sample(x,size=nX,replace = TRUE)))
    BtSmplMu<-apply(BtSmplX,2,mean)
    
    TS<-qty*(1-P2)
    
    if(P2>0.9){
      
      esMat<-apply(BtSmplX,2,ExpShort,n=nX)
      
      diff<-TS-esMat
      
      lo<-apply(diff,2,maxneg)
      
      hi<-apply(diff,2,minpos)
      
      s_hat<-1:B
      
      for (k in 1:B)
      {
        
        if(length(which(diff[,k]==TS))==nX1)
        {
          s_hat[k]<-BtSmplX[1,k]
        }
        
        else if(length(which(diff[,k]<0))==0 && length(which(diff[,k]==TS))<nX)
        {
          
          m0<-max(which(BtSmplX[,k]==min(BtSmplX[,k])))+1
          s_hat[k]<-BtSmplX[1,k]-(((TS-esMat[1,k])*(BtSmplX[m0,k]-BtSmplX[1,k]))/
                                    (esMat[1,k]-esMat[m0,k]))
        }
        
        else
        {
          s_hat[k]<-BtSmplX[hi[k],k]-(((TS-esMat[hi[k],k])*(BtSmplX[hi[k],k]-
                                                              BtSmplX[lo[k],k]))/
                                        (esMat[lo[k],k]-esMat[hi[k],k]))
        }
      }
      
      if(rop == TRUE){
        return(mean(s_hat))
      }else{
        return(mean(s_hat-BtSmplMu))
      }
      
      ######               END BOOTSTRAP ALGORITHM FOR P2>0.9                 ######
      
    }else{
      
      ###### SILVER MODIFIED FILL RATE BOOTSTRAP ALGORITHM FOR FIXED qty & P_2 <= 0.9 ######
      
      SMesMat<-apply(BtSmplX,2,SMExpShort,n=nX)
      
      TS<-qty*(1-P2)
      
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
        
        if(length(which(BtSmplX[,k]==BtSmplX[1,k]))==nX)
        {
          SMs_hat[k]<-BtSmplX[1,k]
        }
        
        else if(length(which(SMesMat[,k]>TS))==0 && 
                length(which(BtSmplX[,k]==BtSmplX[1,k]))<nX)
        {
          
          m0<-max(which(BtSmplX[,k]==min(BtSmplX[,k])))+1
          SMs_hat[k]<-BtSmplX[1,k]-(((TS-SMesMat[1,k])*(BtSmplX[m0,k]-BtSmplX[1,k]))/
                                      (SMesMat[1,k]-SMesMat[m0,k]))
        }
        
        else
        {
          SMs_hat[k]<-BtSmplX[hi[k],k]-(((TS-SMesMat[hi[k],k])*
                                           (BtSmplX[hi[k],k]-BtSmplX[lo[k],k]))/
                                          (SMesMat[lo[k],k]-SMesMat[hi[k],k]))
        }
      }
      
      if(rop == TRUE){
        return(mean(SMs_hat))
      }else{
        return(mean(SMs_hat-BtSmplMu))
      }
    }
  }
  
  #ADD ALL THE REQUIRED PACKAGES HERE
  library(truncnorm)
  library(stats)
  library(ggplot2)
  
  #ADD ALL THE FILE INPUTS HERE
  Qexps<-read.csv("QExps.csv",header = FALSE)
  SKUQ<-read.csv("SKUQ.csv",header=FALSE)
  TruROP<-read.csv("TruROP.csv",header = FALSE)
  SKUTruROP<-read.csv("SKUTruROP.csv",header=FALSE)
  ExpInputs<-read.csv("ExpInputs.csv",header = FALSE)
  SKU3<-read.csv("sku3.csv")
  SKU6<-read.csv("sku6.csv")
  SKU7<-read.csv("sku7.csv")
  SKU9<-read.csv("sku9.csv")
  SKU10<-read.csv("sku10.csv")
  SKU13<-read.csv("sku13.csv")
  SKU14<-read.csv("sku14.csv")
  SKU16<-read.csv("sku16.csv")
  SKU17<-read.csv("sku17.csv")
  
  
 #########
 # TAB 1 #
 #########
  
  #INPUTS
  d1<-eventReactive(input$run1,{
    as.numeric(input$dist1)
  })
  pi1<-eventReactive(input$run1,{
    as.numeric(input$P2exps1) 
  })
  nX1<-10^6
  
  #FIND THE CV TO BE USED WITH Q1
  CV1<-reactive({
    if (d1()==1|d1()==4|d1()==7|d1()==12|d1()==17|d1()==20) {
      1
    } else if (d1()==2|d1()==5|d1()==8|d1()==13|d1()==18|d1()==21) {
      2
    } else if (d1()==3|d1()==6|d1()==9|d1()==14|d1()==19|d1()==22) {
      3
    } else if (d1()==10|d1()==15) {
      4
    } else {
      5
    }
  })

  # RETURN P2 ROW No. TO BE USED TO FIND Q1
  P2_1<-reactive({
    if (pi1()==0.99) {
      1
    } else if (pi1()==0.98) {
      2
    } else if (pi1()==0.95) {
      3
    } else if (pi1()==0.9) {
      4
    } else if (pi1()==0.85) {
      5
    } else {
      6
    }
  })
  
  #TRUE LTD DISTRIBUTION
  LTDsmpl1<-reactive({
    if (d1()<=3){
      runif(nX1,ExpInputs[d1(),1],ExpInputs[d1(),2])
      } else if (d1()>=4 && d1()<=6){
      rtruncnorm(nX1,0,mean=ExpInputs[d1(),1],sd=ExpInputs[d1(),2])
      } else if (d1()>=7 && d1()<=11){
      rlnorm(nX1,ExpInputs[d1(),1],ExpInputs[d1(),2])
      } else if (d1()>=12){
      bimodDistFunc(nX1,ExpInputs[d1(),1],
                    ExpInputs[d1(),2],ExpInputs[d1(),3],ExpInputs[d1(),4],ExpInputs[d1(),5]
                    )}
  })
  
  #TRUE ROP
  TruROP1<-reactive({
    row1<-d1()
    if (pi1()==0.80) {
      col1<-1
    } else if (pi1()==0.85) {
      col1<-2
    } else if (pi1()==0.90) {
      col1<-3
    } else if (pi1()==0.95) {
      col1<-4
    } else if (pi1()==0.98) {
      col1<-5
    } else {
      col1<-6
    }
    TruROP[row1,col1]
  })
  
  #TRADITIONAL NORMAL ROP
  normROP1<-reactive({
    Q1<-Qexps[P2_1(),CV1()]
      normloss<-Q1/sd(LTDsmpl1())*(1-pi1())
      NormROP1<-optimize(function(z){abs((dnorm(z)-z*(1-pnorm(z)))-normloss)},
                        lower=0,upper = 6)$minimum*sd(LTDsmpl1())+mean(LTDsmpl1())
      return(NormROP1)
  })
  
  #SILVER MODIFIED NORMAL ROP
  SMnormROP1<-reactive({
    Q1<-Qexps[P2_1(),CV1()]
    normloss<-Q1/sd(LTDsmpl1())*(1-pi1())
      smNormROP1<-optimize(function(z){abs(((dnorm(z)-z*(1-pnorm(z)))-
                 dnorm(z+Q1/sd(LTDsmpl1()))-(z+Q1/sd(LTDsmpl1()))*
                   (1-pnorm(z+Q1/sd(LTDsmpl1()))))-normloss)},
                 lower=0,upper = 6)$minimum*sd(LTDsmpl1())+mean(LTDsmpl1())
      return(smNormROP1)
  })
  
  #TYWORTH GUO & GANESHAN (1996) GAMMA ROP
  gammaROP1<-reactive({
    
    mu1<-mean(LTDsmpl1())
    sigma1<-sd(LTDsmpl1())
    Gshape1<-mu1^2/sigma1^2 
    Gscale1<-mu1/sigma1^2
    Q1<-Qexps[P2_1(),CV1()]
    
    gESC1<-Q1*(1-pi1()) # Calculates the ESC
    GammROP1<-optimize(function(s){abs((Gshape1/Gscale1*(1-pgamma(s,Gshape1+1,
                Gscale1))-s*(1-pgamma(s,Gshape1,Gscale1)))-gESC1)},
                        lower=0,upper=qgamma(0.99,Gshape1,Gscale1))$minimum
    return(GammROP1)
  })
  
  #SILVER MODIFIED GAMMA ROP
  SMgammaROP1<-reactive({
    
    mu1<-mean(LTDsmpl1())
    sigma1<-sd(LTDsmpl1())
    Gshape1<-mu1^2/sigma1^2 
    Gscale1<-mu1/sigma1^2
    Q1<-Qexps[P2_1(),CV1()]
    
    gESC1<-Q1*(1-pi1()) # Calculates the ESC
      smGammROP1<-optimize(function(s)
      {abs(Q1-(Q1*pgamma(s,Gshape1,Gscale1)+(s+Q1)*(pgamma(s+Q1,Gshape1,Gscale1)-
                                                 pgamma(s,Gshape1,Gscale1))-Gshape1/Gscale1*
                (pgamma(s+Q1,Gshape1+1,Gscale1)-pgamma(s,Gshape1+1,Gscale1)))-gESC1)
      },lower=0,upper=qgamma(0.99,Gshape1,Gscale1))$minimum
      return(smGammROP1)
  })
  
  #DISTRIBUTION NAME FOR HISTOGRAM TITLE
  distname1<-reactive({
    if (d1()==1) {"Uniform CV=1/5"}
    else if (d1()==2) {"Uniform CV=1/3"} 
    else if (d1()==3) {"Uniform CV=1/2"} 
    else if (d1()==4) {"Truncated Normal CV=1/5"} 
    else if (d1()==5) {"Truncated Normal CV=1/5"} 
    else if (d1()==6) {"Truncated Normal CV=1/5"}
    else if (d1()==7) {"Lognormal CV=1/5"} 
    else if (d1()==8) {"Lognormal CV=1/3"} 
    else if (d1()==9) {"Lognormal CV=1/2"} 
    else if (d1()==10) {"Lognormal CV=1"} 
    else if (d1()==11) {"Lognormal CV=2"}
    else if (d1()==12) {"Right Skew Bimodal CV=1/5"} 
    else if (d1()==13) {"Right Skew Bimodal CV=1/3"} 
    else if (d1()==14) {"Right Skew Bimodal CV=1/2"} 
    else if (d1()==15) {"Right Skew Bimodal CV=1"} 
    else if (d1()==16) {"Right Skew Bimodal CV=2"}
    else if (d1()==17) {"Symmetric Bimodal CV=1/5"} 
    else if (d1()==18) {"Symmetric Bimodal CV=1/3"} 
    else if (d1()==19) {"Symmetric Bimodal CV=1/2"} 
    else if (d1()==20) {"Left Skew Bimodal CV=1/5"} 
    else if (d1()==21) {"Left Skew Bimodal CV=1/3"}
    else {"Left Skew Bimodal CV=1/2"}
})
  
  #CALCULATE THE DIFFERENCE FROM TRUE
  diffnormTru1<-reactive({round(abs(TruROP1()-normROP1())/TruROP1(),4)*100})
  diffgammaTru1<-reactive({round(abs(TruROP1()-gammaROP1())/TruROP1(),4)*100})
  diffSMnormTru1<-reactive({round(abs(TruROP1()-SMnormROP1())/TruROP1(),4)*100})
  diffSMgammaTru1<-reactive({round(abs(TruROP1()-SMgammaROP1())/TruROP1(),4)*100})
  
  #TAB1 HISTOGRAM OUTPUT
  output$tab1plot <- renderPlot({
    hist(LTDsmpl1(), breaks = 30, col = "grey",xlab="Lead Time Demand",main="")
    if(pi1()>0.9){
      abline(v=c(TruROP1(),normROP1(),gammaROP1()), col=c("black","red","blue"),lwd=c(3,2,2))
      title(main = paste(distname1()," at P2=",as.numeric(as.numeric(input$P2exps1))))
    } else{
      abline(v=c(TruROP1(),SMnormROP1(),SMgammaROP1()),
             col=c("black","red","blue"),lwd=c(3,2,2),lty=c(1,1,1))
      title(main = paste(distname1()," at P2=",pi1()))
    }
  })

  #TAB1 HISTOGRAM PLOT EXPLANATION
  output$tab1text <-renderText({
    if(pi1()>0.9){
    paste("In the histogram plot the true reorder point (ROP) at ",
          round(TruROP1(),1)," is marked by the black line, the normal ROP at ",
          round(normROP1(),1)," is marked by the red line, and the gamma ROP at ",
          round(gammaROP1(),1)," is marked by the blue line. ",
          "The normal ROP is ",diffnormTru1(),"% from the true and the gamma ROP is ",
          diffgammaTru1(),"% from the true.",
    ### Move the text from lines 481-2 and 492-3 to a sepearate text output with html tag em()###
          "Note: The normal and gamma ROP marks may not be visible",
          " if they are outside the range of the plot.",sep="")
    } else {
      paste("In the histogram plot the true reorder point (ROP) at ",
            round(TruROP1(),1)," is marked by the black line,",
            " the Silver (1970) modified normal ROP at ",
            round(SMnormROP1(),1)," is marked by the red line,",
            " and the Silver (1970) modified ",
            "gamma ROP at ",round(SMgammaROP1(),1)," is marked by the blue line. ",
            "The Silver (1970) modified normal ROP is ",diffSMnormTru1(),
            "% from the true, the Silver (1970) modified gamma ROP is ",diffSMgammaTru1(),
            "% from the true.  ","Note: The normal and gamma ROP marks may not be visible",
            " if they are outside the range of the plot.",sep="")
    }
  })
  
    #########
    # TAB 2 #
    #########

    #INPUTS
  
  R2<-100
  nH2<-10^6
  d2<-eventReactive(input$run2,{
    as.numeric(input$dist2)
  })
  pi2<-eventReactive(input$run2,{
    as.numeric(input$P2exps2) 
  })
  nX2<-eventReactive(input$run2,{
    as.numeric(input$nX2) 
  })
  nB2<-eventReactive(input$run2,{
    as.numeric(input$B2)  
  }) 
  ropEst2<-eventReactive(input$run2,{
    as.numeric(input$ropEst2)
  })
      
    #FIND THE CV TO BE USED WITH Q2
    CV2<-reactive({
      if (d2()==1|d2()==4|d2()==7|d2()==12|d2()==17|d2()==20) {
        1
      } else if (d2()==2|d2()==5|d2()==8|d2()==13|d2()==18|d2()==21) {
        2
      } else if (d2()==3|d2()==6|d2()==9|d2()==14|d2()==19|d2()==22) {
        3
      } else if (d2()==10|d2()==15) {
        4
      } else {
        5
      }
    })

    # RETURN P2 ROW No. TO BE USED TO FIND Q2
    P2_2<-reactive({
      if (pi2()==0.8) {
        1
      } else if (pi2()==0.85) {
        2
      } else if (pi2()==0.9) {
        3
      } else if (pi2()==0.95) {
        4
      } else if (pi2()==0.98) {
        5
      } else {
        6
      }
    })
    
    LTDhist2<-reactive({
      if (d2()<=3){
        runif(nH2,ExpInputs[d2(),1],ExpInputs[d2(),2])
      } else if (d2()>=4 && d2()<=6){
        rtruncnorm(nH2,0,mean=ExpInputs[d2(),1],sd=ExpInputs[d2(),2])
      } else if (d2()>=7 && d2()<=11){
        rlnorm(nH2,ExpInputs[d2(),1],ExpInputs[d2(),2])
      } else if (d2()>=12){
        bimodDistFunc(nH2,ExpInputs[d2(),1],ExpInputs[d2(),2],ExpInputs[d2(),3],
                      ExpInputs[d2(),4],ExpInputs[d2(),5]
        )}
    })

    #TRUE LTD R SAMPLES
    LTDsmpl2<-reactive({
      if (d2()<=3){
        replicate(R2,runif(nX2(),ExpInputs[d2(),1],ExpInputs[d2(),2]))
      } else if (d2()>=4 && d2()<=6){
        replicate(R2,rtruncnorm(nX2(),0,mean=ExpInputs[d2(),1],sd=ExpInputs[d2(),2]))
      } else if (d2()>=7 && d2()<=11){
        replicate(R2,rlnorm(nX2(),ExpInputs[d2(),1],ExpInputs[d2(),2]))
      } else if (d2()>=12){
        replicate(R2,bimodDistFunc(nX2(),ExpInputs[d2(),1],ExpInputs[d2(),2],
                      ExpInputs[d2(),3],ExpInputs[d2(),4],ExpInputs[d2(),5]))
      }
    })

    #TRUE ROP
    TruROP2<-reactive({

      row2<-d2()

      if (pi2()==0.80) {
        col2<-1
      } else if (pi2()==0.85) {
        col2<-2
      } else if (pi2()==0.90) {
        col2<-3
      } else if (pi2()==0.95) {
        col2<-4
      } else if (pi2()==0.98) {
        col2<-5
      } else {
        col2<-6
      }
      TruROP[row2,col2]
    })

    #BOOTSTRAP ROP
    bootROP2<-reactive({
      withProgress(message = "Calculating Bootstrap ROP",value = 0,{incProgress(0.1)})
      Bootrop2<-c(1:R2)
      Q2<-Qexps[P2_2(),CV2()]
      Bootrop2<-apply(LTDsmpl2(),2,bootxfr,qty=Q2,P2=pi2(),B=nB2())
      return(Bootrop2)
    })
    
    #TRADITIONAL NORMAL ROP
    normROP2<-reactive({
      withProgress(message = "Calculating Normal ROP",value = 0,{incProgress(0.1)})
      Q2<-Qexps[P2_2(),CV2()]
      NormROP2<-c(1:R2)
      for (i in 1:R2) {
        NormROP2[i]<-optimize(function(z){abs((dnorm(z)-z*(1-pnorm(z)))-
                                                Q2/sd(LTDsmpl2()[,i])*(1-pi2()))},
                              lower=0,upper = 6)$minimum*sd(LTDsmpl2()[,i])+
          mean(LTDsmpl2()[,i]) 
      }
      return(NormROP2)
    })

    #SILVER MODIFIED NORMAL ROP
    SMnormROP2<-reactive({
      withProgress(message = "Calculating the Silver (1970) modified normal ROP",
                   value = 0,{incProgress(0.1)})
      Q2<-Qexps[P2_2(),CV2()]
      smNormROP2<-c(1:R2)
      for (j in 1:R2) {
        smNormROP2[j]<-optimize(function(z){abs(((dnorm(z)-z*(1-pnorm(z)))-
                    (dnorm(z+Q2/sd(LTDsmpl2()[,j]))-(z+Q2/sd(LTDsmpl2()[,j]))*
                      (1-pnorm(z+Q2/sd(LTDsmpl2()[,j])))))-
                      Q2/sd(LTDsmpl2()[,j])*(1-pi2()))},lower=0,upper = 6)$minimum*
          sd(LTDsmpl2()[,j])+mean(LTDsmpl2()[,j]) 
      }
      return(smNormROP2)
    })

    #TYWORTH GUO & GANESHAN (1996) GAMMA ROP
    gammaROP2<-reactive({
      withProgress(message = "Calculating the gamma ROP",value = 0,{incProgress(0.1)})
      Q2<-Qexps[P2_2(),CV2()]
      gESC2<-Q2*(1-pi2())
      GammROP2<-c(1:R2)
      for (k in 1:R2) {
        mu2<-mean(LTDsmpl2()[,k])
        sigma2<-sd(LTDsmpl2()[,k])
        Gshape2<-mu2^2/sigma2^2
        Gscale2<-mu2/sigma2^2
        GammROP2[k]<-optimize(function(s){abs((Gshape2/Gscale2*(1-pgamma(s,Gshape2+1,
                  Gscale2))-s*(1-pgamma(s,Gshape2,Gscale2)))-gESC2)},
                  lower=0,upper=qgamma(0.99,Gshape2,Gscale2))$minimum 
      }
      return(GammROP2)
    })

    #SILVER MODIFIED GAMMA ROP
    SMgammaROP2<-reactive({
      withProgress(message = "Calculating the Silver (1970) modified gamma ROP",
                   value = 0,{incProgress(0.1)})
      Q2<-Qexps[P2_2(),CV2()]
      gESC2<-Q2*(1-pi2())
      smGammaROP2<-c(1:R2)
      
      for (l in 1:R2) {
        mu2<-mean(LTDsmpl2()[,l])
        sigma2<-sd(LTDsmpl2()[,l])
        Gshape2<-mu2^2/sigma2^2
        Gscale2<-mu2/sigma2^2
        smGammaROP2[l]<-optimize(function(s)
        {abs(Q2-(Q2*pgamma(s,Gshape2,Gscale2)+(s+Q2)*(pgamma(s+Q2,Gshape2,Gscale2)-
          pgamma(s,Gshape2,Gscale2))-Gshape2/Gscale2*
          (pgamma(s+Q2,Gshape2+1,Gscale2)-pgamma(s,Gshape2+1,Gscale2)))-gESC2)
        },lower=0,upper=qgamma(0.99,Gshape2,Gscale2))$minimum 
      }
      return(smGammaROP2)
    })

    #DISTRIBUTION NAME FOR HISTOGRAM TITLE
    distname2<-reactive({
      if (d2()==1) {"Uniform CV=1/5"}
      else if (d2()==2) {"Uniform CV=1/3"}
      else if (d2()==3) {"Uniform CV=1/2"}
      else if (d2()==4) {"Truncated Normal CV=1/5"}
      else if (d2()==5) {"Truncated Normal CV=1/5"}
      else if (d2()==6) {"Truncated Normal CV=1/5"}
      else if (d2()==7) {"Lognormal CV=1/5"}
      else if (d2()==8) {"Lognormal CV=1/3"}
      else if (d2()==9) {"Lognormal CV=1/2"}
      else if (d2()==10) {"Lognormal CV=1"}
      else if (d2()==11) {"Lognormal CV=2"}
      else if (d2()==12) {"Right Skew Bimodal CV=1/5"}
      else if (d2()==13) {"Right Skew Bimodal CV=1/3"}
      else if (d2()==14) {"Right Skew Bimodal CV=1/2"}
      else if (d2()==15) {"Right Skew Bimodal CV=1"}
      else if (d2()==16) {"Right Skew Bimodal CV=2"}
      else if (d2()==17) {"Symmetric Bimodal CV=1/5"}
      else if (d2()==18) {"Symmetric Bimodal CV=1/3"}
      else if (d2()==19) {"Symmetric Bimodal CV=1/2"}
      else if (d2()==20) {"Left Skew Bimodal CV=1/5"}
      else if (d2()==21) {"Left Skew Bimodal CV=1/3"}
      else {"Left Skew Bimodal CV=1/2"}
    })

    #CALCULATE THE DIFFERENCE FROM TRUE
    diffbootTru2<-reactive({round(abs(TruROP2()-mean(bootROP2()))/TruROP2(),4)*100})
    diffnormTru2<-reactive({round(abs(TruROP2()-mean(normROP2()))/TruROP2(),4)*100})
    diffgammaTru2<-reactive({round(abs(TruROP2()-mean(gammaROP2()))/TruROP2(),4)*100})
    diffSMnormTru2<-reactive({round(abs(TruROP2()-mean(SMnormROP2()))/TruROP2(),4)*100})
    diffSMgammaTru2<-reactive({round(abs(TruROP2()-mean(SMgammaROP2()))/TruROP2(),4)*100})

    #FORMAT A DATA TABLE TO PRODUCE OVERLAY HISTOGRAM PLOTS
    l2<-reactive({as.data.frame(LTDhist2())})
    b2<-reactive({as.data.frame(bootROP2())})
    n2<-reactive({as.data.frame(normROP2())})
    sn2<-reactive({as.data.frame(SMnormROP2())})
    g2<-reactive({as.data.frame(gammaROP2())})
    sg2<-reactive({as.data.frame(SMgammaROP2())})
    
    LTD2<-reactive({
      templ2<-l2()
      colnames(templ2)<-"LTD"
      templ2$Dist<-"Parent Distrib"
      return(templ2)
    })
    boot2<-reactive({
      tempb2<-b2()
      colnames(tempb2)<-"LTD"
      tempb2$Dist<-"Bootstrap Est"
      return(tempb2)
    })
    norm2<-reactive({
      tempn2<-n2()
      colnames(tempn2)<-"LTD"
      tempn2$Dist<-"Normal Est"
      return(tempn2)
    })
    SMnorm2<-reactive({
      tempsn2<-sn2()
      colnames(tempsn2)<-"LTD"
      tempsn2$Dist<-"Silver Normal Est"
      return(tempsn2)
    })
    gamm2<-reactive({
      tempg2<-g2()
      colnames(tempg2)<-"LTD"
      tempg2$Dist<-"Gamma Est"
      return(tempg2)
    })
    SMgamm2<-reactive({
      tempsg2<-sg2()
      colnames(tempsg2)<-"LTD"
      tempsg2$Dist<-"Silver Gamma Est"
      return(tempsg2)
    })
    
    dat2<-reactive({
      if(pi2()>0.9){
        if(ropEst2()==1){
          rbind(LTD2(),boot2(),norm2()) 
        }else{
          rbind(LTD2(),boot2(),gamm2())
        }
      }else{
        if(ropEst2()==1){
          rbind(LTD2(),boot2(),SMnorm2()) 
        }else{
          rbind(LTD2(),boot2(),SMgamm2())
        }
      }
    })
    
    #TAB2 HISTOGRAM OUTPUT
    output$tab2text0<-renderText({
      if(pi2()>0.9){
        if(ropEst2()==1){
          paste("Histogram plot of",distname2(),"at",pi2(),"overlayed with", 
                "a histogram of normal (red) and bootstrap (green) estimates",
                "for 100 random samples.")
        }else{
          paste("Histogram plot of",distname2(),"at",pi2(),"overlayed with", 
                "a histogram of gamma (blue) and bootstrap (green) estimates",
                "for 100 random samples.")
        }
      }else{
        if(ropEst2()==1){
          paste("Histogram plot of",distname2(),"at",pi2(),"overlayed with", 
                "a histogram of Silver (1970) modified (red) normal and bootstrap (green)",
                "estimates for 100 random samples.")
        }else{
          paste("Histogram plot of",distname2(),"at",pi2(),"overlayed with", 
                "a histogram of Silver (1970) modified gamma (blue) and bootstrap (green)",
                "estimates for 100 random samples.")
        }
      }
    })
    
    
    output$tab2plot <- renderPlot({
     withProgress(message = "Drawing the Plot",value = 0,{incProgress(0.1)})
      data2<-dat2()
      group.colors2<-c("Parent Distrib" = "grey",
                      "Bootstrap Est" = "limegreen",
                      "Normal Est" = "red",
                      "Gamma Est" = "blue",
                      "Silver Normal Est" = "red",
                      "Silver Gamma Est" = "blue")
      ggplot(data2, aes(x = LTD, fill = Dist)) +
      scale_fill_manual(values = group.colors2)  +
      geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity',bins = 30) +
      geom_vline(xintercept=TruROP2(), color="black", size=1) +
      geom_vline(xintercept=mean(bootROP2()), color="limegreen", size=1) +
      if(pi2()>0.9){
        if(ropEst2()==1){
          geom_vline(xintercept=mean(normROP2()), color="red", size=1)
        }else{
          geom_vline(xintercept=mean(gammaROP2()), color="blue", size=1)
        }
      }else{
        if(ropEst2()==1){
          geom_vline(xintercept=mean(SMnormROP2()), color="red", size=1)
        }else{
          geom_vline(xintercept=mean(SMgammaROP2()), color="blue", size=1)
        }
      }
    })
    
    # TAB2 HISTOGRAM PLOT EXPLANATION
    output$tab2text1<-renderText({
      if(pi2()>0.9){
        if(ropEst2()==1){
          paste("The  grey histogram is the true parent distribution ",
          "the green histogram is of the bootstrap estimate ROPs and the red histogram ",
          "is of the normal estimate ROPs for the 100 samples. The true ROP at ",
                round(TruROP2(),1)," is marked by the black line, the mean normal ROP at ",
                round(mean(normROP2()),1)," is marked by the red line, and the mean ",
                "bootstrap ROP at ",round(mean(bootROP2()),1)," is marked by the green line. ",
                "The normal ROP is ",mean(diffnormTru2()),"% from the true and the bootstrap ",
                "ROP is ",mean(diffbootTru2()),"% from the true.",sep = "")
        }else{
          paste("The  grey histogram is the true parent distribution ",
                "the blue histogram is of the gamma estimate ROPs and the green histogram ",
                "is of the bootstrap estimate ROPs for the 100 samples. The true ROP at ",
                round(TruROP2(),1)," is marked by the black line, the gamma ROP at ",
                round(mean(gammaROP2()),1)," is marked by the blue line, and the bootstrap ",
                "ROP at ",round(mean(bootROP2()),1)," is marked by the green line. ",
                "The normal ROP is ",mean(diffgammaTru2()),"% from the true and the ",
                "bootstrap ROP is ",mean(diffbootTru2()),"% from the true.",sep = "")
        }
      }else{
        if(ropEst2()==1){
          paste("The  grey histogram is the true parent distribution ",
                "the green histogram is of the Silver (1970) modified bootstrap estimate ",
                "ROPs and the red histogram is of the Silver (1970) normal estimate ",
                "ROPs for the 100 samples. The true ROP at ",round(TruROP2(),1)," is marked ",
                "by the black line, the mean normal ROP at ",round(mean(SMnormROP2()),1),
                " is marked by the red line, and the mean",
                " bootstrap ROP at ",round(mean(bootROP2()),1)," is marked by the green line.",
                " The normal ROP is ",mean(diffSMnormTru2()),"% from the true and the ",
                "bootstrap ROP is ",mean(diffbootTru2()),"% from the true.",sep = "")
        }else{
          paste("The  grey histogram is the true parent distribution ",
                "the green histogram is of the Silver (1970) modified bootstrap estimate ROPs ",
                "and the blue histogram is of the Silver modified gamma estimate ROPs for ",
                "the 100 samples. The true ROP at",round(TruROP2(),1)," is marked by the ",
                "black line, the gamma ROP at ",round(mean(SMgammaROP2()),1)," is marked by ",
                "the blue line, and the bootstrap ROP at ",round(mean(bootROP2()),1),
                " is marked by the green line. ",
                "The gamma ROP is ",mean(diffSMgammaTru2()),"% from the true and the bootstrap",
                " ROP is",mean(diffbootTru2()),"% from the true.",sep = "")
        }
      }
    
    })
    #########
    # TAB 3 #
    #########
    
    #INPUTS
    
    R3<-100
    nH3<-5*10^5
    sk3<-eventReactive(input$run3,{
      as.numeric(input$skutab3)
    })
    pi3<-eventReactive(input$run3,{
      as.numeric(input$P2exps3) 
    })
    nX3<-eventReactive(input$run3,{
      as.numeric(input$nX3) 
    })
    nB3<-eventReactive(input$run3,{
      as.numeric(input$B3)  
    }) 
    ropEst3<-eventReactive(input$run3,{
      as.numeric(input$ropEst3)
    })
    
    
    # RETURN P2 ROW No. TO BE USED TO FIND Q3
    P2_3<-reactive({
      if (pi3()==0.8) {
        1
      } else if (pi3()==0.85) {
        2
      } else if (pi3()==0.9) {
        3
      } else if (pi3()==0.95) {
        4
      } else if (pi3()==0.98) {
        5
      } else {
        6
      }
    })
    
    LTDhist3<-reactive({
      if (sk3()==1){
        sample(SKU3$LTD,nH3)
      } else if (sk3()==2){
        sample(SKU6$LTD,nH3)
      } else if (sk3()==3){
        sample(SKU7$LTD,nH3)
      } else if (sk3()==4){
        sample(SKU9$LTD,nH3)
      } else if (sk3()==5){
        sample(SKU10$LTD,nH3)
      } else if (sk3()==6){
        sample(SKU13$LTD,nH3)
      } else if (sk3()==7){
        sample(SKU14$LTD,nH3)
      } else if (sk3()==8){
        sample(SKU16$LTD,nH3)
      } else {sample(SKU17$LTD,nH3)}
    })
    
    
    #TRUE LTD R SAMPLES
    LTDsmpl3<-reactive({
      replicate(R3,sample(LTDhist3(),nX3()))
    })
    
    #TRUE ROP
    TruROP3<-reactive({
      
      row3<-sk3()
      
      if (pi3()==0.80) {
        col3<-1
      } else if (pi3()==0.85) {
        col3<-2
      } else if (pi3()==0.90) {
        col3<-3
      } else if (pi3()==0.95) {
        col3<-4
      } else if (pi3()==0.98) {
        col3<-5
      } else {
        col3<-6
      }
      SKUTruROP[row3,col3]
    })
    
    #BOOTSTRAP ROP
    bootROP3<-reactive({
      Bootrop3<-c(1:R3)
      Q3<-SKUQ[sk3(),1]
      Bootrop3<-apply(LTDsmpl3(),2,bootxfr,qty=Q3,P2=pi3(),B=nB3())
      return(Bootrop3)
    })
    
    #TRADITIONAL NORMAL ROP
    normROP3<-reactive({
      Q3<-SKUQ[sk3(),1]
      NormROP3<-c(1:R3)
      for (i in 1:R3) {
        NormROP3[i]<-optimize(function(z){abs((dnorm(z)-z*(1-pnorm(z)))-
                                                Q3/sd(LTDsmpl3()[,i])*(1-pi3()))},
                              lower=0,upper = 6)$minimum*sd(LTDsmpl3()[,i])+
          mean(LTDsmpl3()[,i]) 
      }
      return(NormROP3)
    })
    
    #SILVER MODIFIED NORMAL ROP
    SMnormROP3<-reactive({
      Q3<-SKUQ[sk3(),1]
      smNormROP3<-c(1:R3)
      for (j in 1:R3) {
        smNormROP3[j]<-optimize(function(z){abs(((dnorm(z)-z*(1-pnorm(z)))-
                       (dnorm(z+Q3/sd(LTDsmpl3()[,j]))-(z+Q3/sd(LTDsmpl3()[,j]))*
                       (1-pnorm(z+Q3/sd(LTDsmpl3()[,j])))))-
                       Q3/sd(LTDsmpl3()[,j])*(1-pi3()))},lower=0,upper = 6)$minimum*
          sd(LTDsmpl3()[,j])+mean(LTDsmpl3()[,j]) 
      }
      return(smNormROP3)
    })
    
    #TYWORTH GUO & GANESHAN (1996) GAMMA ROP
    gammaROP3<-reactive({
      Q3<-SKUQ[sk3(),1]
      gESC3<-Q3*(1-pi3())
      GammROP3<-c(1:R3)
      for (k in 1:R3) {
        mu3<-mean(LTDsmpl3()[,k])
        sigma3<-sd(LTDsmpl3()[,k])
        Gshape3<-mu3^2/sigma3^2
        Gscale3<-mu3/sigma3^2
        GammROP3[k]<-optimize(function(s){abs((Gshape3/Gscale3*(1-pgamma(s,Gshape3+1,
                              Gscale3))-s*(1-pgamma(s,Gshape3,Gscale3)))-gESC3)},
                              lower=0,upper=qgamma(0.99,Gshape3,Gscale3))$minimum 
      }
      return(GammROP3)
    })
    
    #SILVER MODIFIED GAMMA ROP
    SMgammaROP3<-reactive({
      Q3<-SKUQ[sk3(),1]
      gESC3<-Q3*(1-pi3())
      smGammaROP3<-c(1:R3)
      
      for (l in 1:R3) {
        mu3<-mean(LTDsmpl3()[,l])
        sigma3<-sd(LTDsmpl3()[,l])
        Gshape3<-mu3^2/sigma3^2
        Gscale3<-mu3/sigma3^2
        smGammaROP3[l]<-optimize(function(s)
        {abs(Q3-(Q3*pgamma(s,Gshape3,Gscale3)+(s+Q3)*(pgamma(s+Q3,Gshape3,Gscale3)-
                    pgamma(s,Gshape3,Gscale3))-Gshape3/Gscale3*
                   (pgamma(s+Q3,Gshape3+1,Gscale3)-pgamma(s,Gshape3+1,Gscale3)))-gESC3)
        },lower=0,upper=qgamma(0.99,Gshape3,Gscale3))$minimum 
      }
      return(smGammaROP3)
    })
    
    #DISTRIBUTION NAME FOR HISTOGRAM TITLE
    distname3<-reactive({
      if (sk3()==1) {"SKU 3"}
      else if (sk3()==2) {"SKU 6"}
      else if (sk3()==3) {"SKU 7"}
      else if (sk3()==4) {"SKU 9"}
      else if (sk3()==5) {"SKU 10"}
      else if (sk3()==6) {"SKU 13"}
      else if (sk3()==7) {"SKU 14"}
      else if (sk3()==8) {"SKU 16"}
      else {"SKU 17"}
    })
    
    #CALCULATE THE DIFFERENCE FROM TRUE
    diffbootTru3<-reactive({round(abs(TruROP3()-mean(bootROP3()))/TruROP3(),4)*100})
    diffnormTru3<-reactive({round(abs(TruROP3()-mean(normROP3()))/TruROP3(),4)*100})
    diffgammaTru3<-reactive({round(abs(TruROP3()-mean(gammaROP3()))/TruROP3(),4)*100})
    diffSMnormTru3<-reactive({round(abs(TruROP3()-mean(SMnormROP3()))/TruROP3(),4)*100})
    diffSMgammaTru3<-reactive({round(abs(TruROP3()-mean(SMgammaROP3()))/TruROP3(),4)*100})
    
    #FORMAT A DATA TABLE TO PRODUCE OVERLAY HISTOGRAM PLOTS
    l3<-reactive({as.data.frame(LTDhist3())})
    b3<-reactive({as.data.frame(bootROP3())})
    n3<-reactive({as.data.frame(normROP3())})
    sn3<-reactive({as.data.frame(SMnormROP3())})
    g3<-reactive({as.data.frame(gammaROP3())})
    sg3<-reactive({as.data.frame(SMgammaROP3())})
    
    LTD3<-reactive({
      templ3<-l3()
      colnames(templ3)<-"LTD"
      templ3$Dist<-"SKU LTD Dist"
      return(templ3)
    })
    boot3<-reactive({
      tempb3<-b3()
      colnames(tempb3)<-"LTD"
      tempb3$Dist<-"Bootstrap Est"
      return(tempb3)
    })
    norm3<-reactive({
      tempn3<-n3()
      colnames(tempn3)<-"LTD"
      tempn3$Dist<-"Normal Est"
      return(tempn3)
    })
    SMnorm3<-reactive({
      tempsn3<-sn3()
      colnames(tempsn3)<-"LTD"
      tempsn3$Dist<-"Silver Normal Est"
      return(tempsn3)
    })
    gamm3<-reactive({
      tempg3<-g3()
      colnames(tempg3)<-"LTD"
      tempg3$Dist<-"Gamma Est"
      return(tempg3)
    })
    SMgamm3<-reactive({
      tempsg3<-sg3()
      colnames(tempsg3)<-"LTD"
      tempsg3$Dist<-"Silver Gamma Est"
      return(tempsg3)
    })
    
    dat3<-reactive({
      if(pi3()>0.9){
        if(ropEst3()==1){
          rbind(LTD3(),boot3(),norm3()) 
        }else{
          rbind(LTD3(),boot3(),gamm3())
        }
      }else{
        if(ropEst3()==1){
          rbind(LTD3(),boot3(),SMnorm3()) 
        }else{
          rbind(LTD3(),boot3(),SMgamm3())
        }
      }
    })
    
    #TAB3 HISTOGRAM OUTPUT
    output$tab3text0<-renderText({
      if(pi3()>0.9){
        if(ropEst3()==1){
          paste("Histogram plot of",distname3(),"at",pi3(),"overlayed with", 
                "a histogram of normal (red) and bootstrap (green) estimates",
                "for 100 random samples.")
        }else{
          paste("Histogram plot of",distname3(),"at",pi3(),"overlayed with", 
                "a histogram of gamma (blue) and bootstrap (green) estimates",
                "for 100 random samples.")
        }
      }else{
        if(ropEst3()==1){
          paste("Histogram plot of",distname3(),"at",pi3(),"overlayed with", 
                "a histogram of Silver (1970) modified (red) normal and bootstrap (green)",
                "estimates for 100 random samples.")
        }else{
          paste("Histogram plot of",distname3(),"at",pi3(),"overlayed with", 
                "a histogram of Silver (1970) modified gamma (blue) and bootstrap (green)",
                "estimates for 100 random samples.")
        }
      }
    })
    
    
    output$tab3plot <- renderPlot({
      data3<-dat3()
      group.colors3<-c("SKU LTD Dist" = "grey60",
                      "Bootstrap Est" = "limegreen",
                      "Normal Est" = "red",
                      "Gamma Est" = "blue",
                      "Silver Normal Est" = "red",
                      "Silver Gamma Est" = "blue")
      ggplot(data3, aes(x = LTD, fill = Dist)) +
        scale_fill_manual(values = group.colors3)  +
        geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity',bins = 30) +
        geom_vline(xintercept=TruROP3(), color="black", size=1) +
        geom_vline(xintercept=mean(bootROP3()), color="limegreen", size=1) +
        if(pi3()>0.9){
          if(ropEst3()==1){
            geom_vline(xintercept=mean(normROP3()), color="red", size=1)
          }else{
            geom_vline(xintercept=mean(gammaROP3()), color="blue", size=1)
          }
        }else{
          if(ropEst3()==1){
            geom_vline(xintercept=mean(SMnormROP3()), color="red", size=1)
          }else{
            geom_vline(xintercept=mean(SMgammaROP3()), color="blue", size=1)
          }
        }
    })
    
    # TAB2 HISTOGRAM PLOT EXPLANATION
    output$tab3text1<-renderText({
      if(pi3()>0.9){
        if(ropEst3()==1){
          paste("The  grey histogram is the ",distname3()," LTD distribution ",
                "the green histogram is of the bootstrap estimate ROPs and the red histogram ",
                "is of the normal estimate ROPs for the 100 samples. The true ROP at ",
                round(TruROP3(),1)," is marked by the black line, the mean normal ROP at ",
                round(mean(normROP3()),1)," is marked by the red line, and the mean ",
                "bootstrap ROP at ",round(mean(bootROP3()),1)," is marked by the green line. ",
                "The normal ROP is ",mean(diffnormTru3()),"% from the true and the bootstrap ",
                "ROP is ",mean(diffbootTru3()),"% from the true.",sep = "")
        }else{
          paste("The  grey histogram is the ",distname3()," LTD distribution ",
                "the blue histogram is of the gamma estimate ROPs and the green histogram ",
                "is of the bootstrap estimate ROPs for the 100 samples. The true ROP at ",
                round(TruROP3(),1)," is marked by the black line, the gamma ROP at ",
                round(mean(gammaROP3()),1)," is marked by the blue line, and the bootstrap ",
                "ROP at ",round(mean(bootROP3()),1)," is marked by the green line. ",
                "The normal ROP is ",mean(diffgammaTru3()),"% from the true and the ",
                "bootstrap ROP is ",mean(diffbootTru3()),"% from the true.",sep = "")
        }
      }else{
        if(ropEst3()==1){
          paste("The  grey histogram is the ",distname3()," LTD distribution ",
                "the green histogram is of the Silver (1970) modified bootstrap estimate ",
                "ROPs and the red histogram is of the Silver (1970) normal estimate ",
                "ROPs for the 100 samples. The true ROP at ",round(TruROP3(),1)," is marked ",
                "by the black line, the mean normal ROP at ",round(mean(SMnormROP3()),1),
                " is marked by the red line, and the mean",
                " bootstrap ROP at ",round(mean(bootROP3()),1)," is marked by the green line.",
                " The normal ROP is ",mean(diffSMnormTru3()),"% from the true and the ",
                "bootstrap ROP is ",mean(diffbootTru3()),"% from the true.",sep = "")
        }else{
          paste("The  grey histogram is the ",distname3()," LTD distribution ",
                "the green histogram is of the Silver (1970) modified bootstrap estimate ROPs ",
                "and the blue histogram is of the Silver modified gamma estimate ROPs for ",
                "the 100 samples. The true ROP at",round(TruROP3(),1)," is marked by the ",
                "black line, the gamma ROP at ",round(mean(SMgammaROP3()),1)," is marked by ",
                "the blue line, and the bootstrap ROP at ",round(mean(bootROP3()),1),
                " is marked by the green line. ",
                "The gamma ROP is ",mean(diffSMgammaTru3()),"% from the true and the bootstrap",
                " ROP is",mean(diffbootTru3()),"% from the true.",sep = "")
        }
      }
      
    })
}

shinyApp(server = server, ui = ui)