# Crea†e Graphs of All Input Distributions
# Must run the TrueValues.R program first

TruX14df<-data.frame(TruX14)
ggplot(TruX14df,aes(x=TruX14)) + geom_density() +
  geom_vline(xintercept = quantile(TruX14,0.9), linetype="dotted", 
             color = "blue", size=1.5) +
  geom_vline(xintercept = qnorm(0.9,100,20), linetype="dashed", 
             color = "red", size=1.5)
