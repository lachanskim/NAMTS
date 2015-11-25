## All data analysis was scripted in open-source language R Version 3.2.0 "Full of Ingredients" 
## with GUI RStudio Version 0.99.473.
#set seed for reproducibility
set.seed(7); library(PerformanceAnalytics); library(xlsxjars); library(xlsx)
library(quantmod);
#read in data using quantmod

## @knitr timing

#simulate t-distribution
KappaEst = function(data){
        sum(data^2)*length(data)/(4*(sum(abs(data)))^2)
}
sim.t.4.0 = rt(n=10000, df=4); sim.t.4.5 = rt(n=10000, df=4.5)
sim.t.5.0 = rt(n=10000, df=5); sim.t.10.0 = rt(n=10000, df=10)

getSymbols("AAPL", from = "2010-01-01", to = "2014-12-31", src ="google") 
getSymbols("GOOG", from = "2010-01-01", to = "2014-12-31", src ="google")
getSymbols("MSFT", from="2010-01-01", to = "2014-12-31", src ="google") 
getSymbols("AMZN", from="2010-01-01", to = "2014-12-31", src ="google")
getSymbols("IVV", from="2010-01-01", to = "2014-12-31", src ="google") 
getSymbols("DJIA", from="2010-01-01", to = "2014-12-31", src ="FRED")

AAPL.r = log(lag(Cl(AAPL))) - log(Cl(AAPL)); KappaEst(AAPL.r[-1])
GOOG.r = log(lag(Cl(GOOG))) - log(Cl(GOOG)); KappaEst(GOOG.r[-1])
MSFT.r = log(lag(Cl(MSFT))) - log(Cl(MSFT)); KappaEst(MSFT.r[-1])
AMZN.r = log(lag(Cl(AMZN))) - log(Cl(AMZN)); KappaEst(AMZN.r[-1])
IVV.r = log(lag(Cl(IVV))) - log(Cl(IVV)); KappaEst(IVV.r[-1])

#remove missing data
DJIA = DJIA[!is.na(DJIA)]; length(DJIA)

DJIA.r = log(lag(DJIA)) - log((DJIA)); KappaEst(DJIA.r[-1])
KappaEst(sim.t.4.0); KappaEst(sim.t.4.5); KappaEst(sim.t.5.0); KappaEst(sim.t.10.0)

## @knitr emp
AAPL.r.vu = AAPL.r["2011-04-01/2011-05-31"]; KappaEst(AAPL.r.vu); 
GOOG.r.vu = GOOG.r["2011-04-01/2011-05-31"]; KappaEst(GOOG.r.vu); 
MSFT.r.vu = MSFT.r["2011-04-01/2011-05-31"]; KappaEst(MSFT.r.vu); 
AMZN.r.vu = AMZN.r["2011-04-01/2011-05-31"]; KappaEst(AMZN.r.vu); 
DJIA.r.bol = DJIA.r["2008-04-01/2011-05-31"]; KappaEst(DJIA.r.bol); 


AAPL.r.sd.vu = sd(AAPL.r.vu)*sqrt(252); GOOG.r.sd.vu = sd(GOOG.r.vu)*sqrt(252); 
MSFT.r.sd.vu = sd(MSFT.r.vu)*sqrt(252); AMZN.r.sd.vu = sd(AMZN.r.vu)*sqrt(252);
DJIA.r.sd.bol = sd(DJIA.r.bol)*sqrt(252)
AAPL.r.sd.vu; GOOG.r.sd.vu; MSFT.r.sd.vu; AMZN.r.sd.vu; DJIA.r.sd.bol

mean(c(KappaEst(AAPL.r.vu), KappaEst(GOOG.r.vu), KappaEst(MSFT.r.vu), 
       KappaEst(AMZN.r.vu), KappaEst(DJIA.r.bol)))
mean(c(sd(AAPL.r.vu), sd(GOOG.r.vu), sd(MSFT.r.vu), 
       sd(AMZN.r.vu), sd(DJIA.r.sd.bol)))

RRA = 6.5
upbd = function(k,R,sig){ #upper bound function
        sqrt(k*(exp((R*sig)^2)-1)/(251+exp((R*sig)^2)))
}

#calculate upper bounds
upbd(k=KappaEst(AAPL.r.vu),R=RRA,sig = AAPL.r.sd.vu)
upbd(k=KappaEst(GOOG.r.vu),R=RRA,sig = GOOG.r.sd.vu)
upbd(k=KappaEst(MSFT.r.vu),R=RRA,sig = MSFT.r.sd.vu)
upbd(k=KappaEst(AMZN.r.vu),R=RRA,sig = AMZN.r.sd.vu)
upbd(k=KappaEst(DJIA.r.bol),R=RRA, sig= DJIA.r.sd.bol)

