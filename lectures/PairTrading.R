# Market Neutral Strategy
# Pairs Trading
# Pair Trading is designed to harness mean-reverting behaviour
#of co-integrated instruments.

# Idea:
# Find two stocks whose prices have moved together historically.
#When the spread between them widens, short the winner and buy the loser.

library(tseries)
library(timeSeries)
# Cointegration example 1, BHP(Billiton Limited of Australia)
# and Vale S.A. of Brazil

#load the bhp and vale data
setwd("D:/AAnus/semester2/ST5218 Advanced Statistical Methods in Finance/codes")
da1=read.table("data/d-bhp0206.txt",header=T)
da2=read.table("data/d-vale0206.txt",header=T)

bhp=log(da1[,9])
vale=log(da2[,9])

plot(bhp,type='l', ylim=c(-1,5))
lines(vale,type='l', col='red')
legend(800,0, c("bhp", "vale"), col=c("black","red"), 
       text.col = c("black","red"))
title('Price of BHP and VALE')
# move together historically

m1=lm(bhp~vale)
summary(m1)
e = residuals(m1)

# ADF test
adf.test(vale)
# p-value =0.07705 , do not reject H0.
adf.test(e)
# p-value < 0.01, reject H0. The residual is stationary. 

plot(e, type = "l", ylab = "BHP - 0.72*VALE")

# Example2
name0 = ""

name0[1	]="AXP"
name0[2	]="BA"
name0[3	]="CAT"
name0[4	]="CSCO"
name0[5	]="CVX"
name0[6	]="DD"
name0[7	]="DIS"
name0[8	]="GE"
name0[9	]="GS"
name0[10	]="HD"
name0[11	]="IBM"
name0[12	]="INTC"
name0[13	]="JNJ"
name0[14	]="JPM"
name0[15	]="KO"
name0[16	]="MCD"
name0[17	]="MMM"
name0[18	]="MRK"
name0[19	]="MSFT"
name0[20	]="NKE"
name0[21	]="PFE"
name0[22	]="PG"
name0[23	]="T"
name0[24	]="TRV"
name0[25	]="UNH"


start = "2020-01-01"
end = "2023-03-01"

x = get.hist.quote(instrument = name0[1], start=start, end=end, 
               quote = c("AdjClose"),provider = "yahoo", compression = "d")

for (i in 2:25)
{
  print(i)
	xi = get.hist.quote(instrument = name0[i], start=start, end=end,
               quote = c("AdjClose"),provider = "yahoo", compression = "d")
	x = merge(x, xi)
}

x = na.omit(x)

x = data.matrix(x)
colnames(x) = name0
# look at the correlation matrix
R = cor(x)
### We consider "BA" and "GE": Boeing Company and GE

k1 = 2 # index of BA
k2 = 8 # index of GE
x1 = x[,k1]/x[1,k1]
x2 = x[,k2]/x[1,k2]

# Linear regression
reg = lm(x1~x2+0) # no intercept
summary(reg)

adf.test(reg$residuals)

s = sd(reg$residuals); s
mu = mean(reg$residuals); mu

n = length(x1)

coInt = reg$residuals
 
# Set the pair trading strategy

r = 0;   
stock = 1;  # own 1 dollar of stock "BA"
 
for (i in 2:n)
{	
    if (stock[i-1] == 1)
    {
  	  if (coInt[i-1] < mu+s) 
		{
 			r[i] = log(x1[i]/x1[i-1])
			stock[i] = 1
		}else{
			r[i] = log(x2[i]/x2[i-1])
			stock[i] = 2
		}
     }
     else
     {
  	  if (coInt[i-1] > -s) 
		{
 			r[i] = log(x2[i]/x2[i-1])
                  stock[i] = 2
		}
    	 else
      	{
			r[i] = log(x1[i]/x1[i-1])
			stock[i] = 1
		}
  	}	
}

par(xpd=TRUE)
plot(coInt,type="l", ylim=c(min(coInt),
                            max(coInt)), col="blue", lty=1)
 
maxy = max(max(cumprod(exp(r))), max(x1), max(x2))
plot(cumprod(exp(r)),     type="l", ylim=c(-1, maxy), col='red',
	 xlab="days of investiment", ylab="Price of portfolios", lty=1)

lines(x1, col="blue", lty=2)
lines(x2*0.7, lty=4)

legend(0,0,c("portfolio based on Co-integraion trading", 
	paste('price of ', name0[k1]),
	paste('price of ', name0[k2])),
	col = c("red", "blue", "black"),cex=0.4, lty = c(1, 2,4) )



