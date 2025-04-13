# One way to estimate volatility 
# Estimating Conditional Variance (Volatility Clustering)

library(tseries)     # provide get.hist.quote()
library(timeSeries)  # to provide as.timeSeries()

# Dow Jones Industrial Average price time series
P = get.hist.quote(instrument = "^DJI", start="2008-01-01", end="2013-01-1",
               quote = c("AdjClose"),provider = "yahoo", compression = "d")
plot(P)

# Log Return 
r = diff(data.matrix(log(P)))

plot(r, type='l')
# Volatility clustering 

# Estimated daily volatility
s = r*0; #conditional variance
for (i in 21:length(r))
#	s[i] = mean(r[(i-19):(i)]^2)
	s[i] = weighted.mean(r[(i-19):(i)]^2, 0.9^rev(1:20))
# starting from the 21st observation, it calculates a weighted average of the past 20 days' squared returns.
# The weighted.mean() uses weights (0.9 raised to the power of the reverse of 1:20) 
# so that more recent returns have a higher weight
# weight: 0.9,0.9^2,0.9^3...0.9^20

lines(sqrt(s), col='red', lwd=2)

plot(r, type='l', ylim=c(-0.1, 0.1))

# Upper bound and lower bound
lines(1.96*sqrt(s), col='red', lwd=2)
lines(-1.96*sqrt(s), col='red', lwd=2)

