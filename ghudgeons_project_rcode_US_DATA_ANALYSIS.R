library(tswge)

# Read in the Arizona Data
azData = read.csv('C:/Dropbox/smu/time_series/homeworks/project/original_data/AZdaily.csv', header=TRUE)
# Read in the United States Data
usData = read.csv('C:/Dropbox/smu/time_series/homeworks/project/original_data/USdaily.csv', header=TRUE)
# Sort the data by date
as2 = azData[order(azData$date),]
us2 = usData[order(usData$date),]
# Plot of date, just as a QA/QC Check - all looks good.
plotts.sample.wge(us2$date)
# Examine some plots of the data, such as the cumulative death rate.
plotts.sample.wge(us2$death[12:140])
# Plot the total number of positive cases, negative cases, and the ratio between he two.
plotts.sample.wge(us2$positive)
plotts.sample.wge(us2$negative)
plotts.sample.wge(us2$negative/us2$positive)

# This plot shows the increase in positive cases over time.  
plotts.sample.wge(us2$positiveIncrease)
# This shows the same for negative cases over time.
plotts.sample.wge(us2$negativeIncrease)
# The totalIncrease created by summing the positive and negative increase should be equivalent to the number of tests administered without considering pending cases. 
totalIncrease = us2$positiveIncrease + us2$negativeIncrease
# This variable (totalTestResultsIncresase) should closely match the totalIncrease 
plotts.sample.wge(totalIncrease)
# The totalIncrease variable shows to match exactly with the totalTestResultsIncrease variable, so now we can have confidence on how that variable was calculated and use it moving forward.
# Here is a graph of increase count in total tests.
plotts.sample.wge(us2$totalTestResultsIncrease)
# Now we are divding the total test by cumulative positive cases. 
pp = totalIncrease / us2$positive
plotts.sample.wge(pp[40:140])
# Now we are creating the variable posper, which stands for posive percentage, given as decimal percentages.

posper = (us2$positiveIncrease[50:140]/us2$totalTestResultsIncrease[50:140])
plotts.sample.wge(posper)
# The realization of the posper plot shows an decreasing trend over time for approximately the past sixty days of data indicating that positive test results are decreasing relative to the number of administered tests.

# Proceed with stationary model being aware that short term stationarity is possibly unlikely. 
aic5.wge(posper, p=0:5, q=0:2) # arma(2,2)
aic5.wge(posper, p=0:5, q=0:2, type='bic') # arma(2,2)
estPosper22 = est.arma.wge(posper, p=2, q=2)
mean(posper)
estPosper22$phi
estPosper22$theta
factor.wge(phi=estPosper22$phi)

# Forcasting 10 ahead for both ARMA models.  Both of these will eventually reach the mean of 0.16
fore.arma.wge(posper,phi=estPosper22$phi, theta=estPosper22$theta, n.ahead=10,plot=TRUE)

# Calculate the ASE for 20 steps back for both ARMA models.
for22 = fore.arma.wge(posper,phi=estPosper22$phi, theta=estPosper22$theta, n.ahead=20,lastn=TRUE, plot=TRUE)
ASE22 = mean((posper[(100-20+1):100] - for22$f)^2)

# Try adding a non-stationary component (1-B). This Does a very bad job.
p.ns = artrans.wge(posper,phi.tr=(1))
plotts.sample.wge(p.ns)
aic5.wge(p.ns, p=0:10, q=0:2) # arima(10,1,0)
aic5.wge(p.ns, p=0:10, q=0:2, type='bic') #arima(2,1,0)
estp.ns = est.arma.wge(p.ns,p=2,q=0)
fore.arma.wge(posper,phi=estp.ns$phi, n.ahead=10,plot=TRUE,limits=FALSE)
forNS = fore.arma.wge(posper,phi=estp.ns$phi, n.ahead=10,lastn=TRUE,plot=TRUE,limits=FALSE)
ASENS = mean((posper[(100-20+1):100] - forNS$f)^2)

# Beginning the process of building a seasonal model (s=7)
y=artrans.wge(posper, phi.tr=c(0,0,0,0,0,0,1))
y_difTwice=artrans.wge(p.ns, phi.tr=c(0,0,0,0,0,0,1))
aic5.wge(y_difTwice, p = 0:12, type='bic') #selects a aruma(9,1,0)s=7
esty = est.arma.wge(y_difTwice,p=9, q=0)
esty$phi
esty$theta
factor.wge(phi=esty$phi)
fore.aruma.wge(posper,phi=esty$phi, theta=esty$theta, d=1,s=7,n.ahead=20, lastn=FALSE, plot=TRUE)
forS7 = fore.aruma.wge(posper,phi=esty$phi, theta=esty$theta, d=1,s=7,n.ahead=20, lastn=TRUE, plot=TRUE)
ASEs7 = mean((posper[(100-20+1):100] - forS7$f)^2)

print('So, In Summary, the ASE Values Are:')
print(paste('Arma(2,2):', ASE22))
print(paste('Arima(2,1,0):', ASENS))
print(paste('Aruma(9,1,0)s=7:', ASEs7))

# Based on the ASEs, the ARMA(3,1) is producing the best model, although, I am having trouble getting the s=7 forecast to work.



