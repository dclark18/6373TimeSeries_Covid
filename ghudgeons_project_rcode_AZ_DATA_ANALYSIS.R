library(tswge)

# Read in the Arizona Data
azData = read.csv('C:/Dropbox/smu/time_series/homeworks/project/original_data/AZdaily.csv', header=TRUE)
# Read in the United States Data
usData = read.csv('C:/Dropbox/smu/time_series/homeworks/project/original_data/USdaily.csv', header=TRUE)
# Sort the Arizona data by date
az2 = azData[order(azData$date),]
us2 = usData[order(usData$date),]
# Plot of date, just as a QA/QC Check - all looks good.
plotts.sample.wge(az2$date)
# Examine some plots of the data, such as the cumulative death rate.
plotts.sample.wge(az2$death[12:140])
# Plot the total number of positive cases, negative cases, and the ratio between he two.
plotts.sample.wge(az2$positive)
plotts.sample.wge(az2$negative)
plotts.sample.wge(az2$negative/az2$positive)
# Once the data stabilizes there seems to be a trend, that as the ratio of neg/pos decreases, that positives are increasing relative to negatives.  

# This plot shows the increase in positive cases over time.  This is more useful than cummulative cases as this gives more information about change over time without needing subtraction for the cumulative totals.  
plotts.sample.wge(az2$positiveIncrease)
# This shows the same for negative cases over time.
plotts.sample.wge(az2$negativeIncrease)
# The totalIncrease created by summing the positive and negative increase should be equivalent to the number of tests administered without considering pending cases. 
totalIncrease = az2$positiveIncrease + az2$negativeIncrease
# This variable (totalTestResultsIncresase) should closely match the totalIncrease 
plotts.sample.wge(totalIncrease)
# The totalIncrease variable shows to match exactly with the totalTestResultsIncrease variable, so now we can have confidence on how that variable was calculated and use it moving forward.
# Here is a graph of increase count in total tests.
plotts.sample.wge(az2$totalTestResultsIncrease)
# Now we are divding the total test by cumulative positive cases. 
pp = totalIncrease / az2$positive
plotts.sample.wge(pp[40:140])
# Now we are creating the variable posper, which stands for posive percentage, given as decimal percentages.

# This is a correction that gives the correct positive percentage.
posper = (az2$positiveIncrease[40:140]/az2$totalTestResultsIncrease[40:140])
plotts.sample.wge(posper)
# The realization of the posper plot shows an increasing trend over time for appoximately the past sixty days of data indicating that positive test results are increasing relative to the number of administered tests.
# the Parzen window shows a peak at about 0.14 which could indicate a weekly (1/7) period.  
# Proceed with stationary model being aware that short term stationarity is possibly unlikely. 
aic5.wge(posper, p=0:5, q=0:2) # arma(3,1)
aic5.wge(posper, p=0:5, q=0:2, type='bic') # arma(1,1)
estPosper31 = est.arma.wge(posper, p=3, q=1)
estPosper11 = est.arma.wge(posper, p=1, q=1)
mean(posper)
estPosper31$phi
estPosper31$theta
estPosper11$phi
estPosper11$theta
factor.wge(phi=estPosper31$phi)
factor.wge(phi=estPosper11$phi)

# Forcasting 10 ahead for both ARMA models.  Both of these will eventually reach the mean of 0.16
fore.arma.wge(posper,phi=estPosper11$phi, theta=estPosper11$theta, n.ahead=10,plot=TRUE)
fore.arma.wge(posper,phi=estPosper31$phi, theta=estPosper31$theta, n.ahead=10,plot=TRUE)

# Calculate the ASE for 20 steps back for both ARMA models.
for11 = fore.arma.wge(posper,phi=estPosper11$phi, theta=estPosper11$theta, n.ahead=20,lastn=TRUE, plot=TRUE)
ASE11 = mean((posper[(100-20+1):100] - for11$f)^2)

for31 = fore.arma.wge(posper,phi=estPosper31$phi, theta=estPosper31$theta, n.ahead=20,lastn=TRUE, plot=TRUE)
ASE31 = mean((posper[(100-20+1):100] - for31$f)^2)

# Try adding a non-stationary component (1-B). This Does a very bad job.
p.ns = artrans.wge(posper,phi.tr=(1))
plotts.sample.wge(p.ns)
aic5.wge(p.ns, p=0:10, q=0:2) # arima(10,1,1)
aic5.wge(p.ns, p=0:10, q=0:2, type='bic') #arima(0,1,1)
estp.ns = est.arma.wge(p.ns,p=6,q=0)
fore.arma.wge(posper,phi=estp.ns$phi, n.ahead=10,plot=TRUE,limits=FALSE)
forNS = fore.arma.wge(posper,phi=estp.ns$phi, n.ahead=10,lastn=TRUE,plot=TRUE,limits=FALSE)
ASENS = mean((posper[(100-20+1):100] - forNS$f)^2)

# Beginning the process of building a seasonal model (s=7)
y=artrans.wge(posper, phi.tr=c(0,0,0,0,0,0,1))
y_difTwice=artrans.wge(p.ns, phi.tr=c(0,0,0,0,0,0,1))
aic5.wge(y, p = 0:12, type='bic') #selects a 3,1
esty = est.arma.wge(y,p=3, q=1)
esty$phi
esty$theta
factor.wge(phi=esty$phi)
fore.aruma.wge(posper,phi=esty$phi, theta=esty$theta, d=0,s=7,n.ahead=20, lastn=FALSE, plot=TRUE)
forS7 = fore.aruma.wge(posper,phi=esty$phi, theta=esty$theta, d=0,s=7,n.ahead=20, lastn=TRUE, plot=TRUE)
ASEs7 = mean((posper[(100-20+1):100] - forS7$f)^2)

print('So, In Summary, the ASE Values Are:')
print(paste('Arma(1,1):', ASE11))
print(paste('Arma(3,1):', ASE31))
print(paste('Arima(6,1,0):', ASENS))
print(paste('Aruma(3,0,1)s=7:', ASEs7))

# Based on the ASEs, the ARMA(3,1) is producing the best model, although, I am having trouble getting the s=7 forecast to work.



