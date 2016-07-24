# Week9HW_GARZA
Tony Garza  
July 20, 2016  
# Part1: ukcars
## Load data and Plot

```r
library(fpp) # fpp package must be installed first
```

```
## Loading required package: forecast
```

```
## Loading required package: zoo
```

```
## 
## Attaching package: 'zoo'
```

```
## The following objects are masked from 'package:base':
## 
##     as.Date, as.Date.numeric
```

```
## Loading required package: timeDate
```

```
## This is forecast 7.1
```

```
## Loading required package: fma
```

```
## Loading required package: tseries
```

```
## Loading required package: expsmooth
```

```
## Loading required package: lmtest
```

```r
data(ukcars) # call the ukcars data
plot(ukcars, main="ukcars Time Series Plot") # plot ukcars data
```

![](Week9HW_Garza_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

### Plotting the time series you can see both seasonal fluctuations and an upward trend throughout most of the data series.

## Use a classical decomposition to calculate the trend-cycle and seasonal indices

```r
fitd <- decompose(ukcars) #classical decompostion method
plot(fitd) #plot the decomposition to study the data further
```

![](Week9HW_Garza_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

### The results support the analysis from the plot above that there are both consistent seasonal fluctuations as well as an overall trend to the data.

## Compute and plot the seasonally adjusted data

```r
eeadj <- seasadj(fitd)
plot(eeadj,main="Seasonally Adjusted Data") # plot the seasonally adjusted data
```

![](Week9HW_Garza_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
ukcars2 <- ts(c(ukcars[1:54],ukcars[55]+500,ukcars[56:191]),start=c(1978,1),frequency=12) # create outlier in value 55
fitd <- decompose(ukcars2) # use classic decomp
eeadj2 <- seasadj(fitd) # calculate seasonally adjusted data
plot(eeadj2,main="Adding 500 to Value #55") # plot to see incfluence of outlier
```

![](Week9HW_Garza_files/figure-html/unnamed-chunk-3-2.png)<!-- -->


```r
ukcars3 <- ts(c(ukcars[1:54],ukcars[55:160],ukcars[161]+500,ukcars[162:191]),start=c(1978,1),frequency=12) # create outlier in value 161
fitd <- decompose(ukcars3) # use classic decomp
eeadj3 <- seasadj(fitd) # calculate seasonally adjusted data
plot(eeadj3,main="Adding 500 to Value #161") # plot to see incfluence of outlier
```

![](Week9HW_Garza_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

### Above I plotted the seasonally adjusted data. I also added 500 to one observation toward the beginning of the time series and then adjusted a value toward the end of the time series. Adding 500 to value 55 displayed a large upward spike in the data, however adding 500 to value 161 had a very minimal affect. So outliers have a greater influence toward the beginning of the time series.

## Use STL to decompose the series

```r
fit <- stl(ukcars, s.window=5) # STL function to decompose the series
plot(fit) 
```

![](Week9HW_Garza_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

# Part2: Volatility Study for GW Pharmaceuticals PLC- ADR Stock (GWPH)
## Download Data and Calculate Volatility

```r
library(tseries)

stockdata <- get.hist.quote('gwph',quote="Close") #downloads the stock data
```

```
## time series starts 2013-05-01
```

```r
length(stockdata)
```

```
## [1] 814
```

```r
stockret <- log(lag(stockdata)) - log(stockdata) #create the log returns of the GWPH data set

stockvol <- sd(stockret) * sqrt(250) * 100 #formula for volatilty
stockvol #returns the volatility
```

```
## [1] 76.127
```

### This stock has a volatility of 76%

## Create Volatility Function (Continuous Lookback Window)

```r
## create volatility function
get
```

```
## function (x, pos = -1L, envir = as.environment(pos), mode = "any", 
##     inherits = TRUE) 
## .Internal(get(x, envir, mode, inherits))
## <bytecode: 0x7fc1db1d87f8>
## <environment: namespace:base>
```

```r
Vol <- function(d, logrets)
{
  
  var = 0
  
  lam = 0
  
  varlist <- c()
  
  for (r in logrets) {
    
    lam = lam*(1 - 1/d) + 1
    
    var = (1 - 1/lam)*var + (1/lam)*r^2
    
    varlist <- c(varlist, var)
    
  }
  
  sqrt(varlist)
}
```
## Plot the results with varying weights

```r
volest <- Vol(10,stockret) # use function to provide volatility for different values of d (10)

volest2 <- Vol(30,stockret) # use function to provide volatility for different values of d (30)

volest3 <- Vol(100,stockret) # use function to provide volatility for different values of d (100)

plot(volest,type="l", main="Volatility of GWPH Stock", sub="Continuous Lookback Window") # plot the results

lines(volest2,type="l",col="red") # overlay d=30 plot

lines(volest3, type = "l", col="blue") # overlay d=100 plot

legend(0,.25,c("d=10","d=30","d=100"),lty=c(1,1),lwd=c(2.5,2.5),col=c("black","red","blue")) # add legend
```

![](Week9HW_Garza_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

### This stock started somewhat volatile, stabalized, and then went extremely volatile toward the end of the data stream
