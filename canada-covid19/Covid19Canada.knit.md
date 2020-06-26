---
title: "Project_583_Canada"
author: "Sofia Bahmutsky"
date: "16/04/2020"
output:
  pdf_document: default
  html_document: default
---



#### 1.

Loading the Canada data, goes up to April 24

```r
library(ggplot2)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(timelineR)
library(data.table)
```

```
## 
## Attaching package: 'data.table'
```

```
## The following objects are masked from 'package:dplyr':
## 
##     between, first, last
```

```r
library(forecast)
```

```
## Registered S3 method overwritten by 'quantmod':
##   method            from
##   as.zoo.data.frame zoo
```

```r
library(tidyr)
library(gdata)
```

```
## gdata: read.xls support for 'XLS' (Excel 97-2004) files ENABLED.
```

```
## 
```

```
## gdata: read.xls support for 'XLSX' (Excel 2007+) files ENABLED.
```

```
## 
## Attaching package: 'gdata'
```

```
## The following objects are masked from 'package:data.table':
## 
##     first, last
```

```
## The following objects are masked from 'package:dplyr':
## 
##     combine, first, last
```

```
## The following object is masked from 'package:stats':
## 
##     nobs
```

```
## The following object is masked from 'package:utils':
## 
##     object.size
```

```
## The following object is masked from 'package:base':
## 
##     startsWith
```

```r
library(stringr)
```


```r
### second data set
cases_2 <- read.csv("/Users/Sofia/Desktop/Data583Project/canada-covid19/cases_2.csv", stringsAsFactors = F)
cases_2 <- subset(cases_2, Country.Region=='Canada')
#cases_2 <- cases_2[,-c(3:82)]
cases_2 <- cases_2[,-c(2, 3, 4)]

data_long <- gather(cases_2, date_report, n, X1.22.20:X4.24.20, factor_key=TRUE)
data_long$date_report <- as.character(data_long$date_report)

data_long$date_report<-sub("X", "", data_long$date_report)
data_long$date_report<-str_replace_all(data_long$date_report, "[.]", "/")
data_long$date_report <- as.Date(data_long$date_report, "%m/%d/%y")
data_long$date_report <- as.Date(data_long$date_report, "%d/%m/%Y")
```

## ARIMA for daily cases 
## Time series of Daily Cases - Canada wide

```r
#daily cases

canada <- data_long %>% group_by(data_long$date_report)
canada <- data_long %>%group_by(data_long$date_report) %>% summarise(total_cases= sum(n))
#canada <- subset(canada[-1,])
new <- diff(canada$total_cases, differences = 1)
new <- c(0, new)
canada$new <- new
setnames(canada, old = c('data_long$date_report'), new = c('date'))

ts_cases <- ts(canada$new)
ts.plot(ts_cases, xlab="Days", ylab="Daily New Cases")
```

![](Covid19Canada_files/figure-latex/unnamed-chunk-3-1.pdf)<!-- --> 

```r
acf(diff(canada$new))
```

![](Covid19Canada_files/figure-latex/unnamed-chunk-3-2.pdf)<!-- --> 

```r
plot(diff(canada$new))
```

![](Covid19Canada_files/figure-latex/unnamed-chunk-3-3.pdf)<!-- --> 

```r
arima_cases <- auto.arima(canada$new, seasonal = F)
arima_cases
```

```
## Series: canada$new 
## ARIMA(0,1,3) with drift 
## 
## Coefficients:
##           ma1     ma2     ma3    drift
##       -0.9890  0.1911  0.3089  13.7211
## s.e.   0.1013  0.1418  0.1880  15.7687
## 
## sigma^2 estimated as 92460:  log likelihood=-662.54
## AIC=1335.08   AICc=1335.77   BIC=1347.75
```

```r
arima_cases2 <- arima(canada$new, order=c(2, 1, 3))
arima_cases2
```

```
## 
## Call:
## arima(x = canada$new, order = c(2, 1, 3))
## 
## Coefficients:
##          ar1      ar2      ma1     ma2      ma3
##       1.1303  -0.5149  -2.1928  1.9829  -0.6283
## s.e.  0.1996   0.1004   0.2202  0.3378   0.2098
## 
## sigma^2 estimated as 78673:  log likelihood = -659.32,  aic = 1330.63
```

```r
prediction_time <- 30
preds <- forecast(arima_cases2, h=30)

dates<-c(as.Date(canada$date),as.Date(seq(as.Date(canada$date[nrow(canada)]),by="day",length.out = prediction_time+1)))
a <-plot(preds,xaxt='n',main=("Forecast of daily confirmed cases - Canada"), ylab="Number of New Cases")
at  <-  seq(1,nrow(canada)+prediction_time,length.out=12)
axis(1, at = at+5, labels = dates[at],cex.axis = .7,las=3) 
```

![](Covid19Canada_files/figure-latex/unnamed-chunk-3-4.pdf)<!-- --> 


## Daily cases for Canada - Normalized

```r
tests <- read.csv("/Users/Sofia/Desktop/Data583Project/canada-covid19/testing_cumulative.csv", stringsAsFactors = F)

tests <- select(tests, date_testing, cumulative_testing)
setnames(tests, old = c('date_testing', 'cumulative_testing'), new = c('Date', 'csum'))
tests$Date <- as.Date(tests$Date, "%d-%m-%Y")

tests <- tests %>%group_by(Date) %>% summarise(cumulative_tests= sum(csum))

new_tests <- diff(tests$cumulative_tests, differences = 1)

new_tests <- c(0, new_tests)
tests$daily_tests <- new_tests
setnames(tests, old = c('Date'), new = c('date'))

canada
```

```
## # A tibble: 94 x 3
##    date       total_cases   new
##    <date>           <int> <dbl>
##  1 2020-01-22           0     0
##  2 2020-01-23           0     0
##  3 2020-01-24           0     0
##  4 2020-01-25           0     0
##  5 2020-01-26           1     1
##  6 2020-01-27           1     0
##  7 2020-01-28           2     1
##  8 2020-01-29           2     0
##  9 2020-01-30           2     0
## 10 2020-01-31           4     2
## # ... with 84 more rows
```

```r
canada_normal <- inner_join(canada, tests, by = 'date')

canada_normal$proportion <- canada_normal$new/canada_normal$daily_tests
##############################################################

ts_cases_normalized <- ts(canada_normal$proportion)
ts.plot(ts_cases_normalized, xlab="Days", ylab="Positive daily cases (normalized)")
```

![](Covid19Canada_files/figure-latex/unnamed-chunk-4-1.pdf)<!-- --> 

```r
canada_normal <- drop_na(canada_normal)
acf(diff(canada_normal$proportion, 2))
```

![](Covid19Canada_files/figure-latex/unnamed-chunk-4-2.pdf)<!-- --> 

```r
arima_cases_normal <- auto.arima(canada_normal$proportion, seasonal = F)
arima_cases_normal
```

```
## Series: canada_normal$proportion 
## ARIMA(0,1,2) 
## 
## Coefficients:
##           ma1     ma2
##       -1.2007  0.4933
## s.e.   0.2669  0.2673
## 
## sigma^2 estimated as 0.002013:  log likelihood=33.96
## AIC=-61.91   AICc=-60.41   BIC=-58.92
```

```r
arima_cases_normal2 <- arima(canada_normal$proportion, order=c(7, 2, 2))
arima_cases_normal2
```

```
## 
## Call:
## arima(x = canada_normal$proportion, order = c(7, 2, 2))
## 
## Coefficients:
##           ar1      ar2      ar3      ar4      ar5     ar6     ar7      ma1
##       -0.6851  -0.4341  -0.2457  -0.1826  -0.0609  0.3061  0.2527  -1.5823
## s.e.   1.1252   1.4586   1.4268   1.2465   1.0845  0.9325  0.4452   1.1205
##          ma2
##       0.5823
## s.e.  1.0762
## 
## sigma^2 estimated as 0.001416:  log likelihood = 30.87,  aic = -41.74
```

```r
prediction_time <- 30
preds2 <- forecast(arima_cases_normal2, h=30)

dates<-c(as.Date(canada_normal$date),as.Date(seq(as.Date(canada_normal$date[nrow(canada_normal)]),by="day",length.out = prediction_time+1)))
plot(preds2,xaxt='n',main=("Forecast of Normalized Cases (proportion)"), ylab="Positive daily cases (normalized)")
at  <-  seq(1,nrow(canada_normal)+prediction_time,length.out=12)
axis(1, at = at+3, labels = dates[at],cex.axis = .7,las=3)
```

![](Covid19Canada_files/figure-latex/unnamed-chunk-4-3.pdf)<!-- --> 














#### Not using this part ...
## Canada-wide Cumulative

```r
#cumulative cases
cumulative <- aggregate(data_long$n, by=list(Category=data_long$date_report), FUN=sum)

ts_cases <- ts(cumulative$x)
ts.plot(ts_cases, xlab="Days", ylab="Cumulative Cases")
```

![](Covid19Canada_files/figure-latex/unnamed-chunk-5-1.pdf)<!-- --> 

## ARIMA for cumulative (I'm not sure if this is useful...I thought ARIMA was to observe patterns in the daily data, cumulative data shows entirely different pattern. )


```r
#daily cases
arima_cases_cumulative <- auto.arima(cumulative$x, seasonal = F)
arima_cases_cumulative
```

```
## Series: cumulative$x 
## ARIMA(2,2,1) 
## 
## Coefficients:
##           ar1      ar2      ma1
##       -0.4624  -0.2730  -0.4612
## s.e.   0.1635   0.1373   0.1390
## 
## sigma^2 estimated as 100307:  log likelihood=-659.26
## AIC=1326.52   AICc=1326.98   BIC=1336.61
```

```r
preds_cumulative <- forecast(arima_cases_cumulative, h=30)
plot(preds_cumulative)
```

![](Covid19Canada_files/figure-latex/unnamed-chunk-6-1.pdf)<!-- --> 



## Province Breakdown - Cumulative

```r
# cases by province
cases_prov <- aggregate(data_long$n, by=list(Category=data_long$date_report, data_long$Province.State), FUN=sum)
DT <- data.table(cases_prov, key = "Group.2")
DT <- DT[, csum := cumsum(x), by = key(DT)]
setnames(DT, old = c('Category','Group.2'), new = c('date','province'))

ggplot(data=DT,
       aes(x=DT$date, y=DT$csum, colour=DT$province)) +
       geom_line()
```

![](Covid19Canada_files/figure-latex/unnamed-chunk-7-1.pdf)<!-- --> 

```r
DT_reduced <- subset(DT, DT$province == c('Alberta', 'British Columbia', 'Ontario', 'Quebec'))
```

```
## Warning in DT$province == c("Alberta", "British Columbia", "Ontario",
## "Quebec"): longer object length is not a multiple of shorter object length
```

```r
ggplot(data=DT_reduced,
       aes(x=DT_reduced$date, y=DT_reduced$csum, colour=DT_reduced$province)) +
       geom_line()
```

![](Covid19Canada_files/figure-latex/unnamed-chunk-7-2.pdf)<!-- --> 
