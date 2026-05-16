# Purpose

The primary objective of the accompanying R code is to empirically
evaluate the stylised facts of the United States business cycle over a
forty-three-year period spanning from Q1 1982 to Q3 2025. Specifically,
it is constructed to obtain key macroeconomic time-series data, filter
out long-term structural trends, and compute crucial second-order
moments, being standard deviations and correlation coefficients.

``` r
rm(list = ls()) # Clean your environment:
gc() # garbage collection - It can be useful to call gc after a large object has been removed, as this may prompt R to return memory to the operating system.
```

    ##           used (Mb) gc trigger (Mb) limit (Mb) max used (Mb)
    ## Ncells  564353 30.2    1254612 67.1         NA   715648 38.3
    ## Vcells 1078100  8.3    8388608 64.0      16384  2010398 15.4

``` r
if(!require(pacman)) { install.packages("pacman"); require(pacman)}
```

    ## Loading required package: pacman

``` r
pacman::p_load(tidyverse, readxl, mFilter, knitr, tseries)

list.files('code/', full.names = T, recursive = T) %>% .[grepl('.R', .)] %>% as.list() %>% walk(~source(.))
```

# Loading Data

``` r
Data <- read_excel('data/Macro_Data.xlsx',
                   sheet = "Logged Data")  # change to your sheet name

head(Data)
```

    ## # A tibble: 6 × 6
    ##   Date                Log_GDP Log_Cons Log_Debt Inflation    FFR
    ##   <dttm>                <dbl>    <dbl>    <dbl>     <dbl>  <dbl>
    ## 1 1982-01-01 00:00:00    3.91     3.45     2.78  0.00888  0.142 
    ## 2 1982-04-01 00:00:00    3.91     3.44     2.78  0.0145   0.145 
    ## 3 1982-07-01 00:00:00    3.90     3.45     2.82  0.0174   0.110 
    ## 4 1982-10-01 00:00:00    3.90     3.46     2.86  0.00307  0.0929
    ## 5 1983-01-01 00:00:00    3.92     3.47     2.89  0.000684 0.0865
    ## 6 1983-04-01 00:00:00    3.93     3.49     2.94  0.0116   0.088

``` r
colnames(Data)
```

    ## [1] "Date"      "Log_GDP"   "Log_Cons"  "Log_Debt"  "Inflation" "FFR"

# Applying HP filter to logged quantity variables

We do this because the model’s business cycle statistics are computed
from cyclical component, which are the deviations from trend. We thus
need to apply the HP filter to each log-transformed series (which we
can’t do in excel directly)

``` r
hp_gdp  <- hpfilter(Data$Log_GDP,  freq = 1600)
hp_cons <- hpfilter(Data$Log_Cons, freq = 1600)
hp_debt <- hpfilter(Data$Log_Debt, freq = 1600)
```

# Extracting cyclical components of the data

``` r
cycle_gdp  <- hp_gdp$cycle
cycle_cons <- hp_cons$cycle
cycle_debt <- hp_debt$cycle
```

# Obtaining Inflation and Fed Funds Rate

``` r
inf  <- Data$Inflation
ffr  <- Data$FFR
```

# Testing for Stationarity

``` r
adf_inf  <- adf.test(Data$Inflation)
adf_ffr  <- adf.test(Data$FFR)
cat("Inflation    - p-value:", round(adf_inf$p.value,  3), "\n")
```

    ## Inflation    - p-value: 0.02

``` r
cat("FFR          - p-value:", round(adf_ffr$p.value,  3), "\n")
```

    ## FFR          - p-value: 0.249

# Making FFR Stationary

``` r
# First difference of FFR
diff_ffr  <- diff(Data$FFR)

# Confirm stationarity of differenced FFR
adf_diff_ffr <- adf.test(diff_ffr)
```

    ## Warning in adf.test(diff_ffr): p-value smaller than printed p-value

``` r
cat("ADF p-value after differencing:", round(adf_diff_ffr$p.value, 3), "\n")
```

    ## ADF p-value after differencing: 0.01

# Obtaining the Standard Deviations

``` r
sd_gdp  <- sd(cycle_gdp)  * 100
sd_cons <- sd(cycle_cons) * 100
sd_debt <- sd(cycle_debt) * 100
sd_inf  <- sd(inf) * 100
sd_ffr  <- sd(diff_ffr) * 100
```

# Obtaining the Correlations

``` r
corr_cons <- cor(cycle_cons, cycle_gdp)
corr_debt <- cor(cycle_debt, cycle_gdp)
corr_inf  <- cor(inf,  cycle_gdp)
corr_ffr  <- cor(diff_ffr,  cycle_gdp[-1])
```

# Build the results table

``` r
results_table <- data.frame(
  Variable = c("Output", "Household Consumption", "Inflation (CPI)", 
               "Public Debt", "Federal Funds Rate"),
  
  Std_Dev = c(round(sd_gdp,  3),
              round(sd_cons, 3),
              round(sd_inf,  3),
              round(sd_debt, 3),
              round(sd_ffr,  3)),
  
  Corr_with_Output = c(1.000,
                       round(corr_cons, 3),
                       round(corr_inf,  3),
                       round(corr_debt, 3),
                       round(corr_ffr,  3))
)
```

# Print results as a clean table

``` r
colnames(results_table) <- c("Variable", "Standard Deviation (%)", 
                              "Correlation with Output")
kable(results_table, align = c("l", "c", "c"))
```

| Variable              | Standard Deviation (%) | Correlation with Output |
|:----------------------|:----------------------:|:-----------------------:|
| Output                |         1.244          |          1.000          |
| Household Consumption |         1.236          |          0.884          |
| Inflation (CPI)       |         0.536          |          0.344          |
| Public Debt           |         2.552          |         -0.356          |
| Federal Funds Rate    |         0.568          |          0.282          |
