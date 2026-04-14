# Purpose

Providing the r code required to obtain the statistics for our
Macroeconomics project.

``` r
rm(list = ls()) # Clean your environment:
gc() # garbage collection - It can be useful to call gc after a large object has been removed, as this may prompt R to return memory to the operating system.
```

    ##           used (Mb) gc trigger (Mb) limit (Mb) max used (Mb)
    ## Ncells  563058 30.1    1250910 66.9         NA   715654 38.3
    ## Vcells 1075000  8.3    8388608 64.0      16384  2010319 15.4

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.2.0     ✔ readr     2.2.0
    ## ✔ forcats   1.0.1     ✔ stringr   1.6.0
    ## ✔ ggplot2   4.0.2     ✔ tibble    3.3.1
    ## ✔ lubridate 1.9.5     ✔ tidyr     1.3.2
    ## ✔ purrr     1.2.1     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
if(!require(readxl)) { install.packages("readxl"); require(readxl)} 
```

    ## Loading required package: readxl

``` r
library(readxl)
if(!require(mFilter)) { install.packages("mFilter"); require(mFilter)} 
```

    ## Loading required package: mFilter

``` r
library(mFilter)
if(!require(knitr)) { install.packages("knitr"); require(knitr)} 
```

    ## Loading required package: knitr

``` r
library(knitr)

list.files('code/', full.names = T, recursive = T) %>% .[grepl('.R', .)] %>% as.list() %>% walk(~source(.))
```

# Loading Data

``` r
Data <- read_excel('/Users/diaz/Documents/Masters/Coursework/First Semester/Macroeconomics/Project/Macro Project/data/Macro_Data.xlsx',
                   sheet = "Logged Data")  # change to your sheet name

head(Data)
```

    ## # A tibble: 6 × 6
    ##   Date                Log_GDP Log_Cons Log_Debt Inflation    FFR
    ##   <dttm>                <dbl>    <dbl>    <dbl>     <dbl>  <dbl>
    ## 1 1982-01-01 00:00:00    3.91     3.45     2.78    0.0758 0.142 
    ## 2 1982-04-01 00:00:00    3.91     3.44     2.78    0.0691 0.145 
    ## 3 1982-07-01 00:00:00    3.90     3.45     2.82    0.0582 0.110 
    ## 4 1982-10-01 00:00:00    3.90     3.46     2.86    0.0444 0.0929
    ## 5 1983-01-01 00:00:00    3.92     3.47     2.89    0.0359 0.0865
    ## 6 1983-04-01 00:00:00    3.93     3.49     2.94    0.0330 0.088

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
hp_inf  <- hpfilter(Data$Inflation, freq = 1600)
hp_ffr  <- hpfilter(Data$FFR,       freq = 1600)
```

# Extracting cyclical components of the data

``` r
cycle_gdp  <- hp_gdp$cycle
cycle_cons <- hp_cons$cycle
cycle_debt <- hp_debt$cycle
cycle_inf  <- hp_inf$cycle
cycle_ffr  <- hp_ffr$cycle
```

# Obtaining the Standard Deviations

``` r
sd_gdp  <- sd(cycle_gdp)  * 100
sd_cons <- sd(cycle_cons) * 100
sd_debt <- sd(cycle_debt) * 100
sd_inf  <- sd(cycle_inf)  * 100
sd_ffr  <- sd(cycle_ffr)  * 100
```

# Obtaining the Correlations

``` r
corr_cons <- cor(cycle_cons, cycle_gdp)
corr_debt <- cor(cycle_debt, cycle_gdp)
corr_inf  <- cor(cycle_inf,  cycle_gdp)
corr_ffr  <- cor(cycle_ffr,  cycle_gdp)
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
| Inflation (CPI)       |         1.135          |          0.425          |
| Public Debt           |         2.552          |         -0.356          |
| Federal Funds Rate    |         1.163          |          0.537          |
