#https://www.r-bloggers.com/dow-jones-stock-market-index-3-4-log-returns-garch-model/
#https://datascienceplus.com/dow-jones-stock-market-index-1-4-log-returns-exploratory-analysis/
suppressPackageStartupMessages(library(lubridate)) # dates manipulation
suppressPackageStartupMessages(library(fBasics)) # enhanced summary statistics
suppressPackageStartupMessages(library(lmtest)) # coefficients significance tests
suppressPackageStartupMessages(library(urca)) # unit rooit test
suppressPackageStartupMessages(library(ggplot2)) # visualization
suppressPackageStartupMessages(library(quantmod)) # getting financial data
suppressPackageStartupMessages(library(PerformanceAnalytics)) # calculating returns
suppressPackageStartupMessages(library(rugarch)) # GARCH modeling
suppressPackageStartupMessages(library(FinTS)) # ARCH test
suppressPackageStartupMessages(library(forecast)) # ARMA modeling
suppressPackageStartupMessages(library(strucchange)) # structural changes
suppressPackageStartupMessages(library(TSA)) # ARMA order identification

install.packages("FinTS", repos="http://R-Forge.R-project.org")
install.packages("fBasics", dependencies = T)
install.packages("rugarch", dependencies = T)



# Packages version are herein listed.

packages <- c("lubridate", "fBasics", "lmtest", "urca", "ggplot2", "quantmod", "PerformanceAnalytics", "rugarch", "FinTS", "forecast", "strucchange", "TSA")
version <- lapply(packages, packageVersion)
version_c <- do.call(c, version)
data.frame(packages=packages, version = as.character(version_c))
R.version

suppressMessages(getSymbols("^DJI", from = "2007-01-01", to = "2019-01-01"))
dim(DJI)
class(DJI)
head(DJI)
tail(DJI)
# More precisely, we have available OHLC (Open, High, Low, Close) index value, adjusted close value and trade volume. Here we can see the corresponding chart as produced by the chartSeries within the quantmod package.
chartSeries(DJI, type = "bars", theme="white")
dj_close <- DJI[,"DJI.Adjusted"]  #We herein analyse the adjusted close value.
# We compute log returns by taking advantage of CalculateReturns within PerformanceAnalytics package.
dj_ret <- CalculateReturns(dj_close, method = "log")
dj_ret <- na.omit(dj_ret)

# Let us have a look.
head(dj_ret)
tail(dj_ret)
plot(dj_ret)
# Sharp increases and decreases in volatility can be eye-balled. That will be in-depth verified in part 3.



##Helper Functions

# We need some helper functions to ease some basic data conversion, summaries and plots. Here below the details.

# 1. Conversion from xts to dataframe with year and value column. That allows for summarizations and plots on year basis.

xts_to_dataframe <- function(data_xts) {
  df_t <- data.frame(year = factor(year(index(data_xts))), value = coredata(data_xts))
  colnames(df_t) <- c( "year", "value")
  df_t
}

# 2. Enhanced summaries statistics for data stored as data frame columns.

bs_rn <- rownames(basicStats(rnorm(10,0,1))) # gathering the basic stats dataframe output row names that get lost with tapply()

dataframe_basicstats <- function(dataset) {
  result <- with(dataset, tapply(value, year, basicStats))
  df_result <- do.call(cbind, result)
  rownames(df_result) <- bs_rn
  as.data.frame(df_result)
}

# 3. Basic statistics dataframe row threshold filtering to return the associated column names.

filter_dj_stats <- function(dataset_basicstats, metricname, threshold) {
  r <- which(rownames(dataset_basicstats) ==  metricname)
  colnames(dataset_basicstats[r, which(dataset_basicstats[r,] > threshold), drop = FALSE])
}

# 4. Box-plots with faceting based on years.

dataframe_boxplot <- function(dataset, title) {
  p <- ggplot(data = dataset, aes(x = year, y = value)) + theme_bw() + theme(legend.position = "none") + geom_boxplot(fill = "lightblue")
  p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle(title) + ylab("year")
  p
}

# 5. Density plots with faceting based on years.

dataframe_densityplot <- function(dataset, title) {
  p <- ggplot(data = dataset, aes(x = value)) + geom_density(fill = "lightblue") 
  p <- p + facet_wrap(. ~ year)
  p <- p + theme_bw() + theme(legend.position = "none") + ggtitle(title)
  p
}

# 6. QQ plots with faceting based on years.

dataframe_qqplot <- function(dataset, title) {
  p <- ggplot(data = dataset, aes(sample = value)) + stat_qq(colour = "blue") + stat_qq_line() 
  p <- p + facet_wrap(. ~ year)
  p <- p + theme_bw() + theme(legend.position = "none") + ggtitle(title)
  p
}

# 7. Shapiro Test

shp_pvalue <- function (v) {
  shapiro.test(v)$p.value
}

dataframe_shapirotest <- function(dataset) {
 result <- with(dataset, tapply(value, year, shp_pvalue))
 as.data.frame(result)
}


##Daily Log-returns Exploratory Analysis

# We transform our original Dow Jones time series into a dataframe with year and value columns. That will ease plots and summaries by year.
dj_ret_df <- xts_to_dataframe(dj_ret)
head(dj_ret_df)
tail(dj_ret_df)

##Basic statistics summary

(dj_stats <- dataframe_basicstats(dj_ret_df))

# In the following, we make specific comments to some relevant aboveshown metrics.
# Mean

# Years when Dow Jones daily log-returns have positive mean values are:

filter_dj_stats(dj_stats, "Mean", 0)
## [1] "2007" "2009" "2010" "2011" "2012" "2013" "2014" "2016" "2017"

# All Dow Jones daily log-returns mean values in ascending order.

dj_stats["Mean",order(dj_stats["Mean",,])]
##           2008      2018   2015     2011     2007    2012     2014
## Mean -0.001633 -0.000231 -9e-05 0.000214 0.000246 0.00028 0.000288
##          2010  2016     2009     2017     2013
## Mean 0.000415 5e-04 0.000684 0.000892 0.000933

# Median

# Years when Dow Jones daily log-returns have positive median are:

filter_dj_stats(dj_stats, "Median", 0)
## [1] "2007" "2009" "2010" "2011" "2013" "2014" "2016" "2017" "2018"

# All Dow Jones daily log-returns median values in ascending order.

dj_stats["Median",order(dj_stats["Median",,])]
##            2008      2015      2012     2017     2010     2018     2014
## Median -0.00089 -0.000211 -0.000122 0.000655 0.000681 0.000695 0.000728
##            2016     2011     2009     2007     2013
## Median 0.000738 0.000941 0.001082 0.001098 0.001158

# Skewness

# A spatial distribution has positive skewness when it has tendency to produce more positive extreme values above rather than negative ones. Negative skew commonly indicates that the tail is on the left side of the distribution, and positive skew indicates that the tail is on the right. Here is a sample picture to explain.

# Years when Dow Jones daily log-returns have positive skewness are:

filter_dj_stats(dj_stats, "Skewness", 0)
## [1] "2008" "2009" "2012"

# All Dow Jones daily log-returns skewness values in ascending order.

dj_stats["Skewness",order(dj_stats["Skewness",,])]
##               2007      2011      2018      2016      2014      2013
## Skewness -0.613828 -0.526083 -0.522618 -0.449311 -0.332766 -0.199407
##               2017      2010      2015     2012    2009     2008
## Skewness -0.189808 -0.174816 -0.127788 0.027235 0.07084 0.224042

# Excess Kurtosis

# The Kurtosis is a measure of the “tailedness” of the probability distribution of a real-valued random variable. The kurtosis of any univariate normal distribution is 3. The excess kurtosis is equal to the kurtosis minus 3 and eases the comparison to the normal distribution. The basicStats function within the fBasics package reports the excess kurtosis. Here is a sample picture to explain.

# Years when Dow Jones daily log-returns have positive excess kurtosis are:

filter_dj_stats(dj_stats, "Kurtosis", 0)
##  [1] "2007" "2008" "2009" "2010" "2011" "2012" "2013" "2014" "2015" "2016"
## [11] "2017" "2018"

# All Dow Jones daily log-returns excess kurtosis values in ascending order.

dj_stats["Kurtosis",order(dj_stats["Kurtosis",,])]
##             2012     2014     2013     2015     2007     2010    2009
## Kurtosis 0.84289 1.073234 1.275821 1.394268 1.525069 2.055407 2.07424
##              2016     2017     2011     2018     2008
## Kurtosis 2.079671 2.244076 2.453822 2.802996 3.670796

# Year 2018 has the closest excess kurtosis to 2008.
# Box-plots
dataframe_boxplot(dj_ret_df, "DJIA daily log-returns box plots 2007-2018")

# We can see how the most extreme values occurred on 2008. Starting on 2009, the values range gets narrow, with the exception of 2011 and 2015. However, comparing 2017 with 2018, it is remarkable an improved tendency to produce more extreme values on last year.
# Density plots
dataframe_densityplot(dj_ret_df, "DJIA daily log-returns density plots 2007-2018")

# Year 2007 has remarkable negative skewness. Year 2008 is characterized by flatness and extreme values. The 2017 peakness is in constrant with the flatness and left skeweness of 2018 results.

# Shapiro Tests
dataframe_shapirotest(dj_ret_df)
# The null hypothesis of normality is rejected for all years 2007-2018.

# QQ plots
dataframe_qqplot(dj_ret_df, "DJIA daily log-returns QQ plots 2007-2018")
# Strong departure from normality of daily log returns (and hence log-normality for discrete returns) is visible on years 2008, 2009, 2010, 2011 and 2018.

##Weekly Log-returns Exploratory Analysis

# The weekly log returns can be computed starting from the daily ones. Let us suppose to analyse the trading week on days {t-4, t-3, t-2, t-1, t} and to know closing price at day t-5 (last day of the previous week). 
# We take a look at Dow Jones weekly log-returns.
dj_weekly_ret <- apply.weekly(dj_ret, sum)
plot(dj_weekly_ret)
# That plot shows sharp increses and decreases of volatility.

# We transform the original time series data and index into a dataframe.
dj_weekly_ret_df <- xts_to_dataframe(dj_weekly_ret)
dim(dj_weekly_ret_df)
## [1] 627   2

head(dj_weekly_ret_df)
tail(dj_weekly_ret_df)

# Basic statistics summary
(dj_stats <- dataframe_basicstats(dj_weekly_ret_df))


# In the following, we make specific comments to some relevant aboveshown metrics.
# Mean

# Years when Dow Jones weekly log-returns have positive mean are:

filter_dj_stats(dj_stats, "Mean", 0)
## [1] "2007" "2009" "2010" "2011" "2012" "2013" "2014" "2016" "2017"

# All mean values in ascending order.
dj_stats["Mean",order(dj_stats["Mean",,])]


# Median
# Years when Dow Jones weekly log-returns have positive median are:
filter_dj_stats(dj_stats, "Median", 0)

# All median values in ascending order.
dj_stats["Median",order(dj_stats["Median",,])]


# Skewness

# Years when Dow Jones weekly log-returns have positive skewness are:
filter_dj_stats(dj_stats, "Skewness", 0)
## [1] "2009" "2017"

# All skewness values in ascending order.
dj_stats["Skewness",order(dj_stats["Skewness",,])]


# Excess Kurtosis

# Years when Dow Jones weekly log-returns have positive excess kurtosis are:
filter_dj_stats(dj_stats, "Kurtosis", 0)
## [1] "2008" "2010" "2011" "2014" "2015" "2016"

# All excess kurtosis values in ascending order.
dj_stats["Kurtosis",order(dj_stats["Kurtosis",,])]
# Year 2008 has also highest weekly kurtosis. However in this scenario, 2017 has negative kurtosis and year 2016 has the second highest kurtosis.

# Box-plots
dataframe_boxplot(dj_weekly_ret_df, "DJIA weekly log-returns box plots 2007-2018")

# Density plots
dataframe_densityplot(dj_weekly_ret_df, "DJIA weekly log-returns density plots 2007-2018")

# Shapiro Tests
dataframe_shapirotest(dj_weekly_ret_df)
# The null hypothesis of normality is rejected for years 2007, 2008, 2016.

# QQ plots
dataframe_qqplot(dj_weekly_ret_df, "DJIA weekly log-returns QQ plots 2007-2018")
# Strong departure from normality is particularly visible on year 2008.

# Saving the current enviroment for further analysis.
save.image(file='DowEnvironment.RData')


### Part 2/4 ###
load(file='DowEnvironment.RData')
# Daily Volume Exploratory Analysis
# From the saved environment, we can find back our DJI object. We plot the daily volume.
dj_vol <- DJI[,"DJI.Volume"]
plot(dj_vol)
# It is remarkable the level jump at the beginning of 2017, something that we will investigate in part 4.
# We transform the volume time series data and timeline index into a dataframe.
dj_vol_df <- xts_to_dataframe(dj_vol)
head(dj_vol_df)
tail(dj_vol_df)
# Basic statistics summary
(dj_stats <- dataframe_basicstats(dj_vol_df))

# In the following, we make specific comments to some relevant above shown metrics.
# Mean

# Years when Dow Jones daily volume has positive mean are:

filter_dj_stats(dj_stats, "Mean", 0)


# All Dow Jones daily volume mean values in ascending order.

dj_stats["Mean",order(dj_stats["Mean",,])]


# Median

# Years when Dow Jones daily volume has positive median are:

filter_dj_stats(dj_stats, "Median", 0)


# All Dow Jones daily volume median values in ascending order.

dj_stats["Median",order(dj_stats["Median",,])]


# Skewness

# Years when Dow Jones daily volume has positive skewness are:

filter_dj_stats(dj_stats, "Skewness", 0)
##  [1] "2007" "2008" "2009" "2010" "2011" "2012" "2013" "2014" "2015" "2016"
## [11] "2017" "2018"

# All Dow Jones daily volume skewness values in ascending order.

dj_stats["Skewness",order(dj_stats["Skewness",,])]


# Excess Kurtosis

# Years when Dow Jones daily volume has positive excess kurtosis are:
filter_dj_stats(dj_stats, "Kurtosis", 0)
# All Dow Jones daily volume excess kurtosis values in ascending order.
dj_stats["Kurtosis",order(dj_stats["Kurtosis",,])]


# Box-plots

dataframe_boxplot(dj_vol_df, "DJIA daily volume box plots 2007-2018")

# The trade volume starts to decrease from 2010 and on 2017 a remarkable increase occurred. Year 2018 volume has been even larger than 2017 and other years as well.

# Density plots

dataframe_densityplot(dj_vol_df, "DJIA daily volume density plots 2007-2018")

# Shapiro Tests

dataframe_shapirotest(dj_vol_df)
# The null hypothesis of normality is rejected for all years.

# QQ plots
dataframe_qqplot(dj_vol_df, "DJIA daily volume QQ plots 2007-2018")
# QQplots visually confirm the non-normality of daily trade volume distribution.

## Daily volume log-ratio Exploratory Analysis

# Similarly to log-returns, we can define the trade volume log ratio as.

# vt :=lnVtVt−1


# We can compute it by CalculateReturns within the PerformanceAnalytics package and plot it.

dj_vol_log_ratio <- CalculateReturns(dj_vol, "log")
dj_vol_log_ratio <- na.omit(dj_vol_log_ratio)
plot(dj_vol_log_ratio)

# Mapping the trade volume log-ratio time series data and timeline index into a dataframe.

dj_vol_df <- xts_to_dataframe(dj_vol_log_ratio)
head(dj_vol_df)


tail(dj_vol_df)


# Basic statistics summary
(dj_stats <- dataframe_basicstats(dj_vol_df))

# In the following, we make specific comments to some relevant above-shown metrics.
# Mean

# Years when Dow Jones daily volume log-ratio has positive mean are:

filter_dj_stats(dj_stats, "Mean", 0)
## [1] "2008" "2011" "2012" "2014" "2015" "2016" "2018"

# All Dow Jones daily volume log-ratio mean values in ascending order.

dj_stats["Mean",order(dj_stats["Mean",,])]


# Median

# Years when Dow Jones daily volume log-ratio has positive median are:

filter_dj_stats(dj_stats, "Median", 0)
## [1] "2008" "2014" "2015" "2018"

# All Dow Jones daily volume log-ratio median values in ascending order.

dj_stats["Median",order(dj_stats["Median",,])]

# Skewness

# Years when Dow Jones daily volume log-ratio has positive skewness are:

filter_dj_stats(dj_stats, "Skewness", 0)
## [1] "2009" "2011" "2016" "2017"

# All Dow Jones daily volume log-ratio mean values in ascending order.

dj_stats["Skewness",order(dj_stats["Skewness",,])]


# Excess Kurtosis

# Years when Dow Jones daily volume has positive excess kurtosis are:

filter_dj_stats(dj_stats, "Kurtosis", 0)

# All Dow Jones daily volume log-ratio excess kurtosis values in ascending order.

dj_stats["Kurtosis",order(dj_stats["Kurtosis",,])]


# Box-plots

dataframe_boxplot(dj_vol_df, "DJIA daily volume box plots 2007-2018")
# The most positive extreme values can be spotted on years 2011, 2014 and 2016. The most negative extreme values, on years 2007, 2011, 2012, 2014.

# Density plots
dataframe_densityplot(dj_vol_df, "DJIA daily volume density plots 2007-2018")

# Shapiro Tests
dataframe_shapirotest(dj_vol_df)
# Based on reported p-values, for all we can reject the null hypothesis of normal distribution.
# QQ plots
dataframe_qqplot(dj_vol_df, "DJIA daily volume QQ plots 2007-2018")
# Departure from normality can be spotted for all reported years.

# Saving the current enviroment for further analysis.
save.image(file='DowEnvironment.RData')


### Dow Jones Stock Market Index (3/4): Log Returns GARCH Model  ###
load(file='DowEnvironment.RData')
# This is the plot of DJIA daily log-returns.
plot(dj_ret)


Outliers Detection

The Return.clean function within Performance Analytics package is able to clean return time series from outliers. Here below we compare the original time series with the outliers adjusted one.

dj_ret_outliersadj <- Return.clean(dj_ret, "boudt")
p <- plot(dj_ret)
p <- addSeries(dj_ret_outliersadj, col = 'blue', on = 1)
p

The prosecution of the analysis will be carried on with the original time series as a more conservative approach to volatility evaluation.
Correlation plots

Here below are the total and partial correlation plots.

acf(dj_ret)

pacf(dj_ret)

Above correlation plots suggest some ARMA(p,q) model with p and q > 0. That will be verified within the prosecution of the present analysis.
Unit root tests

We run the Augmented Dickey-Fuller test as available within the urca package. The no trend and no drift test flavor is run.

(urdftest_lag = floor(12* (nrow(dj_ret)/100)^0.25))
## [1] 28

summary(ur.df(dj_ret, type = "none", lags = urdftest_lag, selectlags="BIC"))


# Based on reported test statistics compared with critical values, we reject the null hypothesis of unit root presence. See ref. [6] for further details about the Augmented Dickey-Fuller test.
# ARMA model
# 
# We now determine the ARMA structure of our time series in order to run the ARCH effects test on the resulting residuals. That is in agreement with what outlined in ref. [4] $4.3.
# 
# ACF and PACF plots tailing off suggests an ARMA(2,2) (ref. [5], table $3.1). We take advantage of auto.arima() function within forecast package (ref. [7]) to have an idea to start with.

auto_model <- auto.arima(dj_ret)
summary(auto_model)
# ARMA(2,4) model is suggested. However, the ma3 coefficient is not significative, as further verified by:
coeftest(auto_model)
# Hence we put as a constraint MA order q <= 2.
auto_model2 <- auto.arima(dj_ret, max.q=2)
summary(auto_model2)
# Now all coefficients are significative.
coeftest(auto_model2)
# Further verifications with ARMA(2,1) and ARMA(1,2) results with higher AIC values than ARMA(2,2). Hence ARMA(2,2) is preferable. Here are the results.

auto_model3 <- auto.arima(dj_ret, max.p=2, max.q=1)
summary(auto_model3)
coeftest(auto_model3)

# All coefficients are statistically significative.

auto_model4 <- auto.arima(dj_ret, max.p=1, max.q=2)
summary(auto_model4)
coeftest(auto_model4)
# All coefficients are statistically significative.

# Furthermore, we investigate what eacf() function within the TSA package reports.
eacf(dj_ret)
# The upper left triangle with “O” as a vertex seems to be located somehow within (p,q) = {(1,2),(2,2),(1,3)}, which represents a set of potential candidate (p,q) values according to eacf function output. To remark that we prefer to consider parsimoniuos models, that is why we do not go too far as AR and MA orders.

# ARMA(1,2) model was already verified. ARMA(2,2) is already a candidate model. Let us verify ARMA(1,3).

(arima_model5 <- arima(dj_ret, order=c(1,0,3), include.mean = FALSE))
coeftest(arima_model5)
# Only one coefficient is statistically significative.

# As a conclusion, we choose ARMA(2,2) as mean model. We can now proceed on with ARCH effect tests.

# ARCH effect test

# Now, we can test if any ARCH effects are present on residuals of our model. If ARCH effects are statistical significative for the residuals of our time series, a GARCH model is required.

model_residuals <- residuals(auto_model2)
ArchTest(model_residuals - mean(model_residuals))
## 
##  ARCH LM-test; Null hypothesis: no ARCH effects
## 
## data:  model_residuals - mean(model_residuals)
## Chi-squared = 986.82, df = 12, p-value < 2.2e-16

# Based on reported p-value, we reject the null hypothesis of no ARCH effects.
# 
# Let us have a look at the residual correlation plots.

par(mfrow=c(1,2))
acf(model_residuals)
pacf(model_residuals)


# Conditional Volatility

# The conditional mean and variance are defined as:

# μt := E(rt| Ft−1)σ2t := Var(rt| Ft−1) = E[(rt−μt)2|Ft−1]

# The conditional volatility can be computed as square root of the conditional variance. See ref. [4] for further details.


# eGARCH Model

# The attempts with sGARCH as variance model did not bring to result with significative coefficients. On the contrary, the exponential GARCH (eGARCH) variance model is capable to capture asymmetries within the volatility shocks.

# To inspect asymmetries within the DJIA log returns, summary statistics and density plot are shown.

basicStats(dj_ret)

# The negative skewness value confirms the presence of asymmetries within the DJIA distribution.

# This gives the density plot.

plot(density(dj_ret))

# We go on proposing as variance model (for conditional variance) the eGARCH model. More precisely, we are about to model an ARMA-GARCH, with ARMA(2,2) as a mean model and exponential GARCH(1,1) as the variance model. Before doing that, we further emphasize how ARMA(0,0) is not satisfactory within this context.

# ARMA-GARCH: ARMA(0,0) + eGARCH(1,1)

garchspec <- ugarchspec(mean.model = list(armaOrder = c(0,0), include.mean = TRUE), 
                        variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
                        distribution.model = "sstd")

(garchfit <- ugarchfit(data = dj_ret, spec = garchspec))
# All coefficients are statistically significative. However, from Weighted Ljung-Box Test on Standardized Residuals reported p-value above (as part of the overall report), we have the confirmation that such model does not capture all the structure (we reject the null hypothesis of no correlation within the residuals).

# As a conclusion, we proceed on by specifying ARMA(2,2) as a mean model within our GARCH fit as hereinbelow shown.
# ARMA-GARCH: ARMA(2,2) + eGARCH(1,1)

garchspec <- ugarchspec(mean.model = list(armaOrder = c(2,2), include.mean = FALSE), 
                        variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
                        distribution.model = "sstd")

(garchfit <- ugarchfit(data = dj_ret, spec = garchspec))

# All coefficients are statistically significative. No correlation within standardized residuals or standardized squared residuals is found. All ARCH effects are properly captured by the model. However:
# 
# * the Nyblom stability test null hypothesis that the model parameters are constant over time is rejected for some of them
# 
# * the Positive Sign Bias null hypothesis is rejected at 5% level of significance; this kind of test focuses on the effect of large and small positive shocks
# 
# * the Adjusted Pearson Goodness-of-fit test null hypothesis that the empirical and theoretical distribution of standardized residuals are identical is rejected

# See ref. [11] for an explanation about GARCH model diagnostic.

# Note: The ARMA(1,2) + eGARCH(1,1) fit also provides with significative coefficients, no correlation within standardized residuals, no correlation within standardized squared residuals and all ARCH effects are properly captured. However the bias tests are less satisfactory at 5% than the ARMA(2,2) + eGARCH(1,1) model ones.

# We show some diagnostic plots further.

par(mfrow=c(2,2))
plot(garchfit, which=8)
plot(garchfit, which=9)
plot(garchfit, which=10)
plot(garchfit, which=11)

# We show the original DJIA log-returns time series with the mean model fit (red line) and the conditional volatility (blue line).

par(mfrow=c(1,1))
cond_volatility <- sigma(garchfit)
mean_model_fit <- fitted(garchfit)
p <- plot(dj_ret, col = "grey")
p <- addSeries(mean_model_fit, col = 'red', on = 1)
p <- addSeries(cond_volatility, col = 'blue', on = 1)
p

# Model Equation
# 
# Combining both ARMA(2,2) and eGARCH models we have:
# 
# ⎧⎪
# ⎪
# ⎪
# ⎪
# ⎪
# ⎪⎨⎪
# ⎪
# ⎪
# ⎪
# ⎪
# ⎪⎩yt − ϕ1yt−1 − ϕ2yt−2= ϕ0 + ut + θ1ut−1 + θ2ut−2ut = σtϵt,     ϵt=N(0,1)ln(σ2t) = ω +∑qj=1(αjϵ2t−j +γ(ϵt−j–E|ϵt−j|))+ ∑pi=1βiln(σ2t−1)
# 
# Using the model resulting coefficients, it results as follows.
# 
# ⎧⎪
# ⎪
# ⎪
# ⎪
# ⎪⎨⎪
# ⎪
# ⎪
# ⎪
# ⎪⎩yt +0.476 yt−1 +0.575 yt−2= ut +0.429 ut−1 +0.563 ut−2ut = σtϵt,     ϵt=N(0,1)ln(σ2t) = −0.313 −0.174ϵ2t−1 +0.189 (ϵt−1–E|ϵt−1|))+ 0.966 ln(σ2t−1)

# Volatility Analysis

# Here is the plot of conditional volatility as resulting from our ARMA(2,2) + eGARCH(1,1) model.

plot(cond_volatility)

# Line plots of conditional volatility by year are shown.

par(mfrow=c(6,2))
pl <- lapply(2007:2018, function(x) { plot(cond_volatility[as.character(x)], main = "DJIA Daily Log returns conditional volatility")})
pl

# Conditional volatility box-plots by year are shown.

par(mfrow=c(1,1))
cond_volatility_df <- xts_to_dataframe(cond_volatility)
dataframe_boxplot(cond_volatility_df, "Dow Jones Daily Log-returns conditional Volatility 2007-2018")

# Afterwards 2008, the daily volatility basically tends to decrease. In the year 2017, the volatility was lower with respect any other year under analysis. Differently, on year 2018, we experienced a remarkable increase of volatility with respect year 2017.

# Saving the current enviroment for further analysis.

save.image(file='DowEnvironment.RData')
