## ----setup, include=FALSE-----------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----eval=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------
## ### For data (retrival):
## #install.packages("readr")
## #install.packages("readxl")
## #install.packages("openxlsx")
## 
## ### For data manipulation:
## #install.packages("dplyr")
## #install.packages("tidyr")
## #install.packages("data.table")
## #install.packages("tidyverse")
## #install.packages("Matrix")
## #install.packages("reshape2")
## 
## ### For time and date manipulaton:
## #install.packages("quantmod")
## #install.packages("xts")
## #install.packages("lubridate")
## #install.packages("scales")
## #install.packages("purrr")
## #install.packages("TSstudio")
## #install.packages("RQuantLib")
## #install.packages("timeDate")
## #install.packages("feasts")
## #install.packages("tsbox")
## #install.packages("tseries")
## 
## ### For the analysis:
## ## (i) For the implementation of the strategies:
## #install.packages("TTR")
## #install.packages("PortfolioAnalytics")
## #install.packages("nlshrink")
## #install.packages("ranger")
## 
## #install.packages("NMOF")
## #install.packages("pbapply")
## #install.packages("tidymodels")
## #install.packages("caret")
## #install.packages("parallel")
## ## (ii) For the evaluation:
## #install.packages("ggplot2")
## #install.packages("htmlTable")
## #install.packages("patchwork")
## #install.packages("gridExtra")
## #install.packages("stargazer")
## #install.packages("skimr")
## #install.packages("kableExtra")
## #install.packages("rpart")
## #install.packages("rpart.plot")


## ----loadlib, echo=T, results='hide', message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------
# For data (retrival):
library(readr)
library(readxl)
library(openxlsx)

# For data manipulation:
library(dplyr)
library(tidyr)
library(data.table)
library(tidyverse)
library(Matrix)
library(reshape2)

# For time and date manipulaton:
library(quantmod)
library(xts)
library(lubridate)
library(scales)
library(purrr)
library(TSstudio)
library(RQuantLib)
library(timeDate)
library(feasts)
library(tsbox)
library(tseries)

# For the analysis:
## (i) For the implementation of the strategies:
library(TTR)
library(PortfolioAnalytics)
library(nlshrink)
library(ranger)

library(NMOF)
library(pbapply)
library(tidymodels)
library(caret)
library(parallel)
## (ii) For the evaluation:
library(ggplot2)
library(htmlTable)
library(patchwork)
library(gridExtra)
library(stargazer)
library(skimr)
library(kableExtra)
library(rpart)
library(rpart.plot)


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
# Read Total Return Indices (RIs) and transform to xts-format:
ETFs <- read.xlsx("C:/Users/Berger/Documents/Studium/Master/5. Semester/Masterarbeit/Daten/ETFs/Data.xlsx", sheet = 1)
Dates <- as.Date(ETFs[,1], "%d.%m.%Y")
ETFs <- xts(ETFs[,-1], order.by = Dates)

# Assign the (short) ETF names to columns:
Names <- scan("C:/Users/Berger/Documents/Studium/Master/5. Semester/Masterarbeit/Daten/ETFs/ETFs Namen.txt", character(), sep = ",")
colnames(ETFs) <- Names

head(ETFs)


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
# Import total costs per annum:
TER.pa <- c(0.5, 0.0945, 0.24, 0.06, 0.39, 0.39, 0.4, 0.39, 0.39, 0.4, 0.39, 0.39, 0.39, 0.39, 0.45, 0.35, 
            0.39, 0.39, 0.18, 0.18, 0.15, 0.15, 0.15, 0.19, 0.14, 0.49, 0.4, 0.7)/100
# Divide by 100 to get non percentage values
names(TER.pa) <- colnames(ETFs)

# Transform from annualized values to monthly costs:
TER.monthly <- sapply(TER.pa, function(x) round((1+x)^(1/12)-1, 7))



## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
## DATA FROM FAMA/FRENCH:
FF <- read.csv(file = "C:/Users/Berger/Documents/Studium/Master/5. Semester/Masterarbeit/Daten/Macro/Fama French Three Factor.CSV")
FF <- xts(FF[,-1], order.by = timeLastDayInMonth(ym(FF$X)))
FF$RF <- FF$RF/100

#*************************************************************************

## DATA FROM FRED:
# Indices with monthly observations:
Macro.M <- read_xls(path = "C:/Users/Berger/Documents/Studium/Master/5. Semester/Masterarbeit/Daten/Macro/Macro Data.xls", sheet = 2)
Macro.M <- xts(Macro.M[,-1], order.by = Macro.M$DATE)

# Indices with quarterly observations:
Macro.Q <- read_xls(path = "C:/Users/Berger/Documents/Studium/Master/5. Semester/Masterarbeit/Daten/Macro/Macro Data.xls", sheet = 3)
Macro.Q <- xts(Macro.Q[,-1], order.by = Macro.Q$DATE)



## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
# Select the variables that need to be lagged:
ad <- c("S&P CoreLogic Case-Shiller U.S. National Home Price Index","Monthly Supply of New Houses in the United States","Producer Price Index by Industry: Food Manufacturing","Producer Price Index by Industry: Pharmaceutical Preparation","Producer Price Index by Industry: Oil and Gas Field Machinery and","Producer Price Index by Industry: Semiconductor and Other Electronic","Producer Price Index by Industry: Total Manufacturing Industries","Producer Price Index by Commodity: All Commodities","Producer Price Index by Commodity: Industrial Commodities")

# Select the length of the lags:
l <- c(2,1,1,1,1,1,1,1,1)

# Lag the specific variables by the indicated length:
for(i in 1:length(ad)){
  Macro.M[,ad[i]] <- Lag(Macro.M[,ad[i]], k = l[i])
}



## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
# Lag the index by one month:
index(Macro.Q) <- index(Macro.Q) %m+% months(1)



## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
# Replace negative values with NAs:
tmp <- ETFs
ETFs <- xts(apply(ETFs, 2, function(x) replace(x, which(x<0), NA)), order.by = Dates)

# Manually check again for non-trading days and replace values with NA:
holidays <- Dates %in% getHolidayList("UnitedStates", from = Dates[1], to = Dates[length(Dates)])
# If there are days on which values are reported, replace them with NA:
ETFs[holidays == T,] <- NA



## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
# Cut all dates before 2007-04-11 (dates before the launch of the oldest ETF):
first <- which(Dates == "2007-04-11")
tmp.nona <- tmp[first:nrow(tmp),]
tmp.na <- ETFs[first:nrow(ETFs),]

# The data set for all further analysis (return calculation, backtests, etc.) start from 2004-11-18 (launch date of the second youngest ETF [when the MSCI World ETF is disregarded]):
start <- which(Dates == "2004-11-18")
ETFs <- ETFs[start:nrow(ETFs),]

# Compare the number of NAs for all ETFs before and after cleaning for holidays:
na.count.nona <- colSums(is.na(tmp.nona))
na.count.nona
na.count.na <- colSums(is.na(tmp.na))
na.count.na <- data.frame(ETFs = names(na.count.na), NAs = na.count.na)

# Create barplot to compare number of NAs per ETF:
ggplot(na.count.na, aes(x=ETFs, y=NAs))+
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "NAs per ETF",
       x = "ETFs",
       y = "Number of NAs") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# For the MSCI World ETF: how many of the days of the NA-days are at the end of the month
length(which(which(is.na(tmp.na[,1])) %in% endpoints(tmp.na)[-1]))



## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
## a) Only consider the last days of the month
ETFs.mon <- ETFs[endpoints(ETFs, on = "months"),]
# Next, let's find all dates where there is no data for all ETFs
NT.days <- index(ETFs.mon[which(rowSums(is.na(ETFs.mon)) == ncol(ETFs.mon)),])
# Find where these dates are in the daily ETF table:
NT.indx <- which(index(ETFs) %in% NT.days)
# Use the observations from the day before
use <- NT.indx-1
# Insert these observations into the monthly table
ETFs.mon[NT.days,] <- ETFs[use,]

# Now let's do the same for the remaining missing values:
na_indices_list <- lapply(1:ncol(ETFs.mon), function(x) {
  # Find the indices (dates) of NAs in the x-th column
  na_indices <- index(ETFs.mon)[is.na(ETFs.mon[, x])]
  return(na_indices)
})
# Find the ETFs where the remaining missing values are
NA.ETFs <- which(sapply(na_indices_list, function(e) !is_empty(e)))
# Save their dates where there are NAs:
for(i in 1:length(NA.ETFs)){
  NA.days <- na_indices_list[[NA.ETFs[i]]]
  NA.indx <- which(index(ETFs[,i]) %in% NA.days)
  use <- NA.indx-1
  ETFs.mon[NA.days,NA.ETFs[i]] <- ETFs[use,NA.ETFs[i]]
}
# One more case-specific adjustment:
ETFs.mon["2008-09-30",15] <- ETFs["2008-09-26",15]

# Now there are no more missing values after the launch of the youngest ETF
sum(is.na(ETFs.mon["2007-04-30:2024-08-26",]))



## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
## b) Compute the returns:
Ret <- ROC(ETFs.mon)
Dates.Ret <- index(Ret)

head(Ret)


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
# Lags with lengths ranging from 1 month up to 12 months
Ret.lag1 <- lag.xts(Ret, k = 1)
Ret.lag2 <- lag.xts(Ret, k = 2)
Ret.lag3 <- lag.xts(Ret, k = 3)
Ret.lag4 <- lag.xts(Ret, k = 4)
Ret.lag5 <- lag.xts(Ret, k = 5)
Ret.lag6 <- lag.xts(Ret, k = 6)
Ret.lag7 <- lag.xts(Ret, k = 7)
Ret.lag8 <- lag.xts(Ret, k = 8)
Ret.lag9 <- lag.xts(Ret, k = 9)
Ret.lag10 <- lag.xts(Ret, k = 10)
Ret.lag11 <- lag.xts(Ret, k = 11)
Ret.lag12 <- lag.xts(Ret, k = 12)



## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
# Cumulative Returns with rolling window ranging from 1 up to 12 months
cum.Ret1 <- rollapply(Ret, width = 1, FUN = function(x) prod(1 + x) - 1, by = 1, fill = NA, align = "right")
cum.Ret2 <- rollapply(Ret, width = 2, FUN = function(x) prod(1 + x) - 1, by = 1, fill = NA, align = "right")
cum.Ret3 <- rollapply(Ret, width = 3, FUN = function(x) prod(1 + x) - 1, by = 1, fill = NA, align = "right")
cum.Ret4 <- rollapply(Ret, width = 4, FUN = function(x) prod(1 + x) - 1, by = 1, fill = NA, align = "right")
cum.Ret5 <- rollapply(Ret, width = 5, FUN = function(x) prod(1 + x) - 1, by = 1, fill = NA, align = "right")
cum.Ret6 <- rollapply(Ret, width = 6, FUN = function(x) prod(1 + x) - 1, by = 1, fill = NA, align = "right")
cum.Ret7 <- rollapply(Ret, width = 7, FUN = function(x) prod(1 + x) - 1, by = 1, fill = NA, align = "right")
cum.Ret8 <- rollapply(Ret, width = 8, FUN = function(x) prod(1 + x) - 1, by = 1, fill = NA, align = "right")
cum.Ret9 <- rollapply(Ret, width = 9, FUN = function(x) prod(1 + x) - 1, by = 1, fill = NA, align = "right")
cum.Ret10 <- rollapply(Ret, width = 10, FUN = function(x) prod(1 + x) - 1, by = 1, fill = NA, align = "right")
cum.Ret11 <- rollapply(Ret, width = 11, FUN = function(x) prod(1 + x) - 1, by = 1, fill = NA, align = "right")
cum.Ret12 <- rollapply(Ret, width = 12, FUN = function(x) prod(1 + x) - 1, by = 1, fill = NA, align = "right")



## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
# Find the rows of the macro table which report the months that also occur in the return table
Macro.M.trans <- Macro.M[which(format(index(Macro.M), "%Y-%m") %in% format(Dates.Ret, "%Y-%m")),]
# Assign the dates of the return table to the macro table
index(Macro.M.trans) <- Dates.Ret
# Merge both tables
merged <- merge(Ret, Macro.M.trans, join = "left")



## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
# First, select the subset of the quarterly macro table:
subs <- which(format(index(Macro.Q), "%Y-%m") %in% format(index(Macro.M.trans), "%Y-%m"))
subs <- c(subs[1]-1, subs)
Macro.Q.subs <- Macro.Q[subs,]

# Next, fill the months between the quarters with the most recent reportings:
tmp <- seq(start(Macro.Q.subs), as.POSIXct(end(Macro.M.trans)), by = "months")
tmp <- xts(, order.by = tmp)
Macro.from.Q.to.M <- na.locf(merge(tmp, Macro.Q.subs, all = T), fromLast = F)[-c(1:3),]
index(Macro.from.Q.to.M) <- index(Macro.M.trans)

# Merge both macro tables:
n1 <- colnames(Macro.M.trans)
n2 <- colnames(Macro.Q)
names <- c(n1,n2)
Macro <- merge.xts(Macro.M.trans, Macro.from.Q.to.M)
colnames(Macro) <- names



## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
# Find rows with NAs:
check.NA <- Macro[!complete.cases(Macro),]
count.NA <- colSums(is.na(check.NA))
count.NA <- data.frame(Macros = names(count.NA), NAs = count.NA)

# Create barplot to compare number of NAs per each macro measure:
ggplot(count.NA, aes(x=Macros, y=NAs))+
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "NAs per Macro Index",
       x = "Macros",
       y = "Number of NAs") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
# Cut NAs in last rows (from 2024-04-30 to the end [i.e. keep all values from 2004-11-30 to 2024-03-29]):
Macro <- Macro["2004-11-30::2024-03-29",]

# For all remaining NAs use the latest reported number:
Macro <- na.locf(Macro, fromLast = TRUE)



## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
# Compute percentage change:
Macro.p <- Macro
for(i in 2:nrow(Macro)){
  for(j in 1:ncol(Macro)){
    Macro.p[i,j] <- (as.numeric(Macro[i,j])-as.numeric(Macro[i-1,j]))/abs(as.numeric(Macro[i-1,j]))
  }
}
names <- colnames(Macro)
names2 <- paste0("% ", names)
Macro <- merge(Macro, Macro.p)
colnames(Macro) <- c(names, names2)
Macro <- Macro[2:nrow(Macro),]

# Check if there are any NAs left:
sum(is.na(Macro))


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
# Also remove the last rows from the return data set (from 2024-04-30 to the end [i.e. keep all values from 2004-12-31 to 2024-03-29]):
Ret <- Ret["2004-12-31::2024-03-29",]
Ret.lag1 <- Ret.lag1["2004-12-31::2024-03-29",]
Ret.lag2 <- Ret.lag2["2004-12-31::2024-03-29",]
Ret.lag3 <- Ret.lag3["2004-12-31::2024-03-29",]
Ret.lag4 <- Ret.lag4["2004-12-31::2024-03-29",]
Ret.lag5 <- Ret.lag5["2004-12-31::2024-03-29",]
Ret.lag6 <- Ret.lag6["2004-12-31::2024-03-29",]
Ret.lag7 <- Ret.lag7["2004-12-31::2024-03-29",]
Ret.lag8 <- Ret.lag8["2004-12-31::2024-03-29",]
Ret.lag9 <- Ret.lag9["2004-12-31::2024-03-29",]
Ret.lag10 <- Ret.lag10["2004-12-31::2024-03-29",]
Ret.lag11 <- Ret.lag11["2004-12-31::2024-03-29",]
Ret.lag12 <- Ret.lag12["2004-12-31::2024-03-29",]

# To not stress the global environment too much let's save all of them in  a list:
Ret.LAG <- list(Ret.lag1,Ret.lag2,Ret.lag3,Ret.lag4,Ret.lag5,Ret.lag6,Ret.lag7,Ret.lag8,Ret.lag9,Ret.lag10,Ret.lag11,Ret.lag12)
rm(Ret.lag1,Ret.lag2,Ret.lag3,Ret.lag4,Ret.lag5,Ret.lag6,Ret.lag7,Ret.lag8,Ret.lag9,Ret.lag10,Ret.lag11,Ret.lag12)

# Assign Lags to colnames:
Ret.LAG <- lapply(c(1:length(Ret.LAG)), function(x){
  colnames(Ret.LAG[[x]]) <- paste0("L", x, " ", colnames(Ret.LAG[[x]]))
  Ret.LAG[[x]]
  })



## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
# Do the same for the cumulative return data sets:
cum.Ret1 <- cum.Ret1["2004-12-31::2024-03-29",]
cum.Ret2 <- cum.Ret2["2004-12-31::2024-03-29",]
cum.Ret3 <- cum.Ret3["2004-12-31::2024-03-29",]
cum.Ret4 <- cum.Ret4["2004-12-31::2024-03-29",]
cum.Ret5 <- cum.Ret5["2004-12-31::2024-03-29",]
cum.Ret6 <- cum.Ret6["2004-12-31::2024-03-29",]
cum.Ret7 <- cum.Ret7["2004-12-31::2024-03-29",]
cum.Ret8 <- cum.Ret8["2004-12-31::2024-03-29",]
cum.Ret9 <- cum.Ret9["2004-12-31::2024-03-29",]
cum.Ret10 <- cum.Ret10["2004-12-31::2024-03-29",]
cum.Ret11 <- cum.Ret11["2004-12-31::2024-03-29",]
cum.Ret12 <- cum.Ret12["2004-12-31::2024-03-29",]

# To not stress the global environment too much let's save all of them in  a list:
CUM.ret <- list(cum.Ret1,cum.Ret2,cum.Ret3,cum.Ret4,cum.Ret5,cum.Ret6,cum.Ret7,cum.Ret8,cum.Ret9,cum.Ret10,cum.Ret11,cum.Ret12)
rm(cum.Ret1,cum.Ret2,cum.Ret3,cum.Ret4,cum.Ret5,cum.Ret6,cum.Ret7,cum.Ret8,cum.Ret9,cum.Ret10,cum.Ret11,cum.Ret12)

# Assign rolling window to colnames:
CUM.ret <- lapply(c(1:length(CUM.ret)), function(x){
  colnames(CUM.ret[[x]]) <- paste0("CUM", x, " ", colnames(CUM.ret[[x]]))
  CUM.ret[[x]]
  })



## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
# Check if **all** dates from the significant tables are the same:
all(index(Macro) == index(Ret))



## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
# Use the correct subset:
FF <-  FF["2004-12-31::2024-03-31",]

# Show that there aren't in fact any missing values for the FF data:
sum(is.na(FF))

# Compare dimensions:
nrow(FF)
nrow(Macro)



## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
# Find the months where the (last) days (of the month) aren't the same:
tmp <- index(Macro) %in% as_date(index(FF))
names(tmp) <- index(Macro)
which(tmp == F)

# Check if all months are the same:
all(format(index(Macro), "%Y-%m") == format(as_date(index(FF)), "%Y-%m"))

# Assign the correct days to the FF table:
index(FF) <- index(Macro)



## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
# Merge both tables and once more assign the correct variable names to the columns:
n1 <- colnames(Macro)
n2 <- colnames(FF)
names <- c(n1,n2)
Macro <- merge(Macro, FF)
colnames(Macro) <- names



## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
# Non time-series specific overview:
Ret.overview <- skim(Ret)

# Add annualized return, standard deviation, Sharpe Ratio and Value at Risk as well as the monthly Maximum Drawdown:
RF.ann <- (1+mean(Macro$RF))^12-1
Ret.overview <- Ret.overview %>%
  mutate(Ret.ann = (1+numeric.mean)^12-1) %>%
  mutate(SD.ann = numeric.sd * sqrt(12)) %>%
  mutate(SR.ann = (Ret.ann-RF.ann)/SD.ann) %>%
  mutate(VaR.ann = abs(as.numeric(VaR(Ret, p = 0.95, method = "historical")))*sqrt(12)) %>%
  mutate(Max.Draw = as.numeric(maxDrawdown(Ret)))

Ret.overview



## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
## For each variable show the min and max values:
cols <- c("numeric.mean", "numeric.sd", "numeric.p0", "numeric.p25", "numeric.p50", "numeric.p75", "numeric.p100" ,"Ret.ann", "SD.ann", "SR.ann", "VaR.ann", "Max.Draw")
Base.stats.ETFs.min.max <- Ret.overview %>%
  select(skim_variable, all_of(cols)) %>%
  pivot_longer(cols = -skim_variable, names_to = "metric", values_to = "value") %>%
  group_by(metric) %>%
  summarise(
    min_value = min(value, na.rm = TRUE),
    max_value = max(value, na.rm = TRUE),
    min_ETF = skim_variable[value == min_value],
    max_ETF = skim_variable[value == max_value])

## Find the months of the largest and smallest monthly losses (p0):
# Largest loss
print(c("largest p0", as.character(index(Ret[which.min(Ret[,as.character(Ret.overview[which.min(Ret.overview$numeric.p0),2])]),as.character(Ret.overview[which.min(Ret.overview$numeric.p0),2])]))))
# Smallest loss
print(c("smallest p0", as.character(index(Ret[which.min(Ret[,as.character(Ret.overview[which.max(Ret.overview$numeric.p0),2])]),as.character(Ret.overview[which.min(Ret.overview$numeric.p0),2])]))))

## Find the months of the largest and smallest monthly wins (p100):
# Largest win
print(c("largest p100", as.character(index(Ret[which.max(Ret[,as.character(Ret.overview[which.max(Ret.overview$numeric.p100),2])]),as.character(Ret.overview[which.max(Ret.overview$numeric.p100),2])]))))
# Smallest win
print(c("smallest p100", as.character(index(Ret[which.max(Ret[,as.character(Ret.overview[which.min(Ret.overview$numeric.p100),2])]),as.character(Ret.overview[which.max(Ret.overview$numeric.p100),2])]))))

## Find time frame of Maximum Drawdown of Financials ETF:
table.Drawdowns(Ret$Financials)

## Look for outliers via the Z-score:
outliers_zscore <- function(column, threshold = 3) {
  z_scores <- scale(column, center = TRUE, scale = TRUE)
  outliers <- column[abs(z_scores) > threshold]
  return(outliers)
}
apply(na.omit(Ret), 2, outliers_zscore)

Base.stats.ETFs.min.max



## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
# Non time-series specific:
Macro.overview <- skim(Macro)

# Look for outliers via the Z-score:
head(apply(Macro, 2, outliers_zscore)[sapply(apply(Macro, 2, outliers_zscore), length) > 0])
# Only display variables for which outliers could be found


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
# Visualize the cross-correlations between the ETF returns:
corrplot::corrplot(cor(na.omit(Ret)))


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
# Visualize the cross-correlations between the Macros:
corrplot::corrplot(cor(na.omit(Macro)), tl.pos = "n", cl.pos = "n")



## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
# Remove various Producer Price Indices and nominal GDP:
remove <- which(colnames(Macro) %in% c("Producer Price Index by Industry: Food Manufacturing", 
                                     "Producer Price Index by Industry: Pharmaceutical Preparation",
                                     "Producer Price Index by Industry: Oil and Gas Field Machinery and",
                                     "Producer Price Index by Industry: Total Manufacturing Industries",
                                     "Producer Price Index by Commodity: Industrial Commodities",
                                     "Gross Domestic Product"))
Macro <- Macro[,-remove]



## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
# 1. Check if time series is stationary (computes p-values of Augmented Dickey-Fuller Test):
rbind(do.call(cbind,lapply(c(1:ncol(Ret)), function(x){adf.test(na.omit(Ret[,x]))}))[4,])
rbind(do.call(cbind,lapply(c(1:ncol(ETFs.mon)), function(x){adf.test(na.omit(ETFs.mon[,x]))}))[4,])

# 2. Time-series specific overview:
#ts_summary(Ret)
TSret.overview <- tsfeatures::tsfeatures(Ret)
rownames(TSret.overview) <- colnames(Ret)
head(TSret.overview)

# 3. Compare means of one-month and ten-month autocorrelation:
paste0("mean e_acf1: ", round(mean(TSret.overview$e_acf1), 4), ", mean e_acf10: ", round(mean(TSret.overview$e_acf10), 4))
paste0("mean x_acf1: ", round(mean(TSret.overview$x_acf1), 4), ",mean x_acf10: ", round(mean(TSret.overview$x_acf10), 4))



## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
# Create a line plot for the raw monthly returns of the MSCI World ETF
dates <- index(Ret)
plot.ret <- ggplot(Ret) +
  geom_line(aes(x = dates, y = `MSCI World`, color = "Returns")) +
  scale_x_date(labels = date_format("%d-%m-%Y")) +
  scale_color_manual(" ", values = c("Returns" = "skyblue"))+
  labs(x = "Dates", y = "MSCI World ETF Returns")

# Compute the rolling Z-score (over the last 5 months) of the MSCI World
tmp <- as.data.frame(Ret) %>% 
  mutate(mean = rollapplyr(`MSCI World`, 5, mean, partial = T),
         sd = rollapplyr(`MSCI World`, 5, sd, partial = T)) %>% 
  mutate(rolling_Z = (`MSCI World` - mean) / sd)

plot.Z <- ggplot(tmp) +
  geom_line(aes(x = index(Ret), y = `rolling_Z`, color = "Z-Score")) +
  scale_x_date(labels = date_format("%d-%m-%Y")) +
  scale_color_manual(" ", values = c("Z-Score" = "blue"))+
  labs(x = "Dates", y = "MSCI World ETF Z-Score")

# Arrange plots
plot.ret + plot.Z + plot_layout(ncol=1, guides = "collect")

# Look at the Z-Score time series charactersitics
head(tsfeatures::tsfeatures(tmp))



## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
# Create a line plot for the returns of the MSCI World ETF
dates <- index(ETFs.mon)
ggplot(ETFs.mon) +
  geom_line(aes(x = dates, y = `MSCI World`, color = "Value")) +
  scale_x_date(labels = date_format("%d-%m-%Y")) +
  scale_color_manual(" ", values = c("Value" = "skyblue"))+
  labs(x = "Dates", y = "MSCI World ETF Value")

# Compute the time series summary of the Total Return Indices of all ETFs:
TStotalri.overview <- tsfeatures::tsfeatures(ETFs.mon)
rownames(TStotalri.overview) <- colnames(ETFs.mon)
TStotalri.overview



## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
# 1. Check if time series is stationary (computes p-values of Augmented Dickey-Fuller Test):
rbind(do.call(cbind,lapply(c(1:ncol(Macro)), function(x){adf.test(na.omit(Macro[,x]))}))[4,])

# 2. Time-series specific overview:
head(ts_summary(Macro))
TSmacro.overview <- tsfeatures::tsfeatures(Macro)
rownames(TSmacro.overview) <- colnames(Macro)
head(TSmacro.overview)

# 3. Compare means of one-month and ten-month autocorrelation:
paste0("mean e_acf1: ", round(mean(TSmacro.overview$e_acf1), 4), ",mean e_acf10: ", round(mean(TSmacro.overview$e_acf10), 4))
paste0("mean x_acf1: ", round(mean(TSmacro.overview$x_acf1), 4), ",mean x_acf10: ", round(mean(TSmacro.overview$x_acf10), 4))
paste0("mean diff1_acf1: ", round(mean(TSmacro.overview$diff1_acf1), 4), ",mean diff1_acf10: ", round(mean(TSmacro.overview$diff1_acf10), 4))
paste0("mean diff2_acf1: ", round(mean(TSmacro.overview$diff2_acf1), 4), ",mean diff2_acf10: ", round(mean(TSmacro.overview$diff2_acf10), 4))



## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
# Create the distributional density plot of a subset consisting of representative equity ETF returns:
p1 <- ggplot(Ret) +
  geom_density(aes(x=`MSCI World`, fill = "MSCI World"), alpha = 0.4) +
  geom_density(aes(x=Semiconductor, fill = "Semiconductor"), alpha = 0.4) +
  geom_density(aes(x=Telecom, fill = "Telecom"), alpha = 0.4) +
  geom_density(aes(x=Financials, fill = "Financials"), alpha = 0.4) +
  geom_density(aes(x=Energy, fill = "Energy"), alpha = 0.4) +
  geom_density(aes(x=`Real Estate`, fill = "Real Estate"), alpha = 0.4) +
  scale_fill_manual(" ", values = c("MSCI World" = "blue", "Semiconductor" = "red", "Telecom" = "green", "Financials" = "purple", "Energy" = "grey", "Real Estate" = "yellow")) +
  labs(title = "Equity", x = "Returns", y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5, face ="bold"))

# Create the distributional density plot of the bond ETF returns:
p2 <- ggplot(Ret) +
  geom_density(aes(x=`Y1-3 Treasury Bond`, fill = "Y1-3 Treasury"), alpha = 0.4) +
  geom_density(aes(x=`Y7-10 Treasury Bond`, fill = "Y7-10 Treasury"), alpha = 0.4) +
  geom_density(aes(x=`Y20+ Treasury Bond`, fill = "Y20+ Treasury"), alpha = 0.4) +
  geom_density(aes(x=`Inv Grade Corporate Bond`, fill = "Inv Grade Corporate"), alpha = 0.4) +
  geom_density(aes(x=`High Yield Corporate Bond`, fill = "High Yield Corporate"), alpha = 0.4) +
  geom_density(aes(x=`Tips Bond (Inflation)`, fill = "Tips Bond (Inflation)"), alpha = 0.4) +
  scale_fill_manual(" ", values = c("Y1-3 Treasury" = "blue", "Y7-10 Treasury" = "red", "Y20+ Treasury" = "green", "Inv Grade Corporate" = "purple", "High Yield Corporate" = "grey", "Tips Bond (Inflation)" = "yellow")) +
  labs(title = "Bonds", x = "Returns", y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5, face ="bold"))

# Create the distributional density plot of the Gold ETF returns:
p3 <- ggplot(Ret) +
  geom_density(aes(x=`Gold Shares`, fill = "Gold"), alpha = 0.4) +
  geom_vline(aes(xintercept = mean(Ret$`Gold Shares`, na.rm = T)), color = "black", linetype = "dashed", size = 0.5) +
  annotate("text", x = mean(Ret$`Gold Shares`, na.rm = T), y = 0.02, label = paste("Mean =", round(mean(Ret$`Gold Shares`, na.rm = T), 2)), color = "black", vjust = -0.5, size = 3) +
  scale_fill_manual(" ", values = c("Gold" = "yellow")) +
  labs(title = "Gold Shares", x = "Returns", y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5, face ="bold"))

# Combine all plots in one:
plot.1 <- p1 + p2 + p3 & theme(legend.position = "bottom")
plot.1

# Compute monthly mean, skewness and excess kurtosis:
stats <- rbind(colMeans(na.omit(Ret)),skewness(Ret), kurtosis(Ret) - 3)
rownames(stats)[1] = "Mean"
stats



## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
ret <- Ret[,-1] # Remove MSCI World
bench <- Ret[,"MSCI World"] # MSCI World as benchmark
macro <- Macro[,-c(29:62)]
# Remove percentage changes to reduce risk of overfitting by reducing number of (time-series variables) and redundant information
rf <- macro$RF
lag <- 4*12
horizon <- 12
lookback <- 5*12
train.perc <- 0.5
limit <- 7*12
trans.Z <- FALSE
last <- 5
split <- round((lag+ncol(macro))/3,0) # set to 1/3 of all features
clusters <- parallel::detectCores()
tune_grid <- expand.grid(
  mtry = c(split-10, split-5, split, split+5, split+10),
  splitrule = "variance",
  min.node.size = c(1, 5, 10, 20)
)
# Alternatively:
tune_grid <- NULL
train_control <- trainControl(
  method = "cv",
  number = 5,
  search = "grid"
)
# Alternatively:
train_control <- trainControl(method = "none")
n.trees <- 1000
trans.costs <- c(10,20,30,40,50)/10000
fix.tc <- 0.01
VaR.CI <- 0.95
use.long.only <- 0.25
use.long.short <- 0.25
disc.positive <- FALSE
lend <- 1
use.minvar <- 0.25
min.max <- c(0,1) # Or "rank"
asset.classes <- list(equity = c("MSCI World","S&P 500 TRUST","S&P Midcap 400","S&P Smallcap","Technology","Financials","Telecom",
                                 "Basic Materials","Consumer Discretionary","Consumer Staples","Energy","Healthcare",
                                 "Industrials","Utilities","Biotechnology","Semiconductor","Transportation","S&P 500 Growth",
                                 "S&P 500 Value","Emerging Markets"),
                      real.estate = c("Real Estate"),
                      bonds = c("Y1-3 Treasury Bond","Y7-10 Treasury Bond","Y20+ Treasury Bond","Tips Bond (Inflation)",
                                "Inv Grade Corporate Bond","High Yield Corporate Bond"),
                      commodities = c("Gold Shares"))
init <- 1



## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
`%notin%` <- Negate(`%in%`)


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
source("C:/Users/Berger/Documents/Studium/Master/5. Semester/Masterarbeit/Code/linshrink_cov2.R")


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
Z.TRANS <- function(ret, Names, last){
  
  ret.Z <- as.data.frame(ret) %>%
    # Compute mean
    mutate(across(everything(), ~ rollapplyr(.x, last, mean, partial = TRUE), .names = "mean_{.col}")) %>%
    # Compute SD
    mutate(across(everything(), ~ rollapplyr(.x, last, sd, partial = TRUE), .names = "sd_{.col}"))
  
  # Compute Z-Score
  ret.Z <- xts(do.call(cbind, lapply(1:ncol(ret), function(x){
    z_score <- (ret.Z[,x] - ret.Z[,(x+ncol(ret))]) / ret.Z[,(x+ncol(ret)*2)]
    return(z_score)
    })), 
    order.by = index(ret)
    )
  
  # Assign names
  colnames(ret.Z) <- Names[-1]
  ret.Z <- ret.Z[-c(1:2),]
  
  return(ret.Z)
}


## ----Prevailing Mean, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------
PREVAILING.MEAN <- function(ret, horizon, train.rows, limit = NA, counter = 1, mean.ret.mon.all, mean.ret.cum.all, y_test){
  
  # Create list to store all results:
  if(counter == 1){
    mean.ret.mon.all <- matrix(data = NA, nrow = nrow(y_test), ncol = ncol(ret))
    colnames(mean.ret.mon.all) <- colnames(ret)
  }
  
  # Adjust horizon if it's the last iteration:
  if((counter+horizon-1) > nrow(y_test)){
    horizon <- nrow(y_test) - counter + 1
  }
  
  # Adjust training data set accordingly to the training frame:
  if(is.na(limit)){
    tmp <- ret[train.rows,]
  }
  if(!is.na(limit) && limit > length(train.rows)){
    tmp <- ret[train.rows,]
  }
  if(!is.na(limit) && limit < length(train.rows)){
    tmp <- ret[train.rows[(length(train.rows) - limit + 1):length(train.rows)],]
  }
  
  # Compute mean monthly return:
  res.mon <- lapply(1:ncol(tmp), function(x){
    mean.ret.mon <- mean(tmp[,x], na.rm = T)
    return(mean.ret.mon)
  })
  res.mon <- do.call(cbind, res.mon)
  
  # Compute cumulative return over the horizon:
  n <- nrow(tmp)
  group_indices <- rep(1:ceiling(n / horizon), each = horizon)[1:n]
  tmp$group <- group_indices
  mean.ret.cum <- aggregate(. ~ group, data=tmp, FUN = function(x) prod(1 + x) - 1)
  mean.ret.cum <- mean.ret.cum[, -which(names(mean.ret.cum) == "group")]
  mean.ret.cum <- colMeans(mean.ret.cum)
  
  # Store all the results
  for(j in 1:ncol(mean.ret.mon.all)){
    mean.ret.mon.all[counter:(counter + horizon-1),j] <- res.mon[j]
  }
  mean.ret.cum.all <- rbind(mean.ret.cum.all, mean.ret.cum)
  
  # Update the index
  train.rows <- c(train.rows, c(train.rows[length(train.rows)]+c(1:horizon)))
  counter <- counter + horizon
  
  return(list(mean.ret.mon.all = mean.ret.mon.all, 
              mean.ret.cum.all = mean.ret.cum.all, 
              train.rows = train.rows, 
              counter = counter))
}


## ----Tree Example, warning=FALSE----------------------------------------------------------------------------------------------------------------------------------
set.seed(42) # Reproducible results

# Generate simulation data
n <- 100
x <- runif(n, 0, 10)
y <- 3 * x + rnorm(n, mean = 0, sd = 2)

# Create a data frame
data <- data.frame(x = x, y = y)

# Fit a regression tree
tree_model <- rpart(y ~ x, data = data)

# Print example tree
rpart.plot(tree_model)

set.seed(NULL) # Remove seed


## ----Transform Macro Data, warning=FALSE--------------------------------------------------------------------------------------------------------------------------
TRANS.MACRO <- function(data, lookback){
  
  macro.ema <- lapply(data, function(x){
    ema_col <- rep(NA, length(x))
    
    for (i in seq_along(x)){
      # If not enough observations are available (to match *lookback*) use all remaining observations
      n_periods <- min(i, lookback)
      # Compute EMA
      ema_col[i] <- EMA(x[1:i], n = n_periods)[i]
      }
    
    return(ema_col)
    
    })
  # Transform list to xts data frame
  macro.ema <- do.call(cbind, macro.ema)
  macro.ema <- xts(macro.ema, order.by = index(macro))
  
  return(macro.ema)
  
}


## ----Training Rows, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------
Train.Rows <- function(ret, train.perc){
  c(1:round(train.perc*nrow(ret), 0))
}


## ----Test Data Set, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------
Test_DS <- function(ret, train.rows, horizon){
  
  # Actual monthly test values (returns)
  y_test <- ret[-train.rows,]
  
  # Actual cumulative test values (returns) over the investment horizon
  tmp <- y_test
  n <- nrow(tmp)
  group_indices <- rep(1:ceiling(n / horizon), each = horizon)[1:n]
  tmp$group <- group_indices
  # Compute cumulative return
  y_test.cum <- aggregate(. ~ group, data=tmp, FUN = function(x) prod(1 + x) - 1)
  # Remove the 'group' column
  y_test.cum <- y_test.cum[, -which(names(y_test.cum) == "group")]
  
  return(list(y_test = y_test,
              y_test.cum = y_test.cum))
}


## ----Training Data Set, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------
Train_DS <- function(ret, macro.ema, train.rows, lag, limit=NA){
  
  # If a limit is set we first need to check whether this limit is breached
  if(!is.na(limit) && limit < length(train.rows)){
    train.rows.tmp <- train.rows[(length(train.rows) - limit + 1):length(train.rows)]
  }
  if(!is.na(limit) && limit > length(train.rows)){
    train.rows.tmp <- train.rows
  }
  # If no limit is set do nothing
  if(is.na(limit)){
    train.rows.tmp <- train.rows
  }
  
  # For the last iteration
  if(!is.na(limit) && max(train.rows) >= nrow(ret)){
    train.rows.tmp <- c(min(train.rows):nrow(ret))[(length(train.rows) - limit + 1):length(train.rows)]
  }
  if(is.na(limit) && max(train.rows) >= nrow(ret)){
    train.rows.tmp <- c(min(train.rows):nrow(ret))
  }
  
  train <- ret[train.rows.tmp,]
  train.macro.ema.compl <- macro.ema[train.rows.tmp[-c(1:(lag-1))]]
  # -c(1:(lag-1)) is necessary so the indices are aligned correctly later when merging with x_train
  # --> Macro information from t is used to predict t+1
  # --> Specifically speaking the command "embed" shrinks down the training data set from N to N-lag
  # --> To insure a consistent training data set the above command is executed
  train.macro.ema <- train.macro.ema.compl[-nrow(train.macro.ema.compl),]
  # Cut last row because it is the month which needs to be predicted (i.e. t+1 and NOT t)
  
  ret.lagged <- lapply(c(1:ncol(ret)), function(x){
    embed(train[,x], lag + 1)
    # +1 is necessary because the LAST column is always lag == 0
  })
  
  ret.lagged <- lapply(ret.lagged, function(x){ 
    colnames(x) <- paste0("L", lag:0)
    return(x)
  })
  
  # We want to estimate the return in *lag* months (i.e. column 1 == response)
  y_train <- lapply(ret.lagged, function(x){x[,1]})
  # use all other (lagged) time series variables as features
  x_train <- lapply(ret.lagged, function(x){x[,-1]})
  # Also use macro information and store data with dates and as matrix
  x_train <- lapply(x_train, function(x){
    as.matrix(merge.xts(x, train.macro.ema))
  })
  
  # Test features (last available observation in our window) for our predictions over the next *horizon* months
  # Note: The last row can't just be selected by itself because otherwise we wouldn't consider all data correctly and the index would shift by one
  x_test <- lapply(1:length(ret.lagged), function(x){
    tmp <- c(ret.lagged[[x]][nrow(ret.lagged[[x]]),-ncol(ret.lagged[[x]])], train.macro.ema.compl[nrow(train.macro.ema.compl),])
    # Remove last column from ret.lagged because otherwise we would have one time-series observation too many
    tmp <- matrix(tmp, nrow = 1)
    colnames(tmp) <- colnames(x_train[[1]])
    return(tmp)
    })
  
  return(list(x_train,
              y_train,
              x_test,
              train))
  
}


## ----Random Forest, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------
RANDOM_FOREST <- function(input, horizon, y_test, counter = 1, old.forecasts, cum.perf, tune_grid = NULL, train_control, clusters = NULL, limit, n.trees = 500){
  
  x_train <- input[[1]]
  y_train <- input[[2]]
  x_test <- input[[3]]
  train <- input[[4]]
  y_test <- y_test$y_test
  
  # Check if horizon is set correctly (i.e. if we are in the last iteration)
  if((counter+horizon-1) > nrow(y_test)){
    horizon <- nrow(y_test) - counter + 1
  }
  
  # Create lists to save the forecasts
  if(counter == 1){
    old.forecasts <- lapply(1:length(x_train), function(x){
      return(array(NA,dim = nrow(y_test)))
    })
    old.forecasts <- lapply(old.forecasts, function(x){
      names(x) <- index(y_test)
      return(x)
    })
    cum.perf <- lapply(1:length(x_train), function(x){
      return(NULL)
    })
  }
  forecasts_rf <- lapply(1:length(x_train), function(x){
    return(array(0,dim = horizon))
  })
  
  print(index(y_test[counter]))
  
  # If wished, create clusters for faster computation
  if(!is.null(clusters)){
    cl <- makeCluster(clusters-1) # Leave one core on leave
    
    # Necessary variables need to be exported to the "workers" (i.e. cores):
    clusterExport(cl, varlist = c("x_train", "y_train", "x_test","train_control", "tune_grid", "forecasts_rf", 
                                  "old.forecasts", "counter", "horizon", "limit","n.trees"),
                  envir = environment())
  }
  if(is.null(clusters)){
    cl <- NULL
  }
  
  # Random Forest prediction
  res <- pblapply(c(1:length(x_train)), function(x){
    
    # Check if any ETF still contains NAs and ignore them in the following
    if(!is.na(limit) && any(is.na(x_train[[x]]))){
      return(list(old.forecasts = old.forecasts[[x]], forecasts_rf = forecasts_rf[[x]]))
    }
    
    x_train_temp <- x_train[[x]]
    y_train_temp <- y_train[[x]]
    
    # Here the RF return prediction for the next holding period starts
    for(i in 1:horizon){
      
      train_data <- data.frame(y = y_train_temp, x_train_temp)
      
      # fit the model
      if(!is.na(limit)){
        fit_rf <- caret::train(
          y ~ ., 
          data = train_data,
          method = "ranger",          # Use the RF function from the package ranger
          metric = "RMSE",            # Use appropriate metric for regressions
          trControl = train_control,
          tuneGrid = tune_grid,
          num.trees = n.trees         # Number of trees per model
        )
      }
      if(is.na(limit)){
        fit_rf <- caret::train(
          y ~ ., 
          data = train_data,
          method = "ranger",          # Use the RF function from the package ranger
          metric = "RMSE",            # Use appropriate metric for regressions
          trControl = train_control,
          tuneGrid = tune_grid,
          num.trees = n.trees,        # Number of trees per model
          na.action = na.omit
        )
      }
      # predict using the test set
      forecasts_rf[[x]][i] <- predict(fit_rf, x_test[[x]])
      # here is where we repeatedly reshape the training data to reflect the time distance
      # corresponding to the current forecast horizon.
      # i.e. per iteration we shift the prediction horizon by one month
      y_train_temp <- y_train_temp[-1]
      x_train_temp <- x_train_temp[-nrow(x_train_temp), ]
    }
    
    # Store the results in the forecast vector:
    old.forecasts[[x]][counter:(counter+horizon-1)] <- forecasts_rf[[x]]
    return(list(old.forecasts = old.forecasts[[x]], forecasts_rf = forecasts_rf[[x]]))
    
  }, cl = cl) # Pass over the clusters
  
  # Stop clusters to keep CPU free
  if(!is.null(clusters)){
    stopCluster(cl)
  }
  
  # Extract the results
  old.forecasts <- lapply(res, `[[`, "old.forecasts")
  names(old.forecasts) <- colnames(y_test)
  
  forecasts_rf <- lapply(res, `[[`, "forecasts_rf")
  
  # Save the cumulative performance
  cum.perf <- lapply(1:length(forecasts_rf), function(x){
    c(cum.perf[[x]], prod(1+forecasts_rf[[x]], na.rm = T)-1)
  })
  names(cum.perf) <- colnames(y_test)
  
  # Update the indices
  train.rows <- c(train.rows, c(train.rows[length(train.rows)]+c(1:horizon)))
  counter <- counter + horizon
  
  return(list(old.forecasts = old.forecasts,
              forecasts_rf = forecasts_rf,
              cum.perf = cum.perf,
              train.rows = train.rows, 
              counter = counter))
  
}


## ----Turnover, warning=FALSE--------------------------------------------------------------------------------------------------------------------------------------
TURNOVER <- function(res.costs, ret.portf, weights, remPos, disc, newPos, old.portf){
  
  #print("Turnover")
  
  names(weights) <- colnames(ret.portf)
  
  ## CASE 1: There are new ETFs and some old ETFs got discarded
  # --> Compute TC for ETFs that we sold, as well as for ETFs that we newly bought
  # --> Also compute TC for ETFs that stayed in the portfolio but whose weights changed
  if(!identical(newPos, character(0)) && !identical(disc, character(0))){
    
    res.costs[2:length(res.costs)] <- lapply(2:length(res.costs), function(x){
      
      weights.disc <- res.costs[[x]]$`Weights towards the end of the horizon`[[length(res.costs[[x]]$`Weights towards the end of the horizon`)-1]][disc]
      weights.rem.old <- res.costs[[x]]$`Weights towards the end of the horizon`[[length(res.costs[[x]]$`Weights towards the end of the horizon`)-1]][remPos]
      res.costs[[x]]$Turnover <- res.costs[[x]]$Turnover + sum(abs(weights[remPos] - weights.rem.old)) + as.numeric(sum(abs(weights[newPos]))) + as.numeric(sum(abs(weights.disc)))
      
      return(res.costs[[x]])
    })
    # Turnover of no costs need to be considered separately because the old weights are assigned to another point in time
    # --> index to extract old weights shifted by +1 for weights of portf with no cost consideration
    weights.disc <- res.costs[[1]]$`Weights towards the end of the horizon`[[length(res.costs[[1]]$`Weights towards the end of the horizon`)]][disc]
    weights.rem.old <- res.costs[[1]]$`Weights towards the end of the horizon`[[length(res.costs[[1]]$`Weights towards the end of the horizon`)]][remPos]
    res.costs[[1]]$Turnover <- res.costs[[1]]$Turnover + sum(abs(weights[remPos] - weights.rem.old)) + as.numeric(sum(abs(weights[newPos]))) + as.numeric(sum(abs(weights.disc)))
  }
  
  ## CASE 2: There are new ETFs but no ETFs got discarded (sold)
  # --> Compute TC for weight changes
  # --> Also compute TC for ETFs that got bought additionally
  if(!identical(newPos, character(0)) && identical(disc, character(0))){
    
    res.costs[2:length(res.costs)] <- lapply(2:length(res.costs), function(x){
      
      weights.rem.old <- res.costs[[x]]$`Weights towards the end of the horizon`[[length(res.costs[[x]]$`Weights towards the end of the horizon`)-1]][remPos]
      res.costs[[x]]$Turnover <- res.costs[[x]]$Turnover + sum(abs(weights[remPos] - weights.rem.old)) + as.numeric(sum(abs(weights[newPos])))
      
      return(res.costs[[x]])
    })
    weights.rem.old <- res.costs[[1]]$`Weights towards the end of the horizon`[[length(res.costs[[1]]$`Weights towards the end of the horizon`)]][remPos]
    res.costs[[1]]$Turnover <- res.costs[[1]]$Turnover + sum(abs(weights[remPos] - weights.rem.old)) + as.numeric(sum(abs(weights[newPos])))
  }
    
  ## CASE 3: old portfolio ETFs = new portfolio ETFs 
  # --> Only compute TC for weight changes
  if(identical(newPos, character(0)) && length(old.portf)==length(weights)){
    
    res.costs[2:length(res.costs)] <- lapply(2:length(res.costs), function(x){
      
      weights.rem.old <- res.costs[[x]]$`Weights towards the end of the horizon`[[length(res.costs[[x]]$`Weights towards the end of the horizon`)-1]][remPos]
      res.costs[[x]]$Turnover <- res.costs[[x]]$Turnover + sum(abs(weights[remPos] - weights.rem.old))
      
      return(res.costs[[x]])
    })
    weights.rem.old <- res.costs[[1]]$`Weights towards the end of the horizon`[[length(res.costs[[1]]$`Weights towards the end of the horizon`)]][remPos]
    res.costs[[1]]$Turnover <- res.costs[[1]]$Turnover + sum(abs(weights[remPos] - weights.rem.old))
  }
  
  ## CASE 4: There are no new ETFs but some got sold 
    # --> Compute TC for weight changes
    # --> Also compute TC for ETFs that got discarded (sold)
    if(identical(newPos, character(0)) && length(old.portf)>length(weights)){
      
      res.costs[2:length(res.costs)] <- lapply(2:length(res.costs), function(x){
      
      weights.disc <- res.costs[[x]]$`Weights towards the end of the horizon`[[length(res.costs[[x]]$`Weights towards the end of the horizon`)-1]][disc]
      weights.rem.old <- res.costs[[x]]$`Weights towards the end of the horizon`[[length(res.costs[[x]]$`Weights towards the end of the horizon`)-1]][remPos]
      res.costs[[x]]$Turnover <- res.costs[[x]]$Turnover + sum(abs(weights[remPos] - weights.rem.old)) + as.numeric(sum(abs(weights.disc)))
      
      return(res.costs[[x]])
    })
    weights.disc <- res.costs[[1]]$`Weights towards the end of the horizon`[[length(res.costs[[1]]$`Weights towards the end of the horizon`)]][disc]
    weights.rem.old <- res.costs[[1]]$`Weights towards the end of the horizon`[[length(res.costs[[1]]$`Weights towards the end of the horizon`)]][remPos]
    res.costs[[1]]$Turnover <- res.costs[[1]]$Turnover + sum(abs(weights[remPos] - weights.rem.old)) + as.numeric(sum(abs(weights.disc)))
  }
  
  return(res.costs)
}


## ----Transaction Costs and TER, warning=FALSE---------------------------------------------------------------------------------------------------------------------
TC.AND.TER <- function(ret.portf, portf, weights, trans.costs, TER.monthly, old.portf, t, fix.tc, res.costs){
  
  #print("Transaction costs")
  
  # Select which TER to use and assign the portfolio weights to them
  TER.portf <- TER.monthly[colnames(ret.portf)]
  TER.portf <- TER.portf*abs(weights)
  
  # Monthly weighted returns with TER for given period
  ret.mon.all.TER <- t(apply(ret.portf, 1, function(x){x-as.numeric(TER.portf)}))
  # Portfolio weighted returns for the given period
  ret.mon.portf.TER <- rowSums(ret.mon.all.TER, na.rm = T)
  ret.mon.portf.TER <- xts(ret.mon.portf.TER, order.by = as.Date(names(ret.mon.portf.TER)))
  
  ######## For the first iteration (whole transaction costs in the first month, no turnover consideration and creation of storage tables) ########
  
  if(t==1){
    
    # Create list to store the results with sublists (nested lists) for Returns, Weights and Turnover:
    res.costs <- lapply(c(1:(length(trans.costs)+1)), function(i){
      list(
        `Monthly Portfolio Return` = NULL,
        `Weights towards the end of the horizon` = list(),
        Turnover = 0
        )
      })
    
    # Compute returns and weights
    res.costs[2:length(res.costs)] <- lapply(c(1:length(trans.costs)), function(x){
    # "[2:length(res.costs)]" is necessary, because first entry is for zero costs and thus needs to stay empty
      
      # TC with consideration of weights and fixed percentage share of TC
      TC.costs <- weights*(1-fix.tc)*trans.costs[x] + trans.costs[x]*fix.tc
      ret.mon.all.TER[1,] <- ret.mon.all.TER[1,] - TC.costs
      
      # Monthly portfolio weighted returns (monthly sum of weighted ETF returns) for the given period and given TC
      ret.mon.portf.TER <- rowSums(ret.mon.all.TER, na.rm = T)
      ret.mon.portf.TER <- xts(ret.mon.portf.TER, order.by = as.Date(names(ret.mon.portf.TER)))
      
      # Cumulative return of the specific assets (ETFs)
      ret.cum.TER <- apply(ret.mon.all.TER, 2, function(x){ prod(1 + x, na.rm = TRUE) - 1 })
      
      # Value of the positions (weights) towards the end of the given period
      total.assets <- sum(abs(weights)*(1+ret.cum.TER))
      weights.new <- abs(weights)*(1+ret.cum.TER)/total.assets
      
      # Write results in result-list
      res.costs[[x+1]]$`Monthly Portfolio Return` <- rbind(res.costs[[x+1]]$`Monthly Portfolio Return`, ret.mon.portf.TER)
      res.costs[[x+1]]$`Weights towards the end of the horizon` <- append(res.costs[[x+1]]$`Weights towards the end of the horizon`, list(weights.new))
      
      return(res.costs[[x+1]])
    })
    
    # Assign names for levels of transaction costs
    names(res.costs)[1] <- "No costs"
    names(res.costs)[2:length(res.costs)] <- paste0(trans.costs*10000, "bps")
  }
  
  ######## For all other iterations (targeted transaction costs and turnover consideration) ########
  
  if(t!=1){
    
    # Check, which ETFs remained in the portf, which got discarded and which were newly added :
    remPos <- colnames(old.portf)[which(colnames(old.portf) %in% colnames(ret.portf))]
    disc <- colnames(old.portf)[which(colnames(old.portf) %notin% colnames(ret.portf))]
    newPos <- colnames(ret.portf)[which(colnames(ret.portf) %notin% remPos)]
    
    ## CASE 1: There are new ETFs and some old ETFs got discarded
    # --> Compute TC for ETFs that we sold, as well as for ETFs that we newly bought
    # --> Also compute TC for ETFs that stayed in the portfolio but whose weights changed
    #print("CASE 1")
    if(!identical(newPos, character(0)) && !identical(disc, character(0))){
      
      res.costs[2:length(res.costs)] <- lapply(c(1:length(trans.costs)), function(x){
      
        # 1. TC for discarded (sold) positions
        weights.disc <- res.costs[[x+1]]$`Weights towards the end of the horizon`[[length(res.costs[[x+1]]$`Weights towards the end of the horizon`)]][disc]
        TC.costs.disc <- sum(abs(weights.disc)*(1-fix.tc)*trans.costs[x] + trans.costs[x]*fix.tc)
        
        # 2. TC for new (bought) positions
        TC.costs.new <- abs(portf[,newPos])*(1-fix.tc)*trans.costs[x] + trans.costs[x]*fix.tc
        ret.mon.all.TER[1,newPos] <- ret.mon.all.TER[1,newPos] - TC.costs.new
        
        # 3. TC for remaining positions (consider weight changes)
        weights.rem.old <- res.costs[[x+1]]$`Weights towards the end of the horizon`[[length(res.costs[[x+1]]$`Weights towards the end of the horizon`)]][remPos]
        TC.costs.rem <- abs(weights.rem.old-portf[,remPos])*(1-fix.tc)*trans.costs[x] + trans.costs[x]*fix.tc
        ret.mon.all.TER[1,remPos] <- ret.mon.all.TER[1,remPos] - TC.costs.rem
        
        # Monthly portfolio weighted returns (monthly sum of weighted ETF returns) for the given period and given TC
        ret.mon.portf.TER <- rowSums(ret.mon.all.TER, na.rm = T)
        # Subtract sum of TC for discarded positions (from 1.)
        ret.mon.portf.TER[1] <-  ret.mon.portf.TER[1] - TC.costs.disc
        ret.mon.portf.TER <- xts(ret.mon.portf.TER, order.by = as.Date(names(ret.mon.portf.TER)))
        
        # Cumulative return of the specific assets (ETFs)
        ret.cum.TER <- apply(ret.mon.all.TER, 2, function(x){ prod(1 + x, na.rm = TRUE) - 1 })
        
        # Value of the positions (weights) towards the end of the given period
        total.assets <- sum(abs(weights)*(1+ret.cum.TER))
        weights.new <- abs(weights)*(1+ret.cum.TER)/total.assets
        
        # Write results in result-list
        res.costs[[x+1]]$`Monthly Portfolio Return` <- rbind(res.costs[[x+1]]$`Monthly Portfolio Return`, ret.mon.portf.TER)
        res.costs[[x+1]]$`Weights towards the end of the horizon` <- append(res.costs[[x+1]]$`Weights towards the end of the horizon`, list(weights.new))
        
        return(res.costs[[x+1]])
      })
    
      # Compute Turnover
      res.costs <- TURNOVER(res.costs = res.costs, ret.portf = ret.portf, weights = weights, remPos = remPos, disc = disc, newPos = newPos, old.portf = old.portf)
    
      # Assign names for levels of transaction costs
      names(res.costs)[1] <- "No costs"
      names(res.costs)[2:length(res.costs)] <- paste0(trans.costs*10000, "bps")
      
    }
    
    ## CASE 2: There are new ETFs but no ETFs got discarded (sold)
    # --> Compute TC for weight changes
    # --> Also compute TC for ETFs that got bought additionally
    #print("CASE 2")
    if(!identical(newPos, character(0)) && identical(disc, character(0))){
      
      res.costs[2:length(res.costs)] <- lapply(c(1:length(trans.costs)), function(x){
        
        # 1. TC for new (bought) positions
        TC.costs.new <- abs(portf[,newPos])*(1-fix.tc)*trans.costs[x] + trans.costs[x]*fix.tc
        ret.mon.all.TER[1,newPos] <- ret.mon.all.TER[1,newPos] - TC.costs.new
        
        # 2. TC for remaining positions (consider weight changes)
        weights.rem.old <- res.costs[[x+1]]$`Weights towards the end of the horizon`[[length(res.costs[[x+1]]$`Weights towards the end of the horizon`)]][remPos]
        TC.costs.rem <- abs(weights.rem.old-portf[,remPos])*(1-fix.tc)*trans.costs[x] + trans.costs[x]*fix.tc
        ret.mon.all.TER[1,remPos] <- ret.mon.all.TER[1,remPos] - TC.costs.rem
        
        # Monthly portfolio weighted returns (monthly sum of weighted ETF returns) for the given period and given TC
        ret.mon.portf.TER <- rowSums(ret.mon.all.TER, na.rm = T)
        ret.mon.portf.TER <- xts(ret.mon.portf.TER, order.by = as.Date(names(ret.mon.portf.TER)))
        
        # Cumulative return of the specific assets (ETFs)
        ret.cum.TER <- apply(ret.mon.all.TER, 2, function(x){ prod(1 + x, na.rm = TRUE) - 1 })
        
        # Value of the positions (weights) towards the end of the given period
        total.assets <- sum(abs(weights)*(1+ret.cum.TER))
        weights.new <- abs(weights)*(1+ret.cum.TER)/total.assets
        
        # Write results in result-list
        res.costs[[x+1]]$`Monthly Portfolio Return` <- rbind(res.costs[[x+1]]$`Monthly Portfolio Return`, ret.mon.portf.TER)
        res.costs[[x+1]]$`Weights towards the end of the horizon` <- append(res.costs[[x+1]]$`Weights towards the end of the horizon`, list(weights.new))
        
        return(res.costs[[x+1]])
      })
      
      # Compute Turnover
      res.costs <- TURNOVER(res.costs = res.costs, ret.portf = ret.portf, weights = weights, remPos = remPos, disc = disc, newPos = newPos, old.portf = old.portf)
    
      # Assign names for levels of transaction costs
      names(res.costs)[1] <- "No costs"
      names(res.costs)[2:length(res.costs)] <- paste0(trans.costs*10000, "bps")
      
    }
    
    ## CASE 3: old portfolio ETFs = new portfolio ETFs 
    # --> Only compute TC for weight changes
    #print("CASE 3")
    if(identical(newPos, character(0)) && length(old.portf)==length(weights)){
      
      res.costs[2:length(res.costs)] <- lapply(c(1:length(trans.costs)), function(x){
        
        # 1. TC for remaining positions (consider weight changes)
        weights.rem.old <- res.costs[[x+1]]$`Weights towards the end of the horizon`[[length(res.costs[[x+1]]$`Weights towards the end of the horizon`)]][remPos]
        TC.costs.rem <- abs(weights.rem.old-portf[,remPos])*(1-fix.tc)*trans.costs[x] + trans.costs[x]*fix.tc
        ret.mon.all.TER[1,remPos] <- ret.mon.all.TER[1,remPos] - TC.costs.rem
        
        # Monthly portfolio weighted returns (monthly sum of weighted ETF returns) for the given period and given TC
        ret.mon.portf.TER <- rowSums(ret.mon.all.TER, na.rm = T)
        ret.mon.portf.TER <- xts(ret.mon.portf.TER, order.by = as.Date(names(ret.mon.portf.TER)))
        
        # Cumulative return of the specific assets (ETFs)
        ret.cum.TER <- apply(ret.mon.all.TER, 2, function(x){ prod(1 + x, na.rm = TRUE) - 1 })
        
        # Value of the positions (weights) towards the end of the given period
        total.assets <- sum(abs(weights)*(1+ret.cum.TER))
        weights.new <- abs(weights)*(1+ret.cum.TER)/total.assets
        
        # Write results in result-list
        res.costs[[x+1]]$`Monthly Portfolio Return` <- rbind(res.costs[[x+1]]$`Monthly Portfolio Return`, ret.mon.portf.TER)
        res.costs[[x+1]]$`Weights towards the end of the horizon` <- append(res.costs[[x+1]]$`Weights towards the end of the horizon`, list(weights.new))
        
        return(res.costs[[x+1]])
      })
      
      # Compute Turnover
      res.costs <- TURNOVER(res.costs = res.costs, ret.portf = ret.portf, weights = weights, remPos = remPos, disc = disc, newPos = newPos, old.portf = old.portf)
    
      # Assign names for levels of transaction costs
      names(res.costs)[1] <- "No costs"
      names(res.costs)[2:length(res.costs)] <- paste0(trans.costs*10000, "bps")
        
    }
    
    ## CASE 4: There are no new ETFs but some got sold 
    # --> Compute TC for weight changes
    # --> Also compute TC for ETFs that got discarded (sold)
    #print("CASE 4")
    if(identical(newPos, character(0)) && length(old.portf)>length(weights)){
      
      res.costs[2:length(res.costs)] <- lapply(c(1:length(trans.costs)), function(x){
      
        # 1. TC for discarded (sold) positions
        weights.disc <- res.costs[[x+1]]$`Weights towards the end of the horizon`[[length(res.costs[[x+1]]$`Weights towards the end of the horizon`)]][disc]
        TC.costs.disc <- sum(abs(weights.disc)*(1-fix.tc)*trans.costs[x] + trans.costs[x]*fix.tc)
        
        # 2. TC for remaining positions (consider weight changes)
        weights.rem.old <- res.costs[[x+1]]$`Weights towards the end of the horizon`[[length(res.costs[[x+1]]$`Weights towards the end of the horizon`)]][remPos]
        TC.costs.rem <- abs(weights.rem.old-portf[,remPos])*(1-fix.tc)*trans.costs[x] + trans.costs[x]*fix.tc
        ret.mon.all.TER[1,remPos] <- ret.mon.all.TER[1,remPos] - TC.costs.rem
        
        # Monthly portfolio weighted returns (monthly sum of weighted ETF returns) for the given period and given TC
        ret.mon.portf.TER <- rowSums(ret.mon.all.TER, na.rm = T)
        # Subtract sum of TC for discarded positions (from 1.)
        ret.mon.portf.TER[1] <-  ret.mon.portf.TER[1] - TC.costs.disc
        ret.mon.portf.TER <- xts(ret.mon.portf.TER, order.by = as.Date(names(ret.mon.portf.TER)))
        
        # Cumulative return of the specific assets (ETFs)
        ret.cum.TER <- apply(ret.mon.all.TER, 2, function(x){ prod(1 + x, na.rm = TRUE) - 1 })
        
        # Value of the positions (weights) towards the end of the given period
        total.assets <- sum(abs(weights)*(1+ret.cum.TER))
        weights.new <- abs(weights)*(1+ret.cum.TER)/total.assets
        
        # Write results in result-list
        res.costs[[x+1]]$`Monthly Portfolio Return` <- rbind(res.costs[[x+1]]$`Monthly Portfolio Return`, ret.mon.portf.TER)
        res.costs[[x+1]]$`Weights towards the end of the horizon` <- append(res.costs[[x+1]]$`Weights towards the end of the horizon`, list(weights.new))
        
        return(res.costs[[x+1]])
      })
      
      # Compute Turnover
      res.costs <- TURNOVER(res.costs = res.costs, ret.portf = ret.portf, weights = weights, remPos = remPos, disc = disc, newPos = newPos, old.portf = old.portf)
      
      # Assign names for levels of transaction costs
      names(res.costs)[1] <- "No costs"
      names(res.costs)[2:length(res.costs)] <- paste0(trans.costs*10000, "bps")
      
    }
  }
  
  return(res.costs)
}


## ----Return, warning=FALSE----------------------------------------------------------------------------------------------------------------------------------------
RET <- function(ret, portf, trans.costs, TER.monthly, old.portf, all.ret.mon.portf, t, fix.tc, res.costs){
  
  #print("Return")
  
  # Select returns of portfolio
  ret.hor.portf <- ret[,colnames(portf)]
  weights <- as.numeric(portf)
  
  # Monthly weighted returns for given period
  ret.mon.all <- t(apply(ret.hor.portf, 1, function(x){ x*weights}))
  # Monthly portfolio weighted returns (monthly sum of weighted ETF returns) for the given period and given TC
  ret.mon.portf <- rowSums(ret.mon.all, na.rm = T)
  ret.mon.portf <- xts(ret.mon.portf, order.by = as.Date(names(ret.mon.portf)))
  
  # Save monthly portfolio returns
  all.ret.mon.portf <- rbind(all.ret.mon.portf, ret.mon.portf)
  
  # Cumulative return of the portfolio
  ret.cum.portf <- prod(1 + ret.mon.portf, na.rm = TRUE) - 1
  
  # Cumulative return of the specific assets (ETFs)
  ret.cum <- apply(ret.hor.portf, 2, function(x){ prod(1 + x, na.rm = TRUE) - 1 })
  
  # Value of the positions (weights) towards the end of the given period
  total.assets <- sum(abs(weights)*(1+ret.cum))
  weights.new <- abs(weights)*(1+ret.cum)/total.assets
  
  # Consideration of transaction costs and TER:
  res.costs <- TC.AND.TER(ret.portf = ret.mon.all, portf = portf, weights = weights, trans.costs = trans.costs, 
                          TER.monthly = TER.monthly, old.portf = old.portf, t = t, fix.tc = fix.tc,
                          res.costs = res.costs)
  
  # Bind results without and with costs:
  res.costs$`No costs`$`Monthly Portfolio Return` <- all.ret.mon.portf
  res.costs$`No costs`$`Weights towards the end of the horizon` <- append(res.costs$`No costs`$`Weights towards the end of the horizon`, list(weights.new))
  
  res.all <- res.costs
  
  return(res.all)
  
}


## ----Summary Statistics, warning=FALSE----------------------------------------------------------------------------------------------------------------------------
STATISTICS <- function(res, RF, VaR.CI, horizon, trans.costs){
  
  # Annualized Return
  ret.ann <- lapply(1:length(res), function(x){
    (1+mean(res[[x]]$`Monthly Portfolio Return`))^12-1
    })
  names(ret.ann)[1] <- "no costs"
  names(ret.ann)[2:length(ret.ann)] <- paste0(trans.costs*10000, "bps")
  
  # Annualized SD
  sd.ann <- lapply(1:length(res), function(x){
    sd(res[[x]]$`Monthly Portfolio Return`)*sqrt(12)
    })
  names(sd.ann)[1] <- "no costs"
  names(sd.ann)[2:length(sd.ann)] <- paste0(trans.costs*10000, "bps")
  
  # Annualized Sharpe Ratio
  RF.ann <- (1+mean(RF))^(12)-1
  SR.ann <- lapply(1:length(res), function(x){
    (ret.ann[[x]]-RF.ann)/sd.ann[[x]]
  })
  names(SR.ann)[1] <- "no costs"
  names(SR.ann)[2:length(SR.ann)] <- paste0(trans.costs*10000, "bps")
  
  # Annualized Sharpe Ratio Loss
  SR.loss <- lapply(1:length(SR.ann), function(x){
    SR.ann[[x]]/SR.ann[[1]]-1
  })
  names(SR.ann)[1] <- "no costs"
  names(SR.ann)[2:length(SR.ann)] <- paste0(trans.costs*10000, "bps")
  
  # Annualized Sortino Ratio
  SortinoR.ann <- lapply(1:length(res), function(x){
    as.numeric((SortinoRatio(res[[x]]$`Monthly Portfolio Return` - RF[index(res[[x]]$`Monthly Portfolio Return`)], MAR = 0))*sqrt(12))
  })
  names(SortinoR.ann)[1] <- "no costs"
  names(SortinoR.ann)[2:length(SortinoR.ann)] <- paste0(trans.costs*10000, "bps")
  
  # Annualized Value at Risk
  VaR.ann <- lapply(1:length(res), function(x){
    abs(as.numeric(VaR(res[[x]]$`Monthly Portfolio Return`, p = VaR.CI, method = "historical")))*sqrt(12)
  })
  names(VaR.ann)[1] <- "no costs"
  names(VaR.ann)[2:length(VaR.ann)] <- paste0(trans.costs*10000, "bps")
  
  # Maximum Drawdown
  Max.Draw <- lapply(1:length(res), function(x){
    as.numeric(maxDrawdown(res[[x]]$`Monthly Portfolio Return`))
  })
  names(Max.Draw)[1] <- "no costs"
  names(Max.Draw)[2:length(Max.Draw)] <- paste0(trans.costs*10000, "bps")
  
  # Normalized Turnover
  Turnover.norm <- lapply(1:length(res), function(x){
    res[[x]]$`Turnover`/(length(res[[x]]$`Monthly Portfolio Return`)-ceiling(length(res[[x]]$`Monthly Portfolio Return`)/horizon)-1)
  })
  names(Turnover.norm)[1] <- "no costs"
  names(Turnover.norm)[2:length(Turnover.norm)] <- paste0(trans.costs*10000, "bps")
  
  return(do.call(cbind, list(ret.ann = ret.ann,
                             sd.ann = sd.ann,
                             SR.ann = SR.ann,
                             SR.loss = SR.loss,
                             SortinoR.ann = SortinoR.ann,
                             VaR.ann = VaR.ann,
                             Max.Draw = Max.Draw,
                             Turnover.norm = Turnover.norm)))
}


## ----Return Distribution, warning=FALSE---------------------------------------------------------------------------------------------------------------------------
DISTRIBUTION <- function(res){
  
  return_df <- as.data.frame(res)
  
  # Reshape data from wide to long format and separate 'Type' and 'Cost'
  long_data <- return_df %>%
    pivot_longer(cols = everything(), names_to = "Var", values_to = "Return") %>%
    separate(Var, into = c("Type", "Cost"), sep = "\\.")
    
  # Ensure 'Type' and 'Cost' are factors to preserve order
  long_data$Type <- factor(long_data$Type, levels = unique(long_data$Type))
  long_data$Cost <- factor(long_data$Cost, levels = unique(long_data$Cost))
  
  # Get unique 'Cost' and 'Type' levels
  unique_Costs <- levels(long_data$Cost)
  unique_Types <- levels(long_data$Type)
  
  # Initialize a matrix to store plots with 'Cost' as rows and 'Type' as columns
  nrow <- length(unique_Costs)
  ncol <- length(unique_Types)
  plot_matrix <- matrix(list(), nrow = nrow, ncol = ncol, dimnames = list(unique_Costs, unique_Types))
  
  # Loop over 'Cost' (rows) and 'Type' (columns) to create plots
  for (i in seq_along(unique_Costs)) {
    for (j in seq_along(unique_Types)) {
      cost <- unique_Costs[i]
      type <- unique_Types[j]
      subset_data <- filter(long_data, Type == type, Cost == cost)
      returns <- subset_data$Return
      
      # Compute skewness and excess kurtosis
      skew <- skewness(returns, na.rm = TRUE)
      kurt <- kurtosis(returns, na.rm = TRUE) - 3  # Subtract 3 for excess kurtosis
      
      # Create plot with annotations
      p <- ggplot(subset_data, aes(x = Return)) +
        geom_density(fill = "skyblue", alpha = 0.5) +
        ggtitle(paste("", cost, "", type)) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5)) +
        annotate("text", x = Inf, y = Inf, label = sprintf("Skewness: %.2f\nExcess Kurtosis: %.2f", skew, kurt),
                 hjust = 1.1, vjust = 1.1, size = 4, color = "blue")
        
      # Store the plot in the matrix
      plot_matrix[i, j] <- list(p)  # Use 'list(p)' to store the plot correctly
    }
  }
  
  # Convert the plot matrix to a list in the correct order
  plot_list <- as.vector(t(plot_matrix))  # Transpose before unlisting to maintain row-wise order
  
  # Arrange the plots with 'Cost' as rows and 'Type' as columns
  grid.arrange(grobs = plot_list, nrow = nrow, ncol = ncol)
  
}


## ----Exposure, warning=FALSE--------------------------------------------------------------------------------------------------------------------------------------
EXPOSURE <- function(weights.init, horizon, y_test, asset.classes){
  
  # Delist input if necessary and save values as long format
  if(is.list(weights.init)){
    weights.init <- do.call(rbind, lapply(weights.init, function(x) {
      
      trans <- melt(x, value.name = "Weights", varnames = c("Weights", "ETF"))
      trans <- trans[, -1]
      
      return(trans)
    }))
  }
  
  ## Exposure for the specific ETFs:
  # Compute grouped sums and normalize them by the number of rebalancings
  Exposure <- weights.init %>%
    group_by(ETF) %>%
    summarize(Exposure = sum(Weights)/ceiling((nrow(y_test)/horizon)))
  
  # Plot the Exposures:
  # Consider all ETFs
  all.ETFs <- data.frame(ETF = colnames(y_test))
  Exposure <- all.ETFs %>%
    full_join(Exposure, by = "ETF") %>%
    mutate(Exposure = ifelse(is.na(Exposure), 0, Exposure))
  
  Exposure.ETF.plot <- ggplot(Exposure, aes(x = ETF, y = Exposure)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    labs(x = "ETF", y = "Exposure") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ## Analyze the exposure against the asset classes:
  if(!is.null(asset.classes)){
    asset.class.df <- do.call(rbind, lapply(names(asset.classes), function(x){
      data.frame(ETF = asset.classes[[x]], AssetClass = x)
      }))
    # Assign ETF exposures
    Exposure <- Exposure %>%
      left_join(asset.class.df, by = "ETF")
    
    # Compute Exposure for asset classes
    Exposure.asset.classes <- Exposure %>%
      group_by(AssetClass) %>%
      summarize(TotalExposure = sum(Exposure))
    # Plot the exposures
    Exposure.asset.classes.plot <- ggplot(Exposure.asset.classes, aes(x = AssetClass, y = TotalExposure)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      labs(x = "Asset Class", y = "Total Exposure") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  
  if(!is.null(asset.classes)){
    return(list(Exposure.ETFs = Exposure,
                Exposure.ETFs.plot = Exposure.ETF.plot,
                Exposure.asset.classes = Exposure.asset.classes,
                Exposure.asset.classes.plot = Exposure.asset.classes.plot))
  }
  if(is.null(asset.classes)){
    return(list(Exposure.ETFs = Exposure,
                Exposure.ETFs.plot = Exposure.ETF.plot))
  }
}


## ----Accuracy and Precision, warning=FALSE------------------------------------------------------------------------------------------------------------------------
EVAL <- function(y_predictions, y_actual, y_predictions.cum, y_actual.cum){
  
  # Compute measures for monthly return predictions
  if(all(class(y_predictions) == "list") == T){
    y_predictions <- do.call(cbind, y_predictions)
  }
  precision <- lapply(1:ncol(y_predictions), function(x){
    caret::postResample(pred = y_predictions[,x], obs = y_actual[,x])
    })
  names(precision) <- colnames(y_predictions)
  precision <- do.call(cbind, precision)
  rownames(precision) <- paste0("monthly ", rownames(precision))
  
  # Compute measures for cumulative return predictions
  if(all(class(y_predictions.cum) == "list") == T){
    y_predictions.cum <- do.call(cbind, y_predictions.cum)
  }
  precision.cum <- lapply(1:ncol(y_predictions.cum), function(x){
    caret::postResample(y_predictions.cum[,x], y_actual.cum[,x])
    })
  names(precision.cum) <- colnames(y_predictions.cum)
  precision.cum <- do.call(cbind, precision.cum)
  rownames(precision.cum) <- paste0("cumulative ", rownames(precision.cum))
  
  return(list(monthly = precision,
              cumulative = precision.cum))
}


## ----Wealth Simulation, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------
WEALTH.SIMULATION <- function(ret, init){
  
  # We need one more row because we invest at the start (i.e. first row == amount invested)
  Wealth <- matrix(nrow = length(ret)+1)
  Wealth <- xts(Wealth, order.by = c(floor_date(index(ret)[1], "month"), index(ret)))
  colnames(Wealth) <- "Wealth"
  dates <- index(Wealth)
  
  # Compute Wealth
  for(i in 1:nrow(Wealth)){
    if(i == 1){
      Wealth[i,] <- init
    }
    if(i != 1){
      Wealth[i,] <- as.numeric(Wealth[i-1,])*(1+as.numeric(ret[i-1,])) #Indices are shifted by one
    }
  }
  
  # Compute absolute and relative return over time
  abs.ret <- as.numeric(Wealth[nrow(Wealth),]) - as.numeric(Wealth[1,])
  rel.ret <- as.numeric(Wealth[nrow(Wealth),]) / as.numeric(Wealth[1,]) - 1
  
  # Maximum Drawdown
  maxDraw <- table.Drawdowns(ret, top = 1, geometric = T)
  maxDraw <- maxDraw[-c(5:7)]
  
  # Plot the result
  Plot <- ggplot(Wealth) +
    geom_line(aes(x = dates, y = `Wealth`, color = "Value")) +
    scale_x_date(labels = date_format("%d-%m-%Y")) +
    scale_color_manual(" ", values = c("Value" = "skyblue"))+
    labs(x = "Dates", y = "Wealth")+
    theme_minimal()
  
  return(list(Wealth = Wealth,
              abs.ret = abs.ret,
              rel.ret = rel.ret,
              Plot = Plot,
              maxDraw = maxDraw))
  
}


## ----Equal Weights, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------
EQUAL.WEIGHTS <- function(portf.names, short = FALSE){
  
  # Should a long portfolio be weighted
  if(short == FALSE){
    n <- length(portf.names)
    wi <- rep(1/n, n)
    portf <- matrix(wi, nrow = 1)
    colnames(portf) <- portf.names
    return(portf)
  }
  
  # Or a short portfolio
  if(short == TRUE){
    n <- length(portf.names)
    wi <- -rep(1/n, n) # Assign negative weights
    portf <- matrix(wi, nrow = 1)
    colnames(portf) <- portf.names
    return(portf)
  }
}


## ----Rank Weights, warning=FALSE----------------------------------------------------------------------------------------------------------------------------------
RANK.WEIGHTS <- function(portf.names, short = FALSE){
  
  # For long portfolios
  if(short == FALSE){
    n <- length(portf.names)
    # Assign weights proportional to the inverse of their rank
    # Works, because ETFs are already sorted by their ranks (left to right == highest to lowest predicted cumulative return)
    wi_raw <- 1 / (1:n)
    # Normalize weights so that they sum to 1
    wi <- wi_raw / sum(wi_raw)
    # Create a matrix with one row (the normalized weights)
    portf <- matrix(wi, nrow = 1)
    # Set the column names to be the ETF names
    colnames(portf) <- portf.names
    return(portf)
  }
  
  # For short portfolios
  if(short == TRUE){
    n <- length(portf.names)
    wi_raw <- 1 / (1:n)
    wi <- -wi_raw / sum(wi_raw) # Assign negative weights
    portf <- matrix(wi, nrow = 1)
    colnames(portf) <- portf.names
    return(portf)
  }
  
}


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
B.A.H <- function(bench, BT.Dates, trans.costs, TER.monthly, RF, VaR.CI = 0.95){
  
  ret.bench <- bench[BT.Dates,]
  
  # Return:
  mean.ret.mon <- mean(ret.bench, na.rm = T)
  mean.ret.ann <- (1+mean.ret.mon)^(12)-1
  
  # Standard deviation:
  SD.mon <- sd(ret.bench, na.rm=T)
  SD.ann <- SD.mon*sqrt(12)
  
  # Risk free rate:
  mean.RF.mon <- mean(RF[BT.Dates], na.rm = T)
  mean.RF.ann <- (1+mean.RF.mon)^(12)-1
  
  # Sharpe Ratio:
  SR.mon <- (mean.ret.mon-mean.RF.mon)/SD.mon
  SR.ann <- (mean.ret.ann-mean.RF.ann)/SD.ann
  
  # Sharpe Ratio Loss for no costs:
  SR.loss <- 0
  
  # Sortino Ratio:
  SortinoR.mon <- SortinoRatio(ret.bench-RF[BT.Dates], MAR = 0)
  SortinoR.ann <- SortinoR.mon*sqrt(12)
  
  # Value at Risk:
  VaR.mon <- abs(as.numeric(VaR(ret.bench, p = VaR.CI, method = "historical")))
  VaR.ann <- VaR.mon*sqrt(12)
  
  # Maximum Drawdown:
  MDD <- as.numeric(maxDrawdown(ret.bench))
  
  ## With costs:
  # Total (rolling) costs (TER):
  Returns_TER <- ret.bench - as.numeric(TER.monthly[colnames(bench)])
  
  # Transaction costs:
  # Note:   Since we look at a buy-and-hold strategy we only buy one time
  #         Hence, we only need to consider the transaction costs in month one!
  SR.first <- SR.ann
  res.costs <- lapply(c(1:length(trans.costs)), function(x){
    # Consider transaction costs only in the first month:
    Returns_TER[1] <- Returns_TER[1] - trans.costs[x]
    
    # Returns:
    mean.ret.mon.costs <- mean(Returns_TER, na.rm = T)
    mean.ret.ann.costs <- (1+mean.ret.mon.costs)^12-1
    
    # Standard deviation:
    sd.mon.costs <- sd(Returns_TER, na.rm = TRUE)
    sd.ann.costs <- sd.mon.costs*sqrt(12)
    
    # Sharpe Ratio:
    SR.ann <- (mean.ret.ann.costs-mean.RF.ann)/sd.ann.costs
    
    # Sharpe Ratio Loss:
    SR.loss <- SR.ann/SR.first-1
    
    # Sortino Ratio:
    SortinoR.mon.costs <- SortinoRatio(Returns_TER-RF, MAR = 0)
    SortinoR.ann.costs <- SortinoR.mon.costs*sqrt(12)
    
    # Value at Risk:
    VaR.mon.costs <- abs(as.numeric(VaR(Returns_TER, p = VaR.CI, method = "historical")))
    VaR.ann.costs <- VaR.mon.costs*sqrt(12)
    
    # Maximum Drawdown:
    MDD.costs <- as.numeric(maxDrawdown(Returns_TER))
    
    return(list(mean.ret.ann = mean.ret.ann.costs,
                sd.ann = sd.ann.costs,
                SR.ann = SR.ann,
                SR.loss = SR.loss,
                SortinoR.ann = SortinoR.ann.costs,
                VaR.ann = VaR.ann.costs,
                MaxDraw = MDD.costs,
                ret.Costs = Returns_TER))
  })
  
  names(res.costs) <- paste0(trans.costs * 10000, "bps")

  # Combine lists using `c()`, which preserves names better than `append()`
  res.all <- c(list("no costs" = list(mean.ret.ann = mean.ret.ann,
                                      sd.ann = SD.ann,
                                      SR.ann = SR.ann,
                                      SR.loss = SR.loss,
                                      SortinoR.ann = SortinoR.ann,
                                      VaR.ann = VaR.ann,
                                      MaxDraw = MDD,
                                      ret.noCosts = ret.bench)),
               res.costs)
  
  return(res.all)
}


## ----Long-Only Portfolio, warning=FALSE---------------------------------------------------------------------------------------------------------------------------
LONG.ONLY <- function(y_test, cum.pred = NULL, use.long.only, counter = 1, t = 1, horizon, portf.EW.all, portf.RW.all,
                      trans.costs, TER.monthly, all.ret.mon.portf.EW, all.ret.mon.portf.RW, fix.tc, res.all){
  
  # Check if horizon is set correctly (i.e. if we are in the last iteration)
  if((counter+horizon-1) > nrow(y_test)){
    horizon <- nrow(y_test) - counter + 1
  }
  
  ######## Select ETFs to invest in: ########
  # If return predictions are available
  if(!is.null(cum.pred)){
    # Assign correct ETF-names
    colnames(cum.pred) <- colnames(y_test)
    # Order ETFs decreasingly according to theier predictions:
    order.t <- cum.pred[t,][order(cum.pred[t,], decreasing = TRUE)]
    order.t <- rbind(order.t, c(1:length(order.t)))
    # Select share of ETFs to invest in (i.e. create ETF-portfolio list with predicted returns)
    consider <- round(use.long.only*ncol(order.t), 0)
    portf.names <- names(order.t[1, c(1:consider)])
  }
  
  # If no return predictions are available, use all ETFs
  if(is.null(cum.pred)){
    portf.names <- colnames(y_test)
  }
  
  ######## Compute weights: ########
  # Equal weighted
  portf.EW <- EQUAL.WEIGHTS(portf.names = portf.names)
  portf.EW.all[[t]] <- portf.EW
  
  # Rank weighted (only if return predictions are available)
  if(!is.null(cum.pred)){
    portf.RW <- RANK.WEIGHTS(portf.names = portf.names)
    portf.RW.all[[t]] <- portf.RW
  }
  
  ######## Compute Returns: ########
  # Select out-of-sample returns for the next investing period (next "horizon" months)
  ret.hor <- y_test[counter:(counter+horizon-1),]
  
  ## Returns for the equally weighted portfolio
  # For the first iteration
  if(t==1){
    ret.portf.EW <- RET(ret = ret.hor, portf = portf.EW, trans.costs = trans.costs, TER.monthly = TER.monthly, 
                        old.portf = portf.EW.all, all.ret.mon.portf = all.ret.mon.portf.EW, t = t, 
                        fix.tc = fix.tc, res.costs = res.all$ret.portf.EW)
  }
  # For all other iterations
  if(t!=1){
    ret.portf.EW <- RET(ret = ret.hor, portf = portf.EW, trans.costs = trans.costs, TER.monthly = TER.monthly, 
                        old.portf = portf.EW.all[[t-1]], all.ret.mon.portf = all.ret.mon.portf.EW, t = t, 
                        fix.tc = fix.tc, res.costs = res.all$ret.portf.EW)
  }
  
  ## Returns for the rank weighted portfolio
  # For the first iteration
  if(!is.null(cum.pred)){
    if(t==1){
      ret.portf.RW <- RET(ret = ret.hor, portf = portf.RW, trans.costs = trans.costs, TER.monthly = TER.monthly, 
                          old.portf = portf.RW.all, all.ret.mon.portf = all.ret.mon.portf.RW, t = t, 
                          fix.tc = fix.tc, res.costs = res.all$ret.portf.RW)
    }
    # For all other iterations
    if(t!=1){
      ret.portf.RW <- RET(ret = ret.hor, portf = portf.RW, trans.costs = trans.costs, TER.monthly = TER.monthly, 
                          old.portf = portf.RW.all[[t-1]], all.ret.mon.portf = all.ret.mon.portf.RW, t = t, 
                          fix.tc = fix.tc, res.costs = res.all$ret.portf.RW)
    }
  }
  
  t <-  t + 1
  counter <- counter + horizon
  
  if(!is.null(cum.pred)){
    res <- list(ret.portf.EW = ret.portf.EW,
                ret.portf.RW = ret.portf.RW,
                t = t,
                counter = counter,
                portf.EW.all = portf.EW.all,
                portf.RW.all = portf.RW.all
                )
  }
  if(is.null(cum.pred)){
    res <- list(ret.portf.EW = ret.portf.EW,
                t = t,
                counter = counter,
                portf.EW.all = portf.EW.all
                )
  }
  
  return(res)
}


## ----Long-Short Portfolio, warning=FALSE--------------------------------------------------------------------------------------------------------------------------
LONG.SHORT <- function(y_test, cum.pred, use.long.short, disc.positive = F, lend = 1, counter = 1, t = 1, horizon, portf.EW.all, portf.RW.all,
                       trans.costs, TER.monthly, all.ret.mon.portf.EW, all.ret.mon.portf.RW, fix.tc, res.all){
  
  # Check if horizon is set correctly (i.e. if we are in the last iteration)
  if((counter+horizon-1) > nrow(y_test)){
    horizon <- nrow(y_test) - counter + 1
  }
  
  ######## Select ETFs to invest in: ########
  # Assign correct ETF-names
  colnames(cum.pred) <- colnames(y_test)
  # Order ETFs decreasingly according to their predictions:
  order.t <- cum.pred[t,][order(cum.pred[t,], decreasing = TRUE)]
  order.t <- rbind(order.t, c(1:length(order.t)))
  # Select share of ETFs to invest in (i.e. create ETF-portfolio list with predicted returns)
  consider.long <- round(use.long.short*ncol(order.t), 0)
  # Select ETFs to go long
  portf.names.long <- names(order.t[1, c(1:consider.long)])
  # Select ETFs to short
  portf.names.short <- names(order.t[1, c((ncol(order.t) - consider.long + 1):ncol(order.t))])
  # Check if only ETFs with negative predicted cumulative returns shall be considered
  if(disc.positive == T){
    tmp.shorts <- order.t[1, c((ncol(order.t) - consider.long + 1):ncol(order.t))]
    tmp.shorts <- tmp.shorts[-which(tmp.shorts>0)]
    portf.names.short <- names(tmp.shorts)
    if(identical(portf.names.short, character(0))){
      # Only use worst predicted ETF if no ETFs are predicted to be negative
      tmp.shorts <- order.t[1, c((ncol(order.t) - consider.long + 1):ncol(order.t))]
      tmp.shorts <- tmp.shorts[length(tmp.shorts)]
      portf.names.short <- names(tmp.shorts)
    }
  }
  
  ######## Compute weights: ########
  # Equal weighted
  portf.EW.long <- as.matrix(EQUAL.WEIGHTS(portf.names = portf.names.long, short = FALSE)/2*lend)
  portf.EW.short <- as.matrix(EQUAL.WEIGHTS(portf.names = portf.names.short, short = TRUE)/2*lend)
  portf.EW <- cbind(portf.EW.long, portf.EW.short)
  portf.EW.all[[t]] <- portf.EW
  
  # Rank weighted
  portf.RW.long <- as.matrix(RANK.WEIGHTS(portf.names = portf.names.long, short = FALSE)/2*lend)
  portf.RW.short <- as.matrix(RANK.WEIGHTS(portf.names = portf.names.short, short = TRUE)/2*lend)
  portf.RW <- cbind(portf.RW.long, portf.RW.short)
  portf.RW.all[[t]] <- portf.RW
  
  ####### Compute Returns: ########
  # Select out-of-sample returns for the next investing period (next "horizon" months)
  ret.hor <- y_test[counter:(counter+horizon-1),]
  
  ## Returns for the equally weighted portfolio
  # For the first iteration
  if(t==1){
    ret.portf.EW <- RET(ret = ret.hor, portf = portf.EW, trans.costs = trans.costs, TER.monthly = TER.monthly, 
                        old.portf = portf.EW.all, all.ret.mon.portf = all.ret.mon.portf.EW, t = t, 
                        fix.tc = fix.tc, res.costs = res.all$ret.portf.EW)
  }
  # For all other iterations
  if(t!=1){
    ret.portf.EW <- RET(ret = ret.hor, portf = portf.EW, trans.costs = trans.costs, TER.monthly = TER.monthly, 
                        old.portf = portf.EW.all[[t-1]], all.ret.mon.portf = all.ret.mon.portf.EW, t = t, 
                        fix.tc = fix.tc, res.costs = res.all$ret.portf.EW)
  }
  
  ## Returns for the rank weighted portfolio
  # For the first iteration
  if(!is.null(cum.pred)){
    if(t==1){
      ret.portf.RW <- RET(ret = ret.hor, portf = portf.RW, trans.costs = trans.costs, TER.monthly = TER.monthly, 
                          old.portf = portf.RW.all, all.ret.mon.portf = all.ret.mon.portf.RW, t = t, 
                          fix.tc = fix.tc, res.costs = res.all$ret.portf.RW)
    }
    # For all other iterations
    if(t!=1){
      ret.portf.RW <- RET(ret = ret.hor, portf = portf.RW, trans.costs = trans.costs, TER.monthly = TER.monthly, 
                          old.portf = portf.RW.all[[t-1]], all.ret.mon.portf = all.ret.mon.portf.RW, t = t, 
                          fix.tc = fix.tc, res.costs = res.all$ret.portf.RW)
    }
  }
  
  t <-  t + 1
  counter <- counter + horizon
  
  res <- list(ret.portf.EW = ret.portf.EW,
              ret.portf.RW = ret.portf.RW,
              t = t,
              counter = counter,
              portf.EW.all = portf.EW.all,
              portf.RW.all = portf.RW.all
              )
  return(res)
}


## ----Minimum Variance Portfolio, warning=FALSE--------------------------------------------------------------------------------------------------------------------
MIN.VAR <- function(ret, train.rows, y_test, cum.pred, use.minvar, counter = 1, t = 1, horizon, portf.shrink.all, portf.sample.all,
                    trans.costs, TER.monthly, all.ret.mon.portf.shrink, all.ret.mon.portf.sample, limit, fix.tc, min.max, res.all){
  
  # Check if horizon is set correctly (i.e. if we are in the last iteration)
  if((counter+horizon-1) > nrow(y_test)){
    horizon <- nrow(y_test) - counter + 1
  }
  
  ######## Select ETFs to invest in: ########
  # Assign correct ETF-names
  colnames(cum.pred) <- colnames(y_test)
  # Order ETFs decreasingly according to their predictions:
  order.t <- cum.pred[t,][order(cum.pred[t,], decreasing = TRUE)]
  order.t <- rbind(order.t, c(1:length(order.t)))
  # Select share of ETFs to invest in (i.e. create ETF-portfolio list with predicted returns)
  consider<- round(use.minvar*ncol(order.t), 0)
  portf.names <- names(order.t[1, c(1:consider)])
  
  ######## Select historical observations (tine window) to compute the covariance matrix with ######## 
  if(!is.na(limit) && limit < length(train.rows)){
    train.rows.tmp <- train.rows[(length(train.rows) - limit + 1):length(train.rows)]
  }
  if(!is.na(limit) && limit > length(train.rows)){
    train.rows.tmp <- train.rows
  }
  # If no limit is set do nothing
  if(is.na(limit)){
    train.rows.tmp <- train.rows
  }
  
  # For the last iteration
  if(!is.na(limit) && max(train.rows) >= nrow(ret)){
    train.rows.tmp <- c(min(train.rows):nrow(ret))[(length(train.rows) - limit + 1):length(train.rows)]
  }
  if(is.na(limit) && max(train.rows) >= nrow(ret)){
    train.rows.tmp <- c(min(train.rows):nrow(ret))
  }
  
  ######## Compute weights: ########
  # Select out-of-sample returns for the next investing period (next "horizon" months)
  ret.hor <- y_test[counter:(counter+horizon-1),]
  # Data to compute the covariance matrix with
  ret.hist <- ret[train.rows.tmp, portf.names]
  # Calculate sample covariance matrix
  cov.sample <- cov(na.omit(ret.hist))
  # Calculate shrinked covariance matrix
  cov.shrink <- linshrink_cov2(na.omit(ret.hist))
  
  # Rank based weight constraint
   if(!is.null(cum.pred) && identical(min.max, "rank")){
    w.max <- ((1 / (1:ncol(ret.hist)))/sum(1 / (1:ncol(ret.hist))))[1]
    w.min <- ((1 / (1:ncol(ret.hist)))/sum(1 / (1:ncol(ret.hist))))
    w.min <- w.min[length(w.min)]
   }
  # Manual weight constraint
  if(is.numeric(min.max) == T && length(min.max) == 2){
    w.min <- min.max[1]
    w.max <- min.max[2]
  }
  
  # Minimum Variance optimization with sample cov matrix
  weights.sample.wmax <- minvar(cov.sample, wmin = w.min, wmax = w.max, method = "qp")
  portf.sample <- t(as.matrix(weights.sample.wmax))
  colnames(portf.sample) <- portf.names
  portf.sample.all[[t]] <- portf.sample
  
  # Minimum Variance optimization with shrinked cov matrix
  weights.shrink.wmax <- minvar(cov.shrink, wmin = w.min, wmax = w.max, method = "qp")
  portf.shrink <- t(as.matrix(weights.shrink.wmax))
  colnames(portf.shrink) <- portf.names
  portf.shrink.all[[t]] <- portf.shrink
  
  ####### Compute Returns: ########
  ## Sample Version:
  # For the first iteration
  if(t==1){
    ret.portf.sample <- RET(ret = ret.hor, portf = portf.sample, trans.costs = trans.costs, TER.monthly = TER.monthly,
                            old.portf = portf.sample.all, all.ret.mon.portf = all.ret.mon.portf.sample, t = t,
                            fix.tc = fix.tc, res.costs = res.all$ret.portf.sample)
  }
  # For all other iterations
  if(t!=1){
    ret.portf.sample <- RET(ret = ret.hor, portf = portf.sample, trans.costs = trans.costs, TER.monthly = TER.monthly,
                            old.portf = portf.sample.all[[t-1]], all.ret.mon.portf = all.ret.mon.portf.sample, t = t,
                            fix.tc = fix.tc, res.costs = res.all$ret.portf.sample)
  }
  ## Shrinked Version:
  # For the first iteration
  if(t==1){
    ret.portf.shrink <- RET(ret = ret.hor, portf = portf.shrink, trans.costs = trans.costs, TER.monthly = TER.monthly,
                            old.portf = portf.shrink.all, all.ret.mon.portf = all.ret.mon.portf.shrink, t = t,
                            fix.tc = fix.tc, res.costs = res.all$ret.portf.shrink)
  }
  # For all other iterations
  if(t!=1){
    ret.portf.shrink <- RET(ret = ret.hor, portf = portf.shrink, trans.costs = trans.costs, TER.monthly = TER.monthly,
                            old.portf = portf.shrink.all[[t-1]], all.ret.mon.portf = all.ret.mon.portf.shrink, t = t,
                            fix.tc = fix.tc, res.costs = res.all$ret.portf.shrink)
  }
  
  # Update the indices
  train.rows <- c(train.rows, c(train.rows[length(train.rows)]+c(1:horizon)))
  t <-  t + 1
  counter <- counter + horizon
  
  res <- list(ret.portf.sample = ret.portf.sample,
              ret.portf.shrink = ret.portf.shrink,
              portf.sample.all = portf.sample.all,
              portf.shrink.all = portf.shrink.all,
              train.rows = train.rows,
              t = t,
              counter = counter)
  
  return(res)
}


## ----Return Predictions - Compute Prevailing Mean, warning=FALSE--------------------------------------------------------------------------------------------------
# If no Z-Score trans is wished
tmp <- ret
# If Z-score transformation is wished
if(trans.Z == T){
  tmp <- Z.TRANS(ret = ret, Names = Names, last = last)
}
# Initial number of training rows:
train.rows <- Train.Rows(ret = tmp, train.perc = train.perc)
# All test values (IMPORTANT: NOW USE "ret"):
y_test <- Test_DS(ret = tmp, train.rows = train.rows, horizon = horizon)

mean.ret.mon.all <- NULL
mean.ret.cum.all <- NULL
t=1

## Start predictions:
while(t<nrow(y_test$y_test)){
  
  res.prev <- PREVAILING.MEAN(ret = tmp, horizon = horizon, 
                              train.rows = train.rows, limit = limit, 
                              counter = t, mean.ret.mon.all = mean.ret.mon.all,
                              mean.ret.cum.all = mean.ret.cum.all, 
                              y_test = y_test$y_test)
  
  t <- res.prev$counter
  train.rows <- res.prev$train.rows
  mean.ret.mon.all <- res.prev$mean.ret.mon.all
  mean.ret.cum.all <- res.prev$mean.ret.cum.all
  
}

rownames(mean.ret.cum.all) <- c(1:nrow(mean.ret.cum.all))

## Evaluation for monthly and cumulative return predictions:
eval.pr.mean <- EVAL(y_predictions = mean.ret.mon.all, y_actual = y_test$y_test,
                     y_predictions.cum = mean.ret.cum.all, 
                     y_actual.cum = y_test$y_test.cum)
eval.pr.mean <- do.call(rbind, eval.pr.mean)
round(eval.pr.mean,4)
rowMeans(eval.pr.mean)



## ----Return Predictions - Compute Random Forest, warning=FALSE----------------------------------------------------------------------------------------------------
# If no Z-Score trans is wished
tmp <- ret
# If Z-score transformation is wished
if(trans.Z == T){
  tmp <- Z.TRANS(ret = ret, Names = Names, last = last)
}
# Initial number of training rows:
train.rows <- Train.Rows(ret = tmp, train.perc = train.perc)
# Compute EMAs with max. 5 years of data laying back for Macro variables:
macro.ema <- TRANS.MACRO(data = macro, lookback = lookback)
# All test values
y_test <- Test_DS(ret = tmp, train.rows = train.rows, horizon = horizon)
if(!is.null(clusters)){
  print("!!Attention: You set the variable clusters. Don't perform several other (complex) parallel tasks on your machine while the loop is running!!")
}
if(round(train.perc*nrow(ret)-lag,0) < (horizon+1)){
  print("!!ATTENTION: INTERFERENCE OF INITIAL TRAINING ROWS WITH TIME LAG AND HORIZON --> RANDOM FOREST CAN'T BE EXECUTED!!")
}
if((limit-lag) < (horizon+1)){
  print("!!ATTENTION: INTERFERENCE OF LIMIT WITH TIME LAG AND HORIZON --> RANDOM FOREST CAN'T BE EXECUTED!!")
}
t=1

## Start predictions:
while(t<nrow(y_test$y_test)){
  
  input <- Train_DS(ret = tmp, macro.ema = macro.ema, train.rows = train.rows, 
                    lag = lag, limit = limit)
  
  res <- RANDOM_FOREST(input = input, horizon = horizon, y_test = y_test, counter =  t, old.forecasts = old.forecasts,
                       cum.perf = cum.perf, tune_grid = tune_grid, train_control = train_control, clusters = clusters, 
                       limit = limit, n.trees = n.trees)
  
  t <- res$counter
  train.rows <- res$train.rows
  old.forecasts <- res$old.forecasts
  cum.perf <- res$cum.perf
  
}

## Extract return predictions:
monthly.pred <- do.call(cbind, res$old.forecasts)
cumulative.pred <- do.call(cbind, res$cum.perf)

## Evaluation for monthly and cumulative return predictions:
eval <- EVAL(y_predictions = res$old.forecasts, y_actual = y_test$y_test,
             y_predictions.cum = res$cum.perf, y_actual.cum = y_test$y_test.cum)
eval <- do.call(rbind, eval)
round(eval,4)
rowMeans(eval)



## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
train.rows <- Train.Rows(ret = ret, train.perc = train.perc)
y_test <- Test_DS(ret = ret, train.rows = train.rows, horizon = horizon)
BT.Dates <- index(y_test$y_test)
RF <- rf

## Start the backtest
res.bench <- B.A.H(bench = bench, BT.Dates = BT.Dates, trans.costs = trans.costs, TER.monthly = TER.monthly, RF = rf, VaR.CI = VaR.CI)

## Summary statistics:
stat.B.A.H <- do.call(rbind, res.bench)[,-ncol(do.call(rbind, res.bench))]
stat.B.A.H

## Distribution of returns:
tmp.nocost <- res.bench$`no costs`$ret.noCosts
colnames(tmp.nocost) <- "BuyandHold.noCost"
tmp.50bps <- res.bench$`50bps`$ret.Costs
colnames(tmp.50bps) <- "BuyandHold.50bps"
DISTRIBUTION(res = as.data.frame(do.call(cbind,list(BuyandHold.NoCosts = tmp.nocost,
                                                    BuyandHold.50bps = tmp.50bps))))

## Positional exposure:
# == Is always one for the single ETF case



## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
train.rows <- Train.Rows(ret = ret, train.perc = train.perc)
y_test <- Test_DS(ret = ret, train.rows = train.rows, horizon = horizon)
BT.Dates <- index(y_test$y_test)
B.a.H <- bench[BT.Dates,]
B.a.H <- cbind(B.a.H, y_test$y_test[,"Y20+ Treasury Bond"])
colnames(B.a.H) <- c("MSCI World", "Y20+ Treasury Bond")
counter = 1
t = 1
portf.EW.all <- list()
portf.RW.all <- list()
all.ret.mon.portf.EW <- NULL
all.ret.mon.portf.RW <- NULL
res.50_50 <- NULL

## Start backtest:
while(counter<nrow(y_test$y_test)){
  
  #print(t)
  
  res.50_50 <- LONG.ONLY(y_test = B.a.H, cum.pred = NULL, use.long.only = NULL, counter = counter, 
                         t = t, horizon = horizon, portf.EW.all = portf.EW.all, portf.RW.all = list(),
                         all.ret.mon.portf.EW = all.ret.mon.portf.EW, all.ret.mon.portf.RW = NULL, fix.tc = fix.tc,
                         trans.costs = trans.costs, TER.monthly = TER.monthly, res.all = res.50_50)
  t <- res.50_50$t
  counter <- res.50_50$counter
  portf.EW.all <- res.50_50$portf.EW.all
  all.ret.mon.portf.EW <- res.50_50$ret.portf.EW$`No costs`$`Monthly Portfolio Return`
  
}

## Summary statistics:
stat.B.A.H <- STATISTICS(res = res.50_50$ret.portf.EW, RF = rf, VaR.CI = VaR.CI, horizon = horizon, trans.costs = trans.costs)
stat.B.A.H

## Distribution of returns:
# ATTENTION: THE NAMING OF THE VECTORS IS ESSENTIAL AND HAS TO FOLLOW THE SEQUENCE "WEIGHTINGTYPE.COSTS"
DISTRIBUTION(res = as.data.frame(do.call(cbind,list(EquallyWeightedBench.NoCosts = res.50_50$ret.portf.EW$`No costs`$`Monthly Portfolio Return`,
                                                    EquallyWeightedBench.50bps = res.50_50$ret.portf.EW$`50bps`$`Monthly Portfolio Return`))))

## Positional exposure:
EXPOSURE(weights.init = portf.EW.all, horizon = horizon, y_test = y_test$y_test, asset.classes = asset.classes)



## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
# If no Z-Score trans is wished
tmp <- ret
# If Z-score transformation is wished
if(trans.Z == T){
  tmp <- Z.TRANS(ret = ret, Names = Names, last = last)
}
train.rows <- Train.Rows(ret = tmp, train.perc = train.perc)
dates <- index(Test_DS(ret = tmp, train.rows = train.rows, horizon = horizon)$y_test)
y_test <- Test_DS(ret = ret, train.rows = train.rows, horizon = horizon)
y_test$y_test <- y_test$y_test[dates,]
counter = 1
t = 1
portf.EW.all <- list()
portf.RW.all <- list()
all.ret.mon.portf.EW <- NULL
all.ret.mon.portf.RW <- NULL
res.PR.LO <- NULL

## Start the backtest
while(counter < nrow(y_test$y_test)){
  
  #print(t)
  
  res.PR.LO <- LONG.ONLY(y_test = y_test$y_test, cum.pred = mean.ret.cum.all, use.long.only = use.long.only,
                          counter = counter, t = t, horizon = horizon, portf.EW.all = portf.EW.all, portf.RW.all = portf.RW.all,
                          all.ret.mon.portf.EW = all.ret.mon.portf.EW, all.ret.mon.portf.RW = all.ret.mon.portf.RW, fix.tc = fix.tc,
                          trans.costs = trans.costs, TER.monthly = TER.monthly, res.all = res.PR.LO)
  
  t <- res.PR.LO$t
  counter <- res.PR.LO$counter
  # Extract initial weights
  portf.EW.all <- res.PR.LO$portf.EW.all
  portf.RW.all <- res.PR.LO$portf.RW.all
  # All other performance results
  all.ret.mon.portf.EW <- res.PR.LO$ret.portf.EW$`No costs`$`Monthly Portfolio Return`
  all.ret.mon.portf.RW <- res.PR.LO$ret.portf.RW$`No costs`$`Monthly Portfolio Return`
}

## Summary statistics:
# EW:
stat.EW.L.O.PR <- STATISTICS(res = res.PR.LO$ret.portf.EW, RF = rf, VaR.CI = VaR.CI, horizon = horizon, trans.costs = trans.costs)
# RW:
stat.RW.L.O.PR <- STATISTICS(res = res.PR.LO$ret.portf.RW, RF = rf, VaR.CI = VaR.CI, horizon = horizon, trans.costs = trans.costs)
LO.PR <- list(Equally.Weighted.L.O = stat.EW.L.O.PR, Rank.Weighted.L.O = stat.RW.L.O.PR)
LO.PR

## Distribution of returns:
# ATTENTION: THE NAMING OF THE VECTORS IS ESSENTIAL AND HAS TO FOLLOW THE SEQUENCE "WEIGHTINGTYPE.COSTS"
DISTRIBUTION(res = as.data.frame(do.call(cbind,list(EquallyWeightedLO.NoCosts = res.PR.LO$ret.portf.EW$`No costs`$`Monthly Portfolio Return`,
                                                    EquallyWeightedLO.50bps = res.PR.LO$ret.portf.EW$`50bps`$`Monthly Portfolio Return`,
                                                    RankWeightedLO.NoCosts = res.PR.LO$ret.portf.RW$`No costs`$`Monthly Portfolio Return`,
                                                    RankWeightedLO.50bps = res.PR.LO$ret.portf.RW$`50bps`$`Monthly Portfolio Return`))))

## Positional exposure:
# EW:
EXPOSURE(weights.init = portf.EW.all, horizon = horizon, y_test = y_test$y_test, asset.classes = asset.classes)
# RW:
EXPOSURE(weights.init = portf.RW.all, horizon = horizon, y_test = y_test$y_test, asset.classes = asset.classes)



## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
train.rows <- Train.Rows(ret = ret, train.perc = train.perc)
y_test <- Test_DS(ret = ret, train.rows = train.rows, horizon = horizon)
counter = 1
t = 1
portf.EW.all <- list()
portf.RW.all <- list()
all.ret.mon.portf.EW <- NULL
all.ret.mon.portf.RW <- NULL
res.PR.L.S <- NULL

while(counter < nrow(y_test$y_test)){
  
  #print(t)
  
  res.PR.L.S <- LONG.SHORT(y_test = y_test$y_test, cum.pred = mean.ret.cum.all, disc.positive = disc.positive, 
                           use.long.short = use.long.short, lend = lend, counter = counter, t = t, horizon = horizon, 
                           portf.EW.all = portf.EW.all, portf.RW.all = portf.RW.all, all.ret.mon.portf.EW = all.ret.mon.portf.EW, 
                           all.ret.mon.portf.RW = all.ret.mon.portf.RW, fix.tc = fix.tc, trans.costs = trans.costs, 
                           TER.monthly = TER.monthly, res.all = res.PR.L.S)
  
  t <- res.PR.L.S$t
  counter <- res.PR.L.S$counter
  # Extract initial weights
  portf.EW.all <- res.PR.L.S$portf.EW.all
  portf.RW.all <- res.PR.L.S$portf.RW.all
  # All other performance results
  all.ret.mon.portf.EW <- res.PR.L.S$ret.portf.EW$`No costs`$`Monthly Portfolio Return`
  all.ret.mon.portf.RW <- res.PR.L.S$ret.portf.RW$`No costs`$`Monthly Portfolio Return`
}

## Summary statistics:
# EW:
stat.EW.L.S.PR <- STATISTICS(res = res.PR.L.S$ret.portf.EW, RF = rf, VaR.CI = VaR.CI, horizon = horizon, trans.costs = trans.costs)
# RW:
stat.RW.L.S.PR <- STATISTICS(res = res.PR.L.S$ret.portf.RW, RF = rf, VaR.CI = VaR.CI, horizon = horizon, trans.costs = trans.costs)
LS.PR <- list(Equally.Weighted.L.S = stat.EW.L.S.PR, Rank.Weighted.L.S = stat.RW.L.S.PR)
LS.PR

## Distribution of returns:
# ATTENTION: THE NAMING OF THE VECTORS IS ESSENTIAL AND HAS TO FOLLOW THE SEQUENCE "WEIGHTINGTYPE.COSTS"
DISTRIBUTION(res = as.data.frame(do.call(cbind,list(EquallyWeightedLO.NoCosts = res.PR.L.S$ret.portf.EW$`No costs`$`Monthly Portfolio Return`,
                                                    EquallyWeightedLO.50bps = res.PR.L.S$ret.portf.EW$`50bps`$`Monthly Portfolio Return`,
                                                    RankWeightedLO.NoCosts = res.PR.L.S$ret.portf.RW$`No costs`$`Monthly Portfolio Return`,
                                                    RankWeightedLO.50bps = res.PR.L.S$ret.portf.RW$`50bps`$`Monthly Portfolio Return`))))

## Positional exposure:
# EW:
EXPOSURE(weights.init = portf.EW.all, horizon = horizon, y_test = y_test$y_test, asset.classes = asset.classes)
# RW:
EXPOSURE(weights.init = portf.RW.all, horizon = horizon, y_test = y_test$y_test, asset.classes = asset.classes)



## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
train.rows <- Train.Rows(ret = ret, train.perc = train.perc)
y_test <- Test_DS(ret = ret, train.rows = train.rows, horizon = horizon)
counter = 1
t = 1
portf.sample.all <- list()
portf.shrink.all <- list()
all.ret.mon.portf.sample <- NULL
all.ret.mon.portf.shrink <- NULL
res.MV.PR <- NULL

while(counter < nrow(y_test$y_test)){
  
  #print(t)
  
  res.MV.PR <- MIN.VAR(ret = ret, train.rows = train.rows, y_test = y_test$y_test, cum.pred = mean.ret.cum.all,
                       use.minvar = use.minvar,counter = counter, t = t, horizon = horizon,
                       portf.shrink.all = portf.shrink.all, portf.sample.all = portf.sample.all,trans.costs = trans.costs, 
                       TER.monthly = TER.monthly, all.ret.mon.portf.shrink = all.ret.mon.portf.shrink,
                       all.ret.mon.portf.sample = all.ret.mon.portf.sample, limit = limit, fix.tc = fix.tc, 
                       min.max = min.max, res.all = res.MV.PR)
  
  train.rows <- res.MV.PR$train.rows
  t <- res.MV.PR$t
  counter <- res.MV.PR$counter
  # Extract initial weights
  portf.sample.all <- res.MV.PR$portf.sample.all
  portf.shrink.all <- res.MV.PR$portf.shrink.all
  # All other performance results
  all.ret.mon.portf.sample<- res.MV.PR$ret.portf.sample$`No costs`$`Monthly Portfolio Return`
  all.ret.mon.portf.shrink <- res.MV.PR$ret.portf.shrink$`No costs`$`Monthly Portfolio Return`
}

## Summary statistics:
# EW:
stat.MV.sample.PR <- STATISTICS(res = res.MV.PR$ret.portf.sample, RF = rf, VaR.CI = VaR.CI, horizon = horizon, trans.costs = trans.costs)
# RW:
stat.MV.shrink.PR <- STATISTICS(res = res.MV.PR$ret.portf.shrink, RF = rf, VaR.CI = VaR.CI, horizon = horizon, trans.costs = trans.costs)
MV.PR <- list(sample.MV = stat.MV.sample.PR, shrink.MV = stat.MV.shrink.PR)
MV.PR

## Distribution of returns:
# ATTENTION: THE NAMING OF THE VECTORS IS ESSENTIAL AND HAS TO FOLLOW THE SEQUENCE "WEIGHTINGTYPE.COSTS"
DISTRIBUTION(res = as.data.frame(do.call(cbind,list(SampleMV.NoCosts = res.MV.PR$ret.portf.sample$`No costs`$`Monthly Portfolio Return`,
                                                    SampleMV.50bps = res.MV.PR$ret.portf.sample$`50bps`$`Monthly Portfolio Return`,
                                                    ShrinkMV.NoCosts = res.MV.PR$ret.portf.shrink$`No costs`$`Monthly Portfolio Return`,
                                                    ShrinkMV.50bps = res.MV.PR$ret.portf.shrink$`50bps`$`Monthly Portfolio Return`))))

## Positional exposure:
# EW:
EXPOSURE(weights.init = portf.sample.all, horizon = horizon, y_test = y_test$y_test, asset.classes = asset.classes)
# RW:
EXPOSURE(weights.init = portf.shrink.all, horizon = horizon, y_test = y_test$y_test, asset.classes = asset.classes)



## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
# If no Z-Score trans is wished
tmp <- ret
# If Z-score transformation is wished
if(trans.Z == T){
  tmp <- Z.TRANS(ret = ret, Names = Names, last = last)
}
train.rows <- Train.Rows(ret = tmp, train.perc = train.perc)
dates <- index(Test_DS(ret = tmp, train.rows = train.rows, horizon = horizon)$y_test)
y_test <- Test_DS(ret = ret, train.rows = train.rows, horizon = horizon)
y_test$y_test <- y_test$y_test[dates,]
counter = 1
t = 1
portf.EW.all <- list()
portf.RW.all <- list()
all.ret.mon.portf.EW <- NULL
all.ret.mon.portf.RW <- NULL
res.RF.LO <- NULL
cumulative.pred <- as.matrix(cumulative.pred)
colnames(cumulative.pred) <- colnames(mean.ret.cum.all)

## Start the backtest
while(counter < nrow(y_test$y_test)){
  
  #print(t)
  
  res.RF.LO <- LONG.ONLY(y_test = y_test$y_test, cum.pred = cumulative.pred, use.long.only = use.long.only,
                          counter = counter, t = t, horizon = horizon, portf.EW.all = portf.EW.all, portf.RW.all = portf.RW.all,
                          all.ret.mon.portf.EW = all.ret.mon.portf.EW, all.ret.mon.portf.RW = all.ret.mon.portf.RW, 
                          fix.tc = fix.tc, trans.costs = trans.costs, TER.monthly = TER.monthly, res.all = res.RF.LO)
  
  t <- res.RF.LO$t
  counter <- res.RF.LO$counter
  # Extract initial weights
  portf.EW.all <- res.RF.LO$portf.EW.all
  portf.RW.all <- res.RF.LO$portf.RW.all
  # All other performance results
  all.ret.mon.portf.EW <- res.RF.LO$ret.portf.EW$`No costs`$`Monthly Portfolio Return`
  all.ret.mon.portf.RW <- res.RF.LO$ret.portf.RW$`No costs`$`Monthly Portfolio Return`
}

## Summary statistics:
# EW:
stat.EW.LO.RF <- STATISTICS(res = res.RF.LO$ret.portf.EW, RF = rf, VaR.CI = VaR.CI, horizon = horizon, trans.costs = trans.costs)
# RW:
stat.RW.LO.RF <- STATISTICS(res = res.RF.LO$ret.portf.RW, RF = rf, VaR.CI = VaR.CI, horizon = horizon, trans.costs = trans.costs)
LO.RF <- list(Equally.Weighted.L.O = stat.EW.LO.RF, Rank.Weighted.L.O = stat.RW.LO.RF)
LO.RF

## Distribution of returns:
DISTRIBUTION(res = as.data.frame(do.call(cbind,list(EquallyWeightedRF.NoCosts = res.RF.LO$ret.portf.EW$`No costs`$`Monthly Portfolio Return`,
                                                    EquallyWeightedRF.50bps = res.RF.LO$ret.portf.EW$`50bps`$`Monthly Portfolio Return`,
                                                    RankWeightedRF.NoCosts = res.RF.LO$ret.portf.RW$`No costs`$`Monthly Portfolio Return`,
                                                    RankWeightedRF.50bps = res.RF.LO$ret.portf.RW$`50bps`$`Monthly Portfolio Return`))))

## Positional exposure:
# EW:
EXPOSURE(weights.init = portf.EW.all, horizon = horizon, y_test = y_test$y_test, asset.classes = asset.classes)
# RW:
EXPOSURE(weights.init = portf.RW.all, horizon = horizon, y_test = y_test$y_test, asset.classes = asset.classes)



## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
train.rows <- Train.Rows(ret = ret, train.perc = train.perc)
y_test <- Test_DS(ret = ret, train.rows = train.rows, horizon = horizon)
counter = 1
t = 1
portf.EW.all <- list()
portf.RW.all <- list()
all.ret.mon.portf.EW <- NULL
all.ret.mon.portf.RW <- NULL
res.RF.L.S <- NULL
cumulative.pred <- as.matrix(cumulative.pred)
colnames(cumulative.pred) <- colnames(mean.ret.cum.all)

while(counter < nrow(y_test$y_test)){
  
  #print(t)
  
  res.RF.L.S <- LONG.SHORT(y_test = y_test$y_test, cum.pred = cumulative.pred, use.long.short = use.long.short, lend = lend,
                           counter = counter, t = t, horizon = horizon, portf.EW.all = portf.EW.all, portf.RW.all = portf.RW.all,
                           all.ret.mon.portf.EW = all.ret.mon.portf.EW, all.ret.mon.portf.RW = all.ret.mon.portf.RW, fix.tc = fix.tc,
                           trans.costs = trans.costs, TER.monthly = TER.monthly, res.all = res.RF.L.S)
  
  t <- res.RF.L.S$t
  counter <- res.RF.L.S$counter
  # Extract initial weights
  portf.EW.all <- res.RF.L.S$portf.EW.all
  portf.RW.all <- res.RF.L.S$portf.RW.all
  # All other performance results
  all.ret.mon.portf.EW <- res.RF.L.S$ret.portf.EW$`No costs`$`Monthly Portfolio Return`
  all.ret.mon.portf.RW <- res.RF.L.S$ret.portf.RW$`No costs`$`Monthly Portfolio Return`
}

## Summary statistics:
# EW:
stat.EW.L.S.RF <- STATISTICS(res = res.RF.L.S$ret.portf.EW, RF = rf, VaR.CI = VaR.CI, horizon = horizon, trans.costs = trans.costs)
# RW:
stat.RW.L.S.RF <- STATISTICS(res = res.RF.L.S$ret.portf.RW, RF = rf, VaR.CI = VaR.CI, horizon = horizon, trans.costs = trans.costs)
LS.RF <- list(Equally.Weighted.L.S = stat.EW.L.S.RF, Rank.Weighted.L.S = stat.RW.L.S.RF)
LS.RF

## Distribution of returns:
# ATTENTION: THE NAMING OF THE VECTORS IS ESSENTIAL AND HAS TO FOLLOW THE SEQUENCE "WEIGHTINGTYPE.COSTS"
DISTRIBUTION(res = as.data.frame(do.call(cbind,list(EquallyWeightedLO.NoCosts = res.RF.L.S$ret.portf.EW$`No costs`$`Monthly Portfolio Return`,
                                                    EquallyWeightedLO.50bps = res.RF.L.S$ret.portf.EW$`50bps`$`Monthly Portfolio Return`,
                                                    RankWeightedLO.NoCosts = res.RF.L.S$ret.portf.RW$`No costs`$`Monthly Portfolio Return`,
                                                    RankWeightedLO.50bps = res.RF.L.S$ret.portf.RW$`50bps`$`Monthly Portfolio Return`))))

## Positional exposure:
# EW:
EXPOSURE(weights.init = portf.EW.all, horizon = horizon, y_test = y_test$y_test, asset.classes = asset.classes)
# RW:
EXPOSURE(weights.init = portf.RW.all, horizon = horizon, y_test = y_test$y_test, asset.classes = asset.classes)



## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
train.rows <- Train.Rows(ret = ret, train.perc = train.perc)
y_test <- Test_DS(ret = ret, train.rows = train.rows, horizon = horizon)
counter = 1
t = 1
portf.sample.all <- list()
portf.shrink.all <- list()
all.ret.mon.portf.sample <- NULL
all.ret.mon.portf.shrink <- NULL
res.MV.RF <- NULL
cumulative.pred <- as.matrix(cumulative.pred)
colnames(cumulative.pred) <- colnames(mean.ret.cum.all)

while(counter < nrow(y_test$y_test)){
  
  #print(t)
  
  res.MV.RF <- MIN.VAR(ret = ret, train.rows = train.rows, y_test = y_test$y_test, cum.pred = cumulative.pred, 
                       use.minvar = use.minvar, counter = counter, t = t, horizon = horizon,
                       portf.shrink.all = portf.shrink.all, portf.sample.all = portf.sample.all,
                       trans.costs = trans.costs, TER.monthly = TER.monthly, all.ret.mon.portf.shrink = all.ret.mon.portf.shrink,
                       all.ret.mon.portf.sample = all.ret.mon.portf.sample, limit = limit, fix.tc = fix.tc, min.max = min.max, 
                       res.all = res.MV.RF)
  
  train.rows <- res.MV.RF$train.rows
  t <- res.MV.RF$t
  counter <- res.MV.RF$counter
  # Extract initial weights
  portf.sample.all <- res.MV.RF$portf.sample.all
  portf.shrink.all <- res.MV.RF$portf.shrink.all
  # All other performance results
  all.ret.mon.portf.sample<- res.MV.RF$ret.portf.sample$`No costs`$`Monthly Portfolio Return`
  all.ret.mon.portf.shrink <- res.MV.RF$ret.portf.shrink$`No costs`$`Monthly Portfolio Return`
}

## Summary statistics:
# EW:
stat.MV.sample.RF <- STATISTICS(res = res.MV.RF$ret.portf.sample, RF = rf, VaR.CI = VaR.CI, horizon = horizon, trans.costs = trans.costs)
# RW:
stat.MV.shrink.RF <- STATISTICS(res = res.MV.RF$ret.portf.shrink, RF = rf, VaR.CI = VaR.CI, horizon = horizon, trans.costs = trans.costs)
MV.RF <- list(sample.MV = stat.MV.sample.RF, shrink.MV = stat.MV.shrink.RF)
MV.RF

## Distribution of returns:
# ATTENTION: THE NAMING OF THE VECTORS IS ESSENTIAL AND HAS TO FOLLOW THE SEQUENCE "WEIGHTINGTYPE.COSTS"
DISTRIBUTION(res = as.data.frame(do.call(cbind,list(SampleMV.NoCosts = res.MV.RF$ret.portf.sample$`No costs`$`Monthly Portfolio Return`,
                                                    SampleMV.50bps = res.MV.RF$ret.portf.sample$`50bps`$`Monthly Portfolio Return`,
                                                    ShrinkMV.NoCosts = res.MV.RF$ret.portf.shrink$`No costs`$`Monthly Portfolio Return`,
                                                    ShrinkMV.50bps = res.MV.RF$ret.portf.shrink$`50bps`$`Monthly Portfolio Return`))))

## Positional exposure:
# EW:
EXPOSURE(weights.init = portf.sample.all, horizon = horizon, y_test = y_test$y_test, asset.classes = asset.classes)
# RW:
EXPOSURE(weights.init = portf.shrink.all, horizon = horizon, y_test = y_test$y_test, asset.classes = asset.classes)



## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
train.rows <- Train.Rows(ret = ret, train.perc = train.perc)
y_test <- Test_DS(ret = ret, train.rows = train.rows, horizon = horizon)
counter = 1
t = 1
portf.sample.all <- list()
portf.shrink.all <- list()
all.ret.mon.portf.sample <- NULL
all.ret.mon.portf.shrink <- NULL
res.MV.raw <- NULL
use.minvar = 1 # Use all ETFs
min.max = c(0,1)

while(counter < nrow(y_test$y_test)){
  
  #print(t)
  
  res.MV.raw <- MIN.VAR(ret = ret, train.rows = train.rows, y_test = y_test$y_test, cum.pred = mean.ret.cum.all,
                        use.minvar = use.minvar,counter = counter, t = t, horizon = horizon, 
                        portf.shrink.all = portf.shrink.all, portf.sample.all = portf.sample.all,
                        trans.costs = trans.costs, TER.monthly = TER.monthly, all.ret.mon.portf.shrink = all.ret.mon.portf.shrink,
                        all.ret.mon.portf.sample = all.ret.mon.portf.sample, limit = limit, fix.tc = fix.tc,
                        min.max = min.max, res.all = res.MV.raw)
  
  train.rows <- res.MV.raw$train.rows
  t <- res.MV.raw$t
  counter <- res.MV.raw$counter
  # Extract initial weights
  portf.sample.all <- res.MV.raw$portf.sample.all
  portf.shrink.all <- res.MV.raw$portf.shrink.all
  # All other performance results
  all.ret.mon.portf.sample<- res.MV.raw$ret.portf.sample$`No costs`$`Monthly Portfolio Return`
  all.ret.mon.portf.shrink <- res.MV.raw$ret.portf.shrink$`No costs`$`Monthly Portfolio Return`
}

## Summary statistics:
# EW:
stat.MV.sample <- STATISTICS(res = res.MV.raw$ret.portf.sample, RF = rf, VaR.CI = VaR.CI, horizon = horizon, trans.costs = trans.costs)
# RW:
stat.MV.shrink <- STATISTICS(res = res.MV.raw$ret.portf.shrink, RF = rf, VaR.CI = VaR.CI, horizon = horizon, trans.costs = trans.costs)
list(sample.MV = stat.MV.sample, shrink.MV = stat.MV.shrink)

## Distribution of returns:
# ATTENTION: THE NAMING OF THE VECTORS IS ESSENTIAL AND HAS TO FOLLOW THE SEQUENCE "WEIGHTINGTYPE.COSTS"
DISTRIBUTION(res = as.data.frame(do.call(cbind,list(SampleMV.NoCosts = res.MV.raw$ret.portf.sample$`No costs`$`Monthly Portfolio Return`,
                                                    SampleMV.50bps = res.MV.raw$ret.portf.sample$`50bps`$`Monthly Portfolio Return`,
                                                    ShrinkMV.NoCosts = res.MV.raw$ret.portf.shrink$`No costs`$`Monthly Portfolio Return`,
                                                    ShrinkMV.50bps = res.MV.raw$ret.portf.shrink$`50bps`$`Monthly Portfolio Return`))))

## Positional exposure:
# EW:
EXPOSURE(weights.init = portf.sample.all, horizon = horizon, y_test = y_test$y_test, asset.classes = asset.classes)
# RW:
EXPOSURE(weights.init = portf.shrink.all, horizon = horizon, y_test = y_test$y_test, asset.classes = asset.classes)



## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
BAH.noCost <- WEALTH.SIMULATION(ret = res.bench$`no costs`$ret.noCosts, init = init)
BAH.50bps <- WEALTH.SIMULATION(ret = res.bench$`50bps`$ret.Costs, init = init)


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
# Extract out-of-sample dates
date <- index(BAH.noCost$Wealth)

# Representation of EW and RW strategy each with noCost and 50bps scenario in one plot
ggplot(BAH.noCost$Wealth) +
  geom_line(aes(x=date, y = Wealth, color = "Buy-and-Hold Benchmark no Costs")) +
  geom_line(aes(x=date, y = BAH.50bps$Wealth, color = "Buy-and-Hold Benchmark 50bps")) +
  scale_x_date(labels = date_format("%d-%m-%Y")) +
  scale_color_manual("Legend", values = c("Buy-and-Hold Benchmark no Costs" = "skyblue", "Buy-and-Hold Benchmark 50bps" = "orange")) +
  labs(title = "Wealth Development Buy-and-Hold Benchmark", x = "Dates", y = "Wealth") +
  theme_minimal()


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
# Show Maximum Drawdowns
rbind(BAHBenchmark.noCosts = BAH.noCost$maxDraw, BAHBenchmark.50bps = BAH.50bps$maxDraw)


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
# Display absolute and relative returns in a table:
returns.BAHBench.Wealth <- list(BAH.abs.noCost = BAH.noCost$abs.ret, 
                                  BAH.abs.50bps = BAH.50bps$abs.ret,
                                  BAH.rel.noCost = BAH.noCost$rel.ret, 
                                  BAH.rel.50bps = BAH.50bps$rel.ret)

returns.BAHBench.Wealth <- bind_rows(lapply(names(returns.BAHBench.Wealth), function(name){
  data_frame <- data.frame(value = returns.BAHBench.Wealth[[name]])
  data_frame$Type <- name  # Add the list name as a new column
  return(data_frame)
}))

returns.BAHBench.Wealth <- returns.BAHBench.Wealth %>%
    mutate(
      Strategy = ifelse(grepl("^BAH", Type), "Buy-and-Hold"),
      Return_Type = ifelse(grepl("rel", Type), "rel", "abs"),
      Cost = ifelse(grepl("noCost", Type), "noCost", "50bps")
      ) %>%
  select(-Type)

# Reshape the data to a wide format table
returns.BAHBench.Wealth <- returns.BAHBench.Wealth %>%
  pivot_wider(names_from = Return_Type, values_from = value) %>%
  arrange(Strategy, Cost)

returns.BAHBench.Wealth



## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
eqBench.noCost <- WEALTH.SIMULATION(ret = res.50_50$ret.portf.EW$`No costs`$`Monthly Portfolio Return`, init = init)
eqBench.50bps <- WEALTH.SIMULATION(ret = res.50_50$ret.portf.EW$`50bps`$`Monthly Portfolio Return`, init = init)


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
# Extract out-of-sample dates
date <- index(eqBench.noCost$Wealth)

# Representation of EW and RW strategy each with noCost and 50bps scenario in one plot
ggplot(eqBench.noCost$Wealth) +
  geom_line(aes(x=date, y = Wealth, color = "50-50 Benchmark no Costs")) +
  geom_line(aes(x=date, y = eqBench.50bps$Wealth, color = "50-50 Benchmark 50bps")) +
  scale_x_date(labels = date_format("%d-%m-%Y")) +
  scale_color_manual("Legend", values = c("50-50 Benchmark no Costs" = "skyblue", "50-50 Benchmark 50bps" = "orange")) +
  labs(title = "Wealth Development 50-50 Benchmark", x = "Dates", y = "Wealth") +
  theme_minimal()


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
# Show Maximum Drawdowns
rbind(EquallyWeightedBenchmark.noCosts = eqBench.noCost$maxDraw, EquallyWeightedBenchmark.50bps = eqBench.50bps$maxDraw)


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
# Display absolute and relative returns in a table:
returns.50_50Bench.Wealth <- list(EquallyWeightedBenchmark.abs.noCost = eqBench.noCost$abs.ret, 
                                  EquallyWeightedBenchmark.abs.50bps = eqBench.50bps$abs.ret,
                                  EquallyWeightedBenchmark.rel.noCost = eqBench.noCost$rel.ret, 
                                  EquallyWeightedBenchmark.rel.50bps = eqBench.50bps$rel.ret)

returns.50_50Bench.Wealth <- bind_rows(lapply(names(returns.50_50Bench.Wealth), function(name){
  data_frame <- data.frame(value = returns.50_50Bench.Wealth[[name]])
  data_frame$Type <- name  # Add the list name as a new column
  return(data_frame)
}))

returns.50_50Bench.Wealth <- returns.50_50Bench.Wealth %>%
    mutate(
      Strategy = ifelse(grepl("^EquallyWeightedBenchmark", Type), "Benchmark"),
      Return_Type = ifelse(grepl("rel", Type), "rel", "abs"),
      Cost = ifelse(grepl("noCost", Type), "noCost", "50bps")
      ) %>%
  select(-Type)

# Reshape the data to a wide format table
returns.50_50Bench.Wealth <- returns.50_50Bench.Wealth %>%
  pivot_wider(names_from = Return_Type, values_from = value) %>%
  arrange(Strategy, Cost)

returns.50_50Bench.Wealth



## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
# Compute Wealth
EW.PR.noCost <- WEALTH.SIMULATION(ret = res.PR.LO$ret.portf.EW$`No costs`$`Monthly Portfolio Return`, init = init)
EW.PR.50bps <- WEALTH.SIMULATION(ret = res.PR.LO$ret.portf.EW$`50bps`$`Monthly Portfolio Return`, init = init)
RW.PR.noCost <- WEALTH.SIMULATION(ret = res.PR.LO$ret.portf.RW$`No costs`$`Monthly Portfolio Return`, init = init)
RW.PR.50bps <- WEALTH.SIMULATION(ret = res.PR.LO$ret.portf.RW$`50bps`$`Monthly Portfolio Return`, init = init)


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
# Extract out-of-sample dates
date <- index(EW.PR.noCost$Wealth)

# Representation of EW and RW strategy each with noCost and 50bps scenario in one plot
ggplot(EW.PR.noCost$Wealth) +
  geom_line(aes(x=date, y = Wealth, color = "Equal weights Wealth no costs")) +
  geom_line(aes(x=date, y = EW.PR.50bps$Wealth, color = "Equal weights Wealth 50bps")) +
  geom_line(aes(x=date, y = RW.PR.noCost$Wealth, color = "Rank weights Wealth no costs")) +
  geom_line(aes(x=date, y = RW.PR.50bps$Wealth, color = "Rank weights Wealth 50bps")) +
  scale_x_date(labels = date_format("%d-%m-%Y")) +
  scale_color_manual("Legend", values = c("Equal weights Wealth no costs" = "blue", "Equal weights Wealth 50bps" = "red",
                                          "Rank weights Wealth no costs" = "skyblue", "Rank weights Wealth 50bps" = "orange")) +
  labs(title = "Wealth Development Long Only Prevailing Mean", x = "Dates", y = "Wealth") +
  theme_minimal()


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
# Show Maximum Drawdowns
rbind(EquallyWeighted.noCosts = EW.PR.noCost$maxDraw, EquallyWeighted.50bps = EW.PR.50bps$maxDraw,
      RankWeighted.noCosts = RW.PR.noCost$maxDraw, RankWeighted.50bps = RW.PR.50bps$maxDraw)


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
# Display absolute and relative returns in a table:
returns.PR.Wealth <- list(EW.abs.noCost = EW.PR.noCost$abs.ret, EW.abs.50bps = EW.PR.50bps$abs.ret,
                          EW.rel.noCost = EW.PR.noCost$rel.ret, EW.rel.50bps = EW.PR.50bps$rel.ret,
                          RW.abs.noCost = RW.PR.noCost$abs.ret, RW.abs.50bps = RW.PR.50bps$abs.ret,
                          RW.rel.noCost = RW.PR.noCost$rel.ret, RW.rel.50bps = RW.PR.50bps$rel.ret)

returns.PR.Wealth <- bind_rows(lapply(names(returns.PR.Wealth), function(name){
  data_frame <- data.frame(value = returns.PR.Wealth[[name]])
  data_frame$Type <- name  # Add the list name as a new column
  return(data_frame)
}))

# Extract `EW`/`RW`, `rel`/`abs`, and `noCost`/`50bps` from the `Type` column
returns.PR.Wealth <- returns.PR.Wealth %>%
    mutate(
      Strategy = ifelse(grepl("^EW", Type), "EW", "RW"),
      Return_Type = ifelse(grepl("rel", Type), "rel", "abs"),
      Cost = ifelse(grepl("noCost", Type), "noCost", "50bps")
      ) %>%
  select(-Type)  # Remove the original Type column

# Reshape the data to a wide format table
returns.PR.Wealth <- returns.PR.Wealth %>%
  pivot_wider(names_from = Return_Type, values_from = value) %>%
  arrange(Strategy, Cost)

returns.PR.Wealth



## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
# Compute Wealth
EW.PR.noCost <- WEALTH.SIMULATION(ret = res.PR.L.S$ret.portf.EW$`No costs`$`Monthly Portfolio Return`, init = init)
EW.PR.50bps <- WEALTH.SIMULATION(ret = res.PR.L.S$ret.portf.EW$`50bps`$`Monthly Portfolio Return`, init = init)
RW.PR.noCost <- WEALTH.SIMULATION(ret = res.PR.L.S$ret.portf.RW$`No costs`$`Monthly Portfolio Return`, init = init)
RW.PR.50bps <- WEALTH.SIMULATION(ret = res.PR.L.S$ret.portf.RW$`50bps`$`Monthly Portfolio Return`, init = init)


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
# Extract out-of-sample dates
date <- index(EW.PR.noCost$Wealth)

# Representation of EW and RW strategy each with noCost and 50bps scenario in one plot
ggplot(EW.PR.noCost$Wealth) +
  geom_line(aes(x=date, y = Wealth, color = "Equal weights Wealth no costs")) +
  geom_line(aes(x=date, y = EW.PR.50bps$Wealth, color = "Equal weights Wealth 50bps")) +
  geom_line(aes(x=date, y = RW.PR.noCost$Wealth, color = "Rank weights Wealth no costs")) +
  geom_line(aes(x=date, y = RW.PR.50bps$Wealth, color = "Rank weights Wealth 50bps")) +
  scale_x_date(labels = date_format("%d-%m-%Y")) +
  scale_color_manual("Legend", values = c("Equal weights Wealth no costs" = "blue", "Equal weights Wealth 50bps" = "red",
                                          "Rank weights Wealth no costs" = "skyblue", "Rank weights Wealth 50bps" = "orange")) +
  labs(title = "Wealth Development Long-Short Prevailing Mean", x = "Dates", y = "Wealth") +
  theme_minimal()


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
# Show Maximum Drawdowns
rbind(EquallyWeighted.noCosts = EW.PR.noCost$maxDraw, EquallyWeighted.50bps = EW.PR.50bps$maxDraw,
      RankWeighted.noCosts = RW.PR.noCost$maxDraw, RankWeighted.50bps = RW.PR.50bps$maxDraw)


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
# Display absolute and relative returns in a table:
returns.PR.Wealth <- list(EW.abs.noCost = EW.PR.noCost$abs.ret, EW.abs.50bps = EW.PR.50bps$abs.ret,
                          EW.rel.noCost = EW.PR.noCost$rel.ret, EW.rel.50bps = EW.PR.50bps$rel.ret,
                          RW.abs.noCost = RW.PR.noCost$abs.ret, RW.abs.50bps = RW.PR.50bps$abs.ret,
                          RW.rel.noCost = RW.PR.noCost$rel.ret, RW.rel.50bps = RW.PR.50bps$rel.ret)

returns.PR.Wealth <- bind_rows(lapply(names(returns.PR.Wealth), function(name){
  data_frame <- data.frame(value = returns.PR.Wealth[[name]])
  data_frame$Type <- name  # Add the list name as a new column
  return(data_frame)
}))

# Extract `EW`/`RW`, `rel`/`abs`, and `noCost`/`50bps` from the `Type` column
returns.PR.Wealth <- returns.PR.Wealth %>%
    mutate(Strategy = ifelse(grepl("^EW", Type), "EW", "RW"),
           Return_Type = ifelse(grepl("rel", Type), "rel", "abs"),
           Cost = ifelse(grepl("noCost", Type), "noCost", "50bps")
           ) %>%
  select(-Type)  # Remove the original Type column

# Reshape the data to a wide format table
returns.PR.Wealth <- returns.PR.Wealth %>%
  pivot_wider(names_from = Return_Type, values_from = value) %>%
  arrange(Strategy, Cost)

returns.PR.Wealth



## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
# Compute Wealth
MVsample.PR.noCost <- WEALTH.SIMULATION(ret = res.MV.PR$ret.portf.sample$`No costs`$`Monthly Portfolio Return`, init = init)
MVsample.PR.50bps <- WEALTH.SIMULATION(ret = res.MV.PR$ret.portf.sample$`50bps`$`Monthly Portfolio Return`, init = init)
MVshrink.PR.noCost <- WEALTH.SIMULATION(ret = res.MV.PR$ret.portf.shrink$`No costs`$`Monthly Portfolio Return`, init = init)
MVshrink.PR.50bps <- WEALTH.SIMULATION(ret = res.MV.PR$ret.portf.shrink$`50bps`$`Monthly Portfolio Return`, init = init)


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
# Extract out-of-sample dates
date <- index(MVsample.PR.noCost$Wealth)

# Representation of EW and RW strategy each with noCost and 50bps scenario in one plot
ggplot(MVsample.PR.noCost$Wealth) +
  geom_line(aes(x=date, y = Wealth, color = "Sample MV Wealth no costs")) +
  geom_line(aes(x=date, y = MVsample.PR.50bps$Wealth, color = "Sample MV Wealth 50bps")) +
  geom_line(aes(x=date, y = MVshrink.PR.noCost$Wealth, color = "Shrink MV Wealth no costs")) +
  geom_line(aes(x=date, y = MVshrink.PR.50bps$Wealth, color = "Shrink MV Wealth 50bps")) +
  scale_x_date(labels = date_format("%d-%m-%Y")) +
  scale_color_manual("Legend", values = c("Sample MV Wealth no costs" = "blue", "Sample MV Wealth 50bps" = "red",
                                          "Shrink MV Wealth no costs" = "skyblue", "Shrink MV Wealth 50bps" = "orange")) +
  labs(title = "Wealth Development MV Prevailing Mean", x = "Dates", y = "Wealth") +
  theme_minimal()


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
# Show Maximum Drawdowns
rbind(SampleMV.noCosts = MVsample.PR.noCost$maxDraw, SampleMV.50bps = MVsample.PR.50bps$maxDraw,
      ShrinkMV.noCosts = MVshrink.PR.noCost$maxDraw, ShrinkMV.PR.50bps = MVshrink.PR.50bps$maxDraw)


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
# Display absolute and relative returns in a table:
returns.MV.PR.Wealth <- list(MVsample.abs.noCost = MVsample.PR.noCost$abs.ret, MVsample.abs.50bps = MVsample.PR.50bps$abs.ret,
                             MVsample.rel.noCost = MVsample.PR.noCost$rel.ret, MVsample.rel.50bps = MVsample.PR.50bps$rel.ret,
                             MVshrink.abs.noCost = MVshrink.PR.noCost$abs.ret, MVshrink.abs.50bps = MVshrink.PR.50bps$abs.ret,
                             MVshrink.rel.noCost = MVshrink.PR.noCost$rel.ret, MVshrink.rel.50bps = MVshrink.PR.50bps$rel.ret)

returns.MV.PR.Wealth <- bind_rows(lapply(names(returns.MV.PR.Wealth), function(name){
  data_frame <- data.frame(value = returns.MV.PR.Wealth[[name]])
  data_frame$Type <- name  # Add the list name as a new column
  return(data_frame)
}))

# Extract `EW`/`RW`, `rel`/`abs`, and `noCost`/`50bps` from the `Type` column
returns.MV.PR.Wealth <- returns.MV.PR.Wealth %>%
  mutate(Strategy = ifelse(grepl("^MVsample", Type), "sample", "shrink"),
         Return_Type = ifelse(grepl("rel", Type), "rel", "abs"),
         Cost = ifelse(grepl("noCost", Type), "noCost", "50bps")
         ) %>%
  select(-Type)  # Remove the original Type column

# Reshape the data to a wide format table
returns.MV.PR.Wealth <- returns.MV.PR.Wealth %>%
  pivot_wider(names_from = Return_Type, values_from = value) %>%
  arrange(Strategy, Cost)

returns.MV.PR.Wealth



## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
# Compute Wealth
EW.RF.noCost <- WEALTH.SIMULATION(ret = res.RF.LO$ret.portf.EW$`No costs`$`Monthly Portfolio Return`, init = init)
EW.RF.50bps <- WEALTH.SIMULATION(ret = res.RF.LO$ret.portf.EW$`50bps`$`Monthly Portfolio Return`, init = init)
RW.RF.noCost <- WEALTH.SIMULATION(ret = res.RF.LO$ret.portf.RW$`No costs`$`Monthly Portfolio Return`, init = init)
RW.RF.50bps <- WEALTH.SIMULATION(ret = res.RF.LO$ret.portf.RW$`50bps`$`Monthly Portfolio Return`, init = init)


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
# Extract out-of-sample dates
date <- index(EW.RF.noCost$Wealth)

# Representation of EW and RW strategy each with noCost and 50bps scenario in one plot
ggplot(EW.RF.noCost$Wealth) +
  geom_line(aes(x=date, y = Wealth, color = "Equal weights Wealth no costs")) +
  geom_line(aes(x=date, y = EW.RF.50bps$Wealth, color = "Equal weights Wealth 50bps")) +
  geom_line(aes(x=date, y = RW.RF.noCost$Wealth, color = "Rank weights Wealth no costs")) +
  geom_line(aes(x=date, y = RW.RF.50bps$Wealth, color = "Rank weights Wealth 50bps")) +
  scale_x_date(labels = date_format("%d-%m-%Y")) +
  scale_color_manual("Legend", values = c("Equal weights Wealth no costs" = "blue", "Equal weights Wealth 50bps" = "red",
                                          "Rank weights Wealth no costs" = "skyblue", "Rank weights Wealth 50bps" = "orange")) +
  labs(title = "Wealth Development Long Only Random Forest", x = "Dates", y = "Wealth") +
  theme_minimal()


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
# Show Maximum Drawdowns
rbind(EquallyWeighted.noCosts = EW.RF.noCost$maxDraw, EquallyWeighted.50bps = EW.RF.50bps$maxDraw,
      RankWeighted.noCosts = RW.RF.noCost$maxDraw, RankWeighted.50bps = RW.RF.50bps$maxDraw)


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
# Display absolute and relative returns in a table:
returns.RF.Wealth <- list(EW.abs.noCost = EW.RF.noCost$abs.ret, EW.abs.50bps = EW.RF.50bps$abs.ret,
                          EW.rel.noCost = EW.RF.noCost$rel.ret, EW.rel.50bps = EW.RF.50bps$rel.ret,
                          RW.abs.noCost = RW.RF.noCost$abs.ret, RW.abs.50bps = RW.RF.50bps$abs.ret,
                          RW.rel.noCost = RW.RF.noCost$rel.ret, RW.rel.50bps = RW.RF.50bps$rel.ret)

returns.RF.Wealth <- bind_rows(lapply(names(returns.RF.Wealth), function(name){
  data_frame <- data.frame(value = returns.RF.Wealth[[name]])
  data_frame$Type <- name  # Add the list name as a new column
  return(data_frame)
}))

# Extract `EW`/`RW`, `rel`/`abs`, and `noCost`/`50bps` from the `Type` column
returns.RF.Wealth <- returns.RF.Wealth %>%
    mutate(
      Strategy = ifelse(grepl("^EW", Type), "EW", "RW"),
      Return_Type = ifelse(grepl("rel", Type), "rel", "abs"),
      Cost = ifelse(grepl("50bps", Type), "50bps", "noCost")) %>%
  select(-Type)  # Remove the original Type column

# Reshape the data to a wide format table
returns.RF.Wealth <- returns.RF.Wealth %>%
  pivot_wider(names_from = Return_Type, values_from = value) %>%
  arrange(Strategy, Cost)

returns.RF.Wealth



## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
# Compute Wealth
EW.RF.noCost <- WEALTH.SIMULATION(ret = res.RF.L.S$ret.portf.EW$`No costs`$`Monthly Portfolio Return`, init = init)
EW.RF.50bps <- WEALTH.SIMULATION(ret = res.RF.L.S$ret.portf.EW$`50bps`$`Monthly Portfolio Return`, init = init)
RW.RF.noCost <- WEALTH.SIMULATION(ret = res.RF.L.S$ret.portf.RW$`No costs`$`Monthly Portfolio Return`, init = init)
RW.RF.50bps <- WEALTH.SIMULATION(ret = res.RF.L.S$ret.portf.RW$`50bps`$`Monthly Portfolio Return`, init = init)


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
# Extract out-of-sample dates
date <- index(EW.RF.noCost$Wealth)

# Representation of EW and RW strategy each with noCost and 50bps scenario in one plot
ggplot(EW.RF.noCost$Wealth) +
  geom_line(aes(x=date, y = Wealth, color = "Equal weights Wealth no costs")) +
  geom_line(aes(x=date, y = EW.RF.50bps$Wealth, color = "Equal weights Wealth 50bps")) +
  geom_line(aes(x=date, y = RW.RF.noCost$Wealth, color = "Rank weights Wealth no costs")) +
  geom_line(aes(x=date, y = RW.RF.50bps$Wealth, color = "Rank weights Wealth 50bps")) +
  scale_x_date(labels = date_format("%d-%m-%Y")) +
  scale_color_manual("Legend", values = c("Equal weights Wealth no costs" = "blue", "Equal weights Wealth 50bps" = "red",
                                          "Rank weights Wealth no costs" = "skyblue", "Rank weights Wealth 50bps" = "orange")) +
  labs(title = "Wealth Development Long-Short Random Forest", x = "Dates", y = "Wealth") +
  theme_minimal()


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
# Show Maximum Drawdowns
rbind(EquallyWeighted.noCosts = EW.RF.noCost$maxDraw, EquallyWeighted.50bps = EW.RF.50bps$maxDraw,
      RankWeighted.noCosts = RW.RF.noCost$maxDraw, RankWeighted.50bps = RW.RF.50bps$maxDraw)


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
# Display absolute and relative returns in a table:
returns.RF.Wealth <- list(EW.abs.noCost = EW.RF.noCost$abs.ret, EW.abs.50bps = EW.RF.50bps$abs.ret,
                          EW.rel.noCost = EW.RF.noCost$rel.ret, EW.rel.50bps = EW.RF.50bps$rel.ret,
                          RW.abs.noCost = RW.RF.noCost$abs.ret, RW.abs.50bps = RW.RF.50bps$abs.ret,
                          RW.rel.noCost = RW.RF.noCost$rel.ret, RW.rel.50bps = RW.RF.50bps$rel.ret)

returns.RF.Wealth <- bind_rows(lapply(names(returns.RF.Wealth), function(name){
  data_frame <- data.frame(value = returns.RF.Wealth[[name]])
  data_frame$Type <- name  # Add the list name as a new column
  return(data_frame)
}))

# Extract `EW`/`RW`, `rel`/`abs`, and `noCost`/`50bps` from the `Type` column
returns.RF.Wealth <- returns.RF.Wealth %>%
    mutate(Strategy = ifelse(grepl("^EW", Type), "EW", "RW"),
           Return_Type = ifelse(grepl("rel", Type), "rel", "abs"),
           Cost = ifelse(grepl("noCost", Type), "noCost", "50bps")
           ) %>%
  select(-Type)  # Remove the original Type column

# Reshape the data to a wide format table
returns.RF.Wealth <- returns.RF.Wealth %>%
  pivot_wider(names_from = Return_Type, values_from = value) %>%
  arrange(Strategy, Cost)

returns.RF.Wealth



## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
# Compute Wealth
MVsample.RF.noCost <- WEALTH.SIMULATION(ret = res.MV.RF$ret.portf.sample$`No costs`$`Monthly Portfolio Return`, init = init)
MVsample.RF.50bps <- WEALTH.SIMULATION(ret = res.MV.RF$ret.portf.sample$`50bps`$`Monthly Portfolio Return`, init = init)
MVshrink.RF.noCost <- WEALTH.SIMULATION(ret = res.MV.RF$ret.portf.shrink$`No costs`$`Monthly Portfolio Return`, init = init)
MVshrink.RF.50bps <- WEALTH.SIMULATION(ret = res.MV.RF$ret.portf.shrink$`50bps`$`Monthly Portfolio Return`, init = init)


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
# Extract out-of-sample dates
date <- index(MVsample.RF.noCost$Wealth)

# Representation of EW and RW strategy each with noCost and 50bps scenario in one plot
ggplot(MVsample.RF.noCost$Wealth) +
  geom_line(aes(x=date, y = Wealth, color = "Sample MV Wealth no costs")) +
  geom_line(aes(x=date, y = MVsample.RF.50bps$Wealth, color = "Sample MV Wealth 50bps")) +
  geom_line(aes(x=date, y = MVshrink.RF.noCost$Wealth, color = "Shrink MV Wealth no costs")) +
  geom_line(aes(x=date, y = MVshrink.RF.50bps$Wealth, color = "Shrink MV Wealth 50bps")) +
  scale_x_date(labels = date_format("%d-%m-%Y")) +
  scale_color_manual("Legend", values = c("Sample MV Wealth no costs" = "blue", "Sample MV Wealth 50bps" = "red",
                                          "Shrink MV Wealth no costs" = "skyblue", "Shrink MV Wealth 50bps" = "orange")) +
  labs(title = "Wealth Development MV Random Forest", x = "Dates", y = "Wealth") +
  theme_minimal()


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
# Show Maximum Drawdowns
rbind(SampleMV.noCosts = MVsample.RF.noCost$maxDraw, SampleMV.50bps = MVsample.RF.50bps$maxDraw,
      ShrinkMV.noCosts = MVshrink.RF.noCost$maxDraw, ShrinkMV.PR.50bps = MVshrink.RF.50bps$maxDraw)


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
# Display absolute and relative returns in a table:
returns.MV.RF.Wealth <- list(MVsample.abs.noCost = MVsample.RF.noCost$abs.ret, MVsample.abs.50bps = MVsample.RF.50bps$abs.ret,
                             MVsample.rel.noCost = MVsample.RF.noCost$rel.ret, MVsample.rel.50bps = MVsample.RF.50bps$rel.ret,
                             MVshrink.abs.noCost = MVshrink.RF.noCost$abs.ret, MVshrink.abs.50bps = MVshrink.RF.50bps$abs.ret,
                             MVshrink.rel.noCost = MVshrink.RF.noCost$rel.ret, MVshrink.rel.50bps = MVshrink.RF.50bps$rel.ret)

returns.MV.RF.Wealth <- bind_rows(lapply(names(returns.MV.RF.Wealth), function(name){
  data_frame <- data.frame(value = returns.MV.RF.Wealth[[name]])
  data_frame$Type <- name  # Add the list name as a new column
  return(data_frame)
}))

# Extract `EW`/`RW`, `rel`/`abs`, and `noCost`/`50bps` from the `Type` column
returns.MV.RF.Wealth <- returns.MV.RF.Wealth %>%
  mutate(Strategy = ifelse(grepl("^MVsample", Type), "sample", "shrink"),
         Return_Type = ifelse(grepl("rel", Type), "rel", "abs"),
         Cost = ifelse(grepl("noCost", Type), "noCost", "50bps")
         ) %>%
  select(-Type)  # Remove the original Type column

# Reshape the data to a wide format table
returns.MV.RF.Wealth <- returns.MV.RF.Wealth %>%
  pivot_wider(names_from = Return_Type, values_from = value) %>%
  arrange(Strategy, Cost)

returns.MV.RF.Wealth



## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
# Compute Wealth
MVsample.raw.noCost <- WEALTH.SIMULATION(ret = res.MV.raw$ret.portf.sample$`No costs`$`Monthly Portfolio Return`, init = init)
MVsample.raw.50bps <- WEALTH.SIMULATION(ret = res.MV.raw$ret.portf.sample$`50bps`$`Monthly Portfolio Return`, init = init)
MVshrink.raw.noCost <- WEALTH.SIMULATION(ret = res.MV.raw$ret.portf.shrink$`No costs`$`Monthly Portfolio Return`, init = init)
MVshrink.raw.50bps <- WEALTH.SIMULATION(ret = res.MV.raw$ret.portf.shrink$`50bps`$`Monthly Portfolio Return`, init = init)


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
# Extract out-of-sample dates
date <- index(MVsample.raw.noCost$Wealth)

# Representation of EW and RW strategy each with noCost and 50bps scenario in one plot
ggplot(MVsample.raw.noCost$Wealth) +
  geom_line(aes(x=date, y = Wealth, color = "Sample MV Wealth no costs")) +
  geom_line(aes(x=date, y = MVsample.raw.50bps$Wealth, color = "Sample MV Wealth 50bps")) +
  geom_line(aes(x=date, y = MVshrink.raw.noCost$Wealth, color = "Shrink MV Wealth no costs")) +
  geom_line(aes(x=date, y = MVshrink.raw.50bps$Wealth, color = "Shrink MV Wealth 50bps")) +
  scale_x_date(labels = date_format("%d-%m-%Y")) +
  scale_color_manual("Legend", values = c("Sample MV Wealth no costs" = "blue", "Sample MV Wealth 50bps" = "red",
                                          "Shrink MV Wealth no costs" = "skyblue", "Shrink MV Wealth 50bps" = "orange")) +
  labs(title = "Wealth Development (Raw) MV All ETFs", x = "Dates", y = "Wealth") +
  theme_minimal()


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
# Show Maximum Drawdowns
rbind(SampleMV.noCosts = MVsample.raw.noCost$maxDraw, SampleMV.50bps = MVsample.raw.50bps$maxDraw,
      ShrinkMV.noCosts = MVshrink.raw.noCost$maxDraw, ShrinkMV.PR.50bps = MVshrink.raw.50bps$maxDraw)


## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
# Display absolute and relative returns in a table:
returns.MV.raw.Wealth <- list(MVsample.abs.noCost = MVsample.raw.noCost$abs.ret, MVsample.abs.50bps = MVsample.raw.50bps$abs.ret,
                             MVsample.rel.noCost = MVsample.raw.noCost$rel.ret, MVsample.rel.50bps = MVsample.raw.50bps$rel.ret,
                             MVshrink.abs.noCost = MVshrink.raw.noCost$abs.ret, MVshrink.abs.50bps = MVshrink.raw.50bps$abs.ret,
                             MVshrink.rel.noCost = MVshrink.raw.noCost$rel.ret, MVshrink.rel.50bps = MVshrink.raw.50bps$rel.ret)

returns.MV.raw.Wealth <- bind_rows(lapply(names(returns.MV.raw.Wealth), function(name){
  data_frame <- data.frame(value = returns.MV.raw.Wealth[[name]])
  data_frame$Type <- name  # Add the list name as a new column
  return(data_frame)
}))

# Extract `EW`/`RW`, `rel`/`abs`, and `noCost`/`50bps` from the `Type` column
returns.MV.raw.Wealth <- returns.MV.raw.Wealth %>%
  mutate(Strategy = ifelse(grepl("^MVsample", Type), "sample", "shrink"),
         Return_Type = ifelse(grepl("rel", Type), "rel", "abs"),
         Cost = ifelse(grepl("noCost", Type), "noCost", "50bps")
         ) %>%
  select(-Type)  # Remove the original Type column

# Reshape the data to a wide format table
returns.MV.raw.Wealth <- returns.MV.raw.Wealth %>%
  pivot_wider(names_from = Return_Type, values_from = value) %>%
  arrange(Strategy, Cost)

returns.MV.raw.Wealth



## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------



## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------



## ----warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------



## ----eval = FALSE-------------------------------------------------------------------------------------------------------------------------------------------------
## tune_grid <- NULL
## train_control <- trainControl(method = "none")
## lookback <-c(5*12,2*12,12)
## train.perc <- c(0.5, 0.3, 0.7)
## lag <- c(12,3*12,5*12,7*12)
## horizon <- c(12,6,1)
## limit <- c(NA,4*12, 7*12)
## use <- c(0.25,0.5,1)
## n.trees <- 500
## 
## # Create the combinations
## input.grid <- expand.grid(lookback, train.perc, lag, horizon, limit, use)
## colnames(input.grid) <- c("lookback", "train.perc", "lag", "horizon", "limit", "use")
## 
## # Remove combinations for which the RF predictions can't be computed
## input.grid <- filter(input.grid, is.na(limit) | limit - lag >= horizon + 1)
## input.grid <- filter(input.grid, round(train.perc*nrow(ret),0) - lag >= horizon + 1)
## 
## # Shuffle the table in case you can't have not much time available
## input.grid <- input.grid[sample(nrow(input.grid)), ]
## # In this case you get a better picture of the impact of different combinations
## # Because the combinations are created systematically by columns.
## # Hence, after 20 min or so you might only get a picture of what the impact is, when you differ the combination between lookback and train.perc
## 
## # Create list to store the performance results in
## results.perf <- vector("list", length = nrow(input.grid))
## results.perf <- lapply(1:length(results.perf), function(x){ results.perf[[x]] <- input.grid[x,]})
## 
## for(x in 1:nrow(input.grid)){
## 
##   print(x)
## 
##   # EXTRACT INPUT PARAMETERS
##   tryCatch({lookback <- input.grid$lookback[x]
##   train.perc <- input.grid$train.perc[x]
##   lag <- input.grid$lag[x]
##   horizon <- input.grid$horizon[x]
##   limit <- input.grid$limit[x]
##   use.long.only <- input.grid$use[x]
##   use.long.short <- input.grid$use[x]
##   use.long.minvar <- input.grid$use[x]
## 
##   ###############################################################################################
##   # PREVAILING MEAN
##   # Initial number of training rows:
##   train.rows <- Train.Rows(ret = ret, train.perc = train.perc)
##   # All test values:
##   y_test <- Test_DS(ret = ret, train.rows = train.rows, horizon = horizon)
## 
##   mean.ret.mon.all <- NULL
##   mean.ret.cum.all <- NULL
##   t=1
## 
##   ## Start predictions:
##   while(t<nrow(y_test$y_test)){
## 
##     res.prev <- PREVAILING.MEAN(ret = ret, horizon = horizon,
##                                 train.rows = train.rows, limit = limit,
##                                 counter = t, mean.ret.mon.all = mean.ret.mon.all,
##                                 mean.ret.cum.all = mean.ret.cum.all,
##                                 y_test = y_test$y_test)
## 
##     t <- res.prev$counter
##     train.rows <- res.prev$train.rows
##     mean.ret.mon.all <- res.prev$mean.ret.mon.all
##     mean.ret.cum.all <- res.prev$mean.ret.cum.all
## 
##   }
##   rownames(mean.ret.cum.all) <- c(1:nrow(mean.ret.cum.all))
##   eval.pr.mean <- EVAL(y_predictions = mean.ret.mon.all, y_actual = y_test$y_test,
##                        y_predictions.cum = mean.ret.cum.all,
##                        y_actual.cum = y_test$y_test.cum)
##   eval.pr.mean <- do.call(rbind, eval.pr.mean)
## 
##   ###############################################################################################
## 
##   #RANDOM FOREST
##   # Initial number of training rows:
##   train.rows <- Train.Rows(ret = ret, train.perc = train.perc)
##   # Compute EMAs with max. 5 years of data laying back for Macro variables:
##   macro.ema <- TRANS.MACRO(data = macro, lookback = lookback)
##   # All test values
##   y_test <- Test_DS(ret = ret, train.rows = train.rows, horizon = horizon)
##   if(!is.null(clusters)){
##     print("!!Attention: You set the variable clusters. Don't perform several other (complex) parallel tasks on your machine while the loop is running!!")
##   }
##   t=1
## 
##   ## Start predictions:
##   while(t<nrow(y_test$y_test)){
## 
##     input <- Train_DS(ret = ret, macro.ema = macro.ema, train.rows = train.rows,
##                       lag = lag, limit = limit)
## 
##     res <- RANDOM_FOREST(input = input, horizon = horizon, y_test = y_test,counter =  t, old.forecasts = old.forecasts,
##                          cum.perf = cum.perf, tune_grid = tune_grid, train_control = train_control, clusters = clusters,
##                          limit = limit, n.trees = n.trees)
## 
##     t <- res$counter
##     train.rows <- res$train.rows
##     old.forecasts <- res$old.forecasts
##     cum.perf <- res$cum.perf
## 
##   }
##   ## Extract return predictions:
##   monthly.pred <- do.call(cbind, res$old.forecasts)
##   cumulative.pred <- do.call(cbind, res$cum.perf)
## 
##   eval.RF <- EVAL(y_predictions = res$old.forecasts, y_actual = y_test$y_test,
##                y_predictions.cum = res$cum.perf, y_actual.cum = y_test$y_test.cum)
##   eval.RF <- do.call(rbind, eval.RF)
## 
##   ###############################################################################################
## 
##   ###################
##   #### BACKTESTS ####
##   ###################
## 
## 
##   ###############################################################################################
##   # LO PR
##   print("LO PR")
##   train.rows <- Train.Rows(ret = ret, train.perc = train.perc)
##   y_test <- Test_DS(ret = ret, train.rows = train.rows, horizon = horizon)
##   counter = 1
##   t = 1
##   portf.EW.all <- list()
##   portf.RW.all <- list()
##   all.ret.mon.portf.EW <- NULL
##   all.ret.mon.portf.RW <- NULL
##   res.PR.LO <- NULL
## 
##   ## Start the backtest
##   while(counter < nrow(y_test$y_test)){
## 
##     #print(t)
## 
##     res.PR.LO <- LONG.ONLY(y_test = y_test$y_test, cum.pred = mean.ret.cum.all, use.long.only = use.long.only,
##                             counter = counter, t = t, horizon = horizon, portf.EW.all = portf.EW.all, portf.RW.all = portf.RW.all,
##                             all.ret.mon.portf.EW = all.ret.mon.portf.EW, all.ret.mon.portf.RW = all.ret.mon.portf.RW, fix.tc = fix.tc,
##                             trans.costs = trans.costs, TER.monthly = TER.monthly, res.all = res.PR.LO)
## 
##     t <- res.PR.LO$t
##     counter <- res.PR.LO$counter
##     # Extract initial weights
##     portf.EW.all <- res.PR.LO$portf.EW.all
##     portf.RW.all <- res.PR.LO$portf.RW.all
##     # All other performance results
##     all.ret.mon.portf.EW <- res.PR.LO$ret.portf.EW$`No costs`$`Monthly Portfolio Return`
##     all.ret.mon.portf.RW <- res.PR.LO$ret.portf.RW$`No costs`$`Monthly Portfolio Return`
##   }
## 
##   ## Summary statistics:
##   # EW:
##   stat.EW.L.O.PR <- STATISTICS(res = res.PR.LO$ret.portf.EW, RF = rf, VaR.CI = VaR.CI, horizon = horizon, trans.costs = trans.costs)
##   # RW:
##   stat.RW.L.O.PR <- STATISTICS(res = res.PR.LO$ret.portf.RW, RF = rf, VaR.CI = VaR.CI, horizon = horizon, trans.costs = trans.costs)
##   LO.PR <- list(Equally.Weighted.L.O = stat.EW.L.O.PR, Rank.Weighted.L.O = stat.RW.L.O.PR)
## 
##   ###############################################################################################
## 
##   # LO RF
##   print("LO RF")
##   train.rows <- Train.Rows(ret = ret, train.perc = train.perc)
##   y_test <- Test_DS(ret = ret, train.rows = train.rows, horizon = horizon)
##   counter = 1
##   t = 1
##   portf.EW.all <- list()
##   portf.RW.all <- list()
##   all.ret.mon.portf.EW <- NULL
##   all.ret.mon.portf.RW <- NULL
##   res.RF.LO <- NULL
##   cumulative.pred <- as.matrix(cumulative.pred)
##   colnames(cumulative.pred) <- colnames(mean.ret.cum.all)
## 
##   ## Start the backtest
##   while(counter < nrow(y_test$y_test)){
## 
##     #print(t)
## 
##     res.RF.LO <- LONG.ONLY(y_test = y_test$y_test, cum.pred = cumulative.pred, use.long.only = use.long.only,
##                             counter = counter, t = t, horizon = horizon, portf.EW.all = portf.EW.all, portf.RW.all = portf.RW.all,
##                             all.ret.mon.portf.EW = all.ret.mon.portf.EW, all.ret.mon.portf.RW = all.ret.mon.portf.RW, fix.tc = fix.tc,
##                             trans.costs = trans.costs, TER.monthly = TER.monthly, res.all = res.RF.LO)
## 
##     t <- res.RF.LO$t
##     counter <- res.RF.LO$counter
##     # Extract initial weights
##     portf.EW.all <- res.RF.LO$portf.EW.all
##     portf.RW.all <- res.RF.LO$portf.RW.all
##     # All other performance results
##     all.ret.mon.portf.EW <- res.RF.LO$ret.portf.EW$`No costs`$`Monthly Portfolio Return`
##     all.ret.mon.portf.RW <- res.RF.LO$ret.portf.RW$`No costs`$`Monthly Portfolio Return`
##   }
## 
##   ## Summary statistics:
##   # EW:
##   stat.EW.LO.RF <- STATISTICS(res = res.RF.LO$ret.portf.EW, RF = rf, VaR.CI = VaR.CI, horizon = horizon, trans.costs = trans.costs)
##   # RW:
##   stat.RW.LO.RF <- STATISTICS(res = res.RF.LO$ret.portf.RW, RF = rf, VaR.CI = VaR.CI, horizon = horizon, trans.costs = trans.costs)
##   LO.RF <- list(Equally.Weighted.L.O = stat.EW.LO.RF, Rank.Weighted.L.O = stat.RW.LO.RF)
## 
##   ###############################################################################################
## 
##   # LS PR
##   print("LS PR")
##   train.rows <- Train.Rows(ret = ret, train.perc = train.perc)
##   y_test <- Test_DS(ret = ret, train.rows = train.rows, horizon = horizon)
##   counter = 1
##   t = 1
##   portf.EW.all <- list()
##   portf.RW.all <- list()
##   all.ret.mon.portf.EW <- NULL
##   all.ret.mon.portf.RW <- NULL
##   res.PR.L.S <- NULL
## 
##   while(counter < nrow(y_test$y_test)){
## 
##     #print(t)
## 
##     res.PR.L.S <- LONG.SHORT(y_test = y_test$y_test, cum.pred = mean.ret.cum.all, disc.positive = disc.positive, use.long.short = use.long.short,
##                              lend = lend, counter = counter, t = t, horizon = horizon, portf.EW.all = portf.EW.all, portf.RW.all = portf.RW.all,
##                              all.ret.mon.portf.EW = all.ret.mon.portf.EW, all.ret.mon.portf.RW = all.ret.mon.portf.RW, fix.tc = fix.tc,
##                              trans.costs = trans.costs, TER.monthly = TER.monthly, res.all = res.PR.L.S)
## 
##     t <- res.PR.L.S$t
##     counter <- res.PR.L.S$counter
##     # Extract initial weights
##     portf.EW.all <- res.PR.L.S$portf.EW.all
##     portf.RW.all <- res.PR.L.S$portf.RW.all
##     # All other performance results
##     all.ret.mon.portf.EW <- res.PR.L.S$ret.portf.EW$`No costs`$`Monthly Portfolio Return`
##     all.ret.mon.portf.RW <- res.PR.L.S$ret.portf.RW$`No costs`$`Monthly Portfolio Return`
##   }
## 
##   ## Summary statistics:
##   # EW:
##   stat.EW.L.S.PR <- STATISTICS(res = res.PR.L.S$ret.portf.EW, RF = rf, VaR.CI = VaR.CI, horizon = horizon, trans.costs = trans.costs)
##   # RW:
##   stat.RW.L.S.PR <- STATISTICS(res = res.PR.L.S$ret.portf.RW, RF = rf, VaR.CI = VaR.CI, horizon = horizon, trans.costs = trans.costs)
##   LS.PR <- list(Equally.Weighted.L.S = stat.EW.L.S.PR, Rank.Weighted.L.S = stat.RW.L.S.PR)
## 
##   ###############################################################################################
## 
##   # LS RF
##   print("LS RF")
##   train.rows <- Train.Rows(ret = ret, train.perc = train.perc)
##   y_test <- Test_DS(ret = ret, train.rows = train.rows, horizon = horizon)
##   counter = 1
##   t = 1
##   portf.EW.all <- list()
##   portf.RW.all <- list()
##   all.ret.mon.portf.EW <- NULL
##   all.ret.mon.portf.RW <- NULL
##   res.RF.L.S <- NULL
##   cumulative.pred <- as.matrix(cumulative.pred)
##   colnames(cumulative.pred) <- colnames(mean.ret.cum.all)
## 
##   while(counter < nrow(y_test$y_test)){
## 
##     #print(t)
## 
##     res.RF.L.S <- LONG.SHORT(y_test = y_test$y_test, cum.pred = cumulative.pred, use.long.short = use.long.short, lend = lend,
##                              counter = counter, t = t, horizon = horizon, portf.EW.all = portf.EW.all, portf.RW.all = portf.RW.all,
##                              all.ret.mon.portf.EW = all.ret.mon.portf.EW, all.ret.mon.portf.RW = all.ret.mon.portf.RW, fix.tc = fix.tc,
##                              trans.costs = trans.costs, TER.monthly = TER.monthly, res.all = res.RF.L.S)
## 
##     t <- res.RF.L.S$t
##     counter <- res.RF.L.S$counter
##     # Extract initial weights
##     portf.EW.all <- res.RF.L.S$portf.EW.all
##     portf.RW.all <- res.RF.L.S$portf.RW.all
##     # All other performance results
##     all.ret.mon.portf.EW <- res.RF.L.S$ret.portf.EW$`No costs`$`Monthly Portfolio Return`
##     all.ret.mon.portf.RW <- res.RF.L.S$ret.portf.RW$`No costs`$`Monthly Portfolio Return`
##   }
## 
##   ## Summary statistics:
##   # EW:
##   stat.EW.L.S.RF <- STATISTICS(res = res.RF.L.S$ret.portf.EW, RF = rf, VaR.CI = VaR.CI, horizon = horizon, trans.costs = trans.costs)
##   # RW:
##   stat.RW.L.S.RF <- STATISTICS(res = res.RF.L.S$ret.portf.RW, RF = rf, VaR.CI = VaR.CI, horizon = horizon, trans.costs = trans.costs)
##   LS.RF <- list(Equally.Weighted.L.S = stat.EW.L.S.RF, Rank.Weighted.L.S = stat.RW.L.S.RF)
## 
##   ###############################################################################################
## 
##   # MV PR
##   print("MV PR")
##   train.rows <- Train.Rows(ret = ret, train.perc = train.perc)
##   y_test <- Test_DS(ret = ret, train.rows = train.rows, horizon = horizon)
##   counter = 1
##   t = 1
##   portf.sample.all <- list()
##   portf.shrink.all <- list()
##   all.ret.mon.portf.sample <- NULL
##   all.ret.mon.portf.shrink <- NULL
##   res.MV.PR <- NULL
## 
##   while(counter < nrow(y_test$y_test)){
## 
##     #print(t)
## 
##     res.MV.PR <- MIN.VAR(ret = ret, train.rows = train.rows, y_test = y_test$y_test, cum.pred = mean.ret.cum.all,
##                          use.minvar = use.minvar, counter = counter, t = t, horizon = horizon,
##                          portf.shrink.all = portf.shrink.all, portf.sample.all = portf.sample.all,trans.costs = trans.costs,
##                          TER.monthly = TER.monthly, all.ret.mon.portf.shrink = all.ret.mon.portf.shrink,
##                          all.ret.mon.portf.sample = all.ret.mon.portf.sample, limit = limit, fix.tc = fix.tc,
##                          min.max = min.max, res.all = res.MV.PR)
## 
##     train.rows <- res.MV.PR$train.rows
##     t <- res.MV.PR$t
##     counter <- res.MV.PR$counter
##     # Extract initial weights
##     portf.sample.all <- res.MV.PR$portf.sample.all
##     portf.shrink.all <- res.MV.PR$portf.shrink.all
##     # All other performance results
##     all.ret.mon.portf.sample<- res.MV.PR$ret.portf.sample$`No costs`$`Monthly Portfolio Return`
##     all.ret.mon.portf.shrink <- res.MV.PR$ret.portf.shrink$`No costs`$`Monthly Portfolio Return`
##   }
## 
##   ## Summary statistics:
##   # EW:
##   stat.MV.sample.PR <- STATISTICS(res = res.MV.PR$ret.portf.sample, RF = rf, VaR.CI = VaR.CI, horizon = horizon, trans.costs = trans.costs)
##   # RW:
##   stat.MV.shrink.PR <- STATISTICS(res = res.MV.PR$ret.portf.shrink, RF = rf, VaR.CI = VaR.CI, horizon = horizon, trans.costs = trans.costs)
##   MV.PR <- list(sample.MV = stat.MV.sample.PR, shrink.MV = stat.MV.shrink.PR)
## 
## 
##   ###############################################################################################
## 
##   # MV RF
##   print("MV RF")
##   train.rows <- Train.Rows(ret = ret, train.perc = train.perc)
##   y_test <- Test_DS(ret = ret, train.rows = train.rows, horizon = horizon)
##   counter = 1
##   t = 1
##   portf.sample.all <- list()
##   portf.shrink.all <- list()
##   all.ret.mon.portf.sample <- NULL
##   all.ret.mon.portf.shrink <- NULL
##   res.MV.RF <- NULL
##   cumulative.pred <- as.matrix(cumulative.pred)
##   colnames(cumulative.pred) <- colnames(mean.ret.cum.all)
## 
##   while(counter < nrow(y_test$y_test)){
## 
##     #print(t)
## 
##     res.MV.RF <- MIN.VAR(ret = ret, train.rows = train.rows, y_test = y_test$y_test, cum.pred = cumulative.pred,
##                          use.minvar = use.minvar, counter = counter, t = t, horizon = horizon,
##                          portf.shrink.all = portf.shrink.all, portf.sample.all = portf.sample.all,
##                          trans.costs = trans.costs, TER.monthly = TER.monthly, all.ret.mon.portf.shrink = all.ret.mon.portf.shrink,
##                          all.ret.mon.portf.sample = all.ret.mon.portf.sample, limit = limit, fix.tc = fix.tc, min.max = min.max,
##                          res.all = res.MV.RF)
## 
##     train.rows <- res.MV.RF$train.rows
##     t <- res.MV.RF$t
##     counter <- res.MV.RF$counter
##     # Extract initial weights
##     portf.sample.all <- res.MV.RF$portf.sample.all
##     portf.shrink.all <- res.MV.RF$portf.shrink.all
##     # All other performance results
##     all.ret.mon.portf.sample<- res.MV.RF$ret.portf.sample$`No costs`$`Monthly Portfolio Return`
##     all.ret.mon.portf.shrink <- res.MV.RF$ret.portf.shrink$`No costs`$`Monthly Portfolio Return`
##   }
## 
##   ## Summary statistics:
##   # EW:
##   stat.MV.sample.RF <- STATISTICS(res = res.MV.RF$ret.portf.sample, RF = rf, VaR.CI = VaR.CI, horizon = horizon, trans.costs = trans.costs)
##   # RW:
##   stat.MV.shrink.RF <- STATISTICS(res = res.MV.RF$ret.portf.shrink, RF = rf, VaR.CI = VaR.CI, horizon = horizon, trans.costs = trans.costs)
##   MV.RF <- list(sample.MV = stat.MV.sample.RF, shrink.MV = stat.MV.shrink.RF)
## 
##   ###############################################################################################
## 
## 
##   #######################
##   #### STORE RESULTS ####
##   #######################
## 
## 
##   ###############################################################################################
## 
##   results.perf[[x]] <- list(eval.PR = eval.pr.mean, eval.RF = eval.RF, # Prediction model results
##                             LO.PR = LO.PR, LO.RF = LO.RF,
##                             LS.PR = LS.PR, LS.RF = LS.RF,
##                             MV.PR = MV.PR, MV.RF = MV.RF)
##   results.perf[[x]] <- append(list(input.grid[x,]), results.perf[[x]])
## 
##   saveRDS(results.perf, file = "C:/Users/Berger/Documents/Studium/Master/5. Semester/Masterarbeit/Ergebnisse/Robustness Checks/results.perf.rds")
##   },
## 
##   error = function(e){
##     message("Error encountered in iteration ", x, ": ", e$message)
##   })
## }
## 
## #saveRDS(results.perf, file = "C:/Users/Berger/Documents/Studium/Master/5. Semester/Masterarbeit/Ergebnisse/Robustness Checks/results.perf.rds")
## #x=180
## 
## #results.perf <- readRDS("C:/Users/Berger/Documents/Studium/Master/5. Semester/Masterarbeit/Ergebnisse/Robustness Checks/results.perf.rds")
## 
## 
## results.LO <- do.call(rbind, lapply(1:length(results.perf), function(x){
##   c(ifelse(is.null(results.perf[[x]]$LO.PR$Equally.Weighted.L.O[1,1]), NA, results.perf[[x]]$LO.PR$Equally.Weighted.L.O[1,1]),
##     ifelse(is.null(results.perf[[x]]$LO.RF$Equally.Weighted.L.O[1,1]), NA, results.perf[[x]]$LO.RF$Equally.Weighted.L.O[1,1]),
##     ifelse(is.null(results.perf[[x]]$LO.PR$Rank.Weighted.L.O[1,1]), NA, results.perf[[x]]$LO.PR$Rank.Weighted.L.O[1,1]),
##     ifelse(is.null(results.perf[[x]]$LO.RF$Rank.Weighted.L.O[1,1]), NA, results.perf[[x]]$LO.RF$Rank.Weighted.L.O[1,1]))
##   })
## )
## 
## inputs <- do.call(rbind, lapply(results.perf, function(x) {
##   if (is.data.frame(x)) {
##     return(x)
##   } else {
##     return(x[[1]])
##   }
## }))
## 
## results.LO <- cbind(results.LO, inputs)
## 
## colnames(results.LO) <- c("LO.PR.EQ", "LO.RF.EQ", "LO.PR.RW", "LO.RF.RW", "lookback", "train.perc", "lag", "horizon", "limit", "use")
## 
## 
## results.LO[which(as.numeric(results.LO$LO.RF.RW) > as.numeric(results.LO$LO.PR.RW)),]
## 
## which(as.numeric(results.LO$LO.RF.EW) > as.numeric(results.LO$LO.PR.EW))
## 
## results.LO[,-c(5,7,9,10)] %>%
##   group_by(horizon) %>%
##   summarize(mean.LO.PR.EQ = mean(as.numeric(LO.PR.EQ), na.rm = T),
##             mean.LO.RF.EQ = mean(as.numeric(LO.RF.EQ), na.rm = T),
##             mean.LO.PR.RW = mean(as.numeric(LO.PR.RW), na.rm = T),
##             mean.LO.RF.RW = mean(as.numeric(LO.RF.RW), na.rm = T))


## ----warning=FALSE, eval=FALSE------------------------------------------------------------------------------------------------------------------------------------
## # Monthly RIs:
## write.xlsx(as.data.frame(ETFs.mon), file.path("C:\\Users\\Berger\\Documents\\Studium\\Master\\5. Semester\\Masterarbeit\\Daten\\Bereinigt", "ETFs monthly.xlsx"), colNames = T, rowNames = T)
## 
## # Monthly Returns:
## write.xlsx(as.data.frame(Ret), file.path("C:\\Users\\Berger\\Documents\\Studium\\Master\\5. Semester\\Masterarbeit\\Daten\\Bereinigt", "Returns monthly.xlsx"), colNames = T, rowNames = T)
## 
## # Lagged Returns:
## write.xlsx(Ret.LAG, file.path("C:\\Users\\Berger\\Documents\\Studium\\Master\\5. Semester\\Masterarbeit\\Daten\\Bereinigt", "Lagged Returns monthly.xlsx"), colNames = T, rowNames = T)
## 
## # Cumulative Returns:
## write.xlsx(CUM.ret, file.path("C:\\Users\\Berger\\Documents\\Studium\\Master\\5. Semester\\Masterarbeit\\Daten\\Bereinigt", "Cumulative Returns monthly.xlsx"), colNames = T, rowNames = T)
## 
## # Macros:
## write.xlsx(as.data.frame(Macro), file.path("C:\\Users\\Berger\\Documents\\Studium\\Master\\5. Semester\\Masterarbeit\\Daten\\Bereinigt", "Macros monthly.xlsx"), colNames = T, rowNames = T)
## 


## ----warning=FALSE, eval=FALSE------------------------------------------------------------------------------------------------------------------------------------
## # Save table as .xlsx-file
## write.xlsx(as.data.frame(Ret.overview)[,-1], file.path("C:\\Users\\Berger\\Documents\\Studium\\Master\\5. Semester\\Masterarbeit\\Ergebnisse\\Deskriptive Statistiken", "Base Statistics ETFs.xlsx"), colNames = T, rowNames = T)
## Ret.overview


## ----warning=FALSE, eval=FALSE------------------------------------------------------------------------------------------------------------------------------------
## # Save table as .xlsx-file:
## write.xlsx(as.data.frame(Macro.overview)[,-1], file.path("C:\\Users\\Berger\\Documents\\Studium\\Master\\5. Semester\\Masterarbeit\\Ergebnisse\\Deskriptive Statistiken", "Base Statistics Macros.xlsx"), colNames = T, rowNames = T)
## head(Macro.overview)


## ----warning=FALSE, eval=FALSE------------------------------------------------------------------------------------------------------------------------------------
## # Store the time series summary in the desired folder:
## write.xlsx(TSret.overview, file.path("C:\\Users\\Berger\\Documents\\Studium\\Master\\5. Semester\\Masterarbeit\\Ergebnisse\\Deskriptive Statistiken", "TS summary Returns.xlsx"), colNames = T, rowNames = T)
## 
## # Save the time series summary in the desired folder:
## write.xlsx(TStotalri.overview, file.path("C:\\Users\\Berger\\Documents\\Studium\\Master\\5. Semester\\Masterarbeit\\Ergebnisse\\Deskriptive Statistiken", "TS summary ETFs.xlsx"), colNames = T, rowNames = T)


## ----warning=FALSE, eval=FALSE------------------------------------------------------------------------------------------------------------------------------------
## # Save the TSmacro.overview table in the desired folder:
## write.xlsx(TSmacro.overview, file.path("C:\\Users\\Berger\\Documents\\Studium\\Master\\5. Semester\\Masterarbeit\\Ergebnisse\\Deskriptive Statistiken", "TS summary Macro.xlsx"), colNames = T, row.names = T)


## ----warning=FALSE, eval=FALSE------------------------------------------------------------------------------------------------------------------------------------
## # Save the table in the desired folder:
## write.xlsx(as.data.frame(stats), file.path("C:\\Users\\Berger\\Documents\\Studium\\Master\\5. Semester\\Masterarbeit\\Ergebnisse\\Deskriptive Statistiken", "Skewness and Kurtosis.xlsx"), colNames = T, rowNames = T)


## ----warning=FALSE, eval=FALSE------------------------------------------------------------------------------------------------------------------------------------
## # Monthly Predictions:
## write.xlsx(as.data.frame(mean.ret.mon.all), file.path("C:\\Users\\Berger\\Documents\\Studium\\Master\\5. Semester\\Masterarbeit\\Ergebnisse\\Prevailing Mean Vorhersagen", paste("Monthly", ifelse(all(ret == log(Ret[,-1]+1), na.rm = T)==T, "Log-", ""), "Return Predictions with lag", lag/12, "years, limit", ifelse(is.na(limit), 0, limit), "and horizon", horizon, ".xlsx")), colNames = T, rowNames = T)
## 
## # Cumulative Predictions:
## write.xlsx(as.data.frame(mean.ret.cum.all), file.path("C:\\Users\\Berger\\Documents\\Studium\\Master\\5. Semester\\Masterarbeit\\Ergebnisse\\Prevailing Mean Vorhersagen", paste("Cumulative", ifelse(all(ret == log(Ret[,-1]+1), na.rm = T)==T, "Log-", ""), "Return Predictions with lag", lag/12, "years, limit", ifelse(is.na(limit), 0, limit), "and horizon", horizon, ".xlsx")), colNames = T, rowNames = T)
## 
## # Evaluation:
## write.xlsx(as.data.frame(eval.pr.mean), file.path("C:\\Users\\Berger\\Documents\\Studium\\Master\\5. Semester\\Masterarbeit\\Ergebnisse\\Prevailing Mean Vorhersagen", paste("Evaluation of", ifelse(all(ret == log(Ret[,-1]+1), na.rm = T)==T, "Log-", ""), "Predictions with lag", lag/12, "years, limit", ifelse(is.na(limit), 0, limit), "and horizon", horizon, ".xlsx")), colNames = T, rowNames = T)


## ----warning=FALSE, eval=FALSE------------------------------------------------------------------------------------------------------------------------------------
## # Monthly Predictions:
## write.xlsx(as.data.frame(monthly.pred), file.path("C:\\Users\\Berger\\Documents\\Studium\\Master\\5. Semester\\Masterarbeit\\Ergebnisse\\Random Forest Vorhersagen", paste("Monthly", ifelse(all(ret == log(Ret[,-1]+1), na.rm = T)==T, "Log-", ""), "RF Return Predictions with lag", lag/12, "years, limit", ifelse(is.na(limit), 0, limit), ", horizon", horizon, ",", ifelse(is.null(tune_grid), "without hyperparameter tuning", "with hyperparameter tuning"), "and", ifelse(train_control$method == "none", "without cross-validation", "with cross-validation"), "equity only).xlsx")), colNames = T, rowNames = T)
## 
## # Cumulative Predictions:
## write.xlsx(as.data.frame(cumulative.pred), file.path("C:\\Users\\Berger\\Documents\\Studium\\Master\\5. Semester\\Masterarbeit\\Ergebnisse\\Random Forest Vorhersagen", paste("Cumulative", ifelse(all(ret == log(Ret[,-1]+1), na.rm = T)==T, "Log-", ""), "RF Return Predictions with lag", lag/12, "years, limit", ifelse(is.na(limit), 0, limit),  ", horizon", horizon, ",", ifelse(is.null(tune_grid), "without hyperparameter tuning", "with hyperparameter tuning"), "and", ifelse(train_control$method == "none", "without cross-validation", "with cross-validation"), ".xlsx")), colNames = T, rowNames = T)
## 
## # Evaluation:
## write.xlsx(as.data.frame(eval), file.path("C:\\Users\\Berger\\Documents\\Studium\\Master\\5. Semester\\Masterarbeit\\Ergebnisse\\Random Forest Vorhersagen", paste("Evaluation of RF", ifelse(all(ret == log(Ret[,-1]+1), na.rm = T)==T, "Log-", ""), "Predictions with lag", lag/12, "years, limit", ifelse(is.na(limit), 0, limit), ", horizon", horizon, ",", ifelse(is.null(tune_grid), "without hyperparameter tuning", "with hyperparameter tuning"), "and", ifelse(train_control$method == "none", "without cross-validation", "with cross-validation"), ".xlsx")), colNames = T, rowNames = T)


## ----warning=FALSE, eval=FALSE------------------------------------------------------------------------------------------------------------------------------------
## 


## ----warning=FALSE, eval=FALSE------------------------------------------------------------------------------------------------------------------------------------
## 


## ----warning=FALSE, eval=FALSE------------------------------------------------------------------------------------------------------------------------------------
## 


## ----warning=FALSE, eval=FALSE------------------------------------------------------------------------------------------------------------------------------------
## # Set the path name where the results are stored:
## path <- "C:/Users/Berger/Documents/Studium/Master/5. Semester/Masterarbeit/Ergebnisse/Random Forest Vorhersagen/"
## 
## # Monthly predictions
## monthly.pred <- read.xlsx(paste0(path, "Monthly  RF Return Predictions with lag 4 years, limit 84 , horizon 12 , with hyperparameter tuning and with cross-validation .xlsx"), sheet = 1)[,-1]
## 
## # Cumulative predictions (over the investing horizon)
## cumulative.pred <- read.xlsx(paste0(path, "Cumulative  RF Return Predictions with lag 4 years, limit 84 , horizon 12 , with hyperparameter tuning and with cross-validation .xlsx"), sheet = 1)[,-1]
## 
## # Accuracy measures
## eval <- read.xlsx(paste0(path, "Evaluation of RF  Predictions with lag 4 years, limit 84 , horizon 12 , with hyperparameter tuning and with cross-validation .xlsx"), sheet = 1)
## rownames(eval) <- eval[,1]
## eval <- eval[,-1]


## ----warning=FALSE, eval=FALSE------------------------------------------------------------------------------------------------------------------------------------
## path <- "C:/Users/Berger/Documents/Studium/Master/5. Semester/Masterarbeit/Ergebnisse/Robustness Checks/"
## 
## results.perf <- read.xlsx(paste0(path, "Input Parameters Test Long Only.xlsx"), sheet = 1)[,-1]

