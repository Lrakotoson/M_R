source('scripts/global.R')
library(tidyverse)
library(rAmCharts)

T_cas_dates <- T_cas[-c(1,2,3,4)]
sum_cas <- as.data.frame(apply(T_cas_dates, 2, sum))
dates <- rownames(sum_cas)
data_sum_cas<- cbind(sum_cas,dates)
names(data_sum_cas)[1] <- "cas"


T_mort_dates <- T_morts[-c(1,2,3,4)]
sum_mort <- as.data.frame(apply(T_mort_dates, 2, sum))
data_sum_mort <- cbind(sum_mort,dates)
names(data_sum_mort)[1] <- "mort"

T_retablis_dates <- T_retablis[-c(1,2,3,4)]
sum_ret <- as.data.frame(apply(T_retablis_dates, 2, sum))
data_sum_ret <- cbind(sum_ret,dates)
names(data_sum_ret)[1] <- "retablis"

data_sum <- merge(data_sum_cas,data_sum_mort,key="dates")
data_sum <- merge(data_sum,data_sum_ret,key="dates")
data_sum$dates <- as.character(data_sum$dates)
data_sum$dates <- as.Date(data_sum$dates,"%m/%d/%y")
library(data.table)

trie <- setkey(as.data.table(data_sum),dates)
trie <- as.data.frame(trie)

france <- as.data.frame(geocodeGratuit("France"))
italie <- as.data.frame(geocodeGratuit("Italie"))

