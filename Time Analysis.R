rm(list=ls())

source(file.path("Functions","Functions.R"))

load("ClientTradeMins.RData")

mx <- c(100,500,1000,2000,5000,10000,20000,50000,100000,1000000)
mn <- c(1,100,500,1000,2000,5000,10000,20000,50000,100000)

lst <- NULL
for(i  in 1:length(mx)){
	lst <- c(lst, sample(as.numeric(row.names(data[data$time < mx[i] & data$time > mn[i],])),5))}

lst <- data[lst,]
lst <- lst[order(lst$time),]
lst <- lst[lst$time > 0,]
lst$Markets <- NA
lst$Trades <- NA
lst$QueryTime <- NA
lst$AggregateTime <- NA

total <- nrow(lst)
i=1
for(i in 1:total){
	pct <- proc.time()
	dt <- GetClientTrades(lst$accountid[i])
	lst$Markets[i] <- try({length(unique(dt$GrpMarket))})
	lst$Trades[i] <- try({length(unique(dt$OpeningTradeId))})
	lst$QueryTime[i] <- (proc.time()-pct)[3]
	out <- CalcStats(dt)
	lst$AggregateTime[i] <- (proc.time()-pct)[3]
	cat(i,"\n")
	if(i %% 5 == 0) print(lst)
}



