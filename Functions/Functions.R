################
### FUNCTIONS ##
################
library(RODBC)

n <- function(x)  if(sum(dim(x)) > 0) return(nrow(x)) else return(length(x)) # Returns the length of vectors or the number of rows in a matix/dataframe

query <- function(sqlstring) {
	channel <- odbcConnect("London")
	#connect to SQLSERVER and return query
	q <- sqlQuery(channel, sqlstring)
	close(channel)
	return(q)
}

Get.Client.List <- function(conditions=""){
	#example data <- Get.Client.List("MarketID = 19090")
	where <- ""
	if(sum(nchar(conditions)) > 0) where <- paste("and t.",conditions, sep="")
	where <- paste(where, collapse=' ')
	q <-	query(sprintf("select distinct t.accountid from MasterTradeTable as t	%s", where))
	return(q[,1])
}

GetClientTrades <- cmpfun(function(client){
	mrks <- as.character(query(sprintf("select distinct GrpMarket from mastertradetable as m where accountid = %s",client))[,1])
	
	key <- data.frame(market = c('UK 100 Rolling Daily','DAX 30  Rolling Daily','X EUR/USD Rolling Daily','X GBP/USD Rolling Daily'),
										ticktable = c("TICK_DATA_MINUTE_FT","TICK_DATA_MINUTE_DAX","TICK_DATA_MINUTE_EUDOLLAR","TICK_DATA_MINUTE_POUNDOLLAR"))
	temp <- NULL
	for(market in mrks){
		tick <- key$ticktable[market==key$market]
		
		temp <- rbind(temp,
									query(sprintf("
																select
																m.OpeningTradeId,
																m.accountid,
																m.GrpMarket,
																m.MOpeningTime,
																m.MClosingTime,
																m.pipsize,
																m.OpeningPrice,
																m.ClosingPrice,
																m.bet_direction,
																--m.PL_GBP/((m.ClosingPrice - m.OpeningPrice) / m.pipsize) as StakeGBP,
																m.PL_GBP as PLGBP,
																tick.TimeStamp,
																tick.AvgPrice,
																Imputed
																from mastertradetable as m
																JOIN %s as tick on
																tick.TimeStamp > MOpeningTime and tick.TimeStamp < MClosingTime
																where accountid = %s and m.GrpMarket = '%s'",tick, client, market)))
	}
	temp <- subset(temp, temp$PLGBP != 0)
	temp$StakeGBP <- temp$PLGBP /((temp$ClosingPrice - temp$OpeningPrice) /temp$pipsize)
	return(temp)
	})



CalcStats <- cmpfun(function(data){
	
	data$Paper <- (data[,"AvgPrice"] - data[,"OpeningPrice"]) / data[,"pipsize"] * data[,"StakeGBP"] 
	
	MaxPaper <- aggregate(data[,"Paper"],list(data[,"OpeningTradeId"]), max)[,2]
	MinPaper <- aggregate(data[,"Paper"],list(data[,"OpeningTradeId"]), min)[,2]
	
	Trades <- aggregate(data[,"OpeningTradeId"],list(as.Date(data[,"MOpeningTime"]),data[,"OpeningTradeId"]), length)[,1:2]
	TradesPerTradingDay <- mean(aggregate(Trades[,"Group.1"], list(Trades[,"Group.1"]), length)[,2], na.rm=T)
	
	returns <- NULL
	for(mrk in unique(data[,"GrpMarket"])){
		x <- subset(data, data[,"GrpMarket"] == mrk, select = "AvgPrice")[,1]
		returns <- c(returns, diff(log(x)))
	}
	
	
	row <- data.frame(Client=data[1,"accountid"],
										FirstTrade = min(as.Date(data[,"MOpeningTime"])),
										LastTrade = max(as.Date(data[,"MOpeningTime"])),
										TypicalStake = mean(abs(data[,"StakeGBP"])),
										TradesPerTradingDay = TradesPerTradingDay,
										MeanRealized = mean(data[,"PLGBP"]),
										StDevRealized = sd(data[,"PLGBP"]),
										PaperLoss = sum(data[,"Paper"] < 0),
										PaperGain = sum(data[,"Paper"] > 0),
										RealizedLoss = sum(aggregate(data[,"PLGBP"], list(data[,"OpeningTradeId"]), head, 1) < 0),
										RealizedGain = sum(aggregate(data[,"PLGBP"], list(data[,"OpeningTradeId"]), head, 1) > 0),
										AvgMaxPaperGain = mean(MaxPaper[MaxPaper > 0]),
										AvgMaxPaperLoss =  mean(MinPaper[MinPaper < 0]),
										MarketReturn = mean(returns),
										MarketVolatility = sd(returns)
	)
	
	return(row)
})



