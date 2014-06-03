################
### FUNCTIONS ##
################
library(RODBC)
channel <- odbcConnect("London")

n <- function(x)  if(sum(dim(x)) > 0) return(nrow(x)) else return(length(x)) # Returns the length of vectors or the number of rows in a matix/dataframe

query <- function(sqlstring) {
	#connect to SQLSERVER and return query
	return(sqlQuery(channel, sqlstring))
}

Get.Client.List <- function(conditions=""){
	#example data <- Get.Client.List("MarketID = 19090")
	where <- ""
	if(sum(nchar(conditions)) > 0) where <- paste("and t.",conditions, sep="")
	where <- paste(where, collapse=' ')
	q <-	query(sprintf("select distinct t.accountid from MainTradesTable as t	%s", where))
	return(q[,1])
}




