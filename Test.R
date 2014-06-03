rm(list=ls())
library(compiler)

source(file.path("Functions","Functions.R"))

clients <- Get.Client.List()

out <- NULL

total <- length(clients)

pb <- winProgressBar(title ="progress bar", min = 0, max = total, width = 300)


for(i in 1:total){
	try({
		data <- GetClientTrades(clients[i])	
		row <- CalcStats(data)
		out <- rbind(row,out)
		cat(i,"\n")
		if(i %% 2500 == 0) save(out, file=file.path("Data",sprintf("ClientSummaryTable_%s.RData",i)))
		setWinProgressBar(pb, i, title=paste( round(i/total*100, 0),"% complete"))
	})
}

save(out, file=file.path("ClientSummaryTable.RData"))