rm(list=ls())
library(compiler)

setwd("J:/Management Research/UoS PFM/Git/Survival")

source(file.path("Functions","Functions.R"))

clients <- Get.Client.List()

out <- NULL

total <- length(clients)

client <- clients[1]

pb <- winProgressBar(title ="progress bar", min = 0, max = total, width = 300)


for(i in 1:total){
	try({
		data <- GetClientTrades(clients[i])	
		row <- CalcStats(data)
		out <- rbind(row,out)
		if(i %% 2500 == 0) save(out, file=file.path("Data",sprintf("ClientSummaryTable_%s.RData",i)))
		setWinProgressBar(pb, i, title=paste( round(i/total*100, 0),"% complete"))
	})
	cat(i,"\n")
}

save(out, file=file.path("ClientSummaryTable.RData"))