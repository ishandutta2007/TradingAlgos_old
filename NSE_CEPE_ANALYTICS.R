library(httr)
library(plyr)
library(XML)
library(stringr)
library(reshape2)
library(ggplot2)
library(data.table)
stoploss.percent =0
# offset = 2
pos.beakout.val = 3
neg.beakout.val = -3
exp.dates<-c("28-01-2016","25-02-2016","31-03-2016","28-04-2016")
#exp.dates<-c("31-03-2016")
base <- "/Users/dutishan/Desktop/NSECEPE/"
gg.scrip.returns <- data.frame(company=c("scrip"), returns=c(0), buy.day=c(as.Date("2016-01-28")),  sell.day = c(as.Date("2016-01-28")))

get.symbols<-function(){
  url <- paste0("https://www.nseindia.com/content/fo/fo_underlyinglist.htm")
  response <- GET(url, user_agent("Mozilla"))
  tf <- tempfile()
  writeBin(content(response, "raw"), tf)
  tables <- readHTMLTable(tf)
  symbols <- as.vector(tables[4]$'NULL'$Symbol)
  return(symbols)# symbols <- c("AJANTPHARM")
}

prepapre.csv.useful <- function(files){
  csv.useful <- data.frame()
  ord<-c()
  for (i in 1:length(files)) {
    ord<- c(ord,lapply(files,strsplit,"_")[[i]][[1]][3])
  }
  df=data.frame(files<-as.character(files),order<-as.numeric(ord))
  df=df[with(df, order(order)), ]
  files<-as.character(df$files)
  for (i in 1:length(files)) {
    csv_i <- read.csv(paste0(base,expiryDate,"/",files[i]))
    strnameCE = paste0("CE.",csv_i$Strike.Price[1])
    csv_i[strnameCE] <- csv_i$CE.LTP
    strnamePE = paste0("PE.",csv_i$Strike.Price[1])
    csv_i[strnamePE] <- csv_i$PE.LTP
    if (i == 1) {
      csv.useful <- csv_i[c("Date", "Underlying.Value", strnameCE, strnamePE)]
    } else {
      csv_i_needed <- csv_i[c("Date", "Underlying.Value", strnameCE, strnamePE)]
      csv.useful <- merge(csv.useful, csv_i_needed, by.x=c("Date", "Underlying.Value"), by.y=c("Date", "Underlying.Value"), all = TRUE)
    }
    csv.useful$Date<-as.Date(csv.useful$Date)
    csv.useful<-csv.useful[with(csv.useful, order(Date)), ]
  }
  csv.useful$Value.Change<-c(0,diff(csv.useful$Underlying.Value)/csv.useful$Underlying.Value[-1])*100
  return(csv.useful)
}

get.firsts<- function(files, csv.useful){
  first.idx<-list()
  first.idx$all.strikes.day <-which.max(complete.cases(csv.useful))
  
  if(csv.useful$Value.Change[1]>=pos.beakout.val) {
    first.idx$pos.breakout = 1
  } else {
    first.idx$pos.breakout <-which.max(csv.useful$Value.Change>=pos.beakout.val)
    if(first.idx$pos.breakout == 1) first.idx$pos.breakout = -1
  }
  
  if(csv.useful$Value.Change[1] <= neg.beakout.val) {
    first.idx$neg.beakout.idx = 1
  } else {
    first.idx$neg.beakout <-which.max(csv.useful$Value.Change <= neg.beakout.val)
    if(first.idx$neg.beakout == 1)first.idx$neg.beakout = -1
  }
  
  csv.useful_long <- melt(csv.useful[ , !(names(csv.useful) %in% c("Underlying.Value"))], id = "Date")
  l<-(strsplit(as.character(unique(csv.useful_long$variable)),"E."))
  strikes<-c()
  for(i in 1:length(l)){
    strikes<-c(strikes,as.vector(l[i])[[1]][2])
  }
  strikes<-c(NA,NA,strikes)
  
  first.idx$closest.above<-NULL
  first.idx$closest.below<-NULL
  for(day in 1:nrow(csv.useful)){
    strike.diff<-as.numeric(strikes)-csv.useful$Underlying.Value[day]
    temp = strike.diff
    temp[strike.diff<0]=NA
    temp[is.na(csv.useful[day,])]=NA
    if(all(is.na(temp))){
      first.idx$closest.above <- c(first.idx$closest.above, NA)
    }else{
      first.idx$closest.above <- c(first.idx$closest.above, which.min(temp))
    }
    temp = strike.diff
    temp[strike.diff>=0]=NA
    temp[is.na(csv.useful[day,])]=NA
    if(all(is.na(temp))){
      first.idx$closest.below <- c(first.idx$closest.below, NA)
    }else{
      first.idx$closest.below <- c(first.idx$closest.below, which.min(abs(temp)))
    }
  }
  return(first.idx)
}

algorithm <- function(scrip, csv.useful, first.idx) {
  scrip.returns <- data.frame()
  #chose a buy day and  buying the closest above strike option for positive breakout 
  buy.day <- first.idx$pos.breakout
  strike.sel.idx <- NA
  if(buy.day>0){# && min.mul.fac <= mulfac[first.idx$closest.above[buy.day]-offset]&&mulfac[first.idx$closest.above[buy.day]-offset] <= max.mul.fac) {
    strike.sel.idx <- first.idx$closest.above[buy.day]
  } else {
    #No positive breakout
    #chose a buy day and  buying the closest below strike option for negative breakout 
    buy.day <- first.idx$neg.beakout
    if(buy.day>0){# && min.mul.fac <= mulfac[first.idx$closest.above[buy.day]-offset-1]&&mulfac[first.idx$closest.above[buy.day]-offset-1] <= max.mul.fac) {
      strike.sel.idx <- first.idx$closest.below[buy.day]
    }
  }

  if(buy.day > 0 && !is.na(strike.sel.idx)){
    buy.at <- NA
    if(8 <= (nrow(csv.useful)-buy.day) && (nrow(csv.useful)-buy.day) <= 30) {
      #Since you are buying next day midday after the breakout
      nCall.prev = 1
      nPut.prev = csv.useful[buy.day, strike.sel.idx]/csv.useful[buy.day, strike.sel.idx+1]
      eod.prev.day <- nCall.prev*csv.useful[buy.day, strike.sel.idx]+nPut.prev*csv.useful[buy.day, strike.sel.idx+1]

      nCall = 1
      nPut = csv.useful[buy.day+1, strike.sel.idx]/csv.useful[buy.day+1, strike.sel.idx+1]
      eod.day <- nCall*csv.useful[buy.day+1, strike.sel.idx] + nPut*csv.useful[buy.day+1, strike.sel.idx+1]

      buy.at <- eod.prev.day+eod.day
    }
    if(!is.na(buy.at)) {
      sell.day <- nrow(csv.useful)
      #Revise sell day if it hits stoploss
      for(stop.loss.day in ((buy.day+1):(sell.day-1))) {
        stop.loss.at <- (nCall.prev + nCall)*csv.useful[stop.loss.day, strike.sel.idx] + (nPut.prev + nPut)*csv.useful[stop.loss.day, strike.sel.idx+1]
        if(stop.loss.at < (1-stoploss.percent/100)*buy.at){
          sell.day = stop.loss.day
          break;
        }
      }

      sell.at <- (nCall.prev + nCall)*csv.useful[sell.day, strike.sel.idx] + (nPut.prev + nPut)*csv.useful[sell.day, strike.sel.idx+1]
      returns = 100 * (sell.at - buy.at) / buy.at
      scrip.returns <- rbind(scrip.returns,data.frame(company=c(scrip), returns=c(as.character(returns)), buy.day=c(csv.useful[buy.day,1]), sell.day=c(csv.useful[sell.day,1])))
      #print(paste0(scrip,":",returns[[1]][[1]]))
    }
  }
  return(scrip.returns);
}

symbols=get.symbols()#[10:184]
for(expiryDate in exp.dates) {
  #expiryDate = exp.dates[2]
  g.scrip.returns <- data.frame(company=c("scrip"), returns=c(0), buy.day=c(as.Date("2016-01-28")),  sell.day = c(as.Date("2016-01-28")))
  for(s in symbols){#s="EICHER"#CROMPGREAV"
    tryCatch({
      files <- list.files(path = paste0(base,expiryDate,"/"), pattern = s)
      csv.useful <- prepapre.csv.useful(files)
      first.idx <- get.firsts(files, csv.useful)
      g.scrip.returns <- rbind(g.scrip.returns, algorithm(s, csv.useful, first.idx))
    }, error = function(error) {
    })
  }
  g.scrip.returns = g.scrip.returns[-1,]
  g.scrip.returns$returns<-as.numeric(g.scrip.returns$returns)
  print(paste0("Monthly.Ret:", mean(g.scrip.returns$returns,na.rm = T), " Calls:", nrow(g.scrip.returns), " Wins: ", sum(g.scrip.returns$returns>1.5,na.rm = T),  " Losses: ", sum(g.scrip.returns$returns<=1.5,na.rm = T)))
  gg.scrip.returns <- rbind(gg.scrip.returns, g.scrip.returns)
}

gg.scrip.returns = gg.scrip.returns[-1,]
print(paste0("Overall.Ret:", mean(gg.scrip.returns$returns,na.rm = T), " Calls:", nrow(gg.scrip.returns), " Wins: ", sum(gg.scrip.returns$returns>1.5,na.rm = T),  " Losses: ", sum(gg.scrip.returns$returns<=1.5,na.rm = T)))