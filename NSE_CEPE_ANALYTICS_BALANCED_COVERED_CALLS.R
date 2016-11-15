library(httr)
library(plyr)
library(XML)
library(stringr)
library(reshape2)
library(ggplot2)
library(data.table)

PositiveBreakout.call.expiry.hit = 0
PositiveBreakout.put.expiry.hit = 0
PositiveBreakout.call.target.hit = 0
PositiveBreakout.put.target.hit = 0
PositiveBreakout.call.stoploss.hit = 0
PositiveBreakout.put.stoploss.hit = 0

NegativeBreakout.call.expiry.hit = 0
NegativeBreakout.put.expiry.hit = 0
NegativeBreakout.call.target.hit = 0
NegativeBreakout.put.target.hit = 0
NegativeBreakout.call.stoploss.hit = 0
NegativeBreakout.put.stoploss.hit = 0

pos.beakout.val = 2
neg.beakout.val = -2
exp.dates<-c("28-01-2016","25-02-2016","31-03-2016","28-04-2016")
#exp.dates<-c("31-03-2016")
base <- "/Users/dutishan/Desktop/NSECEPE/"
# gg.scrip.returns <- data.frame(company=c("scrip"), returns=c(0),
#                                strike.sel.idx=c(0),
#                                bought.at=c(0),
#                                buy.day=c(as.Date("2016-01-28")),
#                                sold.at=c(0),
#                                sell.day = c(as.Date("2016-01-28"),
#                                sell.type=c("abc")))
get.symbols<-function() {
  url <- paste0("https://www.nseindia.com/content/fo/fo_underlyinglist.htm")
  response <- GET(url, user_agent("Mozilla"))
  tf <- tempfile()
  writeBin(content(response, "raw"), tf)
  tables <- readHTMLTable(tf)
  symbols <- as.vector(tables[4]$'NULL'$Symbol)
  return(symbols)# symbols <- c("AJANTPHARM")
}

prepapre.csv.useful <- function(files) {
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

get.firsts<- function(files, csv.useful) {
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
  
  csv.useful_long <- melt(csv.useful[, !(names(csv.useful) %in% c("Underlying.Value"))], id = "Date")
  l<-(strsplit(as.character(unique(csv.useful_long$variable)),"E."))
  strikes<-c()
  for(i in 1:length(l)) {
    strikes<-c(strikes,as.vector(l[i])[[1]][2])
  }
  strikes<-c(NA,NA,strikes)
  
  first.idx$closest.above<-NULL
  first.idx$closest.below<-NULL
  for(day in 1:nrow(csv.useful)) {
    strike.diff<-as.numeric(strikes)-csv.useful$Underlying.Value[day]
    temp = strike.diff
    temp[strike.diff<0]=NA
    temp[is.na(csv.useful[day,])]=NA
    if(all(is.na(temp))) {
      first.idx$closest.above <- c(first.idx$closest.above, NA)
    } else {
      first.idx$closest.above <- c(first.idx$closest.above, which.min(temp))
    }
    temp = strike.diff
    temp[strike.diff>=0]=NA
    temp[is.na(csv.useful[day,])]=NA
    if(all(is.na(temp))) {
      first.idx$closest.below <- c(first.idx$closest.below, NA)
    } else {
      first.idx$closest.below <- c(first.idx$closest.below, which.min(abs(temp)))
    }
  }
  return(first.idx)
}

get.buy.point <- function(first.idx) {
  #chose a buy day and  buying the closest above strike option for positive breakout 
  buy.day <- first.idx$pos.breakout
  strike.sel.idx <- NA
  mode <- NA
  if(buy.day>0) {# && min.mul.fac <= mulfac[first.idx$closest.above[buy.day]-offset]&&mulfac[first.idx$closest.above[buy.day]-offset] <= max.mul.fac) {
    mode="PositiveBreakout"
    strike.sel.idx <- first.idx$closest.above[buy.day]
  } else {
    #No positive breakout
    #chose a buy day and  buying the closest below strike option for negative breakout 
    buy.day <- first.idx$neg.beakout
    if(buy.day>0) {# && min.mul.fac <= mulfac[first.idx$closest.above[buy.day]-offset-1]&&mulfac[first.idx$closest.above[buy.day]-offset-1] <= max.mul.fac) {
      mode="NegativeBreakout"
      strike.sel.idx <- first.idx$closest.below[buy.day]
    }
  }
  return(list(buy.day=buy.day,strike.sel.idx=strike.sel.idx, mode=mode ))
}

get.buy.info <- function(buy.day, strike.sel.idx) {
  buy.info <- list()
  buy.info$buy.at <- NA
  buy.info$nCall.prev <- NA
  buy.info$nPut.prev <- NA
  buy.info$nCall <- NA
  buy.info$nPut <- NA
  #if(8 <= (nrow(csv.useful)-buy.day) && (nrow(csv.useful)-buy.day) <= 30) {
  #Since you are buying next day midday after the breakout
  buy.info$nCall.prev = 1
  buy.info$nPut.prev = csv.useful[buy.day, strike.sel.idx]/csv.useful[buy.day, strike.sel.idx + 1]
  eod.prev.day <- buy.info$nCall.prev*csv.useful[buy.day, strike.sel.idx]+
    buy.info$nPut.prev*csv.useful[buy.day, strike.sel.idx + 1]
  buy.info$nCall = 1
  buy.info$nPut = csv.useful[buy.day+1, strike.sel.idx]/csv.useful[buy.day+1, strike.sel.idx + 1]
  eod.day <- buy.info$nCall*csv.useful[buy.day+1, strike.sel.idx] + 
    buy.info$nPut*csv.useful[buy.day+1, strike.sel.idx + 1]
  
  buy.info$buy.at <- eod.prev.day+eod.day
  #}
  return(buy.info)
}

stoploss<-function(csv.useful, strike.sel.idx, buy.day, buy.at) {
  return (buy.day+which(csv.useful[(buy.day+1):sell.day$call,strike.sel.idx] <= (buy.at/4)/3)[1]);
}

target<-function(csv.useful, strike.sel.idx, buy.day, buy.at) {
  return (buy.day+which(csv.useful[(buy.day+1):sell.day$call,strike.sel.idx] >= 3*(buy.at/4))[1]);
}

get.sell.details <- function(csv.useful, buy.point, buy.info) {
  sell.day <- list()
  sell.day$call <- nrow(csv.useful)
  sell.day$put <- nrow(csv.useful)
  comments <- c()
  call.first.stoploss <- stoploss(csv.useful, buy.point$strike.sel.idx, buy.point$buy.day, buy.info$buy.at)
  call.first.target <-  target(csv.useful, buy.point$strike.sel.idx, buy.point$buy.day, buy.info$buy.at)
  #   print(call.first.stoploss)
  #   print(call.first.target)
  if(is.na(call.first.stoploss) && is.na(call.first.target)) {
    #do nothing
    if(buy.point$mode=="PositiveBreakout") {
      assign("PositiveBreakout.call.expiry.hit", PositiveBreakout.call.expiry.hit + 1, envir = .GlobalEnv)
      comments = c(comments, "PositiveBreakout.call.expiry.hit")
    } else {
      assign("NegativeBreakout.call.expiry.hit", NegativeBreakout.call.expiry.hit + 1, envir = .GlobalEnv)
      comments = c(comments, "NegativeBreakout.call.expiry.hit")
    }
  } else if(is.na(call.first.stoploss) && !is.na(call.first.target)) {
    
    if(buy.point$mode=="PositiveBreakout") {
      assign("PositiveBreakout.call.target.hit", PositiveBreakout.call.target.hit + 1, envir = .GlobalEnv)
      comments = c(comments, "PositiveBreakout.call.target.hit")
    } else {
      assign("NegativeBreakout.call.target.hit", NegativeBreakout.call.target.hit + 1, envir = .GlobalEnv)
      comments = c(comments, "NegativeBreakout.call.target.hit")
    }
    sell.day$call <- call.first.target
  } else if(!is.na(call.first.stoploss) && is.na(call.first.target)) {
    if(buy.point$mode=="PositiveBreakout") {
      assign("PositiveBreakout.call.stoploss.hit", PositiveBreakout.call.stoploss.hit + 1, envir = .GlobalEnv)
      comments = c(comments, "PositiveBreakout.call.stoploss.hit")
    } else {
      assign("NegativeBreakout.call.stoploss.hit", NegativeBreakout.call.stoploss.hit + 1, envir = .GlobalEnv)
      comments = c(comments, "NegativeBreakout.call.stoploss.hit")
    }
    sell.day$call <- call.first.stoploss
  } else if(call.first.stoploss <= call.first.target) {
    
    if(buy.point$mode=="PositiveBreakout") {
      assign("PositiveBreakout.call.stoploss.hit", PositiveBreakout.call.stoploss.hit + 1, envir = .GlobalEnv)
      comments = c(comments, "PositiveBreakout.call.stoploss.hit")
    } else {
      assign("NegativeBreakout.call.stoploss.hit", NegativeBreakout.call.stoploss.hit + 1, envir = .GlobalEnv)
      comments = c(comments, "NegativeBreakout.call.stoploss.hit")
    }
    sell.day$call <- call.first.stoploss
  } else {
    if(buy.point$mode=="PositiveBreakout") {
      assign("PositiveBreakout.call.target.hit", PositiveBreakout.call.target.hit + 1, envir = .GlobalEnv)
      comments = c(comments, "PositiveBreakout.call.target.hit")
    } else {
      assign("NegativeBreakout.call.target.hit", NegativeBreakout.call.target.hit + 1, envir = .GlobalEnv)
      comments = c(comments, "NegativeBreakout.call.target.hit")
    }
    sell.day$call <- call.first.target
  }
  
  put.first.stoploss <- stoploss(csv.useful, buy.point$strike.sel.idx+1, buy.point$buy.day,buy.info$buy.at)
  put.first.target <-  target(csv.useful, buy.point$strike.sel.idx+1, buy.point$buy.day,buy.info$buy.at)
  #    print(put.first.stoploss)
  #    print(put.first.target)
  if(is.na(put.first.stoploss) && is.na(put.first.target)) {
    #do nothing
    if(buy.point$mode=="PositiveBreakout") {
      assign("PositiveBreakout.put.expiry.hit", PositiveBreakout.put.expiry.hit + 1, envir = .GlobalEnv)
      comments = c(comments, "PositiveBreakout.put.expiry.hit")
    } else {
      assign("NegativeBreakout.put.expiry.hit", PositiveBreakout.put.expiry.hit + 1, envir = .GlobalEnv)
      comments = c(comments, "NegativeBreakout.put.expiry.hit")
    }
  } else if(is.na(put.first.stoploss) && !is.na(put.first.target)) {
    
    if(buy.point$mode=="PositiveBreakout") {
      assign("PositiveBreakout.put.target.hit", PositiveBreakout.put.target.hit + 1, envir = .GlobalEnv)
      comments = c(comments, "PositiveBreakout.put.target.hit")
    } else {
      assign("NegativeBreakout.put.target.hit", NegativeBreakout.put.target.hit + 1, envir = .GlobalEnv)
      comments = c(comments, "NegativeBreakout.put.target.hit")
    }
    sell.day$put <- put.first.target
  } else if(!is.na(put.first.stoploss) && is.na(put.first.target)) {
    
    if(buy.point$mode=="PositiveBreakout") {
      assign("PositiveBreakout.put.stoploss.hit", PositiveBreakout.put.stoploss.hit + 1, envir = .GlobalEnv)
      comments = c(comments, "PositiveBreakout.put.stoploss.hit")
    } else {
      assign("NegativeBreakout.put.stoploss.hit", NegativeBreakout.put.stoploss.hit + 1, envir = .GlobalEnv)
      comments = c(comments, "NegativeBreakout.put.stoploss.hit")
    }
    sell.day$put <- put.first.stoploss
  } else if(put.first.stoploss <= put.first.target) {
    if(buy.point$mode=="PositiveBreakout") {
      assign("PositiveBreakout.put.stoploss.hit", PositiveBreakout.put.stoploss.hit + 1, envir = .GlobalEnv)
      comments = c(comments, "PositiveBreakout.put.stoploss.hit")
    } else {
      assign("NegativeBreakout.put.stoploss.hit", NegativeBreakout.put.stoploss.hit + 1, envir = .GlobalEnv)
      comments = c(comments, "NegativeBreakout.put.stoploss.hit")
    }
    sell.day$put <- put.first.stoploss
  } else {
    if(buy.point$mode=="PositiveBreakout") {
      assign("PositiveBreakout.put.target.hit", PositiveBreakout.put.target.hit + 1, envir = .GlobalEnv)
      comments = c(comments, "PositiveBreakout.put.target.hit")
    } else {
      assign("NegativeBreakout.put.target.hit", NegativeBreakout.put.target.hit + 1, envir = .GlobalEnv)
      comments = c(comments, "NegativeBreakout.put.target.hit")
    }
    sell.day$put <- put.first.target
  }
  
  #Revise sell day if it hits stoploss
  #       for(stop.loss.day in ((buy.day+1):(sell.day-1))) {
  #         stop.loss.at <- (nCall.prev + nCall)*csv.useful[stop.loss.day, strike.sel.idx] + (nPut.prev + nPut)*csv.useful[stop.loss.day, strike.sel.idx + 1]
  #         if(stop.loss.at < (1-stoploss.percent/100)*buy.at) {
  #           sell.day = stop.loss.day
  #           break;
  #         }
  #       }
  return(list(sell.day=sell.day, sell.type=comments))
}

algorithm <- function(scrip, csv.useful, first.idx) {
  buy.point <- get.buy.point(first.idx)
  if(!anyNA(buy.point) && buy.point$buy.day > 0) {
    buy.info <- get.buy.info(buy.point$buy.day, buy.point$strike.sel.idx)
    if(!anyNA(buy.info)) {
      sell.details <- get.sell.details(csv.useful, buy.point, buy.info)
      sell.at <- (buy.info$nCall.prev + buy.info$nCall)*csv.useful[sell.details$sell.day$call, buy.point$strike.sel.idx] +
        (buy.info$nPut.prev + buy.info$nPut)*csv.useful[sell.details$sell.day$put, buy.point$strike.sel.idx + 1]
      returns = 100 * (sell.at - buy.info$buy.at) / buy.info$buy.at
      #print(paste0(scrip,":",returns[[1]][[1]]))
      scrip.returns <- data.frame(company=c(scrip),
                                                      returns=c(as.character(returns)),
                                                      strike.sel.idx=c(names(csv.useful)[buy.point$strike.sel.idx]),
                                                      bought.at=c(buy.info$buy.at),
                                                      buy.day=c(csv.useful[buy.point$buy.day,1]),
                                                      sold.at=c(sell.at),
                                                      sell.day=c(csv.useful[sell.details$sell.day$call,1]),
                                                      sell.type=c(paste0(sell.details$sell.type[1]," ",sell.details$sell.type[2])))
    }
  }
  return(scrip.returns);
}

symbols=get.symbols()#[10:184]
#for(expiryDate in exp.dates) {
expiryDate = exp.dates[1]
g.scrip.returns <- data.frame();

for(scrip in symbols[30:50]) {
  #scrip=symbols[31]#"EICHER"#CROMPGREAV"
  tryCatch({
    files <- list.files(path = paste0(base,expiryDate,"/"), pattern = scrip)
    csv.useful <- prepapre.csv.useful(files)
    first.idx <- get.firsts(files, csv.useful)
    if(nrow(g.scrip.returns)==0){
      g.scrip.returns <- algorithm(scrip, csv.useful, first.idx)
    }else{
      g.scrip.returns <- rbind(g.scrip.returns, algorithm(scrip, csv.useful, first.idx))
    }
  }, error = function(error) {
  })
}
g.scrip.returns$returns<-as.numeric(g.scrip.returns$returns)
print(paste0("Monthly.Ret:", mean(g.scrip.returns$returns,na.rm = T), " Calls:", nrow(g.scrip.returns), " Wins: ", sum(g.scrip.returns$returns>1.5,na.rm = T),  " Losses: ", sum(g.scrip.returns$returns<=1.5,na.rm = T)))
gg.scrip.returns <- rbind(gg.scrip.returns, g.scrip.returns)
if(nrow(gg.scrip.returns)==0){
  gg.scrip.returns <- g.scrip.returns
}else{
  gg.scrip.returns <- rbind(gg.scrip.returns, g.scrip.returns)
}

#}

print(paste0("Overall.Ret:", mean(gg.scrip.returns$returns,na.rm = T), " Calls:", nrow(gg.scrip.returns), " Wins: ", sum(gg.scrip.returns$returns>1.5,na.rm = T),  " Losses: ", sum(gg.scrip.returns$returns<=1.5,na.rm = T)))
print(paste0("PositiveBreakoutCallStoplossHit=", PositiveBreakout.put.stoploss.hit))
print(paste0("PositiveBreakoutPutStoplossHit=", PositiveBreakout.put.stoploss.hit))
print(paste0("PositiveBreakoutCallTargetHit=", PositiveBreakout.call.target.hit))
print(paste0("PositiveBreakoutPutTargetHit=", PositiveBreakout.put.target.hit))
print(paste0("PositiveBreakoutCallExpiryHit=", PositiveBreakout.call.expiry.hit))
print(paste0("PositiveBreakoutPutExpiryHit=", PositiveBreakout.put.expiry.hit))

print(paste0("NegativeBreakoutCallStoplossHit=", Put.stoploss.hit))
print(paste0("NegativeBreakoutPutStoplossHit=", NegativeBreakout.put.stoploss.hit))
print(paste0("NegativeBreakoutCallTargetHit=", NegativeBreakout.call.target.hit))
print(paste0("NegativeBreakoutPutTargetHit=", NegativeBreakout.put.target.hit))
print(paste0("NegativeBreakoutCallExpiryHit=", NegativeBreakout.call.expiry.hit))
print(paste0("NegativeBreakoutPutExpiryHit=", NegativeBreakout.put.expiry.hit))