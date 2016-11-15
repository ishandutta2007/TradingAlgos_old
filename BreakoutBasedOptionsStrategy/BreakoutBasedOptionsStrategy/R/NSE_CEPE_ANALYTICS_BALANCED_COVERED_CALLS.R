library(httr)
library(plyr)
library(XML)
library(stringr)
library(reshape2)
library(ggplot2)
library(data.table)
rm(list = ls())
f = 2
is.stoploss.enabled = F
is.target.enabled = F

PosBrkout.call.expiry.hit = 0
PosBrkout.put.expiry.hit = 0
PosBrkout.call.target.hit = 0
PosBrkout.put.target.hit = 0
PosBrkout.call.stoploss.hit = 0
PosBrkout.put.stoploss.hit = 0

NegBrkout.call.expiry.hit = 0
NegBrkout.put.expiry.hit = 0
NegBrkout.call.target.hit = 0
NegBrkout.put.target.hit = 0
NegBrkout.call.stoploss.hit = 0
NegBrkout.put.stoploss.hit = 0

weight.my.bet = 1
min.buy.value = 1

pos.beakout.val = 5
max.pos.beakout.val = 7

neg.beakout.val = -5
max.neg.beakout.val = -9

exp.dates <- c("28-01-2016","25-02-2016","31-03-2016","28-04-2016")#,"26-05-2016","30-06-2016")
#exp.dates <- c("31-03-2016")
base <- "/Users/dutishan/Desktop/NSECEPE/"

gg.scrip.returns <- data.frame();
get.symbols <- function() {
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
  ord <- c()
  for (i in 1:length(files)) {
    ord <- c(ord,lapply(files,strsplit,"_")[[i]][[1]][3])
  }
  df=data.frame(files <- as.character(files),order <- as.numeric(ord))
  df=df[with(df, order(order)), ]
  files <- as.character(df$files)
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
    csv.useful$Date <- as.Date(csv.useful$Date)
    csv.useful <- csv.useful[with(csv.useful, order(Date)), ]
  }
  csv.useful$Value.Change <- c(0,diff(csv.useful$Underlying.Value)/csv.useful$Underlying.Value[ - 1])*100
  return(csv.useful)
}

get.firsts <- function(files, csv.useful) {
  first.idx <- list()
  first.idx$all.strikes.day <- which.max(complete.cases(csv.useful))

  if(max.pos.beakout.val>=csv.useful$Value.Change[1] & csv.useful$Value.Change[1]>=pos.beakout.val) {
    first.idx$pos.breakout = 1
  } else {
    first.idx$pos.breakout <- which.max(max.pos.beakout.val>=csv.useful$Value.Change & csv.useful$Value.Change>=pos.beakout.val)
    if(first.idx$pos.breakout == 1) first.idx$pos.breakout = - 1
  }

  if(max.neg.beakout.val<=csv.useful$Value.Change[1] & csv.useful$Value.Change[1] <= neg.beakout.val) {
    first.idx$neg.beakout.idx = 1
  } else {
    first.idx$neg.beakout <- which.max(max.neg.beakout.val <= csv.useful$Value.Change & csv.useful$Value.Change <= neg.beakout.val)
    if(first.idx$neg.beakout == 1)first.idx$neg.beakout = - 1
  }

  csv.useful_long <- melt(csv.useful[, !(names(csv.useful) %in% c("Underlying.Value"))], id = "Date")
  l <- (strsplit(as.character(unique(csv.useful_long$variable)),"E."))
  strikes <- c()
  for(i in 1:length(l)) {
    strikes <- c(strikes,as.vector(l[i])[[1]][2])
  }
  strikes <- c(NA,NA,strikes)

  first.idx$closest.above <- NULL
  first.idx$closest.below <- NULL
  for(day in 1:nrow(csv.useful)) {
    strike.diff <- as.numeric(strikes) - csv.useful$Underlying.Value[day]
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
  #chose a buy day and buying the closest above strike option for positive breakout
  buy.day <- first.idx$pos.breakout
  strike.sel.idx <- NA
  mode <- NA
  if(buy.day>6) {# && min.mul.fac <= mulfac[first.idx$closest.above[buy.day] - offset]&&mulfac[first.idx$closest.above[buy.day] - offset] <= max.mul.fac) {
    rp=mean(csv.useful[(buy.day-6):(buy.day-1),ncol(csv.useful)])
    if((-0.9<=rp && rp<=-0.6) || (0<=rp && rp<=0.3)){
      mode="PosBrkout"
      strike.sel.idx <- first.idx$closest.above[buy.day]
      # strike.sel.idx <- first.idx$closest.below[buy.day]
    }
  } else {
    #No positive breakout
    #chose a buy day and buying the closest below strike option for negative breakout
    buy.day <- first.idx$neg.beakout
    if(buy.day>6) {# && min.mul.fac <= mulfac[first.idx$closest.above[buy.day] - offset - 1]&&mulfac[first.idx$closest.above[buy.day] - offset - 1] <= max.mul.fac) {
      rn=mean(csv.useful[(buy.day-6):(buy.day-1),ncol(csv.useful)])
      if((0<=rn && rn<=0.8)){
        mode="NegBrkout"
        strike.sel.idx <- first.idx$closest.below[buy.day]
      }
      # strike.sel.idx <- first.idx$closest.above[buy.day]
      #}
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
  if(8 <= (nrow(csv.useful) - buy.day) && (nrow(csv.useful) - buy.day) <= 60) {
    #Since you are buying next day midday after the breakout
    if(csv.useful[buy.day,ncol(csv.useful)] <0 ){
      wp=weight.my.bet
      wc=1
    } else {
      wp=1
      wc=weight.my.bet
    }
    buy.info$nCall.prev = 1*wc
    buy.info$nPut.prev = wp*csv.useful[buy.day, strike.sel.idx]/csv.useful[buy.day, strike.sel.idx + 1]
    eod.prev.day <- buy.info$nCall.prev*csv.useful[buy.day, strike.sel.idx]+  buy.info$nPut.prev*csv.useful[buy.day, strike.sel.idx + 1]

    buy.info$nCall = 1*wc
    buy.info$nPut = wp*csv.useful[buy.day+1, strike.sel.idx]/csv.useful[buy.day+1, strike.sel.idx + 1]
    eod.day <- buy.info$nCall*csv.useful[buy.day+1, strike.sel.idx] + buy.info$nPut*csv.useful[buy.day+1, strike.sel.idx + 1]
    buy.info$buy.at <- eod.prev.day+eod.day
  }
  if(abs(mean(csv.useful[(buy.day-6):(buy.day-1),ncol(csv.useful)])*buy.info$nPut)>1.5) buy.info$buy.at = NA
  if(buy.info$nPut <= 0.25 || buy.info$nPut >= 10 || buy.info$buy.at < 4*min.buy.value) buy.info$buy.at = NA
  return(buy.info)
}

stoploss <- function(csv.useful, strike.sel.idx, buy.day, buy.at) {
  if(is.stoploss.enabled) {
    return (buy.day+which(csv.useful[(buy.day+1):nrow(csv.useful),strike.sel.idx] <= (buy.at/4)/f)[1])
  } else {
    return(NA)
  }
}

target <- function(csv.useful, strike.sel.idx, buy.day, buy.at) {
  if(is.target.enabled) {
    return (buy.day+which(csv.useful[(buy.day+1):nrow(csv.useful),strike.sel.idx] >= f*(buy.at/4))[1])
  } else {
    return(NA)
  }
}

get.sell.details <- function(csv.useful, buy.point, buy.info) {
  sell.day <- list()
  sell.day$call <- nrow(csv.useful)
  sell.day$put <- nrow(csv.useful)
  comments <- c()
  call.first.stoploss <- stoploss(csv.useful, buy.point$strike.sel.idx, buy.point$buy.day, buy.info$buy.at)
  call.first.target <- target(csv.useful, buy.point$strike.sel.idx, buy.point$buy.day, buy.info$buy.at)
  # print(call.first.stoploss)
  # print(call.first.target)
  if(is.na(call.first.stoploss) && is.na(call.first.target)) {
    #do nothing
    if(buy.point$mode=="PosBrkout") {
      assign("PosBrkout.call.expiry.hit", PosBrkout.call.expiry.hit + 1, envir = .GlobalEnv)
      comments = c(comments, "PosBrkout.call.expiry.hit")
    } else {
      assign("NegBrkout.call.expiry.hit", NegBrkout.call.expiry.hit + 1, envir = .GlobalEnv)
      comments = c(comments, "NegBrkout.call.expiry.hit")
    }
  } else if(is.na(call.first.stoploss) && !is.na(call.first.target)) {
    if(buy.point$mode=="PosBrkout") {
      assign("PosBrkout.call.target.hit", PosBrkout.call.target.hit + 1, envir = .GlobalEnv)
      comments = c(comments, "PosBrkout.call.target.hit")
    } else {
      assign("NegBrkout.call.target.hit", NegBrkout.call.target.hit + 1, envir = .GlobalEnv)
      comments = c(comments, "NegBrkout.call.target.hit")
    }
    sell.day$call <- call.first.target
  } else if(!is.na(call.first.stoploss) && is.na(call.first.target)) {
    if(buy.point$mode=="PosBrkout") {
      assign("PosBrkout.call.stoploss.hit", PosBrkout.call.stoploss.hit + 1, envir = .GlobalEnv)
      comments = c(comments, "PosBrkout.call.stoploss.hit")
    } else {
      assign("NegBrkout.call.stoploss.hit", NegBrkout.call.stoploss.hit + 1, envir = .GlobalEnv)
      comments = c(comments, "NegBrkout.call.stoploss.hit")
    }
    sell.day$call <- call.first.stoploss
  } else if(call.first.stoploss <= call.first.target) {
    
    if(buy.point$mode=="PosBrkout") {
      assign("PosBrkout.call.stoploss.hit", PosBrkout.call.stoploss.hit + 1, envir = .GlobalEnv)
      comments = c(comments, "PosBrkout.call.stoploss.hit")
    } else {
      assign("NegBrkout.call.stoploss.hit", NegBrkout.call.stoploss.hit + 1, envir = .GlobalEnv)
      comments = c(comments, "NegBrkout.call.stoploss.hit")
    }
    sell.day$call <- call.first.stoploss
  } else {
    if(buy.point$mode=="PosBrkout") {
      assign("PosBrkout.call.target.hit", PosBrkout.call.target.hit + 1, envir = .GlobalEnv)
      comments = c(comments, "PosBrkout.call.target.hit")
    } else {
      assign("NegBrkout.call.target.hit", NegBrkout.call.target.hit + 1, envir = .GlobalEnv)
      comments = c(comments, "NegBrkout.call.target.hit")
    }
    sell.day$call <- call.first.target
  }
  
  put.first.stoploss <- stoploss(csv.useful, buy.point$strike.sel.idx+1, buy.point$buy.day,buy.info$buy.at)
  put.first.target <- target(csv.useful, buy.point$strike.sel.idx+1, buy.point$buy.day,buy.info$buy.at)
  # print(put.first.stoploss)
  # print(put.first.target)
  if(is.na(put.first.stoploss) && is.na(put.first.target)) {
    #do nothing
    if(buy.point$mode=="PosBrkout") {
      assign("PosBrkout.put.expiry.hit", PosBrkout.put.expiry.hit + 1, envir = .GlobalEnv)
      comments = c(comments, "PosBrkout.put.expiry.hit")
    } else {
      assign("NegBrkout.put.expiry.hit", PosBrkout.put.expiry.hit + 1, envir = .GlobalEnv)
      comments = c(comments, "NegBrkout.put.expiry.hit")
    }
  } else if(is.na(put.first.stoploss) && !is.na(put.first.target)) {
    
    if(buy.point$mode=="PosBrkout") {
      assign("PosBrkout.put.target.hit", PosBrkout.put.target.hit + 1, envir = .GlobalEnv)
      comments = c(comments, "PosBrkout.put.target.hit")
    } else {
      assign("NegBrkout.put.target.hit", NegBrkout.put.target.hit + 1, envir = .GlobalEnv)
      comments = c(comments, "NegBrkout.put.target.hit")
    }
    sell.day$put <- put.first.target
  } else if(!is.na(put.first.stoploss) && is.na(put.first.target)) {
    
    if(buy.point$mode=="PosBrkout") {
      assign("PosBrkout.put.stoploss.hit", PosBrkout.put.stoploss.hit + 1, envir = .GlobalEnv)
      comments = c(comments, "PosBrkout.put.stoploss.hit")
    } else {
      assign("NegBrkout.put.stoploss.hit", NegBrkout.put.stoploss.hit + 1, envir = .GlobalEnv)
      comments = c(comments, "NegBrkout.put.stoploss.hit")
    }
    sell.day$put <- put.first.stoploss
  } else if(put.first.stoploss <= put.first.target) {
    if(buy.point$mode=="PosBrkout") {
      assign("PosBrkout.put.stoploss.hit", PosBrkout.put.stoploss.hit + 1, envir = .GlobalEnv)
      comments = c(comments, "PosBrkout.put.stoploss.hit")
    } else {
      assign("NegBrkout.put.stoploss.hit", NegBrkout.put.stoploss.hit + 1, envir = .GlobalEnv)
      comments = c(comments, "NegBrkout.put.stoploss.hit")
    }
    sell.day$put <- put.first.stoploss
  } else {
    if(buy.point$mode=="PosBrkout") {
      assign("PosBrkout.put.target.hit", PosBrkout.put.target.hit + 1, envir = .GlobalEnv)
      comments = c(comments, "PosBrkout.put.target.hit")
    } else {
      assign("NegBrkout.put.target.hit", NegBrkout.put.target.hit + 1, envir = .GlobalEnv)
      comments = c(comments, "NegBrkout.put.target.hit")
    }
    sell.day$put <- put.first.target
  }
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
                                  returns=c(returns[[1]][[1]]),
                                  breakout=c(csv.useful[buy.point$buy.day,ncol(csv.useful)]),
                                  strike.sel.idx=c(names(csv.useful)[buy.point$strike.sel.idx]),
                                  nPut.nCall.ratio=c(buy.info$nPut),
                                  buy.day=c(csv.useful[buy.point$buy.day,1]),
                                  roll.mean.prev5 = c(mean(csv.useful[(buy.point$buy.day-6):(buy.point$buy.day-1),ncol(csv.useful)])),
                                  sold.at=c(sell.at),
                                  sell.day=c(csv.useful[sell.details$sell.day$call,1]),
                                  sell.type=c(paste0(sell.details$sell.type[1]," ",sell.details$sell.type[2])))
    }
  }
  return(scrip.returns);
}

symbols=get.symbols()#[10:184]

for(expiryDate in exp.dates) {
  #expiryDate = exp.dates[1]
  g.scrip.returns <- data.frame();
  
  for(scrip in symbols) {
    if(!(scrip %in% c('CADILAHC', 'ONGC', 'MCDOWELL', 'BEML', 'DRREDDY'))){
      #scrip=symbols[36]#"EICHER"#CROMPGREAV"
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
  }
  g.scrip.returns$returns <- as.numeric(g.scrip.returns$returns)
  print(paste0("Monthly.Ret:", mean(g.scrip.returns$returns,na.rm = T), " Calls:", nrow(g.scrip.returns), " Wins: ", sum(g.scrip.returns$returns>1.5,na.rm = T), " Losses: ", sum(g.scrip.returns$returns<=1.5,na.rm = T)))
  if(nrow(gg.scrip.returns)==0){
    gg.scrip.returns <- g.scrip.returns
  }else{
    gg.scrip.returns <- rbind(gg.scrip.returns, g.scrip.returns)
  }
  
}

# print(paste0("PosBrkoutCallStoplossHit=", PosBrkout.put.stoploss.hit))
# print(paste0("PosBrkoutPutStoplossHit=", PosBrkout.put.stoploss.hit))
# print(paste0("PosBrkoutCallTargetHit=", PosBrkout.call.target.hit))
# print(paste0("PosBrkoutPutTargetHit=", PosBrkout.put.target.hit))
# print(paste0("PosBrkoutCallExpiryHit=", PosBrkout.call.expiry.hit))
# print(paste0("PosBrkoutPutExpiryHit=", PosBrkout.put.expiry.hit))
#
# print(paste0("NegBrkoutCallStoplossHit=",NegBrkout.call.stoploss.hit))
# print(paste0("NegBrkoutPutStoplossHit=", NegBrkout.put.stoploss.hit))
# print(paste0("NegBrkoutCallTargetHit=", NegBrkout.call.target.hit))
# print(paste0("NegBrkoutPutTargetHit=", NegBrkout.put.target.hit))
# print(paste0("NegBrkoutCallExpiryHit=", NegBrkout.call.expiry.hit))
# print(paste0("NegBrkoutPutExpiryHit=", NegBrkout.put.expiry.hit))
#printmetrics()
print(paste0("Overall.Ret:", mean(gg.scrip.returns$returns,na.rm = T), " Calls:", nrow(gg.scrip.returns), " Wins: ", sum(gg.scrip.returns$returns>1.5,na.rm = T), " Losses: ", sum(gg.scrip.returns$returns<=1.5,na.rm = T)))

require(plotly)
plot_ly(data=gg.scrip.returns,x=nPut.nCall.ratio,y=breakout, type = "scatter", mode = "markers", color = sign(returns-30))
plot_ly(data=gg.scrip.returns,x=nPut.nCall.ratio,y=roll.mean.prev5, type = "scatter", mode = "markers", color = sign(returns-30))
plot_ly(data=gg.scrip.returns,x=breakout,y=roll.mean.prev5, type = "scatter", mode = "markers", color = sign(returns-30))
gg.scrip.returns$psudo = abs(gg.scrip.returns$nPut.nCall.ratio*gg.scrip.returns$roll.mean.prev5)
plot_ly(data=gg.scrip.returns,x=breakout,y=psudo, type = "scatter", mode = "markers", color = sign(returns-10))