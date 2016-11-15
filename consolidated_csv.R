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
#,
exp.dates <- c("26-05-2016","30-06-2016","28-01-2016","25-02-2016","31-03-2016","28-04-2016")
#exp.dates <- c("31-03-2016")
base <- "/Users/dutishan/Desktop/NSECEPE/"
output <- "Consolidated/"

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

symbols=get.symbols()#[10:184]

for(expiryDate in exp.dates) {
  #expiryDate = exp.dates[1]
  for(scrip in symbols) {
    if(!(scrip %in% c('CADILAHC', 'ONGC', 'MCDOWELL', 'BEML', 'DRREDDY'))){
      #scrip=symbols[36]#"EICHER"#CROMPGREAV"
      tryCatch({
        files <- list.files(path = paste0(base,expiryDate,"/"), pattern = scrip)
        csv.useful <- prepapre.csv.useful(files)
        write.csv(csv.useful, file = paste0(base,output,expiryDate,"/",scrip,"_",expiryDate,".csv"), sep = ", ",row.names = F)
      }, error = function(error) {
      })
    }
  }

}
