library(httr)
library(plyr)
library(XML)
library(stringr)
library(curl)
#"28-01-2016"
#"25-02-2016"
#"31-03-2016"
#"28-04-2016"
#"26-05-2016"
#"30-06-2016"
#"28-07-2016"
#"25-08-2016"
exp.dates<-c("28-01-2016","25-02-2016","31-03-2016","28-04-2016","26-05-2016", "30-06-2016","28-07-2016","25-08-2016")
expiryDate <- exp.dates[5]
base <- "/Users/dutishan/Desktop/NSECEPE/"

prepare.table <- function(optionType,symbol,strikePrice,expiryDate) {
  baseurl <- "https://www.nseindia.com/products/dynaContent/common/productsSymbolMapping.jsp?"
  instrumentType <- "OPTSTK"
  url <- paste0(
    baseurl,
    "instrumentType=", instrumentType,
    "&symbol=", symbol,
    "&expiryDate=", expiryDate,
    "&optionType=", optionType,
    "&strikePrice=", strikePrice,
    "&dateRange=12month&fromDate=&toDate=&segmentLink=9&symbolCount="
  )
  response <- GET(url, user_agent("Mozilla"))
  #response$status
  tf <- tempfile()
  writeBin(content(response, "raw"), tf)
  tables = readHTMLTable(readLines(tf))
  table1 = tables$'NULL'
  if (nrow(table1) > 2) {
    colnames(table1) = as.vector(as.matrix(table1)[1,])
    table1 = table1[-1,]
    table1$Date <- as.Date(table1$Date,"%d-%b-%Y")
    for(i in 5:17){
      table1[,i] <- as.numeric(gsub(",","",table1[,i]))
    }
    #lapply(table1, class)
    table1 <- table1[table1$LTP > 0,]
    names(table1) <- c(
      names(table1)[1:3],
      paste0(optionType, ".",names(table1)[4:4]),
      names(table1)[5:5],
      paste0(optionType, ".",names(table1)[6:16]),
      names(table1)[17:17]
    )
  }
  return(table1)
}

get.proximity.value <- function(symbol) {
  v = 10
  tryCatch({
    goog.url <-paste0("http://www.google.com/finance/getprices?q=", symbol, "&x=NSE&i=300&f=d%2Cc%2Ch%2Cl%2Co%2Cv&p=60d")
    conn.goog <- curl(goog.url)
    v = as.numeric(strsplit(readLines(conn.goog)[10],',')[[1]][3])
  }, error = function(e) {
    print(paste0(e," Initiating with defalut strikePrice of 10"))
    ?message
    ?message
  }, finally = {
    close(conn.goog)
  })
  return(v)
}

get.symbols<-function() {
  url <- paste0("https://www.nseindia.com/content/fo/fo_underlyinglist.htm")
  response <- GET(url, user_agent("Mozilla"))
  tf <- tempfile()
  writeBin(content(response, "raw"), tf)
  tables <- readHTMLTable(tf)
  symbols <- as.vector(tables[4]$'NULL'$Symbol)
  return(symbols)# symbols <- c("AJANTPHARM")CROMPGREAV
}

symbols <- get.symbols()

for (i in 88:95) {
  tryCatch({
    print(paste0(i,".",symbols[i]))
    pv <- get.proximity.value(symbols[i])
    print(paste0("PV:",pv))
    started = F
    exited = 0
    if (pv < 800)  {
      strikePrice <- 5 * round(pv / 10)
      maxstrikePrice <- 10 * round(pv / 2)
      steps = 5;
    } else if (800 <= pv && pv < 8000) {
      strikePrice <- 50 * round(pv / 100)
      maxstrikePrice <- 50 * round(pv / 12)
      steps = 50
    } else if (pv >= 8000) {
      strikePrice <- 100 * round(pv / 200)
      maxstrikePrice <- 100 * round(pv / 50)
      steps = 100
    }
    
    print(paste0(i,".",symbols[i],".strikePrice:",strikePrice))
    print(paste0(i,".",symbols[i],".maxstrikePrice:",maxstrikePrice))
    while (strikePrice < maxstrikePrice) {
      print(
        paste0(
          i,".",symbols[i], ".strikePrice:",strikePrice, "started=",started,"exitcnt=",exited
        )
      )
      tableCE <- prepare.table("CE", symbols[i], as.character(strikePrice), expiryDate)
      tablePE <- data.frame(class <- lapply(tableCE, class))
      table.Final <- data.frame(class <- lapply(tableCE, class))
      if(length(lapply(tableCE, class)) > 1) {
        tablePE = tablePE[-1,]
        table.Final = table.Final[-1,]
        #print(paste0("nrow(tableCE)",nrow(tableCE)))
      }
      if (nrow(tableCE) >= 3) {
        tablePE <- prepare.table("PE", symbols[i], as.character(strikePrice), expiryDate)
        table.Final <- merge(tableCE, tablePE)
      }
      #print(paste0("nrow(table.Final)",nrow(table.Final),"ncol(table.Final)",ncol(table.Final),"ISNULL=",is.null(table.Final)))
      if (nrow(table.Final) >= 3) {
        started = TRUE
        exited = 0
        drops <- c("CE.Optiontype", "PE.Optiontype")
        table.Final <- table.Final[,!(names(table.Final) %in% drops)]
        filename <- paste0(symbols[i], "_CEPE_", strikePrice, "_", expiryDate,".csv")
        write.csv(table.Final,paste0(base, "/",expiryDate,"/", filename), row.names = F)
      } else if (started == T) {
        exited = exited + 1
        print(paste0(i,".",symbols[i], ". exit at strikePrice:",strikePrice, "cont exited count", exited))
      }
      if (exited > 5)
        break;
      strikePrice <- strikePrice + steps
    }
  }, error = function(error) {
    print(paste0("error:skipping",error))
    ?message
  })
}