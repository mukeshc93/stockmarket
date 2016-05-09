library(quantmod)

googlecur=function(symbol, freq, period) 
{ 
  base.url <- 'http://www.google.com/finance/getprices?' 
  options.url <- paste('q=',symbol,'&i=', freq, '&p=', period, 'Y',  sep = '') 
  full.url <- paste(base.url, options.url, sep = '') 
  
  
  data <- read.csv(full.url, skip = 7, header = FALSE, stringsAsFactors = FALSE) 
  
  
  starting.times.idx <- which(substring(data$V1, 1, 1) == 'a') 
  ending.seconds.idx <- c(starting.times.idx[-1] - 1, nrow(data)) 
  r.str.idx.use <- paste(starting.times.idx, ':', ending.seconds.idx, sep = '') 
  
  
  starting.times <- as.numeric(substring(data[starting.times.idx, 1], 2)) 
  
  
  data[starting.times.idx, 1] <- 0 
  clean.idx <- do.call(c, lapply(seq(1, length(r.str.idx.use)), function(i) { 
    starting.times[i] + freq * as.numeric(data[eval(parse(text = r.str.idx.use[i])), 1]) })) 
  data.xts <- xts(data[,-1], as.Date(as.POSIXct(clean.idx, origin = '1970-01-01', tz = 'Asia/Calcutta'))) 
  
  colnames(data.xts) <- c('USDINR','High', 'Low', 'Open', 'Volume') 
  
  
  data.xts 
}


googleintraday=function(symbol,exchange, freq, period) 
{ 
  base.url <- 'http://www.google.com/finance/getprices?' 
  options.url <- paste('q=',symbol,'&x=',exchange,'&i=', freq, '&p=', period, 'Y&f=d,o,h,l,c,v',  sep = '') 
  full.url <- paste(base.url, options.url, sep = '') 
  
  
  data <- read.csv(full.url, skip = 7, header = FALSE, stringsAsFactors = FALSE) 
  
  
  starting.times.idx <- which(substring(data$V1, 1, 1) == 'a') 
  ending.seconds.idx <- c(starting.times.idx[-1] - 1, nrow(data)) 
  r.str.idx.use <- paste(starting.times.idx, ':', ending.seconds.idx, sep = '') 
  
  
  starting.times <- as.numeric(substring(data[starting.times.idx, 1], 2)) 
  
  
  data[starting.times.idx, 1] <- 0 
  clean.idx <- do.call(c, lapply(seq(1, length(r.str.idx.use)), function(i) { 
    starting.times[i] + freq * as.numeric(data[eval(parse(text = r.str.idx.use[i])), 1]) })) 
  data.xts <- xts(data[,-1], as.Date(as.POSIXct(clean.idx, origin = '1970-01-01', tz = 'Asia/Calcutta')) )
  
  colnames(data.xts) <- c('Close', 'High', 'Low', 'Open', 'Volume') 
  
  
  data.xts 
}

