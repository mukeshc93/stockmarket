datacollection=function(retdays=1,requiretestdata=FALSE)
{
symbols=read.csv("~/R/My Projects/stock market/Symbols.csv", sep="")
s=NULL
for(i in 1:50)
{
  t=googleintraday(symbol=as.character(symbols[i,]),exchange="NSE",freq=86400,period=9)
  t=(t[,c(1,5)])

  colnames(t)=c(paste(symbols[i,],"price"),paste(symbols[i,],"volume"))

  s=merge(t,s,all=T)
}
View(as.data.frame(s))
#s=s[c(-1:-300),]
r=s


################################################################
s=r
USDINR=googlecur(symbol="USDINR",freq=86400,period=10)
USDINR=USDINR[,1]
View(as.data.frame(USDINR))
s=cbind.xts(USDINR,s,join="inner")

#s$USDINR[nrow(s)]=s$USDINR[nrow(s)-1]
n=seq(2,101,2)
stock=(s[,n])
volume=s[,c(-n,-1)]


sdate=row.names(as.data.frame(s))[1:(nrow(s)-120)]
edate=row.names(as.data.frame(s))[121:nrow(s)]
x=cbind(sdate,edate)
ldates=x[(nrow(x)-120):nrow(x),]
set.seed(1)
seqdates=(x[round(runif(500,min=1,max=(nrow(x)-120))),])
dates=rbind(seqdates,ldates)
sdate=dates[,1]
edate=dates[,2]


newret=NULL

if(requiretestdata==TRUE)
{
  l=621
  tstock=as.matrix(window(stock,start = as.Date(sdate[l]),end=as.Date(edate[l])))
  ret=matrix(NA,nrow=nrow(tstock),ncol=ncol(tstock));for(i in 1:ncol(tstock)){ret[,i]=Delt(tstock[,i],k=retdays)};ret=t(ret[-1,])
  tstock=t(tstock)
  
  tvol=as.matrix(window(volume,start = as.Date(sdate[l]),end=as.Date(edate[l])))
  tvol=t(tvol)
  
  USDINR=as.matrix(window(s[,1],start = as.Date(sdate[l]),end=as.Date(edate[l])))
  USDINR=t(USDINR)
  
  
  ret=cbind(USDINR[1,ncol(USDINR)],tstock[,ncol(tstock)],tvol[,ncol(tvol)],ret)
  colnames(ret)=c("USDINR","Previous day price", "previous day vol", colnames(tstock[,-1]))
  
  bseratio <- read.table("file:///C:/Users/mukes_000/Documents/R/My Projects/stock market/BSE PE PB.csv",sep=",",header = T)
  
  mat=NULL
  for(i in 1:nrow(bseratio))
  {
    mat[i]=grepl(as.character(bseratio[i,1]),as.character(colnames(ret[,(ncol(ret)-1):ncol(ret)]))[2])
  }
  random=as.matrix(mat*bseratio)
  random=random[which(random!=0)]
  random=matrix(rep(random,times=nrow(ret)),nrow=nrow(ret),ncol=7,byrow=T)
  colnames(random)=colnames(bseratio)
  ret=cbind(random,ret)
  
  sdev=NULL
  for(i in 1:nrow(ret))
  {
    sdev[i]=sd(ret[i,10:ncol(ret)],na.rm = T)
  }
  ret=cbind(sdev,ret)
  
  inf <- read.csv("file:///C:/Users/mukes_000/Documents/R/My Projects/stock market/inffiidii.csv", header=T)
  
  mat=NULL
  for(i in 1:nrow(inf))
  {
    mat[i]=grepl(as.character(inf[i,1]),as.character(colnames(ret[,(ncol(ret)-1):ncol(ret)]))[2])
  }
  random=as.matrix(mat*inf[,-1])
  random=random[which(random!=0)]
  random=matrix(rep(random,times=nrow(ret)),nrow=nrow(ret),ncol=7,byrow=T)
  colnames(random)=colnames(inf[,-1])
  ret=cbind(random,ret)
  
  
  FiiDii=inf[,c(1,5,8)]
  mat=NULL
  newmat=NULL
  for(i in 1:nrow(FiiDii)) 
  {
    mat[i]=(grepl(as.character(FiiDii[i,1]),as.character(colnames(ret[,(ncol(ret)-1):ncol(ret)]))[2]))
    newmat[i]=(grepl(as.character(FiiDii[i,1]),as.character(colnames(ret[,19:20]))[1]))
  }
  x=as.data.frame(cbind(mat,newmat))
  n=as.numeric(row.names(x[which(x$mat==TRUE),]))
  sums=colSums(FiiDii[n:(n+5),2:3])
  sums=matrix(rep((sums),50),nrow=50,ncol=2,byrow=T)
  colnames(sums)=c("FII for period","DII for period")
  nret=cbind(sums,ret)
  colnames(nret) = c(colnames(nret[, 1:20]), as.character(paste("return", 1:120, sep = ""))) ###################
  newret=rbind(nret,newret)
  
}

else
{
  for(l in 1:620){
    #l=621
    print(l)
    tstock=as.matrix(window(stock,start = as.Date(sdate[l]),end=as.Date(edate[l])))
    ret=matrix(NA,nrow=nrow(tstock),ncol=ncol(tstock));for(i in 1:ncol(tstock)){ret[,i]=Delt(tstock[,i],k=1)};ret=t(ret[-1,])
    tstock=t(tstock)
    
    tvol=as.matrix(window(volume,start = as.Date(sdate[l]),end=as.Date(edate[l])))
    tvol=t(tvol)
    
    USDINR=as.matrix(window(s[,1],start = as.Date(sdate[l]),end=as.Date(edate[l])))
    USDINR=t(USDINR)
    
    
    ret=cbind(USDINR[1,ncol(USDINR)],tstock[,ncol(tstock)],tvol[,ncol(tvol)],ret)
    colnames(ret)=c("USDINR","Previous day price", "previous day vol", colnames(tstock[,-1]))
    
    bseratio <- read.table("file:///C:/Users/mukes_000/Documents/R/My Projects/stock market/BSE PE PB.csv", sep = ",",header = T)    
    
    mat=NULL
    for(i in 1:nrow(bseratio))
    {
      mat[i]=grepl(as.character(bseratio[i,1]),as.character(colnames(ret[,(ncol(ret)-1):ncol(ret)]))[2])
    }
    random=as.matrix(mat*bseratio)
    random=random[which(random!=0)]
    random=matrix(rep(random,times=nrow(ret)),nrow=nrow(ret),ncol=7,byrow=T)
    colnames(random)=colnames(bseratio)
    ret=cbind(random,ret)
    
    sdev=NULL
    for(i in 1:nrow(ret))
    {
      sdev[i]=sd(ret[i,10:ncol(ret)],na.rm = T)
    }
    ret=cbind(sdev,ret)
    
    inf <- read.csv("file:///C:/Users/mukes_000/Documents/R/My Projects/stock market/inffiidii.csv", header=T)

    mat=NULL
    for(i in 1:nrow(inf))
    {
      mat[i]=grepl(as.character(inf[i,1]),as.character(colnames(ret[,(ncol(ret)-1):ncol(ret)]))[2])
    }
    random=as.matrix(mat*inf[,-1])
    random=random[which(random!=0)]
    random=matrix(rep(random,times=nrow(ret)),nrow=nrow(ret),ncol=7,byrow=T)
    colnames(random)=colnames(inf[,-1])
    ret=cbind(random,ret)
    
    
    FiiDii=inf[,c(1,5,8)]
    mat=NULL
    newmat=NULL
    for(i in 1:nrow(FiiDii)) 
    {
      mat[i]=(grepl(as.character(FiiDii[i,1]),as.character(colnames(ret[,(ncol(ret)-1):ncol(ret)]))[2]))
      newmat[i]=(grepl(as.character(FiiDii[i,1]),as.character(colnames(ret[,19:20]))[1]))
    }
    x=as.data.frame(cbind(mat,newmat))
    n=as.numeric(row.names(x[which(x$mat==TRUE),]))
    sums=colSums(FiiDii[n:(n+5),2:3])
    sums=matrix(rep((sums),50),nrow=50,ncol=2,byrow=T)
    colnames(sums)=c("FII for period","DII for period")
    nret=cbind(sums,ret)
    #nret=nret[,c(1:20,(ncol(nret)-20):ncol(nret))]
    colnames(nret) = c(colnames(nret[, 1:20]), as.character(paste("return", 1:120, sep = "")))
    newret=rbind(nret,newret)
    
  }
}
print(max(edate))
return(newret[,! apply( newret , 2 , function(x) all(is.na(x)) ) ])
}

#########################################################

#write.csv(newret,"f.csv",row.names = F)




