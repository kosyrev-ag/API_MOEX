############################
#### Индексы ###############
############################

# Индекс МосБиржи
# https://iss.moex.com/iss/securities/IMOEX
# 
# Board : SNDX
# Engine: stock
# Market: index

url_share = paste0('http://iss.moex.com/iss/history/engines/',
                   'stock','/markets/index/boards/',
                   'SNDX','/securities/IMOEX.csv')
d<-data.frame()
for (b in 0:100*100) {
  u<-paste0(url_share,'?start=',b)
  dat<-read.csv(u,sep=';',skip=1)
  if(nrow(dat)==0) break
  d<-rbind(d,dat)
}
d$TRADEDATE<-as.Date(d$TRADEDATE)
plot(CLOSE~TRADEDATE,d,type='l')
plot(CAPITALIZATION/10^9~TRADEDATE,d[d$CAPITALIZATION!=0,],type='l')

# Индекс Мосбиржи гос обл RGBITR 
# Из общей инфо о бумаге по ссылке
# https://iss.moex.com/iss/securities/RGBITR
# 
# Board : SNDX + GNDX
# Engine: stock + state
# Market: index

boards<-c('SNDX','GNDX')
eng   <-c('stock', 'state')

d.complete<-data.frame()
for(i in 1:2){
  url_share = paste0('http://iss.moex.com/iss/history/engines/',
                     eng[i],'/markets/index/boards/',
                     boards[i],'/securities/RGBITR.csv')
  d<-data.frame()
  for (b in 0:500*100) {
    u<-paste0(url_share,'?start=',b)
    dat<-read.csv(u,sep=';',skip=1)
    if(nrow(dat)==0) break
    d<-rbind(d,dat)
  }
  if (i==1) {d.complete<-d} else {
    d.complete<-merge(d.complete,d,all=T)
  }
}
d.complete$TRADEDATE<-as.Date(d.complete$TRADEDATE)
d.complete<-d.complete[order(d.complete$TRADEDATE),]
plot(CLOSE~TRADEDATE,d.complete,type='l')
plot(YIELD~TRADEDATE,d.complete,type='l')


############################
#### Stocks ################
############################

# Из общей инфо о бумаге по ссылке
# https://iss.moex.com/iss/securities/gazp
# 
# Торги на четырех бордах с начала листинга:
# secid (string:51)	boardid (string:12)	title (string:381)	market (string:45)	engine (string:45)	history_from (date:10)	history_till (date:10)
# GAZP	TQBR	Т+: Акции и ДР - безадрес.	shares	stock	09.06.2014	11.10.2022
# GAZP	TQBS	Т+: А2-Акции и паи - безадрес.	shares	stock	26.12.2013	06.06.2014
# GAZP	TQNE	Т+: Акции, паи и ДР внесписочные - безадрес.	shares	stock	25.03.2013	25.12.2013
# GAZP	EQNE	Основной режим: Акции и паи внесписочные - безадрес.	shares	stock	23.01.2006	30.08.2013

# Везде одинаковый engine и market
# Меняется название борда

boards<-c('TQBR','TQBS','TQNE','EQNE')

d.complete<-data.frame()
for(i in boards){
  url_share = paste0('http://iss.moex.com/iss/history/engines/stock/markets/shares/boards/',
                      i,'/securities/GAZP.csv')
  d<-data.frame()
  for (b in 0:500*100) {
    u<-paste0(url_share,'?start=',b)
    dat<-read.csv(u,sep=';',skip=1)
    if(nrow(dat)==0) break
    d<-rbind(d,dat)
  }
  d.complete<-rbind(d.complete,d)
}
d.complete$TRADEDATE<-as.Date(d.complete$TRADEDATE)
d.complete<-d.complete[order(d.complete$VOLUME,decreasing = T),]
d.complete<-d.complete[!duplicated(d.complete$TRADEDATE),]
d.complete<-d.complete[order(d.complete$TRADEDATE),]

plot(CLOSE~TRADEDATE,d.complete,type='l')


# Дивиденды
u<-'https://iss.moex.com/iss/securities/GAZP/dividends.csv'
read.csv(u,sep=';',skip=1)

# в какие индексы входит бумага
u<-'https://iss.moex.com/iss/securities/GAZP/indices.csv'
read.csv(u,sep=';',skip=1)


############################
#### ETFs ##################
############################

# Фонд Топ Российских акций (ранее - Индекс МосБиржи полной доходности «брутто») - SBMX ETF
# Из общей инфо о бумаге по ссылке
# https://iss.moex.com/iss/securities/gazp
# 
# secid (string:51)	boardid (string:12)	title (string:381)	board_group_id (int32)	market_id (int32)	market (string:45)	engine_id (int32)	engine (string:45)	is_traded (int32)	decimals (int32)	history_from (date:10)	history_till (date:10)	listed_from (date:10)	listed_till (date:10)	is_primary (int32)	currencyid (string:9)
# SBMX	TQTF	Т+: ETF - безадрес.	57	1	shares	1	stock	1	3	2018-09-17	2022-10-11	2018-09-17	2022-10-12	1	RUB

url_share = paste0('http://iss.moex.com/iss/history/engines/stock/markets/shares/boards/',
                   'TQTF','/securities/SBMX.csv')
d<-data.frame()
for (b in 0:500*100) {
  u<-paste0(url_share,'?start=',b)
  dat<-read.csv(u,sep=';',skip=1)
  if(nrow(dat)==0) break
  d<-rbind(d,dat)
}

d$TRADEDATE<-as.Date(d$TRADEDATE)
plot(CLOSE~TRADEDATE,d,type='l')
plot(VOLUME~TRADEDATE,d,type='l')
plot(VALUE~TRADEDATE,d,type='l')

# сплит с 7 июня 2021 года 1:100
# https://www.moex.com/n34360
# все цены до сплита /100; объем *100
d[d$TRADEDATE<'2021-06-07',c("OPEN","LOW","HIGH","LEGALCLOSEPRICE","WAPRICE","CLOSE",                  
    "MARKETPRICE2","MARKETPRICE3","ADMITTEDQUOTE")]<-
  d[d$TRADEDATE<'2021-06-07',c("OPEN","LOW","HIGH","LEGALCLOSEPRICE","WAPRICE","CLOSE",                  
      "MARKETPRICE2","MARKETPRICE3","ADMITTEDQUOTE")]/100
d$VOLUME[d$TRADEDATE<'2021-06-07']<-d$VOLUME[d$TRADEDATE<'2021-06-07']*100
d<-d[!is.na(d$CLOSE),]
plot(CLOSE~TRADEDATE,d,type='l')

plot(VALUE~TRADEDATE,d,type='l')
w<-60   # MA(60)
lines(d$TRADEDATE[-(w-1):0],sapply(w:nrow(d),function(x){
  mean(d$VALUE[-(w-1):0+x])
}),col='red',lwd=4)


