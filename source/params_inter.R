
#Packages:
req_packages <- c("devtools", "dplyr", "lubridate", "xts", "XLConnect")
for(pack in req_packages){if(!require(pack, character.only = TRUE)) install.packages(pack, character.only = TRUE); require(pack, character.only = TRUE)}
if(!require("SuraFixedIncome")){install_github("veldanie/SuraFixedIncome"); require(SuraFixedIncome)}

#Settlemenent Days (Argentina) y fecha previa:
settAR <- as.Date(read.table("input/sett_ar.csv", header = FALSE, colClasses = 'character')[,1], '%d/%m/%Y')
pos_prev_date <- ifelse(length(which(curr_date==settAR)-1)==0,findInterval(curr_date,settAR),which(curr_date==settAR)-1)
prev_date <- settAR[pos_prev_date]

##Parameters:
conv_default <- "30_360" #Default convention
max_var_pr <- 5 # Maximum price variation

#Tables:
isins <- read.table(paste0(bb_dir, "isines.csv"), header=TRUE, sep=",", fill=TRUE, comment.char = "", colClasses="character")
isins$MATURITY <- dmy(isins$MATURITY)
isins$ISSUE_DT <- dmy(isins$ISSUE_DT)
isins$BULLET <- ifelse(isins$BULLET == "Y", TRUE, FALSE)

funds <- read.table(paste0(bb_dir, "fondos.csv"), header=TRUE, sep=",", fill=TRUE, colClasses="character")
pr_funds <- read.table(paste0(bb_dir, "pr_fondos.csv"), header=TRUE, sep=",", fill=TRUE, colClasses=c("character", "numeric"))


#Data Broker (TPCG)
wb = try(loadWorkbook(paste0('tpcg/', format(curr_date, '%Y%m%d'), ' Valuacion SURA.xlsm')), silent = TRUE)

if(class(wb) != 'try-error'){
  df = readWorksheet(wb, sheet = "Valuacion", startRow = 2, header = TRUE)
  prices_bro <- df %>% select(c('ISIN', 'Price.ARS', 'Currency', 'Market.Value.USD', 'Market.Value.ARS')) %>% filter(!is.na(ISIN) & !is.na(Price.ARS)) %>% mutate(DATE = curr_date)
  pos_usd <-which(prices_bro$Currency=='USD')
  if(length(pos_usd) > 0){
    prices_bro[pos_usd, 'Price.ARS'] <- prices_bro[pos_usd, 'Price.ARS'] / (prices_bro[pos_usd, 'Market.Value.ARS']/prices_bro[pos_usd, 'Market.Value.USD'])
  }
  prices_bro <- prices_bro %>% select(c('ISIN', 'Price.ARS'))
  funds$NOMINAL <-  df$Shares[match(funds$ID_SURA, df$Issuer)]
  funds$PRICE_MO <- df$Price.ARS[match(funds$ID_SURA, df$Issuer)]
}else{
  prices_bro <- NULL
  nominal_vec <- NULL
  print('No existe archivo TPCG para la fecha.')
}

##Archivos custodio.
port_cust <- NULL
port_val <- NULL

files_cust_val <- list.files(path = "./custodio/", pattern = paste0('^', format(curr_date, '%d%m%y'), ' POS VAL'))
for (fi in files_cust_val){
  wb <- loadWorkbook(paste0('custodio/',fi))
  df <- readWorksheet(wb, sheet = 1, startRow = 7, header = TRUE)
  df_val <- df[,c(7, 6, 8, 4, 9)]
  colnames(df_val) <- c('NEMOTECNICO', 'ISIN', 'DESCRIP', 'CARTERA', 'NOMINAL')
  df_val$DESCRIP <- gsub(',', '.', df_val$DESCRIP)
  port_val <- rbind(port_val, df_val)
  df_tab <- df[,c(7, 4, 9)]
  colnames(df_tab) <- c('NEMOTECNICO', 'CARTERA', 'NOMINAL')
  port_cust <- rbind(port_cust, df_tab)
}

issuer_tab <- read.table("input/cust_emisor_id.csv", header=TRUE, sep=",", fill=TRUE, colClasses="character")
fx_tab <- read.table("input/cust_moneda_id.csv", header=TRUE, sep=",", fill=TRUE, colClasses="character")
port_pf <- NULL

files_cust_pf <- list.files(path = "./custodio/", pattern = paste0('^', format(curr_date, '%d%m%y'), ' POS PF'))
for (fi in files_cust_pf){
  wb = loadWorkbook(paste0('custodio/',fi))
  df = readWorksheet(wb, sheet = 1, startRow = 8, header = TRUE)#[,c(6, 7, 4, 9)]
  cart <- as.numeric(readWorksheet(wb, sheet = 1, startRow = 7, endRow = 7, startCol = 3, endCol = 3, header = FALSE))
  issuer_id <- issuer_tab[match(trimws(df[,3], 'both'), trimws(issuer_tab[,1], 'both')),2]
  fx_id <- fx_tab[match(trimws(df[,7], 'both'), fx_tab[,1]),2]
  nemo <- paste0('PF', fx_id, issuer_id, '-',format(ymd(df[,5]), "%d%m%y"))

  df_pf <- data.frame(NEMOTECNICO = nemo, EMISOR = df[,3], CARTERA = cart, NOMINAL = df[,9], VTO = df[,5], EMISION = df[,4], MONEDA = fx_id, TNA = df[,10], COD = df[,1])
  port_pf <- rbind(port_pf, df_pf)
  df <- data.frame(NEMOTECNICO = nemo, CARTERA = cart, NOMINAL = df[,9])
  port_cust <- rbind(port_cust, df)
}


# Series:
series_float_rates <- read.table(paste0(bb_dir, "series.csv"), header=TRUE, sep=",")
series_float_rates$Date <- dmy(series_float_rates$Date)
#series_float_rates <- series_float_rates[series_float_rates$Date<=curr_date,]

#Isins:
id_index <- unique(isins$RESET_IDX)
id_index <- id_index[!is.na(id_index)]

# Series Index
series_index <- as.list(rep(NA,length(id_index)))
names(series_index) <- id_index

for (id in id_index){
  series_id <- series_float_rates %>% filter(Index == id) %>% select(Date, Value)
  if(dim(series_id)[1] != 0){
    not_na <- !is.na(as.numeric(series_id[, 2]))
    series_index[[id]] <- xts(as.numeric(series_id[, 2])[not_na], series_id[,1][not_na])
  }
}

#Currencies:

fx <- isins %>% filter(!is.na(CURRENCY) & CURRENCY != 'USD' & CURRENCY != '') %>% select(CURRENCY) %>% unique() %>% unlist()
fx_spot <- rep(NA,length(fx))
names(fx_spot) <- fx
for (x in fx){
  fx_spot[x] <- series_float_rates %>% filter(Index == x & Date <= curr_date) %>% tail(1) %>% select(Value) %>% unlist()
}

#Series prices (Actualmente no se toman precios de bloomberg como referencia!).
pos_isins_raw <- match(port_cust$NEMOTECNICO, isins$COD)
pos_isins_na <- which(is.na(isins$ISSUER))
pos_isins <- pos_isins_raw[!is.na(pos_isins_raw) & !(pos_isins_raw %in% pos_isins_na)]

isins_pub <-  isins$ISIN[pos_isins]
# series_pr <- as.list(rep(NA,length(isins_pub)))
# names(series_pr) <- isins_pub
#
# series_prices <- read.table(paste0(bb_dir, "series_prices.csv"), header=TRUE, sep=",")
#
# for (id in isins_pub){
#  pos_id <- match(id, colnames(series_prices))
#  if(!is.na(pos_id)){
#    not_na <- !is.na(as.numeric(series_prices[, pos_id]))
#    series_pr[[id]] <- xts(as.numeric(series_prices[, pos_id])[not_na], dmy(series_prices[,pos_id-1])[not_na])
#  }
# }
##Tablas de amortización:

cf <- read.csv(paste0(bb_dir, "cf.csv"), header=TRUE, colClasses=c("character", "character", "numeric", "numeric"))
cf$DATES <- dmy(cf$DATES)
amort_tab <- cf %>% filter(!is.na(DATES) | !is.na(PRINCIPAL)) %>% mutate(DATE = as.Date(DATES, "%d/%m/%Y")) %>% group_by(ISIN) %>% mutate(PERC = PRINCIPAL/sum(PRINCIPAL)) %>% select(c(ISIN, DATE, PERC))
isins_amort <- unique(amort_tab$ISIN)
for (isin_i in isins_amort){
  mat_date <- isins$MATURITY[isins$ISIN == isin_i][1]
  amort_dates <- amort_tab$DATE[amort_tab$ISIN == isin_i]
  amort_dates_adj <- dmy(paste0(format(mat_date, "%d"), format(amort_dates, "%m%Y")))
  amort_tab$DATE[amort_tab$ISIN == isin_i] <- amort_dates_adj
}

##1.3. TABLA DE EQUIVALENCIAS IN/OUT:
day_count <- read.table("input/days_count.csv", header = TRUE, sep=",", colClasses="character")

# Archivos iniciales

port_val$REG <- TRUE # Este campo establece si el isin está registrado en la tabla de isines
port_val$REG[!(port_val$ISIN %in% isins$ISIN)] <- FALSE

port_val$VAL_IND <- TRUE # Este campo establece si el isin es valorado.
port_val$VAL_IND[is.na(match(port_val$NEMOTECNICO, isins$COD[pos_isins]))] <- FALSE


port_pf$VAL_IND <- TRUE
port_pf$VAL_IND[is.na(match(port_pf$NEMOTECNICO, isins$COD[pos_isins]))] <- FALSE

write.table(port_pf, "output/val_pf/port_pf.csv", col.names = TRUE, row.names = FALSE, quote = FALSE, sep=",")
write.table(port_val, "output/val_pf/port_val.csv", col.names = TRUE, row.names = FALSE, quote = FALSE, sep="|")

port_cust_val <- port_cust[!is.na(pos_isins_raw) & !(pos_isins_raw %in% pos_isins_na),]

prices_last <- read.table(paste0('output/cartera/cartera_', format(prev_date, '%Y%m%d'), '.csv'), header = TRUE, sep=",", colClasses="character")

print("Revisar archivos 'val_pf/port_pf.csv' y 'val_pf/port_val.csv' y completar archivos bloomberg.")
