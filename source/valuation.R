# Tabla de salida:
isins_out <- data.frame(port_cust_val, isins %>% slice(pos_isins)) %>%
             mutate(CLEAN_PR = 0) %>%
             mutate(CPN_RATE = 0) %>% mutate(CUM_CPN = 0) %>%
             mutate(FULL_PR = 0)  %>% mutate(YTM = 0) %>% mutate(VAR = NA) %>%
             mutate(PRICE_MO = 0) %>% mutate(PRICE_ARS = 0) %>%
             mutate(MARKET_VAL_MO = 0) %>% mutate(MARKET_VAL_ARS = 0) %>% mutate(MARKET_VAL_USD = 0) %>% mutate(MD = 0) %>%
             select(ISSUER, ID_SURA, NEMOTECNICO, CARTERA, CPN, MATURITY, CURRENCY, INDUSTRY_GROUP, NOMINAL, PRICE_MO,
                    PRICE_ARS, MARKET_VAL_MO, MARKET_VAL_ARS, MARKET_VAL_USD, YTM, MD)

missing_tpcg <- c()
isins_expired <- c()
isins_dup_tpcg <- c()
for (j in 1:length(pos_isins)){ # Sólo se procesan bonos que estén en el archivo del custodio.

  i <- pos_isins[j]
  isin <- isins$ISIN[i]
  cod <- isins$COD[i]
  id <- isins$ID_SURA[i]
  type <- as.character(isins$TICKER[i])
  cpn_days <- NA
  amort <- !isins$BULLET[i]
  amort_mat <- NULL
  if(amort){
    amort_mat <- as.data.frame(amort_tab %>% filter(ISIN==isin & DATE > curr_date) %>% ungroup() %>% select(DATE, PERC))
    if(dim(amort_mat)[1]==0){
      print(paste('Actualizar tabla de amortización de isin:', isin))
      amort_mat <- NULL
    }
  }

  mid_last <- as.numeric((prices_last %>% filter(ID_SURA==id) %>% select(PRICE_MO) %>% unlist())[1])# Se asume precio limpio
  if(length(mid_last)==0 | is.na(mid_last)){mid_last <- 1}
  ##Infomación facial.

  coupon <- as.numeric(isins$CPN[i])/100
  coupon_pub <- paste0(round(coupon * 100, 3), "%")
  spread <- as.numeric(isins$FLT_SPREAD[i])/10000
  serie_float_rate <- NULL;

  if(!is.na(spread)){coupon <- spread
                     ticket_rate <- isins$RESET_IDX[i]
                     serie_float_rate <- series_index[[ticket_rate]]/100
                     coupon_pub <- paste0(substr(ticket_rate,1,1), "+", spread*100, "%")
  }
  matur <- isins$MATURITY[i]
  issue <- isins$ISSUE_DT[i]
  freq <- as.numeric(isins$CPN_FREQ[i])
  if(matur<=curr_date){isins_expired = c(isins_expired, cod); next}

  conv <- day_count %>% filter(BB==isins$DS180[i]) %>% select(SURA) %>% unlist()

  ##Precio Nivel 1 y Nivel 2.
  if(!is.null(prices_bro)){
    mid <- 100*(prices_bro %>% filter(ISIN==isin) %>% select(Price.ARS) %>% unlist())
    if(length(mid)>1){
      mid <-mid[1]
      isins_dup_tpcg <- c(isins_dup_tpcg, isin)
    }
  }else{mid <- c()}

  ##Inicio valoración teórica (si NO hay precio del broker).
  if(length(mid)==0){
    missing_tpcg <- c(missing_tpcg, cod)
    if(substr(isin,1,3) == "PF$"){ # Si es PF se causan intereses.
      mid <- round((1 + bond_risk_nom(date_ini = curr_date, ytm = coupon, coupon = coupon, matur=matur,
                               freq=freq, conv=conv, serie_float_rate=serie_float_rate, issue_date=issue, cpn_days=cpn_days, amort_mat = amort_mat,
                               in_arrears=0)$cum_coupon)*100, 5)
    }else{
      #Cálculo de spread a partir de precio y curva/tasa_var del día anterior
      if(is.na(spread)){

        cc_curve0 <- cc_curve1 <- NULL
        ##Curva de Referencia:
        cc_ref <- as.character(isins$REF_CURVE[i])
        ##Curva en Betas:
        betas_ns0 <- betas_ns1 <- as.numeric(read.table(paste0("output/betas/",cc_ref,"/betas_",cc_ref,"_",format(prev_date,"%Y%m%d"),".csv"), header=FALSE, sep = ',')[,-1])
        if(update_curves){
          betas_ns1 <- as.numeric(read.table(paste0("output/betas/",cc_ref,"/betas_",cc_ref,"_",format(curr_date,"%Y%m%d"),".csv"), header=FALSE, sep = ',')[,-1])
        }
        spr <- SuraFixedIncome::price_to_cc_spread(date_ini=prev_date, mk_price=mid_last, cc_curve=cc_curve0, betas_ns=betas_ns0, coupon=coupon, matur=matur, freq=freq,
                                                  conv=conv, serie_float_rate=serie_float_rate, issue_date=issue, cpn_days=cpn_days, amort_mat = amort_mat, round_val=5, y_lim = c(-20, 20), clean=FALSE)
        mid <- round(bond_price_cc(date_ini=curr_date, cc_curve=cc_curve1, betas_ns=betas_ns1, cc_spread=spr, coupon=coupon, matur=matur,
                                   freq=freq, conv=conv, serie_float_rate=serie_float_rate, issue_date=issue, cpn_days=cpn_days, amort_mat = amort_mat, in_arrears=0)$full_price *100,3)

      }else{
        spr <- try(price_to_disc_mar(date_ini=prev_date, mk_price=mid_last, coupon, matur, freq, conv, serie_float_rate, issue_date=issue, amort_mat = amort_mat, in_arrears=0,clean=FALSE), silent = TRUE)
        if(class(spr) == 'try-error'){spr <- 0}

        mid <- round(bond_price_float(date_ini=curr_date, disc_mar=spr, coupon=coupon, matur=matur, freq=freq, conv=conv,
                                      serie_float_rate=serie_float_rate, issue_date=issue, amort_mat = amort_mat, in_arrears=0)$full_price*100,3)
      }
    }
  }

  ytm <- ifelse(type=="B", mid/100,
                price_to_ytm(date_ini=curr_date, mk_price=mid/100, coupon=coupon, matur=matur,
                             freq=freq, conv=conv, serie_float_rate=serie_float_rate, issue_date=issue, cpn_days=cpn_days, amort_mat = amort_mat, in_arrears=0, clean = FALSE))

  bond_val <- bond_risk_nom(date_ini=curr_date, ytm, coupon, matur, freq, conv, serie_float_rate=serie_float_rate, issue_date=issue, cpn_days=cpn_days, amort_mat = amort_mat, in_arrears=0, round_val=5)

  mid <- ifelse(type=="B",bond_val$full_price*100,mid)

  nominal <- as.numeric(port_cust_val[j,c('NOMINAL')])
  isins_out$CPN[j] <- coupon_pub

  isins_out$PRICE_MO[j] <- round(mid / 100, 3) # No se publica en porcentaje
  isins_out$MARKET_VAL_MO[j] <- round(mid,3) * nominal / 100
  isins_out$YTM[j]=round(ytm*100,3)
  isins_out$MD[j]=round(-bond_val$mod_dur,3)

  iso <- iso_quote(isins_out$CURRENCY[j])
  fx_id <- ifelse(substr(iso,1,3)=="USD", substr(iso,4,6), substr(iso,1,3))

  isins_out$MARKET_VAL_USD[j] <- cash_conv(isins_out$MARKET_VAL_MO[j], isins_out$CURRENCY[j], fx_spot[fx_id], iso)
  isins_out$MARKET_VAL_ARS[j] <- ifelse(isins_out$CURRENCY[j] == 'ARS', isins_out$MARKET_VAL_MO[j],
                                        cash_conv(isins_out$MARKET_VAL_MO[j], isins_out$CURRENCY[j], fx_spot["ARS"], paste0(isins_out$CURRENCY[j],'ARS')))
  isins_out$PRICE_ARS[j] <- isins_out$MARKET_VAL_ARS[j]/cash_conv(nominal, isins_out$CURRENCY[j], fx_spot["ARS"], paste0(isins_out$CURRENCY[j],'ARS'))

  #isins_out$CPN_RATE[j]=round(bond_val$coupon1*100,3)
  #isins_out$CUM_CPN[j]=round(bond_val$cum_coupon*100,3)
  #isins_out$FULL_PR[j]=round(mid+isins_out$CUM_CPN[j],3)
  #isins_out$"CONVEX"[j]=round(bond_val$convex,3)
  #isins_out$"DAYS_TO_MAT"[j]=bond_val$dtm
  #mac_dur[j]=round(-bond_val$mac_dur,3)

  #REPORTES
  #Cuadros de Marcha:
  #Para generar estas tablas es necesario conocer las operaciones sobre el bono y la fehca de compra:
  #purchase_date <- isins_out$PURCHASE_DATE[j]
  #price_series <- series_pr[[isin]]
  #bond_rep <- bond_report(purchase_date, nominal, coupon, matur, freq, conv, isin, serie_float_rate, amort_mat, diff_fix_value = 0)
  #bond_summ <- bond_summary(curr_date, purchase_date, price_series, bond_rep$cf_tab_mon, id)
  #write.table(bond_rep$cf_tab, paste0("output/flujos/flujos_", format(curr_date, "%Y%m%d"), ".csv"), col.names = TRUE, row.names = FALSE, quote = FALSE, sep = ",", append = TRUE)

  #write.table(bond_rep$cf_tab_mon, paste0("output/cuadros_de_marcha/cuadros_marcha_", format(curr_date, "%Y%m%d"), ".csv"), col.names = TRUE, row.names = FALSE, quote = FALSE, sep = ",", append = TRUE)

  #write.table(bond_summ$report_tab, paste0("output/asientos/ASTO SSN ON Y TP Ej SUAM_", format(curr_date, "%Y%m%d"), ".csv"), col.names = TRUE, row.names = FALSE, quote = FALSE, sep = ",", append = TRUE)
}

print(paste('Isines duplicados en archivo TPCG:', paste(isins_dup_tpcg, collapse = ', ')))
print(paste('Isines faltantes en archivo TPCG:', paste(missing_tpcg, collapse = ', ')))

## Fondos
pos_pub <- which(funds$PUBLI!="")
funds_bb <- funds %>% slice(pos_pub) %>% mutate(INDUSTRY_GROUP = INDUSTRY_SUBGROUP)  %>% filter(!is.na(NOMINAL)) %>%
            select(ISSUER, ID_SURA, CURRENCY, INDUSTRY_GROUP, NOMINAL, PRICE_MO)
col_names <- colnames(isins_out)
n_funds <- nrow(funds_bb)

funds_out <- as.data.frame(matrix("", nrow = nrow(funds_bb), ncol = ncol(isins_out)))
colnames(funds_out) <- col_names

funds_out[match(colnames(funds_bb),col_names)] <- funds_bb

funds_out$MARKET_VAL_MO <- funds_out$PRICE_MO * funds_out$NOMINAL

iso <- iso_quote(funds_out$CURRENCY)
fx_id <- sapply(iso, function(x) ifelse(substr(x,1,3)=="USD", substr(x,4,6), substr(x,1,3)))
funds_fx_spot <- fx_spot[fx_id]
funds_fx_spot[is.na(funds_fx_spot)] <- 1

funds_out$MARKET_VAL_USD <- mapply(cash_conv, cash_in = funds_out$MARKET_VAL_MO, curr_in = funds_out$CURRENCY, spot = funds_fx_spot, spot_id = iso)

pos_ars <- which(funds_out$CURRENCY == 'ARS')
pos_usd <- which(funds_out$CURRENCY == 'USD')

funds_out$MARKET_VAL_ARS <- 0
funds_out$MARKET_VAL_ARS[pos_ars] <- funds_out$MARKET_VAL_MO[pos_ars]
funds_out$MARKET_VAL_ARS[pos_usd] <- mapply(cash_conv, cash_in = funds_out$MARKET_VAL_MO[pos_usd], MoreArgs = list(curr_in = 'USD', spot = fx_spot['ARS'], spot_id = 'USDARS'))

isins_out$MATURITY <- format(isins_out$MATURITY)

carteras <- unique(isins_out$CARTERA)
for (car in carteras){
  funds_out$NEMOTECNICO <- funds_out$ID_SURA
  funds_out$CARTERA <- car
  isins_out <- rbind(isins_out, funds_out)
}

#Publicación general:
write.table(isins_out, paste0("output/cartera/cartera_", format(curr_date, "%Y%m%d"), ".csv"), col.names = TRUE, row.names = FALSE, quote = FALSE, sep = ",")
