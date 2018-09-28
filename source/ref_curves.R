


#Cero coupon reference curves - Argentina.
cec_inter <- read.table("input/cec_inter.csv", header = TRUE, sep=",")

n_cec <- length(cec_inter$Curva)
id_curva <- paste0(cec_inter$Curva,"_",cec_inter$Moneda)

for(i in c(1:n_cec)){
  id <- id_curva[i]
  bond_data <- read.csv(paste0(bb_dir,id,".csv"), header = TRUE, sep=",", stringsAsFactors = FALSE)# colClasses=c("character","numeric", "numeric", "character","numeric", "numeric"))
  matur <- as.Date(bond_data$MATURITY, "%d/%m/%Y")
  coupon <- bond_data$CPN/100
  freq <- bond_data$CPN_FREQ
  conv <- day_count$SURA[match(bond_data$DS180,day_count$BB)]
  ytm <- bond_data$YLD_CNV_LAST/100
  cpn_days <- rep(NA,length(coupon))
  mk_price=bond_risk_nom_port(curr_date, ytm, coupon, matur, freq, conv,
                              serie_float_rate = NULL, issue_date = NULL,
                              cpn_days = cpn_days)$full_price


  file_betas_prev <- paste0("output/betas/",id ,"/betas_",id, "_",format(prev_date, "%Y%m%d"),".csv")
  if(file.exists(file_betas_prev)){betas_ini=as.numeric(read.csv(file_betas_prev, header=F)[,-1])}else(betas_ini <- c(1,-1,1,1))

  delta_beta <- cec_inter$DeltaBetas[i]
  betas_last <- ns_optim_pr_reg(betas_ini, date_ini=curr_date, mk_price, coupon, matur, freq, conv, y_lim=c(0,5), delta_beta=0, min_beta=c( -10 , -30 , -30 ,2), max_beta=c(30 , 30, 10 ,12))

  prices_ns <- bond_price_ns_fixed(betas_last, date_ini=curr_date, coupon, matur, freq, conv)
  ytm_ns <- price_to_ytm(date_ini=curr_date, mk_price=prices_ns, coupon, matur, freq=freq, conv=conv, cpn_days = cpn_days, clean=0)

  windows()
  plot(matur,ytm*100, type="b", xlab ="Vencimiento", ylab =" Tasas en %", main=id, ylim = 100*c(min(ytm, ytm_ns),max(ytm, ytm_ns)), col= "darkblue")
  lines(matur ,ytm_ns*100, type="b", col="red", pch=2, lty = 2)
  legend("top", legend=c("Tasas de Mercado", "Tasas Estimadas"), col=c("darkblue", "red"), lty=1:2, cex=0.8, bty = "n")

  table_betas <- data.frame(FECHA = format(curr_date, "%d/%m/%Y"), t(betas_last))
  write.table(table_betas, paste0("output/betas/",id ,"/betas_",id, "_",format(curr_date, "%Y%m%d"),".csv"),col.names=F, row.names=F, quote=F, append=F, sep=',')
}

