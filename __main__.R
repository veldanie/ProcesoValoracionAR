
## Working directory:


setwd("D:/OneDrive - Inversiones Internacionales Grupo Sura S.A/Argentina/valoración/");options(warn=-1, scipen=100)
rm(list=lsf.str());rm(list=ls(all=TRUE))

## Bloomber directory:
bb_dir <- "X:/SIM/SOLUCIONES/ARGENTINA/input/"

curr_date <- as.Date("31082018", "%d%m%Y")
# curr_date <- Sys.Date() # Fecha actual

#1. PARÁMETROS INICIALES (Librerías,  parámetros):-------------------------------------------------------------------------------------------------------------------
source("source/params_inter.R", echo=FALSE)
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------

#2.GENERACIÓN CURVAS CERO CUPÓN DE REFERENCIA :----------------------------------------------------------------------------------------------------------------------
source("source/ref_curves.R", echo=FALSE)
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------

#3.PUBLICACION RENTA FIJA INTERNACIONAL :----------------------------------------------------------------------------------------------------------------------------
update_curves <- TRUE # Si FALSE, las curvas de valoración son las generadas el día anterior.
source("source/valuation.R", echo=FALSE)
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------


