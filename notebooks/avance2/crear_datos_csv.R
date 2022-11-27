# Librerías
library(ncdf4)

# Cargar predictores
apcp <- nc_open("../../data/train/apcp_sfc_latlon_subset_19940101_20071231.nc") 
dlwrf <- nc_open("../../data/train/dlwrf_sfc_latlon_subset_19940101_20071231.nc")
dswrf <- nc_open("../../data/train/dswrf_sfc_latlon_subset_19940101_20071231.nc") 
pres_msl <- nc_open("../../data/train/pres_msl_latlon_subset_19940101_20071231.nc") 
pwat <- nc_open("../../data/train/pwat_eatm_latlon_subset_19940101_20071231.nc") 
spfh <- nc_open("../../data/train/spfh_2m_latlon_subset_19940101_20071231.nc")
tcdc <- nc_open("../../data/train/tcdc_eatm_latlon_subset_19940101_20071231.nc")
tcolc <- nc_open("../../data/train/tcolc_eatm_latlon_subset_19940101_20071231.nc")
tmax <- nc_open("../../data/train/tmax_2m_latlon_subset_19940101_20071231.nc")
tmin <- nc_open("../../data/train/tmin_2m_latlon_subset_19940101_20071231.nc")
tmp.2m <- nc_open("../../data/train/tmp_2m_latlon_subset_19940101_20071231.nc")
tmp.sfc <-nc_open("../../data/train/tmp_sfc_latlon_subset_19940101_20071231.nc")
ulwrf.sfc <- nc_open("../../data/train/ulwrf_sfc_latlon_subset_19940101_20071231.nc")
ulwrf.atm <- nc_open("../../data/train/ulwrf_tatm_latlon_subset_19940101_20071231.nc")
uswrf <- nc_open("../../data/train/uswrf_sfc_latlon_subset_19940101_20071231.nc")

# Cargar datos de las estaciones
stations <- read.csv("../../data/station_info.csv")

# Cargar respuesta esperada en el entrenamiento
energy_train <- read.csv("../../data/train.csv")

# Funciones auxiliares
# Get dates with needed format
time <- ncvar_get(dlwrf, "time")
dates <- as.Date(as.POSIXct(time*3600, origin='1800-01-01 00:00:00', 'UTC'))
dates <- gsub('-', '', dates)
dates <- strtoi(dates)

namedSeq <- function(name, stop, steps=1, start=1) {
  return(paste(name, seq(from=start, to=stop, by=steps), sep=""))
}

nameCols <- function(data) {
  dimnames(data)[[5]] <- dates
  ## Latitud
  dimnames(data)[[2]] <- namedSeq("", 39, start=31)
  ## Longitud
  dimnames(data)[[1]] <- namedSeq("", 269-360, start=254-360)
  dimnames(data)[[4]] <- namedSeq("ens_", 11)
  dimnames(data)[[3]] <- namedSeq("fhour_", 24, steps=3, start=12)
  
  return (data)
}

# Obtener las variables de interés
print(apcp)
apcp.data <- ncvar_get(apcp, "Total_precipitation")
dlwrf.data <- ncvar_get(dlwrf, "Downward_Long-Wave_Rad_Flux")
dswrf.data <- ncvar_get(dswrf, "Downward_Short-Wave_Rad_Flux")
pres_msl.data <- ncvar_get(pres_msl, "Pressure")
pwat.data <- ncvar_get(pwat, "Precipitable_water")
spfh.data <- ncvar_get(spfh, "Specific_humidity_height_above_ground")
tcdc.data <- ncvar_get(tcdc, "Total_cloud_cover")
tcolc.data <- ncvar_get(tcolc, "Total_Column-Integrated_Condensate")
tmax.data <- ncvar_get(tmax, "Maximum_temperature")
tmin.data <- ncvar_get(tmin, "Minimum_temperature")
tmp2m.data <- ncvar_get(tmp.2m, "Temperature_height_above_ground")
tmpsfc.data <- ncvar_get(tmp.sfc, "Temperature_surface")
ulwrfsfc.data <- ncvar_get(ulwrf.sfc, "Upward_Long-Wave_Rad_Flux_surface")
ulwrfatm.data <- ncvar_get(ulwrf.atm, "Upward_Long-Wave_Rad_Flux")
uswrf.data <- ncvar_get(uswrf, "Upward_Short-Wave_Rad_Flux")

# Dar nombres significativos a las columnas
apcp.data <- nameCols(apcp.data)
dlwrf.data <- nameCols(dlwrf.data)
dswrf.data <- nameCols(dswrf.data)
pres_msl.data <- nameCols(pres_msl.data)
pwat.data <- nameCols(pwat.data)
spfh.data <- nameCols(spfh.data)
tcdc.data <- nameCols(tcdc.data)
tcolc.data <- nameCols(tcolc.data)
tmax.data <- nameCols(tmax.data)
tmin.data <- nameCols(tmin.data)
tmp2m.data <- nameCols(tmp2m.data)
tmpsfc.data <- nameCols(tmpsfc.data)
ulwrfsfc.data <- nameCols(ulwrfsfc.data)
ulwrfatm.data <- nameCols(ulwrfatm.data)
uswrf.data <- nameCols(uswrf.data)

# Extraer el "ensemble" de control
apcp.data.e1 <- apcp.data[,,,1,]
dlwrf.data.e1 <- dlwrf.data[,,,1,]
dswrf.data.e1 <- dswrf.data[,,,1,]
pres_msl.data.e1 <- pres_msl.data[,,,1,]
pwat.data.e1 <- pwat.data[,,,1,]
spfh.data.e1 <- spfh.data[,,,1,]
tcdc.data.e1 <- tcdc.data[,,,1,]
tcolc.data.e1 <- tcolc.data[,,,1,]
tmax.data.e1 <- tmax.data[,,,1,]
tmin.data.e1 <- tmin.data[,,,1,]
tmp2m.data.e1 <- tmp2m.data[,,,1,]
tmpsfc.data.e1 <- tmpsfc.data[,,,1,]
ulwrfsfc.data.e1 <- ulwrfsfc.data[,,,1,]
ulwrfatm.data.e1 <- ulwrfatm.data[,,,1,]
uswrf.data.e1 <- uswrf.data[,,,1,]

# Obtener las coordenadas de ACME
acme <- stations[stations["stid"] == "ACME"]
acme.lat <- as.character(round(as.numeric(acme[2])))
acme.lon <- as.character(round(as.numeric(acme[3])))

# Obtener los predictores correspondientes a esas coordenadas
acme.apcp <- apcp.data.e1[acme.lon, acme.lat, ,]
acme.dlwrf <- dlwrf.data.e1[acme.lon, acme.lat, ,]
acme.dswrf <- dswrf.data.e1[acme.lon, acme.lat, ,]
acme.pres_msl <- pres_msl.data.e1[acme.lon, acme.lat, ,]
acme.pwat <- pwat.data.e1[acme.lon, acme.lat, ,]
acme.spfh <- spfh.data.e1[acme.lon, acme.lat, ,]
acme.tcdc <- tcdc.data.e1[acme.lon, acme.lat, ,]
acme.tcolc <- tcolc.data.e1[acme.lon, acme.lat, ,]
acme.tmax <- tmax.data.e1[acme.lon, acme.lat, ,]
acme.tmin <- tmin.data.e1[acme.lon, acme.lat, ,]
acme.tmp2m <- tmp2m.data.e1[acme.lon, acme.lat, ,]
acme.tmpsfc <- tmpsfc.data.e1[acme.lon, acme.lat, ,]
acme.ulwrfsfc <- ulwrfsfc.data.e1[acme.lon, acme.lat, ,]
acme.ulwrfatm <- ulwrfatm.data.e1[acme.lon, acme.lat, ,]
acme.uswrf <- uswrf.data.e1[acme.lon, acme.lat, ,]

# Promediar las 5 mediciones diarias en un solo valor
acme.apcp.means <- colMeans(acme.apcp)
acme.dlwrf.means <- colMeans(acme.dlwrf)
acme.dswrf.means <- colMeans(acme.dswrf)
acme.pres_msl.means <- colMeans(acme.pres_msl)
acme.pwat.means <- colMeans(acme.pwat)
acme.spfh.means <- colMeans(acme.spfh)
acme.tcdc.means <- colMeans(acme.tcdc)
acme.tcolc.means <- colMeans(acme.tcolc)
acme.tmax.means <- colMeans(acme.tmax)
acme.tmin.means <- colMeans(acme.tmin)
acme.tmp2m.means <- colMeans(acme.tmp2m)
acme.tmpsfc.means <- colMeans(acme.tmpsfc)
acme.ulwrfsfc.means <- colMeans(acme.ulwrfsfc)
acme.ulwrfatm.means <- colMeans(acme.ulwrfatm)
acme.uswrf.means <- colMeans(acme.uswrf)

# Unir los predictores en un solo dataframe
acme.final <- data.frame(
  Date = names(acme.dlwrf.means),
  apcp = acme.apcp.means,
  dlwrf = acme.dlwrf.means,
  dswrf = acme.dswrf.means,
  pres_msl = acme.pres_msl.means,
  pwat = acme.pwat.means,
  spfh = acme.spfh.means,
  tcdc = acme.tcdc.means,
  tcolc = acme.tcolc.means,
  tmax = acme.tmax.means,
  tmin = acme.tmin.means,
  tmp2m = acme.tmp2m.means,
  tmpsfc = acme.tmpsfc.means,
  ulwrfsfc = acme.ulwrfsfc.means,
  ulwrfatm = acme.ulwrfatm.means,
  uswrf = acme.uswrf.means
)

# Pre-procesar los datos de ACME
energy.acme <- energy_train[c("Date", "ACME")]
energy.acme <- transform(energy.acme, Date=as.character(Date))

# Generar un dataframe con la totalidad de los datos
energy.acme <- merge(energy.acme, acme.final, by="Date")

# Ver resultado
print(energy.acme)

# Almacenar dataframe
write.csv(energy.acme, "datos_procesados.csv")
