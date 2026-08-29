make_x <- function(
  dt, tm, lon, lat, dur, dis, ...,
  check_xy=TRUE) {
  ## checking lengths
  nn <- c(dt=length(dt), tm=length(tm), lon=length(lon), lat=length(lat), dur=length(dur), dis=length(dis))
  n1 <- nn[nn == 1L]
  n2 <- nn[nn > 1L]
  if (!all(n2 == n2[1L]))
    stop("input lengths must be equal or 1")
  n <- unname(if (length(n2)) n2[1L] else n1[1L])
  if (length(dt) == 1L)
    dt <- rep(dt, n)
  if (length(tm) == 1L)
    tm <- rep(tm, n)
  if (length(lon) == 1L)
    lon <- rep(lon, n)
  if (length(lat) == 1L)
    lat <- rep(lat, n)
  if (length(dur) == 1L)
    dur <- rep(dur, n)
  if (length(dis) == 1L)
    dis <- rep(dis, n)
  ## types
  lat <- as.numeric(lat)
  lon <- as.numeric(lon)
  dur <- as.numeric(dur)
  dis <- as.numeric(dis)
  ## parse date+time into POSIXlt
  dt <- as.character(dt)
  tm <- as.character(tm)
  dtm <- strptime(paste0(dt, " ", tm, ":00"),
    format="%Y-%m-%d %H:%M:%S", tz="America/Edmonton")
  day <- as.integer(dtm$yday)
  hour <- as.numeric(round(dtm$hour + dtm$min/60, 2))
  ## checks
  checkfun <- function(x, name="", range=c(-Inf, Inf)) {
    if (any(x[!is.na(x)] %)(% range))
      stop(sprintf("Parameter %s is out of range [%.0f, %.0f]", name, range[1], range[2]))
    invisible(NULL)
  }
  ## BCR 4:14 included
  ## crs: WGS84 (EPSG: 4326)
  ## "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  #         min       max
  #x -163.89547 -52.66936
  #y   39.66214  68.98741
  if (check_xy) {
    checkfun(lon, "lon", c(-164, -52))
    checkfun(lat, "lat", c(39, 69))
  }
  checkfun(day, "day", c(0, 360))
  checkfun(hour, "hour", c(0, 24))
  checkfun(dur, "dur", c(0, Inf))
  #checkfun(dis, "dis", c(0, Inf))
  if (any(is.infinite(lon)))
    stop("Parameter lon must be finite")
  if (any(is.infinite(lat)))
    stop("Parameter lat must be finite")
  ## handling missing values
  ok_xy <- !is.na(lon) & !is.na(lat)

  ## intersect here
  xy <- data.frame(x=lon, y=lat)
  xy$x[is.na(xy$x)] <- mean(xy$x, na.rm=TRUE)
  xy$y[is.na(xy$y)] <- mean(xy$y, na.rm=TRUE)
  xy <- vect(xy, geom=c("x", "y"), crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  xy <- project(xy, crs)

  ## LCC4 and LCC2
  vlcc <- extract(rlcc, xy)$lcc
  # 0: No data (NA/NA)
  # 1: Temperate or sub-polar needleleaf forest (Conif/Forest)
  # 2: Sub-polar taiga needleleaf forest (Conif/Forest)
  # 5: Temperate or sub-polar broadleaf deciduous (DecidMixed/Forest)
  # 6:  Mixed Forest (DecidMixed/Forest)
  # 8: Temperate or sub-polar shrubland (Open/OpenWet)
  # 10: Temperate or sub-polar grassland (Open/OpenWet)
  # 11: Sub-polar or polar shrubland-lichen-moss (Open/OpenWet)
  # 12: Sub-polar or polar grassland-lichen-moss (Open/OpenWet)
  # 13: Sub-polar or polar barren-lichen-moss (Open/OpenWet)
  # 14: Wetland (Wet/OpenWet)
  # 15: Cropland (Open/OpenWet)
  # 16: Barren Lands (Open/OpenWet)
  # 17: Urban and Built-up (Open/OpenWet)
  # 18: Water (NA/NA)
  # 19: Snow and Ice (NA/NA)
  lcclevs <- c("0"="", "1"="Conif", "2"="Conif", "3"="", "4"="",
               "5"="DecidMixed", "6"="DecidMixed", "7"="", "8"="Open", "9"="",
               "10"="Open", "11"="Open", "12"="Open", "13"="Open", "14"="Wet",
               "15"="Open", "16"="Open", "17"="Open", "18"="", "19"="")
  lcc4 <- factor(lcclevs[vlcc+1], c("DecidMixed", "Conif", "Open", "Wet"))
  lcc2 <- lcc4
  levels(lcc2) <- c("Forest", "Forest", "OpenWet", "OpenWet")

  ## TREE
  vtree <- extract(rtree, xy)$tree
  TREE <- vtree / 100
  TREE[TREE %)(% c(0, 1)] <- 0

  ## extract seedgrow value (this is rounded)
  d1 <- extract(rd1, xy)$seedgrow
  ## UTC offset + 7 makes Alberta 0 (MDT offset)
  tz <- raster::extract(rtz, xy)$utcoffset + 7

  ## transform the rest
  JDAY <- round(day / 365, 4) # 0-365
  TREE <- round(vtree / 100, 4)
  MAXDIS <- round(dis / 100, 4)
  MAXDUR <- round(dur, 4)

  ## sunrise time adjusted by offset
  ok_dt <- !is.na(dtm)
  dtm[is.na(dtm)] <- mean(dtm, na.rm=TRUE)
  sr <- suntools::sunriset(cbind("X"=lon, "Y"=lat),
                           as.POSIXct(dtm, tz="America/Edmonton"),
                           direction="sunrise", POSIXct.out=FALSE) * 24
  TSSR <- round(unname((hour - sr - tz) / 24), 4)

  ## days since local spring
  DSLS <- (day - d1) / 365

  out <- data.frame(
    TSSR=TSSR,
    JDAY=JDAY,
    DSLS=DSLS,
    LCC2=lcc2,
    LCC4=lcc4,
    TREE=TREE,
    MAXDUR=MAXDUR,
    MAXDIS=MAXDIS,
    ...)
  out$TSSR[!ok_xy | !ok_dt] <- NA
  out$DSLS[!ok_xy] <- NA
  out$LCC2[!ok_xy] <- NA
  out$LCC4[!ok_xy] <- NA
  out$TREE[!ok_xy] <- NA
  out
}


make_off <- function(spp, x) {

  if (length(spp) > 1L)
    stop("spp argument must be length 1")
  spp <- as.character(spp)
  ## checks
  if (!(spp %in% getBAMspecieslist()))
    stop(sprintf("Species %s has no QPAD estimate", spp))

  ## constant for NA cases
  cf0 <- exp(unlist(coefBAMspecies(spp, 0, 0)))
  ## best model (includes DSLS)
  #mi <- bestmodelBAMspecies(spp, type="BIC",
  #    model.sra=names(getBAMmodellist()$sra)[!grepl("DSLS", getBAMmodellist()$sra)])
  mi <- bestmodelBAMspecies(spp, type="BIC")
  cfi <- coefBAMspecies(spp, mi$sra, mi$edr)

  TSSR <- x$TSSR
  DSLS <- x$DSLS
  JDAY <- x$JDAY
  lcc2 <- x$LCC2
  lcc4 <- x$LCC4
  TREE <- x$TREE
  MAXDUR <- x$MAXDUR
  MAXDIS <- x$MAXDIS
  n <- nrow(x)

  ## make Xp and Xq
  #' Design matrices for singing rates (`Xp`) and for EDR (`Xq`)
  Xp <- cbind(
    "(Intercept)"=1,
    "TSSR"=TSSR,
    "JDAY"=JDAY,
    "TSSR2"=TSSR^2,
    "JDAY2"=JDAY^2,
    "DSLS"=DSLS,
    "DSLS2"=DSLS^2)
  Xq <- cbind("(Intercept)"=1,
    "TREE"=TREE,
    "LCC2OpenWet"=ifelse(lcc4 %in% c("Open", "Wet"), 1, 0),
    "LCC4Conif"=ifelse(lcc4=="Conif", 1, 0),
    "LCC4Open"=ifelse(lcc4=="Open", 1, 0),
    "LCC4Wet"=ifelse(lcc4=="Wet", 1, 0))

  p <- rep(NA, n)
  A <- q <- p
  ## design matrices matching the coefs
  Xp2 <- Xp[,names(cfi$sra),drop=FALSE]
  OKp <- rowSums(is.na(Xp2)) == 0
  Xq2 <- Xq[,names(cfi$edr),drop=FALSE]
  OKq <- rowSums(is.na(Xq2)) == 0
  ## calculate p, q, and A based on constant phi and tau for the respective NAs
  p[!OKp] <- sra_fun(MAXDUR[!OKp], cf0[1])
  unlim <- ifelse(MAXDIS[!OKq] == Inf, TRUE, FALSE)
  A[!OKq] <- ifelse(unlim, pi * cf0[2]^2, pi * MAXDIS[!OKq]^2)
  q[!OKq] <- ifelse(unlim, 1, edr_fun(MAXDIS[!OKq], cf0[2]))
  ## calculate time/lcc varying phi and tau for non-NA cases
  phi1 <- exp(drop(Xp2[OKp,,drop=FALSE] %*% cfi$sra))
  tau1 <- exp(drop(Xq2[OKq,,drop=FALSE] %*% cfi$edr))
  p[OKp] <- sra_fun(MAXDUR[OKp], phi1)
  unlim <- ifelse(MAXDIS[OKq] == Inf, TRUE, FALSE)
  A[OKq] <- ifelse(unlim, pi * tau1^2, pi * MAXDIS[OKq]^2)
  q[OKq] <- ifelse(unlim, 1, edr_fun(MAXDIS[OKq], tau1))
  ## log(0) is not a good thing, apply constant instead
  ii <- which(p == 0)
  p[ii] <- sra_fun(MAXDUR[ii], cf0[1])

  data.frame(
    p=p,
    q=q,
    A=A,
    correction=p*A*q,
    offset=log(p) + log(A) + log(q))
}


