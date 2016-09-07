#' Read Decagon DataTrac 3 SQLite files
#'
#' This is a very limited implementation to read GS3 sensor data out of DataTrac
#' (SQLite) database files directly. This function uses default calibration
#' curves and will issue a warning if calibration data are found in a database.
#'
#' @param file a dot db file with SQLite data used by DataTrac3. Perhaps you can
#'   find some at \code{\%LocalAppData\%\\Decagon\\DataTrac\\databases\\New Devices\\} .
#' @param process ignored
#'
#' @return an S4 class \code{\linkS4class{csdf}}
#' @export
#' @importFrom utils compareVersion
#'
#' @examples
#' fpath <- system.file("extdata", "DataTrac3.db", package="csdf")
#' obj <- read.dt(fpath)
#' \dontrun{
#' write.toa5(obj, "somewhere.dat")
#' }
read.dt <- function(file, process=TRUE) {
  if(!requireNamespace("RSQLite") || !requireNamespace("DBI"))
    stop("Failed to load RSQLite and DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), file)
  on.exit(DBI::dbDisconnect(con))
  h <- DBI::dbReadTable(con, "device")
  meta <- with(h, data.frame(station=name,
                             model=type,
                             serial=sn,
                             os=ver,
                             dld=version,
                             signature=-1,
                             table="DataTrac3"))
  cal <- DBI::dbReadTable(con, "calibrations")
  if(nrow(cal) != 0)
    warning("We don't support calibrations")
  ## Make sure you use 3.7.11 or above SQLite
  ## for max(timestamp), x, y, z to work as expected
  ## http://www.sqlite.org/releaselog/3_7_11.html
  sqlver <- DBI::dbGetInfo(con)$serverVersion
  if(compareVersion(sqlver, "3.7.11") != 1)
    stop(sprintf("Incorrect behavior is expected with %s", sqlver))
  ports <- DBI::dbGetQuery(con, "
SELECT portNum, sensorName,
  label || '_' || depth as name,
  max(timestamp) as timestamp
FROM ports
where not sensorName = 'None Selected'
group by portNum
order by portNum--, timestamp desc
")
  rownames(ports) <- ports$portNum
  stopifnot(all(ports$sensorName == 'GS3 Moisture/Temp/EC'))
  sql <- sprintf("select time, %s from raw",
                 paste(
                   lapply(ports$portNum, function(x)
                     sprintf('(ch%d & 0x0fff)/50. as "%s_VWC",
                             (ch%d>>12) & 0x03ff as "%s_Ec",
                             (ch%d>>22) & 0x03ff as "%s_Temp"',
                             x, ports[as.character(x), 'name'],
                             x, ports[as.character(x), 'name'],
                             x, ports[as.character(x), 'name'])),
                   collapse=", "))
  x0 <- DBI::dbGetQuery(con, sql)
  variables <- data.frame(TIMESTAMP=c("", "TS"))
  # nn <- with(expand.grid(c("_VWC", "_Ec", "_Temp"), ports$name), paste(Var2, Var1, sep=""))
  variables <- cbind(variables, lapply(ports$name, function(x) {
    setNames(list(c("m^3/m^3", "Avg"), c("mS/cm", "Avg"), c("Deg C", "Avg")),
             paste(x, c("_VWC", "_Ec", "_Temp"), sep=""))
  }))
  rownames(variables) <- variables.row.names
  x <- within(x0, {
    time <- as.POSIXct(time, tz="GMT", origin="2000-01-01")
    dummy <- lapply(ports$portNum, function(x) {
      vwc.name <- sprintf("%s_VWC", ports[as.character(x), 'name'])
      ea <- get(vwc.name)
      ec.name <- sprintf("%s_Ec", ports[as.character(x), 'name'])
      rec <- get(ec.name)
      temp.name <- sprintf("%s_Temp", ports[as.character(x), 'name'])
      rt <- get(temp.name)
      miss <- ea==0 & rec==0 & rt==0
      vwc <- ifelse(miss, NA,
                    5.89e-6*ea^3 - 7.62e-4 * ea**2 + 3.67e-2 * ea - 7.53e-2)
      assign(vwc.name, vwc, envir=parent.frame(2))
      ec <- ifelse(miss, NA, 10^(rec/215.-3))
      assign(ec.name, ec, envir=parent.frame(2))
      temp <- ifelse(miss, NA,
                     ifelse(rt <= 900, (rt-400.)/10, ((900+5*(rt-900))-400)/10.))
      assign(temp.name, temp, envir=parent.frame(2))
    })
    rm(dummy)
  })
  names(x)[1] <- "TIMESTAMP"
  new("csdf", data=x, variables=variables, meta=meta)
}
