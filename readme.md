# Campbell Scientific Data Formats

## About
This R package provides rudimentary methods to deal with data formats (TOA5 only
for now) used by LoggerNet software from Campbell Scientific.

## Synposis

```r
library(csdf)
Sys.setenv(TZ='GMT')
fpath <- system.file("extdata", "Station_Daily.dat", package="csdf")
obj <- read.toa5(fpath)
plot(obj)
plot(`AirT_Max` ~ time,
     within(obj@data, {
       time <- as.POSIXct(strftime(AirT_TMx, "%H:%M:%S"), format="%H:%M:%S")
     }))
write.toa5(obj, "elsewhere.dat")
write.csv(as.data.frame(obj), "plain.csv")
```

## Installation
Have [devtools](https://github.com/hadley/devtools) package installed first with
```r
install.packages("devtools")
```
Then install this package from GitHub using
```r
devtools::install_github("mlt/csdf")
```

## License
The source code for this R package is released under [AGPL-3 (or later) license](https://www.gnu.org/licenses/agpl-3.0.en.html).

## Disclaimer
This software is not endorsed by Campbell Scientific. All names and trademarks
are mentioned here for reference only. They are the property of their respective owners.
