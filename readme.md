# Campbell Scientific Data Formats
[![Build Status](https://travis-ci.org/mlt/csdf.svg?branch=master)](https://travis-ci.org/mlt/csdf)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/mlt/csdf?branch=master&svg=true)](https://ci.appveyor.com/project/mlt/csdf)
[![Coverage Status](https://codecov.io/github/mlt/csdf/coverage.svg?branch=master)](https://codecov.io/github/mlt/csdf?branch=master)

This R package provides rudimentary methods to deal with data formats (TOA5 only
for now) used by LoggerNet software from Campbell Scientific.

## Synposis

```r
library(csdf)
Sys.setenv(TZ='GMT')
fpath <- system.file("extdata", "Station_Daily.dat", package="csdf")
obj <- read.toa5(fpath)
summary(obj)
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
If you are on MS Windows and have no RTools installed, you may try to install one of the prebuilt binaries from AppVeyor CI platform like
```r
install.packages("https://github.com/mlt/csdf/releases/download/0.1.0/csdf_0.1.0.zip", repos=NULL)
```
You can find all available tagged releases on the corresponding tab https://github.com/mlt/csdf/releases.

## License
The source code for this R package is released under [AGPL-3 (or later) license](https://www.gnu.org/licenses/agpl-3.0.en.html).

## Disclaimer
This software is not endorsed by Campbell Scientific. All names and trademarks
are mentioned here for reference only. They are the property of their respective owners.
