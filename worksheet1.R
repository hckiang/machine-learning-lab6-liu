library('geosphere')

stations <- read.csv('../question/stations.csv', fileEncoding = 'iso8859-1')
temps <- read.csv('../question/temps50k.csv')

st <- merge(stations, temps, by="station_number")
st$date <- as.Date(st$date)
st$numtime <- as.difftime(as.character(st$time), units = 'secs')

## Transform date to the number of day from the beginning of its year
st$yday <- as.POSIXlt(st$date)$yday

circular_diff <- function (a, b, period) {
    dif <- abs(as.numeric(a - b))
    ifelse( 2*dif > period, period - dif, dif )
}

pred <- function (long, lat, date, times) {
    times <- as.difftime(times, units = 'sec')
    yday <- as.POSIXlt(as.Date(date))$yday

    ## 550 km is approximately from Malmo to Uppsala
    h_distance <- 550000 / sqrt(2)
    h_date <- 90 / sqrt(2)
    h_time <- 3600 * 4.5 / sqrt(2)

    kern.geodist <-
        exp(- (mapply(function(la,lo) distHaversine(c(la,lo), c(long,lat)),
                      st$longitude, st$latitude) / h_distance)^2 )
    kern.datedist <- exp( - (circular_diff(yday, st$yday, 365) / h_date)^2 )
    kern.timedist <- exp( - sapply(times, function (time) {
        circular_diff(time, st$numtime, 86400) / h_time })^2 )

    apply( kern.timedist, 2, function(ktimedist) {
        kerns <- matrix(c(kern.geodist, kern.datedist, ktimedist), ncol = 3)
        t(st$air_temperature) %*% rowSums(kerns) / sum(kerns)
    })
}

times <- c('02:00:00', "04:00:00", "06:00:00", '08:00:00', '10:00:00',
           '12:00:00', '14:00:00', '16:00:00', '18:00:00',
           '20:00:00', '22:00:00', '24:00:00')

forecast <- pred(15.577635, 58.396221, '2016-12-15', times)
pdf('uniwinter.pdf')
plot(forecast, type = 'o', xaxt='n',
     xlab = 'Hour in the day', ylab = 'Temperature forecast',
     main = 'Temperature Forecast of 2016-12-15 at University Library')
axis(1, at = 1:length(times),
     labels = gsub('(\\d{2}):\\d{2}:\\d{2}', '\\1', times))
dev.off()

forecast <- pred(15.577635, 58.396221, '2017-06-25', times)
pdf('unisummer.pdf')
plot(forecast, type = 'o', xaxt='n',
     xlab = 'Hour in the day', ylab = 'Temperature forecast',
     main = 'Temperature Forecast of 2017-06-25 at University Library')
axis(1, at = 1:length(times),
     labels = gsub('(\\d{2}):\\d{2}:\\d{2}', '\\1', times))
dev.off()
