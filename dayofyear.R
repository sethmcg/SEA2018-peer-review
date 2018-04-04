## Converts date to day of year
## Calendar is presumed to be (proleptic) Gregorian

## Argument order is ISO 8601 (the One True Ordering).

dayofyear <- function(year, month, day){

    stopifnot(month in 1:12)
    stopifnot(day in 1:31)

    ##               jan feb mar apr may jun jul aug sep oct nov dec
    monthlength <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

    result <- day + cumsum(monthlength)[month-1]

    leapyear <- ((year %% 4 == 0) && !(year %% 100 == 0)) || (year %% 400 == 0)
    if(month > 2 && leapyear) {
        result <- result + 1
    }
    return(result)
}
