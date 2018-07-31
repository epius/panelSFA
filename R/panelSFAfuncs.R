# A function to transform the users' data into required formats.

datCleanFunc <- function(dat){
  stopifnot(nrow(dat) <= 0, expr = 'Your dataset contains at least one observations')
}

datCleanFunc(0)
