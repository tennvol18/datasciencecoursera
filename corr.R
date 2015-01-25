setwd("C:/Users/arbdc/datasciencecoursera")
getwd()
list.files()

corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating the location of
  ## the CSV files

  ## 'threshold' is a numeric vector of length 1 indicating the number of
  ## completely observed observations (on all variables) required to compute
  ## the correlation between nitrate and sulfate; the default is 0

  ## Return a numeric vector of correlations
  df = complete(directory)
  ids = df[df["nobs"] > threshold, ]$id
  corrr = numeric()
  for (i in ids) {

    newRead = read.csv(paste(directory, "/", formatC(i, width = 3, flag = "0"), 
                             ".csv", sep = ""))
    dff = newRead[complete.cases(newRead), ]
    corrr = c(corrr, cor(dff$sulfate, dff$nitrate))
  }
  return(corrr)
}
complete <- function(directory, id = 1:332) {
  f <- function(i) {
    data = read.csv(paste(directory, "/", formatC(i, width = 3, flag = "0"), 
                          ".csv", sep = ""))
    sum(complete.cases(data))
  }
  nobs = sapply(id, f)
  return(data.frame(id, nobs))
}