pmean <- function (directory, pullutant, id=1:332){
  files <- list.files(path = paste0(directory, "/"))
  f <- c()
  for (i in id){
    if (pullutant=="sulfate"){
      f <- append(f, read.csv(paste0(directory, "/", files[i]))$sulfate)
    } else if (pullutant=="nitrate"){
      f <- append(f, read.csv(paste0(directory, "/", files[i]))$nitrate)
    }
  }
  return(mean(f, na.rm = TRUE))
}

complete <- function (directory, id){
  files <- list.files(path = paste0(directory, "/"))
  nobs <- c()
  for (i in id){
    df = read.csv(paste0(directory, "/", files[i]))
    nobs <- append(nobs, nrow(subset(df, complete.cases(df))))
  }
  return(data.frame(id, nobs))
}

corr <- function (directory, threshold=0){
  files <- list.files(path = paste0(directory, "/"))
  res <- numeric(length = 0)
  for (i in 1:length(files)){
    df <- read.csv(paste0(directory, "/", files[i]))
    if (sum(complete.cases(df))>threshold){
      res <- append(res, cor(df$sulfate, df$nitrate, use = "pairwise.complete.obs"))
    }
  }
  return(res)
}