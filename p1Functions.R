readAll.csv <- function(directory, id = 1:332) {
      files <- list.files(directory, full.names = TRUE)
      
      tmp <- vector(mode = "list", length = length(id))
      for(i in id){
            fname <- paste(directory, as.character(i), ".csv")
            tmp[[i]] <- read.csv(files[[i]])
      }
      df <- do.call(rbind, tmp)
}

pollutantmean <- function(directory, pollutant, id = 1:332){
      ## 'directory' is a character vector of length 1 indicating
      ## the location of the CSV files
      
      ## 'pollutant' is a character vector of length 1 indicating
      ## the name of the pollutant for which we will calculate the
      ## mean; either "sulfate" or "nitrate"
      
      ## 'id'is an integer vector indicating the monitor ID numbers
      ## to be used
      
      ## Return the mean of the pollutant across all monitors list
      ## in the 'id' vector (ignoring NA values)

      df <- readAll.csv(directory, id)
      mean(df[,pollutant], na.rm = TRUE)                    
}

complete <- function(directory, id = 1:332) {
      ## 'directory' is a character vector of length 1 indicating
      ## the location of the CSV files

      ## 'id' is an integer vector indicating the monitor ID numbers 
      ## to be used
      
      ## Return a data frame of the form:
      ## id nobs
      ## 1  117
      ## 2  1041
      ## ...
      ## where 'id' is the monitor ID number and 'nobs' is the
      ## number of complete cases
      
      df <- readAll.csv(directory, id)
      
      tmp <- vector(mode = "list", length = length(id))
      
      for(i in seq_along(id)){
            dat <- df[which(df$ID == id[i]), ]
            tmp[[i]] <- data.frame(id = id[i], nobs = sum(!is.na(dat$sulfate) & !is.na(dat$nitrate)))
      }
      
      do.call(rbind, tmp)
}

corr <- function(directory, threshold = 0) {
      ## 'directory' is a character vector of length 1 indicating
      ## the location of the CSV files
      
      ## 'threshold' is a numeric vector of length 1 indicating the
      ## number of completely observed observations (on all
      ## variables) required to compute the correlation between
      ## nitrate and sulfate; the default is 0
      
      ## Return a numeric vector of correlations
      
      df <- readAll.csv(directory)
      ids <- vector(mode = "logical", length = 332)

      for(i in 1:332){
            dat <- df[which(df$ID == i),]
            if(sum(!is.na(dat$sulfate) & !is.na(dat$nitrate)) >= threshold){
                  ids[i] <- TRUE
            } else {
                  ids[i] <- FALSE
            }
      }
      
      if(!sum(ids)){
            return( vector(mode = "numeric"))
      }
      
      cors <- vector(mode = "numeric", length = length(sum(ids)))
      
      ind <- 1
      
      for(i in seq_along(ids)) {
            if(ids[i]){
                  dat <- df[which(!is.na(df$sulfate) & !is.na(df$nitrate) & df$ID == i), ] 
                  cors[ind] <- cor(x = dat$sulfate, y = dat$nitrate)
                  ind <- ind + 1
            }
      }
      
      cors[which(!is.na(cors))]
}