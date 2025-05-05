#' @import dplyr

setClass("rheocompass", slots = list(file = "vector",
                                     csv = "data.frame",
                                     metadata = "data.frame",
                                     measurement = "list",
                                     long = "data.frame"))


#' Read rheocompass csv
#'
#' @export
readData <- function(filenames, meta_from_file_name=FALSE){
  dat3 <- lapply(filenames, function(f){
    rheo <- new("rheocompass", file = f)
    rheo@csv <- read.csv(f, header = F)
    rheo@measurement <- parseData(rheo)
    rheo@metadata <- getMeta(rheo,meta_from_file_name)
    rheo@long <- makeLong(rheo)
    rheo
  })
  if(length(dat3) > 1){
    return(do.call(merge,dat3))
  }else{
    return(dat3[[1]])
  }
}

parseData <- function(rheo){
  dat <- rheo@csv
  test_start <- which(dat$V1=='Test:')
  test_end <- c(test_start[-1]-2, nrow(dat))
  tests <- dat$V2[test_start]

  dat2 <- lapply(1:length(tests), function(t){
    a <- dat[test_start[t]:test_end[t],]

    measurement_start <- which(a$V1=='Result:')
    if(length(measurement_start) > 1){
      measurement_end <- c(measurement_start[-1]-2, nrow(a))
    }else{
      measurement_end <- nrow(a)
    }
    measurements <- a$V2[measurement_start]

    d <- lapply(1:length(measurements), function(m){

      b <- a[(measurement_start[m]+0):measurement_end[m],]


      interval_start <- which(b$V1=='Interval and data points:')
      if(length(interval_start) > 1){
        interval_end <- c(interval_start[-1]-2, nrow(b))
      }else{
        interval_end <- nrow(b)
      }
      intervals <- b$V2[interval_start]

      e <- lapply(1:length(intervals), function(i){
        f <- b[(interval_start[i]+0):interval_end[i],]

        var_names <- paste(unlist(f[2,]),unlist(f[4,]))


        colnames(f) <- var_names
        f <- as.data.frame(sapply(f, as.numeric))
        f$sample <- tests[t]
        f$measurement <- measurements[m]
        f$interval <- intervals[i]
        f[5:nrow(f),c(-1,-2)]
      }
      )
      names(e) <- paste('Interval',intervals)
      e
    }
    )
    names(d) <- measurements
    d

  })
  names(dat2) <- tests
  return(dat2)
}

getMeta <- function(rheo,meta_from_file_name){
  meta <- data.frame(sample = names(rheo@measurement))
  if(meta_from_file_name==TRUE){
    meta <- meta %>% tidyr::separate(sample, sep='_', into = paste0('V',1:(length(gregexpr('_',meta$sample[1])[[1]])+1)))
  }
  meta$sample <- names(rheo@measurement)
  meta
}

makeLong <- function(rheo){

  tryCatch(
    {
      long <- do.call(rbind, lapply(rheo@measurement, function(x) do.call(rbind, lapply(x, function(y) do.call(rbind, y)))))
    }, error = function(msg){
      message(paste("Column names are not the same. Make sure each sample has the same variables with the same units."))
      return(NA)
    })
  rownames(long) <- c()
  var_names <- colnames(long)
  var_names <- gsub("\\[|\\]",'',var_names)
  var_names <- gsub(' ','_',var_names)
  var_names <- gsub('·','.',var_names)
  var_names <- gsub('°','',var_names)
  var_names <- gsub('_1/s','_Hz',var_names)
  var_names <- gsub('%','percent',var_names)

  print(var_names)
  colnames(long) <- var_names

  sample_map <- data.frame(id = unique(long$sample),
                           new_id = paste0('sample_',1:length(unique(long$sample))))
  long$id <- sample_map$new_id[match(long$sample,sample_map$id)]
  sort(unique(long$sample))

  long
}

#' Merge rheocompass objects
#'
#' @export
merge.rheocompass <- function(...){
  x <- list(...)
  fs <- unlist(sapply(x, function(y) y@file))
  meta <- do.call(rbind, lapply(x, function(y) y@metadata))
  csvs <- do.call(rbind, lapply(x, function(y) y@csv))
  meas <- unlist(lapply(x, function(y) y@measurement), recursive = F)
  l <- do.call(rbind, lapply(x, function(y) y@long))
  return(new("rheocompass",file=fs,csv=csvs,metadata=meta,measurement=meas,long=l))
}
