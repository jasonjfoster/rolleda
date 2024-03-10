palette_jjf <- function(n_cols, n_rows = 1) {
  # if n_cols > length(colors) then repeat with transparency
  # otherwise repeat n_rows with transparency
  
  colors_jjf <- c(grDevices::rgb(236, 105,  65, maxColorValue = 255),
                  grDevices::rgb(253, 197, 129, maxColorValue = 255),
                  grDevices::rgb( 20,  76,  89, maxColorValue = 255),
                  grDevices::rgb( 22, 144, 133, maxColorValue = 255))
  
  rep_cols <- floor(n_cols / length(colors_jjf))
  rep_rows <- n_rows - 1
  
  rep_alpha <- rep_cols + rep_rows + 1
  a <- 1
  b <- 1 / rep_alpha
  
  if (rep_cols > 0) {
    
    result <- colors_jjf
    
    for (j in 1:rep_cols) {
      
      a <- a - b
      result <- c(result, scales::alpha(colors_jjf, alpha = a))
      
    }
    
    result <- result[1:n_cols]
    
  } else {
    result <- colors_jjf[1:n_cols]
  }
  
  if (rep_rows > 0) {
    
    result_ls <- list(result)
    
    for (i in 1:rep_rows) {
      
      a <- a - b
      result_ls <- append(result_ls, list(scales::alpha(result_ls[[i]], alpha = a)))
      
    }
    
    result <- do.call(c, result_ls)
    
  }
  
  return(result)
  
}

is_date <- function(x) {
  
  result <- any(class(x) %in% c("Date", "POSIXct", "POSIXlt", "POSIXt"))
  
  return(result)
  
}

is_blank <- function(x) {
  
  result <- anyNA(x) || any(x == "")
  
  return(result)
  
}

try_formats <- c(
  "%Y-%m-%d", # Year-month-day (e.g., 2024-03-01)
  "%Y/%m/%d", # Year/month/day (e.g., 2024/03/01)
  "%d-%b-%Y", # Day-abbreviated month-year (e.g., 01-Mar-2024)
  "%d %B %Y", # Day-full month-year (e.g., 01 March 2024)
  "%Y-%m-%d %H:%M:%S", # Year-month-day hour:minute:second (e.g., 2024-03-01 12:30:45)
  "%Y-%m-%d %I:%M:%S %p", # Year-month-day hour:minute:second AM/PM (e.g., 2024-03-01 12:30:45 PM)
  "%Y/%m/%d %H:%M", # Year/month/day hour:minute (e.g., 2024/03/01 12:30)
  "%Y-%m-%d %H:%M", # Year-month-day hour:minute (e.g., 2024-03-01 12:30)
  "%d-%b-%Y %I:%M %p", # Day-abbreviated month-year hour:minute AM/PM (e.g., 01-Mar-2024 12:30 PM)
  "%d %B %Y %H:%M:%S" # Day-full month-year hour:minute:second (e.g., 01 March 2024 12:30:45)
)

parse_dates <- function(x, formats = try_formats) {
  
  tryCatch({
    
    parsed_dates <- as.Date(x, tryFormats = formats)
    if (!anyNA(parsed_dates)) {
      return(parsed_dates[!is.na(parsed_dates)])
    }
    
    parsed_dates <- as.POSIXct(x, tryFormats = formats)
    if (!anyNA(parsed_dates)) {
      return(parsed_dates[!is.na(parsed_dates)])
    }
    
    parsed_dates <- as.POSIXlt(x, tryFormats = formats)
    if (!anyNA(parsed_dates)) {
      return(parsed_dates[!is.na(parsed_dates)])
    }
    
    return(x)
    
  }, error = function(e) {
    return(x)
  })
  
}

fread_idx <- function(x) {
  
  file <- data.table::fread(x)
  
  # parse dates from characters
  chr_cols <- colnames(file)[vapply(file, is.character, logical(1))]
  dbl_cols <- colnames(file)[vapply(file, is.numeric, logical(1))]
  
  if (length(chr_cols) > 0) {
    file[ , (chr_cols) := lapply(.SD, parse_dates), .SDcols = chr_cols]
  }
  
  file[ , (dbl_cols) := lapply(.SD, as.numeric), .SDcols = dbl_cols]
  
  # https://stackoverflow.com/a/33196350
  return(file[])
  
}

file_ext <- function(x) {
  
  splits <- strsplit(x, '\\.')[[1]]
  
  result <- splits[length(splits)]
  
  return(result)
  
}
