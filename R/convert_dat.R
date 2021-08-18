#' convert_dat
#'
#' This function convert the original .DAT files containing accelerometer or magnetometer data
#' into either rds (by default) or csv files.
#' @param filenames vector; names of files (without path) to convert
#' @param file_type character string; 'rds' by default, any other will lead to conversion to csv
#' @param dir_in folder path of the .DAT files; if NULL, the working directory is used
#' @param dir_out folder path of the converted files; if NULL, the working directory is used
#' @param id character string; if not NULL, the string is used as prefix for the filenames of the converted files
#' @param tz time zone information; NULL (default) throws an error, as time is your most valuable information!
#' tz could be either a character string being a time zone code (e.g., "Europe/Paris") or a numeric value
#' indicating the number of hours to add/subtract to UTC time (e.g., -1 for times that would be UTC-1).
#'
#'
#' @return files with converted data
#' @export
#'
#' @examples
convert_dat <- function(filenames,file_type="rds",dir_in=NULL,dir_out=NULL,id=NULL,tz=NULL){

  requireNamespace("lubridate") # later do without?
  requireNamespace("stringr")
  if(file_type!="rds"){
    requireNamespace(readr) # readr is ~3* quicker than base R for this, worth it with many files
  }

  # time-zone check and time zone definition if tz is numeric
  # Important note: the sign is intentionally inverted in the definition, see
  # https://en.wikipedia.org/wiki/List_of_tz_database_time_zones
  if(is.null(tz)){
    stop("You MUST provide time zone information! (you will thank me later ;-))")
  }
  if(is.character(tz)){
    if(tz %in% OlsonNames()){
      paste0("The selected time zone code for the data is ",tz)
    } else {
      stop("Your time zone does not seem to exist - typo?")
    }
  }
  if(is.numeric(tz)){
    if(tz>=0){tz <- paste0("Etc/GMT-",tz)} else {tz <- paste0("Etc/GMT+",abs(tz))}
    cat(paste0("The selected time zone code for the data is ",tz,"\n\nAlthough the sign is inverted in the names of Etc area code,
               (see https://en.wikipedia.org/wiki/List_of_tz_database_time_zones) \nthe times in the converted data file will be correct
               \nConvert to another time zone later if that bothers you, but pay attention to DST when relevant"))
  }

  # extract time info from filenames
  in_ftime <- str_sub(filenames,start=nchar(filenames)-16,end=nchar(filenames)-4)
  ref_time <- dmy_hms(in_ftime,tz=tz)
  out_ftime <- format(true_time,"%Y%m%d_%H%M%S") # this could later be removed if filenames are adjusted, and we could use time_info straight

  # adjust file names
  ext <- ifelse(file_type=="rds",".rds",".csv")
  out_filenames <- paste0(sub("_.*", "",filenames),"_",out_ftime,ext)

    # add id info to filenames if id is provided
    if(!is.null(id)){
      out_filenames <- paste0(id,"_",out_filenames)
    }

    # add path info to filenames
    if(!is.null(dir_in)){
      full_in_filenames <- paste0(dir_in,"/",filenames)
    } else {full_in_filenames <- filenames}

    if(!is.null(dir_out)){
      full_out_filenames <- paste0(dir_out,"/",out_filenames)
    }

  # function to uncompress the files
  uncompress <- function(filen,max_file_size = 10e6){
    my_data <- readBin(filen, "raw", max_file_size)
    df      <- as.data.frame(t(matrix(as.numeric(my_data), nrow = 12)))
    as_sign <- function(x, y) {(x + 2^8 * y) -> z; ifelse(z < 2^15, z, z - 2^16)}
    data.frame(time = df$V1 + 2^8 * df$V2 + 2^16 * df$V3 + 2^24 * df$V4,
               rec = df$V5  + 2^8 * df$V6,
               x = as_sign(df$V7,  df$V8),
               y = as_sign(df$V9,  df$V10),
               z = as_sign(df$V11, df$V12))
  }

  # function to write converted file
  write_converted <- function(obj,fname){
    if(file_type=="rds"){
      saveRDS(obj,fname,compress=T)
    } else {
      obj$time <- as.character(obj$time)
      write_csv(obj,fname)
    }
  }


  # do across all files
  for(i in seq_along(filenames)){

    # uncompress
    temp <- uncompress(full_in_filenames[i])[,-2] # simultaneously removing the unneeded rec column
    colnames(temp)[1] <- "ms_to_on"
    temp$ms <- temp$ms_to_on-temp$ms_to_on[1]

    # create time vector of records
    temp$time <- ref_time[i]+milliseconds(temp$ms)

    # write file
    write_converted(temp[,c(6,5,1:4)],full_out_filenames[i])

  }

}
