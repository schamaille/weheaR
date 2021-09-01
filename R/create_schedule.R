#' create_schedule
#'
#' @param default default mode (1: on, 0: off) when the time is not wt
#' @param period
#' @param hour_on
#' @param hour_off
#' @param test_period
#'
#' @return
#' @export
#'
#' @examples
create_schedule <- function(default=1,period=NULL,hour_on=NULL,hour_off=NULL,test_period=NULL){

  requireNamespace("lubridate")

  if(is.character(period)){
    period <- lubridate::ymd(period)
  }

  for(i in seq_along(period)){
    days <- seq.Date(from=period[1],to=period[2],by="1 day")
    start_times <- days+lubridate::hours(hour_on)
    if(hour_off>hour_on){
      end_times <- days+lubridate::hours(hour_off)
    } else {
      end_times <- days+lubridate::days(1)+lubridate::hours(hour_off)
    }
  }

  n <- length(start_times)
  on_df <- data.frame(start=start_times,
                      end=end_times,
                      mode=1,
                      id=seq(1, 2*n, 2))
  off_df <- data.frame(start=end_times[-n]+seconds(1),
                       end=start_times[-1]-seconds(1),
                       mode=0,
                       id=seq(2, 2*(n-1), 2))

  # combine while adding one line for from now to first time?
  all <- data.frame(start=now(tz="UTC"),
                    end=on_df$start[1]-minutes(1),
                    mode=default,
                    id=0)
all <- rbind(all,on_df,off_df)
all <- all[order(all$id),] #rearrange by time

# prepare to correct format
schedule <- paste0(format(all$start, "%d-%m-%Y %H:%M"),
                   " ",
                   format(all$end, "%d-%m-%Y %H:%M"),
                   " 000",all$mode,"m")
schedule <- c(paste0("000",default,"m"),schedule)

# write config.txt file
tempfile <- file("config.txt")
writeLines(text=schedule,con=tempfile,sep="\r\n")
#writeChar(schedule,tempfile,nchars
close(tempfile)

# output.file <- file("./config.txt", "wb")
# write(paste0("000",default,"m"), file = output.file)
# write.table(schedule,
#             row.names = FALSE,
#             col.names = FALSE,
#             file = output.file,
#             quote = FALSE,
#             append = TRUE,
#             sep = "")
# close(output.file)

# return
cat(paste0("The following schedule has been written in config.txt, \n with default mode: ",default))
print(all[,-4])

}



