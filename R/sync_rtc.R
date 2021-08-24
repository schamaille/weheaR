#' sync_rtc
#' This functions allows to sync the Real-Time-Clock of the logger with the computer's time.
#' Plug in the cable, check that the switch is in 'off' position, power the logger.
#' Then call the function as sync_rtc(). This should most of the time works, but in specific
#' cases you may have to provide the port name directly (see below).
#'
#' @param port character string; either "auto" (default), which will use the last port (windows)
#' or the ttyUSB0 port (linux), or directly the name of a port (e.g., "COM3")
#' @param wait number of seconds during which the function tries to sync. Default to 10.
#'
#' @return
#' @export
#'
#' @examples
sync_rtc <- function(port="auto",wait=10){

  requireNamespace("serial")

  # find the port to use
  if(port=="auto"){
    if(Sys.info()['sysname']=="Linux"){
      port <- "ttyUSB0"
    } else {
      list_port <- serial::listPorts()
      port <- utils::tail(list_port,n=1) # the last port is assumed to be the right port
    }
  }

  # create USB-TTL connection
  con <- serial::serialConnection(name = "audiolog",
                                  port = port,
                                  mode = "115200,n,8,1",buffering = "none",newline = 0,
                                  translation = "auto")
  # start connection
  open(con)

  # do sync
  not_done <- TRUE

  start_time <- Sys.time()
  stop_time <- start_time+wait

  while(not_done & (Sys.time()<stop_time)){
    serial::write.serialConnection(con,paste0("*",dt <- format(Sys.time(), "%d/%m/%Y - %H:%M:%S"),"*"))
    not_done <- FALSE
  }

  # close connection
  close(con)

  # return
  if(not_done){
    cat("Failed to sync.")
  } else {
    cat(paste0("Successful sync at: ",dt,".
               \n The green LED should be continuously on.
               \n Push switch on (green LED blinks) and then off (No LED light),
               \n and you will be on stand-by mode.
               \n Switch on to start recording. Enjoy. \n"))
  }
}

