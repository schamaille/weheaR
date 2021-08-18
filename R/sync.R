#' sync
#'
#' This functions serves to synchronise the wehear logger with the clock of a computer
#' @param port character string; either "auto" (default), which will use the last port (windows)
#' or the ttyUSB0 port (linux), or directly the name of a port (e.g., COM3)
#'
#'
#'
#' @return
#' @export
#'
#' @examples
sync <- function(port="auto"){

  # find the port to use
  requireNamespace("serial")

  if(port=="auto"){
    if(Sys.info()['sysname']=="Linux"){
      port <- "ttyUSB0"
    } else {
      list_port <- listPorts()
      port <- utils::tail(list_port,n=1) # the last port is assumed to be the right port
    }
  }

  # create USB-TTL connection
  con <- serialConnection(name = "wehear",
                                  port = port,
                                  mode = "115200,n,8,1",buffering = "none",newline = 0,
                                  translation = "auto")
  # start connection
  open(con)

  # do sync
  change_time <- Sys.time()+5
  sync_done <- FALSE
  stop_time <- Sys.time()+50

  while(Sys.time() < stop_time){

    # give time to the logger to log stuff
    Sys.sleep(0.1)

    # Display incoming bytes just like in any terminal
    new_out <- read.serialConnection(con,100)
    if(nchar(new_out)>0 & nchar(new_out) < 40){ # the <40 is to prevent printing the first buffer obtained
      cat("\r\n", new_out, "\r\n")
    }

    if(!isTRUE(sync_done) & Sys.time()>change_time){
      write.serialConnection(con,paste0("*",format(Sys.time(), "%d/%m/%Y - %H:%M:%S"),"*"))
      sync_done <- TRUE
    }
  }

  # close connection
  close(con)

}
