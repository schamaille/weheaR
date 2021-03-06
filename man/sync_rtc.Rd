% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sync_rtc.R
\name{sync_rtc}
\alias{sync_rtc}
\title{sync_rtc
This functions allows to sync the Real-Time-Clock of the logger with the computer's time.
Plug in the cable, check that the switch is in 'off' position, power the logger.
Then call the function as sync_rtc(). This should most of the time works, but in specific
cases you may have to provide the port name directly (see below).}
\usage{
sync_rtc(port = "auto", wait = 10)
}
\arguments{
\item{port}{character string; either "auto" (default), which will use the last port (windows)
or the ttyUSB0 port (linux), or directly the name of a port (e.g., "COM3")}

\item{wait}{number of seconds during which the function tries to sync. Default to 10.}
}
\value{

}
\description{
sync_rtc
This functions allows to sync the Real-Time-Clock of the logger with the computer's time.
Plug in the cable, check that the switch is in 'off' position, power the logger.
Then call the function as sync_rtc(). This should most of the time works, but in specific
cases you may have to provide the port name directly (see below).
}
