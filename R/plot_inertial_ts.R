#' plot_inertial_ts
#'
#' This function plots the time-series of inertial data
#' @param df data.frame containing the data. Needs to have x,y, and z columns
#' @param title character string; title to be added to the plot
#'
#' @return
#' @export
#'
#' @examples
#' @import ggplot2
plot_inertial_ts <- function(df,title=""){

  #requireNamespace("tidyr")
  #requireNamespace("ggplot2")

  dfl <- tidyr::pivot_longer(df,cols=c("x","y","z"),names_to="axis")

  ggplot(dfl,aes(time,value,color=axis))+
    geom_line()+
    labs(x="time",title = title)+
    theme_bw()

}
