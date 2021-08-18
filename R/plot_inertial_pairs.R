#' plot_inertial_pairs
#'
#' This function plot the values of inertial data across pairs of axes.
#' Useful for magnetometer calibration in particular.
#' @param df data.frame containing the data. Needs to have x,y, and z columns
#' @param title character string; title to be added to the plot
#' @param radius numeric; value of the radius of the circle to be drawn. If NULL (default), no circle is drawn
#' @param lim numeric; limit of the plot (in absolute value, the plot has an aspect ratio of 1);
#' If NULL (default), the choice is left to the ggplot function and all data are shown
#'
#' @return
#' @export
#'
#' @examples
#' @import ggplot2
plot_inertial_pairs <- function(df,title="",radius=NULL,lim=NULL){

  #requireNamespace("ggplot")

  foo <- convert_to_pairs(df)

  if(is.null(radius)){
    ggplot(foo,aes(axis1,axis2,color=pair))+
      geom_point()+
      labs(title=title)+
      theme(aspect.ratio=1,
            panel.background = element_blank())
  } else {

    circleFun <- function(r=radius, npoints = 100){
      tt <- seq(0,2*pi,length.out = npoints)
      xx <- r * cos(tt)
      yy <- r * sin(tt)
      return(data.frame(x = xx, y = yy))
    }

    if(!is.null(lim)){
      lim <- max(c(max(abs(df)),lim))
    } else {lim <- max(abs(df))}
    ggplot(foo,aes(axis1,axis2,color=pair))+
      geom_point()+
      geom_path(data=circleFun(r=radius),aes(x,y),color="black",linetype="dashed")+
      xlim(-lim,lim)+ylim(-lim,lim)+
      labs(title=title)+
      theme(aspect.ratio=1,
            panel.background = element_blank())
  }
}



