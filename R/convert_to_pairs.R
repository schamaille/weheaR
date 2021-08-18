#' convert_to_pairs
#'
#' This function reformats a data.frame containing inertial data so that values for pairs of axes
#' are more easily accessible. Mostly for internal use in other functions.
#' @param df data.frame containing the data. Needs to have x,y, and z columns
#'
#' @return
#' @export
#'
#' @examples
convert_to_pairs <- function(df){

  requireNamespace(dplyr)

  df1 <- df %>%
    mutate(pair="x-y") %>%
    rename(axis1=x,axis2=y) %>%
    select(-z)

  df2 <- df %>%
    mutate(pair="x-z") %>%
    rename(axis1=x,axis2=z) %>%
    select(-y)

  df3 <- df %>%
    mutate(pair="y-z") %>%
    rename(axis1=y,axis2=z) %>%
    select(-x)

  out <- bind_rows(df1,df2,df3)
  return(out)

}
