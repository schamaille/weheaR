#' convert_to_pairs
#'
#' This function reformats a data.frame containing inertial data so that values for pairs of axes
#' are more easily accessible. Mostly for internal use in other functions.
#' @param df data.frame containing the data. Needs to have x,y, and z columns
#'
#' @return a data.frame with columns axis1, axis2 and pair (e.g. "x-y"). axis1 is the first of the two axes
#' of the pair, axis2 the second
#' @export
#'
#' @examples
convert_to_pairs <- function(df){

  requireNamespace("dplyr")

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
