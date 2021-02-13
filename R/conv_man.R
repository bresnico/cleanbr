
#' Reverse scale of selected items using the key provided. Change existing items.
#'
#' @param df a dataframe
#' @param x a character object
#' @param items a character vector
#' @param key an integer object
#'
#' @return a dataframe
#' @import magrittr
#' @import dplyr
#' @export
#'
#' @examples
#' d <- data.frame(id=c("josette", "paul", "louis"), be8_1=c(1,2,5), be8_2=c(2,3,5),be8_3=c(1,5,8))
#' conv_man(d,"be8",c(1,2),8)
conv_man <- function (df, x, items, key) {
  for (i in seq_along(items)) {
    temp_item <- paste0(x,"_",items[[i]])
    df <- df %>%
      mutate("{temp_item}" := key - pull(df,temp_item))
  }
  df
}
