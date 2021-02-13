#' Automatically create scores for many categories of items
#'
#' @param df a dataframe
#' @param x a character vector
#'
#' @return a dataframe
#' @export
#'
#' @examples
#' d_conv <- data.frame(id=c("josette", "paul", "louis"), be8_1=c(1,2,5), be8_2=c(2,3,5),be8_3=c(1,5,8))
#' scoring(d_conv,c("be8","emo4"))
scoring <- function (df, x) {
  temp_name <- vector("character", length = 1)

  for (i in seq_along(x)) {

    temp_name <- paste0(x[[i]],"_sco")

    df <- df %>%
      mutate("{temp_name}" := rowMeans(select(df,starts_with(x[[i]])), na.rm = T))
  }
  df
}
