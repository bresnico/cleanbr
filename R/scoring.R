#' Automatically create scores for many categories of items
#'
#' @param df a dataframe
#' @param x a character vector
#' @importFrom dplyr select starts_with
#' @return a dataframe
#' @export
#'
scoring <- function (df, x) {
  temp_name <- vector("character", length = 1)

  for (i in seq_along(x)) {

    temp_name <- paste0(x[[i]],"_sco")

    df <- df %>%
      dplyr::mutate("{temp_name}" := rowMeans(select(df,starts_with(x[[i]])), na.rm = T))
  }
  df
}
