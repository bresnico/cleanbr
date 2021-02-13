#' Create plots for each score between time 1 and time 2
#'
#' @param df a dataframe
#' @param date a character object
#' @param var a character vector
#' @importFrom ggplot2 ggplot aes_string geom_jitter stat_summary labs theme aes element_text
#'
#' @return a plot
#' @export
#'
plotting <- function (df, date, var) {
  plot_list = list()

  for (i in seq_along(var)) {
    p <- df %>%
    ggplot() +
    aes_string(x = date, y = var[i]) +
    geom_jitter(size = 5, alpha = .5, width = 0.3) +
    stat_summary(fun = mean, geom = "point", size = 3, shape = 4, color = "red") +
    stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red") +
    labs(title = paste0("Mesure item ", var[i]), y = "Score") +
    theme(plot.title = element_text(hjust = 0.5))
    plot_list[[i]] = p

  }
  plot_list
}
