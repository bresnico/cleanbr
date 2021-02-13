#' Strict pairing of ids through time 1 and time 2
#'
#' @param df a dataframe
#' @param id a column name
#' @param date a column name
#' @return a dataframe
#' @export
#'
#'
pairing <- function (df, id, date) {

df_comp <- df %>%
  drop_na(id) %>% #par sécurité
  arrange(id) %>% #visuel
  group_by(id, date) %>%
  count(id) %>%
  filter(n==1) %>% #On a pas fini. On s'est assuré que chaque id est unique dans chaque modalité de temps. On doit encore être sûrs qu'on a maintenant exactement une paire (t1,t2).
  ungroup() %>%
  group_by(id) %>%
  count(id) %>%
  filter(n==2) %>% #on ne garde que les paires de id qui se retrouvent dans t1 et t2. C'est notre grosse perte de données de ce traitement
  ungroup()

df_paired <- df %>%
  filter(id %in% df_comp$id)

}
