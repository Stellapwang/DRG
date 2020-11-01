#' Title
#'
#' @param df a dataframe
#' @param funx a function of statistic input
#'
#' @return A table with a table that provides the result of funx
#' @export
#'
#' @examples
#' Medicare_stat (DRG, mean)
#'
#'
medicare_stat <- function (df, funx) {
  stat <- df %>%
    select(`DRG Code`,`DRG Description`,`Average Medicare Payments`) %>%
    group_by(`DRG Code`,`DRG Description`) %>%
    summarise_if(is.numeric, funx) %>%
    rename(Result=`Average Medicare Payments`) %>%
    kable(caption = 'Medicare Payment statistics')
  stat
}
