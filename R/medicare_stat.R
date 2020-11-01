#' Title
#'
#' @param df a dataframe
#' @param funx a function of statistic input
#'
#' @return A table with a table that provides the result of funx
#' @export
#'
#' @examples
#'
#' Use this function by calling DRG dataframe and input varialbe for a function mean
#' Medicare_stat (DRG, mean)
#'
#'
medicare_stat <-
  function (df, funx) {
    # a functioin that takes dataframe (df) and a function (funx) as argument
    stat <- df %>%
      select(`DRG Code`, `DRG Description`, `Average Medicare Payments`) %>% # from DRG datafile, take 3 columns
      group_by(`DRG Code`, `DRG Description`) %>% # group by DRG code and DRG description
      summarise_if(is.numeric, funx) %>% # summarize the Average Medicare Payments column
      rename(Result = `Average Medicare Payments`) %>% # rename the column as 'Result' as this column is now the result of function input
      kable(caption = 'Medicare Payment statistics') # make a table for the result
    stat
  }
