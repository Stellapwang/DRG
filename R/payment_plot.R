#' Title
#'
#' @param df a dataframe
#' @param x a column name with type of payment from the df
#'
#' @return A boxplot with DRG code at x-axis and type of payment at y-axis, colored by various DRG code
#' @export
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_boxplot
#'
#' @examples
#'
#' DRG_payment_plot (DRG, 'Average Total Payments')
#'
payment_plot <- function (df,x) {
  payment <- df %>%
    rename (type=x) %>%
    ggplot(aes(x= reorder(`DRG Code`, -type), y=type, color=`DRG Code`))+
    geom_boxplot()+
    xlab ('DRG Code')+
    ylab (x)+
    ggtitle (paste0(x, ' by DRG Code'))+
    theme_light()+
    theme(legend.position= 'none',
          axis.text.x = element_text(angle = 90))
  payment
}
