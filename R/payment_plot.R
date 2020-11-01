#' DRG_payment_plot function
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
#'Use the function by calling DRG dataframe, and input variable with the data from column 'Average Total Payments'
#'
#' DRG_payment_plot (DRG, 'Average Total Payments')
#'
payment_plot <-
  function (df, x) {
    # create a function that takes input from a dataframe and a variable x
    payment <- df %>%
      rename (type = x) %>% # rename x to 'type'
      ggplot(aes(
        x = reorder(`DRG Code`,-type),
        y = type,
        color = `DRG Code`
      )) + # make a ggplot with x-axis by DRG code (ordered by type), y-axis is the variable input
      geom_boxplot() + # make a boxplot
      xlab ('DRG Code') + # add x-label 'DRG code'
      ylab (x) + # add y-label and default to the variable input
      ggtitle (paste0(x, ' by DRG Code')) + # add title, which will change depend on input
      theme_light() + # use a light theme
      theme(legend.position = 'none',
            # remove legend title
            axis.text.x = element_text(angle = 90)) # make the x-axis tickmark rotate 90 degree
    payment
  }
