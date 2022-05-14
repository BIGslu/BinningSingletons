#' Apply theme (similar to theme_classic) and add Binning Singletons logo to ggplot
#'
#' @param base_size Numeric base font size. Default is 11
#' @param logo_x Numeric x-axis start location for logo. Used in rasterGrob( ) as normalised parent coordinates (npc)
#' @param logo_y Numeric y-axis start location for logo. Used in rasterGrob( ) as normalised parent coordinates (npc)
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(mtcars, aes(x=mpg,y=wt)) +
#'     geom_point(aes(color=as.character(cyl)), size=3)  +
#'     theme_BinningSingletons(logo_x = 0.5, logo_y = 0.1)
#'
#' ggplot(mtcars, aes(x=disp,y=drat)) +
#'     geom_point(aes(color=as.character(cyl)), size=3) +
#'     scale_color_BinningSingletons(type = "discrete", aesthetic = "color") +
#'     theme_BinningSingletons()

theme_BinningSingletons <- function(base_size = 11,
                                    logo_x = 1, logo_y = 1) {
  #Get logo
  img_url <- "https://binningsingletons.com/wp-content/uploads/cropped-BinningSingletons-Logo-Horiz_RGB.png"
  img <- magick::image_read(img_url)
  #Format image and placement
  img_format <- grid::rasterGrob(img, interpolate = TRUE,
                                 width = grid::unit(0.5,"npc"),
                                 x = grid::unit(logo_x,"npc"),
                                 y = grid::unit(logo_y,"npc"),
                                 hjust = 1, vjust = 1)

  list(
    #Add logo to plot
    ggplot2::annotation_custom(grob = img_format),
    #format theme
    ggplot2::theme_classic(base_size = 11,
                           base_family = "",
                           base_line_size = base_size/22,
                           base_rect_size = base_size/22)
  )
}
