# color/colour, fill, etc
#' Color ggplot figures with Binning Singleton colors
#'
#' @param type Character string for type of data to color. Must be one of 'discrete' or 'continuous'
#' @param aesthetic Character string for type of aesthetic to color. Must be one of 'color', 'colour', 'fill'
#' @param ... Addition parameters fed to ggplot
#' @param na.value Character string for color of missing data. Default is grey50
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(x=mpg,y=wt)) +
#'     geom_point(aes(color=as.character(hp)), size=3) +
#'     scale_color_BinningSingletons(type = "discrete", aesthetic = "colour")
#'
#' ggplot(mtcars,  aes(x=mpg,y=wt)) +
#'     ggplot2::geom_point(ggplot2::aes(fill=as.character(cyl)), size=3, pch=21) +
#'     scale_color_BinningSingletons(type = "discrete", aesthetic = "fill")
#'
#' ggplot(mtcars,  aes(x=mpg,y=wt)) +
#'     ggplot2::geom_point(ggplot2::aes(fill=cyl), size=3, pch=21) +
#'     scale_color_BinningSingletons(type = "continuous", aesthetic = "fill")

scale_color_BinningSingletons <- function(type, aesthetic=NULL,
                                          ...,
                                          na.value = "grey50"){
  #Define colors
  BS_colors <- c("#E37978", "#EFEC57", "#AB73B1", "#4ABA6C")

  #Error messages for incorrect inputs
  if(!(type %in% c("discrete", "continuous"))){
    stop("type must be either 'discrete' or 'continuous'")
  }
  if(!(aesthetic %in% c("color", "colour", "fill"))){
    stop("aesthetic must be either 'color/colour' or 'fill'")
  }

  #For discrete variables
  if(type == "discrete"){

    pal <- function(n) {
      #If more than 4 groups provides, color ramp to get more colors
      if (n > 4) {
        values <- grDevices::colorRampPalette(BS_colors)(n)
      } else{
        #Else use original 4 colors
        values <- BS_colors[1:n]
      }
    }

    ggplot2::discrete_scale(aesthetic, "manual",
                            palette = pal,
                            na.value = na.value, ...)
  }

  #For discrete variables
  if(type == "continuous"){

    ggplot2::continuous_scale(aesthetic, "gradientn",
                              palette = scales::gradient_n_pal(BS_colors),
                              na.value = na.value, guide = "colourbar", ...)
  }

}

#' @rdname scale_color_BinningSingletons
#' @export
scale_colour_BinningSingletons <- scale_color_BinningSingletons
# color/colour, fill, etc
#' Color ggplot figures with Binning Singleton colors
#'
#' @param type Character string for type of data to color. Must be one of 'discrete' or 'continuous'
#' @param aesthetic Character string for type of aesthetic to color. Must be one of 'color', 'colour', 'fill'
#' @param ... Addition parameters fed to ggplot
#' @param na.value Character string for color of missing data. Default is grey50
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' #Discrete color
#' ggplot(mtcars, aes(x=mpg,y=wt)) +
#'     geom_point(aes(color=as.character(cyl)), size=3) +
#'     scale_color_BinningSingletons(type = "discrete", aesthetic = "color")
#'
#' #Discrete fill
#' ggplot(mtcars,  aes(x=mpg,y=wt)) +
#'     ggplot2::geom_point(ggplot2::aes(fill=as.character(hp)), size=3, pch=21) +
#'     scale_color_BinningSingletons(type = "discrete", aesthetic = "fill")
#'
#' #Continuous color
#' ggplot(mtcars,  aes(x=mpg,y=wt)) +
#'     ggplot2::geom_point(ggplot2::aes(color=cyl), size=3) +
#'     scale_color_BinningSingletons(type = "continuous", aesthetic = "color")
#'
#' #Continuous fill
#' ggplot(mtcars,  aes(x=mpg,y=wt)) +
#'     ggplot2::geom_point(ggplot2::aes(fill=hp), size=3, pch=21) +
#'     scale_color_BinningSingletons(type = "continuous", aesthetic = "fill")

scale_color_BinningSingletons <- function(type, aesthetic=NULL,
                                          ...,
                                          na.value = "grey50"){
  #Define colors
  BS_colors <- c("#E37978", "#EFEC57", "#AB73B1", "#4ABA6C")

  #Error messages for incorrect inputs
  if(!(type %in% c("discrete", "continuous"))){
    stop("type must be either 'discrete' or 'continuous'")
  }
  if(!(aesthetic %in% c("color", "colour", "fill"))){
    stop("aesthetic must be either 'color/colour' or 'fill'")
  }

  #For discrete variables
  if(type == "discrete"){

    pal <- function(n) {
      #If more than 4 groups provides, color ramp to get more colors
      if (n > 4) {
        values <- grDevices::colorRampPalette(BS_colors)(n)
      } else{
        #Else use original 4 colors
        values <- BS_colors[1:n]
      }
      values
    }


    ggplot2::discrete_scale(aesthetic, "manual",
                            palette = pal,
                            na.value = na.value, ...)
  } else if(type == "continuous"){
    #For discrete variables
    ggplot2::continuous_scale(aesthetic, "gradientn",
                              palette = scales::gradient_n_pal(BS_colors),
                              na.value = na.value, guide = "colourbar", ...)
  }

}

#' @rdname scale_color_BinningSingletons
#' @export
scale_colour_BinningSingletons <- scale_color_BinningSingletons
