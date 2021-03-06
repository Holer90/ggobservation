
# Title     : geom_observation.R
# Objective : Create geom_observation, which adds the number of observations to each group.
# Created by: tho
# Created on: 14/9/2021

#library(tidyverse)
#library(grid)

library(ggplot2)
library(dplyr)

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatObservationByPanel <- ggproto("StatObservationByPanel", Stat,
  extra_params = c("na.rm", "prefix", "suffix", "separation_factor"),
  compute_panel = function(data, scales) {
    data_persist_in_stat <<- data
    data %>% 
      tidyr::drop_na(one_of("x", "y")) %>%
      summarize(label=n(), x = max(data$x, na.rm = TRUE), y = max(data$y, na.rm = TRUE)) 
  },
  finish_layer = function(self, data, params) {
    data %>% mutate(label = paste0(!!params$prefix, label, !!params$suffix)) %>%
      mutate(separation_factor = !!params$separation_factor)
  }
)

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatObservationByGroup <- ggproto("StatObservationByGroup", Stat,
  extra_params = c("na.rm", "prefix", "suffix", "separation_factor"),
  compute_group = function(data, scales) {
    data %>% 
      tidyr::drop_na(one_of("x", "y")) %>%
      summarize(label=n(), x = max(data$x, na.rm = TRUE), y = max(data$y, na.rm = TRUE))
  },
  finish_layer = function(self, data, params) {
    data %>% mutate(label = paste0(!!params$prefix, label, !!params$suffix)) %>%
      mutate(separation_factor = !!params$separation_factor)
  }
)

GeomObservation <- ggproto("GeomObservation", GeomText,
  #extra_params = c("na.rm", "prefix", "suffix", "separation_factor"),
  default_aes = aes(
    colour = "black", size = 4, angle = 0, hjust = "left", vjust = "top", 
    alpha = NA, family = "", fontface = 1, lineheight = 1.2, force_position = TRUE
  ),
  draw_panel = function(data, panel_params, coord, parse = FALSE,
                        na.rm = FALSE, check_overlap = FALSE) {
      
    
    #data_persist <<- data
    #panel_params_persist <<- panel_params
    #coord_persist <<- coord

    # Convert hjust and vjust to numeric if character
    if (is.character(data$hjust)) {
      data$hjust <- compute_just(data$hjust, data$x)
    }
    if (is.character(data$vjust)) {
     data$vjust <- compute_just(data$vjust, data$y)
    }

    # Move the label to forced position.
    if (data$force_position[[1]]) {
      x_min <- panel_params$x$limits[[1]]
      x_max <- panel_params$x$limits[[2]]
      data$x <- x_min + data$hjust * (x_max - x_min)


      y_min <- panel_params$y$limits[[1]]
      y_max <- panel_params$y$limits[[2]]
      data$y<- y_min + data$vjust * (y_max - y_min)

      # Move the annotations if there are multiple
      if (nrow(data) > 1) {
        y_range <- panel_params$y$limits[[2]] - panel_params$y$limits[[1]]
        if (data$vjust[[1]] < 0.5) {
          data <- data %>% mutate(y = y +  y_range * 0.01 * size * separation_factor * seq(0, n() - 1))
        } else {
          data <- data %>% mutate(y = y +  y_range * 0.01 * size * separation_factor * seq(0, 1 - n()))

        }
      }
    }

    
    lab <- data$label
    if (parse) {
      lab <- parse_safe(as.character(lab))
    }
    
    data <- coord$transform(data, panel_params)
    
    
    grid::textGrob(
      lab,
      data$x, data$y, default.units = "native",
      hjust = data$hjust, vjust = data$vjust,
      rot = data$angle,
      gp = grid::gpar(
        col = alpha(data$colour, data$alpha),
        fontsize = data$size * .pt,
        fontfamily = data$family,
        fontface = data$fontface,
        lineheight = data$lineheight
      ),
      check.overlap = check_overlap
    )
  }
)

#' Observation
#' 
#' Adds the number of observations
#' 
#' @param computer by group or by panel
#' @export
geom_observation <- function(mapping = NULL, data = NULL, position = "identity", 
                             na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, #...) {
                             ..., 
                             prefix = "n = ", 
                             suffix = "",
                             compute = "group",
                             separation_factor = 1) {
  if (compute == "group") {
    layer(
      stat = StatObservationByGroup, geom = GeomObservation, data = data, mapping = mapping,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, 
                    prefix = prefix,
                    suffix = suffix,
                    separation_factor = separation_factor,                    ... )
    ) 
  } else {
    layer(
      stat = StatObservationByPanel, geom = GeomObservation, data = data, mapping = mapping,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, 
                    prefix = prefix,
                    suffix = suffix,
                    separation_factor = separation_factor,
                    ... )
    ) 
  }
}




# # plots without observation
# p <- mpg %>%
#   ggplot(aes(x = cty, y = hwy, colour = fl)) +
#   geom_point() +
#   facet_grid(drv ~ factor(year))
# p

# # plots with stat_count_observations
# p + geom_observation(separation_factor = 2)
# p + geom_observation(compute = "panel")
# p + geom_observation(vjust="center", hjust="right", separation_factor = 2)
# p + geom_observation(prefix = "pre_", suffix = "_suf", separation_factor = 2, vjust = "top", hjust = "middle")

# TODO: add "label" box functionaly

# mpg %>%
#   ggplot(aes(x = cty, y = hwy)) +
#   geom_point() +
#   geom_observation(compute = "panel", vjust="center") 

# mpg %>%
#   ggplot(aes(x = cty, y = hwy, colour= manufacturer)) +
#   geom_point() +
#   geom_observation(compute = "group")


# mpg %>%
#   ggplot(aes(x = cty, y = hwy, colour= manufacturer)) +
#   geom_point() +
#   geom_observation(compute = "panel")



# diamonds %>%
#   #filter(price > 15000) %>%
#   ggplot(aes(carat, price)) +
#   #geom_point() + 
#   geom_violin() +
#   geom_point() +
#   geom_observation(hjust="left") +
#   facet_grid(cut ~ clarity)
  


#### HELPER FUNCTIONS ####
compute_just <- function(just, a, b = a, angle = 0) {
  #  As justification direction is relative to the text, not the plotting area
  #  we need to swap x and y if text direction is rotated so that hjust is
  #  applied along y and vjust along x.
  if (any(grepl("outward|inward", just))) {
    # ensure all angles are in -360...+360
    angle <- angle %% 360
    # ensure correct behaviour for angles in -360...+360
    angle <- ifelse(angle > 180, angle - 360, angle)
    angle <- ifelse(angle < -180, angle + 360, angle)
    rotated_forward <-
      grepl("outward|inward", just) & (angle > 45 & angle < 135)
    rotated_backwards <-
      grepl("outward|inward", just) & (angle < -45 & angle > -135)
    
    ab <- ifelse(rotated_forward | rotated_backwards, b, a)
    just_swap <- rotated_backwards | abs(angle) > 135
    inward <-
      (just == "inward" & !just_swap | just == "outward" & just_swap)
    just[inward] <- c("left", "middle", "right")[just_dir(ab[inward])]
    outward <-
      (just == "outward" & !just_swap) | (just == "inward" & just_swap)
    just[outward] <- c("right", "middle", "left")[just_dir(ab[outward])]
    
  }
  
  unname(c(left = 0, center = 0.5, right = 1,
           bottom = 0, middle = 0.5, top = 1)[just])
}

just_dir <- function(x, tol = 0.001) {
  out <- rep(2L, length(x))
  out[x < 0.5 - tol] <- 1L
  out[x > 0.5 + tol] <- 3L
  out
}
