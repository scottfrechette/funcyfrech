#' Generate counts on data
#'
#' @name theme_green
#' @param base_size font size
#' @param base_family font
#' @export

# remove box
# remove padding from labels to chart

theme_green <- function (base_size = 11, base_family = "Century Gothic") {
  blue <- "#004F71"
  green <- "#99BF3F"
  lightgreen <- "#C7DC96"
  white <- "#FFFFFF"
  grey <- "#808080" #"grey50"
  lightgrey <- "lightgrey"

  extrafont::loadfonts(quiet = TRUE)
  # extrafont::font_import(pattern = "Century Gothic")

  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(line = element_line(colour = blue, size = 0.5,
                              linetype = 1, lineend = "butt"),
          rect = element_rect(fill = white, colour = blue, size = 0.5, linetype = 1),
          text = element_text(family = base_family, face = "plain",
                              colour = blue, size = base_size,
                              lineheight = 0.9, hjust = 0.5, vjust = 0.5,
                              angle = 0, margin = margin(), debug = FALSE),

          plot.title = element_text(size = rel(1.2),
                                    color = green,
                                    face = "bold",
                                    hjust = 0,
                                    margin = margin(t = 0, r = 0, b = 4,
                                                    l = 0, unit = "pt")),
          plot.subtitle = element_text(size = rel(0.9),
                                       color = grey,
                                       hjust = 0,
                                       margin = margin(t = 0, r = 0, b = 3,
                                                       l = 0, unit = "pt")),
          plot.caption = element_text(size = rel(0.8),
                                      color = grey,
                                      hjust = 1, vjust = 1,
                                      margin = margin(t = base_size / 2)),

          axis.line = element_blank(),
          axis.text = element_text(size = rel(0.8),
                                   color = grey),
          axis.ticks = element_line(color = grey, size = rel(1/3)),
          axis.title = element_text(size = rel(1)),


          panel.background = element_rect(fill = white, color = NA),
          panel.border = element_rect(fill = NA, size = rel(1/2), color = blue),
          panel.grid.major = element_line(color = lightgrey, size = rel(1/3)),
          panel.grid.minor = element_line(color = lightgrey, size = rel(1/3)),
          panel.grid.minor.x = element_blank(),
          panel.spacing = unit(0.75, "cm"),


          legend.key = element_rect(fill = white, color = NA),
          legend.position = "bottom",

          # faceted plots
          strip.background = element_rect(fill = blue, color = blue),
          strip.text = element_text(color = white, size = rel(0.8)),

          complete = TRUE)
}
