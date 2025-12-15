# Helpers for generating heatmaps

#' Generate a heatmap (small) and save to PNG
#'
heatmap_generator <- function(
  data,
  path_img = "experience/cockpit_utile/112.png",
  file_name,
  width_size = 160,
  height_size = 90,
  add_img = TRUE,
  only_img = FALSE,
  transparency_img = 0.8
) {
  img <- png::readPNG(path_img)
  img <- grid::rasterGrob(img, interpolate = TRUE)

  p <- NULL
  if (add_img && !only_img) {
    p <- ggplot2::ggplot(data, ggplot2::aes(x = new_x, y = new_y)) +
      ggplot2::theme_void() +
      ggplot2::annotation_custom(img, xmin = 0, xmax = 16, ymin = 0, ymax = 9) +
      ggplot2::stat_density_2d(
        ggplot2::aes(fill = ..density..),
        geom = "raster",
        contour = FALSE,
        alpha = transparency_img
      ) +
      ggplot2::scale_fill_distiller(palette = "Spectral", direction = -1) +
      ggplot2::coord_fixed(ratio = 1, xlim = c(0, 16), ylim = c(0, 9)) +
      ggplot2::theme(
        legend.position = "none",
        axis.title.x = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank()
      )
  } else if (only_img && add_img) {
    p <- ggplot2::ggplot(data, ggplot2::aes(x = x, y = y)) +
      ggplot2::theme_void() +
      ggplot2::annotation_custom(img, xmin = 0, xmax = 16, ymin = 0, ymax = 9) +
      ggplot2::coord_fixed(ratio = 1, xlim = c(0, 16), ylim = c(0, 9)) +
      ggplot2::theme(
        legend.position = "none",
        axis.title.x = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank()
      )
  } else {
    p <- ggplot2::ggplot(data, ggplot2::aes(x = new_x, y = new_y)) +
      ggplot2::theme_void() +
      ggplot2::stat_density_2d(
        ggplot2::aes(fill = ..density..),
        geom = "raster",
        contour = FALSE,
        alpha = transparency_img
      ) +
      ggplot2::scale_fill_distiller(palette = "Spectral", direction = -1) +
      ggplot2::coord_fixed(ratio = 1, xlim = c(0, 16), ylim = c(0, 9)) +
      ggplot2::theme(
        legend.position = "none",
        axis.title.x = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank()
      )
  }

  png(filename = file_name, width = width_size, height = height_size)
  print(p)
  dev.off()
}


#' Larger heatmap generator
heatmap_generator_bigger <- function(
  data,
  path_img = "experience/cockpit_utile/112.png",
  file_name,
  width_size = 160,
  height_size = 90,
  transparency_img = 0.8,
  title = NULL
) {
  img <- png::readPNG(path_img)
  img <- grid::rasterGrob(img, interpolate = TRUE)

  p <- ggplot2::ggplot(data, ggplot2::aes(x = new_x, y = new_y)) +
    ggplot2::annotation_custom(img, xmin = 0, xmax = 16, ymin = 0, ymax = 9) +
    ggplot2::stat_density_2d(
      ggplot2::aes(fill = ..density..),
      geom = "raster",
      contour = FALSE,
      alpha = transparency_img
    ) +
    ggplot2::scale_fill_distiller(palette = "Spectral", direction = -1) +
    ggplot2::coord_fixed(ratio = 1, xlim = c(-8, 24), ylim = c(-4.5, 13.5)) +
    ggplot2::ggtitle(title)

  png(filename = file_name, width = width_size, height = height_size)
  print(p)
  dev.off()
}


#' Run the project-wide double loop (compatibility wrapper)
#' This wraps the project `double_loop` behavior but takes parameters.
generate_heatmaps_double_loop <- function(
  method = "heatmap_corrected",
  width_size = 640,
  height_size = 360
) {
  # Keep original behaviour: load necessary files
  if (!file.exists("data/df_all.RData")) {
    stop("data/df_all.RData is required")
  }
  load("data/df_all.RData")
  time_user_exp <- utils::read.csv(
    "data/time_user_exp.csv",
    sep = ",",
    header = TRUE
  )

  # delegate to existing double_loop function from script if present; source legacy file if needed
  if (!exists("double_loop")) {
    legacy <- "script/2_heatmap/double_looper.R"
    if (file.exists(legacy)) source(legacy)
  }

  if (exists("double_loop")) {
    double_loop(
      width_size = width_size,
      height_size = height_size,
      method = method
    )
  } else {
    stop(
      "double_loop function not found. Please ensure legacy scripts are available or use the smaller generators."
    )
  }
}
