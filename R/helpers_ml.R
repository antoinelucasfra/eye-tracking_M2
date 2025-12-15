#' Helpers for ML dataset preparation
#'
loader_img <- function(
  path_abs = "data/inputs_ML/",
  method_name,
  consumers_data,
  height_size = 360,
  width_size = 640
) {
  col <- colnames(consumers_data)

  consumers_data$nom <- paste0(consumers_data$ordre, "-", consumers_data$nom)
  time_user <- consumers_data %>%
    dplyr::select(nom, dplyr::starts_with("stimu"), dplyr::ends_with("liking"))

  colnames(time_user)[grepl(
    "stimu",
    colnames(time_user),
    fixed = TRUE
  )] <- paste0(
    colnames(time_user)[grepl("stimu", colnames(time_user), fixed = TRUE)],
    "_label"
  )
  colnames(time_user) <- stringr::str_replace_all(
    colnames(time_user),
    "X",
    "stimu"
  )

  time_user <- as.data.frame(apply(
    X = time_user,
    FUN = as.character,
    MARGIN = 2
  ))

  time_user <- time_user %>%
    tidyr::pivot_longer(
      cols = -nom,
      names_to = c("stimu", ".value"),
      names_sep = "_",
      values_ptypes = list(.value = character())
    )

  img_path <- list.files(file.path(path_abs, method_name), full.names = TRUE)
  img_path_short <- gsub(
    ".png",
    "",
    list.files(file.path(path_abs, method_name), full.names = FALSE)
  )

  img <- lapply(img_path, png::readPNG)
  heat_img <- list()
  heat_img$x <- array(0, c(length(img), height_size, width_size, 3))
  for (k in seq_along(img)) {
    heat_img$x[k, , , ] <- img[[k]][,,]
  }

  time_user$consu_id <- paste0(time_user$nom, "_", time_user$label)
  consumers_data_ordered <- time_user %>%
    dplyr::filter(consu_id %in% img_path_short) %>%
    dplyr::select(consu_id, liking)
  consumers_data_final <- consumers_data_ordered[
    match(img_path_short, consumers_data_ordered$consu_id),
  ]

  heat_img$y <- as.factor(consumers_data_final$liking)
  heat_img$y <- as.integer(dplyr::recode(
    heat_img$y,
    "aime" = "1",
    "aime_pas" = "2"
  ))
  heat_img$img_path_short <- img_path_short

  heat_img
}
