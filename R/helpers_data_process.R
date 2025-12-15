#' Helpers for gaze data processing
#'
#' Collection of utility functions used by the data processing scripts.
#' @keywords internal
NULL

#' Preprocess raw gaze data to absolute screen coordinates and seconds
#'
#' @param data numeric matrix or data.frame with (x, y, t) in columns 1,2,3
#' @param screen_size numeric vector length 2: c(height, width) in pixels
#' @return data.frame with columns x, y, t (seconds, anchored to 0)
gaze_preprocess <- function(data, screen_size) {
  stopifnot(is.numeric(screen_size), length(screen_size) == 2)
  tmp <- as.data.frame(data)
  if (ncol(tmp) < 3) {
    stop("data must have at least 3 columns: x, y, t")
  }

  x <- tmp[[1]] * screen_size[2]
  # original y is often [0,1] top-to-bottom; convert to pixel coordinates
  y <- (1 - tmp[[2]]) * screen_size[1]
  t <- (tmp[[3]] - tmp[[3]][1]) / 1000

  data.frame(x = x, y = y, t = t)
}

#' Remove early time points
#'
#' @param data data.frame with column t (seconds)
#' @param t0 numeric threshold in seconds (default 5)
#' @return filtered data.frame
remove_first_time <- function(data, t0 = 5) {
  stopifnot("t" %in% names(data))
  dplyr::filter(data, t > t0)
}

#' Remove late time points
#'
#' @param data data.frame with column t (seconds)
#' @param t1 numeric threshold in seconds (default 60)
#' @return filtered data.frame
remove_last_time <- function(data, t1 = 60) {
  stopifnot("t" %in% names(data))
  dplyr::filter(data, t < t1)
}

#' Split calibration vs stimuli by time
#'
#' @param data data.frame with column t
#' @param time_sep numeric separator in seconds
#' @return list(calibration = df_calibration, stimuli = df_stimuli)
split_time <- function(data, time_sep) {
  stopifnot("t" %in% names(data))
  list(
    calibration = dplyr::filter(data, t < time_sep),
    stimuli = dplyr::filter(data, t >= time_sep)
  )
}

#' Simple PCA-based clustering for calibration
#'
#' @param data numeric data.frame to perform PCA on
#' @param pca_weights numeric weights for columns (passed to FactoMineR::PCA col.w)
#' @param clust_number integer number of clusters
#' @return data.frame with clustering result (includes `clust`)
gaze_classif <- function(data, pca_weights = c(1, 1, 5), clust_number = 5) {
  res_pca <- FactoMineR::PCA(data, col.w = pca_weights, graph = FALSE)
  res_hcpc <- FactoMineR::HCPC(
    res_pca,
    nb.clust = clust_number,
    consol = TRUE,
    graph = FALSE
  )
  res_hcpc$data.clust
}

#' Compute barycenters and apply simple translation correction
gaze_correct_bary <- function(data) {
  bary_coord <- dplyr::group_by(data, group) |>
    dplyr::summarise(
      mean_x_eye = mean(x_eye),
      mean_y_eye = mean(y_eye),
      .groups = "drop"
    )

  trans_mat <- dplyr::full_join(data, bary_coord, by = "group") |>
    dplyr::mutate(
      x_diff = xvec - mean_x_eye,
      y_diff = yvec - mean_y_eye,
      x_trans = x_eye + x_diff,
      y_trans = y_eye + y_diff
    )

  trans_mat
}

#' Compute distances and weights for linear combination correction
#' @param data_real data.frame with columns xvec, yvec, name
#' @param data_bary data.frame with columns mean_x_eye, mean_y_eye, group
#' @param data_stimuli data.frame with columns x, y
#' @param nb_clust integer
#' @return list: real_pivot, bary_pivot, dist, weights
gaze_dist_weight_df <- function(data_real, data_bary, data_stimuli, nb_clust) {
  stopifnot(all(c("x", "y") %in% names(data_stimuli)))
  nb_line_stimuli <- nrow(data_stimuli)

  # pivot real points and barycenters to wide form
  df_real_dup <- data_real %>%
    dplyr::select(xvec, yvec, name) %>%
    tidyr::pivot_wider(names_from = name, values_from = c(xvec, yvec)) %>%
    dplyr::slice(rep(1:dplyr::n(), each = nb_line_stimuli))

  df_bary_dup <- data_bary %>%
    dplyr::select(mean_x_eye, mean_y_eye, group) %>%
    tidyr::pivot_wider(
      names_from = group,
      values_from = c(mean_x_eye, mean_y_eye)
    ) %>%
    dplyr::slice(rep(1:dplyr::n(), each = nb_line_stimuli))

  dist <- matrix(0, nrow = nb_line_stimuli, ncol = nb_clust)
  for (k in seq_len(nb_clust)) {
    bx <- df_bary_dup[[paste0("mean_x_eye_", k)]]
    by <- df_bary_dup[[paste0("mean_y_eye_", k)]]
    dist[, k] <- sqrt((data_stimuli$x - bx)^2 + (data_stimuli$y - by)^2)
  }
  dist_df <- as.data.frame(dist)
  names(dist_df) <- paste0("dist", seq_len(nb_clust))
  dist_df$sum_dist <- rowSums(dist_df)

  weights <- as.data.frame(sapply(seq_len(nb_clust), function(k) {
    (1 - (dist_df[[k]] / dist_df$sum_dist))
  }))
  names(weights) <- paste0("weight", seq_len(nb_clust))

  list(
    real_pivot = df_real_dup,
    bary_pivot = df_bary_dup,
    dist = dist_df,
    weights = weights
  )
}

#' Apply linear combination correction to stimuli points
gaze_stimuli_combi <- function(
  data_stimuli,
  data_real,
  data_bary,
  data_weight,
  nb_clust
) {
  stimuli_correct <- data.frame(
    new_x = data_stimuli$x,
    new_y = data_stimuli$y,
    t = data_stimuli$t
  )
  for (k in seq_len(nb_clust)) {
    rx <- data_real[[paste0("xvec_", k)]]
    ry <- data_real[[paste0("yvec_", k)]]
    bx <- data_bary[[paste0("mean_x_eye_", k)]]
    by <- data_bary[[paste0("mean_y_eye_", k)]]
    w <- data_weight[[paste0("weight", k)]]
    stimuli_correct$new_x <- stimuli_correct$new_x + (rx - bx) * w
    stimuli_correct$new_y <- stimuli_correct$new_y + (ry - by) * w
  }
  stimuli_correct
}
