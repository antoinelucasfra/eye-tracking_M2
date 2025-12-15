""
"
Wrap main data processing into a function-friendly interface.

Usage:
  source('script/1_data_processing/main_data_process.R')
  result <- process_consumer(consumers_name = '1-lucien',
                             screen_size = c(1080, 1920),
                             start_time_vec = matrix(5, nrow = 1, ncol = 10),
                             end_time_vec = matrix(60, nrow = 1, ncol = 10),
                             square_pos_path = 'experience/25_square_position.csv')
"
""

process_consumer <- function(
  consumers_name,
  screen_size = c(1080, 1920),
  start_time_vec,
  end_time_vec,
  square_pos_path = 'experience/25_square_position.csv'
) {
  square_pos <- read.csv(
    square_pos_path,
    sep = ";",
    header = TRUE,
    row.names = 1,
    dec = ".",
    colClasses = c("col" = "factor", "xvec" = "numeric", "yvec" = "numeric")
  )
  square_pos$name <- rownames(square_pos)

  area_number <- nrow(square_pos)

  consumers_path <- file.path('data/gazedata', consumers_name)
  list_stimuli <- list.files(consumers_path, full.names = FALSE)

  df <- data.frame()
  colnames_out <- c("x", "y", "t", "stimu", "consu", "rank")

  for (i in seq_along(list_stimuli)) {
    path_stimuli <- file.path(
      consumers_path,
      list_stimuli[i],
      "ScreenRecorderPath.dat"
    )
    temp_txt <- utils::read.table(path_stimuli, skip = 1)
    temp_txt <- gaze_preprocess(temp_txt, screen_size = screen_size)

    df_temp <- cbind(
      temp_txt[,],
      as.factor(list_stimuli[i]),
      as.factor(consumers_name),
      as.factor(gsub("([0-9]+).*$", "\\1", list_stimuli[i]))
    )
    colnames(df_temp) <- colnames_out

    df <- dplyr::bind_rows(df, df_temp)
  }

  stimu_lvl <- levels(df$stimu)

  correction <- df %>%
    dplyr::filter(stimu == "0_correction") %>%
    dplyr::select(x, y, t)
  correction <- remove_first_time(correction, t0 = start_time_vec[1, 1])
  correction <- remove_last_time(correction, t1 = end_time_vec[1, 1])

  data_class <- gaze_classif(
    correction,
    pca_weights = c(1, 1, 50),
    clust_number = area_number
  )
  df_join <- dplyr::full_join(
    data_class,
    square_pos,
    by = c("clust" = "name")
  ) %>%
    dplyr::rename(x_eye = x, y_eye = y, group = clust)

  df_trans <- gaze_correct_bary(df_join)
  df_bary <- unique(df_trans[, c(
    "group",
    "mean_x_eye",
    "mean_y_eye",
    "xvec",
    "yvec"
  )])

  df_corrected <- data.frame()

  for (k in seq(2, length(stimu_lvl))) {
    stimuli_data <- df %>% dplyr::filter(stimu == stimu_lvl[k])
    stimuli_data <- remove_first_time(stimuli_data, t0 = start_time_vec[1, k])
    stimuli_data <- remove_last_time(stimuli_data, t1 = end_time_vec[1, k])

    dist_weight <- gaze_dist_weight_df(
      square_pos,
      df_bary,
      stimuli_data,
      nb_clust = area_number
    )
    real_pivot <- dist_weight$real_pivot
    bary_pivot <- dist_weight$bary_pivot
    weight <- dist_weight$weights

    stimuli_correct <- gaze_stimuli_combi(
      stimuli_data,
      real_pivot,
      bary_pivot,
      weight,
      nb_clust = area_number
    )
    stimuli_correct$stimu <- stimu_lvl[k]
    df_corrected <- dplyr::bind_rows(df_corrected, stimuli_correct)
  }

  list(df = df, df_corrected = df_corrected)
}
