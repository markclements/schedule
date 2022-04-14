#' prepare_plot_data 
#'
#' @description functions to layout the data to plot as a schedule
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

place_events <- function(d) {
    d <- dplyr::mutate(
        d,
        col = 0
    )
    for (i in seq_along(d$col)) {
        if (i == 1) {
            d$col[i] <- 1
            next
        }
        placed <- FALSE
        for (j in 1:max(d$col)) {
            collide <- d$stime[i] < dplyr::last(d$etime[d$col == j])
            if (!collide) {
                d$col[i] <- j
                placed <- TRUE
                break
            }
        }
        if (!placed) d$col[i] <- j + 1
    }
    return(d)
}

prepare_plot_data <- function(df) {
    df |>
        dplyr::group_by(
            day,
            campus
        ) |>
        dplyr::arrange(
            day,
            stime,
            etime
        ) |>
        dplyr::mutate(indx = c(0, cumsum(dplyr::lead(stime) >=
            cummax(etime))[-dplyr::n()])) |>
        dplyr::group_by(
            day,
            indx,
            campus
        ) |>
        tidyr::nest() |>
        dplyr::mutate(
            data = purrr::map(
                data, ~ place_events(.)
            )
        ) |>
        tidyr::unnest(data) |>
        dplyr::mutate(
            col = col - 0.9
        ) |>
        dplyr::mutate(
            colr = col + 0.9
        ) |>
        dplyr::mutate(
            x1 = (col / (max(col) + 1)) + daynum,
            x2 = (colr / (max(col) + 1)) + daynum
        ) |>
        dplyr::ungroup() -> df

    return(df)
}