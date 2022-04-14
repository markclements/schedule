#' prepare_data 
#'
#' @description A function to parse the excel file and transform into a tidy datatable
#'
#' @return a tidy tibble/dataframe
#'
#' @noRd
#' 

prepare_data <- function(df) {
    df |>
        dplyr::rename_all(~ stringr::str_to_lower(.)) |>
        dplyr::select(
            course,
            title,
            times,
            site,
            dplyr::ends_with("day"),
            room,
            instructor
        ) |>
        dplyr::mutate(
            on_day = dplyr::case_when(
                site == "ON" ~ "ON",
                TRUE ~ NA_character_
            )
        ) |>
        dplyr::mutate(
            stime = stringr::str_extract(times, "^\\d{1,2}:\\d{2}\\s*\\w{2}"),
            etime = stringr::str_extract(times, "\\d{1,2}:\\d{2}\\s*\\w{2}$")
        ) |>
        dplyr::mutate(
            stime = lubridate::parse_date_time(stime, "%I:%M %p"),
            etime = lubridate::parse_date_time(etime, "%I:%M %p")
        ) |>
        dplyr::mutate(
            stime = lubridate::hour(stime) + lubridate::minute(stime) / 60,
            etime = lubridate::hour(etime) + lubridate::minute(etime) / 60
        ) |>
        tidyr::gather(
            day_of_week,
            day,
            dplyr::ends_with("day"),
            na.rm = T
        ) |>
        dplyr::select(
            -day_of_week,
            -times
        ) |>
        dplyr::mutate(id = 1:dplyr::n(), course_id = stringr::str_c(course, id, sep = "_")) |>
        dplyr::arrange(course, day) |>
        tibble::rowid_to_column() |>
        dplyr::rename(campus = site) |>
        dplyr::mutate(campus = dplyr::case_when(
            campus == "H" ~ "Haverhill",
            campus == "L" ~ "Lawrence",
            TRUE ~ "Other"
        )) |>
        dplyr::ungroup() |>
        dplyr::mutate(
            day = forcats::fct_relevel(
                day, c("M", "T", "W", "R", "F", "S")
            ),
            daynum = as.integer(day)
        ) -> df ### output to app ###
    return(df)
}
