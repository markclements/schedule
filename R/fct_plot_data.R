#' plot_data 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

plot_schedule <- function(df, fill) {
    times <- c(stringr::str_c(1:11, " AM"), "12 PM", stringr::str_c(1:11, " PM"))
    days <- c("Mon", "Tue", "Wed", "Thur", "Fri", "Sat")
    ggplot2::ggplot(df) +
        ggplot2::geom_rect(
            ggplot2::aes(
                xmin = x1,
                xmax = x2,
                ymin = stime,
                ymax = etime,
                fill = .data[[fill]]
            ),
            alpha = 0.6,
            color = "black"
        ) +
        ggplot2::geom_text(
            ggplot2::aes(
                x =  x1,
                y = (stime + abs(stime - etime) / 2),
                label = course,
                size = (x2 - x1) * 100
            ),
            #hjust = "right",
            #vjust = 0.5,
            #nudge_x = 0.01
        ) +
        ggplot2::scale_size_area(max_size = 4) +
        ggplot2::scale_y_reverse(
            breaks = 1:23,
            labels = times
        ) +
        ggplot2::scale_x_continuous(
            breaks = 1:6 + 0.5,
            labels = days,
            expand = ggplot2::expansion(add = 0.01)
        ) +
        ggplot2::theme(
            legend.position = "right",
            panel.grid.major.x = ggplot2::element_blank(),
            panel.grid.minor.x = ggplot2::element_blank(),
            axis.ticks.x = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_text(size = 10),
            axis.title = ggplot2::element_blank()
        ) +
       # ggplot2::xlab("") +
       # ggplot2::ylab("") +
        ggplot2::guides(fill = "none") +
        ggplot2::geom_vline(xintercept = 1:7) +
        ggplot2::facet_grid(campus ~ .) -> sched -> p

    return(p)
}
