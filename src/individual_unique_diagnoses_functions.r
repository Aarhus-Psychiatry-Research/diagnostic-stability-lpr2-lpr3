library("ggrepel")

individual_unique_diagnoses_figure_folder <- paste0(here::here("figures", "individual_unique_diagnoses_analyses"), "/")
if (!dir.exists(individual_unique_diagnoses_figure_folder)) {
    dir.create(individual_unique_diagnoses_figure_folder, recursive = TRUE)
}

truncation_levels <- c("FXX.XX", "FXX.X", "FXX", "FX")

scale_x_date <- scale_x_date(
            breaks = seq(as.Date("2013-01-01"), as.Date("2021-07-01"), by = "12 months"),
            date_minor_breaks = "3 months",
            date_labels = "%Y",
            limits = c(ymd("2013-01-01"), ymd("2022-06-01")))

y_limits <- scale_y_continuous(
    limits = c(1.0, 1.09)
)

incident_per_active_y_limits <- scale_y_continuous(
    limits = c(0.6, 0.85)
)

source(here("src", "ggplot_defaults.r"))

save_truncation_plot <- function(df, filename, p_values = NULL) {
    plot_colours = c(
                rgb(219 / 255, 139 / 255, 195 / 255),
                rgb(140 / 255, 140 / 255, 140 / 255),
                rgb(204 / 255, 185 / 255, 116 / 255), # FXX
                rgb(100 / 255, 181 / 255, 205 / 255)
            )

    gg <- ggplot(df, aes(
        x = as.Date(period),
        y = estimate,
        ymin = lcl,
        ymax = ucl,
        shape = truncation_level,
        color = truncation_level, 
        fill = truncation_level
    )) +
        geom_linerange(
            alpha = 0.1
        ) +
        geom_point() +
        geom_vline(
            aes(
                xintercept = ymd("2019-02-15")
            ),
            size = 1,
            alpha = 0.15,
            colour = "black"
        ) +
        default_theme +
        scale_color_manual(
            values = plot_colours,
            limits = truncation_levels
        ) +
        scale_fill_manual(
            values = plot_colours,
            limits = truncation_levels
        ) +
        scale_shape_manual(
            values = c(16, 15, 17, 18),
            limits = truncation_levels
        ) +
        geom_label_repel(
            data = filter(df, period == max(df$period)),
            direction = "y",
            hjust = 0,
            nudge_x = 150,
            xlim = c(-Inf, Inf), ylim = c(-Inf, Inf),
            color = "white",
            force = 0.2,
            mapping = aes(
                fill = truncation_level,
                segment.color = NA,
                label = truncation_level
            )
        ) +
        theme(
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 12),
            axis.text.y = element_text(size = 12),
            axis.title = element_text(size = 15),
            legend.position = "None",
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank()
        ) +
        labs(
            x = "",
            y = "Mean number of different diagnoses \n per active treatment course"
        ) +
        scale_x_date +
        incident_per_active_y_limits

    if (!is.null(p_values)) {
        gg <- gg +
            geom_text(
                data = p_values,
                aes(
                    label = significant,
                    color = truncation_level,
                    ymin = NULL,
                    ymax = NULL
                ),
                size = 5,
                hjust = -1.3,
                show.legend = FALSE
            )
    } 


    ggsave(paste0(individual_unique_diagnoses_figure_folder, filename, ".png"),
        plot = gg,
        width = 10,
        height = 5,
        dpi = 600
    )

    return(gg)
}

save_mitigation_strategy_plot <- function(df, filename, p_values = NULL, nudge_constant = 0, exclusive_column_for_lpr2 = NA) {
    plot_colors <- c(
        rgb(129 / 255, 114 / 255, 179 / 255),
        rgb(148 / 255, 120 / 255, 96 / 255),
        rgb(204 / 255, 185 / 255, 116 / 255)
    )

    if (!is.na(exclusive_column_for_lpr2)) {
        df <- df %>% filter(period > ymd("2019-01-01") | origin == exclusive_column_for_lpr2)
    }

    gg <- ggplot(
        df,
        aes(
            x = as.Date(period),
            y = mean_2,
            color = origin
        )
    ) +
        geom_linerange(
            aes(
                ymin = lcl_2,
                ymax = ucl_2
            ),
            alpha = 0.1,
        ) +
        geom_point(
            aes(
                shape = origin,
                color = origin
            )
        ) +
        scale_shape_manual(
            values = c(7, 9, 17)
        ) +
        geom_vline(
            aes(xintercept = ymd("2019-02-15")),
            size = 1, alpha = 0.15,
            colour = "black"
        ) +
        geom_label_repel(
            data = filter(df, period == max(df$period)),
            direction = "y",
            hjust = 0,
            nudge_x = 150,
            xlim = c(-Inf, Inf), ylim = c(-Inf, Inf),
            color = "white",
            force = 0.2,
            mapping = aes(
                fill = origin,
                segment.color = NA,
                label = origin
            )
        ) +
        scale_color_manual(values = plot_colors) +
        scale_fill_manual(values = plot_colors) +
        default_theme +
        theme(
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 12),
            axis.text.y = element_text(size = 12),
            axis.title = element_text(size = 15),
            legend.position = "None",
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank()
        ) +
        labs(
            x = "",
            y = "Mean number of different diagnoses \n per active treatment course",
            fill = "ICD-10 chapter"
        ) +
        scale_x_date +
        y_limits

    if (!is.null(p_values)) {
        p_values <- p_values %>% 
            mutate(mean_2 = if_else(origin == "Most severe", 
                            mean_2 + nudge_constant, 
                            mean_2))

        gg <- gg +
            geom_text(
                data = p_values,
                aes(label = significant),
                size = 7,
                hjust = -1,
                show.legend = FALSE
            )
    }

    ggsave(here::here(individual_unique_diagnoses_figure_folder, str_c(filename, ".png")),
        plot = gg,
        width = 10,
        height = 6,
        dpi = 600
    )

    return(gg)
}

save_combined_plots <- function(figure1, figure2, title) {
    library("patchwork")

    combined <- figure1 + figure2 +
        plot_annotation(tag_levels = "A") +
        plot_layout(
            ncol = 1,
            guides = "collect"
        )

    ggsave(
        paste0(individual_unique_diagnoses_figure_folder, title, ".png"),
        plot = combined,
        width = 10,
        height = 12,
        dpi = 600
    )

    return(combined)
}

save_incident_per_active_plot <- function(df, filename, p_values = NULL, nudge_constant = 0, exclusive_column_for_lpr2 = NA) {
    plot_colors <- c(
        rgb(129 / 255, 114 / 255, 179 / 255),
        rgb(148 / 255, 120 / 255, 96 / 255),
        rgb(204 / 255, 185 / 255, 116 / 255)
    )

    if (!is.na(exclusive_column_for_lpr2)) {
        df <- df %>% filter(period > ymd("2019-01-01") | origin == exclusive_column_for_lpr2)
    }

    gg <- ggplot(df, aes(
                        x = as.Date(period),
                        y = estimate,
                        color = origin
                        )
        ) +
        geom_linerange(
            aes(
                ymin = lcl,
                ymax = ucl
            ),
            alpha = 0.1,
        ) +
        geom_point(
            aes(
                shape = origin,
                color = origin
            )
        ) +
        scale_shape_manual(
            values = c(7, 9, 17)
        ) +
        geom_vline(
            aes(xintercept = ymd("2019-02-15")),
            size = 1, alpha = 0.15,
            colour = "black"
        ) +
        geom_label_repel(
            data = filter(df, period == max(df$period)),
            direction = "y",
            hjust = 0,
            nudge_x = 150,
            xlim = c(-Inf, Inf), ylim = c(-Inf, Inf),
            color = "white",
            force = 0.2,
            mapping = aes(
                fill = origin,
                segment.color = NA,
                label = origin
            )
        ) +
        scale_color_manual(values = plot_colors) +
        scale_fill_manual(values = plot_colors) +
        default_theme +
        theme(
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 12),
            axis.text.y = element_text(size = 12),
            axis.title = element_text(size = 15),
            legend.position = "None",
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank()
        ) +
        labs(
            x = "",
            y = "Mean number of different diagnoses \n per active treatment course",
            fill = "ICD-10 chapter"
        ) +
        scale_x_date +
        incident_per_active_y_limits

    if (!is.null(p_values)) {
        p_values <- p_values %>% 
            mutate(estimate = if_else(origin == "Most severe", 
                            estimate + nudge_constant, 
                            estimate))

        gg <- gg +
            geom_text(
                data = p_values,
                aes(label = significant),
                size = 7,
                hjust = -1,
                show.legend = FALSE
            )
    }

    ggsave(here::here(individual_unique_diagnoses_figure_folder, str_c(filename, ".png")),
        plot = gg,
        width = 10,
        height = 6,
        dpi = 600
    )

    return(gg)
}

save_combined_plots <- function(figure1, figure2, title) {
    library("patchwork")

    combined <- figure1 + figure2 +
        plot_annotation(tag_levels = "A") +
        plot_layout(
            ncol = 1,
            guides = "collect"
        )

    ggsave(
        paste0(individual_unique_diagnoses_figure_folder, title, ".png"),
        plot = combined,
        width = 10,
        height = 12,
        dpi = 600
    )

    return(combined)
}