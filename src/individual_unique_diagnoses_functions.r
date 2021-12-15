library("ggrepel")

individual_unique_diagnoses_figure_folder <- here::here("figures", "individual_unique_diagnoses_analyses")
if (!dir.exists(individual_unique_diagnoses_figure_folder)) {
    dir.create(individual_unique_diagnoses_figure_folder, recursive = TRUE)
}

truncation_levels <- c("FXX.XX", "FXX.X", "FXX", "FX")

save_truncation_plot <- function(df, filename, p_values = NULL) {
    gg <- ggplot(df, aes(
        x = as.Date(period),
        y = mean,
        ymin = lcl,
        ymax = ucl
    )) +
        geom_linerange(
            alpha = 0.1
        ) +
        geom_point(
            aes(
                shape = truncation,
                color = truncation,
            )
        ) +
        geom_vline(
            aes(
                xintercept = ymd("2019-02-15")
            ),
            size = 1,
            alpha = 0.1,
            colour = "black",
            linetype = "longdash"
        ) +
        geom_label_repel(
            data = filter(df, period == ymd("2020-10-01")),
            direction = "y",
            hjust = 0,
            nudge_x = 150,
            xlim = c(-Inf, Inf), ylim = c(-Inf, Inf),
            color = "white",
            force = 0.2,
            mapping = aes(
                fill = truncation,
                segment.color = NA,
                label = truncation
            )
        ) +
        default_theme +
        scale_color_manual(
            values = c(
                rgb(76 / 255, 114 / 255, 176 / 255),
                rgb(221 / 255, 132 / 255, 82 / 255),
                rgb(75 / 255, 167 / 255, 104 / 255),
                rgb(196 / 255, 78 / 255, 82 / 255)
            ),
            limits = truncation_levels
        ) +
        scale_fill_manual(
            values = c(
                rgb(76 / 255, 114 / 255, 176 / 255),
                rgb(221 / 255, 132 / 255, 82 / 255),
                rgb(75 / 255, 167 / 255, 104 / 255),
                rgb(196 / 255, 78 / 255, 82 / 255)
            ),
            limits = truncation_levels
        ) +
        scale_shape_manual(
            values = c(16, 15, 17, 18),
            limits = truncation_levels
        ) +
        theme(
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
            legend.position = "None",
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank()
        ) +
        labs(
            x = "",
            y = "Number of different diagnoses in quarter \n for patients with at least one contact in quarter"
        ) +
        scale_x_date(
            breaks = seq(as.Date("2013-01-01"), as.Date("2021-07-01"), by = "12 months"),
            date_minor_breaks = "3 months",
            date_labels = "%Y",
            limits = c(ymd("2013-01-01"), ymd("2021-07-01"))
        )

    if (!is.null(p_values)) {
        gg <- gg +
            geom_text(
                data = p_values,
                aes(
                    label = significant,
                    color = truncation,
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
    plot_colors <- c("steelblue", "forestgreen", "#C93312", "#DC863B", "#E1AF00", "slateblue4")

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
        geom_vline(
            aes(xintercept = ymd("2019-02-15")),
            size = 1, alpha = 0.1,
            colour = "black",
            linetype = "longdash"
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
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
            legend.position = "None",
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank()
        ) +
        labs(
            x = "",
            y = "Number of different diagnoses in quarter \n for patients with at least one contact in quarter",
            fill = "ICD-10 chapter"
        ) +
        scale_x_date(
            breaks = seq(as.Date("2013-01-01"), as.Date("2021-07-01"), by = "12 months"),
            date_minor_breaks = "3 months",
            date_labels = "%Y",
            limits = c(ymd("2013-01-01"), ymd("2022-06-01"))
        ) +
        scale_y_continuous(limits = c(1, 1.07))

    if (!is.null(p_values)) {
        p_values <- p_values %>% 
            mutate(mean_2 = if_else(origin == "Most severe", 
                            mean_2 + nudge_constant, 
                            mean_2))

        gg <- gg +
            geom_text(
                data = p_values,
                aes(label = significant),
                size = 5,
                hjust = -1.3,
                show.legend = FALSE
            )
    }

    ggsave(here::here(individual_unique_diagnoses_figure_folder, str_c(filename, ".png")),
        plot = gg,
        width = 10,
        height = 5,
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
        paste0(here("figures", "individual_unique_diagnoses"), title, ".png"),
        plot = combined,
        width = 10,
        height = 10,
        dpi = 600
    )

    return(combined)
}