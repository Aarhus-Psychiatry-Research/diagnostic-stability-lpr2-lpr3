source(here("src", "ggplot_defaults.r"))

format_quarters <- function(x) {
    x <- as.yearqtr(x)
    year <- as.integer(x)
    quart <- as.integer(format(x, "%q"))

    paste(year)

    ## paste(c("Q1","Q2","Q3","Q4")[quart], year)
}


subchapter_figure_folder <- here::here("figures", "subchapter_analyses")
if (!dir.exists(subchapter_figure_folder)) {
    dir.create(subchapter_figure_folder, recursive = TRUE)
}

graph_subchapter_props <- function(df,
                                   filename,
                                   date_range,
                                   with_mitigation = TRUE,
                                   ylabel = "",
                                   p_values = NULL,
                                   nudge_frac = 0.02,
                                   nudge_constant = 0.02,
                                   tag = "NA",
                                   unchanged_only = FALSE) {
    library("patchwork")
    library("gridExtra")
    library("here")

    df <- df %>% 
        mutate(adiagnosekode = case_when(
            adiagnosekode == "F0" ~ "F0 - Organic disorders",
            adiagnosekode == "F1" ~ "F1 - Substance abuse",
            adiagnosekode == "F2" ~ "F2 - Psychotic disorders",
            adiagnosekode == "F3" ~ "F3 - Mood disorders",
            adiagnosekode == "F4" ~ "F4 - Neurotic & stress-related",
            adiagnosekode == "F5" ~ "F5 - Eating & sleeping disorders",
            adiagnosekode == "F6" ~ "F6 - Personality disorders",
            adiagnosekode == "F7" ~ "F7 - Mental retardation",
            adiagnosekode == "F8" ~ "F8 - Developmental disorders",
            adiagnosekode == "F9" ~ "F9 - Child & adolescent disorders"
        ))

    if (with_mitigation) {
        df <- df %>%
            filter(period > ymd("2019-01-01") | origin == "Unchanged")

        aesthetics <- ggplot(df, aes(
            x = as.Date(period), y = prop,
            color = origin,
            shape = origin,
            ymin = prop_lcl,
            ymax = prop_ucl
        )) + 
        default_colour_scale +
        default_theme +
        theme(
            legend.key.size = unit(0.8, "cm"),
            legend.text = element_text(size = 10),
            legend.background = element_rect(fill = "gray97", size = 3),
            legend.position = "bottom"
        )
    } else {
        df <- df %>%
            filter(origin == "Unchanged")

        aesthetics <- ggplot(df, aes(
            x = as.Date(period), y = prop,
            color = origin,
            shape = origin,
            ymin = prop_lcl,
            ymax = prop_ucl
        )) + 
        scale_colour_manual(values = rgb(75 / 255, 167 / 255, 104 / 255)) +
        default_theme +
        theme(legend.position = "none") +
        scale_shape_manual(values = c(15))
    }

    base_plot <- aesthetics +
        geom_point(
            position = position_dodge(width = 60),
            size = 1
        ) +
        theme(
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
            panel.background = element_rect(
                fill = "gray97",
                colour = "gray97",
                size = 0.5
            )
        ) +
        geom_vline(aes(xintercept = ymd("2019-02-15")), size = 0.5, alpha = 0.7, colour = "grey") +
        geom_linerange(alpha = 0.2, size = 0.2, position = position_dodge(width = 60)) +
        scale_x_date(
            breaks = seq(date_range[1], date_range[2], by = "12 months"),
            date_minor_breaks = "3 months",
            date_labels = "%Y",
            limits = date_range
        ) +
        labs(
            color = "Mitigation strategy",
            shape = "Mitigation strategy",
            y = ylabel,
            x = "Quarter",
            tag = tag
        )

    ## combined <- same_y_axes + free_y_axes +
    ##     plot_annotation(tag_levels = "A") +
    ##     plot_layout(ncol = 1,
    ##                 guides = "collect")

    ## Add shared x and y-axis
    ## gt <- patchworkGrob(combined)
    ## output <- gridExtra::grid.arrange(gt,
    ##             left = "Proportion of patients with incident main diagnosis from chapter",
    ##             bottom = "Quarter")
    if (grepl("*free*", filename)) {
        if (!is.null(p_values)) {
            ## nudge prop a fraction of the original size for free axes
            p_values <- p_values %>%
                group_by(adiagnosekode) %>%
                mutate(prop = if_else(origin != "Unchanged",
                                        nudge_min_and_max_values(prop, significant, nudge_frac = nudge_frac),
                                        nudge_min_and_max_values(prop, significant, nudge_frac = nudge_frac * 1.5)
                                    ))

            base_plot <- base_plot +
                geom_text(
                    data = p_values,
                    aes(
                        label = significant,
                        ymin = NULL,
                        ymax = NULL
                    ),
                    size = 5,
                    hjust = -1.3,
                    show.legend = FALSE
                )
        }

        final_plot <- base_plot +
            facet_wrap(vars(adiagnosekode), nrow = 5, scales = "free_y")
    } else {
        if (!is.null(p_values)) {
            ## nudge prop a constant for same axes
            p_values <- p_values %>%
                group_by(adiagnosekode) %>%
                mutate(prop = if_else(origin != "Unchanged",
                                        nudge_min_and_max_values(prop, significant, nudge_constant = nudge_constant),
                                        nudge_min_and_max_values(prop, significant, nudge_constant = nudge_constant * 1.5)
                                    )
                )

            base_plot <- base_plot +
                geom_text(
                    data = p_values,
                    aes(
                        label = significant,
                        ymin = NULL,
                        ymax = NULL
                    ),
                    size = 5,
                    hjust = -1.3,
                    show.legend = FALSE
                )
        }


        final_plot <- base_plot +
            facet_wrap(vars(adiagnosekode), nrow = 5)
    }

    ggsave(here(subchapter_figure_folder, str_c(filename, ".png")),
        plot = final_plot,
        width = 7,
        height = 10,
        dpi = 150
    )

    return(final_plot)
}

## parameter for adding constant or multiplying