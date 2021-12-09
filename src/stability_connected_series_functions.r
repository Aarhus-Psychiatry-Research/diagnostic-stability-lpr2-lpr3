stability_connected_series_figure_folder <- "E:/Users/adminmanber/Desktop/LPR2-LPR3/figures/stability_of_a_connected_series_analyses/"

source(here("src", "ggplot_defaults.r"))

save_alluvial_plot <- function(df = df, filename = "allu_first_last") {
    base_plot <- ggplot(
        allu_df_summarized,
        aes(
            y = n,
            axis1 = first_diagnosis,
            axis2 = last_diagnosis,
            fill = first_diagnosis
        )
    )

    output <- base_plot +
        default_theme +
        geom_alluvium(
            knot.prop = TRUE,
            width = 1 / 6,
            knot.pos = 0
        ) +
        scale_alpha(
            range = c(0.8, 1),
            guide = "none"
        ) +
        labs(
            fill = "Final diagnosis",
            y = "Proportion of connected series of events"
        ) +
        theme(
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            legend.position = "none",
            axis.title = element_text(size = 20),
            axis.title.x.top = element_text(
                margin = margin(-1500, 0, -0, 0)
            ),
            axis.text.x = element_text(size = 18),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_blank(),
        ) +
        scale_fill_manual(values = au_colours_combined) +
        geom_stratum(
            fill = "grey95",
            colour = "grey70",
            width = 1 / 6
        ) +
        geom_text(
            stat = "stratum",
            aes(
                label = after_stat(stratum),
                size = 15
            )
        ) +
        scale_size(guide = "none") +
        scale_x_discrete(
            limits = c("First diagnosis", "Final diagnosis"),
            expand = c(0.0, 0.0),
            position = "top"
        ) +
        scale_y_continuous(n.breaks = NULL)

    ggsave(paste0(stability_connected_series_figure_folder, filename, ".png"),
        width = 20,
        height = 20,
        dpi = 100
    )

    output
}

recode_combinations_with_n_smaller_than_5_first_last <- function(df) {
    df_out <- df %>%
        group_by(first_diagnosis, last_diagnosis) %>%
        summarise(n = n()) %>%
        ungroup() %>%
        mutate(last_diagnosis = ifelse(n < 5, "Other", last_diagnosis)) %>%
        group_by(first_diagnosis, last_diagnosis) %>%
        summarise(n = sum(n))
}

recode_combinations_with_n_smaller_than_5_last_first <- function(df) {
    df_out <- df %>%
        group_by(first_diagnosis, last_diagnosis) %>%
        summarise(n = n()) %>%
        ungroup() %>%
        mutate(last_diagnosis = ifelse(n < 5, "Other", last_diagnosis)) %>%
        group_by(first_diagnosis, last_diagnosis) %>%
        summarise(n = sum(n))
}

gen_first_diagnosis_table <- function(df) {
    df_out <- df %>%
        recode_combinations_with_n_smaller_than_5_first_last() %>%
        filter(n > 4) %>%
        group_by(first_diagnosis) %>%
        mutate(percent_of_first_diagnosis = round(n / sum(n) * 100, 1)) %>%
        mutate(percent_of_first_diagnosis = paste0(as.character(percent_of_first_diagnosis), "%")) %>%
        rename(
            `First Diagnosis` = first_diagnosis,
            `Final Diagnosis` = last_diagnosis,
            `% of First Diagnosis` = percent_of_first_diagnosis
        )

    return(df_out)
}

gen_last_diagnosis_table <- function(df) {
    df_out <- df %>%
        recode_combinations_with_n_smaller_than_5_last_first() %>%
        filter(n > 4) %>%
        group_by(last_diagnosis) %>%
        mutate(percent_of_last_diagnosis = round(n / sum(n) * 100, 1)) %>%
        mutate(percent_of_last_diagnosis = paste0(as.character(percent_of_last_diagnosis), "%")) %>%
        arrange(last_diagnosis, first_diagnosis) %>%
        rename(
            `First Diagnosis` = first_diagnosis,
            `Final Diagnosis` = last_diagnosis,
            `% of Last Diagnosis` = percent_of_last_diagnosis
        )

    return(df_out)
}