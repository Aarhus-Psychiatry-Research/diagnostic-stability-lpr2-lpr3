library("ggrepel")

individual_unique_diagnoses_figure_folder <- "E:/Users/adminmanber/Desktop/LPR2-LPR3/figures/individual_unique_diagnoses_analyses/"

truncation_levels = c("FXX.XX*", "FXX.X*", "FXX*", "FX*")

save_truncation_plot <- function(df, filename) {
    gg <- ggplot(df, aes(x=as.Date(period),
                            y=mean,
                            ymin=lcl,
                            ymax=ucl)
                ) +
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
            xintercept = ymd("2019-02-15")), 
            size=1, 
            alpha=0.1, 
            colour="black", 
            linetype = "longdash"
    ) +
    geom_label_repel(
        data = filter(df, period == ymd("2020-10-01")),
        direction = "y",
        hjust = 0,
        nudge_x = 100,
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
                rgb(76/255, 114/255, 176/255),
                rgb(221/255, 132/255, 82/255),
                rgb(75/255, 167/255, 104/255),
                rgb(196/255, 78/255, 82/255)
        ),
        limits = truncation_levels
    ) +
    scale_fill_manual(
        values = c(
                rgb(76/255, 114/255, 176/255),
                rgb(221/255, 132/255, 82/255),
                rgb(75/255, 167/255, 104/255),
                rgb(196/255, 78/255, 82/255)
        ),
        limits = truncation_levels
    ) +
    scale_shape_manual(
        values = c(16, 15, 17, 18),
        limits = truncation_levels
    ) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            legend.position = "None",
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank()) +
    labs(x = "", 
         y = "Number of different diagnoses in quarter \n for patients with at least one contact in quarter"
    ) +
    scale_x_date(breaks = seq(as.Date("2013-01-01"), as.Date("2021-07-01"), by="12 months"),
                date_minor_breaks = "3 months",
                date_labels = "%Y",
                limits=c(ymd("2013-01-01"), ymd("2021-07-01")))
    

    ggsave(paste0(individual_unique_diagnoses_figure_folder, filename, ".png"), 
        width = 10, 
        height = 5, 
        dpi = 600)

    return(gg)
}

save_mitigation_strategy_plot <- function(df, filename) {
    plot_colors <- c("steelblue", "forestgreen", "#C93312", "#DC863B", "#E1AF00" , "slateblue4")

    gg <- ggplot(df, 
        aes(x=as.Date(period),
                    y=mean_1, 
                    ymin=lcl_1, 
                    ymax=ucl_1
            )
        ) +
        geom_linerange(
            alpha = 0.1,
        ) +
        geom_point(
            aes(
                shape=origin, 
                color=origin
            )
        ) +
        geom_vline(
            aes(xintercept = ymd("2019-02-15")), 
            size=1, alpha=0.1, 
            colour="black", 
            linetype = "longdash"
        ) +
        geom_label_repel(
            data = filter(df, period == ymd("2020-10-01")),
            direction = "y",
            hjust = 0,
            nudge_x = 100,
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
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            legend.position = "None",
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank()) +
        labs(x = "", 
            y = "Number of different diagnoses in quarter \n for patients with at least one contact in quarter",  
            fill = "ICD-10 chapter"
        ) +
        scale_x_date(breaks = seq(as.Date("2013-01-01"), as.Date("2021-07-01"), by="12 months"),
                date_minor_breaks = "3 months",
                date_labels = "%Y",
                limits=c(ymd("2013-01-01"), ymd("2021-07-01"))) +
        scale_y_continuous(limits = c(1, 1.05))

    ggsave(paste0(individual_unique_diagnoses_figure_folder, filename, ".png"), 
        width = 10, 
        height = 5, 
        dpi = 600)

    return(gg)
}

save_combined_plots <- function(figure1, figure2, title) {
    library("patchwork")

    combined <- figure1 + figure2 +
        plot_annotation(tag_levels = "A") +
        plot_layout(
            ncol = 1,
            guides = "collect")

    ggsave(
        paste0(individual_unique_diagnoses_figure_folder, title, ".png"), 
        plot = combined,
        width = 10, 
        height = 10, 
        dpi = 600
    )

    return(combined)
}