source("../src/ggplot_defaults.r")

format_quarters <- function(x) {
  x <- as.yearqtr(x)
  year <- as.integer(x)
  quart <- as.integer(format(x, "%q"))

  paste(year)
  
  #paste(c("Q1","Q2","Q3","Q4")[quart], year)
}

subchapter_figure_folder <- "E:/Users/adminmanber/Desktop/LPR2-LPR3/figures/subchapter_analyses/"

graph_subchapter_props <- function(df, filename, date_range, with_mitigation = TRUE, ylabel = "") {
    library("patchwork")
    library("gridExtra")
    folder <- "E:/Users/adminmanber/Desktop/LPR2-LPR3/figures/"
    df <- df

    if (with_mitigation) {
        df <- df %>% 
            filter(period > ymd("2019-01-01") | origin == "Unchanged")    

        aesthetics <- ggplot(df, aes(x=as.Date(period), y=prop, 
                                        color = origin, 
                                        shape = origin,
                                        ymin=prop_lcl, 
                                        ymax=prop_ucl))
    } else {
        aesthetics <- ggplot(df, aes(x=as.Date(period), y=prop, 
                                        ymin=prop_lcl, 
                                        ymax=prop_ucl))
    }
    

    base_plot <- aesthetics +
    geom_point(position = position_dodge(width = 60),
            size = 1) +
    default_theme +
    default_colour_scale +
    theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.background = element_rect(
            fill = "gray97",
            colour = "gray97",
            size = 0.5
        ),
        legend.key.size = unit(0.8, "cm"),
        legend.text = element_text(size = 10),
        legend.background = element_rect(fill = "gray97", size = 3),
        legend.position = "bottom"
    ) +
    geom_vline(aes(xintercept = ymd("2019-02-15")), size=0.5, alpha=0.7, colour="grey") +
    geom_linerange(alpha = 0.2, size = 0.2, position = position_dodge(width = 60)) +
    scale_x_date(
        breaks = seq(date_range[1], date_range[2], by="12 months"),
        date_minor_breaks = "3 months",
        date_labels = "%Y",
        limits = date_range
    ) +
    labs(
        color = "Mitigation strategy", 
        shape = "Mitigation strategy",
        y = ylabel,
        x = "Quarter"
    )


    same_y_axes <- base_plot +
        facet_wrap(vars(adiagnosekode), nrow = 5)
    free_y_axes <- base_plot + 
        facet_wrap(vars(adiagnosekode), nrow = 5, scales = "free_y")

    # combined <- same_y_axes + free_y_axes +
    #     plot_annotation(tag_levels = "A") + 
    #     plot_layout(ncol = 1, 
    #                 guides = "collect")

    # Add shared x and y-axis
    # gt <- patchworkGrob(combined)
    # output <- gridExtra::grid.arrange(gt, 
    #             left = "Proportion of patients with incident main diagnosis from chapter",
    #             bottom = "Quarter")

    if (grepl("*free*", filename)) {
        ggsave(str_c(subchapter_figure_folder, filename, ".png"), 
            plot = free_y_axes,
            width = 7, 
            height = 10, 
            dpi = 150)

        free_y_axes
    } else {
        ggsave(str_c(subchapter_figure_folder, filename, ".png"), 
            plot = same_y_axes,
            width = 7, 
            height = 10, 
            dpi = 150)

        same_y_axes
    }
}