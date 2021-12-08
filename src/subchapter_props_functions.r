source("../src/ggplot_defaults.r")

format_quarters <- function(x) {
  x <- as.yearqtr(x)
  year <- as.integer(x)
  quart <- as.integer(format(x, "%q"))

  paste(year)
  
  #paste(c("Q1","Q2","Q3","Q4")[quart], year)
}


# subchapter_figure_folder <- "E:/Users/adminmanber/Desktop/LPR2-LPR3/figures/subchapter_analyses/"
subchapter_figure_folder <- here::here("figures", "subchapter_analyses")
if (!dir.exists(subchapter_figure_folder)) {dir.create(subchapter_figure_folder, recursive=TRUE)}

graph_subchapter_props <- function(
    df, 
    filename, 
    date_range, 
    with_mitigation = TRUE, 
    ylabel = "", 
    p_values = NULL,
    nudge_frac=0.02,
    nudge_constant=0.02) {
    library("patchwork")
    library("gridExtra")
    library("here")

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

        if(!is.null(p_values)){
            # nudge prop a fraction of the original size for free axes
            p_values <- p_values %>%
                group_by(adiagnosekode) %>%
                mutate(prop = nudge_min_and_max_values(prop, significant, nudge_frac=nudge_frac))

            base_plot <- base_plot + 
                geom_text(data = p_values,
                        aes(label = significant,
                            ymin = NULL,
                            ymax = NULL),
                            size = 5,
                            hjust = -1.3,
                            show.legend=FALSE) 
        }

        final_plot <- base_plot + 
            facet_wrap(vars(adiagnosekode), nrow = 5, scales = "free_y")


    } else {

        if(!is.null(p_values)){
            # nudge prop a constant for same axes
            p_values <- p_values %>%
                group_by(adiagnosekode) %>%
                mutate(prop = nudge_min_and_max_values(prop, significant, nudge_constant=nudge_constant))

            base_plot <- base_plot + 
                geom_text(data = p_values,
                        aes(label = significant,
                                ymin = NULL,
                                ymax = NULL),
                        size = 5,
                        hjust = -1.3,
                        show.legend=FALSE) 
        }


        final_plot <- base_plot +
            facet_wrap(vars(adiagnosekode), nrow = 5)

    }

    ggsave(here(subchapter_figure_folder, str_c(filename, ".png")), 
            plot = final_plot,
            width = 7, 
            height = 10, 
            dpi = 150)

    return (final_plot)
}

# parameter for adding constant or multiplying 

nudge_min_and_max_values <- function(x, label, nudge_constant=NULL, nudge_frac=NULL){
    if(!is.null(nudge_constant) & !is.null(nudge_frac)){
        stop("Set one of nudge_constant or nudge_frac")
    }
    
    has_content <- if_else(label == "", FALSE, TRUE)
    if(sum(has_content) <= 1){
        return(x)
    }

    min_x <- min(x[has_content])
    max_x <- max(x[has_content])

    if(!is.null(nudge_frac)){
        x[x == min_x] = min_x * (1 - nudge_frac)
        x[x == max_x] = max_x * (1 + nudge_frac)
    }
    if(!is.null(nudge_constant)){
        x[x == min_x] = min_x - nudge_constant
        x[x == max_x] = max_x + nudge_constant
    }
    return(x)
}