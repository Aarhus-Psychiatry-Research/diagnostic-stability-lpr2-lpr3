au_colours_bright <- c(
    rgb(0 / 255, 61 / 255, 115 / 255),
    rgb(250 / 255, 187 / 255, 0 / 255),
    rgb(55 / 255, 160 / 255, 203 / 255),
    rgb(101 / 255, 90 / 255, 159 / 255),
    rgb(139 / 255, 173 / 255, 63 / 255),
    rgb(226 / 255, 0 / 255, 26 / 255),
    rgb(0 / 255, 171 / 255, 164 / 255),
    rgb(238 / 255, 127 / 255, 0 / 255),
    rgb(226 / 255, 0 / 255, 122 / 255),
    rgb(135 / 255, 135 / 255, 135 / 255)
)

au_colours_dark <- c(
    rgb(0 / 255, 37 / 255, 70 / 255),
    rgb(40 / 255, 28 / 255, 65 / 255),
    rgb(0 / 255, 62 / 255, 92 / 255),
    rgb(0 / 255, 69 / 255, 67 / 255),
    rgb(66 / 255, 88 / 255, 33 / 255),
    rgb(99 / 255, 75 / 255, 3 / 255),
    rgb(95 / 255, 52 / 255, 8 / 255),
    rgb(91 / 255, 12 / 255, 12 / 255),
    rgb(95 / 255, 0 / 255, 48 / 255),
    rgb(75 / 255, 75 / 255, 74 / 255)
)

au_colours_combined <- c(au_colours_bright, au_colours_dark)

seaborn <- c(
    rgb(76 / 255, 114 / 255, 176 / 255),
    rgb(221 / 255, 132 / 255, 82 / 255),
    rgb(75 / 255, 167 / 255, 104 / 255),
    rgb(196 / 255, 78 / 255, 82 / 255),
    rgb(129 / 255, 114 / 255, 179 / 255),
    rgb(148 / 255, 120 / 255, 96 / 255),
    rgb(219 / 255, 139 / 255, 195 / 255),
    rgb(140 / 255, 140 / 255, 140 / 255),
    rgb(204 / 255, 185 / 255, 116 / 255),
    rgb(100 / 255, 181 / 255, 205 / 255)
)

default_colour_scale <- scale_colour_manual(values = seaborn)

default_fill_scale <- scale_fill_manual(values = seaborn)

default_theme <- theme_classic() +
    theme(
        axis.line = element_line(colour = "grey50"),
        strip.background = element_rect(
            color = "grey93",
            fill = "grey95",
            size = 1,
            linetype = "solid"
        )
    )