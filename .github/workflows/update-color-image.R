library(ggpubr)
library(aRtsy)

palettes <- c(
  "azul",
  "bell",
  "blackwhite",
  "blossom",
  "boogy1",
  "boogy2",
  "boogy3",
  "dark1",
  "dark2",
  "dark3",
  "flag",
  "flora",
  "house",
  "gogh",
  "jasp",
  "jfa",
  "jungle",
  "klimt",
  "kpd",
  "lava",
  "origami",
  "mixer1",
  "mixer2",
  "mixer3",
  "mixer4",
  "nature",
  "neo1",
  "neo2",
  "neo3",
  "neon1",
  "neon2",
  "retro1",
  "retro2",
  "retro3",
  "retro4",
  "shell1",
  "shell2",
  "shell3",
  "sooph",
  "sky",
  "tuscany1",
  "tuscany2",
  "tuscany3",
  "vrolik1",
  "vrolik2",
  "vrolik3",
  "vrolik4",
  "vrolik5"
)

ncol <- 3
n <- length(palettes)
nrow <- ceiling(n / ncol)

plotList <- list()
for (i in seq_along(palettes)) {
  palette <- aRtsy::colorPalette(palettes[i])
  d <- data.frame(y = rep(1, length(palette)), col = palette)
  p <- ggplot2::ggplot(data = d, mapping = ggplot2::aes(y = y, group = col)) +
    ggplot2::geom_bar(fill = d[["col"]], col = "black", linewidth = 0.1) +
    ggplot2::xlab(palettes[i]) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_text(size = 9),
      axis.title.y = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = NA, colour = NA),
      legend.position = "none",
      panel.border = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_blank()
    )
  plotList[[i]] <- p
}

plot <- do.call("ggarrange", c(plotList, nrow = nrow, ncol = ncol)) +
  ggplot2::theme(panel.background = ggplot2::element_rect(fill = NA, colour = NA))
ggplot2::ggsave(filename = "./man/figures/colors.svg", plot = plot, width = 12.5, height = nrow * 1.25, units = "cm")
ggplot2::ggsave(filename = "./man/figures/colors.pdf", plot = plot, width = 12.5, height = nrow * 1.25, units = "cm")
