library(ggplot2)

pro_con_colors <- c("#C67800", "#205C8A")

#colorfunction <- colorRampPalette(pro_con_colors)
#pred_cols <- colorfunction(4)

dd_theme <-
  function() {
    theme_bw() +
      theme(
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_line(color = '#eeeeee'),
        strip.background = element_blank(),
        legend.position = "bottom",
        text = element_text(family = "Palatino"))
  }

#p <- ggplot(mtcars, aes(wt, mpg))
#p + geom_point() + dd_theme()
