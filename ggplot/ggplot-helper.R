# empty theme
theme_empty <- theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(), 
                     panel.background = element_blank(),
                     axis.line = element_blank(),
                     axis.title = element_blank(),
                     axis.ticks = element_blank(),
                     axis.text = element_blank())

theme_void <- theme(panel.background = element_blank(),
                    plot.title = element_text(face='bold'),
                    panel.grid.major = element_line(color = 'lightgrey'), 
                    panel.grid.minor = element_blank())

# crop legend from plot
g_legend<- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}