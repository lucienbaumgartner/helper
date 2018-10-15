# empty theme
theme_empty <- theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(), 
                     panel.background = element_blank(),
                     axis.line = element_blank(),
                     axis.title = element_blank(),
                     axis.ticks = element_blank(),
                     axis.text = element_blank())

theme_void <- function(grid='major', grid.color='lightgrey', title.spacing=0, subtitle.spacing=0, x.lab.spacing=0, y.lab.spacing=0){
  thm <- theme(
    panel.background = element_blank(),
    plot.title = element_text(face='bold', margin=margin(0,0,title.spacing,0)),
    plot.subtitle = element_text(margin=margin(0,0,subtitle.spacing,0)),
    axis.title.x = element_text(margin=margin(x.lab.spacing,0,0,0)),
    axis.title.y = element_text(margin=margin(0,y.lab.spacing,0,0))
    )
  
  if(identical(grid, 'major')){
    thm <- thm + theme(
      panel.grid.major = element_line(color = grid.color), 
      panel.grid.minor = element_blank()
    )
  }
  if(identical(grid, 'minor')){
    thm <- thm + theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_line(color = grid.color)
    )
  }
  if(identical(grid, 'both')){
    thm <- thm + theme(
      panel.grid.major = element_line(color = grid.color),
      panel.grid.minor = element_line(color = grid.color)
    )
  }
  if(identical(grid, 'none')){
    thm <- thm + theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  }
  return(thm)
}

theme_void_T <- theme(panel.background = element_blank(),
                    plot.title = element_text(face='bold'),
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank())

# crop legend from plot
g_legend<- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}