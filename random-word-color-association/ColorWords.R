df <- fread("/Users/lucienbaumgartner/Documents/Nachhilfe/flights14.csv")
write.csv(df, "/Users/lucienbaumgartner/Documents/Nachhilfe/flights14.csv")

# install.packages("dplyr")
library(dplyr)

setwd("/Users/lucienbaumgartner/Desktop/")
colorlist <- tibble(word=NA,red=NA, green=NA, blue=NA, date=NA)

color_me <- function(){
  color = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
  x <- readline("We don't like grey words.. let's do some magic: type in your word here in the console and press enter:    ")  
  if(is.character(x)) {
    print(noquote(paste('Word:', x)))
    r.col <- sample(color, 1)
    rgb.col <- col2rgb(r.col) %>% c()
    rgb.rel <- round(rgb.col/255,3)
    if(file.access("colorlist.csv", mode=0)==0) colorlist <- read.csv("colorlist.csv") %>% as_tibble()
    colorlist <- colorlist %>% 
      add_row(word=paste(x), red=rgb.rel[1], green=rgb.rel[2], blue=rgb.rel[3], date=paste(Sys.time())) %>%
      na.omit()
    write.csv(colorlist, "colorlist.csv", row.names = F)
    print(noquote(paste('R color:', r.col)))
    return(noquote(paste0('RGB: {', paste0(rgb.rel, collapse=', ') , '}')))
  }
}
color_me()

 
read.csv("colorlist.csv")



