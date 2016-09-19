logo_grob_opta <- "~/logo_opta.png" %>% # You'll need to change the string depending on where you save the logos
  readPNG() %>% 
  rasterGrob() # dimensions = 250 x 109

add_opta_logo <- function(chosen_plot) {
  require(png)
  require(grid)
  
  # Adds Analytics FC logo to bottom right hand corner of the image, the Opta logo to the bottom right and prints ggplot object
  # 
  # For instance: p <- ggplot(...) + ...
  #               add_opta_logo(p)
  #               make_footnote("@Torvaney\nData by Opta")
  #
  # Args:
  #   chosen_plot: ggplot object to be printed with logo
  #
  # Returns:
  #   NULL
  
  # The Opta logo
  g <- logo_grob_opta   # 250 x 109
  
  max_height <- 1
  logo_h <- unit(max_height, "cm")
  
  # Size of Opta logo
  max_width_afc <- max_height*(250/109)
  logo_w_afc <- unit(max_width_afc, "cm")
  
  # Set up the layout for grid 
  heights <- unit.c(unit(1, "npc") - logo_h, logo_h)
  widths <- unit.c(logo_w_afc, unit(1, "npc") - logo_w_afc)
  lo <- grid.layout(2, 2, widths = widths, heights = heights)
  # # Show the layout
  # grid.show.layout(lo)
  
  # Position the elements within the viewports
  grid.newpage()
  pushViewport(viewport(layout = lo))
  
  # The plot
  pushViewport(viewport(layout.pos.row = 1:2, layout.pos.col = 1:2))
  print(chosen_plot, newpage=FALSE)
  popViewport()
  
  # The Opta logo
  pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 1))
  print(grid.draw(g), newpage=FALSE)
  popViewport()
}

make_footnote <- function(footnoteText = "Data by Opta",
                         size = .7, color = grey(.5),
                         position = c("right", "bottom")) {
  require(grid)
  
  # Adds footnote to bottom-right hand corner of image. (stolen from stackoverflow)
  # Should be called immediately after ggplot object has been printed.
  # For instance: p <- ggplot(...) + ...
  #               addLogoAndPrint(p)
  #               makeFootnote("@Torvaney\nData by Opta")
  #
  # Args:
  #   footnoteText: The text to be added.
  #   size: The size of the text.
  #   color: Colour of the text.
  #
  # Returns:
  #   NA 
  
  pushViewport(viewport())
  grid.text(label = footnoteText ,
            x = unit(1,"npc") - unit(2, "mm"),
            y = unit(2, "mm"),
            just = position,
            gp = gpar(cex = size, col = color))
  popViewport()
}