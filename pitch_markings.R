pitch_markings <- function(col_pitch = "green", col_lines = "white", 
                           x_scale = 1, y_scale = 1,
                           x_shift = 0, y_shift = 0,
                           juego = FALSE) {
  require(ggplot2)
  require(grid)
  
  # Adds soccer pitch markings as a layer for use in a ggplot plot. 
  # For instance: ggplot(aes(x = x, y = y)) + 
  #                 pitch_markings() + 
  #                 geom_point() +
  #                 theme_pitch()
  #
  # Assumes 100x100 coordinates.
  #
  # Args:
  #   col_pitch: The colour of the pitch background. Default is green.
  #   col_pitch: The colour of the line markings. Default is white.
  #   x_scale: Amount to scale x-coordinates if not using 100x100. Default is 1.
  #   y_scale: Amount to scale y-coordinates if not using 100x100. Default is 1.
  #   juego: Whether to include Juego de Posicion markings. Default is FALSE.  
  #
  # Returns:
  #   ggplot layer
  
  markings <- list(
    # Add pitch markings
    geom_rect(xmin = 0*x_scale+x_shift, xmax = 100*x_scale+x_shift,
              ymin = 0*y_scale+y_shift, ymax = 100*y_scale+y_shift,
              colour  = col_lines,
              fill  = col_pitch,
              alpha = 1),
    # Centre circle
    annotation_custom(grob=circleGrob(r=unit(1,"npc"),
                                      gp = gpar(col=col_lines, fill = col_pitch, lwd = 2)),
                      xmin=(50-7)*x_scale+x_shift, xmax=(50+7)*x_scale+x_shift, 
                      ymin=(50-7)*y_scale+y_shift, ymax=(50+7)*y_scale+y_shift),
    # Centre spot
    geom_rect(xmin = 49.8*x_scale+x_shift, xmax = 50.2*x_scale+x_shift,
              ymin = 49.8*y_scale+y_shift, ymax = 50.2*y_scale+y_shift,
              colour  = col_lines,
              fill  = col_lines),
    # Halfway line
    annotate("segment",
             x = 50*x_scale+x_shift, xend = 50*x_scale+x_shift,
             y = 0*y_scale+y_shift, yend = 100*y_scale+y_shift,
             colour = col_lines),
    # Add penalty areas (with penalty spot)
    annotation_custom(grob=circleGrob(r=unit(1,"npc"),
                                      gp = gpar(col=col_lines, fill = col_pitch, lwd = 2)),
                      xmin=(88.5-7)*x_scale+x_shift, xmax=(88.5+7)*x_scale+x_shift, 
                      ymin=(50-7)*y_scale+y_shift, ymax=(50+7)*y_scale+y_shift),
    geom_rect(xmin = 83*x_scale+x_shift, xmax = 100*x_scale+x_shift,
              ymin = 21.1*y_scale+y_shift, ymax = 79.9*y_scale+y_shift,
              colour  = col_lines,
              fill = col_pitch,
              alpha = 1),
    geom_rect(xmin = 88.4*x_scale+x_shift, xmax = 88.6*x_scale+x_shift, # Pen spot
              ymin = 49.8*y_scale+y_shift, ymax = 50.2*y_scale+y_shift,
              colour  = col_lines,
              fill  = col_lines),
    annotation_custom(grob=circleGrob(r=unit(1,"npc"),
                                      gp = gpar(col=col_lines, fill = col_pitch, lwd = 2)),
                      xmin=(11.5-7)*x_scale+x_shift, xmax=(11.5+7)*x_scale+x_shift, 
                      ymin=(50-7)*y_scale+y_shift, ymax=(50+7)*y_scale+y_shift),
    geom_rect(xmin = 0*x_scale+x_shift, xmax = 17*x_scale+x_shift,
              ymin = 21.1*y_scale+y_shift, ymax = 79.9*y_scale+y_shift,
              colour  = col_lines,
              fill = col_pitch,
              alpha = 1),
    geom_rect(xmin = 11.4*x_scale+x_shift, xmax = 11.6*x_scale+x_shift, # Pen spot
              ymin = 49.8*y_scale+y_shift, ymax = 50.2*y_scale+y_shift,
              colour  = col_lines,
              fill  = col_lines),
    # Add 6 yards
    geom_rect(xmin = 94.2*x_scale+x_shift, xmax = 100*x_scale+x_shift,
              ymin = 36.8*y_scale+y_shift, ymax = 63.2*y_scale+y_shift,
              colour  = col_lines, fill = col_pitch,
              alpha = 1),
    geom_rect(xmin = 0*x_scale+x_shift, xmax = 5.8*x_scale+x_shift,
              ymin = 36.8*y_scale+y_shift, ymax = 63.2*y_scale+y_shift,
              colour  = col_lines, fill = col_pitch,
              alpha = 1),
    # Add goals
    geom_rect(xmin = 100*x_scale+x_shift, xmax = 102*x_scale+x_shift,
              ymin = 44.2*y_scale+y_shift, ymax = 55.8*y_scale+y_shift,
              colour  = col_lines,
              fill = col_pitch,
              alpha = 1),
    geom_rect(xmin = 0*x_scale+x_shift, xmax = -2*x_scale+x_shift,
              ymin = 44.2*y_scale+y_shift, ymax = 55.8*y_scale+y_shift,
              colour  = col_lines,
              fill = col_pitch,
              alpha = 1))
  
  if (juego) {
    
    # Add juego de posicion markings
    
    markings <- list(markings,
                     geom_segment(aes(x = 17, xend = 17, y = 0, yend = 100), fill = NA, colour  = col_lines,
                                  fill = col_pitch, linetype = "dotted",
                                  alpha = 1),
                     geom_segment(aes(x = 83, xend = 83, y = 0, yend = 100), fill = NA, colour  = col_lines,
                                  fill = col_pitch, linetype = "dotted",
                                  alpha = 1),
                     geom_segment(aes(x = 31.2, xend = 31.2, y = 0, yend = 21.1), fill = NA, colour  = col_lines,
                                  fill = col_pitch, linetype = "dotted",
                                  alpha = 1),
                     geom_segment(aes(x = 31.2, xend = 31.2, y = 79.9, yend = 100), fill = NA, colour  = col_lines,
                                  fill = col_pitch, linetype = "dotted",
                                  alpha = 1),
                     geom_segment(aes(x = 68.8, xend = 68.8, y = 0, yend = 21.1), fill = NA, colour  = col_lines,
                                  fill = col_pitch, linetype = "dotted",
                                  alpha = 1),
                     geom_segment(aes(x = 68.8, xend = 68.8, y = 79.9, yend = 100), fill = NA, colour  = col_lines,
                                  fill = col_pitch, linetype = "dotted",
                                  alpha = 1),
                     geom_segment(aes(x = 0, xend = 100, y = 36.8, yend = 36.8), fill = NA, colour  = col_lines,
                                  fill = col_pitch, linetype = "dotted",
                                  alpha = 1),
                     geom_segment(aes(x = 0, xend = 100, y = 63.2, yend = 63.2), fill = NA, colour  = col_lines,
                                  fill = col_pitch, linetype = "dotted",
                                  alpha = 1),
                     geom_segment(aes(x = 0, xend = 100, y = 79.9, yend = 79.9), fill = NA, colour  = col_lines,
                                  fill = col_pitch, linetype = "dotted",
                                  alpha = 1),
                     geom_segment(aes(x = 0, xend = 100, y = 21.1, yend = 21.1), fill = NA, colour  = col_lines,
                                  fill = col_pitch, linetype = "dotted",
                                  alpha = 1)
    )
  }
  
  return(markings)
}

direction_label <- function(y_label = -3, x_label = 50) {
  require(ggplot2)
  require(grid)
  # Adds 'Direction of play' indicator to pitch plot
  # Args:
  #   y_label: Where on the pitch the arrow and label should show. Defaults to -3.
  #
  # Returns:
  #   ggplot layer
  
  layer <- list(geom_segment(x = x_label - 10, y = y_label, xend = x_label + 10, yend = y_label,
                             arrow = arrow(length = unit(0.02, "npc")),
                             colour = "gray75"),
                annotate("text", x = x_label, y = y_label - 1, label = c("Direction of play"), 
                         vjust = 1.5, size = 3, colour = "gray75"))
  return(layer)
}
