flightpath_plot_custom<-function (flightpath_result = NULL, insitutype_result = NULL, 
          col = NULL, showclusterconfidence = TRUE) 
{
  if (!is.null(flightpath_result) && !is.null(insitutype_result)) {
    warning("flightpath_result and insitutype_result were both provided. Using only flightpath_result.")
    insitutype_result <- NULL
  }
  if (is.null(flightpath_result) && is.null(insitutype_result)) {
    stop("Must provide either flightpath_result or insitutype_result.")
  }
  if (is.null(flightpath_result)) {
    flightpath_result <- flightpath_layout(logliks = insitutype_result$logliks, 
                                           profiles = insitutype_result$profiles)
  }
  if (is.null(col)) {
    utils::data("iocolors", package = "InSituType", envir = environment())
    scols <- c("#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", 
               "#80B1D3", "#FDB462", "#B3DE69", "#FCCDE5", "#D9D9D9", 
               "#BC80BD", "#CCEBC5", "#FFED6F", "#E41A1C", "#377EB8", 
               "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", 
               "#F781BF", "#999999", sample(colors()[!grepl("grey", 
                                                            colors())], 100))[seq_along(unique(flightpath_result$clust))]
    names(scols) <- unique(flightpath_result$clust)
    iotypespresent <- intersect(names(environment()[["iocolors"]]), 
                                names(scols))
    scols[iotypespresent] <- environment()[["iocolors"]][iotypespresent]
    col <- scols[flightpath_result$clust]
  }
  df <- data.frame(x = flightpath_result$cellpos[, 1], y = flightpath_result$cellpos[, 
                                                                                     2], col = scales::alpha(col, 0.6))
  df_text <- data.frame(x = flightpath_result$clustpos[, 1], 
                        y = flightpath_result$clustpos[, 2], group = rownames(flightpath_result$clustpos), 
                        col = "black")
  if (showclusterconfidence) {
    confthresh <- 0.8
    confidencecolors <- c("#FEB24C", "#FD9D43", "#FC863A", 
                          "#FC6330", "#F64226", "#E8251F", "#D2111F", "#B60224", 
                          "#620015", "#000000")
    confidencecolors <- rep('black',10)
    df_text$col <- confidencecolors[1 + round(9 * (pmax(flightpath_result$meanconfidence, 
                                                        confthresh) - confthresh)/(1 - confthresh))]
    df_text$group <- paste0(df_text$group, "(", round(flightpath_result$meanconfidence, 
                                                      2), ")")
  }
  p <- ggplot2::ggplot() + ggplot2::geom_point(df, mapping = ggplot2::aes(x = flightpath_result$cellpos[, 
                                                                                                        1], y = flightpath_result$cellpos[, 2], color = I(col), 
                                                                          size = I(0.1))) + ggplot2::scale_color_identity() + ggplot2::geom_text(df_text, 
                                                                                                                                                 mapping = ggplot2::aes(x = .data$x, y = .data$y, label = .data$group, 
                                                                                                                                                                        col = I(col)), size = 3) + ggplot2::xlab("") + ggplot2::ylab("") + 
    ggplot2::theme_bw() + ggplot2::theme(legend.position = "none", 
                                         panel.grid = ggplot2::element_blank(), axis.text = ggplot2::element_blank())
  return(p)
}