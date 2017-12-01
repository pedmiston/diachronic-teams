# This file should be sourced if pictures are being placed on the x-axis.

# ---- load-axis-pics
data("ItemLabels")
pic_locations <- list.files(system.file("extdata/items", package = "totems"), full.names = TRUE) %>%
  unlist() %>%
  data_frame(Path = .) %>%
  mutate(Name = basename(Path)) %>%
  inner_join(ItemLabels)

item_pics <- purrr::map(
  as.character(ItemLabels$Number),
  function(number) {
    loc <- pic_locations[pic_locations$Number == as.integer(number), "Path"][[1]]
    EBImage::readImage(loc)
  })

innovation_axis = function(img) {
  structure(
    list(img=img),
    class = c("element_custom","element_blank", "element") # inheritance test workaround
  )
}

# returns a gTree with two children: the text label, and a rasterGrob below
element_grob.element_custom <- function(element, x,...)  {
  stopifnot(length(x) == length(element$img))
  tag <- names(element$img)
  # add vertical padding to leave space
  g1 <- textGrob(paste0(tag, "\n\n\n\n\n"), x=x, vjust=0.6)
  g2 <- mapply(rasterGrob, x=x, image=element$img[tag],
               MoreArgs=list(vjust=0.7, interpolate=FALSE,
                             height=unit(3,"lines")),
               SIMPLIFY=FALSE)
  
  gTree(children=do.call(gList, c(g2, list(g1))), cl="custom_axis")
}

# gTrees don't know their size and ggplot would squash it, so give it room
grobHeight.custom_axis = heightDetails.custom_axis = function(x, ...)
  unit(6, "lines")