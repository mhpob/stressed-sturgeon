library(jsonlite)
library(dplyr)

detects <- 's:/up river nanticoke 9-5-23/megadetector_output.json' |> 
  read_json()
library(magick)
img <- image_read('s:/up river nanticoke 9-5-23/photo/im_00001.jpg')
im11 <- sapply(detects$images[[10]]$detections, function(.) as.numeric(.$bbox))

image_draw(img)
rect(xleft = im11[1,]*8416, ybottom = im11[2,]*4736 + im11[4,]*4736, 
     xright = im11[1,]*8416 + im11[3,]*8416, ytop = im11[2,]*4736,
     border = rainbow(7), lty = "dashed", lwd = 15)

dd <- data.frame(
  photo = sapply(
    detects$images,
    function(.) .$file
  ),
  conf = sapply(
    detects$images,
    function(.) .$max_detection_conf
  )
  )
dd$index <- 1:nrow(dd)

dd_subs <- dd |> 
  arrange(-conf)
  

# img <- lapply(
#   dd$photo[1:10],
#   function(.) image_read(paste0('s:/up river nanticoke 9-5-23/photo/', .))
# )
  
vis_md <- function(idx){
  im_bbox <- k[k$index == idx, 'bbox'] |> 
    unlist() |> 
    matrix(ncol = 4, byrow = T)
  im_conf <- k[k$index == idx, 'detect_conf'] * 100
  
  img <- image_read(paste0('s:/up river nanticoke 9-5-23/photo/',
                           k[k$index == idx, 'file'][1])) |> 
    image_draw()
  rect(xleft = im_bbox[, 1]*8416, ybottom = im_bbox[, 2]*4736 + im_bbox[, 4]*4736, 
       xright = im_bbox[, 1]*8416 + im_bbox[, 3]*8416, ytop = im_bbox[, 2]*4736,
       border = rainbow(ncol(im_bbox)), lty = "dashed", lwd = 15)
  text(x = im_bbox[, 1]*8416 + 100, y = im_bbox[, 2]*4736 + 100,
       labels = im_conf, cex = 10)
  dev.off()
  
  print(img)
}













k <- lapply(
  detects$images,
  function(.) data.frame(file = .$file,
                         datetime = as.POSIXct(.$datetime, format = '%Y:%m:%d %H:%M:%S'),
                         detect_conf = if(length(.$detections) == 0) NA else
                           sapply(.$detections, function(.) .$conf),
                         category = if(length(.$detections) == 0) NA else
                           sapply(.$detections, function(.) .$category),
                         bbox = if(length(.$detections) == 0) NA else
                           I(lapply(.$detections, function(.) .$bbox))
  )
)


k <- bind_rows(k, .id = 'index')

j <- k |> 
  arrange(datetime, detect_conf)
