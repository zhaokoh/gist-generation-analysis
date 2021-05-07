## @knitr function-get_image_by_id
get_image_by_id <- function(nishimoto_images_folder, img_id) {
  im_fullpath = paste(nishimoto_images_folder, paste0("im", sprintf("%07d", img_id), ".jpg"), sep="");
  image_raw <- readJPEG(im_fullpath)
  image <- image_read(as.raster(image_raw))
  
  return (image)
}

## @knitr function-get_image_id_by_filename
get_image_id_by_filename <- function(image_filename) {
  return(as.integer(gsub("im[0]*([1-9][0-9]*).jpg", "\\1", image_filename)))
}

## @knitr function-get_batch_image_ids
get_batch_image_ids <- function(base_folder, group, bucket) {
  img_base_folder <- paste(base_folder, group, "buckets", bucket, sep="/")
  files <- str_replace(list.files(pattern="*.jpg", path=img_base_folder), "im0+", "")
  files <- str_replace(files, ".jpg", "")
  return(as.integer(files))
}
