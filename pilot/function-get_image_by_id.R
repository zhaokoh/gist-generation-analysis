## @knitr function-get_image_by_id
get_image_by_id <- function(nishimoto_images_folder, img_id) {
  im_fullpath = paste(nishimoto_images_folder, paste0("im", sprintf("%07d", img_id), ".jpg"), sep="");
  image_raw <- readJPEG(im_fullpath)
  image <- image_read(image_raw)
  
  return (image)
}
