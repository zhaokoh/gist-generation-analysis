plot_neighbors_custom <- function (x, n, connect.lines = "all", start.lines = T, method = "PCA", 
          dims = 3, axes = F, box = F, cex = 1, legend = T, size = c(800, 
                                                                     800), 
          alpha = "graded", alpha.grade = 1, col = "rainbow", 
          tvectors = tvectors, breakdown = FALSE, replace_with_image = FALSE, ...) 
{
  if (!(dims %in% 2:3)) {
    stop("Please set dim to 2 or 3")
  }
  if (class(tvectors) == "data.frame") {
    tvectors <- as.matrix(tvectors)
  }
  else if (class(tvectors) == "textmatrix") {
    tvectors <- matrix(tvectors, nrow = nrow(tvectors), 
                       ncol = ncol(tvectors), dimnames = list(rownames(tvectors), 
                                                              colnames(tvectors)))
  }
  if (class(tvectors) == "matrix") {
    if (class(x) == "factor") {
      x <- as.character(x)
      message("Note: x converted to character")
    }
    if (class(x) == "character") {
      if (breakdown == TRUE) {
        satz1 <- breakdown(x)
      }
      if (breakdown == FALSE) {
        satz1 <- x
      }
      satz1split <- strsplit(satz1, split = " ")[[1]]
      used1 <- satz1split[satz1split %in% rownames(tvectors)]
      if (length(used1) > 1) {
        satz1vec <- colSums(tvectors[used1, ])
      }
      if (length(used1) == 1) {
        satz1vec <- (tvectors[used1, ])
      }
      if (length(used1) == 0) {
        return(warning("no element of x found in rownames(tvectors)"))
      }
      near <- names(neighbors(satz1vec, n, tvectors = tvectors, 
                              breakdown = breakdown))
    }
    if (class(x) == "numeric") {
      satz1vec <- x
      near <- names(neighbors(x, n, tvectors = tvectors, 
                              breakdown = breakdown))
    }
    nearwords <- paste(near, collapse = " ")
    cos.near <- multicos(nearwords, tvectors = tvectors, 
                         breakdown = FALSE)
    if ((class(x) == "character")) {
      if (length(satz1split) > 1) {
        letters <- unlist(strsplit(x, ""))[1:15]
        letters <- letters[!is.na(letters)]
        expressionW <- paste(letters, sep = "", collapse = "")
        if (expressionW != x) {
          expressionW <- paste(expressionW, "[...]", 
                               sep = "")
        }
        rownames(cos.near)[1] <- expressionW
        colnames(cos.near)[1] <- expressionW
      }
    }
    if (class(x) == "numeric") {
      rownames(cos.near)[1] <- "Input Vector"
      colnames(cos.near)[1] <- "Input Vector"
    }
    if (method == "PCA") {
      pca1 <- princomp(covmat = cos.near)
      L <- loadings(pca1) %*% diag(pca1$sdev)
      Lt <- varimax(L[, 1:dims])$loadings
    }
    if (method == "MDS") {
      dissim <- 1 - cos.near
      mds1 <- cmdscale(dissim, eig = TRUE, k = dims)
      Lt <- mds1$points
    }
    Lt <- as.data.frame(Lt[, ])
    if (dims == 2) {
      colnames(Lt) <- c("x", "y")
    }
    if (dims == 3) {
      colnames(Lt) <- c("x", "y", "z")
    }
    Lt$words <- rownames(Lt)
    Lt$words2 <- iconv(Lt$words, to = "ASCII//TRANSLIT")
    if (dims == 2) {
      plot(Lt$x, Lt$y, xlab = "Dimension 1", ylab = "Dimension 2", 
           pch = 20, type = "n", xlim = c(min(Lt$x) - 0.1, 
                                          max(Lt$x) + 0.1), ylim = c(min(Lt$y) - 0.1, 
                                                                     max(Lt$y) + 0.1))
      with(Lt, points(x, y, cex = 0.6, pch = 20))
      
      if (!replace_with_image) {
        with(Lt, text(x, y, words2, cex = cex))
      } else {
      
        scale <- 0.0005
        
        for (i in 1:length(Lt$words2)) {
          img <- get_image_by_id(nishimoto_images_folder, as.integer(Lt$words2[i]))
          img <- image_scale(img, "80")
          img_width <- dim(image_data(img))[2]
          img_height <- dim(image_data(img))[3]
          
          rasterImage(img, xleft = Lt$x[i]-scale*img_width , xright=Lt$x[i]+scale*img_width,
                      ytop = Lt$y[i]+scale*3*img_height, ybottom = Lt$y[i]-scale*3*img_height, interpolate=FALSE)
          
        }
      }
    }
    if (dims == 3) {
      par3d(windowRect = c(20, 30, size[1] + 20, size[2] + 
                             30))
      with(Lt, plot3d(x, y, z, box = box, axes = axes, 
                      xlab = "", ylab = "", zlab = "", xakt = "n", 
                      yakt = "n", zakt = "n", col = "black", ...))
      
      with(Lt, text3d(x, y, z, words2))
      
      if (length(alpha) == 1) {
        alpha <- rep(alpha, 2)
      }
      if (length(col) == 1) {
        col <- rep(col, 2)
      }
      if (alpha[1] == "shade") {
        alpha <- rep("graded", 2)
      }
      palette1 <- rainbow(101, start = 0, end = 0.95)
      palette2 <- rainbow(101, start = 0, end = 0.95)
      if (col[1] == "heat.colors") {
        palette1 <- heat.colors(101)
      }
      if (col[1] == "terrain.colors") {
        palette1 <- terrain.colors(101)
      }
      if (col[1] == "topo.colors") {
        palette1 <- topo.colors(101)
      }
      if (col[1] == "cm.colors") {
        palette1 <- topo.colors(101)
      }
      if (col[2] == "heat.colors") {
        palette2 <- heat.colors(101)
      }
      if (col[2] == "terrain.colors") {
        palette2 <- terrain.colors(101)
      }
      if (col[2] == "topo.colors") {
        palette2 <- topo.colors(101)
      }
      if (col[2] == "cm.colors") {
        palette2 <- topo.colors(101)
      }
      if (length(col) >= 3) {
        palette1 <- colorRampPalette(col)(101)
        palette2 <- colorRampPalette(col)(101)
      }
      if (is.numeric(connect.lines) & connect.lines > 
          (nrow(Lt) - 1)) {
        stop("cannot plot more connecting lines than number of points minus one")
      }
      if (connect.lines == "all") {
        comb <- combn(rownames(Lt), m = 2)
        combs <- c(combn(rownames(Lt), m = 2))
        pwsim <- cos.near[lower.tri(cos.near)]
        pwsim2 <- rep(pwsim, each = 2)
        segm <- Lt[combs, ]
        segm$pwsim <- pwsim2
        colour <- palette2[round(pos(pwsim2 * 100) + 
                                   1)]
        segm$colour <- colour
        if (alpha[2] == "graded") {
          if (!(col[2] %in% c("rainbow", "heat.colors", 
                              "terrain.colors", "topo.colors", "cm.colors") | 
                length(col) >= 3)) {
            suppressWarnings(segments3d(segm, alpha = alpha.grade * 
                                          (segm$pwsim)^2, col = col[2]))
          }
          if (col[2] %in% c("rainbow", "heat.colors", 
                            "terrain.colors", "topo.colors", "cm.colors") | 
              length(col) >= 3) {
            suppressWarnings(segments3d(segm, alpha = alpha.grade * 
                                          (segm$pwsim)^2, col = segm$colour))
          }
        }
        if (is.numeric(alpha[2])) {
          if (!(col[2] %in% c("rainbow", "heat.colors", 
                              "terrain.colors", "topo.colors", "cm.colors") | 
                length(col) >= 3)) {
            suppressWarnings(segments3d(segm, alpha = alpha[2], 
                                        col = col[2]))
          }
          if (col[2] %in% c("rainbow", "heat.colors", 
                            "terrain.colors", "topo.colors", "cm.colors") | 
              length(col) >= 3) {
            suppressWarnings(segments3d(segm, alpha = alpha[2], 
                                        col = segm$colour))
          }
        }
        if (legend == T) {
          bgplot3d({
            par(mar = c(0, 0, 0, 0))
            par(oma = c(0, 0, 0, 0))
            if (col[2] %in% c("rainbow", "heat.colors", 
                              "terrain.colors", "topo.colors", "cm.colors") | 
                length(col) >= 3) {
              legend_image <- as.raster(matrix(rev(palette2), 
                                               ncol = 1))
            }
            if (!(col[2] %in% c("rainbow", "heat.colors", 
                                "terrain.colors", "topo.colors", "cm.colors") | 
                  length(col) >= 3)) {
              legend_image <- as.raster(matrix(rep(col[2], 
                                                   101), ncol = 1))
            }
            plot(c(0, 2), c(0, 1), type = "n", axes = F, 
                 xlab = "", ylab = "")
            text(x = 0.3, y = seq(0.1, 0.35, l = 5), 
                 labels = seq(0, 1, l = 5), cex = 1)
            rasterImage(legend_image, 0.1, 0.1, 0.22, 
                        0.35)
            text(0.18, 0.05, "cosine similarity")
          })
        }
      }
      if (class(connect.lines) == "numeric" && connect.lines > 
          0) {
        which.indices <- t(apply(cos.near, 1, order, 
                                 decreasing = T)[1:(connect.lines + 1), ])
        which.indices <- as.data.frame(which.indices[, 
                                                     -1])
        pre <- as.vector(t(which.indices))
        alternate <- rep((1:n), each = connect.lines)
        indices <- c(rbind(alternate, pre))
        segm <- Lt[indices, ]
        pwsim <- vector(length = nrow(segm)/2)
        for (i in seq(1, nrow(segm), 2)) {
          pwsim[ceiling(i/2)] <- cos.near[segm[i, "words"], 
                                          segm[(i + 1), "words"]]
        }
        pwsim2 <- rep(pwsim, each = 2)
        segm$pwsim <- pwsim2
        colour <- palette2[round(pos(pwsim2 * 100) + 
                                   1)]
        segm$colour <- colour
        if (alpha[2] == "graded") {
          if (!(col[2] %in% c("rainbow", "heat.colors", 
                              "terrain.colors", "topo.colors", "cm.colors") | 
                length(col) >= 3)) {
            suppressWarnings(segments3d(segm, alpha = alpha.grade * 
                                          (segm$pwsim)^2, col = col[2]))
          }
          if (col[2] %in% c("rainbow", "heat.colors", 
                            "terrain.colors", "topo.colors", "cm.colors") | 
              length(col) >= 3) {
            suppressWarnings(segments3d(segm, alpha = alpha.grade * 
                                          (segm$pwsim)^2, col = segm$colour))
          }
        }
        if (is.numeric(alpha[2])) {
          if (!(col[2] %in% c("rainbow", "heat.colors", 
                              "terrain.colors", "topo.colors", "cm.colors") | 
                length(col) >= 3)) {
            suppressWarnings(segments3d(segm, alpha = alpha[2], 
                                        col = col[2]))
          }
          if (col[2] %in% c("rainbow", "heat.colors", 
                            "terrain.colors", "topo.colors", "cm.colors") | 
              length(col) >= 3) {
            suppressWarnings(segments3d(segm, alpha = alpha[2], 
                                        col = segm$colour))
          }
        }
      }
      if (start.lines == T && connect.lines != "all") {
        steps <- vector(length = (2 * n))
        steps[seq(1, 2 * n - 1, 2)] <- rep(1, n)
        steps[seq(2, 2 * n, 2)] <- 1:(n)
        segm0 <- Lt[steps, ]
        pwsim0 <- rep(cos.near[1, ], each = 2)
        segm0$pwsim <- pwsim0
        colour0 <- palette1[round(pos(pwsim0 * 100) + 
                                    1)]
        segm0$colour <- colour0
        if (!(col[1] %in% c("rainbow", "heat.colors", 
                            "terrain.colors", "topo.colors", "cm.colors") | 
              length(col) >= 3) && is.numeric(alpha[1])) {
          suppressWarnings(segments3d(segm0, alpha = alpha[1], 
                                      col = col[1]))
        }
        if (!(col[1] %in% c("rainbow", "heat.colors", 
                            "terrain.colors", "topo.colors", "cm.colors") | 
              length(col) >= 3) && alpha[1] == "graded") {
          suppressWarnings(segments3d(segm0, alpha = alpha.grade * 
                                        (segm0$pwsim)^2, col = col[1]))
        }
        if ((col[1] %in% c("rainbow", "heat.colors", 
                           "terrain.colors", "topo.colors", "cm.colors") | 
             length(col) >= 3) && is.numeric(alpha[1])) {
          suppressWarnings(segments3d(segm0, alpha = alpha[1], 
                                      col = segm0$colour))
        }
        if ((col[1] %in% c("rainbow", "heat.colors", 
                           "terrain.colors", "topo.colors", "cm.colors") | 
             length(col) >= 3) && alpha[1] == "graded") {
          suppressWarnings(segments3d(segm0, alpha = alpha.grade * 
                                        (segm0$pwsim)^2, col = segm0$colour))
        }
      }
    }
    Lt[, -which(colnames(Lt) %in% c("words", "words2"))]
  }
  else {
    warning("tvectors must be a matrix!")
  }
}

pos <- function (x) 
{
  x[x < 0] <- 0
  return(x)
}
