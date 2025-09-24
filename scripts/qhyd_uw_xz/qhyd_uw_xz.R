###################################################
# Title: visualize QHYD, U, W Rscript 
# Author: Gaku YAMANO
# Date: 2025/08/22
###################################################

# load packages
library(ncdf4)
library(fields)
source("../../config.R")

#######################################################
#################### Setting items ####################
#######################################################
# Specify the color palette
QHYD_palette <- colorRampPalette(c("white", "blue", 
                                   "green", "yellow", 
                                   "orange", "red"))(500)

############################################################
#################### Processing Part #######################
############################################################
ncin <- nc_open(input_file)
alltimes <- ncvar_get(ncin, "time")
x <- ncvar_get(ncin, "x")
y <- ncvar_get(ncin, "y")
z <- ncvar_get(ncin, "z")
center_y_index <- as.integer(length(y) / 2)
QHYD <- ncvar_get(ncin, "QHYD", collapse_degen = FALSE)
U <- ncvar_get(ncin, "U", collapse_degen = FALSE)
W <- ncvar_get(ncin, "W", collapse_degen = FALSE)
nc_close(ncin)

# Data processing
QHYD_min <- 0
QHYD_max <- max(QHYD, na.rm=TRUE)
dimnames(QHYD)[[4]] <- alltimes
dimnames(U)[[4]] <- alltimes
dimnames(W)[[4]] <- alltimes

x_km <- x * 10^(-3)
z_km <- z * 10^(-3) 

# make plot function
plot_QHYD_slice <- function(time_val, QHYD_slice, U_slice, W_slice, x_start_mat, z_start_mat, x_indices, z_indices) {
  
  base_filename <- paste("qhyd_uw_xz", sprintf("%05d", as.numeric(time_val)), ".pdf", sep = "")
  pdf_filename <- file.path(output_dir, base_filename)
  
  pdf(pdf_filename, width = pdf_width, height = pdf_height)
  
  # plot contour
  fields::image.plot(x_km, z_km, QHYD_slice * 1000,
                     col = QHYD_palette,
                     zlim = c(-2, 3),
                     main = paste("QHYD U&W (Time =", time_val, "s)"),
                     xlab = "X [km]",
                     ylab = "Z [km]", 
                     xaxt = "n",
                     yaxt = "n"
  )
  
  arrows(x_start_mat, z_start_mat, 
         x_start_mat + U_slice[x_indices, z_indices], 
         z_start_mat + W_slice[x_indices, z_indices], 
         length = 0.05,
         col = rgb(0, 0, 0, alpha = 0.3))
  
  # plot axis
  x_ticks <- pretty(range(x_km), n = 10) 
  axis(side = 1, at = x_ticks, labels = sprintf("%.1f", x_ticks))
  y_ticks <- pretty(range(z_km), n = 8) 
  axis(side = 2, at = y_ticks, labels = sprintf("%.1f", y_ticks), las = 1)
  
  # close PDF device
  dev.off()
}

# plot processing
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

skip <- 5 
x_indices <- seq(1, length(x_km), by = skip)
z_indices <- seq(1, length(z_km), by = skip)

x_start_mat <- matrix(x_km[x_indices], nrow = length(x_indices), ncol = length(z_indices), byrow = FALSE)
z_start_mat <- matrix(z_km[z_indices], nrow = length(x_indices), ncol = length(z_indices), byrow = TRUE)

if (output_alltime) {
  for (time in alltimes) {
    cat(sprintf("processing time = %s [s]\n", time))
    QHYD_slice <- QHYD[, center_y_index, , as.character(time)]
    U_slice <- U[, center_y_index, , as.character(time)]
    W_slice <- W[, center_y_index, , as.character(time)]
    plot_QHYD_slice(time_val = time, QHYD_slice = QHYD_slice, U_slice = U_slice, W_slice = W_slice, x_start_mat = x_start_mat, z_start_mat = z_start_mat, x_indices = x_indices, z_indices = z_indices)
  }
} else {
  last_time <- tail(alltimes, 1)
  cat(sprintf("processing last time = %s [s]\n", last_time))
  QHYD_slice <- QHYD[, center_y_index, , as.character(last_time)]
  U_slice <- U[, center_y_index, , as.character(last_time)]
  W_slice <- W[, center_y_index, , as.character(last_time)]
  plot_QHYD_slice(time_val = last_time, QHYD_slice = QHYD_slice, U_slice = U_slice, W_slice = W_slice, x_start_mat = x_start_mat, z_start_mat = z_start_mat, x_indices = x_indices, z_indices = z_indices)
}

cat(paste("All plots saved in '", output_dir, "' directory.\n", sep=""))
