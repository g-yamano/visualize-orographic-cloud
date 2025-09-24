###################################################
# Title: visualize RH Rscript with Topography
# Author: Gaku YAMANO
# Date: 2025/08/18
# Modified: 2025/09/24
###################################################

# load packages
library(ncdf4)
library(fields)
source("../../config.R")

#######################################################
#################### Setting items ####################
#######################################################
# Specify the color palette
rh_palette <- colorRampPalette(c("white", "lightblue", 
                                 "blue", "green", 
                                 "yellow", "orange"))(500)

############################################################
#################### Processing Part #######################
############################################################
ncin <- nc_open(input_file)
alltimes <- ncvar_get(ncin, "time")
x <- ncvar_get(ncin, "x")
y <- ncvar_get(ncin, "y")
z <- ncvar_get(ncin, "z")
topo <- ncvar_get(ncin, "topo") 
center_y_index <- as.integer(length(y) / 2)
rh <- ncvar_get(ncin, "RH", collapse_degen = FALSE)
nc_close(ncin)

# Data processing
rh_min <- min(rh)
rh_max <- max(rh)
dimnames(rh)[[4]] <- alltimes

x_km <- x * 10^(-3)
z_km <- z * 10^(-3) 

section_altitude <- topo[, center_y_index]
topo_poly_x <- c(x_km, rev(x_km))
topo_poly_z <- c(section_altitude * 10^(-3), rep(min(z_km), length(section_altitude)))


plot_rh_slice <- function(time_val, slice_data, topo_x, topo_z) {
  
  base_filename <- paste("rh_topo_xz.", sprintf("%05d", as.numeric(time_val)), ".pdf", sep = "")
  pdf_filename <- file.path(output_dir, base_filename)
  
  pdf(pdf_filename, width = pdf_width, height = pdf_height)
  
  # plot contour
  fields::image.plot(x_km, z_km, slice_data,
        col = rh_palette,
        zlim = c(rh_min, rh_max),
        main = paste("Relative Humidity (Time =", time_val, "s)"),
        xlab = "X [km]",
        ylab = "Z [km]", 
        xaxt = "n",
        yaxt = "n"
  )
  
  # plot axis
  x_ticks <- pretty(range(x_km), n = 10) 
  axis(side = 1, at = x_ticks, labels = sprintf("%.1f", x_ticks))
  y_ticks <- pretty(range(z_km), n = 8) 
  axis(side = 2, at = y_ticks, labels = sprintf("%.1f", y_ticks), las = 1)
  
  polygon(topo_x, topo_z, col = "gray40", border = "gray70")
  
  # close PDF device
  dev.off()
}

# plot processing
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

if (output_alltime) {
  for (time in alltimes) {
    cat(sprintf("processing time = %s [s]\n", time))
    rh_slice <- rh[, center_y_index, , as.character(time)]
    plot_rh_slice(time_val = time, slice_data = rh_slice, topo_x = topo_poly_x, topo_z = topo_poly_z)
  }
} else {
  last_time <- tail(alltimes, 1)
  cat(sprintf("processing last time = %s [s]\n", last_time))
  rh_slice <- rh[, center_y_index, , as.character(last_time)]
  plot_rh_slice(time_val = last_time, slice_data = rh_slice, topo_x = topo_poly_x, topo_z = topo_poly_z)
}

cat(paste("All plots saved in '", output_dir, "' directory.\n", sep=""))