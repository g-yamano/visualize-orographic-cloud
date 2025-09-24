###################################################
# Title: visualize Temperature Rscript with Topography
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
temp_palette <- colorRampPalette(c("purple", "blue", "darkgreen", 
                                   "green", "white", "yellow", 
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
Tem_K <- ncvar_get(ncin, "T", collapse_degen = FALSE)
topo <- ncvar_get(ncin, "topo")
nc_close(ncin)

# Data processing
Tem_C <- Tem_K - 273.15
temp_min <- floor(min(Tem_C))
temp_max <- ceiling(max(Tem_C))
dimnames(Tem_C)[[4]] <- alltimes

x_km <- x * 10^(-3)
z_km <- z * 10^(-3) 

topo_slice_km <- topo[, center_y_index] * 10^(-3)


# make plot function
plot_temperature_slice <- function(time_val, slice_data, topo_data) {
  
  base_filename <- paste("Temperature_XZ.", sprintf("%05d", as.numeric(time_val)), ".pdf", sep = "")
  pdf_filename <- file.path(output_dir, base_filename)
  
  pdf(pdf_filename, width = pdf_width, height = pdf_height)
  
  # plot contour
  fields::image.plot(x_km, z_km, slice_data,
        col = temp_palette,
        zlim = c(temp_min, temp_max),
        main = paste("Temperature [degree ] (Time =", time_val, "s)"),
        xlab = "X [km]",
        ylab = "Z [km]", 
        xaxt = "n",
        yaxt = "n"
  )
  
  polygon(c(x_km, rev(x_km)), c(topo_data, rep(min(z_km), length(x_km))),
          col = 'grey', border = NA)
  
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

if (output_alltime) {
  for (time in alltimes) {
    cat(sprintf("processing time = %s [s]\n", time))
    temp_slice <- Tem_C[, center_y_index, , as.character(time)]
    plot_temperature_slice(time_val = time, slice_data = temp_slice, topo_data = topo_slice_km)
  }
} else {
  last_time <- tail(alltimes, 1)
  cat(sprintf("processing last time = %s [s]\n", last_time))
  temp_slice <- Tem_C[, center_y_index, , as.character(last_time)]
  plot_temperature_slice(time_val = last_time, slice_data = temp_slice, topo_data = topo_slice_km)
}

cat(paste("All plots saved in '", output_dir, "' directory.\n", sep=""))