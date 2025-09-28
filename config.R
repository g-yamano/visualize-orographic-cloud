###################################################
# Title: Project Configuration File
# Author: Gaku YAMANO
# Date: 2025/08/26
###################################################

# visualize sounding data individually

# Define the absolute path to the project's root directory here.
project_root <- "hogehoge"  # <-- Change this to your project's root path
# ----------------

get_this_script_path <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(normalizePath(sub("--file=", "", file_arg)))
  }
  if (require(rstudioapi) && rstudioapi::isAvailable()) {
    return(normalizePath(rstudioapi::getSourceEditorContext()$path))
  }
  return(NULL)
}

# --- Settings ---
visualize_target <- "tomita08"

# --- Path Settings ---
base_name <- basename(getwd())
output_dir <- file.path(project_root, "output", base_name, visualize_target)

this_script_path <- get_this_script_path()
if (!is.null(this_script_path)) {
  output_filename <- sub(".R$", ".pdf", basename(this_script_path))
} else {
  output_filename <- paste0(base_name, ".pdf")
}

data_dir   <- file.path(project_root, "data", visualize_target)

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

input_file <- file.path(data_dir, "merged-h_history_d01.pe000000.nc")

input_files <- c(
  "tomita08" = "../../data/tomita08/merged-h_history.pe000000.nc",
  "suzuki10" = "../../data/suzuki10/restart_history_corrected.nc",
  "sn14" = "../../data/sn14/merged-h_history.pe000000.nc"
)

plot_colors <- c("#0072B2", "#EECC66", "#EECC66", "#009E73")

# --- Plot Settings ---
pdf_width    <- 10
pdf_height   <- 5

# --- Processing Settings ---
output_alltime <- FALSE
