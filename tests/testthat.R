# Run testthat tests for biobot_mwra project

library(testthat)

# Source all R files from the project
r_files <- list.files(
  file.path(dirname(dirname(getwd())), "R"),
  pattern = "\\.R$",
  full.names = TRUE
)

# Source utility functions first, then others
utils_file <- r_files[grepl("utils\\.R$", r_files)]
other_files <- r_files[!grepl("utils\\.R$", r_files)]

for (f in c(utils_file, other_files)) {
  source(f)
}

test_check("biobot_mwra")
