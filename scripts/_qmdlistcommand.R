# Generate a list of all .qmd files in the repo
files <- list.files(path = ".", pattern = "\\.qmd$", recursive = TRUE, full.names = TRUE)

# Write them to qmd_list.txt
writeLines(files, "qmd_list.txt")

