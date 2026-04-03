# --- CONFIG ---------------------------------------------------------------
qmd_list_path <- "qmd_list.txt"   # path to the list you created

# --- UTILITIES ------------------------------------------------------------
read_file <- function(path) {
  # Read file to a single string, UTF-8 safe
  txt <- tryCatch(paste(readLines(path, warn = FALSE, encoding = "UTF-8"),
                        collapse = "\n"),
                  error = function(e) "") 
  txt
}

strip_yaml_front_matter <- function(txt) {
  # Remove leading YAML front matter: --- ... ---
  # Only if it appears at the very start of the file.
  sub("^---[\\s\\S]*?---\\s*", "", txt, perl = TRUE)
}

strip_code_fences <- function(txt) {
  # Remove fenced code blocks ``` ... ``` (any language)
  gsub("(?s)```.*?```", "", txt, perl = TRUE)
}

strip_inline_code <- function(txt) {
  # Remove inline code `...`
  gsub("`[^`]*`", "", txt, perl = TRUE)
}

strip_math <- function(txt) {
  # Remove all math environments (strict):
  # $$...$$, $...$, \(...\), \[...\]
  # Do display first, then inline
  txt <- gsub("(?s)\\$\\$.*?\\$\\$", "", txt, perl = TRUE)
  txt <- gsub("(?s)\\\\\\(.*?\\\\\\)", "", txt, perl = TRUE)
  txt <- gsub("(?s)\\\\\\[.*?\\\\\\]", "", txt, perl = TRUE)
  # Inline $...$ (not spanning newlines)
  txt <- gsub("\\$[^\\$\\n]*\\$", "", txt, perl = TRUE)
  txt
}

strip_md_links <- function(txt) {
  # Remove Markdown links: label
  # Basic, robust variant
  gsub("\\[[^\\]]*\\]\\([^\\)]*\\)", "", txt, perl = TRUE)
}

extract_bracket_terms <- function(txt) {
  # Extract [ ... ] terms after stripping links & math
  # Returns a character vector of matches (without brackets)
  m <- gregexpr("\\[([^\\]]+)\\]", txt, perl = TRUE)
  if (length(m) == 0 || m[[1]][1] == -1) return(character())
  regmatches(txt, m)[[1]] |>
    sub("^\\[", "", x = _) |>
    sub("\\]$", "", x = _)
}

get_folder <- function(path) {
  # path like "./studyguides/intro.qmd" -> "studyguides"
  p <- sub("^\\./", "", path)
  seg <- strsplit(p, "/")[[1]]
  if (length(seg) >= 2) seg[1] else "."
}

# --- LOAD LIST ------------------------------------------------------------
if (!file.exists(qmd_list_path)) {
  stop("Could not find ", qmd_list_path, " in the working directory.")
}
paths <- readLines(qmd_list_path, warn = FALSE)
paths <- paths[nzchar(paths)]
paths <- unique(paths)

# --- PROCESS --------------------------------------------------------------
per_file <- list()
folder_index <- split(paths, vapply(paths, get_folder, character(1)))

cat("Found", length(paths), "QMD files across",
    length(folder_index), "folders.\n\n")

total_terms_found <- 0L

for (folder in names(folder_index)) {
  files <- folder_index[[folder]]
  folder_terms_count <- 0L
  
  for (fp in files) {
    txt <- read_file(fp)
    if (!nzchar(txt)) next
    
    # Clean text
    txt <- strip_yaml_front_matter(txt)
    txt <- strip_code_fences(txt)
    txt <- strip_inline_code(txt)
    txt <- strip_math(txt)
    txt <- strip_md_links(txt)
    
    # Extract terms
    terms <- extract_bracket_terms(txt)
    
    if (length(terms)) {
      # Count within this file
      tbl <- sort(table(terms))
      df  <- data.frame(filename = fp,
                        term = names(tbl),
                        count_in_file = as.integer(tbl),
                        row.names = NULL,
                        check.names = FALSE)
      per_file[[length(per_file) + 1]] <- df
      folder_terms_count <- folder_terms_count + sum(tbl)
    }
  }
  
  total_terms_found <- total_terms_found + folder_terms_count
  cat(sprintf("Folder: %-24s | files: %3d | terms found: %5d\n",
              folder, length(files), folder_terms_count))
}
cat(sprintf("\nTOTAL terms found across all folders: %d\n", total_terms_found))

# --- AGGREGATE & WRITE ----------------------------------------------------
# Per-file with counts
if (length(per_file)) {
  per_file_df <- do.call(rbind, per_file)
} else {
  per_file_df <- data.frame(filename = character(),
                            term = character(),
                            count_in_file = integer(),
                            check.names = FALSE)
}

# Global counts
if (nrow(per_file_df)) {
  unique_terms_df <- aggregate(count_in_file ~ term,
                               data = per_file_df, FUN = sum)
  names(unique_terms_df) <- c("term", "global_count")
  unique_terms_df <- unique_terms_df[order(tolower(unique_terms_df$term),
                                           unique_terms_df$term), ]
} else {
  unique_terms_df <- data.frame(term = character(),
                                global_count = integer(),
                                check.names = FALSE)
}

# Inventory: files per folder
inv <- data.frame(folder = names(folder_index),
                  n_files = vapply(folder_index, length, integer(1)),
                  row.names = NULL, check.names = FALSE)
inv <- inv[order(inv$folder), ]

# Write CSVs
write.csv(per_file_df, "terms_by_file_with_counts.csv", row.names = FALSE)
write.csv(unique_terms_df, "unique_terms_with_counts.csv", row.names = FALSE)
write.csv(inv, "inventory_files_per_folder.csv", row.names = FALSE)

cat("\nWrote:\n- terms_by_file_with_counts.csv\n- unique_terms_with_counts.csv\n- inventory_files_per_folder.csv\n")
