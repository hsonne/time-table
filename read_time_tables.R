# read_time_tables -------------------------------------------------------------
read_time_tables <- function(file)
{
  # Read pdf file into list of pages
  pages <- suppressMessages(pdftools::pdf_text(file))
  
  # Clean the pages
  clean_pages <- lapply(pages, clean_page)
  
  # Convert strings to matrices of single characters
  char_matrices <- lapply(clean_pages, function(x) get_char_matrix(x$table))
  
  # Convert back to strings, however with vertical separator lines
  string_tables <- lapply(char_matrices, function(m) {
    txt <- apply(insert_separators(m), 1L, paste, collapse = "")
    txt <- gsub("([^|])\\|([^|])", "\\1 \\2", txt)
    gsub("\\|+", "|", txt)
  })
  
  # Convert good looking string tables into clean data frames
  tables <- lapply(string_tables, function(text) {
    data <- read.table(text = text, sep = "|", header = TRUE)
    data[] <- lapply(data, function(x) {
      x[is.na(x)] <- ""
      trimws(x)
    })
    names(data) <- substr(names(data), 1L, 2L)
    kwb.utils::renameColumns(data, list(X = "hr"))
  })
  
  names(tables) <- sapply(clean_pages, kwb.utils::selectElements, "teacher")
  
  failed <- !sapply(tables, looks_good)
  tables[failed] <- lapply(tables[failed], repair_table)
  stopifnot(all(sapply(tables, looks_good)))
  
  tables
}

# clean_page -------------------------------------------------------------------
clean_page <- function(page)
{
  x <- strsplit(page, "\n")[[1L]]
  stopifnot(grepl("^Eduard-", x[1L]))
  stopifnot(grepl("^D-12059", x[2L]))
  teacher <- trimws(x[3L])
  x <- x[-(1:3)]
  x <- x[x != ""]
  list(teacher = teacher, table = x)
}

# get_char_matrix --------------------------------------------------------------
get_char_matrix <- function(x)
{
  chars_in_row <- strsplit(x, "")
  max_chars <- max(lengths(chars_in_row))
  m <- do.call(rbind, lapply(chars_in_row, function(x) {
    length(x) <- max_chars
    x[is.na(x)] <- " "
    x
  }))
  m[, -1L]
}

# insert_separators ------------------------------------------------------------
insert_separators <- function(m, sep = "|")
{
  is_empty <- colSums(m == " ") == nrow(m)
  m[, is_empty] <- sep
  m
}

# looks_good -------------------------------------------------------------------
looks_good <- function(x)
{
  look_for <- c(1:2, "HP1", 3:4,"HP2", 5:9)
  nrow(x) == length(look_for) && all(x[, 1L] == look_for)
}

# repair_table -----------------------------------------------------------------
repair_table <- function(x)
{
  all_indices <- which(x[, 1L] == "")
  stopifnot(length(all_indices) %% 2L == 0L)
  index_pairs <- matrix(all_indices, nrow = 2L)
  stopifnot(all(index_pairs[2L, ] - index_pairs[1L, ] == 2L))
  
  for (pair_index in seq_len(ncol(index_pairs))) {
    indices <- index_pairs[, pair_index]
    i1 <- indices[1L]
    i2 <- indices[2L]
    new_values <- unname(apply(x[i1:i2, ], 2L, function(x) {
      paste(x[x != ""], collapse = "/")
    }))
    x[i1 + 1L, ] <- gsub("\\s+", " ", new_values)
  }
  
  x[-all_indices, ]
}
