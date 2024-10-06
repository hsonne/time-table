# read_time_tables -------------------------------------------------------------
read_time_tables <- function(file)
{
  # Read pdf file into list of pages
  pages <- suppressMessages(pdftools::pdf_text(file))
  
  # Split pages into header, caption and body
  tables <- lapply(pages, function(page) {
    page_parts <- split_page(page)
    data <- text_to_data_frame(add_separator_lines(page_parts$body))
    if (!identical(names(data), c("hr", "Mo", "Di", "Mi", "Do", "Fr"))) {
      print(data)
      stop("Unexpected columns (see console output)")
    }
    structure(data, caption = page_parts$caption)
  })

  stats::setNames(tables, sapply(tables, attr, "caption"))
}

# split_page -------------------------------------------------------------------
split_page <- function(page)
{
  x <- strsplit(page, "\n")[[1L]]
  stopifnot(identical(grep("^\\S", x), 1:2))
  list(
    header = x[1:2], 
    caption = trimws(x[3L]), 
    body = x[-(1:3)]
  )
}

# add_separator_lines ----------------------------------------------------------
add_separator_lines <- function(text, col_sep = "|", row_sep = "-")
{
  #text <- bodies[[2L]];col_sep = "|";row_sep = "-"
  
  is_empty_col <- function(m) colSums(m != " ") == 0L
  is_empty_row <- function(m) rowSums(m != " ") == 0L
  rows_to_strings <- function(m) apply(m, 1L, paste, collapse = "")

  insert_separators <- function(m_in) {
    m_out <- m_in
    m_out[which(is_empty_row(m_in)), ] <- row_sep
    m_out[, which(is_empty_col(m_in))] <- col_sep
    m_out
  }

  m <- get_char_matrix(text)
  rows_to_strings(m)
  #writeLines(rows_to_strings(insert_separators(m)))

  # Remove successive empty rows
  remove_row <- rep(FALSE, nrow(m))
  i <- which(is_empty_row(m))
  remove_row[i[c(FALSE, diff(i) == 1L)]] <- TRUE
  m <- m[!remove_row, , drop = FALSE]
  #writeLines(rows_to_strings(insert_separators(m)))
  
  # Handle column separators in the row strings, not in the matrix!
  y <- rows_to_strings(insert_separators(m))
  
  # Remove column separators at first position
  y <- gsub("^[|]+", "", y)
  
  # A single vertical line is not a true column separator!
  y <- gsub("([^|])[|]([^|])", "\\1 \\2", y)

  # Remove duplicated vertical column separators
  y <- gsub("[|]+", "|", y)
  
  # Repair row separator lines
  is_sep <- is_empty_row(m)
  y[is_sep] <- gsub(" ", "-", y[is_sep])
  
  y
}

# get_char_matrix --------------------------------------------------------------
get_char_matrix <- function(x)
{
  chars_in_row <- strsplit(x, "")
  max_chars <- max(lengths(chars_in_row))
  do.call(rbind, lapply(chars_in_row, function(x) {
    length(x) <- max_chars
    x[is.na(x)] <- " "
    x
  }))
}

# collapsed --------------------------------------------------------------------
collapsed <- function(x)
{
  paste(x, collapse = "")
}

# text_to_data_frame -----------------------------------------------------------
text_to_data_frame <- function(text, row_sep = "-", col_sep = "|")
{
  #text <- string_tables[[1L]];row_sep = "-"; col_sep = "|"
  pattern_is_sep_row <- sprintf("^[%s%s]+$", row_sep, col_sep)
  pattern_starts_with_col_sep <- sprintf("^[%s]", col_sep)
  
  stopifnot(grepl(pattern_is_sep_row, text[1L]))
  text <- gsub(pattern_starts_with_col_sep, "", text[-1L])
  
  data <- read.table(text = text, sep = col_sep, header = TRUE)
  data <- trim_columns(data)
  
  nm <- substr(names(data), 1L, 2L)
  nm[nm == "X"] <- "hr"
  
  stats::setNames(data, nm)
}

# trim_columns -----------------------------------------------------------------
trim_columns <- function(data, ...)
{
  stopifnot(is.data.frame(data))
  data[] <- lapply(data, trimws, ...)
  data
}

