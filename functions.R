# get_full_teacher_table -------------------------------------------------------
get_full_teacher_table <- function(file)
{
  teacher_tables <- read_time_tables(file)
  teacher_data_raw <- merge_tables(teacher_tables, id = "teacher")
  teacher_data <- split_composed_fields(teacher_data_raw)
  lookup_teacher <- create_lookup_table_teacher(composed = teacher_data$composed)
  dplyr::left_join(teacher_data, lookup_teacher, by = "composed")
}

# get_full_class_table ---------------------------------------------------------
get_full_class_table <- function(file)
{
  class_tables <- read_time_tables(file)
  class_data_raw <- merge_tables(class_tables, id = "class")
  class_data <- split_composed_fields(class_data_raw)
  lookup_class <- create_lookup_table_class(composed = gsub("ESSEN (\\d)", "ESSEN-\\1", class_data$composed))
  result <- dplyr::left_join(class_data, lookup_class, by = "composed")
  result$weekday <- to_weekday_factor(result$weekday)
  result <- kwb.utils::moveColumnsToFront(
    result, 
    c("class", "weekday", "hr", "subject", "teacher", "room")
  )
  kwb.utils::fullySorted(result)
}

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
  #x <- tables[[1L]]
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

# create_lookup_table_teacher --------------------------------------------------
create_lookup_table_teacher <- function(composed)
{
  composed <- sort(unique(composed))
  parts <- strsplit(composed, "\\s+")
  n_parts <- lengths(parts)
  
  tokens <- sort(unique(unlist(parts)))
  classes <- grep("\\d[A-Z]|AB\\d", tokens, value = TRUE)
  rooms <- grep("^R_", tokens, value = TRUE)
  
  first_is_class <- sapply(parts, `[`, 1L) %in% classes
  first_is_room <- sapply(parts, `[`, 1L) %in% rooms
  
  is_class_triple <- n_parts == 3L & first_is_class
  is_class_pair <- n_parts == 2L & first_is_class
  is_room_pair <- n_parts == 2L & first_is_room
  
  lookup_class_triple <- cbind(
    split_into(x = parts[is_class_triple], columns = c("class", "room", "subject")),
    composed = composed[is_class_triple]
  )
  
  subjects <- sort(unique(lookup_class_triple$subject))
  first_is_subject <- sapply(parts, `[`, 1L) %in% subjects
  is_subject_only <- n_parts == 1L & first_is_subject
  
  considered <- is_class_triple | is_class_pair | is_room_pair | is_subject_only
  print(parts[!considered])
  
  combined <- dplyr::bind_rows(
    lookup_class_triple, 
    cbind(
      split_into(x = parts[is_class_pair], columns = c("class", "subject")),
      composed = composed[is_class_pair]
    ),
    cbind(
      split_into(x = parts[is_room_pair], columns = c("room", "subject")),
      composed = composed[is_room_pair]
    ),
    cbind(
      split_into(x = parts[is_subject_only], columns = c("subject")),
      composed = composed[is_subject_only]
    )
  )
  
  kwb.utils::moveColumnsToFront(combined, "composed")
}

# create_lookup_table_class ----------------------------------------------------
create_lookup_table_class <- function(composed)
{
  composed <- sort(unique(composed))
  parts <- strsplit(composed, "\\s+")
  n_parts <- lengths(parts)
  
  is_single <- n_parts == 1L  
  is_pair <- n_parts == 2L
  is_triple <- n_parts == 3L
  
  combined <- dplyr::bind_rows(
    cbind(
      split_into(x = parts[is_triple], columns = c("subject", "room", "teacher")),
      composed = composed[is_triple]
    ),
    cbind(
      split_into(x = parts[is_pair], columns = c("subject", "teacher")),
      composed = composed[is_pair]
    ),
    cbind(
      split_into(x = parts[is_single], columns = c("teacher")),
      composed = composed[is_single]
    )
  )
  
  kwb.utils::moveColumnsToFront(combined, "composed")  
}

# split_into -------------------------------------------------------------------
split_into <- function(x, columns)
{
  as.data.frame(matrix(
    unlist(x), 
    ncol = length(columns), 
    byrow = TRUE, 
    dimnames = list(NULL, columns)
  ))
}

# merge_tables -----------------------------------------------------------------
merge_tables <- function(tables, id)
{
  tables_long <- lapply(tables, function(data) {
    as.data.frame(tidyr::pivot_longer(
      data, 
      names(data)[-1L], 
      names_to = "weekday", 
      values_to = "composed"
    ))
  })
  
  data <- dplyr::bind_rows(tables_long, .id = id)
  data[data$composed != "", ]
}

# split_composed_fields --------------------------------------------------------
split_composed_fields <- function(data)
{
  multi_field_indices <- grep("/", data$composed)
  if (length(multi_field_indices) == 0L) {
    return(data)
  }
  parts <- strsplit(data$composed[multi_field_indices], "/")
  repeated <- kwb.utils::removeColumns(
    data[rep(multi_field_indices, lengths(parts)), ],
    "composed"
  )
  repeated$composed <- unlist(parts)
  combined <- rbind(data[-multi_field_indices, ], repeated)
  kwb.utils::resetRowNames(combined)
}

# to_weekday_factor ------------------------------------------------------------
to_weekday_factor <- function(x)
{
  factor(x, levels = c("Mo", "Di", "Mi", "Do", "Fr"))
}
