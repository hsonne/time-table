#
# Read teacher timetables for Eduard-Moerike-Grundschule
# Source the whole script first
#

# MAIN -------------------------------------------------------------------------
if (FALSE)
{
  teacher_tables <- read_teacher_time_tables(
    file = "C:/Users/hsonne/Downloads/Lehrer Neu 2024-2025.pdf"
  )
  
  teacher_data <- dplyr::bind_rows(.id = "teacher", lapply(teacher_tables, function(x) {
    as.data.frame(tidyr::pivot_longer(
      x, 
      names(x)[-1L], 
      names_to = "weekday", 
      values_to = "class_room_subject"
    ))
  }))
  
  teacher_data <- teacher_data[teacher_data$class_room_subject != "", ]

  lookup <- create_lookup_table(composed = sort(unique(teacher_data$class_room_subject)))

  full_teacher_data <- dplyr::left_join(
    x = teacher_data, 
    y = lookup, 
    by = c(class_room_subject = "composed")
  )
  
  View(full_teacher_data)
  
  by_class <- split(
    kwb.utils::removeColumns(full_teacher_data, c("class", "class_room_subject")),
    f = full_teacher_data$class
  )
  
  x <- by_class$`1A`
  x$weekday <- factor(x$weekday, levels = c("Mo", "Di", "Mi", "Do", "Fr"))

  kwb.utils::moveColumnsToFront(
    kwb.utils::orderBy(x, c("weekday", "Std")),
    c("weekday", "Std", "subject", "room")
  )
  
}

# read_teacher_time_tables -----------------------------------------------------
read_teacher_time_tables <- function(file)
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
    kwb.utils::renameColumns(data, list(X = "Std"))
  })

  names(tables) <- sapply(clean_pages, kwb.utils::selectElements, "teacher")
  
  # Which tables look already good?  
  failed <- !sapply(tables, looks_good)
  
  tables[failed] <- lapply(tables[failed], repair_table)
  
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
  indices <- which(x[, 1L] == "")
  stopifnot(length(indices) == 2L)
  stopifnot(diff(indices) == 2L)
  
  which_filled <- which(as.matrix(x[indices, ]) != "", arr.ind = TRUE)
  stopifnot(nrow(which_filled) == 2L)
  cols <- unname(which_filled[, "col"])
  stopifnot(kwb.utils::allAreEqual(cols))
  i <- indices[1L] + 1L
  j <- cols[1L]
  old <- x[i, j]
  stopifnot(old == "")
  x[i, j] <- paste(x[indices, j], collapse = "/")
  x[-indices, ]
}

# create_lookup_table ----------------------------------------------------------
create_lookup_table <- function(composed)
{
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
    data.frame(composed = composed[is_class_triple]),
    as.data.frame(matrix(
      unlist(parts[is_class_triple]), 
      ncol = 3L, 
      byrow = TRUE, 
      dimnames = list(NULL, c("class", "room", "subject"))
    ))
  )

  lookup_class_pair <- cbind(
    data.frame(composed = composed[is_class_pair]),
    as.data.frame(matrix(
      unlist(parts[is_class_pair]), 
      ncol = 2L, 
      byrow = TRUE, 
      dimnames = list(NULL, c("class", "subject"))
    ))
  )

  lookup_room_pair <- cbind(
    data.frame(composed = composed[is_room_pair]),
    as.data.frame(matrix(
      unlist(parts[is_room_pair]), 
      ncol = 2L, 
      byrow = TRUE, 
      dimnames = list(NULL, c("room", "subject"))
    ))
  )

  subjects <- sort(unique(lookup_class_triple$subject))
  first_is_subject <- sapply(parts, `[`, 1L) %in% subjects
  is_subject_only <- n_parts == 1L & first_is_subject
  
  lookup_subject_only <- cbind(
    data.frame(composed = composed[is_subject_only]),
    as.data.frame(matrix(
      unlist(parts[is_subject_only]), 
      ncol = 1L, 
      byrow = TRUE, 
      dimnames = list(NULL, c("subject"))
    ))
  )
  
  considered <- is_class_triple | is_class_pair | is_room_pair | is_subject_only
  print(parts[!considered])

  dplyr::bind_rows(
    lookup_class_triple, 
    lookup_class_pair, 
    lookup_room_pair,
    lookup_subject_only
  )
}
