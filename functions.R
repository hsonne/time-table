#file <- file_classes_3
#kwb.utils::hsOpenWindowsExplorer(file)

# get_full_teacher_table -------------------------------------------------------
get_full_teacher_table <- function(file)
{
  tables <- read_time_tables(file)
  teacher_tables <- repair_tables(tables)
  teacher_data_raw <- merge_tables(teacher_tables, id = "teacher")
  teacher_data <- split_composed_fields(teacher_data_raw)
  lookup_teacher <- create_lookup_table_teacher(composed = teacher_data$composed)
  dplyr::left_join(teacher_data, lookup_teacher, by = "composed")
}

# repair_tables ----------------------------------------------------------------
repair_tables <- function(tables)
{
  failed <- !sapply(tables, looks_good)
  tables[failed] <- lapply(tables[failed], repair_table)
  stopifnot(all(sapply(tables, looks_good)))
  tables
}

# looks_good -------------------------------------------------------------------
looks_good <- function(x)
{
  look_for <- c(1:2, "HP1", 3:4,"HP2", 5:9)
  nrow(x) == length(look_for) && all(x[, 1L] == look_for)
}

# repair_table -----------------------------------------------------------------
repair_table <- function(data)
{
  #data <- tables[[1L]]
  
  keys <- data[, 1L]
  
  keys_1 <- fill_gaps_by_index(keys)
  keys_2 <- fill_gaps_with_neighbours(keys)
  
  stopifnot(is.null(keys_1) || identical(keys_1, keys_2))
  
  data[, 1L] <- keys_2
  
  # Remove separator lines
  data <- data[!grepl("^-+$", data[, 1L]), ]
  
  merge_fields_by_hour(data)
}

# fill_gaps_by_index -----------------------------------------------------------
fill_gaps_by_index <- function(x)
{
  #x <- tables[[1L]]
  empty_rows <- grep("^$", x)

  raise_error <- function() {
    message("filling gaps by index does not work. Returning NULL.")  
  }
 
  if (!length(empty_rows) %% 2L == 0L) {
    raise_error()
    return(NULL)
  }
  
  row_pairs <- matrix(empty_rows, nrow = 2L)
  
  if (!all(row_pairs[2L, ] - row_pairs[1L, ] == 2L)) {
    raise_error()
    return(NULL)
  }
  
  for (j in seq_len(ncol(row_pairs))) {
    i <- row_pairs[, j]
    x[c(i[1], i[2])] <- x[i[1] + 1L]
  }
  x
}

# fill_gaps_with_neighbours ----------------------------------------------------
fill_gaps_with_neighbours <- function(
    x, is_ok = is_key, prefer = function(a, b) a < b
)
{
  left_of <- function(x) c("", x[-length(x)])
  right_of <- function(x) c(x[-1L], "")
  i_last <- NULL
  while (TRUE) {
    i <- which(x == "")
    if (!is.null(i_last) && identical(i, i_last)) {
      break
    }
    left <- left_of(x)[i]
    right <- right_of(x)[i]
    use_left <- is_ok(left) & (!is_ok(right) | prefer(left, right))
    use_right <- is_ok(right) & (!is_ok(left) | prefer(right, left))
    stopifnot(sum(use_left & use_right) == 0L)
    #data.frame(left, right, use_left, use_right)
    x[i[use_left]] <- left[use_left]
    x[i[use_right]] <- right[use_right]
    i_last <- i
  }
  x
}

# is_key -----------------------------------------------------------------------
is_key <- function(x)
{
  #grepl("^(HP)?\\d+$", x)
  x != "" & !grepl("^-+$", x)
}

# merge_fields_by_hour ---------------------------------------------------------
merge_fields_by_hour <- function(data)
{
  sets <- split(data, factor(data$hr, levels = unique(data$hr)))
  #set <- sets[[4]]
  #data[1:5, ]
  new_sets <- lapply(X = sets, FUN = function(set) {
    as.data.frame(lapply(set[-1L], function(x) {
      paste(x[x != ""], collapse = "/")
    }))
  })
  dplyr::bind_rows(new_sets, .id = "hr")
}

# get_full_class_table ---------------------------------------------------------
get_full_class_table <- function(file)
{
  #file = file_classes_3
  tables <- read_time_tables(file)
  class_tables <- repair_tables(tables)
  class_data_raw <- merge_tables(class_tables, id = "class")
  class_data <- split_composed_fields(data = class_data_raw)
  lookup_class <- create_lookup_table_class(composed = gsub("ESSEN (\\d)", "ESSEN-\\1", class_data$composed))
  result <- dplyr::left_join(class_data, lookup_class, by = "composed")
  result$weekday <- to_weekday_factor(result$weekday)
  columns <- c("class", "weekday", "hr", "subject", "teacher", "room")
  result <- kwb.utils::moveColumnsToFront(result, columns)
  kwb.utils::fullySorted(result)
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
  
  if (any(!considered)) {
    cat("Not considered:\n")
    print(sapply(parts[!considered], paste, collapse = "|"))
  }
  
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
    if (any(is_triple)) {
      cbind(
        split_into(x = parts[is_triple], columns = c("subject", "room", "teacher")),
        composed = composed[is_triple]
      )      
    },
    if (any(is_pair)) {
      cbind(
        split_into(x = parts[is_pair], columns = c("subject", "teacher")),
        composed = composed[is_pair]
      )
    },
    if (any(is_single)) {
      cbind(
        split_into(x = parts[is_single], columns = c("teacher")),
        composed = composed[is_single]
      )
    }
  )
  
  kwb.utils::moveColumnsToFront(combined, "composed")  
}

# to_weekday_factor ------------------------------------------------------------
to_weekday_factor <- function(x)
{
  factor(x, levels = c("Mo", "Di", "Mi", "Do", "Fr"))
}
