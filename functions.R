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

# get_full_class_table ---------------------------------------------------------
get_full_class_table <- function(file)
{
  #file = file_classes_3
  tables <- read_time_tables(file)
  class_tables <- repair_tables(tables)
  class_data_raw <- merge_tables(class_tables, id = "class")
  class_data <- split_composed_fields(data = class_data_raw)
  lookup_class <- create_lookup_table_class(composed = prepare_for_split(class_data$composed))
  result <- dplyr::left_join(class_data, lookup_class, by = "composed")
  result$weekday <- to_weekday_factor(result$weekday)
  columns <- c("class", "weekday", "hr", "subject", "teacher", "room")
  result <- kwb.utils::moveColumnsToFront(result, columns)
  kwb.utils::fullySorted(result)
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

