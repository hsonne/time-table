# compare_int_matrices ---------------------------------------------------------
compare_int_matrices <- function(v1, v2, int_removed = -1L, int_added = 2L)
{
  stopifnot(identical(dim(v1), dim(v2)))
  diff_matrix <- v2
  diff_matrix[v1 == 0L & v2 == 1L] <- int_added
  diff_matrix[v1 == 1L & v2 == 0L] <- int_removed
  structure(diff_matrix, int_removed = int_removed, int_added = int_added)
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
    x[i[use_left]] <- left[use_left]
    x[i[use_right]] <- right[use_right]
    i_last <- i
  }
  x
}

# filter_for_changes -----------------------------------------------------------
filter_for_changes <- function(diff_matrix)
{
  int_added <- kwb.utils::getAttribute(diff_matrix, "int_added")
  int_removed <- kwb.utils::getAttribute(diff_matrix, "int_removed")
  has_changed <- diff_matrix == int_added | diff_matrix == int_removed
  any_in_row <- function(x) rowSums(x) > 0L
  any_in_col <- function(x) colSums(x) > 0L
  diff_matrix[any_in_row(has_changed), any_in_col(has_changed)]
}

# get_weekday_hr ---------------------------------------------------------------
get_weekday_hr <- function(data)
{
  factor(
    kwb.utils::pasteColumns(data, c("weekday", "hr"), "-"), 
    levels = grep("HP", invert = TRUE, value = TRUE, kwb.utils::pasteColumns(
      kwb.utils::fullySorted(unique(data[, c("weekday", "hr")])), sep = "-"
    ))
  )
}

# is_key -----------------------------------------------------------------------
is_key <- function(x)
{
  #grepl("^(HP)?\\d+$", x)
  x != "" & !grepl("^-+$", x)
}

# looks_good -------------------------------------------------------------------
looks_good <- function(x)
{
  look_for <- c(1:2, "HP1", 3:4,"HP2", 5:9)
  nrow(x) == length(look_for) && all(x[, 1L] == look_for)
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

# prepare_for_split ------------------------------------------------------------
prepare_for_split <- function(composed)
{
  gsub("ESSEN (\\d)", "ESSEN-\\1", composed)
}

# repair_tables ----------------------------------------------------------------
repair_tables <- function(tables)
{
  failed <- !sapply(tables, looks_good)
  tables[failed] <- lapply(tables[failed], repair_table)
  stopifnot(all(sapply(tables, looks_good)))
  tables
}

# repair_table -----------------------------------------------------------------
repair_table <- function(data)
{
  #data <- tables[[1L]]
  
  data[, 1L] <- fill_gaps_with_neighbours(x = data[, 1L])
  
  # Remove separator lines
  data <- data[!grepl("^-+$", data[, 1L]), ]
  
  merge_fields_by_hour(data)
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

# to_weekday_factor ------------------------------------------------------------
to_weekday_factor <- function(x)
{
  factor(x, levels = c("Mo", "Di", "Mi", "Do", "Fr"))
}

# unify ------------------------------------------------------------------------
unify <- function(data)
{
  column_order <- c("weekday", "hr", "teacher", "class", "subject", "room")  
  kwb.utils::fullySorted(kwb.utils::moveColumnsToFront(data, column_order))
}
