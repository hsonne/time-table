#
# Read teacher timetables for Eduard-Moerike-Grundschule
# Source the whole script first
#

kwb.utils::loadFunctions(c(
  "functions.R",
  "read_time_tables.R"
))

# Read all pdf files -----------------------------------------------------------
{
  file_teachers <- "C:/Users/hsonne/Downloads/L/lernfoerderung/Lehrer Neu 2024-2025.pdf"
  file_classes_1 <- "C:/Users/hsonne/Downloads/L/lernfoerderung/Klassen Neu 2024-2025.pdf"
  file_classes_2 <- "C:/Users/hsonne/Downloads/L/lernfoerderung/Klassen Neu2 024-2025.pdf"
  file_classes_3 <- "C:/Users/hsonne/Downloads/L/lernfoerderung/Klassen 2024-09-27 13-10.pdf"
  
  teacher_data <- get_full_teacher_table(file = file_teachers)
  
  class_data_1 <- get_full_class_table(file = file_classes_1)
  class_data_2 <- get_full_class_table(file = file_classes_2)
  class_data_3 <- get_full_class_table(file = file_classes_3)
}

# Have a look at the data ------------------------------------------------------
{
  column_order <- c("weekday", "hr", "teacher", "class", "subject", "room")
  
  unify <- function(data) kwb.utils::fullySorted(
    kwb.utils::moveColumnsToFront(data, column_order)
  )
  
  my_print <- function(data) print(head(unify(data)))
  
  my_print(teacher_data)
  my_print(class_data_1)
  my_print(class_data_2)
  
  get_teachers <- function(data) sort(unique(data$teacher))
  cmp <- kwb.utils::compareSets
  
  cmp(get_teachers(teacher_data), get_teachers(class_data_1))
  cmp(get_teachers(class_data_2), get_teachers(class_data_1))

  get_weekday_hr <- function(data) factor(
    kwb.utils::pasteColumns(data, c("weekday", "hr"), "-"), 
    levels = grep("HP", invert = TRUE, value = TRUE, kwb.utils::pasteColumns(
      kwb.utils::fullySorted(unique(data[, c("weekday", "hr")])), sep = "-"
    ))
  )
  
  class_data_1$weekday_hr <- get_weekday_hr(class_data_1)
  class_data_2$weekday_hr <- get_weekday_hr(class_data_2)
  class_data_3$weekday_hr <- get_weekday_hr(class_data_3)
  
  class_data <- class_data_1
  
  get_combis <- function(..., data = class_data) {
    kwb.utils::countOrSum(data, c(...))
  }
  
  get_combis("teacher", "subject")
  get_combis("subject", "teacher")
  get_combis("room", "subject")
  get_combis("room", "weekday")
  get_combis("weekday_hr", "room")

  get_deu_combis <- function(..., data = class_data) {
    as.matrix(kwb.utils::countOrSum(data[data$subject == "Deu", ], c(...)))
  }
  
  v1 <- get_deu_combis("weekday_hr", "class", data = class_data_1)
  v2 <- get_deu_combis("weekday_hr", "class", data = class_data_2)

  int_removed <- -1L
  int_added <- 2L
  v1_v2_diff <- v2
  v1_v2_diff[v1 == 0L & v2 == 1L] <- int_added
  v1_v2_diff[v1 == 1L & v2 == 0L] <- int_removed
  has_changed <- v1_v2_diff == int_added | v1_v2_diff == int_removed
  
  any_in_row <- function(x) rowSums(x) > 0L
  any_in_col <- function(x) colSums(x) > 0L
  
  v2[any_in_row(v2), ]
  v1_v2_diff[any_in_row(has_changed), any_in_col(has_changed)]
  v1_v2_diff
}

# Write data to text files for comparison -------------------------------------- 
{
  my_write <- function(data, filename) {
    write.csv(data, file.path(tempdir(), filename), row.names = FALSE, na = "")
  }
  
  my_filter <- function(data) {
    data[data$subject == "Deu", ]
  }
  
  my_write(my_filter(class_data_1), "ems_classes_deu_1.txt")
  my_write(my_filter(class_data_2), "ems_classes_deu_2.txt")
  my_write(my_filter(class_data_3), "ems_classes_deu_3.txt")
  
  #kwb.utils::hsOpenWindowsExplorer(tempdir())
}

# Try to create one table from the other ---------------------------------------
if (FALSE)
{
  by_class <- split(
    kwb.utils::removeColumns(teacher_data, c("class", "composed")),
    f = teacher_data$class
  )
  
  x <- by_class$`1A`
  x$weekday <- to_weekday_factor(x$weekday)
  
  kwb.utils::moveColumnsToFront(
    kwb.utils::orderBy(x, c("weekday", "hr")),
    c("weekday", "hr", "subject", "room")
  )
}
