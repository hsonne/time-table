#
# Read teacher timetables for Eduard-Moerike-Grundschule
# Source the whole script first
#

kwb.utils::loadFunctions(c(
  "functions.R",
  "helpers.R",
  "read_time_tables.R",
  "views.R"
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

# Have a first look at the data ------------------------------------------------
{
  my_print <- function(data) print(head(unify(data)))
  
  my_print(teacher_data)
  my_print(class_data_1)
  my_print(class_data_2)
  my_print(class_data_3)
}

# Identify free periods per class ----------------------------------------------
{
  get_free_periods(class_data = class_data_3)
  get_free_periods(class_data = class_data_3, transpose = TRUE)
  get_free_periods(class_data = class_data_3, layout = 2L)
  get_free_periods(class_data = class_data_3, layout = 2L, transpose = TRUE)
}

# Compare teacher names as extracted from class/teacher timetables -------------
{
  get_teachers <- function(data) sort(unique(data$teacher))
  cmp <- kwb.utils::compareSets
  cmp(get_teachers(teacher_data), get_teachers(class_data_1))
  cmp(get_teachers(class_data_2), get_teachers(class_data_1))
}

# Count occurrences of value combinations in pairs of columns ------------------
{
  class_data <- class_data_1
  
  get_combis <- function(..., data = class_data) {
    kwb.utils::countOrSum(data, c(...))
  }
  
  get_combis("teacher", "subject")
  get_combis("subject", "teacher")
  get_combis("room", "subject")
  get_combis("room", "weekday")
  get_combis("weekday_hr", "room")
}

# When are the German classes? -------------------------------------------------
{
  get_deu_combis <- function(..., data = class_data) {
    as.matrix(kwb.utils::countOrSum(data[data$subject == "Deu", ], c(...)))
  }
  
  v1 <- get_deu_combis("weekday_hr", "class", data = class_data_1)
  v2 <- get_deu_combis("weekday_hr", "class", data = class_data_2)

  diff_matrix <- compare_int_matrices(v1, v2)
  diff_matrix
  filter_for_changes(diff_matrix)
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
