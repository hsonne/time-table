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
  
  full_teacher_data <- get_full_teacher_table(file = file_teachers)
  full_class_data_1 <- get_full_class_table(file = file_classes_1)
  full_class_data_2 <- get_full_class_table(file = file_classes_2)
}

{
  column_order <- c("weekday", "hr", "teacher", "class", "subject", "room")
  
  unify <- function(data) kwb.utils::fullySorted(
    kwb.utils::moveColumnsToFront(data, column_order)
  )
  
  my_print <- function(data) print(head(unify(data)))
  
  my_print(full_teacher_data)
  my_print(full_class_data_1)
  my_print(full_class_data_2)
}

# Write data to text files for comparison -------------------------------------- 
{
  my_write <- function(data, filename) {
    write.csv(data, file.path(tempdir(), filename), row.names = FALSE, na = "")
  }
  
  my_filter <- function(data) {
    data[data$subject == "Deu", ]
  }
  
  my_write(my_filter(full_class_data_1), "ems_classes_deu_1.txt")
  my_write(my_filter(full_class_data_2), "ems_classes_deu_2.txt")
  
  #kwb.utils::hsOpenWindowsExplorer(tempdir())
}

# Try to create one table from the other ---------------------------------------
if (FALSE)
{
  by_class <- split(
    kwb.utils::removeColumns(full_teacher_data, c("class", "composed")),
    f = full_teacher_data$class
  )
  
  x <- by_class$`1A`
  x$weekday <- to_weekday_factor(x$weekday)
  
  kwb.utils::moveColumnsToFront(
    kwb.utils::orderBy(x, c("weekday", "hr")),
    c("weekday", "hr", "subject", "room")
  )
}
