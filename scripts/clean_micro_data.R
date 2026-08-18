library(tidyverse)
library(here)
library(tidyxl)

# Strategy:
# Identify tables in the spreadsheets by contiguous cells with borders
# Restrict to those that have "subjectid" in at least one cell (removes the
# summary tables)
# These tables have hierarchical merged headers eg
#  ---------------------------------------------
#  |       header 1                            |
#  ---------------------------------------------
#  | header 2.1       |  header 2.2            |
#  ---------------------------------------------
#  | hd 3.1 | hd 3.2  | hd 3.3   |  hd 3.4     |
#  ---------------------------------------------
#
# to deal with these, sequentially merge headers so the first col will
# be (for example) header1_header2.1_hd3.1
# remove spaces and add underscore between
#
# the function extract_bordered_tables will do all of this
#
#
# There are then sample type specific cleaning functions

extract_bordered_tables <- function(file_path, sheet_name = 1, n_header_rows = 2) {
 
# 1. Read all cells and all formats
  all_cells <- xlsx_cells(file_path, sheets = sheet_name)
  all_formats <- xlsx_formats(file_path)

  # 2. Track which format IDs have any border (top, bottom, left, or right)
  # Modify this if you only care about specific borders (e.g., just 'bottom' for headers)
  border_formats <- all_formats$local$border
  bordered_ids <- which(
    border_formats$top$style != "none" |
      border_formats$bottom$style != "none" |
      border_formats$left$style != "none" |
      border_formats$right$style != "none"
  )

  # 3. Filter cells to only those within a bordered area
  bordered_cells <- all_cells %>%
    filter(local_format_id %in% bordered_ids) %>%
    select(row, col, data_type, character, numeric, date)

  if (nrow(bordered_cells) == 0) {
    stop("No bordered cells found on this sheet.")
  }

  # 4. Group adjacent cells into distinct tables using a basic flood-fill / cluster logic
  # We look for continuous blocks where rows and columns are close to each other
  # (Max gap allowed between tables can be adjusted; here it is 1 empty cell)
  find_clusters <- function(cells, max_gap = 1) {
    # Generate an adjacency matrix or simple coordinate-based cluster assignment
    # For a robust approach, we loop through and assign IDs to intersecting bounding boxes
    cells <- cells %>% arrange(row, col)
    cluster_id <- numeric(nrow(cells))
    current_id <- 0

    for (i in 1:nrow(cells)) {
      if (cluster_id[i] == 0) {
        current_id <- current_id + 1
        # Find all cells belonging to this table's bounding rectangle recursively
        queue <- i
        while (length(queue) > 0) {
          curr <- queue[1]
          queue <- queue[-1]
          cluster_id[curr] <- current_id

          # Look for unassigned neighbors within the max_gap threshold
          neighbors <- which(
            cluster_id == 0 &
              abs(cells$row - cells$row[curr]) <= (max_gap + 1) &
              abs(cells$col - cells$col[curr]) <= (max_gap + 1)
          )

          if (length(neighbors) > 0) {
            cluster_id[neighbors] <- current_id
            queue <- c(queue, neighbors)
          }
        }
      }
    }
    cells$table_id <- cluster_id
    return(cells)
  }

  clustered_cells <- find_clusters(bordered_cells, max_gap = 0)

  # 5. mung headers to account for merged cells

  mung_hierarchical_headers <- function(sub_table, n_header_rows = 2) {
    if (n_header_rows < 2) {
      stop("Problem in mung_hierarchical_headers: n_header rows must be at least 2")
    }

    for (i in 1:(n_header_rows - 1)) {
      header1 <- filter(sub_table, row == min(row))
      header2 <- filter(sub_table, row == min(row) + 1)

      header_final <-
        left_join(header2, header1 |> ungroup() |> select(-row), by = "col") |>
        arrange(col) |>
        fill(value.x, value.y) |>
        mutate(
          value =
            if_else(is.na(value.x),
              paste0(tolower(gsub(" ", "", value.y))),
              paste0(tolower(gsub(" ", "", value.y)), "_", tolower(gsub(" ", "", value.x)))
            )
        ) |>
        select(-c(value.x, value.y))

      sub_table <-
        bind_rows(
          header_final,
          sub_table |>
            filter(row > min(row) + 1)
        )
    }
    return(sub_table)
  }


  # and 6 Reshape each isolated table cluster back into a rectangular tibble
  tables_list <- clustered_cells %>%
  group_split(table_id) %>%
    map(function(sub_table) {
      # Coalesce values into a single text/value column
      sub_table <- sub_table %>%
        mutate(value = case_when(
          data_type == "character" ~ character,
          data_type == "numeric" ~ as.character(numeric),
          data_type == "date" ~ as.character(date),
          TRUE ~ NA_character_
        )) %>%
        select(row, col, value)

      if ("Subject ID" %in% sub_table$value) {
        sub_table <- mung_hierarchical_headers(sub_table, n_header_rows)
      }

      # Pivot wide to re-create the Excel spreadsheet shape
      wide_matrix <- sub_table %>%
        pivot_wider(names_from = col, values_from = value) %>%
        arrange(row) %>%
        select(-row)

      # Use the first row as column names and drop it from the data rows
      if (any(duplicated(as.character(wide_matrix[1, ])))) {
        colnames(wide_matrix) <- paste0("x", colnames(wide_matrix))
        final_tibble <- wide_matrix
      } else {
        colnames(wide_matrix) <- as.character(wide_matrix[1, ])
        final_tibble <- wide_matrix[-1, ]
      }
      return(as_tibble(final_tibble))
    })

  return(tables_list)
}

# inpatients


file_path <- here("data/raw/Sample Result (Spreadsheet-Excel)/Spreadsheet Sampel INPATIENT.xlsx")

make_inpatient_data_tidy <- function(table_list) {

table_list <- table_list[map(table_list, function(x) "subjectid" %in% colnames(x)) |> unlist()]

map(table_list, \(x) fill(x, c(no, subjectid))) |>
  bind_rows() |> 
  pivot_longer(-c(no, subjectid),
  names_to = c("day", "plate"),
  names_pattern = "swab(.*)_(.*)") |>
  filter(!is.na(value), value != "-", value != "G") |>
  group_by(no,subjectid, day, plate) |>
  mutate(growth = if_else(all(value == "NG"), FALSE, TRUE),
         species  = paste(value, collapse = ";")) |>
  mutate(species = if_else(species == "NG", NA, species)) |> 
  select(-value) |>
  unique() |>
  as.data.frame()

}



df <-
  bind_rows(
    make_inpatient_data_tidy(
      extract_bordered_tables(file_path, sheet_name = 1, n_header_rows = 2)
    ),
    make_inpatient_data_tidy(
      extract_bordered_tables(file_path, sheet_name = 2, n_header_rows = 2)
    ),
    make_inpatient_data_tidy(
      extract_bordered_tables(file_path, sheet_name = 3, n_header_rows = 2)
    )
  ) 



df <-
  df |>
  separate_wider_delim(subjectid, delim = ".", names = c("sid_1", "hospital", "sample_type", "pid")) |>
  mutate(
    hospital =
      case_when(
        hospital == 1 ~ "RSDK",
        hospital == 2 ~ "RSWN",
        hospital == 3 ~ "RSDA"
      ),
    sample_type = case_when(
      sample_type == 1 ~ "ward",
      sample_type  == 2 ~ "ICU",
      sample_type == 3 ~ "community",
      sample_type  == 5 ~ "staff",
      sample_type  == 4 ~ "environment"
    )
  )

write_csv(df, here("data/processed/cleaned_inpatient_cultures.csv"))


# staff


file_path <- here("data/raw/Sample Result (Spreadsheet-Excel)/Spreadsheet Sampel HEALTHWORKERS.xlsx")

#extract_bordered_tables(file_path, sheet_name = 2, n_header_rows = 3) -> table_list

make_staff_data_tidy <- function(table_list) {

table_list <- table_list[map(table_list, function(x) "subjectid" %in% colnames(x)) |> unlist()]

map(table_list, \(x) fill(x, c(no, subjectid))) |>
  bind_rows() |> 
  pivot_longer(-c(no, subjectid),
  names_to = c("day", "sample", "plate"),
  names_sep = "_") |>
  filter(!is.na(value), value != "-", value != "G") |>
  group_by(no,subjectid, day, sample, plate) |>
  mutate(growth = if_else(all(value == "NG"), FALSE, TRUE),
         species  = paste(value, collapse = ";")) |>
  mutate(species = if_else(species == "NG", NA, species)) |> 
  select(-value) |>
  unique() |>
  as.data.frame()

}


df <- bind_rows( make_staff_data_tidy(
      extract_bordered_tables(file_path, sheet_name = 1, n_header_rows = 3)
    ),
    make_staff_data_tidy(
      extract_bordered_tables(file_path, sheet_name = 2, n_header_rows = 3)
    ),
    make_staff_data_tidy(
      extract_bordered_tables(file_path, sheet_name = 3, n_header_rows = 3)
    )
  ) 


df <-
  df |>
  separate_wider_delim(subjectid, delim = ".", names = c("sid_1", "hospital", "sample_type", "pid")) |>
  mutate(
    hospital =
      case_when(
        hospital == 1 ~ "RSDK",
        hospital == 2 ~ "RSWN",
        hospital == 3 ~ "RSDA"
      ),
    sample_type = case_when(
      sample_type == 1 ~ "ward",
      sample_type  == 2 ~ "ICU",
      sample_type  == 5 ~ "staff"

    )
  )

write_csv(df, here("data/processed/cleaned_staff_cultures.csv"))

### environment

file_path <- here("data/raw/Sample Result (Spreadsheet-Excel)/Spreadsheet Sampel ENVIRONMENT.xlsx")

make_env_data_tidy <- function(table_list) {

table_list <- table_list[map(table_list, function(x) "subjectid" %in% colnames(x)) |> unlist()]

map(table_list, \(x) fill(x, c(no, subjectid))) |>
  bind_rows() |> 
  rename(cro = result_cro, esbl = result_esbl) |>
  pivot_longer(-c(no, subjectid),
  names_to = c("plate")) |>
  filter(!is.na(value), value != "-", value != "G") |>
  group_by(no,subjectid, plate) |>
  mutate(growth = if_else(all(value == "NG"), FALSE, TRUE),
         species  = paste(value, collapse = ";")) |>
  mutate(species = if_else(species == "NG", NA, species)) |> 
  select(-value) |>
  unique() |>
  as.data.frame()

}


df <- bind_rows( 
    make_env_data_tidy(
      extract_bordered_tables(file_path, sheet_name = 1, n_header_rows = 2)
    ),
    make_env_data_tidy(
      extract_bordered_tables(file_path, sheet_name = 2, n_header_rows = 2)
    ),
    make_env_data_tidy(
      extract_bordered_tables(file_path, sheet_name = 3, n_header_rows = 2)
    )
  ) 


df <-
  df |>
  separate_wider_delim(subjectid, delim = ".", names = c("sid_1", "hospital", "sample_type", "pid")) |>
  mutate(
    hospital =
      case_when(
        hospital == 1 ~ "RSDK",
        hospital == 2 ~ "RSWN",
        hospital == 3 ~ "RSDA"
      ),
    sample_type = case_when(
      sample_type == 1 ~ "ward",
      sample_type  == 2 ~ "ICU",
      sample_type  == 5 ~ "staff",
      sample_type  == 4 ~ "environment"
    )
  )

write_csv(df, here("data/processed/cleaned_env_cultures.csv"))

## COMMUNITY


file_path <- here("data/raw/Sample Result (Spreadsheet-Excel)/Spreadsheet Sampel COMMUNITY.xlsx")

# env tidy function also works here

df <- bind_rows( 
    make_env_data_tidy(
      extract_bordered_tables(file_path, sheet_name = 1, n_header_rows = 2)
    ),
    make_env_data_tidy(
      extract_bordered_tables(file_path, sheet_name = 2, n_header_rows = 2)
    ),
    make_env_data_tidy(
      extract_bordered_tables(file_path, sheet_name = 3, n_header_rows = 2)
    )
  ) 


df <-
  df |>
  separate_wider_delim(subjectid, delim = ".", names = c("sid_1", "hospital", "sample_type", "pid")) |>
  mutate(
    hospital =
      case_when(
        hospital == 1 ~ "RSDK",
        hospital == 2 ~ "RSWN",
        hospital == 3 ~ "RSDA"
      ),
    sample_type = case_when(
      sample_type == 1 ~ "ward",
      sample_type  == 2 ~ "ICU",
      sample_type == 3 ~ "community",
      sample_type  == 5 ~ "staff",
      sample_type  == 4 ~ "environment"
    )
  )

write_csv(df, here("data/processed/cleaned_community_cultures.csv"))

