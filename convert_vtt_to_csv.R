# Load necessary library
library(tidyverse)
library(stringr)
library(writexl)

# Convert VTT to data frame ----
# Define function to convert VTT time to milliseconds
vtt_time_to_ms <- function(time_str) {
  time_parts <- unlist(strsplit(time_str, "[:\\.]"))
  h <- as.numeric(time_parts[1])
  m <- as.numeric(time_parts[2])
  s <- as.numeric(time_parts[3])
  ms <- as.numeric(time_parts[4])
  return((h * 3600 + m * 60 + s) * 1000 + ms)
}

# Read the VTT file
lines <- readLines("20240926_How_hot_is_our_earth.vtt", warn = FALSE)

# Remove the first line (WEBVTT)
lines <- lines[-1]

# Initialize vectors
start_times <- c()
end_times <- c()
texts <- c()
current_text <- c()

# Process each line
for (line in lines) {
  line <- str_trim(line)
  
  if (str_detect(line, "^\\d{2}:\\d{2}:\\d{2}\\.\\d{3} --> \\d{2}:\\d{2}:\\d{2}\\.\\d{3}$")) {
    # Store previous subtitle text
    if (length(current_text) > 0) {
      texts <- c(texts, paste(current_text, collapse = " "))
      current_text <- c()
    }
    
    # Extract timestamps
    times <- unlist(strsplit(line, " --> "))
    start_times <- c(start_times, vtt_time_to_ms(times[1]))
    end_times <- c(end_times, vtt_time_to_ms(times[2]))
    
  } else if (line != "") {
    # Collect subtitle text
    current_text <- c(current_text, line)
  }
}

# Append the last subtitle
if (length(current_text) > 0) {
  texts <- c(texts, paste(current_text, collapse = " "))
}

# Create a data frame
df <- data.frame(
  start_time_ms = start_times,
  end_time_ms = end_times,
  text = texts,
  stringsAsFactors = FALSE
) |> 
  as_tibble()

# Save as CSV
write.csv(
  df,
  file = "20240926_How_hot_is_our_earth.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# Save as Excel file
write_xlsx(df, "20240926_How_hot_is_our_earth.xlsx")

# Cleaning and transforming the data ----
# Function to clean and transform the text data
process_subtitles <- function(df) {
  new_rows <- list()  # Store new rows
  
  for (i in 1:nrow(df)) {
    text <- df$text[i]
    start_time <- df$start_time_ms[i]
    end_time <- df$end_time_ms[i]
    
    # Extract speakers from <v Name> and remove them from text
    speakers <- str_extract_all(text, "(?<=<v\\s)[^>]+")[[1]]
    clean_text <- str_replace_all(text, "<v\\s[^>]+>", "")
    
    # Handle multiple speakers
    if (length(speakers) > 1) {
      for (speaker in speakers) {
        new_rows <- append(
          new_rows,
          list(
            data.frame(
              start_time_ms = start_time,
              end_time_ms = end_time,
              speaker = speaker,
              text = str_trim(clean_text),
              stringsAsFactors = FALSE
            )
          )
        )
      }
    } else {
      # If only one speaker or none, assign the speaker
      speaker <- ifelse(length(speakers) == 1, speakers, NA)
      
      # Split [Bla bla] into a new row
      bracket_texts <- str_extract_all(clean_text, "\\[.*?\\]")[[1]]
      clean_text <- str_replace_all(clean_text, "\\[.*?\\]", "")
      
      # Main cleaned row
      new_rows <- append(
        new_rows,
        list(
          data.frame(
            start_time_ms = start_time,
            end_time_ms = end_time,
            speaker = speaker,
            text = str_trim(clean_text),
            stringsAsFactors = FALSE
          )
        )
      )
      
      # Additional rows for bracketed text
      for (bracket_text in bracket_texts) {
        new_rows <- append(
          new_rows,
          list(
            data.frame(
              start_time_ms = start_time,
              end_time_ms = end_time,
              speaker = NA,
              text = str_trim(bracket_text),
              stringsAsFactors = FALSE
            )
          )
        )
      }
    }
  }
  
  # Combine all processed rows into a single data frame
  return(bind_rows(new_rows))
}

# Apply transformation
df_transformed <- process_subtitles(df) |> 
  as_tibble() |> 
  filter(text != "")

# Define the reference datetime
reference_time <- as.POSIXct("2024-09-26 13:14:12", tz = "Asia/Jakarta")

# Convert start_time_ms and end_time_ms to datetime format
df_transformed <- df_transformed |> 
  mutate(
    start_time = reference_time + milliseconds(start_time_ms),
    end_time = reference_time + milliseconds(end_time_ms)
  ) |> 
  select(
    start_time, end_time, start_time_ms, end_time_ms, speaker, text
  )

# Save to a new CSV file
write.csv(
  df_transformed,
  file = "20240926_How_hot_is_our_earth_transformed.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# Save as Excel file
write_xlsx(
  df_transformed,
  path = "20240926_How_hot_is_our_earth_transformed.xlsx",
  format_headers = FALSE
)


