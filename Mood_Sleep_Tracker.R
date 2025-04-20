if (file.exists("mood_data.csv")) {
  mood_data <- read.csv("mood_data.csv", stringsAsFactors = FALSE)
  mood_data$Date <- as.Date(mood_data$Date)
}

if(!exists("mood_data")) {
  mood_data <- data.frame(
    Date = as.Date(character()),
    Mood = integer(),
    SleepHours = numeric(),
    Notes = character(),
    stringsAsFactors = FALSE
  )
}

cat("How's your mood today on a scale of 1-10? 1 being HORRIBLE and 10 being AWESOME!")
mood <- as.integer(readline())
cat("How many hours did you sleep last night?")
sleep <- as.integer(readline())
cat("Any note to self for today?")
note <- readline()

if (!is.na(mood) && mood >= 1 && mood <= 10 && !is.na(sleep)) {
  today <- Sys.Date()
  
  if (any(mood_data$Date == today)) {
    cat("An entry already exists for today. Would you like to overwrite it?")
    response <- tolower(readline())
    
    if (response == "yes"){
      mood_data <- mood_data[mood_data$Date != today, ]
      cat("Old entry has been replaced \n")
    } else {
      cat("Keeping existing entry.\n")
      mood <- NULL
      sleep <- NULL
      note <- NULL
    }
  } 
  
  if (!is.null(mood) && !is.null(sleep)) {
    new_entry <- data.frame(
      Date = Sys.Date(),
      Mood = mood,
      SleepHours = sleep,
      Notes = note,
      stringsAsFactors = FALSE
    )
    mood_data <- rbind(mood_data, new_entry)
    
    write.csv(mood_data, "mood_data.csv", row.names =  FALSE)
    cat("New entry saved successfully!\n")
  }
} else {
  print("Invalid input detected. Please make sure mood is between 1 and 10.")
}

library(ggplot2)
ggplot(mood_data, aes(x = Date, y = Mood)) +
  geom_line(color = "steelblue") +
  geom_point() +
  theme_minimal() +
  labs(title = "Mood Over Time", y = "Mood (1-10)")

ggplot(mood_data, aes(x = Date, y = SleepHours)) +
  geom_line(color = "darkgreen") +
  geom_point() +
  theme_minimal() +
  labs(title = "Sleep Hours Over Time", y = "Hours Slept")

