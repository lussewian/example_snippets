#' Script to scan video files and transcripe audio to a text file

library(reticulate)
reticulate::install_miniconda()
reticulate::conda_create("whisper-env", packages = c("python=3.10"))
reticulate::use_condaenv("whisper-env", required = TRUE)
reticulate::py_install("openai-whisper", pip = TRUE)
reticulate::py_install("ffmpeg", method = "conda")

reticulate::py_require("openai-whisper")
reticulate::py_require("ffmpeg")

whisper <- import("whisper")
model <- whisper$load_model("base")

path <- ""
phrase <- ""

log_file <- file.path(path, "whisper_log.txt")
sink(log_file, append = TRUE)

files_list <- list.files(path, pattern = "\\.mp4$", full.names = TRUE)

for (file in files_list) {
  input_file <- file
  output_file <- sub("\\.mp4$", ".mp3", input_file)

  system(sprintf('ffmpeg -i "%s" -vn -acodec libmp3lame "%s"', input_file, output_file), intern = TRUE)
  
  if (!file.exists(output_file)) {
    message <- paste("File does not exist:", input_file)
    cat(message, "\n")
    next
  }
  
  result <- model$transcribe(output_file)
  cat(result$text[1], "\n")

  if (grepl(phrase, result$text[1], ignore.case = TRUE)) {
    cat("The file is:", file, "\n")
  } else {
    file.remove(output_file)
    cat("", file, "\n")
  }
}

sink()
