insert_text_below_cursor_line <- function(text) {
  cursor_selection <- rstudioapi::primary_selection(rstudioapi::getSourceEditorContext())

  cursor_position <- cursor_selection$range$start
  
  rstudioapi::insertText(
    location = rstudioapi::document_position(row  = cursor_position["row"] + 1, column = 0),
    text = paste0("\n", text, "\n")
  )
}

