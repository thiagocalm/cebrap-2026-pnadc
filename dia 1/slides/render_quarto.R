

# HTML format -------------------------------------------------------------

# path
input_path = file.path("presentations","final presentation", "html")

quarto::quarto_render(input = file.path(input_path,"HTML_edsd_FinalPresentation.qmd"))

pagedown::chrome_print(
  file.path(input_path,"HTML_edsd_FinalPresentation.html"),
  options = list(
    printBackground = FALSE,
    preferCSSPageSize = FALSE,
    paperWidth = 8, paperHeight = 11,
    marginTop = 0.0, marginBottom = 0.0,
    marginLeft = 0.0, marginRight = 0.0
  )
)

# PDF format --------------------------------------------------------------

# path
input_path = file.path("presentations","final presentation", "pdf")

quarto::quarto_render(input = file.path(input_path,"PDF_edsd_FInalPresentation.qmd"))

pagedown::chrome_print(
  file.path(input_path,"PDF_edsd_FInalPresentation.html"),
  options = list(
    printBackground = FALSE,
    preferCSSPageSize = FALSE,
    paperWidth = 8, paperHeight = 11,
    marginTop = 0.0, marginBottom = 0.0,
    marginLeft = 0.0, marginRight = 0.0
  )
)

