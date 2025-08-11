# This script renders the R Markdown reports to the output folder.

reports_to_render <- list.files("R", pattern = "\\.Rmd$")

rmarkdown::render(
  input = "R/02_report_unequal-sample-size.Rmd",
  output_format = "pdf_document",
  output_dir = "output",
  output_file = "02_report_unequal-sample-size-exported.pdf"
)
