# This script renders the R Markdown reports to the output folder.

reports_to_render <- list.files("R", pattern = "\\.Rmd$")

for (i in reports_to_render) {

  out_name = gsub("Rmd", "pdf", i)

  rmarkdown::render(
               input = paste0("R/", i),
               output_format = "pdf_document",
               output_dir = "output",
               output_file = out_name
             )
}
