library(pdftools)
library(here)

pdf_combine(here("products","submissions","demography","first_submission",
                 c("main.pdf", "appendix.pdf")),
            output=here("products","submissions","demography",
                        "first_submission","full_manuscript.pdf"))
