library(pdftools)
library(here)

pdf_combine(here("products","submissions","demography","randr1",
                 c("main.pdf", "appendix.pdf")),
            output=here("products","submissions","demography",
                        "randr1","full_manuscript.pdf"))
