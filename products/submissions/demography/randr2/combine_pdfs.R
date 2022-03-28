library(pdftools)
library(here)

pdf_combine(here("products","submissions","demography","randr2",
                 c("main.pdf", "appendix.pdf")),
            output=here("products","submissions","demography",
                        "randr2","full_manuscript.pdf"))
