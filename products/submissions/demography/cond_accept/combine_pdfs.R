library(pdftools)
library(here)

pdf_combine(here("products","submissions","demography","cond_accept",
                 c("main.pdf", "appendix.pdf")),
            output=here("products","submissions","demography",
                        "cond_accept","full_manuscript.pdf"))
