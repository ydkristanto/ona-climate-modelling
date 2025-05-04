library(qrcode)
library(rsvg)


page <- add_logo(
  qr_code(
    "https://ydkristanto.github.io/ona-climate-modelling/",
    ecl = "H"
  ),
  logo = "github-mark.svg",
  ecl = "L"
)

generate_svg(
  qrcode = page,
  filename = "page_qr_code.svg",
  size = 300,
  foreground = "black",
  background = "white"
)

plot(page)
