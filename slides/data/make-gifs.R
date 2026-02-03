library(tidyverse)
library(readxl)
library(animation)

ROUTPUTQvQd_2 <- read_excel("slides/data/ROUTPUTQvQd-2.xlsx", na = "#N/A")

output_growth <- ROUTPUTQvQd_2 |>
  mutate(across(-DATE, ~ 100 * (.x / dplyr::lag(.x, 4) - 1))) |>
  filter(DATE > "1949:Q4")

colnames(output_growth) <- sub("ROUTPUT", "v", colnames(output_growth))

output_growth$DATE <- as.numeric(sub(":Q.*", "", output_growth$DATE)) +
  (as.numeric(sub(".*Q", "", output_growth$DATE)) - 1) / 4



myrange <- 121:nrow(output_growth)

saveGIF(
{

for(j in 113:ncol(output_growth)){
  vintage <- colnames(output_growth)[j]
  plot(output_growth$DATE[myrange], pull(output_growth[myrange, j]), type = "l",
       xlab = "Quarter",
       ylab = "YoY % Change",
       main = paste0("GDP Growth ", vintage),
       col = "blue",
       lwd = 2, bty = "n",
       ylim = c(-8, 18)
  )
}

},
movie.name = paste(getwd(), "/real-time.gif", sep = ""),
interval = 0.1,
ani.width = 1000,
ani.height = 500,
ani.res = 100
)
