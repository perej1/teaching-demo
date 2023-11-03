library(purrr)
library(tibble)
library(reshape2)
library(ggplot2)
library(optparse)

option_list <- list(
  make_option("--m", type = "integer", default = 10,
              help = "Number of paths"),
  make_option("--n", type = "integer", default = 100,
              help = "Length of the path"),
  make_option("--seed", type = "integer", default = 123,
              help = "Set random seed")
)
opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)


if (opt$m > length(letters)) {
  cli::cli_abort("Maximum number of paths must be less than or equal to
                 {length(letters)}.")
}

set.seed(opt$seed)
paths <- rerun(opt$m, sample(c(-1, 1), opt$n, replace = TRUE)) |>
  map(~ c(0, .x)) |>
  map(cumsum) |>
  set_names(letters[1:opt$m]) |>
  as_tibble() |>
  dplyr::mutate(index = 0:opt$n)

colors <- c(rep("gray80", opt$m - 1), "black")

gplot <- melt(paths, id.vars = "index", variable.name = "path",
              value.name = "value") |>
  ggplot(aes(x = index, y = value, group = path, color = path)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = colors) +
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        legend.background = element_blank(),
        legend.position = "none") +
  labs(x = "t", y = "Value")

ggsave("slides/random_walk.jpg", plot = gplot, width = 7, height = 7,
       dpi = 1000)
