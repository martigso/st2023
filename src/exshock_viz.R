# Setting up terminal commands
if("optparse" %in% rownames(installed.packages()) == FALSE){
  cat("Package 'optparse' needed. Want to install? (y/n) ")
  inst <- readLines(file("stdin"), 1)
  if(inst == "y"){
    install.packages("optparse", repos = "https://cloud.r-project.org")
  } else {
    stop("Well, then this will not work.\n", call. = FALSE)
  }
}
library(optparse)

if("stm" %in% rownames(installed.packages()) == FALSE){
  cat("Package 'stm' needed. Want to install? (y/n) ")
  inst <- readLines(file("stdin"), 1)
  if(inst == "y"){
    install.packages("stm", repos = "https://cloud.r-project.org")
  } else {
    stop("Well, then this will not work.\n", call. = FALSE)
  }
}

suppressMessages(library(stm))

if("ggplot2" %in% rownames(installed.packages()) == FALSE){
  cat("Package 'ggplot2' needed. Want to install? (y/n) ")
  inst <- readLines(file("stdin"), 1)
  if(inst == "y"){
    install.packages("ggplot2", repos = "https://cloud.r-project.org")
  } else {
    stop("Well, then this will not work.\n", call. = FALSE)
  }
}

library(ggplot2)

if("tonR" %in% rownames(installed.packages()) == FALSE){
  stop("Please install package tonR (https://github.com/ltgoslo/talk-of-norway/tree/master/src/R/tonR)\n", call. = FALSE)
}

library(tonR)

arguments <- list(
  
  #
  make_option(c("-s", "--stm"), type = "character", default = NULL,
              help = "Path to SVM model [default = %default]", metavar = "character"),
  
  #
  make_option(c("-t", "--topic"), type = "character", default = NULL,
              help = "Topic number to plot in map based on -s [default = %default]", metavar = "integer"),
  
  #
  make_option(c("-o", "--out"), type = "character", default = "id",
              help = "Folder path for output figure [default = %default]", metavar = "character")
  
)

# Retrieving input
rgruments <- OptionParser(option_list = arguments)
parsarg <- parse_args(rgruments)

# Checking for input
if (is.null(parsarg$stm)){
  print_help(rgruments)
  stop("Need to supply an STM model file \n", call.=FALSE)
}


if (is.null(parsarg$topic)){
  print_help(rgruments)
  stop("Need to supply topic number.\n", call.=FALSE)
}


# Extracting model speccs from file name -- used for naming figure file
model_name <- gsub("reps\\_|\\.rda", "", stringr::str_extract(parsarg$stm, "([a-z]+\\_)+[a-z]+[0-9]+|([a-z]+\\_*\\.*)+$"))
out_file <- paste0(parsarg$out, "/", model_name, "_topic", parsarg$topic, ".pdf")

message(paste("Plotting map for", model_name, "topic", parsarg$topic, ":"))

# Function for making map
map_effect <- function(formula, stm, data, nsims, covariate, moderator, moderator.value){
  
  tmp <- estimateEffect(formula, stm, data, nsims = nsims)
  
  treat1 <- plot(tmp, covariate = covariate, moderator = moderator, moderator.value = moderator.value[1])
  treat2 <- plot(tmp, covariate = covariate, moderator = moderator, moderator.value = moderator.value[2])
  
  tmp <- data.frame(fylke = treat1$uvals,
                    y_hat_before = treat1$means[[1]],
                    y_hat_before_lwr = treat1$cis[[1]][1, ],
                    y_hat_before_upr = treat1$cis[[1]][2, ],
                    y_hat_after = treat2$means[[1]],
                    y_hat_after_lwr = treat2$cis[[1]][1, ],
                    y_hat_after_upr = treat2$cis[[1]][2, ])
  
  load("./data/fylker.rda")
  
  tmp <- merge(x = fylker, y = tmp, by.x = "navn", by.y = "fylke", all.x = TRUE)
  
  tmp$diff <- tmp$y_hat_after - tmp$y_hat_before
  
  return(tmp)
}

message("... Reading data")

load(parsarg$stm) # Loading stm

assign("x", as.integer(parsarg$topic), .GlobalEnv) # Assigning topic number object to correct environment


message("...... Estimating effect")

# Estimating map effect
tmp <- map_effect(c(x) ~ factor(session) * factor(county) + factor(party_id), st_stm, meta, 500, "county", "session", c("2013-2014", "2014-2015"))

# Plotting effect to map
message("......... Plotting results")
ggplot(tmp, aes(x = long, y = lat, group = navn, fill = diff)) +
  geom_polygon(alpha = .9) + theme_minimal() +
  geom_path() +
  scale_fill_gradient2(low = "darkred", high = "darkblue", mid = "white", midpoint = 0) +
  guides(fill = guide_colorbar(barwidth = unit(8, "cm"), label.position = "bottom")) +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal")
ggsave(out_file, height = 13, width = 11)

message("............ Converting dpi")

# Converting dpi
system(paste("convert -density 300", out_file, "-resample 300", out_file))

message("Done!")