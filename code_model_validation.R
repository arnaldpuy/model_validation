
## ----setup, include=FALSE, warning=FALSE-----------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, dev = "tikz", cache = TRUE)


## ----preliminary-----------------------------------------------------------------------------------

# PRELIMINARY FUNCTIONS ########################################################

# Load the packages
sensobol::load_packages(c("data.table", "tidyverse", "openxlsx", "tm", "stringr", 
                          "pdftools", "tidytext", "scales", "cowplot", "lsa", 
                          "LSAfun", "ggrepel", "tidyquant"))

# Create custom theme
theme_AP <- function() {
  theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(fill = "transparent", color = NA), 
          strip.background = element_rect(fill = "white"), 
          legend.margin = margin(0.5, 0.1, 0.1, 0.1),
          legend.box.margin = margin(0.2,-4,-7,-7), 
          plot.margin = margin(3, 4, 0, 4), 
          legend.text = element_text(size = 8), 
          axis.title = element_text(size = 10),
          legend.key.width = unit(0.2, "cm"), 
          legend.key.height = unit(0.2, "cm"), 
          legend.title = element_text(size = 9)) 
}



## ----tests-----------------------------------------------------------------------------------------

# FUNCTIONS TO CLEAN THE TEXT ##################################################

# Function to remove words from text
removeWords <- function(str, stopwords) {
  x <- unlist(strsplit(str, " "))
  paste(x[!x %in% stopwords], collapse = " ")
}

# Function to remove punctuation, citations, numbers, stopwords in english, 
# bring to lowercase and strip whitespace, and especial characters, etc...
clear_text <- function(x, stem = TRUE) {
  
  y <- tolower(x)
  y <- str_replace_all(y, "[[:punct:]]", " ") # Remove punctuation characters
  y <- tm::removeNumbers(y)
  y <- tm::removeWords(y, stopwords::stopwords(language = "en"))
  y <- str_remove_all(y, "[^[\\da-zA-Z ]]")# Remove all non-alphanumerical
  y <- gsub("\\s[A-Za-z](?= )", " ", y, perl = TRUE) # Remove isolated letters
  #y <- tm::stripWhitespace(y)
  y <- str_squish(y)
  
  if (stem == TRUE) {
    y <- stemDocument(y) # Stem the document and keep only the root of the word
  }
  
  return(y)
}

# READ DATA ####################################################################

dt <- data.table(read.xlsx("final.dt.xlsx"))
dt[, keywords.large:= tolower(keywords.large)]
dt[, abstract.cleaned:= clear_text(abstract.large, stem = FALSE)]

# TERMS TO SEARCH ##############################################################

keywords <- c("validation", "verification", "calibration", "confirmation", "evaluation", 
              "benchmarking")
keywords.stemmed <- stemDocument(keywords)

# keywords.stemmed <- c(
#  "calibration", "verification", "validation", "evaluation",
#  "calibrations", "verifications", "validations", "evaluations",  
#  "calibrated", "verified", "validated", "evaluated",             
#  "calibrating", "verifying", "validating", "evaluating",        
#  "calibrative", "verificative", "validative", "evaluative",    
#  "calibratively", "verificatively", "validatively", "evaluatively"  
# )


# SEARCHING FOR TERMS ##########################################################

# Check which papers include keywords in abstract, keywords or title -----------
selected_cols <- c("title", "abstract", "keywords")
#selected_cols <- c("title.large", "abstract.large", "keywords.large")
out <- list()

for(i in 1:length(keywords.stemmed)) {
  
  out[[i]] <- dt[, lapply(.SD, function(x) 
    str_detect(x, keywords.stemmed[i])), .SDcols = (selected_cols)]
  
}

# ARRANGE DATA #################################################################

names(out) <- keywords.stemmed

valid.dt <- lapply(out, function(x) rowSums(x, na.rm = TRUE) > 0L) %>%
  do.call(cbind, .) %>%
  data.table() %>%
  .[, any.column:= rowSums(.SD) > 0]

full.dt <- cbind(dt, valid.dt)
full.dt.cols <- data.frame(full.dt[, .SD, .SDcols = keywords.stemmed])

full.dt[any.column == TRUE]

# DESCRIPTIVE STATISTICS #######################################################

full.dt[, lapply(.SD, sum), .SDcols = keywords.stemmed]

# Count number of papers with mentions to validation, validation + verification,
# validation + calibration, etc ------------------------------------------------

# Function to count the number of TRUE values shared between columns
count_shared_true <- function(data, cols) {
  sum(rowSums(data[, cols]) == length(cols))
}

# Create an empty list to store results
results_list <- list()

# Loop through all combinations of columns and count shared TRUE values
for (size in 2:length(keywords.stemmed)) {
  
  for (cols_combination in combn(ncol(full.dt.cols), size, simplify = FALSE)) {
    
    shared_true_count <- count_shared_true(data = full.dt.cols, cols = cols_combination)
    col_names <- colnames(full.dt.cols)[cols_combination]
    
    # Append results to the list
    results_list <- c(results_list, list(data.table(combination = paste(col_names, collapse = ", "), N = shared_true_count)))
  }
}

# Combine the list of data.tables into a single data.table
comb_dt <- rbindlist(results_list)


## ----plots_descriptive, dependson="tests", warning=FALSE-------------------------------------------

# PLOTS ########################################################################

plot.keywords.time <- merge(full.dt, full.dt[, .(total.papers = .N), year], by = "year") %>%
  melt(., measure.vars = keywords.stemmed) %>%
  .[, sum(value, na.rm = TRUE), .(variable, year, total.papers)] %>%
  .[, fraction:= V1 / total.papers] %>%
  ggplot(., aes(year, fraction, color = variable)) +
  scale_color_discrete(name = "") +
  geom_ma(ma_fun = SMA, n = 5, lty = 1) +
  theme_AP() +
  theme(legend.position = c(0.23, 0.85)) +
  labs(x = "Year", y = "Fraction papers")

plot.keywords.time

plot.keyword.per.model <- merge(full.dt, full.dt[, .(total.papers = .N), Model], by = "Model") %>%
  melt(., measure.vars = keywords.stemmed) %>%
  .[, sum(value, na.rm = TRUE), .(variable, Model, total.papers)] %>%
  .[, fraction:= V1 / total.papers] %>%
  ggplot(., aes(Model, fraction)) +
  geom_bar(stat = "identity", position = position_dodge(2)) +
  coord_flip() + 
  facet_wrap(~variable, ncol = 6) + 
  scale_y_continuous(breaks = pretty_breaks(n = 3)) +
  theme_AP() +
  labs(x = "", y = "Fraction papers") + 
  theme(axis.text.y = element_text(size = 7))

plot.keyword.per.model

plot.keyword.comb <- comb_dt[N > 10] %>%
  ggplot(., aes(reorder(combination, N), N)) +
  geom_bar(stat = "identity") + 
  coord_flip() +
  theme_AP() + 
  theme(axis.text.y = element_text(size = 7)) +
  labs(x = "", y = "Nº papers")

plot.keyword.comb 


## ----merge_plots_descriptive, dependson="plots_descriptive", fig.width=5.5, warning=FALSE----------

# MERGE DESCRIPTIVE PLOTS #####################################################

top <- plot_grid(plot.keywords.time, plot.keyword.comb, ncol = 2, labels = "auto", 
                 rel_widths = c(0.45, 0.55))

plot.merged <- plot_grid(top, plot.keyword.per.model, ncol = 1, 
                         labels = c("", "c"), rel_heights = c(0.45, 0.55))

plot.merged



## ----token_function--------------------------------------------------------------------------------

# TOKEN ANALYSIS ###############################################################

# Create function --------------------------------------------------------------
tokenize_fun <- function(dt, word, keywords, N.tokens) {
  
  # Create long dataset
  dt <- melt(dt, measure.vars = keywords)
  output <- dt[variable == word & value == TRUE]
  
  # Token analysis ------------------------------
  # We count the co-occurences of words without taking into account their order
  # within the n-token
  token.analysis <- output %>%
    unnest_tokens(bigram, abstract.cleaned, token = "ngrams", n = N.tokens) %>%
    separate(bigram, into = c("word1", "word2"), sep = " ") %>%
    data.table() %>%
    .[, `:=`(word1= pmin(word1, word2), word2 = pmax(word1, word2))] %>%
    count(word1, word2, sort = TRUE) %>%
    unite(., col = "bigram", c("word1", "word2"), sep = " ") %>%
    data.table()
  
  # Vector to retrieve only the bigrams with uncertainti or sensit 
  vec <- token.analysis[, str_detect(bigram, word)]
  
  # Final dataset
  output.dt <- token.analysis[vec]
  
  # Plot the q0 words most commonly 
  # associated with uncertainti and sensit ------
  plot.token <-  output.dt %>%
    .[, sum(n), bigram] %>%
    .[order(-V1)] %>%
    .[, head(.SD, 10)] %>%
    ggplot(., aes(reorder(bigram, V1, sum), V1)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    scale_y_continuous(breaks = pretty_breaks(n = 3)) +
    theme_AP() +
    labs(y = "$n$", x = "") +
    ggtitle(word) +
    theme(legend.position = "none", 
          plot.title = element_text(size = 11), 
          axis.text.y = element_text(size = 7))
  
  # Arrange and output --------------------------
  
  out <- list(output.dt, plot.token)
  names(out) <- c("data", "token")
  
  return(out)
  
}


## ----token_analysis, dependson=c("token_function", "tests")----------------------------------------

# RUN TOKEN ANALYSIS ###########################################################

N.tokens <- 2
token.dt <- list()

for (j in keywords.stemmed) {
  
  token.dt[[j]] <- tokenize_fun(dt = full.dt, word = j, 
                                keywords = keywords.stemmed, 
                                N.tokens = N.tokens)
  
}


## ----plot_merged_token, dependson=c("token_analysis", "merge_plots_descriptive"), fig.height=8, warning=FALSE, fig.width=6----

# PLOT RESULTS #################################################################

plot.tokens <- plot_grid(token.dt$valid[[2]], token.dt$verif[[2]], token.dt$calibr[[2]], 
                         token.dt$confirm[[2]], token.dt$evalu[[2]], token.dt$benchmark[[2]], 
                         ncol = 3)

plot.tokens

plot_grid(plot.merged, plot.tokens, ncol = 1, labels = c("", "d"), 
          rel_heights = c(0.57, 0.43))

