# Loading packages
library(readr)      # Read data through C++
library(dplyr)      # Parse manipulation of data through C++
library(quanteda)   # Manipulate text data
library(tidytext)   # Structure text through C++
library(tonR)       # Some functions for reading Talk of Norway CoNLL-data
library(pbmcapply)  # Parallelization with progress bar
library(stringr)

# Reading the main frame of ToN
meta <- read_csv("../../../gitDebates/talk-of-norway/data/ton_updated.csv", progress = TRUE)

com <- pbmclapply(1:nrow(meta), function(x){
  
  # Extrating committee membership
  data.frame(com_member = unlist(strsplit(meta$com_member[x], " ; ")),
             com_date = unlist(strsplit(meta$com_date[x], " ; ")),
             com_role = unlist(strsplit(meta$com_role[x], " ; ")),
             date = meta$date[x],
             stringsAsFactors = FALSE)
  
}, mc.cores = detectCores()-1)

# Seting id on the speeches
names(com) <- meta$id

# Binding all rows
com <- do.call(rbind, com)

# Fixing id names
com$id <- gsub("\\.(.*?)$", "", rownames(com))

# Making start and end date variables for committee membership
com$date <- as.Date(com$date)
com$com_start <- as.Date(str_split(com$com_date, " - ", n = 2, simplify = TRUE)[, 1], format = "%d.%m.%Y")
com$com_end <- as.Date(str_split(com$com_date, " - ", n = 2, simplify = TRUE)[, 2], format = "%d.%m.%Y")
com$com_check <- ifelse(com$com_start < com$date & com$com_end > com$date, "yes", "no")

# Removing rows with committee membership that was not active at the time of the speech
com <- com[which(com$com_check != "no"), ]

com$com_role2 <- factor(com$com_role,
                        levels = rev(c("Stortingets president", "Stortingets visepresident og forsvarskomiteens leder",
                                       "Leder", "Forsvarskomiteens leder", "Fung. leder gruppestyret",
                                       "Nestleder", "Sekretær", "Første nestleder", "Andre nestleder",
                                       "Medlem", "Varamedlem", "Personlig varamedlem")))

com <- com |>
  group_by(id) |>
  summarize(high_com = com_member[which.max(com_role2)],
            com_role = com_role2[which.max(com_role2)])

# Making dummy sets for all committee memberships
for(i in unique(com$high_com)){
  com[, tolower(gsub(" |[[:punct:]]", "", i))] <- ifelse(com$high_com == i, 1, 0)
}


# Aggregating to speech level
com <- com |>
  group_by(id, com_role) |>
  summarize_at(vars(matches("komit")), sum)

# Merging with original data
meta <- merge(x = meta, y = com, by = c("id"), all.x = TRUE)


# Subsetting the data to use
meta <- meta |>
  filter(is.na(party_id) == FALSE &
           is.na(county) == FALSE &
           is.na(session) == FALSE) |>       # Excludes rows with missing on party_id, county, and session
  filter(speaker_role == "Representant") |>  # Subsetting only representatives
  filter(session == "2013-2014" |
           session == "2014-2015") |>        # Subsetting the two relevant sessions
  filter_at(vars(matches("komit")), any_vars(is.na(.) == FALSE))

meta$com_role <- meta$com_role.y
meta$com_role.y <- NULL
meta$com_role.x <- NULL

meta$com_role <- ifelse(meta$com_role == "Varamedlem", "Medlem", as.character(meta$com_role))

# Reading the CoNLL-files for the remaining data
lemmas_all <- pbmclapply(meta$id, function(x){
  read.conll("../../../gitDebates/talk-of-norway/", x)
}, mc.cores = detectCores()-1, ignore.interactive = TRUE)

# From list to data frame
lemmas_all <- bind_rows(lemmas_all)

# Manipulating the text
lemmas_all <- lemmas_all |>
  filter(grepl("[[:punct:]]|\\–|^AV$", lemma) == FALSE) |>    # Removing "AV" because of tagger fail (not important as it is a stopword)
  group_by(id, sentence) |>
  mutate(lemma = tolower(lemma),                                        # Lowercasing all lemma
         lemma = ifelse(lemma %in% stopwords("norwegian"), NA, lemma),  # Removing stopwords
         lemma_pos = paste(lemma, part_of_speech, sep = ":"),
         next_lemma_pos = lead(lemma_pos),                                      # Leading lemma for bigram construction
         lemma_pos_bigram = ifelse(grepl("^NA\\:|\\:NA$", lemma_pos) | grepl("^NA\\:|\\:NA$", next_lemma_pos),        # Constructing bibrams
                                   NA, paste(lemma_pos, next_lemma_pos)),
         lemma_pos = ifelse(grepl("^NA\\:|\\:NA$", lemma_pos), NA, lemma_pos)) |>
  filter((is.na(lemma_pos) == FALSE | is.na(lemma_pos_bigram) == FALSE))

lemmas_all <- lemmas_all |>
  mutate(lemma_pos_bigram = ifelse(grepl("\\sNA$", lemma_pos_bigram) == FALSE, lemma_pos_bigram, NA)) |>
  filter(is.na(lemma_pos) == FALSE | is.na(lemma_pos_bigram) == FALSE)

# Counting lemma bigrams
lemma_unigrams_pos <- lemmas_all |>
  filter(is.na(lemma_pos) == FALSE) |>
  group_by(id) |>
  count(lemma_pos)

# Counting lemma bigrams
lemma_bigrams_pos <- lemmas_all |>
  filter(is.na(lemma_pos_bigram) == FALSE) |>
  group_by(id) |>
  count(lemma_pos_bigram)

names(lemma_bigrams_pos)[2] <- "lemma_pos"

lemma_counts <- bind_rows(lemma_unigrams_pos, lemma_bigrams_pos) |>
  filter(is.na(lemma_pos) == FALSE) |>                              # Removing if lemma is NA
  group_by(id) |>                                                   # Grouping by speech
  filter(length(lemma_pos) >= 20) |>                                # Removing speeches shorter than 20 lemmas
  count(lemma_pos) |>                                               # Counting the occurance of each lemma in each group
  cast_dfm(id, lemma_pos, nn) |>                                    # Casting to document frequency matrix
  dfm_trim(min_docfreq = 20, min_termfreq = 5)                       # Trimming dfm to only contain speeches with more than 19 lemmas and
# lemmas that occur more than 4 times across all documents

# Subsetting the speeches that remain in the meta data
meta <- meta[which(meta$id %in% lemma_counts@Dimnames[[1]]), ]

# Removing redundant objects
rm(lemmas_all, lemma_bigrams_pos, lemma_unigrams_pos, i, com)

# Saving the R-environment
save.image(file = "./data/preprocess/reps_preproc_bigram_pos_com.rda")