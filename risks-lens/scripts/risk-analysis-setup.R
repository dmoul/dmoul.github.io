# risks-analysis-setup.R

library(here)
library(tidyverse)
library(tidytext)
library(stringi)
library(janitor)
library(pluralize)
library(lubridate)
library(fs)
library(rvest)
library(xml2)
library(glue)
library(hrbrthemes)
library(scales)
library(ggrepel)
library(ggtext)
library(tidylo)
library(openxlsx)
library(attempt)
library(ggridges)
library(patchwork)
library(gt)

# library(robotstxt) # used initially before scraping risk web pages; code no longer included

# library(spacyr) # included only in scripts that use spacyr to speed up other scripts
# https://spacy.io/models/en
# spacy_install(lang_models = c("en_core_web_md")) # only need to run this once to create spacy_condaenv and load model
# (assuming anaconda is already installed on your workstation?)
# then use spacy_initialize(model = "en_core_web_md")

risks_url <- 'https://catless.ncl.ac.uk/Risks/'

cachepages_path <- 'risks-lens/data/cache-pages'
cachedata_volume_path <- 'risks-lens/data/cache-data/volume'
cachedata_issue_path <- 'risks-lens/data/cache-data/issue'
cachedata_issuebodylemma_path <- 'risks-lens/data/cache-data/issue-body-lemma'
cachedata_issuebodynounphrase_path <- 'risks-lens/data/cache-data/issue-body-nounphrase'
processed_path <- 'risks-lens/data/processed-data'
output_path <- 'risks-lens/output' # for figures

my_dirs <- c("risks-lens/data", cachepages_path, cachedata_volume_path, cachedata_issue_path,
                    cachedata_issuebodylemma_path, cachedata_issuebodynounphrase_path,
                    processed_path, output_path)

if(!all(dir_exists(here(my_dirs)))) {
  message("Making sure all directories exist ...")
  walk(my_dirs, function(x) {
    if(!dir_exists(here(x))) {
      dir_create(here(x))
      message("Created ", x)
    } else {
      message("Already exists: ", x)
    }
  }
  )
}
# Note: assume <project-root>/scripts already exists (it's where this script lives)

fname_volume_table <- here(processed_path, "volume-table.rds")
fname_unigrams_all_title <- here(processed_path, "unigrams-all.rds")
fname_unigrams_basic_title <- here(processed_path, "unigrams-basic.rds")
fname_unigrams_places_title <- here(processed_path, "unigrams-places.rds")
fname_bigrams_title <- here(processed_path, "bigrams.rds")
fname_nounphrases_title <- here(processed_path, "nounphrases.rds")
fname_unigrams_all_body <- here(processed_path, "body-lemmas.rds")
fname_nounphrases_body <- here(processed_path, "body-nounphrases.rds")
fname_lemmas_body <- here(processed_path, "body-lemmas.rds")


set.seed(2020)
last_full_year <- 2020 #2019

n_words <- 20 # number of unigrams to include in graphs
n_bigrams <- 20 # number of bigrams to include in graphs
min_ngram_char <- 2 # filter out any unigrams shorter than this

### for plots and RMarkdown
knitr::opts_chunk$set(echo=FALSE, warning=TRUE, message=FALSE,
                      #fig.height=4, 
                      #dev = "png",
                      out.width='100%', 
                      fig.retina=3,
                      fig.topcaption=TRUE)

# making sure fonts are loaded in R
# https://github.com/hrbrmstr/hrbrthemes/issues/2
# library(extrafont)
# import_plex_sans()
# extrafont::loadfonts(quiet=FALSE)
options(hrbrthemes.loadfonts = TRUE)

theme_set(theme_ipsum_ps(base_size = 10) + ### doesn't appear to be working
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()
            )
)

my_caption = "Plot: Daniel Moul | Source: https://www.risks.org/"
fname_ext_plot <- ".png" # for cases in which we save plots individually
### end for plots

my_caption_sparkline_table <- "**Table**: Daniel Moul | **Source**: https://www.risks.org/<br>**Inspiration**: @thomas_mock"

######## for volumes-issues-subjects-prep.Rmd
url2filename <- function(u) {
  str_remove(u, "http[s]?://") %>%
    str_replace_all(., "/", "-") # make a valid file name
}

n_volumes <- 32
volume_start <- 1
volume_stop <- 32
## end for volumes-issues-subjects-prep.Rmd

###### for volumes-issues-subjects.Rmd ######
### end for volumes-issues-subjects.Rmd ######

###### for issue-detail-prep.Rmd ######
# https://stackoverflow.com/questions/26496538/extract-urls-with-regex-into-a-new-data-frame-column
url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"

pad_risk_urls <- function(url_list) {
  # input: vector of RISK Digest URLs
  # output: equivalently valid URLs but with assurance that 
  #         single-digit volume and issue numbers have a leading zero
  
  make_url_padding <- function(u) {
    new_volume <- str_pad(str_extract(u, "[0-9]+"), width = 2, side = "left", pad = "0")
    new_issue <- str_pad(str_extract(u, "[0-9]+$"), width = 2, side = "left", pad = "0")
    u <- str_replace(u, "[0-9]+", new_volume)
    u <- str_replace(u, "[0-9]+$", new_issue)
  }
  map_chr(url_list, make_url_padding)
}
## end for issue-detail-prep.Rmd ######

###### for title-word-analysis.Rmd ######

parsing_corrections_lemma <- 
  tribble(
    ~token,       ~fixed_word,
    "bia",        "bias",
    "gp",         "gps",
    "safer",      "safe",
    "fake_new",   "fake_news",
    "phishe",     "phishing",
    "americans",  "american",
    "datum",      "data",
    "metadatum",  "metadata",
    "glitches",   "glitch"
  )

parsing_corrections_2gram <-
  tibble::tribble(
                   ~token,       ~fixed_word,
             "cell phone",    "mobile phone",
         "cellular phone",    "mobile phone",
     "cellular telephone",    "mobile phone"
    )

parsing_corrections_body_nounphrase <- 
  parsing_corrections_2gram <-
  tibble::tribble(
    ~entity,        ~fixed_word,
    "krebs",       "krebs on security",
    "cryptogram",  "crypto-gram"
  )


  ### end for title-word-analysis.Rmd ######

###### for submitter-analysis.Rmd ######
# separate_rows() not working correctly
# since https://github.com/tidyverse/tidyr/issues/1014 is not on CRAN as of 2020-08-21
# remotes::install_github("tidyverse/tidyr")
# library(tidyr)

top_submitters_clip <- 100
top_sources_clip <- 25
## end for submitter-analysis.Rmd ######

###### for body-word-analysis.Rmd ######

# words we will ignore when considering frequency and trends
uninteresting_words <- c("use", "can", "kb", "may", "say", "get", "go",
                         "you", "what", "who", "man", "million", "new", "she", "them", "they", "their",
                         "risk", "risks", "computer", "re", "review", "system", "via", "software",
                         "et al", "computer risk", "chapter", "include",
                         "chapter eleven", "chapter twelve", "chapter sixteen") 

media <- c("newsscan", "newsscan daily", "infoword", "infoworld","infoworld home", "ny times", "fox news", 
           "idg news service", "risks-forum digest", "los angeles times", "wall street journal", "wsj",
           "san francisco chronicle", "washington post", "zdnet", "executive news service",
           "cbc news", "bbc news", "nbc news", "mercury news", "internationale medieninformatik",
           "pc world", "gawker media", "redmond magazine", "ars technica", "buzzfeed news",
           "ieee spectrum", "la times", "mit technology review", "edupage", "verge", "dejanews", 
           "daythink", "svenska dagbladet")

# common in body text signatures
companies <- c("sri international", "computers publishing", "mgmt consultant", "lgs inc.",
               "georgia institute of technology", "john wiley sons", "mcgraw-hill ryerson",
               "isect ltd. zealand", "ims health holdings", "macmillan computer publishing",
               "tttech", "lexis-nexis", "tech 1")

# common in body text signatures
places <- c("manvers stree", "canton st.", "north america", "west german", "bath ba1 1px",
            "flight international")

people <- c("lauren", "weinstein", "david", "john", "monte", "solomon", "melissa",
            "peter", "mark", "gene", "pgn", "pgn-ed", "pgn - ed", "rob", "wirchenko", "paul", 
            "henry", "slade", "monty solomon", "gabe goldberg", "dewayne hendricks",
            "monty solomon", "gabe goldberg", "dewayne hendricks", "peter mellor",
            "dave horsfall", "herb lin", "amos shapir", "peter ladkin",
            "dave kennedy", "paul robinson", "ted samson", "catalin cimpanu", 
            "charlie osborne", "lucian constantin", "jim horning", "robert dorsett", 
            "klaus brunnstein", "nick brown", "ross anderson", "don norman", 
            "jim h.", "alan wexelblat", "carpenter", "sean gallagher")
## end for body-word-analysis.Rmd ######

# for submitter-analysis.Rmd
# these were manually identified as "not a person" in the `entity` column created below.
# and added here, so that when the cleaning step is run again, the entities will be properly classified
# sources with frequency of 4 or more were checked (there are too many occurring 3, 2, and 1 to manually check them)
sources <- tibble(
  entity = 
    union(media,
          c("San Francisco Chronicle", "SF Cronicle", "SFO Chronicle", "SanFranChronicle", "NY Times", "The Guardian",
            "InfoWorld", "BBC", "Techcrunch", "Gizmodo", "The Washington Post", "Ars Technica", "PC World",
            "IEEE Spectrum", "Fortune", "ProPublica", "Recode", "Global Voices", "The Atlantic",
            "The New York Times", "NYT", "NY Times", "NYTimes", "Engadget", "BBC News", "BoingBoing", "Slate",
            "EFF", "Seattle Times", "TomsHardware", "Fox News", "The Register", "The Telegraph", "WiReD",
            "Times of Israel", "Bloomberg", "Scientific American", "CBS", "Haaretz", "USA Today", "CNN",
            "Irish Examiner", "San Francisco Chronicle", "News Observer", "Apple Insider",
            "WashPost", "WaPo", "WashPo", "Dashboard", "Foreign Policy", "Trend Micro", "Bleeping Computer",
            "NBC News", "The Verge", "Variety", "WSJ", "The National", "Chromium", "ThinkProgress", "Amazon Grace",
            "TechWorm", "ITProToday", "ZDnet", "Security Intelligence", "TrustWave", "Daily Mail", "MakeUseOf",
            "Coindesk", "Coindesk", "Tenable", "9to6mac", "USNews", "NPR", "The Tribune India", "EWeek",
            "The Times of Israel", "Newsweek", "bbc.com", "BBC.com", "The Weekly Standard", "PopSci", "Google",
            "CSO Online", "Naked Security", "Tech Times", "CNNPolitics", "Cisco", "Vox", "Medium",
            "Consumer Reports", "Fly&Dine", "Straits Times", "Hamodia", "SecurityAffairs", "buzzfeed",
            "Krebs on Security", "Krebs", "Malwarebytes Labs", "TTNews", "Slashdot", "Conde-Nast",
            "Healthcare IT", "CBC News", "CJR", "TechBeacon",
            "The Los Angeles Times", "LATimes", "LA Times", "latimes", "The Wrap", "Fast Company",
            "Talking Points Memo", "GovTech", "ZDNet", "Defense One", "HPE", "Medscape", "Business Insider",
            "Facial Personality Analytics", "Computing Edge", "Web Informant", "CNBC", "JapanTimes", "Japan Times",
            "Hollywood Reporter", "Forbes e-news", "Insider", "StL Today", "HP", "SciAm", "Wol", "McClatchy",
            "vtdigger", "ComputerWorld", "zdnet", "NPR.org", "The Boston Globe", "BostonGlobe", "TacticalTech",
            "Sora News", "9to5Mac", "ycombinator", "MSN", "The Daily Gazette", "San Antonio Business Journal",
            "The Economist", "MIT Technology Review", "The Intercept", "The Hill", "Austin American Statesman",
            "DJC", "MIT News", "InfoSecurity", "CSO", "GC", "CTA", "The Sun", "CES 2019", "TheNextWeb", "Ars",
            "Trendmicro", "Firemountain", "Moscow Times", "SecJuice", "techcrunch.com", "The Times of Israel",
            "Consumer Health", "Washington Consumers' Checkbook", "Cell.com", "YouTu", "TechBeacon",
            "NBC Bay Area", "AZFamily", "USA Today", "Chronicle of Higher Education", "The Straits Times",
            "StraitsTimes", "IBTimes", "IB Times", "Financial Times", "Washington Times", "Times Colonist",
            "Taipei Times", "Chicago Sun Times", "Brisbane Times", "538", "David Farber's IP", "InfoWorld",
            "UPI", "CERT", "ACM", "ACLU", "The Onion", "The New Yorker", "The Statesman", "The State",
            "The Standard", "The Stack", "NNSquad", "The Tech Portal", "The Week", "The White House",
            "AirForceMag", "Akamai", "Island Health", "IT World", "IT News", "IT Pro", "ITProToday",
            "Think Progress", "WSN", "xkcd", "X-Force", "WTU", "WU", "WTOP", "WYFF4", "Yahoo", "Anonymous",
            "Zurich Insurance", "YNetNews", "Year2000InfoNet", "NewsScan", "Edupage", "NNSquad", "EPIC",
            "UK National Crime Agency", "Malware Bytes", "CISA", "WIRED", "Aerospace.org", "Politico Europe",
            "MIT", "CBC", "Vice", "NASA", "WEForum", "MIT Tech Review", "Clean Technica", "DCist",
            "Cryptography", "LW", 	"nbc", "Forbes", "AP", "AIMac", "almac", "Wols", "Politico", "Techdirt",
            "huffpost", "softpedia", "QZ", "ThreatPost", "TorrentFreak", "Alpha Lau", "ark", "Huge",
            "Sydney Morning Herald", "E-Week", "EEKid", "PCMag", "RISKS", "Fox", "TRK", "AFP",
            "cryptogram", "crypto-gram", "new scientist", "mother jones", "ifixit", "reuters",
            "comparitech", "espn", "the hacker news", "federal news network", "lifewire", "outcomehealth",
            "cnet", "flyer talk", "denver post", "nj tech weekly", "macrumors", "thehackernew", "the hacker news",
            "technology review", "skeptical inquirer", "marketwatch", "statnews", "congressional budget office",
            "youtube", "hackaday", "fedscoop", "prospect", "metro", "inquirer", "spectator", "vox.com",
            "abc", "bloomberg law", "citylab", "nypost", "gizmodo", "associated press", "privacy forum",
            "ieee spectrum", "computer weekly", "newsweek", "aviation week", "independent", "der spiegel",
            "federal register", "idg news service")
    )
) %>%
  mutate(entity = tolower(entity)) %>%
  arrange(entity) %>%
  distinct(entity)

# end for submitter-analysis.Rmd