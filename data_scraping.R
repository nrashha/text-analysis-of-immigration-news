# title: "data_scraping"

# Libraries:
library(tidyverse)
library(httr)
library(rvest)
library(tidytext)
# library(robotstxt)
# Note: Used Library robotstxt to check that scraping was allowed for 
# CNBC, Vox, and Dailywire

# Function to scrape and process articles
scrape_and_clean <- function(urls) {
  # Initialize empty vector to store articles
  articles_text <- c()
  
  # Loop through each URL and scrape
  for (url in urls) {
    # Read the HTML content of the page
    page_content <- read_html(url)
    
    # Check if the URL is from Vox and set the appropriate CSS selector
    if (grepl("vox.com", url)) {
      # Vox URL: Use the specific selector for Vox articles
      article <- page_content |>
        html_elements("div._1agbrixy > div.duet--article--article-body-component") |> 
        html_text2() |> 
        str_c(collapse = " ") |> 
        str_to_lower() |>        
        str_replace_all("[^a-z\\s]", "")  
      
    } else if (grepl("dailywire.com", url)) {
    
      article <- page_content |>
        html_elements("article > div.css-j7qwjs") |>
        html_text2() |>
        str_c(collapse = " ") |>
        str_to_lower() |>
        str_replace_all("[^a-z\\s]", "")
      
    } else if (grepl("cnbc.com", url)) {
      # CNBC URL: Use the specific selector for CNBC articles
      article <- page_content |>
        html_elements("div.ArticleBody-articleBody > div.group > p") |> 
        html_text2() |> 
        str_c(collapse = " ") |> 
        str_to_lower() |>        
        str_replace_all("[^a-z\\s]", "")  
    } else {
      stop("Unspecified url", url)
    }
    
    # Append the article content to the articles_text vector
    articles_text <- c(articles_text, article)
  }
  
  # Combine all articles into a single string
  articles_text <- str_c(articles_text, collapse = " ")
  
  # Tokenize text into words, remove stop words, and count occurrences
  cleaned_articles <- data.frame(text = articles_text) |> 
    unnest_tokens(word, text) |> 
    anti_join(stop_words, by = "word") |>
    count(word, sort = TRUE) |> 
    anti_join(stop_words, by = "word") |>
    filter(!word %in% c("immigrants", "immigration", "november"))

  return(cleaned_articles)
}


# CNBC URLs
cnbc_immigration_2024 <- c(
  "https://www.cnbc.com/2024/10/01/vance-fixing-social-security-requires-addressing-immigration-fraud.html?&qsearchterm=US%20immigration",
  "https://www.cnbc.com/2024/09/28/immigrants-ice-report-thirteen-thousand-homicide-us-border-migration.html?&qsearchterm=US%20immigration",
  "https://www.cnbc.com/2024/09/28/are-immigrants-taking-jobs-from-us-workers-heres-what-economists-say.html?&qsearchterm=US%20immigration",
  "https://www.cnbc.com/2024/09/10/jd-vance-false-conspiracy-haitian-immigrants-john-kirby.html?&qsearchterm=US%20immigration",
  "https://www.cnbc.com/2024/08/23/dnc-trump-harris-immigration-border-election.html?&qsearchterm=US%20immigration",
  "https://www.cnbc.com/2024/06/28/black-jobs-trump-draws-pushback-after-anti-immigration-rant.html?&qsearchterm=US%20immigration",
  "https://www.cnbc.com/2024/06/04/biden-immigration-order-economic-impact.html?&qsearchterm=US%20immigration",
  "https://www.cnbc.com/2024/05/03/immigrant-workers-are-helping-boost-the-us-labor-market.html?&qsearchterm=US%20immigration",
  "https://www.cnbc.com/2024/03/22/jpmorgan-research-boss-says-immigration-is-benefiting-the-us-economy.html?&qsearchterm=US%20immigration",
  "https://www.cnbc.com/2024/03/02/immigration-taking-pressure-off-the-job-market-us-economy-expert.html?&qsearchterm=US%20immigration"
)


cnbc_immigration_2016 <- c(
  "https://www.cnbc.com/2016/02/25/presidential-candidates-dont-get-it-on-immigration-commentary.html?&qsearchterm=US%20immigration%202016",
  "https://www.cnbc.com/2015/10/15/the-immigration-dilemma-for-the-gop-field.html?&qsearchterm=US%20immigration%202016",
  "https://www.cnbc.com/2016/02/01/what-iowa-doesnt-tell-us-about-immigration.html?&qsearchterm=US%20immigration",
  "https://www.cnbc.com/2016/09/01/donald-trumps-immigration-speech-proves-hell-never-pivot-commentary.html?&qsearchterm=US%20immigration",
  "https://www.cnbc.com/video/2016/07/21/syrian-trump-supporter-wants-to-see-us-crack-down-on-immigration.html?&qsearchterm=US%20immigration",
  "https://www.cnbc.com/2016/06/15/clinton-vs-trump-what-mexicans-think-about-the-us-election.html?&qsearchterm=US%20immigration",
  "https://www.cnbc.com/2016/07/22/obama-despite-heated-rhetoric-us-values-enduring-partnership-with-mexico.html?&qsearchterm=US%20immigration",
  "https://www.cnbc.com/2016/08/18/hillary-clintons-policies-will-support-us-economic-growth-long-term-says-anz.html?&qsearchterm=US%20immigration",
  "https://www.cnbc.com/2016/05/31/ahead-of-a-possible-trump-wall-a-look-at-one-us-border-town.html?&qsearchterm=US%20immigration",
  "https://www.cnbc.com/2016/07/18/the-5-biggest-state-casualties-in-trump-immigration-war.html?&qsearchterm=US%20immigration"
)
 

cnbc_immigration_2016 <- scrape_and_clean(cnbc_immigration_2016)
cnbc_immigration_2024 <- scrape_and_clean(cnbc_immigration_2024)


# Vox URLs
vox_immigration_urls_2024 <- c(
  "https://www.vox.com/policy/388685/tps-trump-haiti-venezuela-el-salvador-deportation",
  "https://www.vox.com/politics/387525/trump-canada-immigration-border", 
  "https://www.vox.com/policy/386094/birthright-citizenship-trump-2024-immigration",
  "https://www.vox.com/2024-elections/383845/trump-mass-deportations-immigration-family-separation", 
  "https://www.vox.com/policy/383286/chart-trump-immigration-crackdown-private-prison", 
  "https://www.vox.com/politics/380582/mass-deportations-trump-history-alien-enemies", 
  "https://www.vox.com/2024-elections/379883/mass-deportations-trump-harris-polling-immigration-border", 
  "https://www.vox.com/politics/378478/harris-immigration-border-progressive-agenda-2024-election", 
  "https://www.vox.com/politics/376797/trump-immigrants-bad-genes-nationalism",
  "https://www.vox.com/politics/375253/elon-musks-nonsensical-lies-about-immigrant-voting-briefly-explained")


vox_immigration_urls_2016 <- c(
  "https://www.vox.com/policy-and-politics/2016/12/26/14042118/donors-democrats-bernie-sanders", 
  "https://www.vox.com/policy-and-politics/2016/11/16/13649764/trump-muslim-register-database", 
  "https://www.vox.com/2016/11/5/13533816/melania-trump-illegal-immigrant", 
  "https://www.vox.com/2016/9/7/12817566/donald-trump-immigration-position", 
  "https://www.vox.com/2016/9/6/12814608/donald-trump-immigration", 
  "https://www.vox.com/2016/8/31/12735862/donald-trump-immigration-speech", 
  "https://www.vox.com/2016/8/31/12743880/donald-trump-immigration-policy-speech-transcript", 
  "https://www.vox.com/2016/8/31/12726292/immigration-speech-amnesty-deportation-border-meaning", 
  "https://www.vox.com/2016/8/30/12714288/trump-model-immigration", 
  "https://www.vox.com/2016/7/25/12270898/trump-style-immigration-proposals-appeal-big-donors-wall")

vox_immigration_2016 <- scrape_and_clean(vox_immigration_urls_2016)
vox_immigration_2024 <- scrape_and_clean(vox_immigration_urls_2024)


# Dailywire URLs
dw_immigration_urls_2024 <- c(
  "https://www.dailywire.com/news/chinese-illegal-immigrant-arrested-for-allegedly-shipping-weapons-from-california-to-north-korea", 
  "https://www.dailywire.com/news/over-1000-suspected-migrant-gang-members-roaming-nyc-ice-says", 
  "https://www.dailywire.com/news/boston-mayor-doubles-down-on-defying-trump-deportations-as-ma-police-arrest-migrants-for-child-rape", 
  "https://www.dailywire.com/news/illegal-alien-charged-with-raping-14-year-old-girl-in-denver-as-mayor-opposes-deportation-effort", 
  "https://www.dailywire.com/news/family-mourns-senseless-killing-of-michigan-pastor-who-was-struck-by-illegal-immigrant", 
  "https://www.dailywire.com/news/dem-gov-refuses-to-help-deportations-as-ice-busts-alleged-illegal-alien-child-rapists-in-her-state", 
  "https://www.dailywire.com/news/laken-rileys-killer-is-convicted-but-media-must-protect-the-narrative", 
  "https://www.dailywire.com/news/remove-these-criminals-and-thugs-trump-responds-after-laken-rileys-killer-is-convicted", 
  "https://www.dailywire.com/news/this-is-a-moral-outrage-house-gop-rips-biden-admin-for-policies-devastating-immigrant-children", 
  "https://www.dailywire.com/news/laken-rileys-stepdad-reads-one-of-her-last-journal-entries-in-court")


dw_immigration_urls_2016 <- c(
  "https://www.dailywire.com/news/trump-faces-difficult-battle-if-he-tries-pull-frank-camp", 
  "https://www.dailywire.com/news/walk-back-trumps-no-longer-interested-mass-frank-camp", 
  "https://www.dailywire.com/news/uh-oh-senators-seek-stop-deportations-under-trump-amanda-prestigiacomo", 
  "https://www.dailywire.com/news/al-qaeda-mastermind-we-were-going-exploit-frank-camp", 
  "https://www.dailywire.com/news/democrats-urge-obama-pardon-750000-illegal-michael-qazvini", 
  "https://www.dailywire.com/news/texas-legislator-introduces-bill-stop-illegal-frank-camp", 
  "https://www.dailywire.com/news/trump-begins-walk-back-illegal-immigration-frank-camp", 
  "https://www.dailywire.com/news/trump-surrogate-maybe-lets-go-back-self-aaron-bandler", 
  "https://www.dailywire.com/news/university-california-disobey-immigration-law-pardes-seleh", 
  "https://www.dailywire.com/news/report-crime-spikes-sanctuary-cities-hank-berrien")


dw_immigration_2024 <- scrape_and_clean(dw_immigration_urls_2024)
dw_immigration_2016 <- scrape_and_clean(dw_immigration_urls_2016)

save(cnbc_immigration_2016, file = "data/cnbc_immigration_2016.rda")
save(cnbc_immigration_2024, file = "data/cnbc_immigration_2024.rda")
save(vox_immigration_2016, file = "data/vox_immigration_2016.rda")
save(vox_immigration_2024, file = "data/vox_immigration_2024.rda")
save(dw_immigration_2016, file = "data/dw_immigration_2016.rda")
save(dw_immigration_2024, file = "data/dw_immigration_2024.rda")

