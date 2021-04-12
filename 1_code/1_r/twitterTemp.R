twiter_orig <- readr::read_csv("2_output/1_data/twitter_results.csv") %>%
  mutate(tweetLength = nchar(text)) %>% 
  group_by(ID_SALES) %>% 
  summarise(pol_med   = polarity     %>% median(na.rm = TRUE),
            sub_med   = subjectivity %>% median(na.rm = TRUE),
            twl_med   = tweetLength  %>% median(na.rm = TRUE),
            numTweets = n())

twiter_new <- readr::read_csv("2_output/1_data/sentTest.csv", locale = readr::locale(encoding = "latin1")) %>%
  mutate(tweetLength = nchar(text)) %>% 
  filter(set == "train") %>%
  group_by(ID_SALES) %>% 
  summarise(sent_med  = sentiment_clean    %>% median(na.rm = TRUE),
            sub_med   = subjectivity_clean %>% median(na.rm = TRUE),
            twl_med   = tweetLength        %>% median(na.rm = TRUE),
            numTweets = n())



twiter_new %>% filter(sentiment != sentiment_clean)
twiter_new %>% filter(polarity  != polarity_clean)

twiter_new %>% filter(ID_SALES=="HD1076A")
  
twiter_new %>% filter(ID_SALES=="CD4706A")


twiter_orig %>% filter(ID_SALES=="HD1076A")
