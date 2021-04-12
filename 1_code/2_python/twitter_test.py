import pandas as pd
df = pd.read_csv("0_data/test_data/twitter_test.csv", encoding = 'utf8')
def strip_accents(text):
    return ''.join(char for char in
                   unicodedata.normalize('NFKD', text)
                   if unicodedata.category(char) != 'Mn')

df["text"] = df["text"].apply(strip_accents)

import re
import string

def clean_text(text):
    text = re.sub('\n',             '',  text) # Remove new line
    text = re.sub('@\w+ ',          ' ', text) # Removing @mentions
    text = re.sub('#',              '',  text) # Removing '#' hash tag
    text = re.sub('https?:\/\/\S+', '',  text) # Removing hyperlink
    text = re.sub(' +',             ' ', text)
    text = text.strip()
    return text

df['clean_text'] = df['text'].apply(clean_text)
df
from nltk.sentiment import SentimentIntensityAnalyzer
sia = SentimentIntensityAnalyzer()

def sentiment_calc(text):
    sent = sia.polarity_scores(text)
    return sent.get("compound")

df['sentiment'] = df['clean_text'].apply(sentiment_calc)

from textblob import TextBlob
pol = lambda x: TextBlob(x).sentiment.polarity
sub = lambda x: TextBlob(x).sentiment.subjectivity
df['polarity']     = df['clean_text'].apply(pol)
df['subjectivity'] = df['clean_text'].apply(sub)
df
df.to_csv('2_output/1_data/sentTest.csv', index = False)
