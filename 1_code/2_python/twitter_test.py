import pandas as pd

df    = pd.read_csv("0_data/test_data/twitter_test.csv", index_col = 0)
sales = pd.read_csv("0_data/test_data/sales_test.csv",   index_col = 0)

df.reset_index(inplace = True)

import nltk
from nltk.tokenize import sent_tokenize, word_tokenize
from nltk.corpus import stopwords
from sklearn.feature_extraction import text 
from sklearn.feature_extraction.text import CountVectorizer
import re

nltk.download('stopwords')
nltk.download('punkt')
nltk.download('averaged_perceptron_tagger')
nltk.download('wordnet')

stop_words     = stopwords.words('english')
stop_words2    = list(text.ENGLISH_STOP_WORDS)
add_stop_words = ['amp'] # amp is a nosense frequent word

for sw in (stop_words + stop_words2):
    # Consider all the stopwords without "'". For example "cant" instead of "can't"
    text = re.sub('\'', '', sw)
    add_stop_words.append(text)

stop_words = set(stop_words + stop_words2 + add_stop_words)

import re
import string

def clean_text_1(text):
    text = text.lower() # Lowercase
    text = re.sub('[%s]' % re.escape(string.punctuation), '', text) # Remove punctuation
    text = re.sub('[‘’“”…]', '', text) # Remove quotes
    text = re.sub('\n', ' ', text) # Remove new line
    text = re.sub('\\w*\\d\\w*', '', text) # Remove words containing numbers
    text = re.sub('\\[.*?\\]', '', text) # Remove special text in brackets ([chorus],[guitar],etc)
    words = word_tokenize(text)
    new_text = ""
    
    for w in words:
        if w not in stop_words and len(w) > 1:
            new_text = new_text + " " + w
    
    # Remove characters that are not letters or spaces
    new_text = re.sub('[^A-Za-z\\s]+', '', new_text)
    
    # Remove words with 1 char
    new_text = re.sub('\b[A-Za-z]{1}\b', '', new_text)
    return new_text

from sklearn.feature_extraction import text 
from sklearn.feature_extraction.text import CountVectorizer
from nltk.stem import PorterStemmer
from nltk.stem import LancasterStemmer
from nltk.stem import WordNetLemmatizer 
from nltk import word_tokenize, pos_tag
from nltk.stem import WordNetLemmatizer

wnl = WordNetLemmatizer()

data_clean = df.copy()
data_clean['text'] = data_clean['text'].apply(clean_text_1)
data_clean

from nltk.stem import WordNetLemmatizer
wnl = WordNetLemmatizer()

def lemmatize_tag(text):
    lemma=[]
    for i,j in pos_tag(word_tokenize(text)) :
        p=j[0].lower()
        if p in ['j','n','v']:
            if p == 'j':
                p = 'a'
            lemma.append(wnl.lemmatize(i,p))
        else :
            lemma.append(wnl.lemmatize(i))    
    return ' '.join(lemma)

# Apply lemmatizer
data_clean_OK      = data_clean.copy()
data_clean_OK.text = data_clean.text.apply(lemmatize_tag)
data_clean_OK.text

from sklearn.feature_extraction.text import CountVectorizer
# Recreate document-term matrix
cv = CountVectorizer(stop_words=stop_words)
data_cv = cv.fit_transform(data_clean_OK.text)
data_stop = pd.DataFrame(data_cv.toarray(), columns=cv.get_feature_names())
data_stop.index = data_clean_OK.index

# Let's make some word clouds
from wordcloud import WordCloud, STOPWORDS
import matplotlib.pyplot as plt
wc = WordCloud(collocations = False, stopwords = stop_words, background_color = "white", colormap = "Dark2", max_font_size = 150, random_state = 42)

# Reset the output dimensions
import matplotlib.pyplot as plt

plt.rcParams['figure.figsize'] = [20, 10]
# Generate the word cloud
wc.generate(' '.join((data_clean_OK.text.values)))

# Display the word cloud
plt.imshow(wc, interpolation = 'bilinear')
plt.axis('off')
plt.show()

cv      = CountVectorizer(min_df = 1, max_df = 1, stop_words = stop_words)
data_cv = cv.fit_transform(data_clean_OK.text)

from sklearn.feature_extraction.text import TfidfVectorizer

vectorizer = TfidfVectorizer(stop_words = stop_words)
tfidf      = vectorizer.fit_transform(data_clean_OK.text)
data_tfidf = pd.DataFrame(tfidf.toarray(), columns = vectorizer.get_feature_names())
data_tfidf.index = data_clean_OK.index
data_tfidf

from sklearn.decomposition import TruncatedSVD

# SVD represent documents and terms in vectors 
svd_model = TruncatedSVD(n_components = 30)

svd_model.fit(data_tfidf)

print(svd_model.components_.shape)
print(svd_model.singular_values_)

terms = vectorizer.get_feature_names()

for i, comp in enumerate(svd_model.components_):
    terms_comp = zip(terms, comp)
    sorted_terms = sorted(terms_comp, key= lambda x:x[1], reverse=True)[:7]
    print("Topic "+str(i)+": ")
    for t in sorted_terms:
        print("%.2f*%s "% (t[1], t[0]) ,end='')
    print("")
    
import nltk
nltk.download('vader_lexicon')

from nltk.sentiment.vader import SentimentIntensityAnalyzer

sid         = SentimentIntensityAnalyzer()
vader_score = data_clean_OK['text'].apply(lambda text: sid.polarity_scores(text))

from textblob import TextBlob
pol = lambda x: TextBlob(x).sentiment.polarity
sub = lambda x: TextBlob(x).sentiment.subjectivity
data_clean_OK['polarity']     = data_clean_OK['text'].apply(pol)
data_clean_OK['subjectivity'] = data_clean_OK['text'].apply(sub)
print(data_clean_OK.head())

topic_array = svd_model.transform(data_tfidf)
topic_array[topic_array >= 0.1] = 1
topic_array[topic_array <  0.1] = 0.

n_topics       = 10
columns_name   = [f'TOPIC {i}' for i in range(n_topics)]
df_topics      = pd.DataFrame(topic_array[:,:n_topics], index = data_clean_OK.index, columns = columns_name)
df_text_topics = data_clean_OK.merge(df_topics, left_index = True, right_index = True)

# Merge between sales dataset and twitter dataset
merge_sales = pd.merge(left = df_text_topics, right = sales.Retained, left_on = df_text_topics.ID_SALES, right_index = True)
merge_sales.drop(columns = ['key_0'], inplace = True)

# Save DB
merge_sales.to_csv('2_output/1_data/twitter_results_test.csv', index = False)
























