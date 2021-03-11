import os
import nltk
from nltk import FreqDist
from nltk.collocations import *
import re

review_list = []

'''
Part A
Data Pre-processing
Extracts only review texts and create a document to save the extracted text.
'''
# importing dataset
rs = os.path.exists('clothing_shoes_jewelry.txt')
if rs == True:
    read_file = open('clothing_shoes_jewelry.txt', 'r')
    contents = read_file.readlines()
    for line in contents:
        line = line.strip('\n')
        review_list.append(line)

# extracting required data
length = len(review_list)
review_content = []
for i in range(4, length):
    if review_list[i][0:10] == 'reviewText':
        review_content.append(review_list[i][11:])
    else:
        i = i + 1

# Save the list to local computer
fileSave = open('review_Content.txt','w')
for item in review_content:
    fileSave.write(item)
    fileSave.write('\n')
fileSave.close()

'''
Part B
• list the top 50 words by frequency
• list the top 50 bigrams by frequencies, and
• list the top 50 bigrams by their Mutual Information scores (using min frequency 5)
'''
# reading the cleaned dataset
rs = os.path.exists('review_Content.txt')
if rs == True:
    read_file = open('review_Content.txt', 'r')
    reviews = read_file.read()
review_tokens = nltk.word_tokenize(reviews)
review_lower = [w.lower() for w in review_tokens]

# Question One:
# List the top 50 words frequency
print("Top 50 words by using FreDist Function")
# define a re filter function
def alpha_filter(w):
  pattern = re.compile('^[^a-z]+$')
  if (pattern.match(w)):
    return True
  else:
    return False
alpha_review = [w for w in review_lower if not alpha_filter(w)]
# eliminate stop words
stopwords = nltk.corpus.stopwords.words('english')
review_no_stopwords = [w for w in alpha_review if w not in stopwords]
review_last = [w for w in review_no_stopwords if len(w) > 2 and w not in 'n\'t' and w not in '\'ve']
review_top50 = FreqDist(review_last).most_common(50)
# Save the list to local computer
fileSave = open('top_50_words.txt','w')
for item in review_top50:
    fileSave.write(str(item[0]+" " +str(item[1])))
    fileSave.write('\n')
fileSave.close()

# Question Two:
review_bigrams = [w for w in review_lower if not alpha_filter(w)]
review_bigrams = list(nltk.bigrams(review_bigrams))
review_bigrams_no_stopwords = [w for w in review_bigrams if w[0] not in stopwords and w[1] not in stopwords and w[0] not in 'n\'t' and w[1] not in 'n\'t']
scored = FreqDist(review_bigrams_no_stopwords).most_common(50)
# Save the list to local computer
fileSave = open('review_top50_bigram.txt','w')
for item in scored:
    fileSave.write(' '.join(str(a) for a in item) + '\n')
fileSave.close()

# Question Three:
# using finder to find all the bigram words and calculate the pmi
review_bigrams = list(nltk.bigrams(review_lower))
bigram_measures = nltk.collocations.BigramAssocMeasures()
finder = BigramCollocationFinder.from_words(review_lower)
finder.apply_word_filter(alpha_filter)
finder.apply_word_filter(lambda w: w in stopwords)
# filter: setting the word frequency to 5
finder.apply_freq_filter(5)
finder.apply_ngram_filter(lambda w1, w2: len(w1) < 3)
scored = finder.score_ngrams(bigram_measures.pmi)
# Save the list to local computer
fileSave = open('review_top50_pmi.txt','w')
for item in scored[1:50]:
    fileSave.write(' '.join(str(a) for a in item) + '\n')
fileSave.close()