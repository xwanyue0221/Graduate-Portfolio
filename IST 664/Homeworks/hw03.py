#!/usr/bin/env python
# coding: utf-8

# # 664 NLP Assignment 3
# ## Wanyue Xiao
# ## Due: April 12, 2020

# The purpose of this assignment is to explore the sentiment of the comments at the sentence level. Specifically, this includes how to process the words and how to conduct the sentiment analysis using classifiers. Ultimately, two types of sentences, which are positive senetence and negative sentences, will be extracted and saved as files.

# In[1]:


import nltk
import sklearn
from nltk import *
from nltk.corpus import treebank
from nltk.corpus import sentence_polarity
import random
import re
import math
from IPython import display
from pprint import pprint
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.model_selection import train_test_split, cross_val_score, cross_validate, KFold
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.metrics import classification_report, confusion_matrix, accuracy_score
sns.set(style='darkgrid', context='talk', palette='Dark2')


# In[2]:


f = open('review_Content.txt')
review_text = f.read()


# By using the len function, we find that the review_text contains 88129772 words.

# In[3]:


len(review_text)


# ## Sentence Parsing

# Now, since we want to extract sentences from the review text, we need to parse those texts to sentences by using RegExr. Before that, we should replace '\n' with blank space and then use RegExr to parse sentence.

# In[4]:


review_text = review_text.replace('\n',' ')
sentence=re.compile("[A-Z].*?[\.\!\?]+", re.MULTILINE | re.DOTALL )
parsed_sentence = sentence.findall(review_text)


# For the target documents, it contains 1202086 sentences.

# In[5]:


len(parsed_sentence)


# ## Feature Selection

# We can use the sentence_polarity package's dataset to build models (or classifier). First of all, we need to get the sentences dataset.

# In[6]:


sentences = sentence_polarity.sents()


# Then, given that we have to use this dataset to train classifier, both the data of 'sentence' and 'tag' should be extracted from the dataset.

# In[7]:


documents = [(sent, cat) for cat in sentence_polarity.categories()
             for sent in sentence_polarity.sents(categories=cat)]


# Since the documents are in order by label, we mix them up for later separation into training and test sets.

# In[8]:


random.shuffle(documents)


# Here we put all the tokens inside the all_words_list. Then we can use the FreqDist function to calculate the word count of each token that is inside the all_words_list. After several experiments of feature words selection, we decide to use the top 3000 words (which is the optimal number) and store it inside the word_features object.

# In[24]:


all_words_list = [word for (sent,cat) in documents for word in sent]
all_words = nltk.FreqDist(all_words_list)
word_items = all_words.most_common(3000)
word_features = [word for (word,count) in word_items]


# Here we can build a dataframe to go through those top features. 
# <br>The top 20 tokens do not provide much meaningful information since most of them are punctuations and stopwords. We can find that '.' has appeared 14010 times because most of the sentences end up with stop sign. Besides, most of them are determinator or prepositions. Hence, we need to eliminate those words or punctuations.

# In[25]:


word_features_df = pd.DataFrame.from_records(word_items)
word_features_df.columns = ['Feature', 'Count']
word_features_df.head(20)


# Introducing the stopword from nltk package.

# In[11]:


stopwords = nltk.corpus.stopwords.words('english')


# After removing stopwords and punctuations from the all_words_list object, we can then, again, select the top 3000 words and store it in the word_features_noStop object.

# In[12]:


all_words_list_noStop = [word for word in all_words_list if word.isalpha()]
all_words_list_noStop = [w for w in all_words_list_noStop if w not in stopwords]
all_words_noStop = nltk.FreqDist(all_words_list_noStop)
word_items_noStop = all_words_noStop.most_common(3000)
word_features_noStop = [word for (word,count) in word_items_noStop]


# Then, we can go through those top features. Now, we can summarise that the token with the highest frequency is film (which is 1446). The second common token is movie, which is 1268. Besides, words like 'story', 'commedy', and 'characters' are also upon the list. Hence, we can speculate that the dataset records reviews of movie or film.

# In[13]:


word_items_noStop_df = pd.DataFrame.from_records(word_items_noStop)
word_items_noStop_df.columns = ['Feature', 'Count']
word_items_noStop_df.head(20)


# ## Data Cleaning

# Considering all the features we selected are lower-case, we need to convert the review_text to lower_case for the good of classification's performance.

# In[14]:


parsed_sentence_lower = [each.lower() for each in parsed_sentence]


# ## Classification Model

# ### Method 1: Baseline Model

# The first one is a baseline model which only uses the word_features (top 3000 corpus with stopwords and punctuations) we obtained above. Then we can create a document_features function to "dummy" the words contained inside the documents object.

# In[26]:


def document_features(document, word_features):
    document_words = set(document)
    features = {}
    for word in word_features:
        features['contains({})'.format(word)] = (word in document_words)
    return features


# The feature below use the document_features function created above to process the documents with word_features. Now we can get featureset which contains processed features and target tag.

# In[16]:


featuresets_base = [(document_features(d, word_features), c) for (d, c) in documents]


# For model performance evaluation, it is a commonplace to use cross-validation to assess models. The naivebayes model from nltk, however, does not support the cross-validation function or compatible with cross-validation function from sklearn package. Then, we build a crossvalidation function by using nltk.metrics function. This function will return a list which records average value of 'Accuracy', 'Pos Precision','Pos Recall','Pos F-Measure','Neg Precision','Neg Recall', and 'Neg F-Measure'.

# In[27]:


from nltk.metrics import precision
from nltk.metrics import recall
from nltk.metrics import f_measure

def printmeasures(num_folds, subset_size, featuresets_base):
    sum_acc = 0
    summary_get = []
    # define the sum value of precision, recall, and f-value for positive target
    sum_Ppre = 0
    sum_Precall = 0
    sum_Pf = 0
    # define the sum value of precision, recall, and f-value for negative target
    sum_Npre = 0
    sum_Nrecall = 0
    sum_Nf = 0
    
    for i in range(num_folds):
        testing_set = featuresets_base[i*subset_size:(i+1)*subset_size]
        training_set = featuresets_base[:i*subset_size] + featuresets_base[(i+1)*subset_size:]
        classifier = nltk.NaiveBayesClassifier.train(training_set)
        acc = nltk.classify.accuracy(classifier,testing_set)
        print('Iteration',i,'\'s Accuracy:', acc)
        sum_acc = sum_acc + acc

        reflist = []
        testlist = []
        for (features, label) in testing_set:
            reflist.append(label)
            testlist.append(classifier.classify(features))
            
        refpos = set()
        refneg = set()
        testpos = set()
        testneg = set()
        
        for i, label in enumerate(reflist):
            if label == 'pos': refpos.add(i)
            if label == 'neg': refneg.add(i)
        for i, label in enumerate(testlist):
            if label == 'pos': testpos.add(i)
            if label == 'neg': testneg.add(i)
        
        # calculate the precision value
        pre = precision(refpos, testpos)
        if pre is None:
            pre = 0
        sum_Ppre = sum_Ppre + pre
        # calculate the recall value
        rec = recall(refpos, testpos)
        if rec is None:
            rec = 0
        sum_Precall = sum_Precall + rec
        # calculate the f-measure value
        f_mea = f_measure(refpos, testpos)
        if f_mea is None:
            f_mea = 0
        sum_Pf = sum_Pf + f_mea
        
        # calculate the precision value
        Npre = precision(refneg, testneg)
        if Npre is None:
            Npre = 0
        sum_Npre = sum_Npre + Npre
        # calculate the recall value
        Nrecall = recall(refneg, testneg)
        if Nrecall is None:
            Nrecall = 0
        sum_Nrecall = sum_Nrecall + Nrecall
        # calculate the f-measure value
        Nf = f_measure(refneg, testneg)
        if Nf is None:
            Nf = 0
        sum_Nf = sum_Nf + Nf
    
    print("Performance Summary:")
    summary_get.append([round(sum_acc/num_folds,4)])
    summary_get.append([round(sum_Ppre/num_folds,4)])
    summary_get.append([round(sum_Precall/num_folds,4)])
    summary_get.append([round(sum_Pf/num_folds,4)])
    summary_get.append([round(sum_Npre/num_folds,4)])
    summary_get.append([round(sum_Nrecall/num_folds,4)])
    summary_get.append([round(sum_Nf/num_folds,4)])
    print("Accuracy:", sum_acc/num_folds)
    print("Pos_Precision:", sum_Ppre/num_folds)
    print("Pos_Recall:", sum_Precall/num_folds)
    print("Pos_F-Value:", sum_Pf/num_folds)
    print("Neg_Precision:", sum_Npre/num_folds)
    print("Neg_Recall:", sum_Nrecall/num_folds)
    print("Neg_F-Value:",  sum_Nf/num_folds)
    summary_get
    return(classifier)


# Calling the printmeasures() function to display the result. We can see that the average accuracy rate is 0.7624 which seems mediocre. The recall in positive target is 0.7507. Similarly, the recall value in negative target group is 0.7740 which seems fine. This indicates that the ability to predict negative sentence is better than that to predict positive sentences.

# In[18]:


printmeasures(5, 2000, featuresets_base)


# **Stopwords and Punctuations Remove** 
# <br> Now, by using the baseline model, we decide to remove stowords and to see what will happen.

# In[19]:


featuresets_noStop = [(document_features(d, word_features_noStop), c) for (d, c) in documents]


# Compared with the average accuracy of baseline model with stopwords and punctuations, that of baseline model without stopwords and punctuations (which is 0.7549) decreases by approximatly 0.1%. In this case, the recall value in negative group is still higher than recall value in positive group. The recall values in negative group decrease by nearly 0.3%. The recall in positive group reduces approximately by 1%. One can speculate that the removing stopwords and punctuations might negatively influence the model's ablity to predict sentences' category.

# In[20]:


printmeasures(5, 2000, featuresets_noStop)


# ### Method 2: Subjectivity Model with Stopwords

# Now, let's try another madol which introduces the subjectivity words as features.

# In[21]:


SLpath = 'subjclueslen1-HLTEMNLP05.tff'


# We have to create a function to extract contents from the SLpath document.

# In[28]:


def readSubjectivity(path):
    flexicon = open(path, 'r')
    # initialize an empty dictionary
    sldict = { }
    for line in flexicon:
        fields = line.split()   # default is to split on whitespace
        # split each field on the '=' and keep the second part as the value
        strength = fields[0].split("=")[1]
        word = fields[2].split("=")[1]
        posTag = fields[3].split("=")[1]
        stemmed = fields[4].split("=")[1]
        polarity = fields[5].split("=")[1]
        if (stemmed == 'y'):
            isStemmed = True
        else:
            isStemmed = False
        # put a dictionary entry with the word as the keyword
        #     and a list of the other values
        sldict[word] = [strength, posTag, isStemmed, polarity]
    return sldict


# In[23]:


SL = readSubjectivity(SLpath)


# Here we can see that the number of subjectivity words inside the document is 6885.

# In[24]:


len(SL.keys())


# After gathering the subjectivity words, we should create a feature generation function. The SL_features function will 'dummy' each word inside the sentence. Then, it will compare each word to words inside the subjectivity document. Once the word matchs the document, the corresponding variable will increase by 1. Finally, we can get two count varibales which records the count value of positive words and negative words.

# In[29]:


def SL_features(document, word_features, SL):
    document_words = set(document)
    features = {}
    for word in word_features:
        features['contains({})'.format(word)] = (word in document_words)
    # count variables for the 4 classes of subjectivity
    weakPos = 0
    strongPos = 0
    weakNeg = 0
    strongNeg = 0
    for word in document_words:
        if word in SL:
            strength, posTag, isStemmed, polarity = SL[word]
            if strength == 'weaksubj' and polarity == 'positive':
                weakPos += 1
            if strength == 'strongsubj' and polarity == 'positive':
                strongPos += 1
            if strength == 'weaksubj' and polarity == 'negative':
                weakNeg += 1
            if strength == 'strongsubj' and polarity == 'negative':
                strongNeg += 1
            features['positivecount'] = weakPos + (2 * strongPos)
            features['negativecount'] = weakNeg + (2 * strongNeg)      
    return features


# Using the SL_features function to fit the document.

# In[26]:


SL_featuresets = [(SL_features(d, word_features, SL), c) for (d, c) in documents]


# Again, calling the printmeasure function to conduct the cross-validation meathod. The average accuray is 0.7731, which is higher than that of baseline model with stop words and punctuations. The average recall of SL_features model in the positive and negative target groups is 0.770 and 0.775 respectively, which are also higher than those of baseline model in the positive and negative group. Therefore, it seems that this model is better than baseline model.

# In[27]:


printmeasures(5, 2000, SL_featuresets)


# **Stopwords and Punctuations Remove** 
# <br> Let's see what will happen to Subjectivity Model that is without Stopwords.

# In[28]:


SL_featuresets_noStop = [(SL_features(d, word_features_noStop, SL), c) for (d, c) in documents]


# Compared with the average accuracy of SL model with stopwords and punctuations, that of SL model without stopwords and punctuations decrease by nearly 0.9% which is a minor change. The recall values in positive and negative group are the same which is 0.764. The change between recalls in this model, for both groups, and recall of SL with stopwords is nearly 1%. This indicates that, for SL model, removing stopswords and punctuations might negatively influence the model's preidction performance.

# In[29]:


printmeasures(5, 2000, SL_featuresets_noStop)


# ### Method 3: Representing Negation 

# Examples of negation words include not, no, never, cannot, shouldn't, wouldn't, etc. Negation handling is an automatic way of determining the scope of negation and inverting the polarities of opinionated words that are actually affected by a negation.

# In[15]:


negationwords = ['no', 'not', 'never', 'none', 'nowhere', 'nothing', 'noone', 'rather', 'hardly', 'scarcely', 'rarely', 'seldom', 'neither', 'nor'] 


# Here we can create a NOT_features function. Start the feature set with all 6000 word features and 6000 Not word features set to false. If a negation occurs, add the following word as a Not word feature (if it’s in the top 6000 feature words), and otherwise add it as a regular feature word.

# In[30]:


def NOT_features(document, word_features, negationwords):
    features = {}
    for word in word_features:
        features['contains({})'.format(word)] = False
        features['contains(NOT{})'.format(word)] = False
    # go through document words in order
    for i in range(0, len(document)):
        word = document[i]
        if ((i + 1) < len(document)) and ((word in negationwords) or (word.endswith("n't"))):
            i += 1
            features['contains(NOT{})'.format(document[i])] = (document[i] in word_features)
        else:
            features['contains({})'.format(word)] = (word in word_features)
    return features


# Create feature sets as before, using the NOT_features extraction funtion, train the classifier and test the accuracy. 

# In[17]:


NOT_featuresets = [(NOT_features(d, word_features, negationwords), c) for (d, c) in documents]


# Calling the printmeasures() function to summarise the model performance. The average accuray is 0.7747, which is highest among those three models. The average recalls of negative target groups are also the highest up to now. Therefore, Negation Model is being recognized as the best in this report.

# In[20]:


printmeasures(5, 2000, NOT_featuresets)


# After deciding to select the negation model with stopwords and punctuations as the best model to predict the review s, we have to get a classifier based on this model.

# In[22]:


testing_set = NOT_featuresets[:1000]
training_set = NOT_featuresets[1000:]
classifier = nltk.NaiveBayesClassifier.train(training_set)


# **Stopwords and Punctuations Remove** 
# <br> Representing Negation without Stopwords and punctuations.

# Using a stopword list to prune the word features. We’ll start with the NLTK stop word list, but we’ll remove some of the negation words, or parts of words, that our negation filter uses. There are several words that are contained in both the negation and stopwords object. Hence, we need to find out those words and eliminate them from the stowords object.

# In[40]:


stopwords = nltk.corpus.stopwords.words('english')
newstopwords = [word for word in stopwords if word not in negationwords]


# In[41]:


len(newstopwords)


# Then, executing the same procedures to find out the top 3000 features.

# In[42]:


all_words_list = [word for (sent,cat) in documents for word in sent]
new_all_words_list = [w for w in all_words_list if w not in newstopwords]
new_all_words_list = [word for word in new_all_words_list if word.isalpha()]
new_all_words = nltk.FreqDist(new_all_words_list)
new_word_items = new_all_words.most_common(3000)
new_word_features = [word for (word,count) in new_word_items]


# In[43]:


NOT_featuresets_noStop = [(NOT_features(d, new_word_features, negationwords), c) for (d, c) in documents]


# Compared with the average accuracy of negation model with stopwords and punctuations, that of negation model without stopwords and punctuations decrease by nearly 0.03%. The recall values in positive group is 0.76 which is also lower than that of negation model with stopwords. The recalls negative group groups, however, is higher than that of negation model showed above. One can make an conclusion that removing punctuations and stopwords might not be a good idea for this dataset even though the change in accuracy between those two negation models is small.

# In[44]:


printmeasures(5, 2000, NOT_featuresets_noStop)


# ## Additional Experiment Part

# We can get two variables by splitting the documents object into two. The sents contain the sentences while the targets object contain labels.

# In[108]:


features = []
for (sent, tag) in documents:
    temp = []
    for word in sent:
        temp += (word,)
    temp = ' '.join(temp)
    features += (temp,)
labels = [tag for (sent, tag) in documents]


# **Split train and test set**<br>
# Let's split dataset by using function train_test_split(). This function needs 3 parameters, which are features, target, and test_set size. Additionally, we can use random_state to select records randomly.

# We can generate document term matrix by using scikit-learn's CountVectorizer.<br> 
# Inside the CountVectorizer function, we can set several parameters. The lowercase parameter is used to turn the text to lower case. Here we assign the values of stopwords to the stop_words parameter. Besides, we can create a tokenizer to remove unwanted elements from out data like symbols and numbers.

# In[109]:


from sklearn.feature_extraction.text import CountVectorizer
from nltk.tokenize import RegexpTokenizer
cv = CountVectorizer(max_features=3000)


# In[110]:


text_counts = cv.fit_transform(features)


# In[111]:


from sklearn.model_selection import train_test_split
X_train, X_test, y_train, y_test = train_test_split(text_counts, labels, test_size=0.3, random_state=1)


# Import scikit-learn metrics module for accuracy calculation and generate a model by using Multinomial Naive Bayes.

# In[112]:


from sklearn.naive_bayes import MultinomialNB
from sklearn import metrics

clf = MultinomialNB().fit(X_train, y_train)
predicted= clf.predict(X_test)
print("MultinomialNB Accuracy:",'\n',metrics.accuracy_score(y_test, predicted))
print("MultinomialNB Matrix:",'\n',confusion_matrix(y_test,predicted))
print("MultinomialNB Report:",'\n',classification_report(y_test,predicted))


# Let's build the **Text Classification Model using TF-IDF**.<br>
# In Term Frequency(TF), you just count the number of words occurred in each document. The main issue with this Term Frequency is that it will give more weight to longer documents. Term frequency is basically the output of the BoW model.
# <br>
# IDF(Inverse Document Frequency) measures the amount of information a given word provides across the document. IDF is the logarithmically scaled inverse ratio of the number of documents that contain the word and the total number of documents.
# <br>
# TF-IDF(Term Frequency-Inverse Document Frequency) normalizes the document term matrix. It is the product of TF and IDF. Word with high tf-idf in a document, it is most of the times occurred in given documents and must be absent in the other documents. So the words must be a signature word.

# In[115]:


from sklearn.model_selection import train_test_split
X_train, X_test, y_train, y_test = train_test_split(features, labels, test_size=0.3, random_state=1)


# In[116]:


from sklearn import preprocessing
le = preprocessing.LabelEncoder()
y_train = le.fit_transform(y_train)
y_test = le.fit_transform(y_test)


# In[117]:


from sklearn.feature_extraction.text import TfidfVectorizer

vectorizer = TfidfVectorizer(max_features=3000)
vectorizer.fit(features)
X_train = vectorizer.transform(X_train)
X_test = vectorizer.transform(X_test)


# In[118]:


print(vectorizer.vocabulary_)


# In[119]:


clf = MultinomialNB().fit(X_train, y_train)
predicted= clf.predict(X_test)
print("MultinomialNB Accuracy:",'\n',metrics.accuracy_score(y_test, predicted))
print("MultinomialNB Matrix:",'\n',confusion_matrix(y_test,predicted))
print("MultinomialNB Report:",'\n',classification_report(y_test,predicted))


# The classification rate of MultinomialNB using TF-IDF features is 0.7500, which is considered as good accuracy. <br>
# We need to improve the accuracy by using some other preprocessing or feature engineering. Let's suggest in comment box some approach for accuracy improvement.

# ### Random Forest with the Top 3000

# In the code below, we define that the max_features should be 3000, which means that it only uses the 2500 most frequently occurring words to create a bag of words feature vector. Words that occur less frequently are not very useful for classification. <br>
# Similarly, max_df specifies that only use those words that occur in a maximum of 80% of the documents. Words that occur in all documents are too common and are not very useful for classification. Similarly, min-df is set to 7 which shows that include words that occur in at least 7 documents.

# In[79]:


from sklearn.ensemble import RandomForestClassifier
text_classifier = RandomForestClassifier(n_estimators = 400, random_state=1)
text_classifier.fit(X_train, y_train)


# In[80]:


predictions = text_classifier.predict(X_test)


# In[81]:


print("Random Forest Accuracy:",'\n',accuracy_score(y_test, predictions))
print("Random Forest Matrix:",'\n',confusion_matrix(y_test,predictions))
print("Random Forest Report:",'\n',classification_report(y_test,predictions))


# ## Support Vector Machine (negation feature with stopwords)

# We can also use linear SVM. Linear SVM is better compare to other non-linear SVM models because text classification has lots of features. Mapping the data in linear SVM with higher dimensional (or large number of features) will improve the performance.

# In[82]:


import nltk.classify
from sklearn.svm import LinearSVC


# In[83]:


SVM = svm.SVC(C=1.0, kernel='linear', degree=3, gamma='auto')


# In[84]:


SVM.fit(X_train, y_train)
predictions_SVM = SVM.predict(X_test)


# In[85]:


print("Support Vector Machine Accuracy:",'\n',accuracy_score(y_test, predictions_SVM))
print("Support Vector Machine Matrix:",'\n',confusion_matrix(y_test,predictions_SVM))
print("Support Vector Machine Report:",'\n',classification_report(y_test,predictions_SVM))


# ## Label Review Text

# Add label using the classifier

# In[31]:


class_review = []
for sent in parsed_sentence_lower:
    feat = document_features(sent, word_features)
    tag = classifier.classify(feat)
    class_review += ((sent, tag),)


# seperate the pos and neg

# In[32]:


pos_review = []
neg_review = []
for (sent, tag) in class_review:
    temp = ''.join(sent)
    if tag == "pos":
        pos_review += (temp,)
    else:
        neg_review += (temp,)


# Saving the pos_review and neg_review objects to local computer

# In[33]:


# Save the list to local computer
fileSave = open('positive_review.txt','w')
for item in pos_review:
    fileSave.write(item)
fileSave.close()


# In[34]:


# Save the list to local computer
fileSave = open('negative_review.txt','w')
for item in neg_review:
    fileSave.write(item)
fileSave.close()


# Then, we can create a dataframe to list those positive and negative reviews.

# In[35]:


cols = ['Positive_review']
lst = []
for i in range(len(pos_review)):
    lst.append([pos_review[i]])
df_pos = pd.DataFrame(lst, columns=cols)
df_pos


# In[40]:


df_pos[:20]


# In[36]:


cols = ['Negative_review']
lst = []
for i in range(len(neg_review)):
    lst.append([neg_review[i]])
df_neg = pd.DataFrame(lst, columns=cols)


# In[39]:


df_neg[:20]


# In[37]:


df_pos.to_csv('positive.csv')


# In[38]:


df_neg.to_csv('negative.csv')

