#!/usr/bin/python

import operator, time, string, sys, re
import matplotlib.pyplot as plt

def ngrams(input, n):
  input = input.split(' ')
  output = {}
  for i in range(len(input)-n+1):
    g = ' '.join(input[i:i+n])
    output.setdefault(g, 0)
    output[g] += 1
  return output

def textAnalytics(file, stopwords_file):
#folder = '/Users/Luis/Documents/Programming/python-ideas/HuckFinn/'
#f = open(folder + 'TwainHuckFinn.txt', 'r')
  f = open(file, 'r')

  start = time.time()

  huck = {}
  for line in f:
    ngram = ngrams(line, 3)
    line = re.sub('^\d+\s','',line)
    line = line.split()
    for word in line:
      word = word.lower()
      new_word = word.translate(string.maketrans("",""), string.punctuation)
      if new_word in huck:
        huck[new_word] += 1
      else:
        huck[new_word] = 1

  f = open(stopwords_file, 'r')
  for line in f:
    stopword = line.rstrip('\n')
    stopword = stopword.lower()
    new_stopword = stopword.translate(string.maketrans("",""), string.punctuation)
    if new_stopword in huck:
      huck.pop(new_stopword, None)

  sorted_huck = sorted(huck.iteritems(), key=operator.itemgetter(1), reverse = True)
  elapsed = time.time() - start

  print 'Run took ', elapsed, ' seconds.'
  print 'Number of distinct words: ', len(sorted_huck)

  # Printing and plotting most popular words
  npopular = 50
  x = range(npopular)
  y = []
  for pair in range(npopular):
    y = y + [sorted_huck[pair][1]]
    print sorted_huck[pair]

  plt.plot(x, y, 'ro')
  plt.xlabel('Word ranking')
  plt.ylabel('Word frequency')
  plt.show()

def main():
  file = sys.argv[1]
  stopwords_file = sys.argv[2]
  textAnalytics(file, stopwords_file)

if __name__ == '__main__':
  main()
