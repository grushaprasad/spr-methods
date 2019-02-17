import csv
import re

total_sentences = 48

filenames = ['../Stimuli/A1.csv',
            '../Stimuli/A2.csv',
            '../Stimuli/B1.csv',
            '../Stimuli/B2.csv',
            '../Stimuli/A1_reversed.csv',
            '../Stimuli/A2_reversed.csv',
            '../Stimuli/B1_reversed.csv',
            '../Stimuli/B2_reversed.csv']


readers = [list(csv.reader(open(x), delimiter = ',')) for x in filenames]

crit_dict = {}
filler_dict = {}
sent_dict = {}

for s in range(total_sentences):
    for reader in readers:
        row = reader[s]
        row[3] = row[3].strip('\r\n')
        #print row
        #print row[3]
        resp = 'NA'
        if row[3] == 'Yes':
            resp = 0
        elif row[3] == 'No':
            resp = 1
        else:
            print 'Did not find yes or no'
        if s == 0:
            'hello'
            #print '[["%s",1], "DashedSentence", {s: "%s"}, "Question", {q: "%s", hasCorrect: %s}],' %(row[0], row[1], row[2], resp) 
        else:
            'hell0'
            #print '[["%s",[%s,1]], "DashedSentence", {s: "%s"}, "Question", {q: "%s", hasCorrect: %s}],' %(row[0], s+1, row[1], row[2], resp)

        if row[0] in sent_dict:
            sent_dict[row[0]] +=1
        else:
            sent_dict[row[0]] = 1

print 'testing'
for key in sent_dict.keys():
    print sent_dict[key]
  #if sent_dict[key] != 4 or sent_dict[key] != 8 : print key
