import random
import numpy
import re
from copy import deepcopy
import csv

critical_file = '../Stimuli/critical.tsv'
filler_file = '../Stimuli/fillers.tsv'
conds = ['reduced', 'unreduced']

def extract_stimuli(f, delimiter):
    l = []
    with open(f, "r") as file:
        for line in file:
            item = line.split(delimiter)
            l.append(item)
    return l


def gen_random_sequence(length, options, max_repeat, prev = []):
    n = length/options
    opt = range(options)
    l = []
    for item in range(options):
        l.extend([item]*n)

    rand = []
    valid = False

    while not valid:
        valid = True
        random.shuffle(l)
        x = deepcopy(prev)  #if prev is empty it does nothing
        x.extend(l)

        for i, element in enumerate(l[:-max_repeat]):
            sum_repeat = 0
            for j in range(max_repeat+1):
                if x[i] == x[i+j]: 
                    sum_repeat +=1
            if sum_repeat == max_repeat+1: valid = False
    return l


def assign_conditions(conds_list, sentence_list, rand_i):
    assigned = [[] for i in range(len(sentence_list))]

    for i,item in enumerate(sentence_list):
        assigned[rand_i].extend([item, conds_list[i]])
        rand_i +=1
        if rand_i >= len(sentence_list): rand_i = 0  #this makes it loop back
    return assigned


def create_lists(critical_items, max_repeat, conds):
    random.shuffle(critical_items)
    lists = [[] for i in range(len(conds))]
    rand = gen_random_sequence(len(critical_items), max_repeat, len(conds))
    for ind,item in enumerate(critical_items): # for each critical_item (i.e. [tag, sent1 ... sentn, ques, answer]), assigns all the sentences to random lists
        curr_conds = assign_conditions(item[1:-2], conds, rand[ind])

        for k,cond in enumerate(curr_conds):
            #create a list with [listname, cond, ques, answer]  -- cond is one sentence  
            cond_name = item[0] + '_' + cond[0]
            y = [cond_name,cond[1],item[-2], item[-1]]
            lists[k].append(y)
    return(lists)

def insert_fillers(lists, fillers):
    f = deepcopy(fillers)
    new_lists = [[] for i in range(len(lists))]

    random.shuffle(f)

    for i,l in enumerate(new_lists):
        curr = []
        filler_i = 0
        for item_i in range(len(lists[0])): 
            l.append(lists[i][item_i])  #current item
            if item_i%2 == 0: 
                l.append(f[filler_i])
                filler_i+=1

    for item in f[filler_i:]:
        rand_i = numpy.random.randint(len(new_lists[0]))
        for l in new_lists:
            l.insert(rand_i, item)

    return(new_lists)


def make_file(l, filename):
    with open(filename, "wb") as f:
        writer = csv.writer(f)
        writer.writerows(l)

def generate_files(new_filenames, max_repeat, conds):
    crit = extract_stimuli(critical_file, '\t')
    fillers = extract_stimuli(filler_file, '\t')
    list_names = ['1', '2']

    for n in new_filenames:
        x = create_lists(crit, max_repeat, conds)
        y = insert_fillers(x, fillers)

        for i,l in enumerate(y):
            l_reversed = list(reversed(l))

            file_l = '../Stimuli/%s%s.csv'%(n,list_names[i])
            file_l_reversed = '../Stimuli/%s%s_reversed.csv'%(n,list_names[i])

            make_file(l,file_l)
            make_file(l_reversed, file_l_reversed)


generate_files(['A', 'B'], 2, conds)





