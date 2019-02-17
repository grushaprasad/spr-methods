import json
import csv
import re
from string import digits
data_file = '../Data/SPR_results_MTURK.txt'
spr_filename = '../Data/mturk.csv'
demographic_filename = '../Data/mturk_demographic.csv'

#f = open('good_participants.txt', 'r')
#good_participants = json.loads(f.read())
# good_participants = [x.strip() for x in good_participants]
# f.close()



# g = open('good_participants_lists.txt', 'r')
# good_participants_lists = g.read().splitlines()
# good_participants_lists = [x.strip() for x in good_participants]
# g.close()

#repeated_participants = [('NDouglass', 4)]
repeated_participants = []

data = []

indices = {
    'reduced': {0: 'NP1', 1: 'NP1', 2: 'V', 3: 'Ambig', 4: 'Ambig', 5: 'Ambig', 6:'Disambig_region', 7: 'Disambig_region', 8: 'Disambig_region'},
    'unreduced': {0: 'NP1', 1: 'NP1', 2: 'rel_pronoun', 3: 'aux', 4: 'V', 5: 'Ambig', 6: 'Ambig', 7: 'Ambig', 8:'Disambig_region', 9: 'Disambig_region', 10: 'Disambig_region'},
}




#splits data into spr, lextale and demographic data
def split_data(participant):
    clean_data = []
    lextale_data = []
    demographic_data = {}
    age_count = 0
    for item in participant[3]:
        
        # if i<10: print item
        # i+=1
        sent_type = item[3][1][0:3]
        #print sent_type
        if sent_type in ['fil', 'RRC']:
            clean_data.append(item)
        if sent_type == 'dem':
            #demographic_data.append(item[6][1])
            #print item[6][1]
            #print item[5][1]
            if item[5][1] != '_REACTION_TIME_':
                if item[5][1] == 'age':
                    if age_count == 1:
                        demographic_data['age_acquisition'] = item[6][1]
                    else:
                        demographic_data[item[5][1]] = item[6][1]
                    age_count +=1
                else: 
                    demographic_data[item[5][1]] = item[6][1]

    return (clean_data,demographic_data)


#For a given participant, returns lists of all relevant data. Each list is of the same length so that this can later be used in long form in R.         
def process_participant(participant_data):
    sent_type = ''
    i = 0
    all_words = []
    all_sentences = []
    all_rt = []
    all_resps = []
    all_regions = []
    all_sent_types = []
    all_sent_pos = []
    all_sent_nums = []
    all_sent_ids = []
    all_ambig_nums = []
    all_RRC_nums = []
    all_crit_nums = []

    x = 0
    curr_sent_num = 0
    curr_RRC_num = 0

    while i < len(participant_data):
        sentence_pos = 0
        sent_just_ended = False
        words = []
        rt = []
        sentence = []
        region = []
        resps = []
        sent_type = []
        sent_pos = []
        sent_nums = []
        RRC_nums = []
        sent = ''  # having this because every row (in long form) needs to have the sentence it is associated with. This information is not noted for the question. SO keeping track
        sent_ids = []

        while not sent_just_ended:
            sentence_pos+=1

            curr = participant_data[i]
            curr_cond = str.split(str(curr[3][1]), '_')[-1:][0] #split by '_' and get the last element
            
            curr_item = str.split(str(curr[3][1]), '_')[0]
            item_num = int(re.findall('\d+', curr_item)[0])
            curr_cond = curr_cond.translate(None, digits)

            if sentence_pos == 1:
                curr_sent_num +=1
                if curr_cond == 'reduced':
                    curr_RRC_num +=1
            
            sent_type.append(curr_cond)
            sent_pos.append(sentence_pos)
            sent_nums.append(curr_sent_num)
            RRC_nums.append(curr_RRC_num)

            sent_ids.append(curr[3][1])
            
            #print curr_cond
            if re.findall('filler', curr_cond):
                curr_region = 'filler'
            elif curr_cond in indices:
                if sentence_pos-1 in indices[curr_cond]:
                    curr_region = indices[curr_cond][sentence_pos-1]
                else:
                    curr_region = 'other'
            # else:
            #     curr_region = 'other RRC'
            
            if not curr[0][1] == 'Question':
                words.append(curr[6][1])
                rt.append(curr[7][1])
                sent = curr[9][1]
                if sent[-1] != '.': 
                    sent = sent+'.'  #if for some reason sentences don't have full stop at the end add it.
                sentence.append(sent)
                resps.append('NA')
                region.append(curr_region)
            else:
                words.append(curr[5][1])
                rt.append(curr[8][1])
                sentence.append(sent)
                resps.append(curr[7][1])
                sent_just_ended = True
                
                
                region.append('Question')
            

            i+=1
        
        #sent_ids = [sent_id]*len(words)

        all_words.extend(words)
        all_sentences.extend(sentence)
        all_rt.extend(rt)
        all_resps.extend(resps)
        all_regions.extend(region)
        all_sent_types.extend(sent_type)
        all_sent_pos.extend(sent_pos)
        all_sent_nums.extend(sent_nums)
        all_sent_ids.extend(sent_ids)
        all_RRC_nums.extend(RRC_nums)

    return(all_words, all_sentences, all_rt, all_regions, all_sent_pos, all_resps, all_sent_types, all_sent_nums, all_RRC_nums, all_sent_ids)

#For a given participant returns list of all words in lextale and a corresponding list of response (i.e. correct or wrong)
def process_participant_lextale(lextale_data):
    words = []
    resps = []
    for item in lextale_data:
        words.append(item[5][1])
        resps.append(item[7][1])
    return(words,resps)

# Process all participants and generates three csv files
def process_all_data(data, spr_filename, demographic_filename):
    all_words = []
    all_sentences = []
    all_rt = []
    all_regions = []
    all_resps = []
    all_sent_types = []
    all_sent_pos = []
    all_sent_nums = []
    all_RRC_nums = []
    all_sent_ids = []
    participant_ids = []
    participant_lists = []
    all_lextale_words = []
    all_lextale_resps = []
    all_lextale_participant_ids =[]
    all_demographic_data = []
    not_good = []
    good = []

    #part_pos = 0
    for participant in data:
        participant_id = participant[3][0][6][1]
        #used to check for repeated participants.. Will get rewritten later with the appropriate list
        participant_list = (participant[1] % 8)   


       # if str(participant_id.strip()) in good_participants.keys() and ((participant_id.strip(), participant_list) not in repeated_participants):
            #print part_pos
        #participant_list = good_participants[participant_id.strip()]
        #part_pos+=1
        if not (participant_id.strip(), participant_list) in repeated_participants:
            # print (participant_id.strip(), participant_list)
            clean_data,demographic_data = split_data(participant)
            #print demographic_data
            words, sentences, rt, regions, sent_pos, resps, sent_types, sent_nums, RRC_nums, sent_ids = process_participant(clean_data)
            #lextale_words, lextale_resps = process_participant_lextale(lextale_data)
            demographic_data['participant_id'] = participant_id
            #print lextale_data
            if not all(len(lst) == len(words) for lst in [words, sentences, rt, regions, resps]):
                print 'for participant %s, not all lists are equal' %(participant_id)
                print 'Overall not all lists are equal'
                print 'words: %s' %(len(words)) 
                print 'sentences: %s' %(len(sentences))
                print 'rt: %s' %(len(rt))
                print 'indices: %s' %(len(indices))
                print 'resps: %s' %(len(resps))
                print

            all_words.extend(words)
            all_sentences.extend(sentences)
            all_rt.extend(rt)
            all_regions.extend(regions)
            all_resps.extend(resps)
            all_sent_types.extend(sent_types)
            all_sent_pos.extend(sent_pos)
            all_sent_nums.extend(sent_nums)
            all_sent_ids.extend(sent_ids)
            all_RRC_nums.extend(RRC_nums)
            participant_ids.extend([participant_id]*len(words))
            participant_lists.extend([participant_list]*len(words))

            # all_lextale_words.extend(lextale_words)
            # all_lextale_resps.extend(lextale_resps)
            # all_lextale_participant_ids.extend([participant_id]*len(lextale_words))

            all_demographic_data.append(demographic_data)


        
        if not all(len(lst) == len(all_words) for lst in [all_words, all_sentences, all_rt, all_regions, all_sent_pos, all_resps, participant_ids, participant_lists]):
                    print 'Overall not all lists are equal'
                    print 'words: %s' %(len(all_words)) 
                    print 'sentences: %s' %(len(all_sentences))
                    print 'rt: %s' %(len(all_rt))
                    print 'indices: %s' %(len(all_indices))
                    print 'resps: %s' %(len(all_resps))
                    print 'participant_ids: %s' %(len(participant_ids))
                    print 'participant_lists: %s' %(len(participant_lists))
                    print


        with open(spr_filename, "wb") as f:
            f.write(','.join(['sent_id', 'sentence', 'word', 'sentence_type', 'rt', 'region', 'sent_pos', 'response', 'participant', 'list', 'sent_num', 'RRC_num \n']))
            for i in range(len(all_words)):
                #print i
                row = '%s,%s,%s,%s,%s,%s,%s, %s, %s, %s, %s, %s \n' %(all_sent_ids[i], all_sentences[i], all_words[i], all_sent_types[i], all_rt[i], all_regions[i], all_sent_pos[i], all_resps[i], participant_ids[i], participant_lists[i], all_sent_nums[i], all_RRC_nums[i])
                f.write(row)
        f.close()

        #print all_demographic_data

                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             

    print len(all_demographic_data)
    with open(demographic_filename, "wb") as h:
        writer = csv.writer(h)
        writer.writerow(['firstlang_eng', 'first_lang', 'other_langs', 'gender', 'age', 'age_acquisition', 'participant', 'proficiency', 'something', 'something2'])
        for item in all_demographic_data:

            #print item.keys()
            if 'firstlang_eng' not in item.keys(): 
                item['firstlang_eng'] = 'NA'
            if 'first_lang' not in item.keys(): 
                item['first_lang'] = 'NA'
            if 'feedback' not in item.keys(): 
                item['feedback'] = 'NA'
            if 'gender' not in item.keys(): 
                item['gender'] = 'NA'
            if 'age' not in item.keys(): 
                item['age'] = 'NA'
            if 'age_acquisition' not in item.keys(): 
                item['age_acquisition'] = 'NA'
            if 'prof' not in item.keys(): 
                item['prof'] = 'NA'

            for key in item.keys():
                if item[key] == '' or item[key] == ' ':
                    item[key] = 'NA'

            writer.writerow(item.values())
    h.close()


    # Loads the data into a list from the datafile
with open(data_file, "r") as file:
    for line in file:
        if not line[0] == '#':
            results = json.loads(line.replace('\n', ''))
            data.append(results)

# Calls the function to generate the csv files
process_all_data(data, spr_filename, demographic_filename)


