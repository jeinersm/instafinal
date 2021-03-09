# -*- coding: utf-8 -*-
"""
Created on Thu May 31 10:14:46 2018

@author: ohansen
"""

import pandas as pd
import glob
import os.path
import emoji
from textblob import TextBlob

def readit(file_):
    try:
        df = pd.read_csv(file_,index_col=None, header=0)
        return df
    except pd.io.common.ParserError:
        print('parse error: ' + file_)
    except pd.io.common.EmptyDataError:
        print('empty: ' + file_)


###Tweets
path = r'scrape_out/'
all_files = glob.glob(os.path.join(path, "*.csv"))
generator = (readit(file_) for file_ in all_files)        
frame = pd.concat(generator)
frame = frame.reset_index(drop=True)

#Change to CST so dates roughly reflect USA audience times
frame['postdate'] = pd.to_datetime(frame.postdate).dt.tz_localize('UTC').dt.tz_convert('America/Chicago')
frame['scrapedate'] = pd.to_datetime(frame.scrapedate).dt.tz_localize('UTC').dt.tz_convert('America/Chicago')
frame['scrapedate'] = frame.scrapedate.dt.round('1s')
frame['days'] = frame.scrapedate.dt.date-frame.postdate.dt.date
frame['days'] = frame.days.dt.days

frame['isMulti'] = frame.isMulti.fillna(value=False)
frame['desc'] = frame.desc.fillna(value='')
frame['desc'] = frame.desc.astype(str)
frame['followers'] = pd.to_numeric(frame.followers.str.replace(',',''))
frame['likes'] = pd.to_numeric(frame.likes.str.replace(',',''))
frame['follows'] = pd.to_numeric(frame.follows.str.replace(',',''))
frame['post_ct'] = pd.to_numeric(frame.post_ct.str.replace(',',''))

#clean up alt text
frame['alt'] = frame.alt.str.replace('Image may contain: ','')
frame['alt'] = frame.alt.str.replace('No photo description available.','')
frame['alt'] = frame.alt.str.replace(' and ',', ')

#hashtags
frame['hashtags'] = frame.desc.str.findall(r'#.*?(?=\s|$)')
frame['hash_ct'] = frame.hashtags.str.len()
#separated list for R
frame['hashtags'] = frame.hashtags.str.join(' ')

#@mentions
frame['mentions'] = frame.desc.str.findall(r'@.*?(?=\s|$)')
frame['mention_ct'] = frame.mentions.str.len()
#separated list for R
frame['mentions'] = frame.mentions.str.join(' ').str.replace(')','').str.replace(',',' ')


##de-emojify
def extract_emojis(str):
    return ''.join(c for c in str if c in emoji.UNICODE_EMOJI)

frame['emoji'] = frame.desc.apply(extract_emojis)
frame['emoji_ct'] = frame.emoji.str.len()
frame['emoji_text'] = frame.emoji.apply(emoji.demojize).str.replace('::',': :')

##create clean text (no emoji/mentions/hashtags)
frame['desc_clean'] = frame.desc.str.replace(r'#.*?(?=\s|$)','',regex=True)
frame['desc_clean'] = frame.desc_clean.str.replace(r'@.*?(?=\s|$)','',regex=True)
frame['desc_clean'] = frame.desc_clean.str.replace(emoji.get_emoji_regexp(),'',regex=True)

#drop video posts
frame = frame[frame.isVideo != True]

users = frame.drop_duplicates(subset=('user'))
users = users[users.followers <=7500000]
users = users[users.followers >250000]
users.followers.hist()

#limit followers
frame = frame[frame.followers > 250000]
frame = frame[frame.followers <= 7500000]

#require at least 50 posts
frame['count'] = frame.groupby('user')['user'].transform('count')
frame = frame[frame['count'] >= 50]

frame = frame.drop(columns=['isVideo','textResult','follows','count'])


frame.to_csv('all_posts.csv', index=False)
frame.reset_index(drop=True).to_feather('all_posts.feather')


