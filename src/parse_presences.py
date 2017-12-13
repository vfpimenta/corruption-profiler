import csv
import json
import sqlite3
import util
import unidecode
from datetime import datetime

def replace_template(arg):
  return 'replace(replace(replace(replace(replace(replace(replace(replace(replace(replace(replace({}, "Á","A"), "Ã","A"), "Â","A"), "É","E"), "Ê","E"), "Í","I"),"Ó","O") ,"Õ","O") ,"Ô","O"),"Ú","U"), "Ç","C")'.format(arg)

cache = dict()

def get_gid(congressperson_name, congressperson_document):
  if '{}:{}'.format(congressperson_name, congressperson_document) in cache.keys():
    return cache.get('{}:{}'.format(congressperson_name,congressperson_document))

  con = sqlite3.Connection('../data/local.db')
  cur = con.cursor()

  result = None
  sql = 'SELECT DISTINCT congressperson_id FROM previous_years WHERE congressperson_name LIKE "%{}%" OR congressperson_name LIKE "%{}%" OR (congressperson_name LIKE "%{}%" AND congressperson_document = {})'.format(  parse_name(congressperson_name), 
      decode_name(congressperson_name), 
      surname(congressperson_name), 
      congressperson_document)
  for row in cur.execute(sql):
    result = row[0]

  if not result:
    sql = 'SELECT DISTINCT congressperson_id FROM previous_years WHERE {} LIKE "%{}%" OR {} LIKE "%{}%" OR ({} LIKE "%{}%" AND congressperson_document = {})'.format(
        replace_template('congressperson_name'), 
        parse_name(congressperson_name), 
        replace_template('congressperson_name'), 
        decode_name(congressperson_name), 
        replace_template('congressperson_name'), 
        surname(congressperson_name), 
        congressperson_document)
    for row in cur.execute(sql):
      result = row[0]

  cur.close()
  con.commit()
  con.close()

  if not result:
    raise Exception('Congressman not found for name = {}'.format(congressperson_name))

  cache['{}:{}'.format(congressperson_name,congressperson_document)] = result
  return result

def get_index(str_date):
  date = datetime.strptime(str_date, '%Y-%m-%d %H:%M:%S')
  base_idx = date.year-2011 + date.month-1
  return base_idx-1

def parse_name(str_name):
  return str_name[:str_name.index('-')].upper().replace('JÚNIOR','').replace('`','\'').replace('DANGELO','D\'ANGELO').strip()

def surname(str_name):
  return parse_name(str_name)[parse_name(str_name).rfind(' ')+1:]

def decode_name(str_name):
  return unidecode.unidecode(parse_name(str_name))

congressman_json = dict()
with open('../data/congressperson-presences-54.csv','r') as csvfile:
  size = sum(1 for row in csvfile)
with open('../data/congressperson-presences-54.csv','r') as csvfile:
  reader = csv.reader(csvfile)
  idx = 0
  for row in reader:
    idx += 1
    util.printProgressBar(idx, size, prefix='Parsing csv', suffix='Complete')
    if row[0] != '':
      gid = get_gid(row[3], row[2])
      if gid not in congressman_json.keys():
        congressman_json[gid] = [
          parse_name(row[3]),   # congressperson_name
          row[5],               # state
          row[4],               # party
          [0]*48,               # present_on_day 
          [0]*48                # presence
        ]

      if row[7] == 'Present':
        congressman_json[gid][3][get_index(row[6])] += 1
      if row[10] == 'Present':
        congressman_json[gid][4][get_index(row[6])] += 1

with open('../data/congressman_presences.json','w') as jsonfile:
  json.dump(congressman_json, jsonfile)