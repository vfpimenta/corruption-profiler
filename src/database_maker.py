# linux delete first line: sed -i '1d' 2016-11-19-current-year.csv # GNU sed only, creates a temporary file

import csv
import sqlite3

def insert_from(uri, cur):
	f = open(uri)
	csv_reader = csv.reader(f, delimiter=',')
	
	cur.executemany('INSERT INTO "previous_years" VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)', csv_reader)
	f.close()

con = sqlite3.Connection('local.db')
cur = con.cursor()
cur.execute('CREATE TABLE "previous_years" ("document_id" LONG, "congressperson_name" VARCHAR(255), "congressperson_id" VARCHAR(255), "congressperson_document" VARCHAR(255), "term" VARCHAR(255), "state" VARCHAR(255), "party" VARCHAR(255), "term_id" VARCHAR(255), "subquota_number" VARCHAR(255), "subquota_description" VARCHAR(255), "subquota_group_id" VARCHAR(255), "subquota_group_description" VARCHAR(255), "supplier" VARCHAR(255), "cnpj_cpf" VARCHAR(255), "document_number" VARCHAR(255), "document_type" VARCHAR(255), "issue_date" VARCHAR(255), "document_value" DOUBLE, "remark_value" DOUBLE, "net_value" DOUBLE, "month" INTEGER, "year" INTEGER, "installment" VARCHAR(255), "passenger" VARCHAR(255), "leg_of_the_trip" VARCHAR(255), "batch_number" VARCHAR(255), "reimbursement_number" VARCHAR(255), "reimbursement_value" VARCHAR(255), "applicant_id" VARCHAR(255));')

insert_from('../data/2016-11-19-previous-years.csv',cur)
insert_from('../data/2016-11-19-last-year.csv',cur)
insert_from('../data/2016-11-19-current-year.csv',cur)

cur.close()
con.commit()
con.close()