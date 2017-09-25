import sqlite3
import csv
import util

def sqlite_transaction(f):
    def wrap(*args):
        con = sqlite3.Connection('../data/local.db')
        cur = con.cursor()

        ret = f(cur, *args)

        cur.close()
        con.commit()
        con.close()
        return ret
    return wrap

class SQLiteHandler:

    @sqlite_transaction
    def fetch_expenses(cur, self, field, value, trim):
        tuples = []

        sql = 'SELECT c.year, c.month, COALESCE(py.net,0) FROM calendar c LEFT JOIN (SELECT year,month,SUM(net_value) as net FROM previous_years WHERE {} = ? GROUP BY year, month) py ON py.year = c.year AND py.month = c.month'.format(field)
        if trim:
            # jul 2009 ~ ago 2016
            sql += ' WHERE (c.year = ? AND c.month >= ?) OR (c.year = ? AND c.month <= ?) OR (c.year >= ? AND c.year <= ?)'
            for val in cur.execute(sql,[value,2009,7,2016,8,2010,2015]):
                tuples.append(val)
        else:
            for val in cur.execute(sql,[value]):
                tuples.append(val)

        values = [i[2] for i in tuples]
        return values

    @sqlite_transaction
    def fetch_sum(self, trim):
        tuples = []

        sql = 'SELECT c.year, c.month, COALESCE(py.net,0) FROM calendar c LEFT JOIN (SELECT year,month,SUM(net_value) as net FROM previous_years GROUP BY year, month) py ON py.year = c.year AND py.month = c.month'
        if trim:
            # jul 2009 ~ ago 2016
            sql += ' WHERE (c.year = ? AND c.month >= ?) OR (c.year = ? AND c.month <= ?) OR (c.year >= ? AND c.year <= ?)'
            for val in self.cursor.execute(sql,[2009,7,2016,8,2010,2015]):
                tuples.append(val)
        else:
            for val in self.cursor.execute(sql):
                tuples.append(val)

        values = [i[2] for i in tuples]
        return values

    @sqlite_transaction
    def fetch_congressman_info(cur, self, _id):
        for row in cur.execute('SELECT congressperson_name, state FROM previous_years WHERE congressperson_id = ? GROUP BY congressperson_id', [_id]):
            info = row

        return info

    @sqlite_transaction
    def __create_calendar__(cur, self):
        cur.execute('CREATE TABLE "calendar" ("year" INTEGER, "month" INTEGER);')
        data = []

        for year in range(2009,2017):
            for month in range(1,13):
                data.append((year,month))

        cur.executemany('INSERT INTO "calendar" VALUES (?,?)', data)

    @sqlite_transaction
    def __create_database__(cur, self):
        cur.execute('CREATE TABLE "previous_years" ("document_id" LONG, "congressperson_name" VARCHAR(255), "congressperson_id" VARCHAR(255), "congressperson_document" VARCHAR(255), "term" VARCHAR(255), "state" VARCHAR(255), "party" VARCHAR(255), "term_id" VARCHAR(255), "subquota_number" VARCHAR(255), "subquota_description" VARCHAR(255), "subquota_group_id" VARCHAR(255), "subquota_group_description" VARCHAR(255), "supplier" VARCHAR(255), "cnpj_cpf" VARCHAR(255), "document_number" VARCHAR(255), "document_type" VARCHAR(255), "issue_date" VARCHAR(255), "document_value" DOUBLE, "remark_value" DOUBLE, "net_value" DOUBLE, "month" INTEGER, "year" INTEGER, "installment" VARCHAR(255), "passenger" VARCHAR(255), "leg_of_the_trip" VARCHAR(255), "batch_number" VARCHAR(255), "reimbursement_number" VARCHAR(255), "reimbursement_value" VARCHAR(255), "applicant_id" VARCHAR(255));')

        _insert_from('../data/2016-11-19-previous-years.csv',cur)
        _insert_from('../data/2016-11-19-last-year.csv',cur)
        _insert_from('../data/2016-11-19-current-year.csv',cur)

        cur.close()
        con.commit()
        con.close()

    def _insert_from(uri, cur):
        f = open(uri)
        csv_reader = csv.reader(f, delimiter=',')
        
        cur.executemany('INSERT INTO "previous_years" VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)', csv_reader)
        f.close()

class CSVHandler:

    def __init__(self):
        self.filepath = '../data/congressperson-ids.csv'

    def get_congressman_info(self):
        congressman_info = {}

        with open(self.filepath,'r') as csvfile:
            fieldnames = ['Id','Name']
            reader = csv.DictReader(csvfile)
            for row in reader:
                if row[fieldnames[0]] != '':
                    congressman_info[row[fieldnames[0]]] = row[fieldnames[1]]

        return congressman_info