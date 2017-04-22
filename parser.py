
import time
import csv
import simplejson
from match import Match

headers = ["id", "team_a", "team_b", "series", "venue_country", "date", "day_night", "toss_winner", "toss_decision", "first_innings_total", "rain", "duckworth_lewis", "outcome", "winner"]

matches = simplejson.loads(open('1-data-sources/odi.json').read())
matches = list(set(matches)) # dedupe

##################################START PROCESSING DATA#########################################
with open("2-target-data/cricinfo-odi-data.csv", "wb") as csvfile:
    writer = csv.writer(csvfile)
    writer.writerow(headers)
    for match in matches:
        time.sleep(1)
        try:
            m = Match(int(match))
            if m.match_json()['match_status'] == 'forthcoming':
                continue
            if m.result() == '':
                continue
            if m.rain_rule() == 'D/L method':
                duckworth_lewis = 1
            else:
                duckworth_lewis = 0
            if m.lighting() == 'day match':
                day_night  = 0
            else:
                day_night = 1
            try:
                m.team_2()['team_name']
            except KeyError:
                continue
            try:
                m.team_1()['team_name']
            except KeyError:
                continue

            writer.writerow([match,
                             m.team_1()['team_name'],
                             m.team_2()['team_name'],
                             m.series(),
                             m.venue_country(),
                             m.date(),
                             day_night,
                             m.toss_winner_team(),
                             m.toss_decision(),
                             m.first_innings_total(),
                             m.rain_rule(),
                             duckworth_lewis,
                             m.result(),
                             m.match_winner()
                             ])

        except simplejson.scanner.JSONDecodeError:
            continue

##################################FINISHED#########################################
print "DONE." #"Wrote output to %s" %(FINAL_OUTPUT_FILE)
