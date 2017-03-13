#!/usr/bin/env python
import pandas
import db

con = db.connect_to_db()
player_info = pandas.read_sql("""
SELECT ID_Player, Table_Player.ID_Group as ID_Group, Treatment
FROM Table_Player
LEFT JOIN Table_Group
ON Table_Player.ID_Group = Table_Group.ID_Group
""", con)
player_info['ID_Player'] = player_info.ID_Player.astype(int)
diachronic_teams = player_info.ix[player_info.Treatment == 'Diachronic']

# Need to see if any people were in the same diachronic group
# at the same time.

# Get timestamp information from survey
survey = pandas.read_csv('totems/data-raw/totems/Survey.csv')
survey = survey.rename(columns={'Participant ID': 'ID_Player',
                                'Timestamp': 'SurveySubmit'})
survey = survey[['ID_Player', 'SurveySubmit']]

diachronic_times = (diachronic_teams.merge(survey, how='left')
                                    .sort_values(['ID_Group', 'ID_Player']))

# ID_Group times have AM/PM
group_id_time_format = 'G_%m/%d/%Y %I:%M:%S %p'
diachronic_times['GroupTime'] = pandas.to_datetime(diachronic_times.ID_Group,
                                                   format=group_id_time_format)
diachronic_times['PlayerEndTime'] = \
    pandas.to_datetime(diachronic_times.SurveySubmit)

diachronic_times = diachronic_times[['ID_Player', 'GroupTime', 'PlayerEndTime']]
diachronic_times.to_csv('diachronic_times.csv', index=False)
