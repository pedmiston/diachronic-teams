#!/usr/bin/env python
"""Update the Status column of the Group table of the Totems database."""
import pandas
import fire

import db

PLAYER_INFO = 'player_info.csv'


class Updater:
    def download(self, output=None, sort=False):
        con = db.connect_to_db()
        player_info = pandas.read_sql("""
        SELECT ID_Player, Table_Player.ID_Group as ID_Group, Treatment, Status
        FROM Table_Player
        LEFT JOIN Table_Group
        ON Table_Player.ID_Group = Table_Group.ID_Group
        """, con)
        if sort:
            player_info.sort_values(['Treatment', 'ID_Group', 'Ancestor'],
                                    inplace=True)

        if output is None:
            output = PLAYER_INFO
        player_info.to_csv(output, index=False)

    def update(self, player_info=None):
        if player_info is None:
            player_info = PLAYER_INFO

        player_info = pandas.read_csv(player_info)
        group_ids = (player_info.ix[player_info.Status == 'E', 'ID_Group']
                                .unique()
                                .tolist())
        con = db.connect_to_db()

        # Update the database to reflect the valid groups
        con.execute(db.Group.update().values(Status = None))
        con.execute(
            db.Group.update()
                 .values(Status = 'E')
                 .where(db.Group.c.ID_Group.in_(group_ids))
        )


if __name__ == '__main__':
    fire.Fire(Updater)
