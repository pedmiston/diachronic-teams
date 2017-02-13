from sqlalchemy import (MetaData, Table, Column,
                        Integer, Boolean, String,
                        ForeignKeyConstraint)


metadata = MetaData()

Group = Table('Table_Group', metadata,
    Column('ID_Group', String(60), primary_key=True),
    Column('Size', Integer()),
    Column('Open', Boolean()),
    Column('Treatment', String(10)),
    Column('BuildingTime', Integer()),
    Column('Status', String()),
)

Player = Table('Table_Player', metadata,
    Column('ID_Player', String(60), primary_key=True),
    Column('Sex', String(10)),
    Column('Age', Integer()),
    Column('ID_Number', Integer()),
    Column('Status_Start', Boolean()),
    Column('Status_End', Boolean()),
    Column('ID_Call', Integer()),
    Column('Gain', Integer()),
    Column('Score', Integer()),
    Column('BestTotem', Integer()),
    Column('Knowledge', Integer()),
    Column('Ancestor', Boolean()),
    Column('ID_Group', String(60)),
    ForeignKeyConstraint(['ID_Group'], ['Table_Group.ID_Group'],
                         name='FK_Table_Player_ID_Group'),
)
