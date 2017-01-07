DROP TABLE IF EXISTS Table_Group ;
CREATE TABLE Table_Group (ID_Group VARCHAR(60) NOT NULL,
Size INT,
Open BOOL,
Treatment VARCHAR(10),
BuildingTime INT,
Status CHAR,
PRIMARY KEY (ID_Group) ) ENGINE=InnoDB;

DROP TABLE IF EXISTS Table_Player ;
CREATE TABLE Table_Player (ID_Player VARCHAR(60) NOT NULL,
Sex CHAR,
Age INT,
ID_Number INT,
Status_Start BOOL,
Status_End BOOL,
ID_Call INT,
Gain INT,
Score INT,
BestTotem INT,
Knowledge INT,
Ancestor BOOL,
ID_Group VARCHAR(60),
PRIMARY KEY (ID_Player) ) ENGINE=InnoDB;

DROP TABLE IF EXISTS Table_Totem ;
CREATE TABLE Table_Totem (Key_Totem int AUTO_INCREMENT NOT NULL,
Totem1 INT,
Totem2 INT,
Totem3 INT,
ScoreTotem INT,
TotemTime INT,
ID_Player VARCHAR(60) NOT NULL,
PRIMARY KEY (Key_Totem) ) ENGINE=InnoDB;

DROP TABLE IF EXISTS Table_Workshop ;
CREATE TABLE Table_Workshop (Key_Trial int AUTO_INCREMENT NOT NULL,
WorkShop1 INT,
WorkShop2 INT,
WorkShop3 INT,
WorkShop4 INT,
WorkShopString VARCHAR(15),
WorkShopResult INT,
Success BOOL,
Innov BOOL,
TrialTime INT,
ID_Player VARCHAR(60) NOT NULL,
PRIMARY KEY (Key_Trial) ) ENGINE=InnoDB;

DROP TABLE IF EXISTS Table_PlayerObs ;
CREATE TABLE Table_PlayerObs (ID_Obs VARCHAR(60) NOT NULL,
PlayerObs VARCHAR(60),
RankObs INT,
RankFocal INTEGER,
StartTime INT,
StopTime INT,
ID_Player VARCHAR(60) NOT NULL,
PRIMARY KEY (ID_Obs) ) ENGINE=InnoDB;

DROP TABLE IF EXISTS Table_Drop ;
CREATE TABLE Table_Drop (Key_Drop int AUTO_INCREMENT NOT NULL,
Item INT,
DragStart INT,
DragEnd INT,
DropTime INT,
ID_Player VARCHAR(60) NOT NULL,
PRIMARY KEY (Key_Drop) ) ENGINE=InnoDB;

DROP TABLE IF EXISTS Table_WorkShopObs ;
CREATE TABLE Table_WorkShopObs (Key_Tool int AUTO_INCREMENT NOT NULL,
ID_Obs_WS VARCHAR(60),
ToolObs VARCHAR(30),
ToolTime INT,
ID_Player VARCHAR(60) NOT NULL,
PRIMARY KEY (Key_Tool) ) ENGINE=InnoDB;

ALTER TABLE Table_Player ADD CONSTRAINT FK_Table_Player_ID_Group FOREIGN KEY (ID_Group) REFERENCES Table_Group (ID_Group);

ALTER TABLE Table_Totem ADD CONSTRAINT FK_Table_Totem_ID_Player FOREIGN KEY (ID_Player) REFERENCES Table_Player (ID_Player);
ALTER TABLE Table_Workshop ADD CONSTRAINT FK_Table_Workshop_ID_Player FOREIGN KEY (ID_Player) REFERENCES Table_Player (ID_Player);
ALTER TABLE Table_PlayerObs ADD CONSTRAINT FK_Table_PlayerObs_ID_Player FOREIGN KEY (ID_Player) REFERENCES Table_Player (ID_Player);
ALTER TABLE Table_Drop ADD CONSTRAINT FK_Table_Drop_ID_Player FOREIGN KEY (ID_Player) REFERENCES Table_Player (ID_Player);
ALTER TABLE Table_WorkShopObs ADD CONSTRAINT FK_Table_WorkShopObs_ID_Player FOREIGN KEY (ID_Player) REFERENCES Table_Player (ID_Player);
