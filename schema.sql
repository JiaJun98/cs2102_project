drop table if exists Employees, Health_declare, Contacts, Departments, Meeting_room, Sessions, Updates, Participants,
    Juniors, seniors, bookers, managers cascade;

CREATE TABLE Departments (
    did int,
    dname text NOT NULL,
    PRIMARY KEY (did)
);

CREATE TABLE Employees (
    eid int,
    ename text NOT NULL,
    email text UNIQUE,
    resigned_date date DEFAULT NULL,
    em_did int NOT NULL,
    PRIMARY KEY (eid),
    FOREIGN KEY (em_did) REFERENCES Departments (did)
);

CREATE TABLE Juniors (
    eid int,
    PRIMARY KEY (eid),
    FOREIGN KEY (eid) REFERENCES Employees (eid)
 );

 CREATE TABLE Bookers (
     eid int,
     PRIMARY KEY (eid),
     FOREIGN KEY (eid) REFERENCES Employees (eid)
 );

 CREATE TABLE Seniors (
     eid int,
     PRIMARY KEY (eid),
     FOREIGN KEY (eid) REFERENCES Employees (eid)
 );

  CREATE TABLE Managers (
     eid int,
     PRIMARY KEY (eid),
     FOREIGN KEY (eid) REFERENCES Employees (eid)
 );

CREATE TABLE Contacts (
    eid int NOT NULL,
    phone int UNIQUE NOT NULL,
    FOREIGN KEY (eid) REFERENCES  Employees (eid)
);

CREATE TABLE Health_declare (
    eid int,
    temp numeric NOT NULL,
    fever boolean,
    date date,
    PRIMARY KEY (eid, date),
    FOREIGN KEY (eid) REFERENCES Employees (eid) ON DELETE CASCADE,
    CONSTRAINT temp_range CHECK (temp >= 34 AND temp <= 43)
);

CREATE TABLE Meeting_room (
    floor int,
    room int,
    rname text NOT NULL,
    cap int NOT NULL,
    mr_did int NOT NULL,
    PRIMARY KEY(floor, room),
    FOREIGN KEY (mr_did) REFERENCES Departments (did)
);

CREATE TABLE Sessions (
    floor int,
    room int,
    date date,
    time time,
    app_eid int,
    book_eid int NOT NULL,
    PRIMARY KEY (floor, room, date, time),
    FOREIGN KEY (floor, room) REFERENCES Meeting_room (floor, room) ON DELETE CASCADE,
    FOREIGN KEY (app_eid) REFERENCES Managers (eid),
    FOREIGN KEY (book_eid) REFERENCES Bookers (eid)
);

CREATE TABLE Participants (
    floor int NOT NULL,
    room int NOT NULL,
    date date NOT NULL,
    time time NOT NULL,
    part_eid int NOT NULL,
    UNIQUE(date,time,part_eid),
    FOREIGN KEY (floor, room, date, time) REFERENCES Sessions (floor, room, date, time) ON DELETE CASCADE,
    FOREIGN KEY (part_eid) REFERENCES  Employees (eid)
);

CREATE TABLE Updates (
    floor int,
    room int,
    new_cap int NOT NULL,
    date date,
    eid int NOT NULL,
    PRIMARY KEY (floor, room, date),
    FOREIGN KEY (floor, room) REFERENCES Meeting_room (floor, room),
    FOREIGN KEY (eid) REFERENCES Managers (eid)
);