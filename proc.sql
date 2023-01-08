/** APPLICATION FUNCTIONALITIES **/
DROP PROCEDURE IF EXISTS change_capacity, add_department, remove_department, add_room, add_employee, add_contact, remove_employee,
    book_room, unbook_room, join_meeting, leave_meeting, approve_meeting, declare_health;
DROP FUNCTION IF EXISTS has_fever, check_capacity, search_room, contact_tracing, non_compliance,
    view_booking_report, view_future_meeting, view_manager_report;
    

/** HELPER FUNCTIONS **/
CREATE OR REPLACE FUNCTION has_fever
    (hf_temp numeric)
    RETURNS BOOLEAN AS $$
        SELECT CASE
            WHEN hf_temp > 37.5 THEN TRUE
            ELSE FALSE
        END;
    $$
    LANGUAGE sql;
    

CREATE OR REPLACE FUNCTION check_capacity
    (IN checkcap_floor INT, IN checkcap_room INT, IN checkcap_date Date, OUT current_capacity INT)
    RETURNS INT AS $$
    BEGIN
        SELECT new_cap INTO current_capacity
        FROM Updates U
        WHERE U.date >= ALL(SELECT date
            FROM UPDATES U2
            WHERE U2.floor = checkcap_floor
            AND U2.room = checkcap_room
            AND U2.date <= checkcap_date)
        AND U.floor = checkcap_floor
        AND U.room = checkcap_room
        AND U.date <= checkcap_date;
        IF (current_capacity IS NULL) THEN
            SELECT cap INTO current_capacity
            FROM Meeting_room M
            WHERE M.floor = checkcap_floor
            AND M.room = checkcap_room;
        END IF;
    END;
    $$ LANGUAGE plpgsql;


/** TRIGGER FUNCTIONS **/
/** ISA Junior **/
CREATE OR REPLACE FUNCTION check_junior()
RETURNS TRIGGER AS $$
BEGIN
	IF (NEW.eid IN (SELECT M.eid FROM Managers M)) OR (NEW.eid IN ((SELECT S.eid FROM Seniors S))) THEN
	RAISE NOTICE 'Employee % is not a junior.', NEW.eid;
		RETURN NULL;
	ELSE
		RETURN NEW;
	END IF;

END;
$$ LANGUAGE plpgsql;

DROP TRIGGER  IF EXISTS junior_check ON Juniors;
CREATE TRIGGER junior_check
BEFORE INSERT OR UPDATE ON Juniors
FOR EACH ROW EXECUTE FUNCTION check_junior();


/** ISA Booker **/
CREATE OR REPLACE FUNCTION check_booker()
RETURNS TRIGGER AS $$
BEGIN
	IF (NEW.eid IN (SELECT J.eid FROM Juniors J ))  THEN
	RAISE NOTICE 'Employee % is not a Booker.', NEW.eid;
		RETURN NULL;
	ELSE
		RETURN NEW;
	END IF;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS booker_check ON Bookers;
CREATE TRIGGER booker_check
BEFORE INSERT OR UPDATE ON Bookers
FOR EACH ROW EXECUTE FUNCTION check_booker();


/** ISA Manager **/
CREATE OR REPLACE FUNCTION check_manager()
RETURNS TRIGGER AS $$
BEGIN
	IF (NEW.eid IN (SELECT S.eid FROM Seniors S )) OR (NEW.eid IN (SELECT J.eid FROM Juniors J)) THEN
	RAISE NOTICE 'Employee % is not a manager.', NEW.eid;
		RETURN NULL;
	ELSE
	    INSERT INTO Bookers VALUES (NEW.eid);
		RETURN NEW;
	END IF;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS manager_check ON Managers;
CREATE TRIGGER manager_check
BEFORE INSERT OR UPDATE ON Managers
FOR EACH ROW EXECUTE FUNCTION check_manager();


/** ISA Senior **/
CREATE OR REPLACE FUNCTION check_senior()
RETURNS TRIGGER AS $$
BEGIN
	IF (NEW.eid IN (SELECT M.eid FROM Managers M )) OR (NEW.eid IN (SELECT J.eid FROM Juniors J)) THEN
	RAISE NOTICE 'Employee % is not a senior.', NEW.eid;
		RETURN NULL;
	ELSE
	    INSERT INTO Bookers VALUES (NEW.eid);
		RETURN NEW;
	END IF;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS senior_check ON Seniors;
CREATE TRIGGER senior_check
BEFORE INSERT OR UPDATE ON Seniors
FOR EACH ROW EXECUTE FUNCTION check_senior();


/** No Concurrent Meeting Joining Trigger **/
CREATE OR REPLACE FUNCTION check_concurrent_meeting()
RETURNS TRIGGER AS $$
BEGIN
  IF EXISTS (SELECT 1
  FROM participants p
  WHERE p.date = NEW.date AND p.time = NEW.time AND p.part_eid = NEW.part_eid) THEN
      RAISE EXCEPTION 'Employee % cannot join meeting due to concurrent meeting.', NEW.part_eid;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER  IF EXISTS concurrent_meeting_check ON participants;
CREATE TRIGGER concurrent_meeting_check
BEFORE INSERT OR UPDATE ON participants
FOR EACH ROW EXECUTE FUNCTION check_concurrent_meeting();


/** No Concurrent Meeting Booker Trigger **/
CREATE OR REPLACE FUNCTION check_concurrent_meeting_booker()
RETURNS TRIGGER AS $$
BEGIN
	IF EXISTS (SELECT 1
	FROM sessions s
	WHERE s.date = NEW.date AND s.time = NEW.time AND s.book_eid = NEW.book_eid) THEN
	    RAISE EXCEPTION 'Employee % cannot book meeting due to concurrent meeting.', NEW.book_eid;
	END IF;
	RETURN NEW;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS concurrent_meeting_booker_check ON sessions;
CREATE TRIGGER concurrent_meeting_booker_check
BEFORE INSERT ON sessions
FOR EACH ROW EXECUTE FUNCTION check_concurrent_meeting_booker();


/** Contact tracing trigger **/
CREATE OR REPLACE FUNCTION contact_tracing_check()
RETURNS TRIGGER AS $$
BEGIN
  PERFORM contact_tracing(NEW.eid, NEW.date);
  IF NEW.fever = TRUE THEN
  RAISE NOTICE 'Employee % has fever, contact tracing activated.',  NEW.eid;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER  IF EXISTS contact_tracing_check ON health_declare;
CREATE TRIGGER contact_tracing_check
AFTER INSERT ON health_declare
FOR EACH ROW EXECUTE FUNCTION contact_tracing_check();


/** Maximum capacity of current meeting trigger **/
CREATE OR REPLACE FUNCTION current_meetings_capacity_check()
RETURNS TRIGGER AS $$
BEGIN
    IF (EXISTS (SELECT 1
        FROM participants p
        WHERE p.date >= NEW.date
        AND p.floor = NEW.floor
        AND p.room = NEW.room
        GROUP BY (p.date, p.time)
        HAVING COUNT(p.part_eid) > NEW.new_cap))
    THEN
    DELETE FROM sessions s WHERE
    s.date IN (SELECT p.date
        FROM participants p
        WHERE p.date >= NEW.date
        AND p.floor = NEW.floor
        AND p.room = NEW.room
        GROUP BY (p.date, p.time)
        HAVING COUNT(p.part_eid) > NEW.new_cap) AND s.room = NEW.room
        AND
        s.floor = NEW.floor;
        RAISE NOTICE 'Meeting on % for room % on floor % booked exceed this new capacity and has been removed.', NEW.date, NEW.room, NEW.floor;
        RETURN NEW;
        ELSE
	    RETURN NEW;
        END IF;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER  IF EXISTS check_current_meeting_capacity ON Updates;
CREATE TRIGGER check_current_meeting_capacity
AFTER INSERT ON Updates
FOR EACH ROW EXECUTE FUNCTION current_meetings_capacity_check();


/** BASIC FUNCTIONS **/
CREATE OR REPLACE PROCEDURE add_department
    (ad_did int, ad_dname text) AS $$
    BEGIN
        INSERT INTO Departments VALUES (ad_did, ad_dname);
    END;
    $$
    LANGUAGE plpgsql;


CREATE OR REPLACE PROCEDURE remove_department
    (rd_did int) AS $$
    BEGIN
        IF NOT EXISTS (SELECT 1 FROM Departments where did = rd_did) THEN
           RAISE EXCEPTION 'Department % does not exist.', rd_did;
        END IF;
        DELETE FROM Departments d WHERE did = rd_did;
    END;
    $$
    LANGUAGE plpgsql;


CREATE OR REPLACE PROCEDURE add_room
    (ar_floor int, ar_room int, ar_rname text, ar_cap int, ar_did int) AS $$
    BEGIN
        INSERT INTO meeting_room VALUES (ar_floor, ar_room, ar_rname, ar_cap, ar_did);
    END;
    $$
    LANGUAGE plpgsql;


CREATE OR REPLACE PROCEDURE change_capacity
    (ccap_floor int, ccap_room int, new_cap int, ccap_date date, ccap_eid int) AS $$
    BEGIN
        IF (ccap_eid IN (SELECT m.eid FROM managers m)) THEN
            INSERT INTO updates VALUES (ccap_floor, ccap_room, new_cap, ccap_date, ccap_eid);
        END IF;
    END
    $$
    LANGUAGE plpgsql;


CREATE OR REPLACE PROCEDURE add_employee
    (ae_ename text, ae_phone_num int, ae_kind text, ae_em_did int) AS $$
    DECLARE
        ae_eid INT :=  (SELECT COUNT(*) FROM Employees em) + 1;
        ae_email text := ae_ename || ae_eid || '@office.com';
    BEGIN
        INSERT INTO employees VALUES (ae_eid, ae_ename, ae_email, NULL, ae_em_did);
        INSERT INTO contacts VALUES (ae_eid, ae_phone_num);
        IF (ae_kind = 'Junior') THEN
            INSERT INTO Juniors VALUES (ae_eid);
        ELSIF (ae_kind = 'Senior') THEN
            INSERT INTO Seniors VALUES (ae_eid);
        ELSIF (ae_kind = 'Manager') THEN
            INSERT INTO Managers VALUES (ae_eid);
        END IF;
    END
    $$
    LANGUAGE plpgsql;


CREATE OR REPLACE PROCEDURE add_contact
    (ac_eid int, ac_phone_num int) AS $$
    BEGIN
        INSERT INTO contacts VALUES (ac_eid, ac_phone_num);
    END
    $$
    LANGUAGE plpgsql;


CREATE OR REPLACE PROCEDURE remove_employee
    (re_eid int, last_day date) AS $$
    BEGIN
        UPDATE employees
        SET resigned_date = last_day
        WHERE eid = re_eid;
        DELETE FROM Sessions S WHERE S.date >= last_day AND S.book_eid = re_eid;
        DELETE FROM participants P WHERE P.date >= last_day AND P.part_eid = re_eid;
    END;
    $$
    LANGUAGE plpgsql;


/** CORE FUNCTIONS **/
DROP FUNCTION IF EXISTS search_room;
CREATE OR REPLACE FUNCTION search_room
    (sr_cap INT, sr_date Date, sr_start_hour time , sr_end_hour time)
    RETURNS TABLE(sr_floor INT, sr_room INT, sr_did INT, sr_room_cap INT)  AS $$
    BEGIN
        RETURN QUERY (
            SELECT M1.floor, M1.room, M1.mr_did, check_capacity(M1.floor, M1.room, sr_date) AS cap
            FROM Meeting_room M1
            WHERE check_capacity(M1.floor, M1.room, sr_date) >= sr_cap

            EXCEPT

            SELECT M2.floor, M2.room, M2.mr_did, M2.cap
            FROM Meeting_room M2 INNER JOIN Sessions S ON M2.floor = S.floor AND M2.room = S.room
            WHERE S.time >= sr_start_hour
              AND S.time < sr_end_hour
              AND S.date = sr_date
        );
    END;
    $$ LANGUAGE plpgsql;


CREATE OR REPLACE PROCEDURE book_room
    (br_floor INT, br_room INT, br_date Date, br_start_hour time, br_end_hour time, br_eid INT, br_cap INT)
	AS $$
    DECLARE
        start_time time := br_start_hour;
    BEGIN
        IF (SELECT TRUE FROM employees E WHERE E.eid = br_eid AND E.resigned_date <= br_date) THEN
            RAISE EXCEPTION 'Employee % has resigned', br_eid;
        ELSIF (br_eid NOT IN (SELECT eid FROM Bookers)) THEN
            RAISE EXCEPTION 'Employee % is not a senior or manager and thus, cannot book rooms.', br_eid;
        ELSIF EXISTS (SELECT 1
            FROM Health_declare HD
            WHERE HD.eid = br_eid
            AND HD.date = br_date
            AND HD.fever = TRUE) THEN
            RAISE EXCEPTION 'Employee % has a fever and thus, cannot book rooms.', br_eid;
        ELSIF (SELECT COUNT(*)
                FROM search_room(br_cap, br_date, br_start_hour, br_end_hour)
                WHERE sr_floor = br_floor AND sr_room = br_room) = 0 THEN
                    RAISE EXCEPTION 'There are no available rooms that fit the request.';
        END IF;

        IF EXISTS (SELECT * FROM search_room(br_cap, br_date, br_start_hour, br_end_hour) SR
            WHERE SR.sr_floor = br_floor AND SR.sr_room = br_room)
        THEN
        WHILE start_time <> br_end_hour LOOP
            INSERT INTO Sessions VALUES (br_floor, br_room, br_date, start_time, NULL, br_eid);
            INSERT INTO Participants VALUES (br_floor, br_room, br_date, start_time, br_eid);
            start_time :=start_time + interval '1 hours';
        END LOOP;
        END IF;
    END;
    $$ LANGUAGE plpgsql;


CREATE OR REPLACE PROCEDURE unbook_room
    (ur_floor INT, ur_room INT, ur_date Date, ur_start_hour time, ur_end_hour time, ur_eid INT)
	AS $$
    DECLARE
        start_time time := ur_start_hour;
    BEGIN
    WHILE start_time <> ur_end_hour LOOP /* If Start_hour/End_hour is not the same as Book room  */
        IF EXISTS (SELECT 1
                    FROM Sessions S
                    WHERE S.floor = ur_floor
                    AND  S.room = ur_room
                    AND S.date = ur_date
                    AND S.book_eid = ur_eid
                    AND S.time = start_time) THEN
            DELETE FROM Sessions S2 WHERE S2.floor = ur_floor AND S2.room = ur_room AND S2.date = ur_date AND S2.book_eid = ur_eid AND S2.time = start_time;
        ELSE
            RAISE NOTICE 'Employee % did not book this room from %.', ur_eid, start_time;
        END IF;
        start_time :=start_time + interval '1 hours';
    END LOOP;
    END;
    $$ LANGUAGE plpgsql;


CREATE OR REPLACE PROCEDURE join_meeting
    (jm_floor INT, jm_room INT, jm_date Date, jm_start_hour time, jm_end_hour time, jm_eid INT)
    AS $$
    DECLARE
        start_time time := jm_start_hour;
    BEGIN
        IF (SELECT TRUE FROM employees E WHERE E.eid = jm_eid AND E.resigned_date <= jm_date) THEN
            RAISE EXCEPTION 'Employee % has resigned.', jm_eid;
        ELSIF EXISTS (SELECT 1
                        FROM Health_declare HD
                        WHERE HD.eid = jm_eid
                        AND HD.date = jm_date
                        AND HD.fever = TRUE) THEN
            RAISE EXCEPTION 'Employee % has fever.',  jm_eid;
        ELSIF NOT EXISTS (SELECT 1
                            FROM Meeting_room mr
                            WHERE mr.floor = jm_floor
                            AND mr.room = jm_room) THEN
            RAISE EXCEPTION 'This room does not exist.';
        END IF;

        WHILE start_time <> jm_end_hour LOOP /* If Start_hour/End_hour is no the same as Book room  */
            IF ((SELECT app_eid
                FROM Sessions S
                WHERE S.floor = jm_floor
                AND S.room = jm_room
                AND S.time = start_time
                AND S.date = jm_date) IS NULL) THEN
                    IF (SELECT COUNT(*)
                        FROM Participants P
                        WHERE P.floor = jm_floor
                        AND P.room = jm_room
                        AND P.time = start_time
                        AND P.date = jm_date) < check_capacity(jm_floor,jm_room,jm_date) THEN
                            INSERT INTO Participants VALUES (jm_floor, jm_room, jm_date, start_time, jm_eid);
                    ELSE
                        RAISE NOTICE 'Employee % cannot join meeting at % as capacity has been reached.', jm_eid, start_time;
                    END IF;
            ELSE
                RAISE NOTICE 'Employee % cannot join meeting at % as this session has already been approved at this timeslot.', jm_eid, start_time;
            END IF;

            start_time := start_time + interval '1 hours';
        END LOOP;
    END;
    $$ LANGUAGE plpgsql;


CREATE OR REPLACE PROCEDURE leave_meeting
    (lm_floor int, lm_room int, lm_date date, lm_start_hour time, lm_end_hour time, lm_eid int)
    AS $$
    DECLARE
        start_time time := lm_start_hour;
    BEGIN
        WHILE start_time <> lm_end_hour LOOP
            IF (lm_eid IN (SELECT p1.part_eid FROM participants p1 WHERE lm_floor = p1.floor AND lm_room = p1.room AND lm_date = p1.date AND start_time = p1.time))
            AND ((SELECT s.app_eid FROM sessions s WHERE lm_floor = s.floor AND lm_room = s.room AND lm_date = s.date AND start_time = s.time) IS NULL) THEN
                DELETE FROM participants p2 WHERE p2.part_eid = lm_eid AND p2.time = start_time AND p2.date = lm_date;
                IF EXISTS (SELECT 1 FROM sessions S
                            WHERE lm_eid = s.book_eid AND lm_floor = s.floor
                            AND lm_room = s.room AND lm_date = s.date AND start_time = s.time) THEN
                                DELETE FROM sessions s1
                                WHERE lm_eid = s1.book_eid
                                AND lm_floor = s1.floor AND lm_room = s1.room
                                AND lm_date = s1.date AND start_time = s1.time;
                                RAISE NOTICE 'As Employee % is the booker of this session at %, the session will be removed.', lm_eid, start_time;
                END IF;
            ELSE
                RAISE NOTICE 'Employee % cannot leave meeting at % as he/she is not a participant or this session timeslot has already been approved.', lm_eid, start_time;
            END IF;
            start_time := start_time + interval '1 hours';
        END LOOP;
    END;
    $$
    LANGUAGE plpgsql;


CREATE OR REPLACE PROCEDURE approve_meeting
    (am_floor int, am_room int, am_date date, am_start_hour time, am_end_hour time, am_eid int)
    AS $$
    DECLARE
        start_time time := am_start_hour;
    BEGIN
        IF (SELECT TRUE FROM employees E WHERE E.eid = am_eid AND E.resigned_date <= am_date) THEN
        RAISE EXCEPTION 'Employee % has resigned.', am_eid;
        END IF;
        IF (am_eid NOT IN (SELECT m.eid FROM managers m)) THEN
            RAISE EXCEPTION 'Employee % is not a manager.',  am_eid;
        END IF;
        IF ((SELECT E.em_did FROM employees E WHERE e.eid = am_eid) <> (SELECT MR.mr_did FROM meeting_room MR WHERE MR.floor = am_floor AND MR.room = am_room)) THEN
            RAISE EXCEPTION 'The meeting room does not belong to the same department as Employee %.', am_eid;
        END IF;
        WHILE start_time <> am_end_hour LOOP
            IF ( NOT EXISTS (SELECT 1 FROM sessions s1 WHERE am_floor = s1.floor AND am_room = s1.room AND am_date = s1.date AND start_time = s1.time)) THEN
                RAISE EXCEPTION 'The session does not exist.';
            END IF;

            IF (SELECT TRUE FROM sessions s2 WHERE am_floor = s2.floor AND am_room = s2.room AND am_date = s2.date AND start_time = s2.time AND S2.app_eid IS NOT NULL) THEN
                RAISE EXCEPTION 'The session has already been approved.';
            END IF;
            UPDATE sessions s3 SET app_eid = am_eid WHERE am_floor = s3.floor AND am_room = s3.room AND am_date = s3.date AND start_time = s3.time;
            start_time := start_time + interval '1 hours';
        END LOOP;
    END;
    $$
    LANGUAGE plpgsql;


/** HEALTH FUNCTIONS **/
CREATE OR REPLACE PROCEDURE declare_health
    (dh_eid int, dh_date date, dh_temp numeric) AS $$
    BEGIN
    INSERT INTO health_declare VALUES (dh_eid, dh_temp, has_fever(dh_temp),dh_date);
    END;
    $$
    LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION contact_tracing
    (ct_eid int, reported_date date default current_date)
    RETURNS TABLE(employee_id int) AS $$
    BEGIN
        IF NOT (SELECT hd.fever FROM health_declare hd WHERE hd.eid = ct_eid AND hd.date = reported_date) THEN
            RAISE NOTICE 'Employee % does not have a fever',  ct_eid;
        END IF;
        Drop TABLE if exists close_contacts_table;
        CREATE TABLE close_contacts_table (
            eid int
        );
        WITH close_contact_rooms AS (
            SELECT p.floor, p.room, p.date, p.time
            FROM participants p JOIN sessions s
            ON p.floor = s.floor AND p.room = s.room AND p.date = s.date AND p.time = s.time
            WHERE p.part_eid = ct_eid AND s.app_eid IS NOT NULL AND (p.date < reported_date AND p.date >= reported_date - 3)
            ),
            close_contacts AS (
            SELECT p1.part_eid
            FROM participants p1 JOIN close_contact_rooms ccr
            ON p1.floor = ccr.floor AND p1.room = ccr.room AND p1.date = ccr.date AND p1.time = ccr.time
            WHERE p1.part_eid <> ct_eid
            )
        INSERT INTO close_contacts_table (SELECT p1.part_eid
            FROM participants p1 JOIN close_contact_rooms ccr
            ON p1.floor = ccr.floor AND p1.room = ccr.room AND p1.date = ccr.date AND p1.time = ccr.time
            WHERE p1.part_eid <> ct_eid);
--      Delete close contact from next 7 days
        DELETE FROM participants p2 WHERE p2.part_eid IN (SELECT * FROM close_contacts_table)
        AND p2.date >= reported_date AND p2.date <= reported_date + 7;
        DELETE FROM Sessions s WHERE s.book_eid IN (SELECT * FROM close_contacts_table)
        AND s.date >= reported_date AND s.date <= reported_date + 7;

--      Delete the employee with from his/her meetings
        DELETE FROM participants p3 WHERE p3.part_eid = ct_eid AND p3.date >= reported_date;
        DELETE FROM Sessions s1 WHERE ct_eid = s1.book_eid AND reported_date <= s1.date;
        RETURN QUERY SELECT DISTINCT * FROM close_contacts_table;
    END;
    $$
    LANGUAGE plpgsql;


/** ADMIN FUNCTION **/
CREATE OR REPLACE FUNCTION non_compliance
    (nc_start_date date, nc_end_date date)
    RETURNS TABLE(nc_eid int, num_days bigint) AS $$
    DECLARE
        curr_date date := nc_start_date;
    BEGIN
        DROP TABLE IF EXISTS big_table;
        CREATE TABLE big_table (
           big_eid int
        );
        WHILE curr_date <= nc_end_date LOOP
            WITH small_table AS
            ((SELECT e.eid FROM employees e) EXCEPT (SELECT hd.eid FROM health_declare hd WHERE hd.date = curr_date))
            INSERT INTO big_table SELECT * FROM small_table;
            curr_date := curr_date + 1;
        END LOOP;
        RETURN QUERY SELECT big_eid, COUNT(big_eid) AS num_days FROM big_table GROUP BY big_eid ORDER BY COUNT(big_eid) DESC;
    END;
    $$
    LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION view_booking_report
    (vbr_start_date date, vbr_eid int)
    RETURNS TABLE(vbr_floor int, vbr_room int, vbr_date date, vbr_time time, is_approved boolean) AS $$
    DECLARE
        is_approved boolean := FALSE;
    BEGIN
        RETURN QUERY (SELECT floor, room, date, time, CASE WHEN app_eid IS NOT NULL THEN TRUE ELSE FALSE END AS is_approved FROM Sessions
        WHERE book_eid = vbr_eid AND date >= vbr_start_date
        ORDER BY date, time);
    END;
    $$
    LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION view_future_meeting
    (vfm_start_date date, vfm_eid int)
    RETURNS TABLE(vfm_floor int, vfm_room int, vfm_date date, vfm_time time) AS $$
    BEGIN
        RETURN QUERY (SELECT P.floor, P.room, P.date, P.time FROM participants P JOIN sessions S ON P.floor = S.floor and P.room = S.room and P.date = S.date and P.time = S.time
        WHERE P.part_eid = vfm_eid AND P.date >= vfm_start_date AND S.app_eid IS NOT NULL
        ORDER BY date, time);
    END;
    $$
    LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION view_manager_report
    (vmr_start_date date, vmr_meid int)
    RETURNS TABLE(vmr_floor int, vmr_room int, vmr_date date, vmr_time time, vmr_eid int) AS $$
    BEGIN
    IF vmr_meid NOT IN (SELECT m.eid FROM managers m) THEN
        RAISE NOTICE 'Employee % is not a manager.', vmr_eid;
        RETURN NEXT;
    END IF;
    RETURN QUERY (SELECT DISTINCT S.floor, S.room, S.date, S.time, S.book_eid FROM sessions S, Employees E, meeting_room MR WHERE
            E.eid = vmr_meid AND (SELECT em_did FROM employees WHERE eid = vmr_meid) = MR.mr_did AND MR.floor = S.floor AND MR.room = S.room AND S.date >= vmr_start_date AND S.app_eid IS NULL);
    END;
    $$
    LANGUAGE plpgsql;


/** End of proc.sql **/