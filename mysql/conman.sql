
DROP TABLE IF EXISTS conman CASCADE;

--users------------------------------------------------------

DROP TABLE IF EXISTS conman;
CREATE TABLE conman
(id SERIAL PRIMARY KEY,
        batchid VARCHAR(25),
        pmid VARCHAR(25),
        affilid INTEGER,
        qname  VARCHAR(250),
	wholen  VARCHAR(250),
	firstn  VARCHAR(250),
	lastn  VARCHAR(250),
	email  VARCHAR(250),
        unsubscribe TIMESTAMP,
        updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        sent TIMESTAMP,
	registered TIMESTAMP
	);

DROP TABLE IF EXISTS affils;
CREATE TABLE affils
(id SERIAL PRIMARY KEY,
	affil  VARCHAR(65000),
	updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP);


DROP TABLE IF EXISTS ref;
CREATE TABLE ref
(id SERIAL PRIMARY KEY,
        pmid VARCHAR(250),
	journal  VARCHAR(65000),
	title  VARCHAR(65000),
	updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP);




DROP TABLE IF EXISTS conmanstats;
CREATE TABLE conmanstats
(id SERIAL PRIMARY KEY,
        batchid VARCHAR(25),
        article INTEGER,
        author INTEGER,
        author_search INTEGER,
        author_find INTEGER,
        elapsed INTEGER,
	updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP);


DROP FUNCTION IF exists get_unique_email_batch( _start_date VARCHAR(250));

CREATE OR REPLACE FUNCTION get_unique_email_batch(_start_date VARCHAR(250))
RETURNS TABLE( firstn VARCHAR(250),  emails VARCHAR(250)) AS
$BODY$
DECLARE
ids INTEGER[];
id_var INTEGER;
emails VARCHAR[];
start_time TIMESTAMP = _start_date || ' 00:00:00';
ts TIMESTAMP = CURRENT_TIMESTAMP;
BEGIN

DROP TABLE IF EXISTS t1;
CREATE TEMP TABLE t1 AS SELECT DISTINCT ON (email) email, conman.ID from conman WHERE updated >  start_time and email != 'null';
select array (select t1.id from t1) into ids;

FOR id_var IN 1..array_length(ids,1) LOOP
update conman  SET sent= ts WHERE conman.id = ids[id_var];
END LOOP;

DROP TABLE IF EXISTS t1;

RETURN QUERY SELECT conman.firstn, conman.email FROM conman WHERE sent = ts;

END;
$BODY$
  LANGUAGE plpgsql VOLATILE;


-- SELECT * FROM get_unique_email_batch('2020-09-10');
-- SELECT email, sent FROM conman ORDER BY email;
-- UPDATE conman SET sent = NULL;

-- SELECT DISTINCT on (email) email, conman.ID from conman WHERE updated >  '2020-09-10 00:00:00' and email != 'null' ORDER BY email;




stored procedure update_conman    pmid journal title affil
CREATE PROCEDURE update_conman(_affil TEXT(650))
BEGIN
DECLARE last_affils_id INT DEFAULT 0;
INSERT INTO affils(affils) VALUES(_affil);
SET last_affils_id = LAST_INSERT_ID();
UPDATE  payment SET customer_id =last_customer_id, expect=_coin_req  WHERE transaction_id IS NULL AND customer_id IS NULL and currency LIKE _currency LIMIT 1;
SELECT wallet_id INTO @returned_wallet_id FROM payment WHERE customer_id = last_customer_id;
SELECT @returned_wallet_id;

END

-- -------------------------------------------------------------------

DROP procedure IF EXISTS update_conman;

DELIMITER  //
CREATE PROCEDURE update_conman( IN _batchid VARCHAR(25), IN _pmid VARCHAR(25), IN _qname CHARACTER(250), IN _wholen CHARACTER(250), IN _firstn CHARACTER(250), IN _lastn CHARACTER(250), IN _affil CHARACTER(250), IN _email CHARACTER(250))
BEGIN
DECLARE last_affils_id BIGINT DEFAULT 0;
-- check if the affiliation is already in the db; if yes return its id
-- if no insert and return the new id
If EXISTS (SELECT id FROM affils WHERE affil LIKE _affil)
THEN
BEGIN
SELECT id FROM affils WHERE affil LIKE _affil INTO  last_affils_id;
END;
ELSE
BEGIN
INSERT INTO affils(affil) VALUES ( _affil );
SET last_affils_id = LAST_INSERT_ID();
END;
End IF; 
-- check if the email is already in the db; if yes, is it marked unsubscribe?
-- if yes do not insert, if no insert because this is most likely a new pmid
IF NOT EXISTS (SELECT email FROM conman WHERE email LIKE _email)
THEN
BEGIN
INSERT INTO conman ( batchid, pmid, affilid, qname, wholen, firstn, lastn, email )
VALUES( _batchid, _pmid, last_affils_id, _qname, _wholen, _firstn, _lastn, _email);
END;
ELSE
  IF (SELECT unsubscribe FROM conman WHERE email LIKE _email LIMIT 1)
  THEN
  BEGIN
  -- do nothing - unsubscribed
  END;
  ELSE
  BEGIN
  INSERT INTO conman ( batchid, pmid, affilid, qname, wholen, firstn, lastn, email )
  VALUES( _batchid, _pmid, last_affils_id, _qname, _wholen, _firstn, _lastn, _email);
  END;
  END if;
End IF; 
SELECT last_affils_id;
END //
DELIMITER  ;




stored procedure new_customer

BEGIN
DECLARE last_customer_id INT DEFAULT 0;
INSERT INTO customer (first_name, last_name, institution, email, product) VALUES(_fname, _lname, _institution, _email, _product);
SET last_customer_id = LAST_INSERT_ID();
UPDATE  payment SET customer_id =last_customer_id, expect=_coin_req  WHERE transaction_id IS NULL AND customer_id IS NULL and currency LIKE _currency LIMIT 1;
SELECT wallet_id INTO @returned_wallet_id FROM payment WHERE customer_id = last_customer_id;
SELECT @returned_wallet_id;

END














