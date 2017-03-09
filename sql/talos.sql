CREATE SEQUENCE clients_id_seq;
CREATE TABLE clients (
    id smallint PRIMARY KEY DEFAULT nextval('clients_id_seq'),
    name varchar(24) NOT NULL UNIQUE,
    status smallint NOT NULL,
    status_description varchar(128) NULL,
    first_seen timestamp NULL DEFAULT now(),
    last_seen timestamp NULL DEFAULT now(),
    reachable boolean NULL
);
ALTER SEQUENCE clients_id_seq OWNED BY clients.id;
CREATE INDEX clients_idx ON clients (name, status, reachable, last_seen);

CREATE SEQUENCE fixes_id_seq;
CREATE TABLE fixes (
    id integer PRIMARY KEY DEFAULT nextval('fixes_id_seq'),
    client smallint REFERENCES clients (id),
    date_fixed timestamp NOT NULL
);
ALTER SEQUENCE fixes_id_seq OWNED BY fixes.id;
CREATE INDEX fixes_idx ON fixes (client, date_fixed);

CREATE SEQUENCE errors_id_seq;
CREATE TABLE errors (
    id integer PRIMARY KEY DEFAULT nextval('errors_id_seq'),
    client smallint REFERENCES clients (id),
    error_date timestamp NOT NULL,
    description varchar(128) NULL,
    last_fix integer NULL REFERENCES fixes (id)
);
ALTER SEQUENCE errors_id_seq OWNED BY errors.id;
CREATE INDEX errors_idx ON errors (client, last_fix);

CREATE TABLE client_exceptions (
    name varchar(24) PRIMARY KEY
);

CREATE OR REPLACE VIEW check_clients AS
    SELECT * FROM clients WHERE status <> 0 AND status <> 999 AND
        EXTRACT(DAY FROM (now() - COALESCE(last_seen, (now() - INTERVAL '30 days')))) < 30 AND
		name NOT IN (SELECT * FROM client_exceptions);

CREATE OR REPLACE FUNCTION insert_fix(client varchar(24), date_fixed timestamp with time zone) RETURNS smallint AS $$
DECLARE
    cid smallint;
BEGIN
    SELECT id INTO cid FROM clients WHERE name = client LIMIT 1;
    IF cid IS NOT NULL THEN
        INSERT INTO fixes (client, date_fixed) VALUES (cid, date_fixed);
    END IF;
    RETURN cid;
END
$$ LANGUAGE plpgsql SECURITY DEFINER;

CREATE OR REPLACE FUNCTION update_client(client varchar(24),
                                         status smallint,
                                         status_description varchar(128),
                                         reachable boolean) RETURNS void AS $$
DECLARE
    cid smallint;
BEGIN
    SELECT id INTO cid FROM clients WHERE name = client LIMIT 1;
    IF cid IS NOT NULL THEN
        IF reachable THEN
            UPDATE clients SET status = $2,
                               status_description = $3,
                               reachable = $4,
                               last_seen = now()
                           WHERE name = $1;
        ELSE
            UPDATE clients SET status = $2,
                               status_description = $3,
                               reachable = $4
                           WHERE name = $1;
        END IF;
    ELSE
        INSERT INTO clients (name, status, status_description, reachable, first_seen)
                    VALUES (client, 2, 'No reparado', false, now());
    END IF;
END
$$ LANGUAGE plpgsql SECURITY DEFINER;

CREATE OR REPLACE FUNCTION mark_fixed(client varchar(24)) RETURNS void AS $$
DECLARE
    cid smallint;
BEGIN
    SELECT id INTO cid FROM clients WHERE name = client LIMIT 1;
    IF cid IS NOT NULL THEN
        PERFORM update_client(client, 0::smallint, 'OK', true);
        PERFORM insert_fix(client, now());
    ELSE
        RAISE NOTICE 'El cliente (%) no existe; se ignora el comando.', client;
    END IF;
END
$$ LANGUAGE plpgsql SECURITY DEFINER;