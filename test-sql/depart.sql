--
-- PostgreSQL database dump
--

SET statement_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = public, pg_catalog;

--
-- Name: rrule_instance; Type: TYPE; Schema: public; Owner: davical_dba
--

CREATE TYPE rrule_instance AS (
	dtstart timestamp with time zone,
	rrule text,
	instance timestamp with time zone
);


ALTER TYPE public.rrule_instance OWNER TO davical_dba;

--
-- Name: rrule_parts; Type: TYPE; Schema: public; Owner: davical_dba
--

CREATE TYPE rrule_parts AS (
	base timestamp with time zone,
	until timestamp with time zone,
	freq text,
	count integer,
	"interval" integer,
	bysecond integer[],
	byminute integer[],
	byhour integer[],
	bymonthday integer[],
	byyearday integer[],
	byweekno integer[],
	byday text[],
	bymonth integer[],
	bysetpos integer[],
	wkst text
);


ALTER TYPE public.rrule_parts OWNER TO davical_dba;

--
-- Name: alarm_changed(); Type: FUNCTION; Schema: public; Owner: davical_dba
--

CREATE FUNCTION alarm_changed() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
  oldcomponent TEXT;
  newcomponent TEXT;
BEGIN
  -- in case we trigger on other events in future
  IF TG_OP = 'UPDATE' THEN
    IF NEW.component != OLD.component THEN
      UPDATE caldav_data
         SET caldav_data = replace( caldav_data, OLD.component, NEW.component ),
             dav_etag = md5(replace( caldav_data, OLD.component, NEW.component ))
       WHERE caldav_data.dav_id = NEW.dav_id;
    END IF;
  END IF;
  RETURN NEW;
END;
$$;


ALTER FUNCTION public.alarm_changed() OWNER TO davical_dba;

--
-- Name: apply_month_byday(timestamp with time zone, text); Type: FUNCTION; Schema: public; Owner: davical_dba
--

CREATE FUNCTION apply_month_byday(timestamp with time zone, text) RETURNS timestamp with time zone
    LANGUAGE plpgsql IMMUTABLE STRICT
    AS $_$
DECLARE
  in_time ALIAS FOR $1;
  byday ALIAS FOR $2;
  weeks INT;
  dow INT;
  temp_txt TEXT;
  dd INT;
  mm INT;
  yy INT;
  our_dow INT;
  our_answer TIMESTAMP WITH TIME ZONE;
BEGIN
  dow := position(substring( byday from '..$') in 'SUMOTUWETHFRSA') / 2;
  temp_txt   := substring(byday from '([0-9]+)');
  weeks      := temp_txt::int;

  -- RAISE NOTICE 'DOW: %, Weeks: %(%s)', dow, weeks, temp_txt;

  IF substring(byday for 1) = '-' THEN
    -- Last XX of month, or possibly second-to-last, but unlikely
    mm := extract( 'month' from in_time);
    yy := extract( 'year' from in_time);

    -- Start with the last day of the month
    our_answer := (yy::text || '-' || (mm+1)::text || '-01')::timestamp - '1 day'::interval;
    dd := extract( 'dow' from our_answer);
    dd := dd - dow;
    IF dd < 0 THEN
      dd := dd + 7;
    END IF;

    -- Having calculated the right day of the month, we now apply that back to in_time
    -- which contains the otherwise-unobtainable timezone detail (and the time)
    our_answer = our_answer - (dd::text || 'days')::interval;
    dd := extract( 'day' from our_answer) - extract( 'day' from in_time);
    our_answer := in_time + (dd::text || 'days')::interval;

    IF weeks > 1 THEN
      weeks := weeks - 1;
      our_answer := our_answer - (weeks::text || 'weeks')::interval;
    END IF;

  ELSE

    -- Shift our date to the correct day of week..
    our_dow := extract( 'dow' from in_time);
    our_dow := our_dow - dow;
    dd := extract( 'day' from in_time);
    IF our_dow >= dd THEN
      our_dow := our_dow - 7;
    END IF;
    our_answer := in_time - (our_dow::text || 'days')::interval;
    dd = extract( 'day' from our_answer);

    -- Shift the date to the correct week...
    dd := weeks - ((dd+6) / 7);
    IF dd != 0 THEN
      our_answer := our_answer + ((dd::text || 'weeks')::interval);
    END IF;

  END IF;

  RETURN our_answer;

END;
$_$;


ALTER FUNCTION public.apply_month_byday(timestamp with time zone, text) OWNER TO davical_dba;

--
-- Name: bits_to_legacy_privilege(bit); Type: FUNCTION; Schema: public; Owner: davical_dba
--

CREATE FUNCTION bits_to_legacy_privilege(bit) RETURNS text
    LANGUAGE plpgsql IMMUTABLE STRICT
    AS $_$
DECLARE
  in_bits ALIAS FOR $1;
  out_priv TEXT;
BEGIN
  out_priv := '';
  IF in_bits = (~ 0::BIT(24)) THEN
    out_priv = 'A';
    RETURN out_priv;
  END IF;

  -- The CALDAV:read-free-busy privilege MUST be aggregated in the DAV:read privilege.
  --    1 DAV:read
  --  512 CalDAV:read-free-busy
  -- 4096 CALDAV:schedule-query-freebusy
  IF (in_bits & 4609::BIT(24)) != 0::BIT(24) THEN
    IF (in_bits & 1::BIT(24)) != 0::BIT(24) THEN
      out_priv := 'R';
    ELSE
      out_priv := 'F';
    END IF;
  END IF;

  -- DAV:write => DAV:write MUST contain DAV:bind, DAV:unbind, DAV:write-properties and DAV:write-content
  --    2 DAV:write-properties
  --    4 DAV:write-content
  --   64 DAV:bind
  --  128 DAV:unbind
  IF (in_bits & 198::BIT(24)) != 0::BIT(24) THEN
    IF (in_bits & 6::BIT(24)) != 0::BIT(24) THEN
      out_priv := out_priv || 'W';
    ELSE
      IF (in_bits & 64::BIT(24)) != 0::BIT(24) THEN
        out_priv := out_priv || 'B';
      END IF;
      IF (in_bits & 128::BIT(24)) != 0::BIT(24) THEN
        out_priv := out_priv || 'U';
      END IF;
    END IF;
  END IF;

  RETURN out_priv;
END
$_$;


ALTER FUNCTION public.bits_to_legacy_privilege(bit) OWNER TO davical_dba;

--
-- Name: bits_to_privilege(bit); Type: FUNCTION; Schema: public; Owner: davical_dba
--

CREATE FUNCTION bits_to_privilege(bit) RETURNS text[]
    LANGUAGE plpgsql IMMUTABLE STRICT
    AS $_$
DECLARE
  in_bits ALIAS FOR $1;
  out_priv TEXT[];
BEGIN
  IF in_bits = (~ 0::BIT(24)) THEN
    out_priv := out_priv || ARRAY['DAV:all'];
  END IF;

  IF (in_bits & 513::BIT(24)) != 0::BIT(24) THEN
    IF (in_bits & 1::BIT(24)) != 0::BIT(24) THEN
      out_priv := out_priv || ARRAY['DAV:read'];
    END IF;
    IF (in_bits & 512::BIT(24)) != 0::BIT(24) THEN
      out_priv := out_priv || ARRAY['caldav:read-free-busy'];
    END IF;
  END IF;

  IF (in_bits & 198::BIT(24)) != 0::BIT(24) THEN
    IF (in_bits & 198::BIT(24)) = 198::BIT(24) THEN
      out_priv := out_priv || ARRAY['DAV:write'];
    ELSE
      IF (in_bits & 2::BIT(24)) != 0::BIT(24) THEN
        out_priv := out_priv || ARRAY['DAV:write-properties'];
      END IF;
      IF (in_bits & 4::BIT(24)) != 0::BIT(24) THEN
        out_priv := out_priv || ARRAY['DAV:write-content'];
      END IF;
      IF (in_bits & 64::BIT(24)) != 0::BIT(24) THEN
        out_priv := out_priv || ARRAY['DAV:bind'];
      END IF;
      IF (in_bits & 128::BIT(24)) != 0::BIT(24) THEN
        out_priv := out_priv || ARRAY['DAV:unbind'];
      END IF;
    END IF;
  END IF;

  IF (in_bits & 8::BIT(24)) != 0::BIT(24) THEN
    out_priv := out_priv || ARRAY['DAV:unlock'];
  END IF;

  IF (in_bits & 16::BIT(24)) != 0::BIT(24) THEN
    out_priv := out_priv || ARRAY['DAV:read-acl'];
  END IF;

  IF (in_bits & 32::BIT(24)) != 0::BIT(24) THEN
    out_priv := out_priv || ARRAY['DAV:read-current-user-privilege-set'];
  END IF;

  IF (in_bits & 256::BIT(24)) != 0::BIT(24) THEN
    out_priv := out_priv || ARRAY['DAV:write-acl'];
  END IF;

  IF (in_bits & 7168::BIT(24)) != 0::BIT(24) THEN
    IF (in_bits & 7168::BIT(24)) = 7168::BIT(24) THEN
      out_priv := out_priv || ARRAY['caldav:schedule-deliver'];
    ELSE
      IF (in_bits & 1024::BIT(24)) != 0::BIT(24) THEN
        out_priv := out_priv || ARRAY['caldav:schedule-deliver-invite'];
      END IF;
      IF (in_bits & 2048::BIT(24)) != 0::BIT(24) THEN
        out_priv := out_priv || ARRAY['caldav:schedule-deliver-reply'];
      END IF;
      IF (in_bits & 4096::BIT(24)) != 0::BIT(24) THEN
        out_priv := out_priv || ARRAY['caldav:schedule-query-freebusy'];
      END IF;
    END IF;
  END IF;

  IF (in_bits & 57344::BIT(24)) != 0::BIT(24) THEN
    IF (in_bits & 57344::BIT(24)) = 57344::BIT(24) THEN
      out_priv := out_priv || ARRAY['caldav:schedule-send'];
    ELSE
      IF (in_bits & 8192::BIT(24)) != 0::BIT(24) THEN
        out_priv := out_priv || ARRAY['caldav:schedule-send-invite'];
      END IF;
      IF (in_bits & 16384::BIT(24)) != 0::BIT(24) THEN
        out_priv := out_priv || ARRAY['caldav:schedule-send-reply'];
      END IF;
      IF (in_bits & 32768::BIT(24)) != 0::BIT(24) THEN
        out_priv := out_priv || ARRAY['caldav:schedule-send-freebusy'];
      END IF;
    END IF;
  END IF;

  RETURN out_priv;
END
$_$;


ALTER FUNCTION public.bits_to_privilege(bit) OWNER TO davical_dba;

--
-- Name: calculate_later_timestamp(timestamp with time zone, timestamp with time zone, text); Type: FUNCTION; Schema: public; Owner: davical_dba
--

CREATE FUNCTION calculate_later_timestamp(timestamp with time zone, timestamp with time zone, text) RETURNS timestamp with time zone
    LANGUAGE plpgsql IMMUTABLE STRICT
    AS $_$
DECLARE
  earliest ALIAS FOR $1;
  basedate ALIAS FOR $2;
  repeatrule ALIAS FOR $3;
  frequency TEXT;
  temp_txt TEXT;
  length INT;
  count INT;
  byday TEXT;
  bymonthday INT;
  basediff INTERVAL;
  past_repeats INT8;
  units TEXT;
  dow TEXT;
  our_answer TIMESTAMP WITH TIME ZONE;
  loopcount INT;
BEGIN
  IF basedate > earliest THEN
    RETURN basedate;
  END IF;

  temp_txt   := substring(repeatrule from 'UNTIL=([0-9TZ]+)(;|$)');
  IF temp_txt IS NOT NULL AND temp_txt::timestamp with time zone < earliest THEN
    RETURN NULL;
  END IF;

  frequency  := substring(repeatrule from 'FREQ=([A-Z]+)(;|$)');
  IF frequency IS NULL THEN
    RETURN NULL;
  END IF;

  past_repeats = 0;
  length = 1;
  temp_txt   := substring(repeatrule from 'INTERVAL=([0-9]+)(;|$)');
  IF temp_txt IS NOT NULL THEN
    length     := temp_txt::int;
    basediff   := earliest - basedate;

    -- RAISE NOTICE 'Frequency: %, Length: %(%), Basediff: %', frequency, length, temp_txt, basediff;

    -- Calculate the number of past periods between our base date and our earliest date
    IF frequency = 'WEEKLY' OR frequency = 'DAILY' THEN
      past_repeats := extract('epoch' from basediff)::INT8 / 86400;
      -- RAISE NOTICE 'Days: %', past_repeats;
      IF frequency = 'WEEKLY' THEN
        past_repeats := past_repeats / 7;
      END IF;
    ELSE
      past_repeats = extract( 'years' from basediff );
      IF frequency = 'MONTHLY' THEN
        past_repeats = (past_repeats *12) + extract( 'months' from basediff );
      END IF;
    END IF;
    IF length IS NOT NULL THEN
      past_repeats = (past_repeats / length) + 1;
    END IF;
  END IF;

  -- Check that we have not exceeded the COUNT= limit
  temp_txt := substring(repeatrule from 'COUNT=([0-9]+)(;|$)');
  IF temp_txt IS NOT NULL THEN
    count := temp_txt::int;
    -- RAISE NOTICE 'Periods: %, Count: %(%), length: %', past_repeats, count, temp_txt, length;
    IF ( count <= past_repeats ) THEN
      RETURN NULL;
    END IF;
  ELSE
    count := NULL;
  END IF;

  temp_txt := substring(repeatrule from 'BYSETPOS=([0-9-]+)(;|$)');
  byday := substring(repeatrule from 'BYDAY=([0-9A-Z,]+-)(;|$)');
  IF byday IS NOT NULL AND frequency = 'MONTHLY' THEN
    -- Since this could move the date around a month we go back one
    -- period just to be extra sure.
    past_repeats = past_repeats - 1;

    IF temp_txt IS NOT NULL THEN
      -- Crudely hack the BYSETPOS onto the front of BYDAY.  While this
      -- is not as per rfc2445, RRULE syntax is so complex and overblown
      -- that nobody correctly uses comma-separated BYDAY or BYSETPOS, and
      -- certainly not within a MONTHLY RRULE.
      byday := temp_txt || byday;
    END IF;
  END IF;

  past_repeats = past_repeats * length;

  units := CASE
    WHEN frequency = 'DAILY' THEN 'days'
    WHEN frequency = 'WEEKLY' THEN 'weeks'
    WHEN frequency = 'MONTHLY' THEN 'months'
    WHEN frequency = 'YEARLY' THEN 'years'
  END;

  temp_txt   := substring(repeatrule from 'BYMONTHDAY=([0-9,]+)(;|$)');
  bymonthday := temp_txt::int;

  -- With all of the above calculation, this date should be close to (but less than)
  -- the target, and we should only loop once or twice.
  our_answer := basedate + (past_repeats::text || units)::interval;

  IF our_answer IS NULL THEN
    RAISE EXCEPTION 'our_answer IS NULL! basedate:% past_repeats:% units:%', basedate, past_repeats, units;
  END IF;


  loopcount := 500;  -- Desirable to stop an infinite loop if there is something we cannot handle
  LOOP
    -- RAISE NOTICE 'Testing date: %', our_answer;
    IF frequency = 'DAILY' THEN
      IF byday IS NOT NULL THEN
        LOOP
          dow = substring( to_char( our_answer, 'DY' ) for 2);
          EXIT WHEN byday ~* dow;
          -- Increment for our next time through the loop...
          our_answer := our_answer + (length::text || units)::interval;
        END LOOP;
      END IF;
    ELSIF frequency = 'WEEKLY' THEN
      -- Weekly repeats are only on specific days
      -- This is really not right, since a WEEKLY on MO,WE,FR should
      -- occur three times each week and this will only be once a week.
      dow = substring( to_char( our_answer, 'DY' ) for 2);
    ELSIF frequency = 'MONTHLY' THEN
      IF byday IS NOT NULL THEN
        -- This works fine, except that maybe there are multiple BYDAY
        -- components.  e.g. 1TU,3TU might be 1st & 3rd tuesdays.
        our_answer := apply_month_byday( our_answer, byday );
      ELSE
        -- If we did not get a BYDAY= then we kind of have to assume it is the same day each month
        our_answer := our_answer + '1 month'::interval;
      END IF;
    ELSIF bymonthday IS NOT NULL AND frequency = 'MONTHLY' AND bymonthday < 1 THEN
      -- We do not deal with this situation at present
      RAISE NOTICE 'The case of negative BYMONTHDAY is not handled yet.';
    END IF;

    EXIT WHEN our_answer >= earliest;

    -- Give up if we have exceeded the count
    IF ( count IS NOT NULL AND past_repeats > count ) THEN
      RETURN NULL;
    ELSE
      past_repeats := past_repeats + 1;
    END IF;

    loopcount := loopcount - 1;
    IF loopcount < 0 THEN
      RAISE NOTICE 'Giving up on repeat rule "%" - after 100 increments from % we are still not after %', repeatrule, basedate, earliest;
      RETURN NULL;
    END IF;

    -- Increment for our next time through the loop...
    our_answer := our_answer + (length::text || units)::interval;

  END LOOP;

  RETURN our_answer;

END;
$_$;


ALTER FUNCTION public.calculate_later_timestamp(timestamp with time zone, timestamp with time zone, text) OWNER TO davical_dba;

--
-- Name: caldav_data_modified(); Type: FUNCTION; Schema: public; Owner: davical_dba
--

CREATE FUNCTION caldav_data_modified() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
  coll_id caldav_data.collection_id%TYPE;
BEGIN
  IF TG_OP = 'UPDATE' THEN
    IF NEW.caldav_data = OLD.caldav_data AND NEW.collection_id = OLD.collection_id THEN
      -- Nothing for us to do
      RETURN NEW;
    END IF;
  END IF;

  IF TG_OP = 'INSERT' OR TG_OP = 'UPDATE' THEN
    -- On insert or update modified, we set the NEW collection tag to the md5 of the
    -- etag of the updated row which gives us something predictable for our regression
    -- tests, but something different from the actual etag of the new event.
    UPDATE collection
       SET modified = current_timestamp, dav_etag = md5(NEW.dav_etag)
     WHERE collection_id = NEW.collection_id;
    IF TG_OP = 'INSERT' THEN
      RETURN NEW;
    END IF;
  END IF;

  IF TG_OP = 'DELETE' THEN
    -- On delete we set the OLD collection tag to the md5 of the old path & the old
    -- etag, which again gives us something predictable for our regression tests.
    UPDATE collection
       SET modified = current_timestamp, dav_etag = md5(OLD.dav_name::text||OLD.dav_etag)
     WHERE collection_id = OLD.collection_id;
    RETURN OLD;
  END IF;

  IF NEW.collection_id != OLD.collection_id THEN
    -- If we've switched the collection_id of this event, then we also need to update
    -- the etag of the old collection - as we do for delete.
    UPDATE collection
       SET modified = current_timestamp, dav_etag = md5(OLD.dav_name::text||OLD.dav_etag)
     WHERE collection_id = OLD.collection_id;
  END IF;
  RETURN NEW;
END;
$$;


ALTER FUNCTION public.caldav_data_modified() OWNER TO davical_dba;

--
-- Name: check_db_revision(integer, integer, integer); Type: FUNCTION; Schema: public; Owner: davical_dba
--

CREATE FUNCTION check_db_revision(integer, integer, integer) RETURNS boolean
    LANGUAGE plpgsql
    AS $_$
   DECLARE
      major ALIAS FOR $1;
      minor ALIAS FOR $2;
      patch ALIAS FOR $3;
      matching INT;
   BEGIN
      SELECT COUNT(*) INTO matching FROM awl_db_revision
             WHERE (schema_major = major AND schema_minor = minor AND schema_patch > patch)
                OR (schema_major = major AND schema_minor > minor)
                OR (schema_major > major)
             ;
      IF matching >= 1 THEN
        RAISE EXCEPTION 'Database revisions after %.%.% have already been applied.', major, minor, patch;
        RETURN FALSE;
      END IF;
      SELECT COUNT(*) INTO matching FROM awl_db_revision
                      WHERE schema_major = major AND schema_minor = minor AND schema_patch = patch;
      IF matching >= 1 THEN
        RETURN TRUE;
      END IF;
      RAISE EXCEPTION 'Database has not been upgraded to %.%.%', major, minor, patch;
      RETURN FALSE;
   END;
$_$;


ALTER FUNCTION public.check_db_revision(integer, integer, integer) OWNER TO davical_dba;

--
-- Name: collection_modified(); Type: FUNCTION; Schema: public; Owner: davical_dba
--

CREATE FUNCTION collection_modified() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
BEGIN
  -- in case we trigger on other events in future
  IF TG_OP = 'UPDATE' THEN
    IF NEW.dav_name != OLD.dav_name THEN
      UPDATE caldav_data
        SET dav_name = replace( dav_name, OLD.dav_name, NEW.dav_name),
            user_no = NEW.user_no
      WHERE substring(dav_name from 1 for char_length(OLD.dav_name)) = OLD.dav_name;
    END IF;
  END IF;
  RETURN NEW;
END;
$$;


ALTER FUNCTION public.collection_modified() OWNER TO davical_dba;

--
-- Name: collections_within(integer, integer); Type: FUNCTION; Schema: public; Owner: davical_dba
--

CREATE FUNCTION collections_within(integer, integer) RETURNS SETOF integer
    LANGUAGE plpgsql
    AS $_$
DECLARE
    in_collection_id ALIAS FOR $1;
    in_depth ALIAS FOR $2;
    resource_id INT;
    found_some BOOLEAN;
BEGIN
    in_depth := in_depth - 1;
    found_some = FALSE;
	FOR resource_id IN SELECT b.bound_source_id FROM dav_binding b
                        JOIN collection pc ON (b.parent_container = pc.dav_name)
                        WHERE pc.collection_id = in_collection_id
    LOOP
        found_some = TRUE;
        RETURN NEXT resource_id;
        IF in_depth > 0 THEN
            FOR resource_id IN SELECT * FROM collections_within( resource_id, in_depth ) LOOP
                RETURN NEXT resource_id;
            END LOOP;
        END IF;
    END LOOP;
    FOR resource_id IN SELECT c.collection_id FROM collection c
                        JOIN collection pc ON (c.parent_container = pc.dav_name)
                        WHERE pc.collection_id = in_collection_id
    LOOP
        found_some = TRUE;
        RETURN NEXT resource_id;
        IF in_depth > 0 THEN
            FOR resource_id IN SELECT * FROM collections_within( resource_id, in_depth ) LOOP
                RETURN NEXT resource_id;
            END LOOP;
        END IF;
    END LOOP;
    IF found_some THEN
        RETURN;
    END IF;
    FOR resource_id IN SELECT c.collection_id FROM collection c
                        JOIN dav_principal pc ON (c.parent_container = pc.dav_name)
                        WHERE pc.principal_id = in_collection_id
    LOOP
        RETURN NEXT resource_id;
        IF in_depth > 0 THEN
            FOR resource_id IN SELECT * FROM collections_within( resource_id, in_depth ) LOOP
                RETURN NEXT resource_id;
            END LOOP;
        END IF;
    END LOOP;
END;
$_$;


ALTER FUNCTION public.collections_within(integer, integer) OWNER TO davical_dba;

--
-- Name: daily_set(timestamp with time zone, rrule_parts); Type: FUNCTION; Schema: public; Owner: davical_dba
--

CREATE FUNCTION daily_set(timestamp with time zone, rrule_parts) RETURNS SETOF timestamp with time zone
    LANGUAGE plpgsql IMMUTABLE STRICT
    AS $_$
DECLARE
  after ALIAS FOR $1;
  rrule ALIAS FOR $2;
BEGIN

  IF rrule.bymonth IS NOT NULL AND NOT date_part('month',after) = ANY ( rrule.bymonth ) THEN
    RETURN;
  END IF;

  IF rrule.byweekno IS NOT NULL AND NOT date_part('week',after) = ANY ( rrule.byweekno ) THEN
    RETURN;
  END IF;

  IF rrule.byyearday IS NOT NULL AND NOT date_part('doy',after) = ANY ( rrule.byyearday ) THEN
    RETURN;
  END IF;

  IF rrule.bymonthday IS NOT NULL AND NOT date_part('day',after) = ANY ( rrule.bymonthday ) THEN
    RETURN;
  END IF;

  IF rrule.byday IS NOT NULL AND NOT substring( to_char( after, 'DY') for 2 from 1) = ANY ( rrule.byday ) THEN
    RETURN;
  END IF;

  -- Since we don't do BYHOUR, BYMINUTE or BYSECOND yet this becomes a trivial
  RETURN NEXT after;

END;
$_$;


ALTER FUNCTION public.daily_set(timestamp with time zone, rrule_parts) OWNER TO davical_dba;

--
-- Name: event_has_exceptions(text); Type: FUNCTION; Schema: public; Owner: davical_dba
--

CREATE FUNCTION event_has_exceptions(text) RETURNS boolean
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
  SELECT $1 ~ E'\nRECURRENCE-ID(;TZID=[^:]+)?:[[:space:]]*[[:digit:]]{8}(T[[:digit:]]{6})?'
$_$;


ALTER FUNCTION public.event_has_exceptions(text) OWNER TO davical_dba;

--
-- Name: event_instances(timestamp with time zone, text); Type: FUNCTION; Schema: public; Owner: davical_dba
--

CREATE FUNCTION event_instances(timestamp with time zone, text) RETURNS SETOF timestamp with time zone
    LANGUAGE plpgsql IMMUTABLE STRICT
    AS $_$
DECLARE
  basedate ALIAS FOR $1;
  repeatrule ALIAS FOR $2;
  maxdate TIMESTAMP WITH TIME ZONE;
BEGIN
  maxdate := current_date + '10 years'::interval;
  RETURN QUERY SELECT d FROM rrule_event_instances_range( basedate, repeatrule, basedate, maxdate, 300 ) d;
END;
$_$;


ALTER FUNCTION public.event_instances(timestamp with time zone, text) OWNER TO davical_dba;

--
-- Name: expand_members(bigint, integer); Type: FUNCTION; Schema: public; Owner: davical_dba
--

CREATE FUNCTION expand_members(bigint, integer) RETURNS SETOF bigint
    LANGUAGE sql STABLE STRICT
    AS $_$
  SELECT member_id FROM group_member WHERE group_id = $1
      UNION
  SELECT expanded.m_id FROM (SELECT CASE WHEN $2 > 0 THEN expand_members( member_id, $2 - 1) END AS m_id
                               FROM group_member WHERE group_id = $1) AS expanded
                       WHERE expanded.m_id IS NOT NULL;
$_$;


ALTER FUNCTION public.expand_members(bigint, integer) OWNER TO davical_dba;

--
-- Name: expand_memberships(bigint, integer); Type: FUNCTION; Schema: public; Owner: davical_dba
--

CREATE FUNCTION expand_memberships(bigint, integer) RETURNS SETOF bigint
    LANGUAGE sql STABLE STRICT
    AS $_$
  SELECT group_id FROM group_member WHERE member_id = $1
      UNION
  SELECT expanded.g_id FROM (SELECT CASE WHEN $2 > 0 THEN expand_memberships( group_id, $2 - 1) END AS g_id
                               FROM group_member WHERE member_id = $1) AS expanded
                       WHERE expanded.g_id IS NOT NULL;
$_$;


ALTER FUNCTION public.expand_memberships(bigint, integer) OWNER TO davical_dba;

--
-- Name: get_group_role_no(); Type: FUNCTION; Schema: public; Owner: davical_dba
--

CREATE FUNCTION get_group_role_no() RETURNS integer
    LANGUAGE sql IMMUTABLE
    AS $$
  SELECT role_no FROM roles WHERE role_name = 'Group'
$$;


ALTER FUNCTION public.get_group_role_no() OWNER TO davical_dba;

--
-- Name: get_permissions(integer, integer); Type: FUNCTION; Schema: public; Owner: davical_dba
--

CREATE FUNCTION get_permissions(integer, integer) RETURNS text
    LANGUAGE plpgsql IMMUTABLE STRICT
    AS $_$
DECLARE
  in_from ALIAS FOR $1;
  in_to   ALIAS FOR $2;
  out_confers TEXT;
  bit_confers BIT(24);
  group_role_no INT;
  tmp_txt TEXT;
  dbg TEXT DEFAULT '';
  r RECORD;
  counter INT;
BEGIN
  -- Self can always have full access
  IF in_from = in_to THEN
    RETURN 'A';
  END IF;

  -- dbg := 'S-';
  SELECT bits_to_legacy_privilege(r1.confers) INTO out_confers FROM relationship r1
                    WHERE r1.from_user = in_from AND r1.to_user = in_to AND NOT usr_is_role(r1.to_user,'Group');
  IF FOUND THEN
    RETURN dbg || out_confers;
  END IF;
  -- RAISE NOTICE 'No simple relationships between % and %', in_from, in_to;

  SELECT bit_or(r1.confers & r2.confers) INTO bit_confers
              FROM relationship r1
              JOIN relationship r2 ON r1.to_user=r2.from_user
         WHERE r1.from_user=in_from AND r2.to_user=in_to
           AND r2.from_user IN (SELECT user_no FROM roles LEFT JOIN role_member USING(role_no) WHERE role_name='Group');
  IF bit_confers != 0::BIT(24) THEN
    RETURN dbg || bits_to_legacy_privilege(bit_confers);
  END IF;

  RETURN '';
  -- RAISE NOTICE 'No complex relationships between % and %', in_from, in_to;

  SELECT bits_to_legacy_privilege(r1.confers) INTO out_confers FROM relationship r1 LEFT OUTER JOIN relationship r2 ON(r1.to_user = r2.to_user)
       WHERE r1.from_user = in_from AND r2.from_user = in_to AND r1.from_user != r2.from_user
         AND NOT EXISTS( SELECT 1 FROM relationship r3 WHERE r3.from_user = r1.to_user ) ;

  IF FOUND THEN
    -- dbg := 'H-';
    -- RAISE NOTICE 'Permissions to shared group % ', out_confers;
    RETURN dbg || out_confers;
  END IF;

  -- RAISE NOTICE 'No common group relationships between % and %', in_from, in_to;

  RETURN '';
END;
$_$;


ALTER FUNCTION public.get_permissions(integer, integer) OWNER TO davical_dba;

--
-- Name: get_usr_setting(integer, text); Type: FUNCTION; Schema: public; Owner: davical_dba
--

CREATE FUNCTION get_usr_setting(integer, text) RETURNS text
    LANGUAGE sql
    AS $_$SELECT setting_value FROM usr_setting
            WHERE usr_setting.user_no = $1
            AND usr_setting.setting_name = $2 $_$;


ALTER FUNCTION public.get_usr_setting(integer, text) OWNER TO davical_dba;

--
-- Name: grants_modified(); Type: FUNCTION; Schema: public; Owner: davical_dba
--

CREATE FUNCTION grants_modified() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
  old_to_principal INT8;
  new_is_group BOOL;
BEGIN
  -- in case we trigger on other events in future
  IF TG_OP = 'INSERT' THEN
    old_to_principal := NULL;
  ELSE
    old_to_principal := OLD.to_principal;
  END IF;
  IF TG_OP = 'INSERT' OR NEW.to_principal != old_to_principal THEN
    SELECT (type_id = 3) INTO new_is_group FROM principal WHERE principal_id = NEW.to_principal;
    IF NEW.is_group != new_is_group THEN
      NEW.is_group := new_is_group;
    END IF;
  END IF;
  RETURN NEW;
END;
$$;


ALTER FUNCTION public.grants_modified() OWNER TO davical_dba;

--
-- Name: grants_proxy_access_from_p(bigint, integer); Type: FUNCTION; Schema: public; Owner: davical_dba
--

CREATE FUNCTION grants_proxy_access_from_p(bigint, integer) RETURNS SETOF bigint
    LANGUAGE sql STABLE STRICT
    AS $_$
  SELECT DISTINCT by_principal
    FROM grants
   WHERE by_collection IS NULL AND by_principal != $1
     AND by_principal IN (SELECT expand_members(g2.to_principal,$2) FROM grants g2 WHERE g2.by_principal = $1)
   ;
$_$;


ALTER FUNCTION public.grants_proxy_access_from_p(bigint, integer) OWNER TO davical_dba;

--
-- Name: has_legacy_privilege(integer, text, integer); Type: FUNCTION; Schema: public; Owner: davical_dba
--

CREATE FUNCTION has_legacy_privilege(integer, text, integer) RETURNS boolean
    LANGUAGE plpgsql IMMUTABLE STRICT
    AS $_$
DECLARE
  in_from ALIAS FOR $1;
  in_legacy_privilege ALIAS FOR $2;
  in_to   ALIAS FOR $3;
  in_confers BIT(24);
  group_role_no INT;
BEGIN
  -- Self can always have full access
  IF in_from = in_to THEN
    RETURN TRUE;
  END IF;

  SELECT get_group_role_no() INTO group_role_no;
  SELECT legacy_privilege_to_bits(in_legacy_privilege) INTO in_confers;

  IF EXISTS(SELECT 1 FROM relationship WHERE from_user = in_from AND to_user = in_to
                      AND (in_confers & confers) = in_confers
                      AND NOT EXISTS(SELECT 1 FROM role_member WHERE to_user = user_no AND role_no = group_role_no) ) THEN
    -- A direct relationship from A to B that grants sufficient
    -- RAISE NOTICE 'Permissions directly granted';
    RETURN TRUE;
  END IF;

  IF EXISTS( SELECT 1 FROM relationship r1 JOIN relationship r2 ON r1.to_user=r2.from_user
         WHERE (in_confers & r1.confers & r2.confers) = in_confers
           AND r1.from_user=in_from AND r2.to_user=in_to
           AND r2.from_user IN (SELECT user_no FROM role_member WHERE role_no=group_role_no) ) THEN
    -- An indirect relationship from A to B via group G that grants sufficient
    -- RAISE NOTICE 'Permissions mediated via group';
    RETURN TRUE;
  END IF;

  IF EXISTS( SELECT 1 FROM relationship r1 JOIN relationship r2 ON r1.to_user=r2.to_user
         WHERE (in_confers & r1.confers & r2.confers) = in_confers
           AND r1.from_user=in_from AND r2.from_user=in_to
           AND r2.to_user IN (SELECT user_no FROM role_member WHERE role_no=group_role_no)
           AND NOT EXISTS(SELECT 1 FROM relationship WHERE from_user=r2.to_user) ) THEN
    -- An indirect reflexive relationship from both A & B to group G which grants sufficient
    -- RAISE NOTICE 'Permissions to shared group';
    RETURN TRUE;
  END IF;

  -- RAISE NOTICE 'No common group relationships between % and %', in_from, in_to;

  RETURN FALSE;
END;
$_$;


ALTER FUNCTION public.has_legacy_privilege(integer, text, integer) OWNER TO davical_dba;

--
-- Name: has_members_list(bigint); Type: FUNCTION; Schema: public; Owner: davical_dba
--

CREATE FUNCTION has_members_list(bigint) RETURNS text
    LANGUAGE plpgsql STRICT
    AS $_$
DECLARE
  in_member_id ALIAS FOR $1;
  m RECORD;
  mlist TEXT;
BEGIN
  mlist := '';
  FOR m IN SELECT displayname, group_id FROM group_member JOIN principal ON (member_id = principal_id)
                          WHERE group_id = in_member_id
  LOOP
    mlist := mlist
             || CASE WHEN mlist = '' THEN '' ELSE ', ' END
             || COALESCE( m.displayname, m.group_id::text);
  END LOOP;
  RETURN mlist;
END;
$_$;


ALTER FUNCTION public.has_members_list(bigint) OWNER TO davical_dba;

--
-- Name: icalendar_interval_to_sql(text); Type: FUNCTION; Schema: public; Owner: davical_dba
--

CREATE FUNCTION icalendar_interval_to_sql(text) RETURNS interval
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
  SELECT CASE WHEN substring($1,1,1) = '-' THEN -1 ELSE 1 END * regexp_replace( regexp_replace($1, '[PT-]', '', 'g'), '([A-Z])', E'\\1 ', 'g')::interval;
$_$;


ALTER FUNCTION public.icalendar_interval_to_sql(text) OWNER TO davical_dba;

--
-- Name: is_member_of_list(bigint); Type: FUNCTION; Schema: public; Owner: davical_dba
--

CREATE FUNCTION is_member_of_list(bigint) RETURNS text
    LANGUAGE plpgsql STRICT
    AS $_$
DECLARE
  in_member_id ALIAS FOR $1;
  m RECORD;
  mlist TEXT;
BEGIN
  mlist := '';
  FOR m IN SELECT displayname, group_id FROM group_member JOIN principal ON (group_id = principal_id)
                          WHERE member_id = in_member_id
  LOOP
    mlist := mlist
             || CASE WHEN mlist = '' THEN '' ELSE ', ' END
             || COALESCE( m.displayname, m.group_id::text);
  END LOOP;
  RETURN mlist;
END;
$_$;


ALTER FUNCTION public.is_member_of_list(bigint) OWNER TO davical_dba;

--
-- Name: legacy_get_permissions(integer, integer); Type: FUNCTION; Schema: public; Owner: davical_dba
--

CREATE FUNCTION legacy_get_permissions(integer, integer) RETURNS text
    LANGUAGE plpgsql IMMUTABLE STRICT
    AS $_$
DECLARE
  in_from ALIAS FOR $1;
  in_to   ALIAS FOR $2;
  out_confers TEXT;
  tmp_confers1 TEXT;
  tmp_confers2 TEXT;
  tmp_txt TEXT;
  dbg TEXT DEFAULT '';
  r RECORD;
  counter INT;
BEGIN
  -- Self can always have full access
  IF in_from = in_to THEN
    RETURN 'A';
  END IF;

  -- dbg := 'S-';
  SELECT rt1.confers INTO out_confers FROM relationship r1 JOIN relationship_type rt1 USING ( rt_id )
                    WHERE r1.from_user = in_from AND r1.to_user = in_to AND NOT usr_is_role(r1.to_user,'Group');
  IF FOUND THEN
    RETURN dbg || out_confers;
  END IF;
  -- RAISE NOTICE 'No simple relationships between % and %', in_from, in_to;

  out_confers := '';
  FOR r IN SELECT rt1.confers AS r1, rt2.confers AS r2 FROM relationship r1 JOIN relationship_type rt1 USING(rt_id)
              JOIN relationship r2 ON r1.to_user=r2.from_user JOIN relationship_type rt2 ON r2.rt_id=rt2.rt_id
         WHERE r1.from_user=in_from AND r2.to_user=in_to
           AND EXISTS( SELECT 1 FROM role_member JOIN roles USING(role_no) WHERE role_member.user_no=r1.to_user AND roles.role_name='Group')
           AND NOT EXISTS( SELECT 1 FROM role_member JOIN roles USING(role_no) WHERE role_member.user_no=r2.to_user AND roles.role_name='Group')
           AND NOT EXISTS( SELECT 1 FROM role_member JOIN roles USING(role_no) WHERE role_member.user_no=r1.from_user AND roles.role_name='Group')
  LOOP
    -- RAISE NOTICE 'Permissions to group % from group %', r.r1, r.r2;
    -- FIXME: This is an oversimplification
    -- dbg := 'C-';
    tmp_confers1 := r.r1;
    tmp_confers2 := r.r2;
    IF tmp_confers1 != tmp_confers2 THEN
      IF tmp_confers1 ~* 'A' THEN
        -- Ensure that A is expanded to all supported privs before being used as a mask
        tmp_confers1 := 'AFBRWU';
      END IF;
      IF tmp_confers2 ~* 'A' THEN
        -- Ensure that A is expanded to all supported privs before being used as a mask
        tmp_confers2 := 'AFBRWU';
      END IF;
      -- RAISE NOTICE 'Expanded permissions to group % from group %', tmp_confers1, tmp_confers2;
      tmp_txt = '';
      FOR counter IN 1 .. length(tmp_confers2) LOOP
        IF tmp_confers1 ~* substring(tmp_confers2,counter,1) THEN
          tmp_txt := tmp_txt || substring(tmp_confers2,counter,1);
        END IF;
      END LOOP;
      tmp_confers2 := tmp_txt;
    END IF;
    FOR counter IN 1 .. length(tmp_confers2) LOOP
      IF NOT out_confers ~* substring(tmp_confers2,counter,1) THEN
        out_confers := out_confers || substring(tmp_confers2,counter,1);
      END IF;
    END LOOP;
  END LOOP;
  IF out_confers ~* 'A' OR (out_confers ~* 'B' AND out_confers ~* 'F' AND out_confers ~* 'R' AND out_confers ~* 'W' AND out_confers ~* 'U') THEN
    out_confers := 'A';
  END IF;
  IF out_confers != '' THEN
    RETURN dbg || out_confers;
  END IF;

  -- RAISE NOTICE 'No complex relationships between % and %', in_from, in_to;

  SELECT rt1.confers INTO out_confers, tmp_confers1 FROM relationship r1 JOIN relationship_type rt1 ON ( r1.rt_id = rt1.rt_id )
              LEFT OUTER JOIN relationship r2 ON ( rt1.rt_id = r2.rt_id )
       WHERE r1.from_user = in_from AND r2.from_user = in_to AND r1.from_user != r2.from_user AND r1.to_user = r2.to_user
         AND NOT EXISTS( SELECT 1 FROM relationship r3 WHERE r3.from_user = r1.to_user )
          AND usr_is_role(r1.to_user,'Group');

  IF FOUND THEN
    -- dbg := 'H-';
    -- RAISE NOTICE 'Permissions to shared group % ', out_confers;
    RETURN dbg || out_confers;
  END IF;

  -- RAISE NOTICE 'No common group relationships between % and %', in_from, in_to;

  RETURN '';
END;
$_$;


ALTER FUNCTION public.legacy_get_permissions(integer, integer) OWNER TO davical_dba;

--
-- Name: legacy_privilege_to_bits(text); Type: FUNCTION; Schema: public; Owner: davical_dba
--

CREATE FUNCTION legacy_privilege_to_bits(text) RETURNS bit
    LANGUAGE plpgsql IMMUTABLE STRICT
    AS $_$
DECLARE
  in_priv ALIAS FOR $1;
  out_bits BIT(24);
BEGIN
  out_bits := 0::BIT(24);
  IF in_priv ~* 'A' THEN
    out_bits = ~ out_bits;
    RETURN out_bits;
  END IF;

  -- The CALDAV:read-free-busy privilege MUST be aggregated in the DAV:read privilege.
  --    1 DAV:read
  --  512 CalDAV:read-free-busy
  -- 4096 CALDAV:schedule-query-freebusy
  IF in_priv ~* 'R' THEN
    out_bits := out_bits | 4609::BIT(24);
  END IF;

  -- DAV:write => DAV:write MUST contain DAV:bind, DAV:unbind, DAV:write-properties and DAV:write-content
  --    2 DAV:write-properties
  --    4 DAV:write-content
  --   64 DAV:bind
  --  128 DAV:unbind
  IF in_priv ~* 'W' THEN
    out_bits := out_bits |   198::BIT(24);
  END IF;

  --   64 DAV:bind
  IF in_priv ~* 'B' THEN
    out_bits := out_bits | 64::BIT(24);
  END IF;

  --  128 DAV:unbind
  IF in_priv ~* 'U' THEN
    out_bits := out_bits | 128::BIT(24);
  END IF;

  --  512 CalDAV:read-free-busy
  -- 4096 CALDAV:schedule-query-freebusy
  IF in_priv ~* 'F' THEN
    out_bits := out_bits | 4608::BIT(24);
  END IF;

  RETURN out_bits;
END
$_$;


ALTER FUNCTION public.legacy_privilege_to_bits(text) OWNER TO davical_dba;

--
-- Name: max_roles(); Type: FUNCTION; Schema: public; Owner: davical_dba
--

CREATE FUNCTION max_roles() RETURNS integer
    LANGUAGE sql
    AS $$SELECT max(role_no) FROM roles$$;


ALTER FUNCTION public.max_roles() OWNER TO davical_dba;

--
-- Name: max_session(); Type: FUNCTION; Schema: public; Owner: davical_dba
--

CREATE FUNCTION max_session() RETURNS integer
    LANGUAGE sql
    AS $$SELECT max(session_id) FROM session$$;


ALTER FUNCTION public.max_session() OWNER TO davical_dba;

--
-- Name: max_usr(); Type: FUNCTION; Schema: public; Owner: davical_dba
--

CREATE FUNCTION max_usr() RETURNS integer
    LANGUAGE sql
    AS $$SELECT max(user_no) FROM usr$$;


ALTER FUNCTION public.max_usr() OWNER TO davical_dba;

--
-- Name: monthly_set(timestamp with time zone, rrule_parts); Type: FUNCTION; Schema: public; Owner: davical_dba
--

CREATE FUNCTION monthly_set(timestamp with time zone, rrule_parts) RETURNS SETOF timestamp with time zone
    LANGUAGE plpgsql IMMUTABLE STRICT
    AS $_$
DECLARE
  after ALIAS FOR $1;
  rrule ALIAS FOR $2;
  valid_date TIMESTAMP WITH TIME ZONE;
  curse REFCURSOR;
  setpos INT;
  i INT;
BEGIN

  /**
  * Need to investigate whether it is legal to set both of these, and whether
  * we are correct to UNION the results, or whether we should INTERSECT them.
  * So at this point, we refer to the specification, which grants us this
  * wonderfully enlightening vision:
  *
  *     If multiple BYxxx rule parts are specified, then after evaluating the
  *     specified FREQ and INTERVAL rule parts, the BYxxx rule parts are
  *     applied to the current set of evaluated occurrences in the following
  *     order: BYMONTH, BYWEEKNO, BYYEARDAY, BYMONTHDAY, BYDAY, BYHOUR,
  *     BYMINUTE, BYSECOND and BYSETPOS; then COUNT and UNTIL are evaluated.
  *
  * My guess is that this means 'INTERSECT'
  */
  IF rrule.byday IS NOT NULL AND rrule.bymonthday IS NOT NULL THEN
    OPEN curse SCROLL FOR SELECT r FROM rrule_month_byday_set(after, rrule.byday ) r
                INTERSECT SELECT r FROM rrule_month_bymonthday_set(after, rrule.bymonthday ) r
                    ORDER BY 1;
  ELSIF rrule.bymonthday IS NOT NULL THEN
    OPEN curse SCROLL FOR SELECT r FROM rrule_month_bymonthday_set(after, rrule.bymonthday ) r ORDER BY 1;
  ELSE
    OPEN curse SCROLL FOR SELECT r FROM rrule_month_byday_set(after, rrule.byday ) r ORDER BY 1;
  END IF;

  RETURN QUERY SELECT d FROM rrule_bysetpos_filter(curse,rrule.bysetpos) d;

END;
$_$;


ALTER FUNCTION public.monthly_set(timestamp with time zone, rrule_parts) OWNER TO davical_dba;

--
-- Name: new_db_revision(integer, integer, integer, text); Type: FUNCTION; Schema: public; Owner: davical_dba
--

CREATE FUNCTION new_db_revision(integer, integer, integer, text) RETURNS void
    LANGUAGE plpgsql
    AS $_$
   DECLARE
      major ALIAS FOR $1;
      minor ALIAS FOR $2;
      patch ALIAS FOR $3;
      blurb ALIAS FOR $4;
      new_id INT;
   BEGIN
      SELECT MAX(schema_id) + 1 INTO new_id FROM awl_db_revision;
      IF NOT FOUND OR new_id IS NULL THEN
        new_id := 1;
      END IF;
      INSERT INTO awl_db_revision (schema_id, schema_major, schema_minor, schema_patch, schema_name)
                    VALUES( new_id, major, minor, patch, blurb );
      RETURN;
   END;
$_$;


ALTER FUNCTION public.new_db_revision(integer, integer, integer, text) OWNER TO davical_dba;

--
-- Name: new_sync_token(bigint, bigint); Type: FUNCTION; Schema: public; Owner: davical_dba
--

CREATE FUNCTION new_sync_token(bigint, bigint) RETURNS bigint
    LANGUAGE plpgsql STRICT
    AS $_$
DECLARE
  in_old_sync_token ALIAS FOR $1;
  in_collection_id ALIAS FOR $2;
  tmp_int INT8;
  new_token sync_tokens.sync_token%TYPE;
  old_modification_time sync_tokens.modification_time%TYPE;
BEGIN
  IF in_old_sync_token > 0 THEN
    SELECT modification_time INTO old_modification_time FROM sync_tokens
           WHERE sync_token = in_old_sync_token AND collection_id = in_collection_id;
    IF NOT FOUND THEN
      -- They are in an inconsistent state: we return NULL so they can re-start the process
      RETURN NULL;
    END IF;
  END IF;
  
  -- Find the most recent sync_token
  SELECT sync_token, modification_time INTO new_token, old_modification_time FROM sync_tokens
         WHERE collection_id = in_collection_id ORDER BY modification_time DESC LIMIT 1;
  IF FOUND THEN
    SELECT 1 INTO tmp_int FROM sync_changes WHERE collection_id = in_collection_id AND sync_time > old_modification_time LIMIT 1;
    IF NOT FOUND THEN
      -- Return the latest sync_token we have for this collection, since there are no changes.
      RETURN new_token;
    END IF;
  END IF;
  
  -- Looks like we need a new sync_token for this collection...
  SELECT nextval('sync_tokens_sync_token_seq') INTO new_token;
  INSERT INTO sync_tokens(collection_id, sync_token) VALUES( in_collection_id, new_token );
  
  -- Having created our new token we do some clean-up of old tokens
  SELECT modification_time, sync_token INTO old_modification_time, tmp_int FROM sync_tokens
  		WHERE collection_id = in_collection_id AND modification_time < (current_timestamp - '7 days'::interval)
  		ORDER BY collection_id, modification_time DESC;
  DELETE FROM sync_changes WHERE collection_id = in_collection_id AND sync_time < old_modification_time;
  DELETE FROM sync_tokens WHERE collection_id = in_collection_id AND sync_token < tmp_int;
  
  -- Returning the new token
  RETURN new_token;
END
$_$;


ALTER FUNCTION public.new_sync_token(bigint, bigint) OWNER TO davical_dba;

--
-- Name: p_has_proxy_access_to(bigint, integer); Type: FUNCTION; Schema: public; Owner: davical_dba
--

CREATE FUNCTION p_has_proxy_access_to(bigint, integer) RETURNS SETOF bigint
    LANGUAGE sql STABLE STRICT
    AS $_$
  SELECT by_principal
    FROM (
      SELECT by_principal FROM grants
           WHERE to_principal IN (SELECT $1 UNION SELECT expand_memberships($1,$2))
             AND (privileges & 5::BIT(24)) != 0::BIT(24)
             AND by_collection IS NULL
             AND by_principal != $1
      UNION
      SELECT principal_id AS by_principal FROM principal
           WHERE (default_privileges & 5::BIT(24)) != 0::BIT(24)
             AND principal_id != $1
    ) subquery;
$_$;


ALTER FUNCTION public.p_has_proxy_access_to(bigint, integer) OWNER TO davical_dba;

--
-- Name: parse_rrule_parts(timestamp with time zone, text); Type: FUNCTION; Schema: public; Owner: davical_dba
--

CREATE FUNCTION parse_rrule_parts(timestamp with time zone, text) RETURNS rrule_parts
    LANGUAGE plpgsql IMMUTABLE STRICT
    AS $_$
DECLARE
  basedate   ALIAS FOR $1;
  repeatrule ALIAS FOR $2;
  result rrule_parts%ROWTYPE;
  tempstr TEXT;
BEGIN
  result.base       := basedate;
  result.until      := substring(repeatrule from 'UNTIL=([0-9TZ]+)(;|$)');
  result.freq       := substring(repeatrule from 'FREQ=([A-Z]+)(;|$)');
  result.count      := substring(repeatrule from 'COUNT=([0-9]+)(;|$)');
  result.interval   := COALESCE(substring(repeatrule from 'INTERVAL=([0-9]+)(;|$)')::int, 1);
  result.wkst       := substring(repeatrule from 'WKST=(MO|TU|WE|TH|FR|SA|SU)(;|$)');

  result.byday      := string_to_array( substring(repeatrule from 'BYDAY=(([+-]?[0-9]{0,2}(MO|TU|WE|TH|FR|SA|SU),?)+)(;|$)'), ',');

  result.byyearday  := string_to_array(substring(repeatrule from 'BYYEARDAY=([0-9,+-]+)(;|$)'), ',');
  result.byweekno   := string_to_array(substring(repeatrule from 'BYWEEKNO=([0-9,+-]+)(;|$)'), ',');
  result.bymonthday := string_to_array(substring(repeatrule from 'BYMONTHDAY=([0-9,+-]+)(;|$)'), ',');
  result.bymonth    := string_to_array(substring(repeatrule from 'BYMONTH=(([+-]?[0-1]?[0-9],?)+)(;|$)'), ',');
  result.bysetpos   := string_to_array(substring(repeatrule from 'BYSETPOS=(([+-]?[0-9]{1,3},?)+)(;|$)'), ',');

  result.bysecond   := string_to_array(substring(repeatrule from 'BYSECOND=([0-9,]+)(;|$)'), ',');
  result.byminute   := string_to_array(substring(repeatrule from 'BYMINUTE=([0-9,]+)(;|$)'), ',');
  result.byhour     := string_to_array(substring(repeatrule from 'BYHOUR=([0-9,]+)(;|$)'), ',');

  RETURN result;
END;
$_$;


ALTER FUNCTION public.parse_rrule_parts(timestamp with time zone, text) OWNER TO davical_dba;

--
-- Name: path_privs(bigint, text, integer); Type: FUNCTION; Schema: public; Owner: davical_dba
--

CREATE FUNCTION path_privs(bigint, text, integer) RETURNS bit
    LANGUAGE plpgsql STABLE STRICT
    AS $_$
DECLARE
  in_accessor ALIAS FOR $1;
  in_path  ALIAS FOR $2;
  in_depth    ALIAS FOR $3;

  alt1_path TEXT;
  alt2_path TEXT;
  grantor_collection    INT8;
  grantor_principal     INT8;
  collection_path       TEXT;
  collection_privileges BIT(24);
  out_conferred         BIT(24);
BEGIN
  out_conferred := 0::BIT(24);

  IF in_path ~ '^/?$' THEN
    -- RAISE NOTICE 'Collection is root: Collection: %', in_path;
    RETURN 1; -- basic read privileges on root directory
  END IF;

  -- We need to canonicalise the path, so:
  -- If it matches '/' + some characters (+ optional '/')  => a principal URL
  IF in_path ~ '^/[^/]+/?$' THEN
    alt1_path := replace(in_path, '/', '');
    SELECT pprivs(in_accessor,principal_id, in_depth) INTO out_conferred FROM usr JOIN principal USING(user_no) WHERE username = alt1_path;
    -- RAISE NOTICE 'Path is Principal: Principal: %, Collection: %, Permissions: %', in_accessor, in_path, out_conferred;
    RETURN out_conferred;
  END IF;

  -- Otherwise look for the longest segment matching up to the last '/', or if we append one, or if we replace a final '.ics' with one.
  alt1_path := in_path;
  IF alt1_path ~ E'\\.ics$' THEN
    alt1_path := substr(alt1_path, 1, length(alt1_path) - 4) || '/';
  END IF;
  alt2_path := regexp_replace( in_path, '[^/]*$', '');
  SELECT collection.collection_id, grantor.principal_id, collection.dav_name, collection.default_privileges
    INTO grantor_collection, grantor_principal, collection_path, collection_privileges
                      FROM collection JOIN principal grantor USING (user_no)
                      WHERE dav_name = in_path || '/' OR dav_name = alt1_path OR dav_name = alt2_path
                      ORDER BY LENGTH(collection.dav_name) DESC LIMIT 1;

  -- Self will always need full access to their own collections!
  IF grantor_principal = in_accessor THEN
    -- RAISE NOTICE 'Principal IS owner: Principal: %, Collection: %', in_accessor, in_path;
    RETURN ~ out_conferred;
  END IF;

  SELECT bit_or(privileges) INTO out_conferred FROM grants
                   WHERE by_collection = grantor_collection
                     AND (to_principal=in_accessor OR to_principal IN (SELECT expand_memberships(in_accessor,in_depth)));

  IF out_conferred IS NULL THEN
    IF collection_privileges IS NULL THEN
      IF grantor_principal IS NULL THEN
        alt1_path := regexp_replace( in_path, '/[^/]+/?$', '/');
        SELECT path_privs(in_accessor,alt1_path,in_depth) INTO out_conferred;
        -- RAISE NOTICE 'Collection is NULL: Principal: %, Collection: %, Permissions: %', in_accessor, in_path, out_conferred;
      ELSE
        SELECT pprivs(in_accessor,grantor_principal,in_depth) INTO out_conferred;
        -- RAISE NOTICE 'Collection priveleges are NULL: Principal: %, Collection: %, Permissions: %', in_accessor, in_path, out_conferred;
      END IF;
    ELSE
      out_conferred := collection_privileges;
      -- RAISE NOTICE 'Default Collection priveleges apply: Principal: %, Collection: %, Permissions: %', in_accessor, in_path, out_conferred;
    END IF;
  END IF;

  RETURN out_conferred;
END;
$_$;


ALTER FUNCTION public.path_privs(bigint, text, integer) OWNER TO davical_dba;

--
-- Name: pprivs(bigint, bigint, integer); Type: FUNCTION; Schema: public; Owner: davical_dba
--

CREATE FUNCTION pprivs(bigint, bigint, integer) RETURNS bit
    LANGUAGE plpgsql STABLE STRICT
    AS $_$
DECLARE
  in_accessor ALIAS FOR $1;
  in_grantor  ALIAS FOR $2;
  in_depth    ALIAS FOR $3;
  out_conferred BIT(24);
BEGIN
  out_conferred := 0::BIT(24);
  -- Self can always have full access
  IF in_grantor = in_accessor THEN
    RETURN ~ out_conferred;
  END IF;

  SELECT bit_or(subquery.privileges) INTO out_conferred FROM
    (
      SELECT privileges FROM grants WHERE by_principal=in_grantor AND by_collection IS NULL
                                          AND (to_principal=in_accessor OR to_principal IN (SELECT expand_memberships(in_accessor,in_depth)))
            UNION
      SELECT bit_or(sq2.privileges) FROM
      (
          SELECT 32::BIT(24) AS privileges FROM expand_memberships(in_accessor,in_depth) WHERE expand_memberships = in_grantor
      			UNION
          SELECT default_privileges AS privileges FROM principal WHERE principal_id = in_grantor
      ) AS sq2
    ) AS subquery ;

  IF out_conferred IS NULL THEN
    SELECT default_privileges INTO out_conferred FROM principal WHERE principal_id = in_grantor;
  END IF;

  RETURN out_conferred;
END;
$_$;


ALTER FUNCTION public.pprivs(bigint, bigint, integer) OWNER TO davical_dba;

--
-- Name: principal_modified(); Type: FUNCTION; Schema: public; Owner: davical_dba
--

CREATE FUNCTION principal_modified() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
BEGIN
  -- in case we trigger on other events in future
  IF TG_OP = 'UPDATE' THEN
    IF NEW.type_id != OLD.type_id THEN
      UPDATE grants
        SET is_group = (NEW.type_id = 3)
      WHERE grants.to_principal = NEW.principal_id;
    END IF;
  END IF;
  RETURN NEW;
END;
$$;


ALTER FUNCTION public.principal_modified() OWNER TO davical_dba;

--
-- Name: privilege_to_bits(text); Type: FUNCTION; Schema: public; Owner: davical_dba
--

CREATE FUNCTION privilege_to_bits(text) RETURNS bit
    LANGUAGE plpgsql IMMUTABLE STRICT
    AS $_$
DECLARE
  raw_priv ALIAS FOR $1;
  in_priv TEXT;
BEGIN
  in_priv := trim(lower(regexp_replace(raw_priv, '^.*:', '')));
  IF in_priv = 'all' THEN
    RETURN ~ 0::BIT(24);
  END IF;

  RETURN (CASE
            WHEN in_priv = 'read'                            THEN  4609 -- 1 + 512 + 4096
            WHEN in_priv = 'write'                           THEN   198 -- 2 + 4 + 64 + 128
            WHEN in_priv = 'write-properties'                THEN     2
            WHEN in_priv = 'write-content'                   THEN     4
            WHEN in_priv = 'unlock'                          THEN     8
            WHEN in_priv = 'read-acl'                        THEN    16
            WHEN in_priv = 'read-current-user-privilege-set' THEN    32
            WHEN in_priv = 'bind'                            THEN    64
            WHEN in_priv = 'unbind'                          THEN   128
            WHEN in_priv = 'write-acl'                       THEN   256
            WHEN in_priv = 'read-free-busy'                  THEN  4608 --  512 + 4096
            WHEN in_priv = 'schedule-deliver'                THEN  7168 -- 1024 + 2048 + 4096
            WHEN in_priv = 'schedule-deliver-invite'         THEN  1024
            WHEN in_priv = 'schedule-deliver-reply'          THEN  2048
            WHEN in_priv = 'schedule-query-freebusy'         THEN  4096
            WHEN in_priv = 'schedule-send'                   THEN 57344 -- 8192 + 16384 + 32768
            WHEN in_priv = 'schedule-send-invite'            THEN  8192
            WHEN in_priv = 'schedule-send-reply'             THEN 16384
            WHEN in_priv = 'schedule-send-freebusy'          THEN 32768
          ELSE 0 END)::BIT(24);
END
$_$;


ALTER FUNCTION public.privilege_to_bits(text) OWNER TO davical_dba;

--
-- Name: privilege_to_bits(text[]); Type: FUNCTION; Schema: public; Owner: davical_dba
--

CREATE FUNCTION privilege_to_bits(text[]) RETURNS bit
    LANGUAGE plpgsql IMMUTABLE STRICT
    AS $_$
DECLARE
  raw_privs ALIAS FOR $1;
  in_priv TEXT;
  out_bits BIT(24);
  i INT;
  all_privs BIT(24);
  start INT;
  finish INT;
BEGIN
  out_bits := 0::BIT(24);
  all_privs := ~ out_bits;
  SELECT array_lower(raw_privs,1) INTO start;
  SELECT array_upper(raw_privs,1) INTO finish;
  FOR i IN start .. finish  LOOP
    SELECT out_bits | privilege_to_bits(raw_privs[i]) INTO out_bits;
    IF out_bits = 65535::BIT(24) THEN
      RETURN all_privs;
    END IF;
  END LOOP;
  RETURN out_bits;
END
$_$;


ALTER FUNCTION public.privilege_to_bits(text[]) OWNER TO davical_dba;

--
-- Name: privileges_list(bit); Type: FUNCTION; Schema: public; Owner: davical_dba
--

CREATE FUNCTION privileges_list(bit) RETURNS text
    LANGUAGE plpgsql IMMUTABLE STRICT
    AS $_$
DECLARE
  in_privileges ALIAS FOR $1;
  privileges TEXT[];
  plist TEXT;
  start INT;
  finish INT;
  i INT;
BEGIN
  plist := '';

  privileges := bits_to_privilege(in_privileges);
  SELECT array_lower(privileges,1) INTO start;
  IF start IS NOT NULL THEN
    SELECT array_upper(privileges,1) INTO finish;
    FOR i IN start .. finish  LOOP
      plist := plist
              || CASE WHEN plist = '' THEN '' ELSE ', ' END
              || privileges[i];
    END LOOP;
  END IF;
  RETURN plist;
END;
$_$;


ALTER FUNCTION public.privileges_list(bit) OWNER TO davical_dba;

--
-- Name: real_path_exists(text); Type: FUNCTION; Schema: public; Owner: davical_dba
--

CREATE FUNCTION real_path_exists(text) RETURNS boolean
    LANGUAGE plpgsql
    AS $_$
DECLARE
  in_path ALIAS FOR $1;
  tmp BOOLEAN;
BEGIN
  IF in_path = '/' THEN
    RETURN TRUE;
  END IF;
  IF in_path ~ '^/[^/]+/$' THEN
    SELECT TRUE INTO tmp FROM usr WHERE username = substring( in_path from 2 for length(in_path) - 2);
    IF FOUND THEN
      RETURN TRUE;
    END IF;
  ELSE
    IF in_path ~ '^/.*/$' THEN
      SELECT TRUE INTO tmp FROM collection WHERE dav_name = in_path;
      IF FOUND THEN
        RETURN TRUE;
      END IF;
    END IF;
  END IF;
  RETURN FALSE;
END;
$_$;


ALTER FUNCTION public.real_path_exists(text) OWNER TO davical_dba;

--
-- Name: relationship_list(bigint); Type: FUNCTION; Schema: public; Owner: davical_dba
--

CREATE FUNCTION relationship_list(bigint) RETURNS text
    LANGUAGE plpgsql
    AS $_$
DECLARE
  user ALIAS FOR $1;
  r RECORD;
  rlist TEXT;
BEGIN
  rlist := '';
  FOR r IN SELECT rt_name, fullname FROM relationship
                          LEFT JOIN relationship_type USING(rt_id) LEFT JOIN usr tgt ON to_user = tgt.user_no
                          WHERE from_user = user
  LOOP
    rlist := rlist
             || CASE WHEN rlist = '' THEN '' ELSE ', ' END
             || r.rt_name || '(' || r.fullname || ')';
  END LOOP;
  RETURN rlist;
END;
$_$;


ALTER FUNCTION public.relationship_list(bigint) OWNER TO davical_dba;

--
-- Name: rrule_bysetpos_filter(refcursor, integer[]); Type: FUNCTION; Schema: public; Owner: davical_dba
--

CREATE FUNCTION rrule_bysetpos_filter(refcursor, integer[]) RETURNS SETOF timestamp with time zone
    LANGUAGE plpgsql IMMUTABLE
    AS $_$
DECLARE
  curse ALIAS FOR $1;
  bysetpos ALIAS FOR $2;
  valid_date TIMESTAMP WITH TIME ZONE;
  i INT;
BEGIN

  IF bysetpos IS NULL THEN
    LOOP
      FETCH curse INTO valid_date;
      EXIT WHEN NOT FOUND;
      RETURN NEXT valid_date;
    END LOOP;
  ELSE
    FOR i IN 1..366 LOOP
      EXIT WHEN bysetpos[i] IS NULL;
      IF bysetpos[i] > 0 THEN
        FETCH ABSOLUTE bysetpos[i] FROM curse INTO valid_date;
      ELSE
        MOVE LAST IN curse;
        FETCH RELATIVE (bysetpos[i] + 1) FROM curse INTO valid_date;
      END IF;
      IF valid_date IS NOT NULL THEN
        RETURN NEXT valid_date;
      END IF;
    END LOOP;
  END IF;
  CLOSE curse;
END;
$_$;


ALTER FUNCTION public.rrule_bysetpos_filter(refcursor, integer[]) OWNER TO davical_dba;

--
-- Name: rrule_event_instances(timestamp with time zone, text); Type: FUNCTION; Schema: public; Owner: davical_dba
--

CREATE FUNCTION rrule_event_instances(timestamp with time zone, text) RETURNS SETOF rrule_instance
    LANGUAGE plpgsql IMMUTABLE STRICT
    AS $_$
DECLARE
  basedate ALIAS FOR $1;
  repeatrule ALIAS FOR $2;
  maxdate TIMESTAMP WITH TIME ZONE;
  current TIMESTAMP WITH TIME ZONE;
  result rrule_instance%ROWTYPE;
BEGIN
  maxdate := current_date + '10 years'::interval;

  result.dtstart := basedate;
  result.rrule   := repeatrule;

  FOR current IN SELECT d FROM rrule_event_instances_range( basedate, repeatrule, basedate, maxdate, 300 ) d LOOP
    result.instance := current;
    RETURN NEXT result;
  END LOOP;

END;
$_$;


ALTER FUNCTION public.rrule_event_instances(timestamp with time zone, text) OWNER TO davical_dba;

--
-- Name: rrule_event_instances_range(timestamp with time zone, text, timestamp with time zone, timestamp with time zone, integer); Type: FUNCTION; Schema: public; Owner: davical_dba
--

CREATE FUNCTION rrule_event_instances_range(timestamp with time zone, text, timestamp with time zone, timestamp with time zone, integer) RETURNS SETOF timestamp with time zone
    LANGUAGE plpgsql IMMUTABLE STRICT
    AS $_$
DECLARE
  basedate ALIAS FOR $1;
  repeatrule ALIAS FOR $2;
  mindate ALIAS FOR $3;
  maxdate ALIAS FOR $4;
  max_count ALIAS FOR $5;
  loopmax INT;
  loopcount INT;
  base_day TIMESTAMP WITH TIME ZONE;
  current_base TIMESTAMP WITH TIME ZONE;
  current TIMESTAMP WITH TIME ZONE;
  rrule rrule_parts%ROWTYPE;
BEGIN
  loopcount := 0;

  SELECT * INTO rrule FROM parse_rrule_parts( basedate, repeatrule );

  IF rrule.count IS NOT NULL THEN
    loopmax := rrule.count;
  ELSE
    -- max_count is pretty arbitrary, so we scale it somewhat here depending on the frequency.
    IF rrule.freq = 'DAILY' THEN
      loopmax := max_count * 20;
    ELSIF rrule.freq = 'WEEKLY' THEN
      loopmax := max_count * 10;
    ELSE
      loopmax := max_count;
    END IF;
  END IF;

  current_base := basedate;
  base_day := date_trunc('day',basedate);
  WHILE loopcount < loopmax AND current_base <= maxdate LOOP
    IF rrule.freq = 'DAILY' THEN
      FOR current IN SELECT d FROM daily_set(current_base,rrule) d WHERE d >= base_day LOOP
--        IF test_byday_rule(current,rrule.byday) AND test_bymonthday_rule(current,rrule.bymonthday) AND test_bymonth_rule(current,rrule.bymonth) THEN
          EXIT WHEN rrule.until IS NOT NULL AND current > rrule.until;
          IF current >= mindate THEN
            RETURN NEXT current;
          END IF;
          loopcount := loopcount + 1;
          EXIT WHEN loopcount >= loopmax;
--        END IF;
      END LOOP;
      current_base := current_base + (rrule.interval::text || ' days')::interval;
    ELSIF rrule.freq = 'WEEKLY' THEN
      FOR current IN SELECT w FROM weekly_set(current_base,rrule) w WHERE w >= base_day LOOP
        IF test_byyearday_rule(current,rrule.byyearday)
               AND test_bymonthday_rule(current,rrule.bymonthday)
               AND test_bymonth_rule(current,rrule.bymonth)
        THEN
          EXIT WHEN rrule.until IS NOT NULL AND current > rrule.until;
          IF current >= mindate THEN
            RETURN NEXT current;
          END IF;
          loopcount := loopcount + 1;
          EXIT WHEN loopcount >= loopmax;
        END IF;
      END LOOP;
      current_base := current_base + (rrule.interval::text || ' weeks')::interval;
    ELSIF rrule.freq = 'MONTHLY' THEN
      FOR current IN SELECT m FROM monthly_set(current_base,rrule) m WHERE m >= base_day LOOP
--        IF /* test_byyearday_rule(current,rrule.byyearday)
--               AND */ test_bymonth_rule(current,rrule.bymonth)
--        THEN
          EXIT WHEN rrule.until IS NOT NULL AND current > rrule.until;
          IF current >= mindate THEN
            RETURN NEXT current;
          END IF;
          loopcount := loopcount + 1;
          EXIT WHEN loopcount >= loopmax;
--        END IF;
      END LOOP;
      current_base := current_base + (rrule.interval::text || ' months')::interval;
    ELSIF rrule.freq = 'YEARLY' THEN
      FOR current IN SELECT y FROM yearly_set(current_base,rrule) y WHERE y >= base_day LOOP
        EXIT WHEN rrule.until IS NOT NULL AND current > rrule.until;
        IF current >= mindate THEN
          RETURN NEXT current;
        END IF;
        loopcount := loopcount + 1;
        EXIT WHEN loopcount >= loopmax;
      END LOOP;
      current_base := current_base + (rrule.interval::text || ' years')::interval;
    ELSE
      RAISE NOTICE 'A frequency of "%" is not handled', rrule.freq;
      RETURN;
    END IF;
    EXIT WHEN rrule.until IS NOT NULL AND current > rrule.until;
  END LOOP;
  -- RETURN QUERY;
END;
$_$;


ALTER FUNCTION public.rrule_event_instances_range(timestamp with time zone, text, timestamp with time zone, timestamp with time zone, integer) OWNER TO davical_dba;

--
-- Name: rrule_event_overlaps(timestamp with time zone, timestamp with time zone, text, timestamp with time zone, timestamp with time zone); Type: FUNCTION; Schema: public; Owner: davical_dba
--

CREATE FUNCTION rrule_event_overlaps(timestamp with time zone, timestamp with time zone, text, timestamp with time zone, timestamp with time zone) RETURNS boolean
    LANGUAGE plpgsql IMMUTABLE
    AS $_$
DECLARE
  dtstart ALIAS FOR $1;
  dtend ALIAS FOR $2;
  repeatrule ALIAS FOR $3;
  in_mindate ALIAS FOR $4;
  in_maxdate ALIAS FOR $5;
  base_date TIMESTAMP WITH TIME ZONE;
  mindate TIMESTAMP WITH TIME ZONE;
  maxdate TIMESTAMP WITH TIME ZONE;
BEGIN

  IF dtstart IS NULL THEN
    RETURN NULL;
  END IF;
  IF dtend IS NULL THEN
    base_date := dtstart;
  ELSE
    base_date := dtend;
  END IF;

  IF in_mindate IS NULL THEN
    mindate := current_date - '10 years'::interval;
  ELSE
    mindate := in_mindate;
  END IF;

  IF in_maxdate IS NULL THEN
    maxdate := current_date + '10 years'::interval;
  ELSE
    -- If we add the duration onto the event, then an overlap occurs if dtend <= increased end of range.
    maxdate := in_maxdate + (base_date - dtstart);
  END IF;

  IF repeatrule IS NULL THEN
    RETURN (dtstart <= maxdate AND base_date >= mindate);
  END IF;

  SELECT d INTO mindate FROM rrule_event_instances_range( base_date, repeatrule, mindate, maxdate, 60 ) d LIMIT 1;
  RETURN FOUND;

END;
$_$;


ALTER FUNCTION public.rrule_event_overlaps(timestamp with time zone, timestamp with time zone, text, timestamp with time zone, timestamp with time zone) OWNER TO davical_dba;

--
-- Name: rrule_month_byday_set(timestamp with time zone, text[]); Type: FUNCTION; Schema: public; Owner: davical_dba
--

CREATE FUNCTION rrule_month_byday_set(timestamp with time zone, text[]) RETURNS SETOF timestamp with time zone
    LANGUAGE plpgsql IMMUTABLE
    AS $_$
DECLARE
  in_time ALIAS FOR $1;
  byday ALIAS FOR $2;
  dayrule TEXT;
  i INT;
  dow INT;
  index INT;
  first_dow INT;
  each_day TIMESTAMP WITH TIME ZONE;
  this_month INT;
  results TIMESTAMP WITH TIME ZONE[];
BEGIN

  IF byday IS NULL THEN
    -- We still return the single date as a SET
    RETURN NEXT in_time;
    RETURN;
  END IF;

  i := 1;
  dayrule := byday[i];
  WHILE dayrule IS NOT NULL LOOP
    dow := position(substring( dayrule from '..$') in 'SUMOTUWETHFRSA') / 2;
    each_day := date_trunc( 'month', in_time ) + (in_time::time)::interval;
    this_month := date_part( 'month', in_time );
    first_dow := date_part( 'dow', each_day );

    -- Coerce each_day to be the first 'dow' of the month
    each_day := each_day - ( first_dow::text || 'days')::interval
                        + ( dow::text || 'days')::interval
                        + CASE WHEN dow < first_dow THEN '1 week'::interval ELSE '0s'::interval END;

    -- RAISE NOTICE 'From "%", for % finding dates. dow=%, this_month=%, first_dow=%', each_day, dayrule, dow, this_month, first_dow;
    IF length(dayrule) > 2 THEN
      index := (substring(dayrule from '^[0-9-]+'))::int;

      IF index = 0 THEN
        RAISE NOTICE 'Ignored invalid BYDAY rule part "%".', bydayrule;
      ELSIF index > 0 THEN
        -- The simplest case, such as 2MO for the second monday
        each_day := each_day + ((index - 1)::text || ' weeks')::interval;
      ELSE
        each_day := each_day + '5 weeks'::interval;
        WHILE date_part('month', each_day) != this_month LOOP
          each_day := each_day - '1 week'::interval;
        END LOOP;
        -- Note that since index is negative, (-2 + 1) == -1, for example
        index := index + 1;
        IF index < 0 THEN
          each_day := each_day + (index::text || ' weeks')::interval ;
        END IF;
      END IF;

      -- Sometimes (e.g. 5TU or -5WE) there might be no such date in some months
      IF date_part('month', each_day) = this_month THEN
        results[date_part('day',each_day)] := each_day;
        -- RAISE NOTICE 'Added "%" to list for %', each_day, dayrule;
      END IF;

    ELSE
      -- Return all such days that are within the given month
      WHILE date_part('month', each_day) = this_month LOOP
        results[date_part('day',each_day)] := each_day;
        each_day := each_day + '1 week'::interval;
        -- RAISE NOTICE 'Added "%" to list for %', each_day, dayrule;
      END LOOP;
    END IF;

    i := i + 1;
    dayrule := byday[i];
  END LOOP;

  FOR i IN 1..31 LOOP
    IF results[i] IS NOT NULL THEN
      RETURN NEXT results[i];
    END IF;
  END LOOP;

  RETURN;

END;
$_$;


ALTER FUNCTION public.rrule_month_byday_set(timestamp with time zone, text[]) OWNER TO davical_dba;

--
-- Name: rrule_month_bymonthday_set(timestamp with time zone, integer[]); Type: FUNCTION; Schema: public; Owner: davical_dba
--

CREATE FUNCTION rrule_month_bymonthday_set(timestamp with time zone, integer[]) RETURNS SETOF timestamp with time zone
    LANGUAGE plpgsql IMMUTABLE STRICT
    AS $_$
DECLARE
  in_time ALIAS FOR $1;
  bymonthday ALIAS FOR $2;
  month_start TIMESTAMP WITH TIME ZONE;
  daysinmonth INT;
  i INT;
BEGIN

  month_start := date_trunc( 'month', in_time ) + (in_time::time)::interval;
  daysinmonth := date_part( 'days', (month_start + interval '1 month') - interval '1 day' );

  FOR i IN 1..31 LOOP
    EXIT WHEN bymonthday[i] IS NULL;

    CONTINUE WHEN bymonthday[i] > daysinmonth;
    CONTINUE WHEN bymonthday[i] < (-1 * daysinmonth);

    IF bymonthday[i] > 0 THEN
      RETURN NEXT month_start + ((bymonthday[i] - 1)::text || 'days')::interval;
    ELSIF bymonthday[i] < 0 THEN
      RETURN NEXT month_start + ((daysinmonth + bymonthday[i])::text || 'days')::interval;
    ELSE
      RAISE NOTICE 'Ignored invalid BYMONTHDAY part "%".', bymonthday[i];
    END IF;
  END LOOP;

  RETURN;

END;
$_$;


ALTER FUNCTION public.rrule_month_bymonthday_set(timestamp with time zone, integer[]) OWNER TO davical_dba;

--
-- Name: rrule_week_byday_set(timestamp with time zone, text[]); Type: FUNCTION; Schema: public; Owner: davical_dba
--

CREATE FUNCTION rrule_week_byday_set(timestamp with time zone, text[]) RETURNS SETOF timestamp with time zone
    LANGUAGE plpgsql IMMUTABLE
    AS $_$
DECLARE
  in_time ALIAS FOR $1;
  byday ALIAS FOR $2;
  dayrule TEXT;
  dow INT;
  our_day TIMESTAMP WITH TIME ZONE;
  i INT;
BEGIN

  IF byday IS NULL THEN
    -- We still return the single date as a SET
    RETURN NEXT in_time;
    RETURN;
  END IF;

  our_day := date_trunc( 'week', in_time ) + (in_time::time)::interval;

  i := 1;
  dayrule := byday[i];
  WHILE dayrule IS NOT NULL LOOP
    dow := position(dayrule in 'SUMOTUWETHFRSA') / 2;
    RETURN NEXT our_day + ((dow - 1)::text || 'days')::interval;
    i := i + 1;
    dayrule := byday[i];
  END LOOP;

  RETURN;

END;
$_$;


ALTER FUNCTION public.rrule_week_byday_set(timestamp with time zone, text[]) OWNER TO davical_dba;

--
-- Name: rrule_yearly_bymonth_set(timestamp with time zone, rrule_parts); Type: FUNCTION; Schema: public; Owner: davical_dba
--

CREATE FUNCTION rrule_yearly_bymonth_set(timestamp with time zone, rrule_parts) RETURNS SETOF timestamp with time zone
    LANGUAGE plpgsql IMMUTABLE STRICT
    AS $_$
DECLARE
  after ALIAS FOR $1;
  rrule ALIAS FOR $2;
  current_base TIMESTAMP WITH TIME ZONE;
  rr rrule_parts;
  i INT;
BEGIN

  IF rrule.bymonth IS NOT NULL THEN
    -- Ensure we don't pass BYSETPOS down
    rr := rrule;
    rr.bysetpos := NULL;
    FOR i IN 1..12 LOOP
      EXIT WHEN rr.bymonth[i] IS NULL;
      current_base := date_trunc( 'year', after ) + ((rr.bymonth[i] - 1)::text || ' months')::interval + (after::time)::interval;
      RETURN QUERY SELECT r FROM monthly_set(current_base,rr) r;
    END LOOP;
  ELSE
    -- We don't yet implement byweekno, byblah
    RETURN NEXT after;
  END IF;

END;
$_$;


ALTER FUNCTION public.rrule_yearly_bymonth_set(timestamp with time zone, rrule_parts) OWNER TO davical_dba;

--
-- Name: set_dav_property(text, integer, text, text); Type: FUNCTION; Schema: public; Owner: davical_dba
--

CREATE FUNCTION set_dav_property(text, integer, text, text) RETURNS boolean
    LANGUAGE plpgsql STRICT
    AS $_$
DECLARE
  path ALIAS FOR $1;
  change_user ALIAS FOR $2;
  key ALIAS FOR $3;
  value ALIAS FOR $4;
BEGIN
  -- Check that there is either a resource, collection or user at this location.
  IF NOT EXISTS(        SELECT 1 FROM caldav_data WHERE dav_name = path
                  UNION SELECT 1 FROM collection WHERE dav_name = path
                  UNION SELECT 1 FROM dav_principal WHERE dav_name = path
                  UNION SELECT 1 FROM dav_binding WHERE dav_name = path
               ) THEN
    RETURN FALSE;
  END IF;
  PERFORM true FROM property WHERE dav_name = path AND property_name = key;
  IF FOUND THEN
    UPDATE property SET changed_by=change_user::integer, changed_on=current_timestamp, property_value=value WHERE dav_name = path AND property_name = key;
  ELSE
    INSERT INTO property ( dav_name, changed_by, changed_on, property_name, property_value ) VALUES( path, change_user::integer, current_timestamp, key, value );
  END IF;
  RETURN TRUE;
END;
$_$;


ALTER FUNCTION public.set_dav_property(text, integer, text, text) OWNER TO davical_dba;

--
-- Name: sync_dav_id(); Type: FUNCTION; Schema: public; Owner: davical_dba
--

CREATE FUNCTION sync_dav_id() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
  DECLARE
  BEGIN

    IF TG_OP = 'DELETE' THEN
      -- Just let the ON DELETE CASCADE handle this case
      RETURN OLD;
    END IF;

    IF NEW.dav_id IS NULL THEN
      NEW.dav_id = nextval('dav_id_seq');
    END IF;

    IF TG_OP = 'UPDATE' THEN
      IF OLD.dav_id != NEW.dav_id OR OLD.collection_id != NEW.collection_id
                 OR OLD.user_no != NEW.user_no OR OLD.dav_name != NEW.dav_name THEN
        UPDATE calendar_item SET dav_id = NEW.dav_id, user_no = NEW.user_no,
                        collection_id = NEW.collection_id, dav_name = NEW.dav_name
            WHERE dav_name = OLD.dav_name OR dav_id = OLD.dav_id;
      END IF;
      RETURN NEW;
    END IF;

    UPDATE calendar_item SET dav_id = NEW.dav_id, user_no = NEW.user_no,
                    collection_id = NEW.collection_id, dav_name = NEW.dav_name
          WHERE dav_name = NEW.dav_name OR dav_id = NEW.dav_id;

    RETURN NEW;

  END
$$;


ALTER FUNCTION public.sync_dav_id() OWNER TO davical_dba;

--
-- Name: test_byday_rule(timestamp with time zone, text[]); Type: FUNCTION; Schema: public; Owner: davical_dba
--

CREATE FUNCTION test_byday_rule(timestamp with time zone, text[]) RETURNS boolean
    LANGUAGE plpgsql IMMUTABLE
    AS $_$
DECLARE
  testme ALIAS FOR $1;
  byday ALIAS FOR $2;
BEGIN
  -- Note that this doesn't work for MONTHLY/YEARLY BYDAY clauses which might have numbers prepended
  -- so don't call it that way...
  IF byday IS NOT NULL THEN
    RETURN ( substring( to_char( testme, 'DY') for 2 from 1) = ANY (byday) );
  END IF;
  RETURN TRUE;
END;
$_$;


ALTER FUNCTION public.test_byday_rule(timestamp with time zone, text[]) OWNER TO davical_dba;

--
-- Name: test_bymonth_rule(timestamp with time zone, integer[]); Type: FUNCTION; Schema: public; Owner: davical_dba
--

CREATE FUNCTION test_bymonth_rule(timestamp with time zone, integer[]) RETURNS boolean
    LANGUAGE plpgsql IMMUTABLE
    AS $_$
DECLARE
  testme ALIAS FOR $1;
  bymonth ALIAS FOR $2;
BEGIN
  IF bymonth IS NOT NULL THEN
    RETURN ( date_part( 'month', testme) = ANY (bymonth) );
  END IF;
  RETURN TRUE;
END;
$_$;


ALTER FUNCTION public.test_bymonth_rule(timestamp with time zone, integer[]) OWNER TO davical_dba;

--
-- Name: test_bymonthday_rule(timestamp with time zone, integer[]); Type: FUNCTION; Schema: public; Owner: davical_dba
--

CREATE FUNCTION test_bymonthday_rule(timestamp with time zone, integer[]) RETURNS boolean
    LANGUAGE plpgsql IMMUTABLE
    AS $_$
DECLARE
  testme ALIAS FOR $1;
  bymonthday ALIAS FOR $2;
BEGIN
  IF bymonthday IS NOT NULL THEN
    RETURN ( date_part( 'day', testme) = ANY (bymonthday) );
  END IF;
  RETURN TRUE;
END;
$_$;


ALTER FUNCTION public.test_bymonthday_rule(timestamp with time zone, integer[]) OWNER TO davical_dba;

--
-- Name: test_byyearday_rule(timestamp with time zone, integer[]); Type: FUNCTION; Schema: public; Owner: davical_dba
--

CREATE FUNCTION test_byyearday_rule(timestamp with time zone, integer[]) RETURNS boolean
    LANGUAGE plpgsql IMMUTABLE
    AS $_$
DECLARE
  testme ALIAS FOR $1;
  byyearday ALIAS FOR $2;
BEGIN
  IF byyearday IS NOT NULL THEN
    RETURN ( date_part( 'doy', testme) = ANY (byyearday) );
  END IF;
  RETURN TRUE;
END;
$_$;


ALTER FUNCTION public.test_byyearday_rule(timestamp with time zone, integer[]) OWNER TO davical_dba;

--
-- Name: to_ical_utc(timestamp with time zone); Type: FUNCTION; Schema: public; Owner: davical_dba
--

CREATE FUNCTION to_ical_utc(timestamp with time zone) RETURNS text
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
  SELECT to_char( $1 at time zone 'UTC', 'YYYYMMDD"T"HH24MISS"Z"' )
$_$;


ALTER FUNCTION public.to_ical_utc(timestamp with time zone) OWNER TO davical_dba;

--
-- Name: uprivs(bigint, bigint, integer); Type: FUNCTION; Schema: public; Owner: davical_dba
--

CREATE FUNCTION uprivs(bigint, bigint, integer) RETURNS bit
    LANGUAGE plpgsql STABLE STRICT
    AS $_$
DECLARE
  in_accessor ALIAS FOR $1;
  in_grantor  ALIAS FOR $2;
  in_depth    ALIAS FOR $3;
  out_conferred BIT(24);
BEGIN
  out_conferred := 0::BIT(24);
  -- Self can always have full access
  IF in_grantor = in_accessor THEN
    RETURN ~ out_conferred;
  END IF;

  SELECT pprivs( p1.principal_id, p2.principal_id, in_depth ) INTO out_conferred
          FROM principal p1, principal p2
          WHERE p1.user_no = in_accessor AND p2.user_no = in_grantor;

  RETURN out_conferred;
END;
$_$;


ALTER FUNCTION public.uprivs(bigint, bigint, integer) OWNER TO davical_dba;

--
-- Name: usr_is_role(integer, text); Type: FUNCTION; Schema: public; Owner: davical_dba
--

CREATE FUNCTION usr_is_role(integer, text) RETURNS boolean
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
  SELECT EXISTS( SELECT 1 FROM role_member JOIN roles USING(role_no) WHERE role_member.user_no=$1 AND roles.role_name=$2 )
$_$;


ALTER FUNCTION public.usr_is_role(integer, text) OWNER TO davical_dba;

--
-- Name: usr_modified(); Type: FUNCTION; Schema: public; Owner: davical_dba
--

CREATE FUNCTION usr_modified() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
  oldpath TEXT;
  newpath TEXT;
BEGIN
  -- in case we trigger on other events in future
  IF TG_OP = 'UPDATE' THEN
    IF NEW.username != OLD.username THEN
      oldpath := '/' || OLD.username || '/';
      newpath := '/' || NEW.username || '/';
      UPDATE collection
        SET parent_container = replace( parent_container, oldpath, newpath),
            dav_name = replace( dav_name, oldpath, newpath)
      WHERE substring(dav_name from 1 for char_length(oldpath)) = oldpath;
    END IF;
  END IF;
  RETURN NEW;
END;
$$;


ALTER FUNCTION public.usr_modified() OWNER TO davical_dba;

--
-- Name: weekly_set(timestamp with time zone, rrule_parts); Type: FUNCTION; Schema: public; Owner: davical_dba
--

CREATE FUNCTION weekly_set(timestamp with time zone, rrule_parts) RETURNS SETOF timestamp with time zone
    LANGUAGE plpgsql IMMUTABLE STRICT
    AS $_$
DECLARE
  after ALIAS FOR $1;
  rrule ALIAS FOR $2;
  valid_date TIMESTAMP WITH TIME ZONE;
  curse REFCURSOR;
  weekno INT;
  i INT;
BEGIN

  IF rrule.byweekno IS NOT NULL THEN
    weekno := date_part('week',after);
    IF NOT weekno = ANY ( rrule.byweekno ) THEN
      RETURN;
    END IF;
  END IF;

  OPEN curse SCROLL FOR SELECT r FROM rrule_week_byday_set(after, rrule.byday ) r;
  RETURN QUERY SELECT d FROM rrule_bysetpos_filter(curse,rrule.bysetpos) d;

END;
$_$;


ALTER FUNCTION public.weekly_set(timestamp with time zone, rrule_parts) OWNER TO davical_dba;

--
-- Name: write_sync_change(bigint, integer, text); Type: FUNCTION; Schema: public; Owner: davical_dba
--

CREATE FUNCTION write_sync_change(bigint, integer, text) RETURNS boolean
    LANGUAGE plpgsql STRICT
    AS $_$
DECLARE
  in_collection_id ALIAS FOR $1;
  in_status ALIAS FOR $2;
  in_dav_name ALIAS FOR $3;
  tmp_int INT8;
BEGIN
  SELECT 1 INTO tmp_int FROM sync_tokens
           WHERE collection_id = in_collection_id
           LIMIT 1;
  IF NOT FOUND THEN
    RETURN FALSE;
  END IF;
  SELECT dav_id INTO tmp_int FROM caldav_data WHERE dav_name = in_dav_name;
  INSERT INTO sync_changes ( collection_id, sync_status, dav_id, dav_name)
                     VALUES( in_collection_id, in_status, tmp_int, in_dav_name);
  RETURN TRUE;
END
$_$;


ALTER FUNCTION public.write_sync_change(bigint, integer, text) OWNER TO davical_dba;

--
-- Name: yearly_set(timestamp with time zone, rrule_parts); Type: FUNCTION; Schema: public; Owner: davical_dba
--

CREATE FUNCTION yearly_set(timestamp with time zone, rrule_parts) RETURNS SETOF timestamp with time zone
    LANGUAGE plpgsql IMMUTABLE STRICT
    AS $_$
DECLARE
  after ALIAS FOR $1;
  rrule ALIAS FOR $2;
  current_base TIMESTAMP WITH TIME ZONE;
  curse REFCURSOR;
  curser REFCURSOR;
  i INT;
BEGIN

  IF rrule.bymonth IS NOT NULL THEN
    OPEN curse SCROLL FOR SELECT r FROM rrule_yearly_bymonth_set(after, rrule ) r;
    FOR current_base IN SELECT d FROM rrule_bysetpos_filter(curse,rrule.bysetpos) d LOOP
      current_base := date_trunc( 'day', current_base ) + (after::time)::interval;
      RETURN NEXT current_base;
    END LOOP;
  ELSE
    -- We don't yet implement byweekno, byblah
    RETURN NEXT after;
  END IF;
END;
$_$;


ALTER FUNCTION public.yearly_set(timestamp with time zone, rrule_parts) OWNER TO davical_dba;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: access_ticket; Type: TABLE; Schema: public; Owner: davical_dba; Tablespace: 
--

CREATE TABLE access_ticket (
    ticket_id text NOT NULL,
    dav_owner_id bigint NOT NULL,
    privileges bit(24),
    target_collection_id bigint NOT NULL,
    target_resource_id bigint,
    expires timestamp without time zone
);


ALTER TABLE public.access_ticket OWNER TO davical_dba;

--
-- Name: addressbook_address_adr; Type: TABLE; Schema: public; Owner: davical_dba; Tablespace: 
--

CREATE TABLE addressbook_address_adr (
    dav_id bigint NOT NULL,
    type text,
    box_no text,
    unit_no text,
    street_address text,
    locality text,
    region text,
    postcode text,
    country text,
    property text
);


ALTER TABLE public.addressbook_address_adr OWNER TO davical_dba;

--
-- Name: addressbook_address_email; Type: TABLE; Schema: public; Owner: davical_dba; Tablespace: 
--

CREATE TABLE addressbook_address_email (
    dav_id bigint NOT NULL,
    type text,
    email text,
    property text
);


ALTER TABLE public.addressbook_address_email OWNER TO davical_dba;

--
-- Name: addressbook_address_tel; Type: TABLE; Schema: public; Owner: davical_dba; Tablespace: 
--

CREATE TABLE addressbook_address_tel (
    dav_id bigint NOT NULL,
    type text,
    tel text,
    property text
);


ALTER TABLE public.addressbook_address_tel OWNER TO davical_dba;

--
-- Name: addressbook_resource; Type: TABLE; Schema: public; Owner: davical_dba; Tablespace: 
--

CREATE TABLE addressbook_resource (
    dav_id bigint NOT NULL,
    version text,
    uid text,
    nickname text,
    fn text,
    n text,
    note text,
    org text,
    url text,
    fburl text,
    caladruri text,
    caluri text
);


ALTER TABLE public.addressbook_resource OWNER TO davical_dba;

--
-- Name: awl_db_revision; Type: TABLE; Schema: public; Owner: davical_dba; Tablespace: 
--

CREATE TABLE awl_db_revision (
    schema_id integer,
    schema_major integer,
    schema_minor integer,
    schema_patch integer,
    schema_name text,
    applied_on timestamp with time zone DEFAULT now()
);


ALTER TABLE public.awl_db_revision OWNER TO davical_dba;

--
-- Name: dav_id_seq; Type: SEQUENCE; Schema: public; Owner: davical_dba
--

CREATE SEQUENCE dav_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.dav_id_seq OWNER TO davical_dba;

--
-- Name: caldav_data; Type: TABLE; Schema: public; Owner: davical_dba; Tablespace: 
--

CREATE TABLE caldav_data (
    user_no integer NOT NULL,
    dav_name text NOT NULL,
    dav_etag text,
    created timestamp with time zone,
    modified timestamp with time zone,
    caldav_data text,
    caldav_type text,
    logged_user integer,
    dav_id bigint DEFAULT nextval('dav_id_seq'::regclass),
    collection_id bigint,
    weak_etag text
);


ALTER TABLE public.caldav_data OWNER TO davical_dba;

--
-- Name: calendar_alarm; Type: TABLE; Schema: public; Owner: davical_dba; Tablespace: 
--

CREATE TABLE calendar_alarm (
    dav_id bigint NOT NULL,
    action text,
    trigger text,
    summary text,
    description text,
    next_trigger timestamp with time zone,
    component text,
    trigger_state character(1) DEFAULT 'N'::bpchar
);


ALTER TABLE public.calendar_alarm OWNER TO davical_dba;

--
-- Name: calendar_attendee; Type: TABLE; Schema: public; Owner: davical_dba; Tablespace: 
--

CREATE TABLE calendar_attendee (
    dav_id bigint NOT NULL,
    status text,
    partstat text,
    cn text,
    attendee text NOT NULL,
    role text,
    rsvp boolean,
    property text,
    attendee_state text,
    weak_etag text
);


ALTER TABLE public.calendar_attendee OWNER TO davical_dba;

--
-- Name: calendar_item; Type: TABLE; Schema: public; Owner: davical_dba; Tablespace: 
--

CREATE TABLE calendar_item (
    user_no integer NOT NULL,
    dav_name text NOT NULL,
    dav_etag text,
    uid text,
    created timestamp without time zone,
    last_modified timestamp without time zone,
    dtstamp timestamp without time zone,
    dtstart timestamp with time zone,
    dtend timestamp with time zone,
    due timestamp with time zone,
    summary text,
    location text,
    description text,
    priority integer,
    class text,
    transp text,
    rrule text,
    url text,
    percent_complete numeric(7,2),
    tz_id text,
    status text,
    completed timestamp with time zone,
    dav_id bigint,
    collection_id bigint,
    first_instance_start timestamp without time zone,
    last_instance_end timestamp without time zone
);


ALTER TABLE public.calendar_item OWNER TO davical_dba;

--
-- Name: collection; Type: TABLE; Schema: public; Owner: davical_dba; Tablespace: 
--

CREATE TABLE collection (
    user_no integer,
    parent_container text,
    dav_name text,
    dav_etag text,
    dav_displayname text,
    is_calendar boolean,
    created timestamp with time zone,
    modified timestamp with time zone,
    public_events_only boolean DEFAULT false NOT NULL,
    publicly_readable boolean DEFAULT false NOT NULL,
    collection_id bigint DEFAULT nextval('dav_id_seq'::regclass) NOT NULL,
    default_privileges bit(24),
    is_addressbook boolean DEFAULT false,
    resourcetypes text DEFAULT '<DAV::collection/>'::text,
    schedule_transp text DEFAULT 'opaque'::text,
    timezone text,
    description text DEFAULT ''::text
);


ALTER TABLE public.collection OWNER TO davical_dba;

--
-- Name: dav_binding; Type: TABLE; Schema: public; Owner: davical_dba; Tablespace: 
--

CREATE TABLE dav_binding (
    bind_id bigint DEFAULT nextval('dav_id_seq'::regclass) NOT NULL,
    bound_source_id bigint,
    access_ticket_id text,
    dav_owner_id bigint NOT NULL,
    parent_container text NOT NULL,
    dav_name text NOT NULL,
    dav_displayname text,
    external_url text,
    type text,
    CONSTRAINT dav_name_does_not_exist CHECK ((NOT real_path_exists(dav_name)))
);


ALTER TABLE public.dav_binding OWNER TO davical_dba;

--
-- Name: principal; Type: TABLE; Schema: public; Owner: davical_dba; Tablespace: 
--

CREATE TABLE principal (
    principal_id bigint DEFAULT nextval('dav_id_seq'::regclass) NOT NULL,
    type_id bigint NOT NULL,
    user_no bigint,
    displayname text,
    default_privileges bit(24)
);


ALTER TABLE public.principal OWNER TO davical_dba;

--
-- Name: usr; Type: TABLE; Schema: public; Owner: davical_dba; Tablespace: 
--

CREATE TABLE usr (
    user_no integer NOT NULL,
    active boolean DEFAULT true,
    email_ok timestamp with time zone,
    joined timestamp with time zone DEFAULT now(),
    updated timestamp with time zone,
    last_used timestamp with time zone,
    username text NOT NULL,
    password text,
    fullname text,
    email text,
    config_data text,
    date_format_type text DEFAULT 'E'::text,
    locale text
);


ALTER TABLE public.usr OWNER TO davical_dba;

--
-- Name: dav_principal; Type: VIEW; Schema: public; Owner: davical_dba
--

CREATE VIEW dav_principal AS
    SELECT principal.user_no, usr.active AS user_active, usr.joined AS created, usr.updated AS modified, usr.username, usr.password, usr.fullname, usr.email, usr.email_ok, usr.date_format_type, usr.locale, principal.principal_id, principal.type_id, principal.displayname, principal.default_privileges, true AS is_principal, false AS is_calendar, principal.principal_id AS collection_id, false AS is_addressbook, (('/'::text || usr.username) || '/'::text) AS dav_name, '<DAV::collection/><DAV::principal/>'::text AS resourcetypes FROM (usr JOIN principal USING (user_no));


ALTER TABLE public.dav_principal OWNER TO davical_dba;

--
-- Name: freebusy_ticket; Type: TABLE; Schema: public; Owner: davical_dba; Tablespace: 
--

CREATE TABLE freebusy_ticket (
    ticket_id text NOT NULL,
    user_no integer NOT NULL,
    created timestamp with time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.freebusy_ticket OWNER TO davical_dba;

--
-- Name: grants; Type: TABLE; Schema: public; Owner: davical_dba; Tablespace: 
--

CREATE TABLE grants (
    by_principal bigint,
    by_collection bigint,
    to_principal bigint,
    privileges bit(24),
    is_group boolean
);


ALTER TABLE public.grants OWNER TO davical_dba;

--
-- Name: group_member; Type: TABLE; Schema: public; Owner: davical_dba; Tablespace: 
--

CREATE TABLE group_member (
    group_id bigint,
    member_id bigint
);


ALTER TABLE public.group_member OWNER TO davical_dba;

--
-- Name: locks; Type: TABLE; Schema: public; Owner: davical_dba; Tablespace: 
--

CREATE TABLE locks (
    dav_name text,
    opaquelocktoken text NOT NULL,
    type text,
    scope text,
    depth integer,
    owner text,
    timeout interval,
    start timestamp without time zone DEFAULT now()
);


ALTER TABLE public.locks OWNER TO davical_dba;

--
-- Name: principal_type; Type: TABLE; Schema: public; Owner: davical_dba; Tablespace: 
--

CREATE TABLE principal_type (
    principal_type_id integer NOT NULL,
    principal_type_desc text
);


ALTER TABLE public.principal_type OWNER TO davical_dba;

--
-- Name: principal_type_principal_type_id_seq; Type: SEQUENCE; Schema: public; Owner: davical_dba
--

CREATE SEQUENCE principal_type_principal_type_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.principal_type_principal_type_id_seq OWNER TO davical_dba;

--
-- Name: principal_type_principal_type_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: davical_dba
--

ALTER SEQUENCE principal_type_principal_type_id_seq OWNED BY principal_type.principal_type_id;


--
-- Name: property; Type: TABLE; Schema: public; Owner: davical_dba; Tablespace: 
--

CREATE TABLE property (
    dav_name text NOT NULL,
    property_name text NOT NULL,
    property_value text,
    changed_on timestamp without time zone DEFAULT now(),
    changed_by integer
);


ALTER TABLE public.property OWNER TO davical_dba;

--
-- Name: relationship; Type: TABLE; Schema: public; Owner: davical_dba; Tablespace: 
--

CREATE TABLE relationship (
    from_user integer NOT NULL,
    to_user integer NOT NULL,
    rt_id integer NOT NULL,
    confers bit(24) DEFAULT privilege_to_bits(ARRAY['DAV::read'::text, 'DAV::write'::text])
);


ALTER TABLE public.relationship OWNER TO davical_dba;

--
-- Name: relationship_type; Type: TABLE; Schema: public; Owner: davical_dba; Tablespace: 
--

CREATE TABLE relationship_type (
    rt_id integer NOT NULL,
    rt_name text,
    rt_togroup boolean,
    confers text DEFAULT 'RW'::text,
    rt_fromgroup boolean,
    bit_confers bit(24) DEFAULT privilege_to_bits(ARRAY['DAV::read'::text, 'DAV::write'::text])
);


ALTER TABLE public.relationship_type OWNER TO davical_dba;

--
-- Name: relationship_type_rt_id_seq; Type: SEQUENCE; Schema: public; Owner: davical_dba
--

CREATE SEQUENCE relationship_type_rt_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.relationship_type_rt_id_seq OWNER TO davical_dba;

--
-- Name: relationship_type_rt_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: davical_dba
--

ALTER SEQUENCE relationship_type_rt_id_seq OWNED BY relationship_type.rt_id;


--
-- Name: role_member; Type: TABLE; Schema: public; Owner: davical_dba; Tablespace: 
--

CREATE TABLE role_member (
    role_no integer,
    user_no integer
);


ALTER TABLE public.role_member OWNER TO davical_dba;

--
-- Name: roles; Type: TABLE; Schema: public; Owner: davical_dba; Tablespace: 
--

CREATE TABLE roles (
    role_no integer NOT NULL,
    role_name text
);


ALTER TABLE public.roles OWNER TO davical_dba;

--
-- Name: roles_role_no_seq; Type: SEQUENCE; Schema: public; Owner: davical_dba
--

CREATE SEQUENCE roles_role_no_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.roles_role_no_seq OWNER TO davical_dba;

--
-- Name: roles_role_no_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: davical_dba
--

ALTER SEQUENCE roles_role_no_seq OWNED BY roles.role_no;


--
-- Name: session; Type: TABLE; Schema: public; Owner: davical_dba; Tablespace: 
--

CREATE TABLE session (
    session_id integer NOT NULL,
    user_no integer,
    session_start timestamp with time zone DEFAULT now(),
    session_end timestamp with time zone DEFAULT now(),
    session_key text,
    session_config text
);


ALTER TABLE public.session OWNER TO davical_dba;

--
-- Name: session_session_id_seq; Type: SEQUENCE; Schema: public; Owner: davical_dba
--

CREATE SEQUENCE session_session_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.session_session_id_seq OWNER TO davical_dba;

--
-- Name: session_session_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: davical_dba
--

ALTER SEQUENCE session_session_id_seq OWNED BY session.session_id;


--
-- Name: supported_locales; Type: TABLE; Schema: public; Owner: davical_dba; Tablespace: 
--

CREATE TABLE supported_locales (
    locale text NOT NULL,
    locale_name_en text,
    locale_name_locale text
);


ALTER TABLE public.supported_locales OWNER TO davical_dba;

--
-- Name: sync_changes; Type: TABLE; Schema: public; Owner: davical_dba; Tablespace: 
--

CREATE TABLE sync_changes (
    sync_time timestamp with time zone DEFAULT now(),
    collection_id bigint,
    sync_status integer,
    dav_id bigint,
    dav_name text
);


ALTER TABLE public.sync_changes OWNER TO davical_dba;

--
-- Name: sync_tokens; Type: TABLE; Schema: public; Owner: davical_dba; Tablespace: 
--

CREATE TABLE sync_tokens (
    sync_token integer NOT NULL,
    collection_id bigint,
    modification_time timestamp with time zone DEFAULT now()
);


ALTER TABLE public.sync_tokens OWNER TO davical_dba;

--
-- Name: sync_tokens_sync_token_seq; Type: SEQUENCE; Schema: public; Owner: davical_dba
--

CREATE SEQUENCE sync_tokens_sync_token_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.sync_tokens_sync_token_seq OWNER TO davical_dba;

--
-- Name: sync_tokens_sync_token_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: davical_dba
--

ALTER SEQUENCE sync_tokens_sync_token_seq OWNED BY sync_tokens.sync_token;


--
-- Name: timezones; Type: TABLE; Schema: public; Owner: davical_dba; Tablespace: 
--

CREATE TABLE timezones (
    our_tzno integer NOT NULL,
    tzid text NOT NULL,
    olson_name text,
    active boolean,
    last_modified timestamp without time zone DEFAULT now(),
    etag text,
    vtimezone text
);


ALTER TABLE public.timezones OWNER TO davical_dba;

--
-- Name: timezones_our_tzno_seq; Type: SEQUENCE; Schema: public; Owner: davical_dba
--

CREATE SEQUENCE timezones_our_tzno_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.timezones_our_tzno_seq OWNER TO davical_dba;

--
-- Name: timezones_our_tzno_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: davical_dba
--

ALTER SEQUENCE timezones_our_tzno_seq OWNED BY timezones.our_tzno;


--
-- Name: tmp_password; Type: TABLE; Schema: public; Owner: davical_dba; Tablespace: 
--

CREATE TABLE tmp_password (
    user_no integer,
    password text,
    valid_until timestamp with time zone DEFAULT (now() + '1 day'::interval)
);


ALTER TABLE public.tmp_password OWNER TO davical_dba;

--
-- Name: tz_aliases; Type: TABLE; Schema: public; Owner: davical_dba; Tablespace: 
--

CREATE TABLE tz_aliases (
    our_tzno bigint,
    tzalias text NOT NULL
);


ALTER TABLE public.tz_aliases OWNER TO davical_dba;

--
-- Name: tz_localnames; Type: TABLE; Schema: public; Owner: davical_dba; Tablespace: 
--

CREATE TABLE tz_localnames (
    our_tzno bigint,
    locale text NOT NULL,
    localised_name text NOT NULL,
    preferred boolean DEFAULT true
);


ALTER TABLE public.tz_localnames OWNER TO davical_dba;

--
-- Name: usr_setting; Type: TABLE; Schema: public; Owner: davical_dba; Tablespace: 
--

CREATE TABLE usr_setting (
    user_no integer NOT NULL,
    setting_name text NOT NULL,
    setting_value text
);


ALTER TABLE public.usr_setting OWNER TO davical_dba;

--
-- Name: usr_user_no_seq; Type: SEQUENCE; Schema: public; Owner: davical_dba
--

CREATE SEQUENCE usr_user_no_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.usr_user_no_seq OWNER TO davical_dba;

--
-- Name: usr_user_no_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: davical_dba
--

ALTER SEQUENCE usr_user_no_seq OWNED BY usr.user_no;


--
-- Name: principal_type_id; Type: DEFAULT; Schema: public; Owner: davical_dba
--

ALTER TABLE ONLY principal_type ALTER COLUMN principal_type_id SET DEFAULT nextval('principal_type_principal_type_id_seq'::regclass);


--
-- Name: rt_id; Type: DEFAULT; Schema: public; Owner: davical_dba
--

ALTER TABLE ONLY relationship_type ALTER COLUMN rt_id SET DEFAULT nextval('relationship_type_rt_id_seq'::regclass);


--
-- Name: role_no; Type: DEFAULT; Schema: public; Owner: davical_dba
--

ALTER TABLE ONLY roles ALTER COLUMN role_no SET DEFAULT nextval('roles_role_no_seq'::regclass);


--
-- Name: session_id; Type: DEFAULT; Schema: public; Owner: davical_dba
--

ALTER TABLE ONLY session ALTER COLUMN session_id SET DEFAULT nextval('session_session_id_seq'::regclass);


--
-- Name: sync_token; Type: DEFAULT; Schema: public; Owner: davical_dba
--

ALTER TABLE ONLY sync_tokens ALTER COLUMN sync_token SET DEFAULT nextval('sync_tokens_sync_token_seq'::regclass);


--
-- Name: our_tzno; Type: DEFAULT; Schema: public; Owner: davical_dba
--

ALTER TABLE ONLY timezones ALTER COLUMN our_tzno SET DEFAULT nextval('timezones_our_tzno_seq'::regclass);


--
-- Name: user_no; Type: DEFAULT; Schema: public; Owner: davical_dba
--

ALTER TABLE ONLY usr ALTER COLUMN user_no SET DEFAULT nextval('usr_user_no_seq'::regclass);


--
-- Data for Name: access_ticket; Type: TABLE DATA; Schema: public; Owner: davical_dba
--

COPY access_ticket (ticket_id, dav_owner_id, privileges, target_collection_id, target_resource_id, expires) FROM stdin;
\.


--
-- Data for Name: addressbook_address_adr; Type: TABLE DATA; Schema: public; Owner: davical_dba
--

COPY addressbook_address_adr (dav_id, type, box_no, unit_no, street_address, locality, region, postcode, country, property) FROM stdin;
\.


--
-- Data for Name: addressbook_address_email; Type: TABLE DATA; Schema: public; Owner: davical_dba
--

COPY addressbook_address_email (dav_id, type, email, property) FROM stdin;
\.


--
-- Data for Name: addressbook_address_tel; Type: TABLE DATA; Schema: public; Owner: davical_dba
--

COPY addressbook_address_tel (dav_id, type, tel, property) FROM stdin;
\.


--
-- Data for Name: addressbook_resource; Type: TABLE DATA; Schema: public; Owner: davical_dba
--

COPY addressbook_resource (dav_id, version, uid, nickname, fn, n, note, org, url, fburl, caladruri, caluri) FROM stdin;
\.


--
-- Data for Name: awl_db_revision; Type: TABLE DATA; Schema: public; Owner: davical_dba
--

COPY awl_db_revision (schema_id, schema_major, schema_minor, schema_patch, schema_name, applied_on) FROM stdin;
1	1	1	0	Dawn	2014-11-13 15:22:56.973559+01
2	1	2	11	Novembre	2014-11-13 15:22:57.383813+01
\.


--
-- Data for Name: caldav_data; Type: TABLE DATA; Schema: public; Owner: davical_dba
--

COPY caldav_data (user_no, dav_name, dav_etag, created, modified, caldav_data, caldav_type, logged_user, dav_id, collection_id, weak_etag) FROM stdin;
1001	/richard/96EECC04-B54C-4365-ACE0-D872DC2F116F/B0A6BA3E-CA2F-4D98-99A9-07BF5B495071.ics	6fd91c80da0ddd26f331bbc924f9f085	2014-11-16 17:09:38+01	2014-11-16 17:10:57+01	BEGIN:VCALENDAR\r\nVERSION:2.0\r\nPRODID:-////NONSGML kigkonsult.se iCalcreator 2.14//\r\nCALSCALE:GREGORIAN\r\nBEGIN:VTIMEZONE\r\nTZID:Europe/Madrid\r\nX-LIC-LOCATION:Europe/Madrid\r\nBEGIN:STANDARD\r\nDTSTART:20141026T030000\r\nTZOFFSETFROM:+0200\r\nTZOFFSETTO:+0100\r\nRDATE:20151025T030000\r\nTZNAME:CET\r\nEND:STANDARD\r\nBEGIN:DAYLIGHT\r\nDTSTART:20150329T020000\r\nTZOFFSETFROM:+0100\r\nTZOFFSETTO:+0200\r\nTZNAME:CEST\r\nEND:DAYLIGHT\r\nEND:VTIMEZONE\r\nBEGIN:VEVENT\r\nUID:B0A6BA3E-CA2F-4D98-99A9-07BF5B495071\r\nDTSTAMP:20141116T160938Z\r\nCLASS:PUBLIC\r\nCREATED:20141116T160938Z\r\nDTSTART;TZID=Europe/Madrid:20141119T170000\r\nDTEND;TZID=Europe/Madrid:20141119T180000\r\nLAST-MODIFIED:20141116T161057Z\r\nSEQUENCE:2\r\nSUMMARY:TEST\r\nTRANSP:OPAQUE\r\nEND:VEVENT\r\nEND:VCALENDAR\r\n	VEVENT	1001	1007	1005	\N
1	/admin/8A6B64D7-8155-4713-996A-5B4AB71472D2/0741F2A5-D3D8-4E69-8911-7B9869EFC0C2.ics	8e05e520d8e83e2f8737d6ee97d991a1	2014-11-29 15:48:15+01	2014-11-29 15:48:15+01	BEGIN:VCALENDAR\r\nVERSION:2.0\r\nPRODID:-////NONSGML kigkonsult.se iCalcreator 2.14//\r\nCALSCALE:GREGORIAN\r\nBEGIN:VTIMEZONE\r\nTZID:Europe/Madrid\r\nX-LIC-LOCATION:Europe/Madrid\r\nBEGIN:STANDARD\r\nDTSTART:20151025T030000\r\nTZOFFSETFROM:+0200\r\nTZOFFSETTO:+0100\r\nTZNAME:CET\r\nEND:STANDARD\r\nBEGIN:DAYLIGHT\r\nDTSTART:20150329T020000\r\nTZOFFSETFROM:+0100\r\nTZOFFSETTO:+0200\r\nTZNAME:CEST\r\nEND:DAYLIGHT\r\nEND:VTIMEZONE\r\nBEGIN:VEVENT\r\nUID:0741F2A5-D3D8-4E69-8911-7B9869EFC0C2\r\nDTSTAMP:20141129T144815Z\r\nCLASS:PUBLIC\r\nCREATED:20141129T144815Z\r\nDTSTART;TZID=Europe/Madrid:20141111T160000\r\nDTEND;TZID=Europe/Madrid:20141111T170000\r\nLAST-MODIFIED:20141129T144815Z\r\nSEQUENCE:0\r\nSUMMARY:TEST\r\nTRANSP:OPAQUE\r\nEND:VEVENT\r\nEND:VCALENDAR\r\n	VEVENT	1	1008	1004	\N
\.


--
-- Data for Name: calendar_alarm; Type: TABLE DATA; Schema: public; Owner: davical_dba
--

COPY calendar_alarm (dav_id, action, trigger, summary, description, next_trigger, component, trigger_state) FROM stdin;
\.


--
-- Data for Name: calendar_attendee; Type: TABLE DATA; Schema: public; Owner: davical_dba
--

COPY calendar_attendee (dav_id, status, partstat, cn, attendee, role, rsvp, property, attendee_state, weak_etag) FROM stdin;
\.


--
-- Data for Name: calendar_item; Type: TABLE DATA; Schema: public; Owner: davical_dba
--

COPY calendar_item (user_no, dav_name, dav_etag, uid, created, last_modified, dtstamp, dtstart, dtend, due, summary, location, description, priority, class, transp, rrule, url, percent_complete, tz_id, status, completed, dav_id, collection_id, first_instance_start, last_instance_end) FROM stdin;
1001	/richard/96EECC04-B54C-4365-ACE0-D872DC2F116F/B0A6BA3E-CA2F-4D98-99A9-07BF5B495071.ics	6fd91c80da0ddd26f331bbc924f9f085	B0A6BA3E-CA2F-4D98-99A9-07BF5B495071	2014-11-16 16:09:38	2014-11-16 16:10:57	2014-11-16 16:09:38	2014-11-19 17:00:00+01	2014-11-19 18:00:00+01	\N	TEST	\N	\N	\N	PUBLIC	OPAQUE	\N	\N	\N	Europe/Madrid	\N	\N	1007	1005	\N	\N
1	/admin/8A6B64D7-8155-4713-996A-5B4AB71472D2/0741F2A5-D3D8-4E69-8911-7B9869EFC0C2.ics	8e05e520d8e83e2f8737d6ee97d991a1	0741F2A5-D3D8-4E69-8911-7B9869EFC0C2	2014-11-29 14:48:15	2014-11-29 14:48:15	2014-11-29 14:48:15	2014-11-11 16:00:00+01	2014-11-11 17:00:00+01	\N	TEST	\N	\N	\N	PUBLIC	OPAQUE	\N	\N	\N	Europe/Madrid	\N	\N	1008	1004	\N	\N
\.


--
-- Data for Name: collection; Type: TABLE DATA; Schema: public; Owner: davical_dba
--

COPY collection (user_no, parent_container, dav_name, dav_etag, dav_displayname, is_calendar, created, modified, public_events_only, publicly_readable, collection_id, default_privileges, is_addressbook, resourcetypes, schedule_transp, timezone, description) FROM stdin;
1001	/richard/	/richard/addresses/	-1	urunuela richard addressbook	f	2014-11-13 16:54:02.150499+01	2014-11-13 16:54:02.150499+01	f	f	1003	\N	t	<DAV::collection/><urn:ietf:params:xml:ns:carddav:addressbook/>	opaque	\N	
1001	/richard/	/richard/96EECC04-B54C-4365-ACE0-D872DC2F116F/	10699f52c01cc02fb8b6337794637d84	testPartage	t	2014-11-16 16:58:56.878755+01	2014-11-16 17:10:57.765515+01	f	f	1005	\N	f	<DAV::collection/><urn:ietf:params:xml:ns:caldav:calendar/>	opaque	\N	
1001	/richard/	/richard/calendar/	af68666cd3c72f413ed20c57bc003630	urunuela richard calendar	t	2014-11-13 16:54:02.148092+01	2014-11-16 17:10:58.006666+01	f	f	1002	\N	f	<DAV::collection/><urn:ietf:params:xml:ns:caldav:calendar/>	opaque	\N	
1	/admin/	/admin/8A6B64D7-8155-4713-996A-5B4AB71472D2/	3532a2ea0c2e1588d4f9440f46d42f8f	CAL1	t	2014-11-13 17:09:58.857285+01	2014-11-29 15:48:15.968519+01	f	f	1004	\N	f	<DAV::collection/><urn:ietf:params:xml:ns:caldav:calendar/>	opaque	\N	
\.


--
-- Data for Name: dav_binding; Type: TABLE DATA; Schema: public; Owner: davical_dba
--

COPY dav_binding (bind_id, bound_source_id, access_ticket_id, dav_owner_id, parent_container, dav_name, dav_displayname, external_url, type) FROM stdin;
\.


--
-- Name: dav_id_seq; Type: SEQUENCE SET; Schema: public; Owner: davical_dba
--

SELECT pg_catalog.setval('dav_id_seq', 1008, true);


--
-- Data for Name: freebusy_ticket; Type: TABLE DATA; Schema: public; Owner: davical_dba
--

COPY freebusy_ticket (ticket_id, user_no, created) FROM stdin;
\.


--
-- Data for Name: grants; Type: TABLE DATA; Schema: public; Owner: davical_dba
--

COPY grants (by_principal, by_collection, to_principal, privileges, is_group) FROM stdin;
\.


--
-- Data for Name: group_member; Type: TABLE DATA; Schema: public; Owner: davical_dba
--

COPY group_member (group_id, member_id) FROM stdin;
\.


--
-- Data for Name: locks; Type: TABLE DATA; Schema: public; Owner: davical_dba
--

COPY locks (dav_name, opaquelocktoken, type, scope, depth, owner, timeout, start) FROM stdin;
\.


--
-- Data for Name: principal; Type: TABLE DATA; Schema: public; Owner: davical_dba
--

COPY principal (principal_id, type_id, user_no, displayname, default_privileges) FROM stdin;
1	1	1	DAViCal Administrator	000000000000000000000000
1001	1	1001	urunuela richard	000000000001111000000000
\.


--
-- Data for Name: principal_type; Type: TABLE DATA; Schema: public; Owner: davical_dba
--

COPY principal_type (principal_type_id, principal_type_desc) FROM stdin;
1	Person
2	Resource
3	Group
\.


--
-- Name: principal_type_principal_type_id_seq; Type: SEQUENCE SET; Schema: public; Owner: davical_dba
--

SELECT pg_catalog.setval('principal_type_principal_type_id_seq', 1, false);


--
-- Data for Name: property; Type: TABLE DATA; Schema: public; Owner: davical_dba
--

COPY property (dav_name, property_name, property_value, changed_on, changed_by) FROM stdin;
/admin/8A6B64D7-8155-4713-996A-5B4AB71472D2/	http://apple.com/ns/ical/:calendar-color	#D4EAEFff	2014-11-13 17:09:58.857285	1
/richard/96EECC04-B54C-4365-ACE0-D872DC2F116F/	http://apple.com/ns/ical/:calendar-color	#D4EAEFff	2014-11-16 16:58:56.878755	1001
\.


--
-- Data for Name: relationship; Type: TABLE DATA; Schema: public; Owner: davical_dba
--

COPY relationship (from_user, to_user, rt_id, confers) FROM stdin;
\.


--
-- Data for Name: relationship_type; Type: TABLE DATA; Schema: public; Owner: davical_dba
--

COPY relationship_type (rt_id, rt_name, rt_togroup, confers, rt_fromgroup, bit_confers) FROM stdin;
1	Administers	\N	A	\N	111111111111111111111111
2	Can read/write to	\N	RW	\N	000000000001001011000111
3	Can read from	\N	R	\N	000000000001001000000001
4	Can see free/busy time of	\N	F	\N	000000000001001000000000
\.


--
-- Name: relationship_type_rt_id_seq; Type: SEQUENCE SET; Schema: public; Owner: davical_dba
--

SELECT pg_catalog.setval('relationship_type_rt_id_seq', 10, true);


--
-- Data for Name: role_member; Type: TABLE DATA; Schema: public; Owner: davical_dba
--

COPY role_member (role_no, user_no) FROM stdin;
1	1
\.


--
-- Data for Name: roles; Type: TABLE DATA; Schema: public; Owner: davical_dba
--

COPY roles (role_no, role_name) FROM stdin;
1	Admin
2	Group
3	Public
4	Resource
\.


--
-- Name: roles_role_no_seq; Type: SEQUENCE SET; Schema: public; Owner: davical_dba
--

SELECT pg_catalog.setval('roles_role_no_seq', 10, true);


--
-- Data for Name: session; Type: TABLE DATA; Schema: public; Owner: davical_dba
--

COPY session (session_id, user_no, session_start, session_end, session_key, session_config) FROM stdin;
8	1	2014-11-28 16:10:15.347902+01	2014-11-28 16:10:28.102587+01	8102ce71ee5ae905c88808f38e782a50	\N
1	1	2014-11-13 15:26:23.034227+01	2014-11-13 16:55:57.728446+01	93c26dcf440f5dadcf26cc2af3a88b1c	\N
3	1	2014-11-16 20:11:36.79245+01	2014-11-16 20:18:42.222311+01	7137b3bca7d9e39480b1eef38b654796	\N
4	1	2014-11-16 20:18:51.88633+01	2014-11-16 20:18:51.88633+01	25bc8c84373e07886dbbc0de4020d884	\N
2	1001	2014-11-16 16:55:18.47578+01	2014-11-16 23:11:49.076098+01	4e1f3feab729d02f6ee699a5653a68e1	\N
9	1	2014-11-29 15:44:38.851078+01	2014-11-30 15:45:16.243811+01	90db4aad78de10d35f6859d15267b238	\N
5	1	2014-11-16 23:12:04.261203+01	2014-11-16 23:38:54.42233+01	2ad816090fdc2c57b04a2f26173c27d9	\N
6	1	2014-11-16 23:38:59.989858+01	2014-11-16 23:38:59.989858+01	f456f85a897474c2ab684dca160a8a00	\N
7	1	2014-11-28 14:28:52.957559+01	2014-11-28 15:27:16.34949+01	daebe3d9e09ef5eccb1fa1ef86981cb9	\N
\.


--
-- Name: session_session_id_seq; Type: SEQUENCE SET; Schema: public; Owner: davical_dba
--

SELECT pg_catalog.setval('session_session_id_seq', 9, true);


--
-- Data for Name: supported_locales; Type: TABLE DATA; Schema: public; Owner: davical_dba
--

COPY supported_locales (locale, locale_name_en, locale_name_locale) FROM stdin;
en	English	English
de	German	Deutsch
es_AR	Spanish (Argentina)	Español (Argentina)
es_ES	Spanish (Spain)	Español (Espana)
es_MX	Spanish (Mexico)	Español (Mexico)
es_VE	Spanish (Venezuela)	Español (Venezuela)
et	Estonian	Eesti
fr	French	Français
hu	Hungarian	Magyar
it	Italian	Italiano
ja	Japanese	日本語
nb	Norwegian	Bokmål
nl	Netherlands	Nederlands
pl	Polish	Polski
pt_BR	Brazilian Portuguese	Brazilian Portuguese
pt_PT	Portuguese	Portuguese
ru	Russian	Русский
sv	Swedish	Svenska
\.


--
-- Data for Name: sync_changes; Type: TABLE DATA; Schema: public; Owner: davical_dba
--

COPY sync_changes (sync_time, collection_id, sync_status, dav_id, dav_name) FROM stdin;
2014-11-16 17:09:39.004116+01	1002	201	1006	/richard/calendar/B0A6BA3E-CA2F-4D98-99A9-07BF5B495071.ics
2014-11-16 17:10:32.632026+01	1002	200	1006	/richard/calendar/B0A6BA3E-CA2F-4D98-99A9-07BF5B495071.ics
2014-11-16 17:10:57.765515+01	1005	201	1007	/richard/96EECC04-B54C-4365-ACE0-D872DC2F116F/B0A6BA3E-CA2F-4D98-99A9-07BF5B495071.ics
2014-11-16 17:10:58.006666+01	1002	404	1006	/richard/calendar/B0A6BA3E-CA2F-4D98-99A9-07BF5B495071.ics
2014-11-29 15:48:15.968519+01	1004	201	1008	/admin/8A6B64D7-8155-4713-996A-5B4AB71472D2/0741F2A5-D3D8-4E69-8911-7B9869EFC0C2.ics
\.


--
-- Data for Name: sync_tokens; Type: TABLE DATA; Schema: public; Owner: davical_dba
--

COPY sync_tokens (sync_token, collection_id, modification_time) FROM stdin;
1	1002	2014-11-16 17:09:39.004116+01
2	1005	2014-11-16 17:10:57.765515+01
3	1004	2014-11-29 15:48:15.968519+01
\.


--
-- Name: sync_tokens_sync_token_seq; Type: SEQUENCE SET; Schema: public; Owner: davical_dba
--

SELECT pg_catalog.setval('sync_tokens_sync_token_seq', 3, true);


--
-- Data for Name: timezones; Type: TABLE DATA; Schema: public; Owner: davical_dba
--

COPY timezones (our_tzno, tzid, olson_name, active, last_modified, etag, vtimezone) FROM stdin;
1	Europe/Madrid	Europe/Madrid	f	2014-11-16 17:09:39.004116	\N	BEGIN:VTIMEZONE\r\nTZID:Europe/Madrid\r\nX-LIC-LOCATION:Europe/Madrid\r\nBEGIN:STANDARD\r\nDTSTART:20141026T030000\r\nTZOFFSETFROM:+0200\r\nTZOFFSETTO:+0100\r\nRDATE:20151025T030000\r\nTZNAME:CET\r\nEND:STANDARD\r\nBEGIN:DAYLIGHT\r\nDTSTART:20150329T020000\r\nTZOFFSETFROM:+0100\r\nTZOFFSETTO:+0200\r\nTZNAME:CEST\r\nEND:DAYLIGHT\r\nEND:VTIMEZONE\r\n
\.


--
-- Name: timezones_our_tzno_seq; Type: SEQUENCE SET; Schema: public; Owner: davical_dba
--

SELECT pg_catalog.setval('timezones_our_tzno_seq', 1, true);


--
-- Data for Name: tmp_password; Type: TABLE DATA; Schema: public; Owner: davical_dba
--

COPY tmp_password (user_no, password, valid_until) FROM stdin;
\.


--
-- Data for Name: tz_aliases; Type: TABLE DATA; Schema: public; Owner: davical_dba
--

COPY tz_aliases (our_tzno, tzalias) FROM stdin;
\.


--
-- Data for Name: tz_localnames; Type: TABLE DATA; Schema: public; Owner: davical_dba
--

COPY tz_localnames (our_tzno, locale, localised_name, preferred) FROM stdin;
\.


--
-- Data for Name: usr; Type: TABLE DATA; Schema: public; Owner: davical_dba
--

COPY usr (user_no, active, email_ok, joined, updated, last_used, username, password, fullname, email, config_data, date_format_type, locale) FROM stdin;
1	t	2014-11-13 00:00:00+01	2014-11-13 15:22:58.054687+01	2014-11-13 16:53:10.200275+01	2014-11-28 16:10:15.347902+01	admin	**davical1602	DAViCal Administrator	calendars@example.net	\N	E	
1001	t	\N	2014-11-13 16:54:02.137379+01	2014-11-30 15:15:24.035379+01	\N	richard	*6FzlDxl3U*{SSHA}7zfqFsm4OuithEg6nD8mla1bBH42RnpsRHhsM1U=	urunuela richard	ricahrd.urunuela@gmail.com	\N	E	fr
\.


--
-- Data for Name: usr_setting; Type: TABLE DATA; Schema: public; Owner: davical_dba
--

COPY usr_setting (user_no, setting_name, setting_value) FROM stdin;
\.


--
-- Name: usr_user_no_seq; Type: SEQUENCE SET; Schema: public; Owner: davical_dba
--

SELECT pg_catalog.setval('usr_user_no_seq', 1001, true);


--
-- Name: access_ticket_pkey; Type: CONSTRAINT; Schema: public; Owner: davical_dba; Tablespace: 
--

ALTER TABLE ONLY access_ticket
    ADD CONSTRAINT access_ticket_pkey PRIMARY KEY (ticket_id);


--
-- Name: addressbook_resource_pkey; Type: CONSTRAINT; Schema: public; Owner: davical_dba; Tablespace: 
--

ALTER TABLE ONLY addressbook_resource
    ADD CONSTRAINT addressbook_resource_pkey PRIMARY KEY (dav_id);


--
-- Name: caldav_data_dav_id_key; Type: CONSTRAINT; Schema: public; Owner: davical_dba; Tablespace: 
--

ALTER TABLE ONLY caldav_data
    ADD CONSTRAINT caldav_data_dav_id_key UNIQUE (dav_id);


--
-- Name: caldav_data_pkey; Type: CONSTRAINT; Schema: public; Owner: davical_dba; Tablespace: 
--

ALTER TABLE ONLY caldav_data
    ADD CONSTRAINT caldav_data_pkey PRIMARY KEY (user_no, dav_name);


--
-- Name: calendar_attendee_pkey; Type: CONSTRAINT; Schema: public; Owner: davical_dba; Tablespace: 
--

ALTER TABLE ONLY calendar_attendee
    ADD CONSTRAINT calendar_attendee_pkey PRIMARY KEY (dav_id, attendee);


--
-- Name: calendar_item_dav_id_key; Type: CONSTRAINT; Schema: public; Owner: davical_dba; Tablespace: 
--

ALTER TABLE ONLY calendar_item
    ADD CONSTRAINT calendar_item_dav_id_key UNIQUE (dav_id);


--
-- Name: calendar_item_pkey; Type: CONSTRAINT; Schema: public; Owner: davical_dba; Tablespace: 
--

ALTER TABLE ONLY calendar_item
    ADD CONSTRAINT calendar_item_pkey PRIMARY KEY (user_no, dav_name);


--
-- Name: collection_pkey; Type: CONSTRAINT; Schema: public; Owner: davical_dba; Tablespace: 
--

ALTER TABLE ONLY collection
    ADD CONSTRAINT collection_pkey PRIMARY KEY (collection_id);


--
-- Name: collection_user_no_dav_name_key; Type: CONSTRAINT; Schema: public; Owner: davical_dba; Tablespace: 
--

ALTER TABLE ONLY collection
    ADD CONSTRAINT collection_user_no_dav_name_key UNIQUE (user_no, dav_name);


--
-- Name: dav_binding_dav_name_key; Type: CONSTRAINT; Schema: public; Owner: davical_dba; Tablespace: 
--

ALTER TABLE ONLY dav_binding
    ADD CONSTRAINT dav_binding_dav_name_key UNIQUE (dav_name);


--
-- Name: dav_binding_pkey; Type: CONSTRAINT; Schema: public; Owner: davical_dba; Tablespace: 
--

ALTER TABLE ONLY dav_binding
    ADD CONSTRAINT dav_binding_pkey PRIMARY KEY (bind_id);


--
-- Name: freebusy_ticket_pkey; Type: CONSTRAINT; Schema: public; Owner: davical_dba; Tablespace: 
--

ALTER TABLE ONLY freebusy_ticket
    ADD CONSTRAINT freebusy_ticket_pkey PRIMARY KEY (ticket_id);


--
-- Name: locks_opaquelocktoken_key; Type: CONSTRAINT; Schema: public; Owner: davical_dba; Tablespace: 
--

ALTER TABLE ONLY locks
    ADD CONSTRAINT locks_opaquelocktoken_key UNIQUE (opaquelocktoken);


--
-- Name: principal_pkey; Type: CONSTRAINT; Schema: public; Owner: davical_dba; Tablespace: 
--

ALTER TABLE ONLY principal
    ADD CONSTRAINT principal_pkey PRIMARY KEY (principal_id);


--
-- Name: principal_type_pkey; Type: CONSTRAINT; Schema: public; Owner: davical_dba; Tablespace: 
--

ALTER TABLE ONLY principal_type
    ADD CONSTRAINT principal_type_pkey PRIMARY KEY (principal_type_id);


--
-- Name: property_pkey; Type: CONSTRAINT; Schema: public; Owner: davical_dba; Tablespace: 
--

ALTER TABLE ONLY property
    ADD CONSTRAINT property_pkey PRIMARY KEY (dav_name, property_name);


--
-- Name: relationship_pkey; Type: CONSTRAINT; Schema: public; Owner: davical_dba; Tablespace: 
--

ALTER TABLE ONLY relationship
    ADD CONSTRAINT relationship_pkey PRIMARY KEY (from_user, to_user, rt_id);


--
-- Name: relationship_type_pkey; Type: CONSTRAINT; Schema: public; Owner: davical_dba; Tablespace: 
--

ALTER TABLE ONLY relationship_type
    ADD CONSTRAINT relationship_type_pkey PRIMARY KEY (rt_id);


--
-- Name: roles_pkey; Type: CONSTRAINT; Schema: public; Owner: davical_dba; Tablespace: 
--

ALTER TABLE ONLY roles
    ADD CONSTRAINT roles_pkey PRIMARY KEY (role_no);


--
-- Name: session_pkey; Type: CONSTRAINT; Schema: public; Owner: davical_dba; Tablespace: 
--

ALTER TABLE ONLY session
    ADD CONSTRAINT session_pkey PRIMARY KEY (session_id);


--
-- Name: supported_locales_pkey; Type: CONSTRAINT; Schema: public; Owner: davical_dba; Tablespace: 
--

ALTER TABLE ONLY supported_locales
    ADD CONSTRAINT supported_locales_pkey PRIMARY KEY (locale);


--
-- Name: sync_tokens_pkey; Type: CONSTRAINT; Schema: public; Owner: davical_dba; Tablespace: 
--

ALTER TABLE ONLY sync_tokens
    ADD CONSTRAINT sync_tokens_pkey PRIMARY KEY (sync_token);


--
-- Name: timezones_pkey; Type: CONSTRAINT; Schema: public; Owner: davical_dba; Tablespace: 
--

ALTER TABLE ONLY timezones
    ADD CONSTRAINT timezones_pkey PRIMARY KEY (our_tzno);


--
-- Name: timezones_tzid_key; Type: CONSTRAINT; Schema: public; Owner: davical_dba; Tablespace: 
--

ALTER TABLE ONLY timezones
    ADD CONSTRAINT timezones_tzid_key UNIQUE (tzid);


--
-- Name: unique_path; Type: CONSTRAINT; Schema: public; Owner: davical_dba; Tablespace: 
--

ALTER TABLE ONLY collection
    ADD CONSTRAINT unique_path UNIQUE (dav_name);


--
-- Name: unique_user; Type: CONSTRAINT; Schema: public; Owner: davical_dba; Tablespace: 
--

ALTER TABLE ONLY principal
    ADD CONSTRAINT unique_user UNIQUE (user_no);


--
-- Name: usr_pkey; Type: CONSTRAINT; Schema: public; Owner: davical_dba; Tablespace: 
--

ALTER TABLE ONLY usr
    ADD CONSTRAINT usr_pkey PRIMARY KEY (user_no);


--
-- Name: usr_setting_pkey; Type: CONSTRAINT; Schema: public; Owner: davical_dba; Tablespace: 
--

ALTER TABLE ONLY usr_setting
    ADD CONSTRAINT usr_setting_pkey PRIMARY KEY (user_no, setting_name);


--
-- Name: caldav_data_collection_id_fkey; Type: INDEX; Schema: public; Owner: davical_dba; Tablespace: 
--

CREATE INDEX caldav_data_collection_id_fkey ON caldav_data USING btree (collection_id);


--
-- Name: calendar_item_collection_id_fkey; Type: INDEX; Schema: public; Owner: davical_dba; Tablespace: 
--

CREATE INDEX calendar_item_collection_id_fkey ON calendar_item USING btree (collection_id);


--
-- Name: grants_pk1; Type: INDEX; Schema: public; Owner: davical_dba; Tablespace: 
--

CREATE UNIQUE INDEX grants_pk1 ON grants USING btree (by_principal, to_principal);


--
-- Name: grants_pk2; Type: INDEX; Schema: public; Owner: davical_dba; Tablespace: 
--

CREATE UNIQUE INDEX grants_pk2 ON grants USING btree (by_collection, to_principal);


--
-- Name: group_member_pk; Type: INDEX; Schema: public; Owner: davical_dba; Tablespace: 
--

CREATE UNIQUE INDEX group_member_pk ON group_member USING btree (group_id, member_id);


--
-- Name: group_member_sk; Type: INDEX; Schema: public; Owner: davical_dba; Tablespace: 
--

CREATE INDEX group_member_sk ON group_member USING btree (member_id);


--
-- Name: locks_dav_name_idx; Type: INDEX; Schema: public; Owner: davical_dba; Tablespace: 
--

CREATE INDEX locks_dav_name_idx ON locks USING btree (dav_name);


--
-- Name: properties_dav_name_idx; Type: INDEX; Schema: public; Owner: davical_dba; Tablespace: 
--

CREATE INDEX properties_dav_name_idx ON property USING btree (dav_name);


--
-- Name: sync_processing_index; Type: INDEX; Schema: public; Owner: davical_dba; Tablespace: 
--

CREATE INDEX sync_processing_index ON sync_changes USING btree (collection_id, dav_id, sync_time);


--
-- Name: usr_sk1_unique_username; Type: INDEX; Schema: public; Owner: davical_dba; Tablespace: 
--

CREATE UNIQUE INDEX usr_sk1_unique_username ON usr USING btree (lower(username));


--
-- Name: dav_principal_delete; Type: RULE; Schema: public; Owner: davical_dba
--

CREATE RULE dav_principal_delete AS ON DELETE TO dav_principal DO INSTEAD (DELETE FROM usr WHERE (usr.user_no = old.user_no); DELETE FROM principal WHERE (principal.principal_id = old.principal_id); );


--
-- Name: dav_principal_insert; Type: RULE; Schema: public; Owner: davical_dba
--

CREATE RULE dav_principal_insert AS ON INSERT TO dav_principal DO INSTEAD (INSERT INTO usr (user_no, active, joined, updated, username, password, fullname, email, email_ok, date_format_type, locale) VALUES (COALESCE(new.user_no, nextval('usr_user_no_seq'::regclass)), COALESCE(new.user_active, true), COALESCE(new.created, now()), COALESCE(new.modified, now()), new.username, new.password, COALESCE(new.fullname, new.displayname), new.email, new.email_ok, COALESCE(new.date_format_type, 'E'::text), new.locale); INSERT INTO principal (user_no, principal_id, type_id, displayname, default_privileges) VALUES (COALESCE(new.user_no, currval('usr_user_no_seq'::regclass)), COALESCE(new.principal_id, nextval('dav_id_seq'::regclass)), new.type_id, COALESCE(new.displayname, new.fullname), COALESCE(new.default_privileges, (0)::bit(24))); );


--
-- Name: dav_principal_update; Type: RULE; Schema: public; Owner: davical_dba
--

CREATE RULE dav_principal_update AS ON UPDATE TO dav_principal DO INSTEAD (UPDATE usr SET user_no = new.user_no, active = new.user_active, updated = now(), username = new.username, password = new.password, fullname = new.fullname, email = new.email, email_ok = new.email_ok, date_format_type = new.date_format_type, locale = new.locale WHERE (usr.user_no = old.user_no); UPDATE principal SET principal_id = new.principal_id, type_id = new.type_id, displayname = new.displayname, default_privileges = new.default_privileges WHERE (principal.principal_id = old.principal_id); );


--
-- Name: alarm_changed; Type: TRIGGER; Schema: public; Owner: davical_dba
--

CREATE TRIGGER alarm_changed AFTER UPDATE ON calendar_alarm FOR EACH ROW EXECUTE PROCEDURE alarm_changed();


--
-- Name: caldav_data_modified; Type: TRIGGER; Schema: public; Owner: davical_dba
--

CREATE TRIGGER caldav_data_modified AFTER INSERT OR DELETE OR UPDATE ON caldav_data FOR EACH ROW EXECUTE PROCEDURE caldav_data_modified();


--
-- Name: caldav_data_sync_dav_id; Type: TRIGGER; Schema: public; Owner: davical_dba
--

CREATE TRIGGER caldav_data_sync_dav_id AFTER INSERT OR UPDATE ON caldav_data FOR EACH ROW EXECUTE PROCEDURE sync_dav_id();


--
-- Name: collection_modified; Type: TRIGGER; Schema: public; Owner: davical_dba
--

CREATE TRIGGER collection_modified AFTER UPDATE ON collection FOR EACH ROW EXECUTE PROCEDURE collection_modified();


--
-- Name: grants_modified; Type: TRIGGER; Schema: public; Owner: davical_dba
--

CREATE TRIGGER grants_modified AFTER INSERT OR UPDATE ON grants FOR EACH ROW EXECUTE PROCEDURE grants_modified();


--
-- Name: principal_modified; Type: TRIGGER; Schema: public; Owner: davical_dba
--

CREATE TRIGGER principal_modified AFTER UPDATE ON principal FOR EACH ROW EXECUTE PROCEDURE principal_modified();


--
-- Name: usr_modified; Type: TRIGGER; Schema: public; Owner: davical_dba
--

CREATE TRIGGER usr_modified AFTER UPDATE ON usr FOR EACH ROW EXECUTE PROCEDURE usr_modified();


--
-- Name: access_ticket_dav_owner_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: davical_dba
--

ALTER TABLE ONLY access_ticket
    ADD CONSTRAINT access_ticket_dav_owner_id_fkey FOREIGN KEY (dav_owner_id) REFERENCES principal(principal_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: access_ticket_target_collection_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: davical_dba
--

ALTER TABLE ONLY access_ticket
    ADD CONSTRAINT access_ticket_target_collection_id_fkey FOREIGN KEY (target_collection_id) REFERENCES collection(collection_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: access_ticket_target_resource_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: davical_dba
--

ALTER TABLE ONLY access_ticket
    ADD CONSTRAINT access_ticket_target_resource_id_fkey FOREIGN KEY (target_resource_id) REFERENCES caldav_data(dav_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: addressbook_address_adr_dav_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: davical_dba
--

ALTER TABLE ONLY addressbook_address_adr
    ADD CONSTRAINT addressbook_address_adr_dav_id_fkey FOREIGN KEY (dav_id) REFERENCES caldav_data(dav_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: addressbook_address_email_dav_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: davical_dba
--

ALTER TABLE ONLY addressbook_address_email
    ADD CONSTRAINT addressbook_address_email_dav_id_fkey FOREIGN KEY (dav_id) REFERENCES caldav_data(dav_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: addressbook_address_tel_dav_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: davical_dba
--

ALTER TABLE ONLY addressbook_address_tel
    ADD CONSTRAINT addressbook_address_tel_dav_id_fkey FOREIGN KEY (dav_id) REFERENCES caldav_data(dav_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: addressbook_resource_dav_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: davical_dba
--

ALTER TABLE ONLY addressbook_resource
    ADD CONSTRAINT addressbook_resource_dav_id_fkey FOREIGN KEY (dav_id) REFERENCES caldav_data(dav_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: caldav_data_collection_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: davical_dba
--

ALTER TABLE ONLY caldav_data
    ADD CONSTRAINT caldav_data_collection_id_fkey FOREIGN KEY (collection_id) REFERENCES collection(collection_id) ON UPDATE CASCADE ON DELETE CASCADE DEFERRABLE;


--
-- Name: caldav_data_logged_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: davical_dba
--

ALTER TABLE ONLY caldav_data
    ADD CONSTRAINT caldav_data_logged_user_fkey FOREIGN KEY (logged_user) REFERENCES usr(user_no) ON UPDATE CASCADE ON DELETE SET DEFAULT DEFERRABLE;


--
-- Name: caldav_data_user_no_fkey; Type: FK CONSTRAINT; Schema: public; Owner: davical_dba
--

ALTER TABLE ONLY caldav_data
    ADD CONSTRAINT caldav_data_user_no_fkey FOREIGN KEY (user_no) REFERENCES usr(user_no) ON UPDATE CASCADE ON DELETE CASCADE DEFERRABLE;


--
-- Name: caldav_exists; Type: FK CONSTRAINT; Schema: public; Owner: davical_dba
--

ALTER TABLE ONLY calendar_item
    ADD CONSTRAINT caldav_exists FOREIGN KEY (user_no, dav_name) REFERENCES caldav_data(user_no, dav_name) MATCH FULL ON UPDATE CASCADE ON DELETE CASCADE DEFERRABLE;


--
-- Name: calendar_alarm_dav_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: davical_dba
--

ALTER TABLE ONLY calendar_alarm
    ADD CONSTRAINT calendar_alarm_dav_id_fkey FOREIGN KEY (dav_id) REFERENCES caldav_data(dav_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: calendar_attendee_dav_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: davical_dba
--

ALTER TABLE ONLY calendar_attendee
    ADD CONSTRAINT calendar_attendee_dav_id_fkey FOREIGN KEY (dav_id) REFERENCES caldav_data(dav_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: calendar_item_collection_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: davical_dba
--

ALTER TABLE ONLY calendar_item
    ADD CONSTRAINT calendar_item_collection_id_fkey FOREIGN KEY (collection_id) REFERENCES collection(collection_id) ON UPDATE CASCADE ON DELETE CASCADE DEFERRABLE;


--
-- Name: calendar_item_tz_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: davical_dba
--

ALTER TABLE ONLY calendar_item
    ADD CONSTRAINT calendar_item_tz_id_fkey FOREIGN KEY (tz_id) REFERENCES timezones(tzid);


--
-- Name: calendar_item_user_no_fkey; Type: FK CONSTRAINT; Schema: public; Owner: davical_dba
--

ALTER TABLE ONLY calendar_item
    ADD CONSTRAINT calendar_item_user_no_fkey FOREIGN KEY (user_no) REFERENCES usr(user_no) ON UPDATE CASCADE ON DELETE CASCADE DEFERRABLE;


--
-- Name: collection_timezone_fkey; Type: FK CONSTRAINT; Schema: public; Owner: davical_dba
--

ALTER TABLE ONLY collection
    ADD CONSTRAINT collection_timezone_fkey FOREIGN KEY (timezone) REFERENCES timezones(tzid) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: collection_user_no_fkey; Type: FK CONSTRAINT; Schema: public; Owner: davical_dba
--

ALTER TABLE ONLY collection
    ADD CONSTRAINT collection_user_no_fkey FOREIGN KEY (user_no) REFERENCES usr(user_no) ON UPDATE CASCADE ON DELETE CASCADE DEFERRABLE;


--
-- Name: dav_binding_access_ticket_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: davical_dba
--

ALTER TABLE ONLY dav_binding
    ADD CONSTRAINT dav_binding_access_ticket_id_fkey FOREIGN KEY (access_ticket_id) REFERENCES access_ticket(ticket_id) ON UPDATE CASCADE ON DELETE SET NULL;


--
-- Name: dav_binding_bound_source_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: davical_dba
--

ALTER TABLE ONLY dav_binding
    ADD CONSTRAINT dav_binding_bound_source_id_fkey FOREIGN KEY (bound_source_id) REFERENCES collection(collection_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: dav_binding_dav_owner_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: davical_dba
--

ALTER TABLE ONLY dav_binding
    ADD CONSTRAINT dav_binding_dav_owner_id_fkey FOREIGN KEY (dav_owner_id) REFERENCES principal(principal_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: freebusy_ticket_user_no_fkey; Type: FK CONSTRAINT; Schema: public; Owner: davical_dba
--

ALTER TABLE ONLY freebusy_ticket
    ADD CONSTRAINT freebusy_ticket_user_no_fkey FOREIGN KEY (user_no) REFERENCES usr(user_no) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: grants_by_collection_fkey; Type: FK CONSTRAINT; Schema: public; Owner: davical_dba
--

ALTER TABLE ONLY grants
    ADD CONSTRAINT grants_by_collection_fkey FOREIGN KEY (by_collection) REFERENCES collection(collection_id) ON UPDATE CASCADE ON DELETE CASCADE DEFERRABLE;


--
-- Name: grants_by_principal_fkey; Type: FK CONSTRAINT; Schema: public; Owner: davical_dba
--

ALTER TABLE ONLY grants
    ADD CONSTRAINT grants_by_principal_fkey FOREIGN KEY (by_principal) REFERENCES principal(principal_id) ON UPDATE CASCADE ON DELETE CASCADE DEFERRABLE;


--
-- Name: grants_to_principal_fkey; Type: FK CONSTRAINT; Schema: public; Owner: davical_dba
--

ALTER TABLE ONLY grants
    ADD CONSTRAINT grants_to_principal_fkey FOREIGN KEY (to_principal) REFERENCES principal(principal_id) ON UPDATE CASCADE ON DELETE CASCADE DEFERRABLE;


--
-- Name: group_member_group_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: davical_dba
--

ALTER TABLE ONLY group_member
    ADD CONSTRAINT group_member_group_id_fkey FOREIGN KEY (group_id) REFERENCES principal(principal_id) ON UPDATE CASCADE ON DELETE CASCADE DEFERRABLE;


--
-- Name: group_member_member_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: davical_dba
--

ALTER TABLE ONLY group_member
    ADD CONSTRAINT group_member_member_id_fkey FOREIGN KEY (member_id) REFERENCES principal(principal_id) ON UPDATE CASCADE ON DELETE CASCADE DEFERRABLE;


--
-- Name: principal_type_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: davical_dba
--

ALTER TABLE ONLY principal
    ADD CONSTRAINT principal_type_id_fkey FOREIGN KEY (type_id) REFERENCES principal_type(principal_type_id) ON UPDATE CASCADE ON DELETE RESTRICT DEFERRABLE;


--
-- Name: principal_user_no_fkey; Type: FK CONSTRAINT; Schema: public; Owner: davical_dba
--

ALTER TABLE ONLY principal
    ADD CONSTRAINT principal_user_no_fkey FOREIGN KEY (user_no) REFERENCES usr(user_no) ON UPDATE CASCADE ON DELETE CASCADE DEFERRABLE;


--
-- Name: property_changed_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: davical_dba
--

ALTER TABLE ONLY property
    ADD CONSTRAINT property_changed_by_fkey FOREIGN KEY (changed_by) REFERENCES usr(user_no) ON UPDATE CASCADE ON DELETE SET DEFAULT;


--
-- Name: relationship_from_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: davical_dba
--

ALTER TABLE ONLY relationship
    ADD CONSTRAINT relationship_from_user_fkey FOREIGN KEY (from_user) REFERENCES usr(user_no) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: relationship_rt_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: davical_dba
--

ALTER TABLE ONLY relationship
    ADD CONSTRAINT relationship_rt_id_fkey FOREIGN KEY (rt_id) REFERENCES relationship_type(rt_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: relationship_to_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: davical_dba
--

ALTER TABLE ONLY relationship
    ADD CONSTRAINT relationship_to_user_fkey FOREIGN KEY (to_user) REFERENCES usr(user_no) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: role_member_role_no_fkey; Type: FK CONSTRAINT; Schema: public; Owner: davical_dba
--

ALTER TABLE ONLY role_member
    ADD CONSTRAINT role_member_role_no_fkey FOREIGN KEY (role_no) REFERENCES roles(role_no);


--
-- Name: role_member_user_no_fkey; Type: FK CONSTRAINT; Schema: public; Owner: davical_dba
--

ALTER TABLE ONLY role_member
    ADD CONSTRAINT role_member_user_no_fkey FOREIGN KEY (user_no) REFERENCES usr(user_no) ON DELETE CASCADE;


--
-- Name: session_user_no_fkey; Type: FK CONSTRAINT; Schema: public; Owner: davical_dba
--

ALTER TABLE ONLY session
    ADD CONSTRAINT session_user_no_fkey FOREIGN KEY (user_no) REFERENCES usr(user_no) ON DELETE CASCADE;


--
-- Name: sync_changes_collection_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: davical_dba
--

ALTER TABLE ONLY sync_changes
    ADD CONSTRAINT sync_changes_collection_id_fkey FOREIGN KEY (collection_id) REFERENCES collection(collection_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: sync_tokens_collection_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: davical_dba
--

ALTER TABLE ONLY sync_tokens
    ADD CONSTRAINT sync_tokens_collection_id_fkey FOREIGN KEY (collection_id) REFERENCES collection(collection_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: tmp_password_user_no_fkey; Type: FK CONSTRAINT; Schema: public; Owner: davical_dba
--

ALTER TABLE ONLY tmp_password
    ADD CONSTRAINT tmp_password_user_no_fkey FOREIGN KEY (user_no) REFERENCES usr(user_no);


--
-- Name: tz_aliases_our_tzno_fkey; Type: FK CONSTRAINT; Schema: public; Owner: davical_dba
--

ALTER TABLE ONLY tz_aliases
    ADD CONSTRAINT tz_aliases_our_tzno_fkey FOREIGN KEY (our_tzno) REFERENCES timezones(our_tzno);


--
-- Name: tz_localnames_our_tzno_fkey; Type: FK CONSTRAINT; Schema: public; Owner: davical_dba
--

ALTER TABLE ONLY tz_localnames
    ADD CONSTRAINT tz_localnames_our_tzno_fkey FOREIGN KEY (our_tzno) REFERENCES timezones(our_tzno);


--
-- Name: usr_setting_user_no_fkey; Type: FK CONSTRAINT; Schema: public; Owner: davical_dba
--

ALTER TABLE ONLY usr_setting
    ADD CONSTRAINT usr_setting_user_no_fkey FOREIGN KEY (user_no) REFERENCES usr(user_no) ON DELETE CASCADE;


--
-- Name: public; Type: ACL; Schema: -; Owner: postgres
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM postgres;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO PUBLIC;


--
-- Name: access_ticket; Type: ACL; Schema: public; Owner: davical_dba
--

REVOKE ALL ON TABLE access_ticket FROM PUBLIC;
REVOKE ALL ON TABLE access_ticket FROM davical_dba;
GRANT ALL ON TABLE access_ticket TO davical_dba;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE access_ticket TO davical_app;


--
-- Name: addressbook_address_adr; Type: ACL; Schema: public; Owner: davical_dba
--

REVOKE ALL ON TABLE addressbook_address_adr FROM PUBLIC;
REVOKE ALL ON TABLE addressbook_address_adr FROM davical_dba;
GRANT ALL ON TABLE addressbook_address_adr TO davical_dba;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE addressbook_address_adr TO davical_app;


--
-- Name: addressbook_address_email; Type: ACL; Schema: public; Owner: davical_dba
--

REVOKE ALL ON TABLE addressbook_address_email FROM PUBLIC;
REVOKE ALL ON TABLE addressbook_address_email FROM davical_dba;
GRANT ALL ON TABLE addressbook_address_email TO davical_dba;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE addressbook_address_email TO davical_app;


--
-- Name: addressbook_address_tel; Type: ACL; Schema: public; Owner: davical_dba
--

REVOKE ALL ON TABLE addressbook_address_tel FROM PUBLIC;
REVOKE ALL ON TABLE addressbook_address_tel FROM davical_dba;
GRANT ALL ON TABLE addressbook_address_tel TO davical_dba;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE addressbook_address_tel TO davical_app;


--
-- Name: addressbook_resource; Type: ACL; Schema: public; Owner: davical_dba
--

REVOKE ALL ON TABLE addressbook_resource FROM PUBLIC;
REVOKE ALL ON TABLE addressbook_resource FROM davical_dba;
GRANT ALL ON TABLE addressbook_resource TO davical_dba;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE addressbook_resource TO davical_app;


--
-- Name: awl_db_revision; Type: ACL; Schema: public; Owner: davical_dba
--

REVOKE ALL ON TABLE awl_db_revision FROM PUBLIC;
REVOKE ALL ON TABLE awl_db_revision FROM davical_dba;
GRANT ALL ON TABLE awl_db_revision TO davical_dba;
GRANT SELECT ON TABLE awl_db_revision TO davical_app;


--
-- Name: dav_id_seq; Type: ACL; Schema: public; Owner: davical_dba
--

REVOKE ALL ON SEQUENCE dav_id_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE dav_id_seq FROM davical_dba;
GRANT ALL ON SEQUENCE dav_id_seq TO davical_dba;
GRANT SELECT,UPDATE ON SEQUENCE dav_id_seq TO davical_app;


--
-- Name: caldav_data; Type: ACL; Schema: public; Owner: davical_dba
--

REVOKE ALL ON TABLE caldav_data FROM PUBLIC;
REVOKE ALL ON TABLE caldav_data FROM davical_dba;
GRANT ALL ON TABLE caldav_data TO davical_dba;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE caldav_data TO davical_app;


--
-- Name: calendar_alarm; Type: ACL; Schema: public; Owner: davical_dba
--

REVOKE ALL ON TABLE calendar_alarm FROM PUBLIC;
REVOKE ALL ON TABLE calendar_alarm FROM davical_dba;
GRANT ALL ON TABLE calendar_alarm TO davical_dba;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE calendar_alarm TO davical_app;


--
-- Name: calendar_attendee; Type: ACL; Schema: public; Owner: davical_dba
--

REVOKE ALL ON TABLE calendar_attendee FROM PUBLIC;
REVOKE ALL ON TABLE calendar_attendee FROM davical_dba;
GRANT ALL ON TABLE calendar_attendee TO davical_dba;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE calendar_attendee TO davical_app;


--
-- Name: calendar_item; Type: ACL; Schema: public; Owner: davical_dba
--

REVOKE ALL ON TABLE calendar_item FROM PUBLIC;
REVOKE ALL ON TABLE calendar_item FROM davical_dba;
GRANT ALL ON TABLE calendar_item TO davical_dba;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE calendar_item TO davical_app;


--
-- Name: collection; Type: ACL; Schema: public; Owner: davical_dba
--

REVOKE ALL ON TABLE collection FROM PUBLIC;
REVOKE ALL ON TABLE collection FROM davical_dba;
GRANT ALL ON TABLE collection TO davical_dba;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE collection TO davical_app;


--
-- Name: dav_binding; Type: ACL; Schema: public; Owner: davical_dba
--

REVOKE ALL ON TABLE dav_binding FROM PUBLIC;
REVOKE ALL ON TABLE dav_binding FROM davical_dba;
GRANT ALL ON TABLE dav_binding TO davical_dba;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE dav_binding TO davical_app;


--
-- Name: principal; Type: ACL; Schema: public; Owner: davical_dba
--

REVOKE ALL ON TABLE principal FROM PUBLIC;
REVOKE ALL ON TABLE principal FROM davical_dba;
GRANT ALL ON TABLE principal TO davical_dba;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE principal TO davical_app;


--
-- Name: usr; Type: ACL; Schema: public; Owner: davical_dba
--

REVOKE ALL ON TABLE usr FROM PUBLIC;
REVOKE ALL ON TABLE usr FROM davical_dba;
GRANT ALL ON TABLE usr TO davical_dba;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE usr TO davical_app;


--
-- Name: dav_principal; Type: ACL; Schema: public; Owner: davical_dba
--

REVOKE ALL ON TABLE dav_principal FROM PUBLIC;
REVOKE ALL ON TABLE dav_principal FROM davical_dba;
GRANT ALL ON TABLE dav_principal TO davical_dba;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE dav_principal TO davical_app;


--
-- Name: freebusy_ticket; Type: ACL; Schema: public; Owner: davical_dba
--

REVOKE ALL ON TABLE freebusy_ticket FROM PUBLIC;
REVOKE ALL ON TABLE freebusy_ticket FROM davical_dba;
GRANT ALL ON TABLE freebusy_ticket TO davical_dba;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE freebusy_ticket TO davical_app;


--
-- Name: grants; Type: ACL; Schema: public; Owner: davical_dba
--

REVOKE ALL ON TABLE grants FROM PUBLIC;
REVOKE ALL ON TABLE grants FROM davical_dba;
GRANT ALL ON TABLE grants TO davical_dba;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE grants TO davical_app;


--
-- Name: group_member; Type: ACL; Schema: public; Owner: davical_dba
--

REVOKE ALL ON TABLE group_member FROM PUBLIC;
REVOKE ALL ON TABLE group_member FROM davical_dba;
GRANT ALL ON TABLE group_member TO davical_dba;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE group_member TO davical_app;


--
-- Name: locks; Type: ACL; Schema: public; Owner: davical_dba
--

REVOKE ALL ON TABLE locks FROM PUBLIC;
REVOKE ALL ON TABLE locks FROM davical_dba;
GRANT ALL ON TABLE locks TO davical_dba;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE locks TO davical_app;


--
-- Name: principal_type; Type: ACL; Schema: public; Owner: davical_dba
--

REVOKE ALL ON TABLE principal_type FROM PUBLIC;
REVOKE ALL ON TABLE principal_type FROM davical_dba;
GRANT ALL ON TABLE principal_type TO davical_dba;
GRANT SELECT ON TABLE principal_type TO davical_app;


--
-- Name: principal_type_principal_type_id_seq; Type: ACL; Schema: public; Owner: davical_dba
--

REVOKE ALL ON SEQUENCE principal_type_principal_type_id_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE principal_type_principal_type_id_seq FROM davical_dba;
GRANT ALL ON SEQUENCE principal_type_principal_type_id_seq TO davical_dba;
GRANT SELECT,UPDATE ON SEQUENCE principal_type_principal_type_id_seq TO davical_app;


--
-- Name: property; Type: ACL; Schema: public; Owner: davical_dba
--

REVOKE ALL ON TABLE property FROM PUBLIC;
REVOKE ALL ON TABLE property FROM davical_dba;
GRANT ALL ON TABLE property TO davical_dba;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE property TO davical_app;


--
-- Name: relationship; Type: ACL; Schema: public; Owner: davical_dba
--

REVOKE ALL ON TABLE relationship FROM PUBLIC;
REVOKE ALL ON TABLE relationship FROM davical_dba;
GRANT ALL ON TABLE relationship TO davical_dba;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE relationship TO davical_app;


--
-- Name: relationship_type; Type: ACL; Schema: public; Owner: davical_dba
--

REVOKE ALL ON TABLE relationship_type FROM PUBLIC;
REVOKE ALL ON TABLE relationship_type FROM davical_dba;
GRANT ALL ON TABLE relationship_type TO davical_dba;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE relationship_type TO davical_app;


--
-- Name: relationship_type_rt_id_seq; Type: ACL; Schema: public; Owner: davical_dba
--

REVOKE ALL ON SEQUENCE relationship_type_rt_id_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE relationship_type_rt_id_seq FROM davical_dba;
GRANT ALL ON SEQUENCE relationship_type_rt_id_seq TO davical_dba;
GRANT SELECT,UPDATE ON SEQUENCE relationship_type_rt_id_seq TO davical_app;


--
-- Name: role_member; Type: ACL; Schema: public; Owner: davical_dba
--

REVOKE ALL ON TABLE role_member FROM PUBLIC;
REVOKE ALL ON TABLE role_member FROM davical_dba;
GRANT ALL ON TABLE role_member TO davical_dba;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE role_member TO davical_app;


--
-- Name: roles; Type: ACL; Schema: public; Owner: davical_dba
--

REVOKE ALL ON TABLE roles FROM PUBLIC;
REVOKE ALL ON TABLE roles FROM davical_dba;
GRANT ALL ON TABLE roles TO davical_dba;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE roles TO davical_app;


--
-- Name: roles_role_no_seq; Type: ACL; Schema: public; Owner: davical_dba
--

REVOKE ALL ON SEQUENCE roles_role_no_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE roles_role_no_seq FROM davical_dba;
GRANT ALL ON SEQUENCE roles_role_no_seq TO davical_dba;
GRANT SELECT,UPDATE ON SEQUENCE roles_role_no_seq TO davical_app;


--
-- Name: session; Type: ACL; Schema: public; Owner: davical_dba
--

REVOKE ALL ON TABLE session FROM PUBLIC;
REVOKE ALL ON TABLE session FROM davical_dba;
GRANT ALL ON TABLE session TO davical_dba;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE session TO davical_app;


--
-- Name: session_session_id_seq; Type: ACL; Schema: public; Owner: davical_dba
--

REVOKE ALL ON SEQUENCE session_session_id_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE session_session_id_seq FROM davical_dba;
GRANT ALL ON SEQUENCE session_session_id_seq TO davical_dba;
GRANT SELECT,UPDATE ON SEQUENCE session_session_id_seq TO davical_app;


--
-- Name: supported_locales; Type: ACL; Schema: public; Owner: davical_dba
--

REVOKE ALL ON TABLE supported_locales FROM PUBLIC;
REVOKE ALL ON TABLE supported_locales FROM davical_dba;
GRANT ALL ON TABLE supported_locales TO davical_dba;
GRANT SELECT ON TABLE supported_locales TO davical_app;


--
-- Name: sync_changes; Type: ACL; Schema: public; Owner: davical_dba
--

REVOKE ALL ON TABLE sync_changes FROM PUBLIC;
REVOKE ALL ON TABLE sync_changes FROM davical_dba;
GRANT ALL ON TABLE sync_changes TO davical_dba;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE sync_changes TO davical_app;


--
-- Name: sync_tokens; Type: ACL; Schema: public; Owner: davical_dba
--

REVOKE ALL ON TABLE sync_tokens FROM PUBLIC;
REVOKE ALL ON TABLE sync_tokens FROM davical_dba;
GRANT ALL ON TABLE sync_tokens TO davical_dba;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE sync_tokens TO davical_app;


--
-- Name: sync_tokens_sync_token_seq; Type: ACL; Schema: public; Owner: davical_dba
--

REVOKE ALL ON SEQUENCE sync_tokens_sync_token_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE sync_tokens_sync_token_seq FROM davical_dba;
GRANT ALL ON SEQUENCE sync_tokens_sync_token_seq TO davical_dba;
GRANT SELECT,UPDATE ON SEQUENCE sync_tokens_sync_token_seq TO davical_app;


--
-- Name: timezones; Type: ACL; Schema: public; Owner: davical_dba
--

REVOKE ALL ON TABLE timezones FROM PUBLIC;
REVOKE ALL ON TABLE timezones FROM davical_dba;
GRANT ALL ON TABLE timezones TO davical_dba;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE timezones TO davical_app;


--
-- Name: timezones_our_tzno_seq; Type: ACL; Schema: public; Owner: davical_dba
--

REVOKE ALL ON SEQUENCE timezones_our_tzno_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE timezones_our_tzno_seq FROM davical_dba;
GRANT ALL ON SEQUENCE timezones_our_tzno_seq TO davical_dba;
GRANT SELECT,UPDATE ON SEQUENCE timezones_our_tzno_seq TO davical_app;


--
-- Name: tmp_password; Type: ACL; Schema: public; Owner: davical_dba
--

REVOKE ALL ON TABLE tmp_password FROM PUBLIC;
REVOKE ALL ON TABLE tmp_password FROM davical_dba;
GRANT ALL ON TABLE tmp_password TO davical_dba;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE tmp_password TO davical_app;


--
-- Name: tz_aliases; Type: ACL; Schema: public; Owner: davical_dba
--

REVOKE ALL ON TABLE tz_aliases FROM PUBLIC;
REVOKE ALL ON TABLE tz_aliases FROM davical_dba;
GRANT ALL ON TABLE tz_aliases TO davical_dba;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE tz_aliases TO davical_app;


--
-- Name: tz_localnames; Type: ACL; Schema: public; Owner: davical_dba
--

REVOKE ALL ON TABLE tz_localnames FROM PUBLIC;
REVOKE ALL ON TABLE tz_localnames FROM davical_dba;
GRANT ALL ON TABLE tz_localnames TO davical_dba;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE tz_localnames TO davical_app;


--
-- Name: usr_setting; Type: ACL; Schema: public; Owner: davical_dba
--

REVOKE ALL ON TABLE usr_setting FROM PUBLIC;
REVOKE ALL ON TABLE usr_setting FROM davical_dba;
GRANT ALL ON TABLE usr_setting TO davical_dba;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE usr_setting TO davical_app;


--
-- Name: usr_user_no_seq; Type: ACL; Schema: public; Owner: davical_dba
--

REVOKE ALL ON SEQUENCE usr_user_no_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE usr_user_no_seq FROM davical_dba;
GRANT ALL ON SEQUENCE usr_user_no_seq TO davical_dba;
GRANT SELECT,UPDATE ON SEQUENCE usr_user_no_seq TO davical_app;


--
-- PostgreSQL database dump complete
--

