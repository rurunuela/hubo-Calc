package org.osaf.caldav4j;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import net.fortuna.ical4j.model.Calendar;
import net.fortuna.ical4j.model.Component;
import net.fortuna.ical4j.model.ComponentList;
import net.fortuna.ical4j.model.Date;
import net.fortuna.ical4j.model.DateTime;
import net.fortuna.ical4j.model.component.CalendarComponent;
import net.fortuna.ical4j.model.component.VEvent;
import net.fortuna.ical4j.model.property.DtStart;
import net.fortuna.ical4j.model.property.Summary;
import net.fortuna.ical4j.model.property.Uid;

import org.apache.commons.httpclient.Header;
import org.apache.commons.httpclient.HttpException;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.webdav.lib.methods.OptionsMethod;
import org.junit.After;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.osaf.caldav4j.exceptions.CalDAV4JException;
import org.osaf.caldav4j.exceptions.ResourceNotFoundException;
import org.osaf.caldav4j.functional.support.CaldavFixtureHarness;
import org.osaf.caldav4j.methods.HttpClient;
import org.osaf.caldav4j.util.ICalendarUtils;

public class CalDAVCalendarCollectionTest extends BaseTestCase {
	public CalDAVCalendarCollectionTest() {
		super();
	}

	private static final Log log = LogFactory
	.getLog(CalDAVCalendarCollectionTest.class);


	public static final Integer TEST_TIMEOUT = 3600;
	public static final boolean TEST_READ = true;
	public static final boolean TEST_WRITE = true;
	public static final Integer TEST_VISITS = CalDAVConstants.INFINITY;

	public static final String  TEST_TIMEOUT_UNITS = "Second";

	@Before
	public void setUp() throws Exception {
		super.setUp();

		CaldavFixtureHarness.provisionGoogleEvents(fixture);
	}

	@After
	public void tearDown() throws Exception {
		fixture.tearDown();
	}

	@Test
	public void testGetCalendar() throws Exception {
		CalDAVCalendarCollection calendarCollection = createCalDAVCalendarCollection();
		Calendar calendar = null;
		try {
			calendar = calendarCollection.getCalendarForEventUID(fixture.getHttpClient(),
					ICS_GOOGLE_DAILY_NY_5PM_UID);
		} catch (CalDAV4JException ce) {
			assertNull(ce);
		}

		assertNotNull(calendar);
		VEvent vevent = ICalendarUtils.getFirstEvent(calendar);
		assertNotNull(vevent);
		String summary = ICalendarUtils.getSummaryValue(vevent);
		assertEquals(ICS_GOOGLE_DAILY_NY_5PM_SUMMARY, summary);

		CalDAV4JException calDAV4JException = null;
		try {
			calendar = calendarCollection.getCalendarForEventUID(fixture.getHttpClient(),
					"NON_EXISTENT_RESOURCE");
		} catch (CalDAV4JException ce) {
			calDAV4JException = ce;
		}

		assertNotNull(calDAV4JException);
	}

	@Test
	public void testGetCalendarByPath() throws Exception {
		CalDAVCalendarCollection calendarCollection = createCalDAVCalendarCollection();
		Calendar calendar = null;
		try {
			calendar = calendarCollection.getCalendarByPath(fixture.getHttpClient(),
					ICS_GOOGLE_DAILY_NY_5PM);
		} catch (CalDAV4JException ce) {
			log.info("Error in testGetCalendarByPath"+ ce.getMessage());
			assertNull(ce);
		}

		assertNotNull(calendar);
		VEvent vevent = ICalendarUtils.getFirstEvent(calendar);
		assertNotNull(vevent);
		String summary = ICalendarUtils.getSummaryValue(vevent);
		assertEquals(ICS_GOOGLE_DAILY_NY_5PM_SUMMARY, summary);

		CalDAV4JException calDAV4JException = null;
		try {
			calendar = calendarCollection.getCalendarByPath(fixture.getHttpClient(),
					"NON_EXISTENT_RESOURCE");
		} catch (CalDAV4JException ce) {
			calDAV4JException = ce;
		}

		assertNotNull(calDAV4JException);
	}

	@Test
	public void testGetEventResources() throws Exception {
		CalDAVCalendarCollection calendarCollection = createCalDAVCalendarCollection();
		Date beginDate = ICalendarUtils.createDateTime(2006, 0, 1, null, true);
		Date endDate = ICalendarUtils.createDateTime(2006, 0, 9, null, true);
		List<Calendar> l = calendarCollection.getEventResources(fixture.getHttpClient(),
				beginDate, endDate);

		for (Calendar calendar : l) {
			ComponentList vevents = calendar.getComponents().getComponents(
					Component.VEVENT);
			VEvent ve = (VEvent) vevents.get(0);
			String uid = ICalendarUtils.getUIDValue(ve);
			int correctNumberOfEvents = -1;
			if (ICS_GOOGLE_DAILY_NY_5PM_UID.equals(uid)) {
				// one for each day
				correctNumberOfEvents = 1;
			} else if (ICS_GOOGLE_ALL_DAY_JAN1_UID.equals(uid)) {
				correctNumberOfEvents = 1;
			} else if (ICS_GOOGLE_NORMAL_PACIFIC_1PM_UID.equals(uid)) {
				correctNumberOfEvents = 1;
			} else if (ICS_GOOGLE_FLOATING_JAN2_7PM_UID.equals(uid)) {
				correctNumberOfEvents = 0;
			} else {
				fail(uid
						+ " is not the uid of any event that should have been returned");
			}

			assertEquals(correctNumberOfEvents, vevents.size());
		}

		// 3 calendars - one for each resource (not including expanded
		// recurrences)
		assertEquals(3, l.size());

	}

	// TODO wait on floating test until we can pass timezones
	@Ignore
	@Test
	public void testGetEventResourcesFloatingIssues() throws Exception {
		CalDAVCalendarCollection calendarCollection = createCalDAVCalendarCollection();

		// make sure our 7pm event gets returned
		Date beginDate = ICalendarUtils.createDateTime(2006, 0, 2, 19, 0, 0, 0,
				null, true);
		Date endDate = ICalendarUtils.createDateTime(2006, 0, 2, 20, 1, 0, 0,
				null, true);
		List<Calendar> l = calendarCollection.getEventResources(fixture.getHttpClient(),
				beginDate, endDate);
		assertTrue(hasEventWithUID(l, ICS_GOOGLE_FLOATING_JAN2_7PM_UID));

		beginDate = ICalendarUtils.createDateTime(2006, 0, 2, 20, 1, 0, 0,
				null, true);
		endDate = ICalendarUtils.createDateTime(2006, 0, 2, 20, 2, 0, 0, null,
				true);
		l = calendarCollection
		.getEventResources(fixture.getHttpClient(), beginDate, endDate);
		assertFalse(hasEventWithUID(l, ICS_GOOGLE_FLOATING_JAN2_7PM_UID));
	}



	@Test
	public void testAddNewRemove() throws Exception {
		String newUid = "NEW_UID";
		String newEvent = "NEW_EVENT";
		VEvent ve = new VEvent();

		DtStart dtStart = new DtStart(new DateTime());
		Summary summary = new Summary(newEvent);
		Uid uid = new Uid(newUid);

		ve.getProperties().add(dtStart);
		ve.getProperties().add(summary);
		ve.getProperties().add(uid);

		CalDAVCalendarCollection calendarCollection = createCalDAVCalendarCollection();
		calendarCollection.addEvent(fixture.getHttpClient(), ve, null);

		Calendar calendar = calendarCollection.getCalendarForEventUID(
				fixture.getHttpClient(), newUid);
		assertNotNull(calendar);

		calendarCollection.deleteEvent(fixture.getHttpClient(), newUid);
		calendar = null;
		try {
			calendar = calendarCollection.getCalendarForEventUID(fixture.getHttpClient(),
					newUid);
		} catch (ResourceNotFoundException e) {

		}
		assertNull(calendar);
	}

	/**
	 * @throws Exception
	 */
	@Test
	public void testUpdateEvent() throws Exception {
		CalDAVCalendarCollection calendarCollection = createCalDAVCalendarCollection();

		Calendar calendar = calendarCollection.getCalendarForEventUID(
				fixture.getHttpClient(), ICS_GOOGLE_NORMAL_PACIFIC_1PM_UID);

		VEvent ve = ICalendarUtils.getFirstEvent(calendar);

		// sanity!
		assertNotNull(calendar);
		assertEquals(ICS_GOOGLE_NORMAL_PACIFIC_1PM_SUMMARY, ICalendarUtils
				.getSummaryValue(ve));

		ICalendarUtils.addOrReplaceProperty(ve, new Summary("NEW"));

		calendarCollection.updateMasterEvent(fixture.getHttpClient(), ve, null);

		calendar = calendarCollection.getCalendarForEventUID(fixture.getHttpClient(),
				ICS_GOOGLE_NORMAL_PACIFIC_1PM_UID);

		ve = ICalendarUtils.getFirstEvent(calendar);
		assertEquals("NEW", ICalendarUtils.getSummaryValue(ve));

	}

	/**
	 * do a calendar-multiget with a valid event and an invalid one
	 */
	@Test
	public void testMultigetCalendar() throws Exception {
		CalDAVCalendarCollection calendarCollection = createCalDAVCalendarCollection();

		String baseUri = caldavCredential.protocol +"://" 
		+ caldavCredential.host+":" + caldavCredential.port 
		+fixture.getCollectionPath();

		List <String> calendarUris =  new ArrayList<String>();
		calendarUris.add( baseUri +"/"+BaseTestCase.ICS_GOOGLE_ALL_DAY_JAN1);
		calendarUris.add( baseUri +"/"+BaseTestCase.CALDAV_SERVER_BAD_USERNAME);

		List<Calendar> calendarList = calendarCollection.multigetCalendarUris(fixture.getHttpClient(),
				calendarUris );

		// sanity
		assertNotNull(calendarList);
		assertEquals( ICS_GOOGLE_ALL_DAY_JAN1_UID, ICalendarUtils.getUIDValue(calendarList.get(0).getComponent(CalendarComponent.VEVENT)) );

	}

	@Test
	public void testGetOptions() {
		CalDAVCalendarCollection  cal =  createCalDAVCalendarCollection();
		OptionsMethod options = new org.apache.webdav.lib.methods.OptionsMethod();
		options.setPath(caldavCredential.home+caldavCredential.collection);
		options.setRequestHeader("Host", cal.hostConfiguration.getHost());
		try {
			fixture.getHttpClient().executeMethod(cal.hostConfiguration, options);
			for (Header h: options.getResponseHeaders("Allow")) {
				log.info(h.getName() + ":" + h.getValue());
			}
			log.info( options.getResponseHeaders() );
		} catch (HttpException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}



	// tickets are not currently supported
	@Ignore
	@Test
	public void testTicket() throws Exception {

		CalDAVCalendarCollection calendarCollection = createCalDAVCalendarCollection();

		// Create the Ticket
		String ticketID = calendarCollection.createTicket(fixture.getHttpClient(),
				BaseTestCase.ICS_GOOGLE_DAILY_NY_5PM,
				CalDAVConstants.INFINITY, TEST_TIMEOUT,
				TEST_READ, TEST_WRITE);

		assertNotNull(ticketID);

		// Make sure ticket is there

		List<String> ticketIDs = calendarCollection.getTicketsIDs(fixture.getHttpClient(),
				BaseTestCase.ICS_GOOGLE_DAILY_NY_5PM);

		assertEquals("Number of IDs Returned from getTickets:", ticketIDs
				.size(), 1);

		// Setup a HttpClient) with no username or password
		HttpClient badHttpClient = createHttpClientWithNoCredentials();

		// Set the ticket
		badHttpClient.setTicket(ticketID);

		// Attempt to get the Calendar
		Calendar calendar = calendarCollection.getCalendarByPath(badHttpClient,
				ICS_GOOGLE_DAILY_NY_5PM);

		assertNotNull(calendar);

		// Attempt to delete the Tickets
		calendarCollection.deleteTicket(fixture.getHttpClient(), BaseTestCase.ICS_GOOGLE_DAILY_NY_5PM, ticketID);

		// Make sure ticket is gone

		CalDAV4JException calDAV4JException = null;
		try {
			calendar = calendarCollection.getCalendarByPath(badHttpClient,
					ICS_GOOGLE_DAILY_NY_5PM);
		} catch (CalDAV4JException ce) {
			calDAV4JException = ce;
		}

		assertNotNull(calDAV4JException);

	}

	private boolean hasEventWithUID(List<Calendar> cals, String uid) {
		for (Calendar cal : cals) {
			ComponentList vEvents = cal.getComponents().getComponents(
					Component.VEVENT);
			if (vEvents.size() == 0) {
				return false;
			}
			VEvent ve = (VEvent) vEvents.get(0);
			String curUid = ICalendarUtils.getUIDValue(ve);
			if (curUid != null && uid.equals(curUid)) {
				return true;
			}
		}

		return false;
	}

	private CalDAVCalendarCollection createCalDAVCalendarCollection() {
		CalDAVCalendarCollection calendarCollection = new CalDAVCalendarCollection(
				fixture.getCollectionPath(), createHostConfiguration(), fixture.getMethodFactory(),
				CalDAVConstants.PROC_ID_DEFAULT);
		return calendarCollection;
	}

}
