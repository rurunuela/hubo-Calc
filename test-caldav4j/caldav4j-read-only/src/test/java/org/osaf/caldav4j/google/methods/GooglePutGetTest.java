/**
 * @author rpolli
 */
package org.osaf.caldav4j.google.methods;

import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.List;

import net.fortuna.ical4j.model.Calendar;
import net.fortuna.ical4j.model.Component;
import net.fortuna.ical4j.model.component.VEvent;
import net.fortuna.ical4j.model.property.DtStamp;

import org.apache.commons.httpclient.HostConfiguration;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.junit.After;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.osaf.caldav4j.BaseTestCase;
import org.osaf.caldav4j.credential.GCaldavCredential;
import org.osaf.caldav4j.methods.CalDAV4JMethodFactory;
import org.osaf.caldav4j.methods.GetMethod;
import org.osaf.caldav4j.methods.HttpClient;
import org.osaf.caldav4j.methods.PutMethod;
import org.osaf.caldav4j.util.CaldavStatus;
import org.osaf.caldav4j.util.ICalendarUtils;

// TODO: fix; see issue 47
public class GooglePutGetTest extends BaseTestCase {

	private static final Log log = LogFactory.getLog(GooglePutGetTest.class);
    private CalDAV4JMethodFactory methodFactory = new CalDAV4JMethodFactory();

    public GooglePutGetTest() {
		super();
	 caldavCredential = new GCaldavCredential();
	}

    private List<String> addedEvents = new ArrayList<String>();

    
    @Before
    public void setUp() throws Exception {
        super.setUp();
        // Google doesn't allow us to create collections
        // mkdir(fixture.getCollectionPath());
    }

    @After
    public void tearDown() throws Exception {
        super.tearDown();
        for (String s: addedEvents) {
        	fixture.caldavDel(s);	
        }
        
        //del(fixture.getCollectionPath());
    }
	@Test
	@Ignore // Google doesn't allow deletion of recurring events
    public void testAddRemoveRecurringCalendarResource() throws Exception{
        HttpClient http = createHttpClient();
        HostConfiguration hostConfig = createHostConfiguration();

        Calendar cal = getCalendarResource(BaseTestCase.ICS_GOOGLE_DAILY_NY_5PM_PATH);
        PutMethod put = methodFactory.createPutMethod();
        put.setIfNoneMatch(true);
        put.setAllEtags(true);
        put.setRequestBody(cal);
        
        log.info("putting " + fixture.getCollectionPath() + "/" + BaseTestCase.ICS_GOOGLE_DAILY_NY_5PM);
        put.setPath(fixture.getCollectionPath() + "/" + BaseTestCase.ICS_GOOGLE_DAILY_NY_5PM);
        http.executeMethod(hostConfig, put);
        int statusCode = put.getStatusCode();
        // google used SC_NO_CONTENT instead of SC_CREATED
        assertEquals("Status code for put" + put.getResponseBodyAsString(), CaldavStatus.SC_CREATED, statusCode);
        addedEvents.add(BaseTestCase.ICS_GOOGLE_DAILY_NY_5PM);

        //ok, so we created it...let's make sure it's there!
        GetMethod get = methodFactory.createGetMethod();
        get.setPath(fixture.getCollectionPath() + "/" + BaseTestCase.ICS_GOOGLE_DAILY_NY_5PM);
        http.executeMethod(hostConfig, get);
        statusCode = get.getStatusCode();
        assertEquals("Status code for get: ", CaldavStatus.SC_OK, statusCode);
        
        //now let's make sure we can get the resource body as a calendar
        Calendar calendar = get.getResponseBodyAsCalendar();
        VEvent event = ICalendarUtils.getFirstEvent(calendar);
        String uid = ICalendarUtils.getUIDValue(event);
        assertEquals(ICS_GOOGLE_DAILY_NY_5PM_UID, uid);
        
        //let's make sure that a subsequent put with "if-none-match: *" fails
        put = methodFactory.createPutMethod();
        put.setIfNoneMatch(true);
        put.setAllEtags(true);
        ICalendarUtils.addOrReplaceProperty(cal.getComponent(Component.VEVENT), new DtStamp());
        put.setRequestBody(cal);
        put.setPath(fixture.getCollectionPath() + "/" + BaseTestCase.ICS_GOOGLE_DAILY_NY_5PM);
        http.executeMethod(hostConfig, put);
        statusCode = put.getStatusCode();

      //was CaldavStatus.SC_PRECONDITION_FAILED but gcalendar doesn't support if-tag and preconditions
        assertEquals("Status code for put:",
                CaldavStatus.SC_CONFLICT, statusCode);  
   }
    
	@Test
    public void testAddRemoveCalendarResource() throws Exception{
        HttpClient http = createHttpClient();
        HostConfiguration hostConfig = createHostConfiguration();

        Calendar cal = getCalendarResource(BaseTestCase.ICS_GOOGLE_ALL_DAY_JAN1_PATH);
        PutMethod put = methodFactory.createPutMethod();
        put.setIfNoneMatch(true);
        put.setAllEtags(true);
        put.setRequestBody(cal);
        
        log.info("putting " + fixture.getCollectionPath() + "/" + BaseTestCase.ICS_GOOGLE_ALL_DAY_JAN1);
        put.setPath(fixture.getCollectionPath() + "/" + BaseTestCase.ICS_GOOGLE_ALL_DAY_JAN1);
        http.executeMethod(hostConfig, put);
        int statusCode = put.getStatusCode();
        // google used SC_NO_CONTENT instead of SC_CREATED
        assertEquals("Status code for put" + put.getResponseBodyAsString(), CaldavStatus.SC_CREATED, statusCode);
        addedEvents.add(BaseTestCase.ICS_GOOGLE_ALL_DAY_JAN1);

        //ok, so we created it...let's make sure it's there!
        GetMethod get = methodFactory.createGetMethod();
        get.setPath(fixture.getCollectionPath() + "/" + BaseTestCase.ICS_GOOGLE_ALL_DAY_JAN1);
        http.executeMethod(hostConfig, get);
        statusCode = get.getStatusCode();
        assertEquals("Status code for get: ", CaldavStatus.SC_OK, statusCode);
        
        //now let's make sure we can get the resource body as a calendar
        Calendar calendar = get.getResponseBodyAsCalendar();
        VEvent event = ICalendarUtils.getFirstEvent(calendar);
        String uid = ICalendarUtils.getUIDValue(event);
        assertEquals(ICS_GOOGLE_ALL_DAY_JAN1_UID, uid);
        
        //let's make sure that a subsequent put with "if-none-match: *" fails
        put = methodFactory.createPutMethod();
        put.setIfNoneMatch(true);
        put.setAllEtags(true);
        ICalendarUtils.addOrReplaceProperty(cal.getComponent(Component.VEVENT), new DtStamp());
        put.setRequestBody(cal);
        put.setPath(fixture.getCollectionPath() + "/" + BaseTestCase.ICS_GOOGLE_ALL_DAY_JAN1);
        http.executeMethod(hostConfig, put);
        statusCode = put.getStatusCode();

      //was CaldavStatus.SC_PRECONDITION_FAILED but gcalendar doesn't support if-tag and preconditions
        assertEquals("Status code for put:",
                CaldavStatus.SC_CONFLICT, statusCode);  
   }

}