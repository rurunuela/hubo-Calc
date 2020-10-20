/**
 * TODO re-implement test using deprecated methods using current methods
 */
package org.osaf.caldav4j;

import java.util.List;

import net.sf.ehcache.Cache;

import org.apache.commons.httpclient.Header;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.webdav.lib.Ace;
import org.apache.webdav.lib.Privilege;
import org.apache.webdav.lib.methods.AclMethod;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.osaf.caldav4j.cache.EhCacheResourceCache;
import org.osaf.caldav4j.functional.support.CaldavFixtureHarness;

public class CalDAVCollectionAceTest extends BaseTestCase {
	public CalDAVCollectionAceTest() {
		super();
	}

	protected static final Log log = LogFactory
	.getLog(CalDAVCollectionAceTest.class);





	// cache
	private static final String HREF_TO_RESOURCE_CACHE = "hrefToResourceCache";
	private static final String UID_TO_HREF_CACHE = "uidToHrefCache";
	private EhCacheResourceCache myCache = null;

	public static final Integer TEST_TIMEOUT = 3600;
	public static final boolean TEST_READ = true;
	public static final boolean TEST_WRITE = true;
	public static final Integer TEST_VISITS = CalDAVConstants.INFINITY;

	public static final String  TEST_TIMEOUT_UNITS = "Second";

	@Before
	public void setUp() throws Exception {
		super.setUp();

		CaldavFixtureHarness.provisionGoogleEvents(fixture);

		myCache = CaldavFixtureHarness.createSimpleCache();
	}

	@After
	public void tearDown() throws Exception {
		CaldavFixtureHarness.removeSimpleCache();

		fixture.tearDown();
	}

	/**
	 * make a OPTIONS  requesto to caldav server
	 * @throws Exception
	 */
	@Test
	public void testGetOptions() throws Exception {

		List<Header> headerList = uncachedCollection.getOptions(fixture.getHttpClient());

		for (Header h : headerList) {
			log.info(h.getName() + ":" + h.getValue());
		}

		Privilege privilege = Privilege.WRITE;


		Ace ace = new Ace("principal");
		ace.addPrivilege(privilege);
		AclMethod aclMethod = new AclMethod("path_to_resource");
		aclMethod.addAce(ace);

		if (uncachedCollection.allows(fixture.getHttpClient(), "MKCOL", headerList)) {
			log.info("MKCOL exists");
		}
		if (uncachedCollection.allows(fixture.getHttpClient(), "REPORT", headerList)) {
			log.info("REPORT exists");
		}
		if (uncachedCollection.allows(fixture.getHttpClient(), "NOOP", headerList)) {
			log.info("NOOP exists");
		}
	}


	//
	// private
	//





}
