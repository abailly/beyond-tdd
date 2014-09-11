package oqube.dab.cli;


import org.jmock.MockObjectTestCase;

import oqube.dab.BankException;
import oqube.dab.CardInvalidException;

import junit.framework.TestCase;

public class InsertCardStateTest extends StateTestCase {

	public void testEnter() {
		state.enter(print);
		this.print.flush();
		assertEquals("wrong banner", getMessage("msg.insert"), this.bytes
				.toString());
	}

	public void testExit() {
		state.exit(print);
		this.print.flush();
		assertEquals("there should be no exit message", "", this.bytes
				.toString());
	}

	public void testUserInsertsValidCard() throws ProcessException {
		// mock dab
		mockd.expects(once()).method("insert");
		mockc.expects(once()).method("loadCard").with(eq("/tmp/mycard"));
		UIState uis = state.process("/tmp/mycard", this.print);
		this.print.flush();
		assertEquals("there should be no exit message", "", this.bytes
				.toString());
		assertSame("bad state", dummy, uis);
	}

	public void testCardReaderSaysCardIsInvalid() {
		mockc.expects(once()).method("loadCard").with(ANYTHING).will(
				throwException(new CardInvalidException()));
		UIState uis;
		try {
			uis = state.process("not-a-card", this.print);
			fail("should have thrown exception");
		} catch (ProcessException e) {
			this.print.flush();
			assertNull("bad state", e.getState());
			assertEquals("bad error message", getMessage("msg.insert.error"), e.getMessage());
		}
	}

	public void testDABFailsToHandleInsert() {
		mockc.expects(once()).method("loadCard").with(ANYTHING);
		mockd.expects(once()).method("insert").will(
				throwException(new BankException()));
		UIState uis;
		try {
			uis = state.process("not-a-card", this.print);
			fail("should have thrown exception");
		} catch (ProcessException e) {
			this.print.flush();
			assertNull("bad state", e.getState());
			assertEquals("bad error message", getMessage("msg.insert.error"), e.getMessage());
		}
	}

	@Override
	protected UIState makeState() {
		return new InsertCardState();
	}
}
