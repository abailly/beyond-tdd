package oqube.dab.cli;

import oqube.dab.BankException;
import oqube.dab.Dispenser;
import oqube.dab.cli.StateTestCase.DummyState;
import junit.framework.TestCase;

public class WithdrawStateTest extends StateTestCase {

	private DummyState error;

	protected void setUp() throws Exception {
		super.setUp();
		((WithdrawState) state).setError(this.error = new DummyState());
	}

	@Override
	protected UIState makeState() {
		return new WithdrawState();
	}

	public void testEnter() {
		state.enter(print);
		this.print.flush();
		assertEquals("wrong banner", getMessage("msg.withdraw.enter"),
				this.bytes.toString());
	}

	public void testExit() {
		state.exit(print);
		this.print.flush();
		assertEquals("there should be no exit message", "", this.bytes
				.toString());
	}

	public void testWithdrawOK() throws ProcessException {
		mockd.expects(once()).method("withdrawal").with(eq(300));
		UIState n = state.process("300", print);
		assertEquals("incorrect message", getMessage("msg.withdraw.deliver"), this.bytes.toString());
		assertSame("incorrect state",dummy,n);
	}
	
	public void testWithdrawError() {
		mockd.expects(once()).method("withdrawal").with(eq(300)).will(throwException(new BankException("error")));
		UIState n;
		try {
			n = state.process("300", print);
			fail("should have thrown exception");
		} catch (ProcessException e) {
			assertEquals("incorrect message", getMessage("msg.withdraw.error"), e.getMessage());
			assertSame("incorrect state",error,e.getState());				
		}
	}

}
