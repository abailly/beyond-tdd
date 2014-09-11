package oqube.dab.cli;

import java.text.MessageFormat;

import oqube.dab.BankException;
import oqube.dab.cli.StateTestCase.DummyState;

public class SelectOpStateTest extends StateTestCase {

	private DummyState withdraw;

	protected void setUp() throws Exception {
		super.setUp();
		this.withdraw = new DummyState();
		((SelectOpState) state).setWithdraw(withdraw);
	}

	@Override
	protected UIState makeState() {
		return new SelectOpState();
	}

	public void testEnter() {
		state.enter(print);
		this.print.flush();
		assertEquals("wrong banner", getMessage("msg.select.enter"), this.bytes
				.toString());
	}

	public void testExit() {
		state.exit(print);
		this.print.flush();
		assertEquals("there should be no exit message", "", this.bytes
				.toString());
	}

	public void testSelectBalanceOperation() throws ProcessException {
		mockd.expects(once()).method("balance").will(returnValue(12));
		UIState n = state.process("1", print);
		assertEquals("wrong banner", MessageFormat.format(
				getMessage("msg.select.balance"), new Object[] { 12 }),
				this.bytes.toString());
		assertSame("incorrect state", state, n);
	}

	public void testSelectBalanceOperationWithError() {
		mockd.expects(once()).method("balance").will(
				throwException(new BankException("error")));
		UIState n;
		try {
			n = state.process("1", print);
			fail("should have thrown exception");
		} catch (ProcessException e) {
			assertEquals("wrong banner", getMessage("msg.select.error"), e
					.getMessage());
			assertSame("incorrect state", state, e.getState());
		}
	}

	public void testSelectWithdrawOperation() throws ProcessException {
		UIState n = state.process("2", print);
		assertSame("incorrect state", withdraw, n);
	}

	public void testSelectExitOperation() throws ProcessException {
		mockc.expects(once()).method("returnCard");
		UIState n = state.process("0", print);
		assertSame("incorrect state", dummy, n);
	}

	public void testSelectIncorrectOperation() throws ProcessException {
		UIState n = state.process("aeaze", print);
		assertEquals("wrong banner", getMessage("msg.select.menu"), this.bytes
				.toString());
		assertSame("incorrect state", state, n);
	}

}