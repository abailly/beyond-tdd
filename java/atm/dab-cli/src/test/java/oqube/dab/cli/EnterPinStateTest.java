package oqube.dab.cli;

import java.io.PrintStream;

import oqube.dab.BankException;
import oqube.dab.CardInvalidException;
import oqube.dab.DAB;
import junit.framework.TestCase;

public class EnterPinStateTest extends StateTestCase {

	private UIState ret;
	private UIState init;

	@Override
	protected void setUp() throws Exception {
		super.setUp();
		this.ret = new UIState() {
		
			public void setNext(UIState state) {
				// TODO Auto-generated method stub
		
			}
		
			public void setDab(DAB dab) {
				// TODO Auto-generated method stub
		
			}
		
			public void setCardReader(CardFileReader reader) {
				// TODO Auto-generated method stub
		
			}
		
			public UIState process(String input, PrintStream output)
					throws ProcessException {
				// TODO Auto-generated method stub
				return null;
			}
		
			public void exit(PrintStream output) {
				// TODO Auto-generated method stub
		
			}
		
			public void enter(PrintStream output) {
				// TODO Auto-generated method stub
		
			}
		
		};
		this.init = new UIState() {
		
			public void setNext(UIState state) {
				// TODO Auto-generated method stub
		
			}
		
			public void setDab(DAB dab) {
				// TODO Auto-generated method stub
		
			}
		
			public void setCardReader(CardFileReader reader) {
				// TODO Auto-generated method stub
		
			}
		
			public UIState process(String input, PrintStream output)
					throws ProcessException {
				// TODO Auto-generated method stub
				return null;
			}
		
			public void exit(PrintStream output) {
				// TODO Auto-generated method stub
		
			}
		
			public void enter(PrintStream output) {
				// TODO Auto-generated method stub
		
			}
		
		};
		((EnterPinState)state).setInsertCard(init);
		((EnterPinState)state).setCardRetained(ret);
	}

	@Override
	protected UIState makeState() {
		return new EnterPinState();
	}

	public void testEnter() {
		state.enter(print);
		this.print.flush();
		assertEquals("wrong banner", getMessage("msg.pincode.enter"),
				this.bytes.toString());
	}

	public void testExit() {
		state.exit(print);
		this.print.flush();
		assertEquals("there should be no exit message", "", this.bytes
				.toString());
	}

	public void testPincodeIsCorrect() throws ProcessException {
		mockd.expects(once()).method("pinCode").with(eq("1234")).will(
				returnValue(true));
		UIState next = state.process("1234", print);
		assertEquals("wrong message", getMessage("msg.pincode.ok"), bytes
				.toString());
		assertSame("bad next state",dummy,next);
	}

	public void testPincodeIsIncorrectOnce() throws ProcessException {
		mockd.expects(once()).method("pinCode").with(eq("1234")).will(
				returnValue(false));
		UIState next = state.process("1234", print);
		assertEquals("wrong message", getMessage("msg.pincode.ko"),
				bytes.toString());
		assertSame("bad next state",state,next);
	}
	
	public void testPincodeIsIncorrectToManyTimes() {
		mockd.expects(once()).method("pinCode").with(eq("1234")).will(
				throwException(new BankException("error")));
		try {
			state.process("1234", print);
			fail("should have thrown exception");
		} catch (ProcessException e) {
			assertEquals("wrong message", getMessage("msg.pincode.failure"),
					e.getMessage());		
			assertSame("bad next state",ret,e.getState());
		}
	}
	
	public void testUnexpectedErrorWithInvalidCard() {
		mockd.expects(once()).method("pinCode").with(eq("1234")).will(
				throwException(new CardInvalidException("error")));
		try {
			state.process("1234", print);
			fail("should have thrown exception");
		} catch (ProcessException e) {
			assertEquals("wrong message", getMessage("msg.pincode.error"),
					e.getMessage());		
			assertSame("bad next state",init,e.getState());
		}
	}

}
