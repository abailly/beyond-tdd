/**
 * 
 */
package oqube.dab.cli;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.util.PropertyResourceBundle;
import java.util.ResourceBundle;

import oqube.dab.DAB;

import org.jmock.Mock;
import org.jmock.MockObjectTestCase;
import org.springframework.context.support.ResourceBundleMessageSource;

/**
 * @author nono
 * 
 */
public abstract class StateTestCase extends MockObjectTestCase {

	protected UIState state;

	protected ByteArrayOutputStream bytes;

	protected PrintStream print;

	protected ResourceBundleMessageSource props;

	class DummyState implements UIState {

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

		public void setCardReader(CardFileReader reader) {
			// TODO Auto-generated method stub

		}

		public void setDab(DAB dab) {
			// TODO Auto-generated method stub

		}

		public void setNext(UIState state) {
			// TODO Auto-generated method stub

		}

	};

	protected UIState dummy = new DummyState();

	protected Mock mockd;

	protected Mock mockc;

	@Override
	protected void setUp() throws Exception {
		super.setUp();
		this.state = makeState();
		this.bytes = new ByteArrayOutputStream();
		this.print = new PrintStream(bytes);
		this.mockd = mock(DAB.class);
		this.mockc = mock(CardFileReader.class);
		this.props = new ResourceBundleMessageSource();
		this.props.setBasename("oqube.dab.cli.messages");
		((AbstractState)state).setMessages(this.props);
		state.setNext(dummy);
		state.setDab((DAB) mockd.proxy());
		state.setCardReader((CardFileReader) mockc.proxy());
	}

	protected abstract UIState makeState();

	public String getMessage(String k) {
		return this.props.getMessage(k, null,k,null);
	}
}
