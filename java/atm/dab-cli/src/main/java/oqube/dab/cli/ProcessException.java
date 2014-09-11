/**
 * 
 */
package oqube.dab.cli;

/**
 * Denotes some error in the processing.
 * 
 * @author nono
 *
 */
public class ProcessException extends Exception {

	private UIState state;

	public ProcessException() {
		super();
		// TODO Auto-generated constructor stub
	}

	public ProcessException(String arg0, Throwable arg1) {
		super(arg0, arg1);
		// TODO Auto-generated constructor stub
	}

	public ProcessException(String arg0) {
		super(arg0);
		// TODO Auto-generated constructor stub
	}

	public ProcessException(Throwable arg0) {
		super(arg0);
		// TODO Auto-generated constructor stub
	}

	public ProcessException(String msg, Throwable t, UIState state) {
		super(msg,t);
		this.state = state;
	}

	public UIState getState() {
		return state;
	}

	public void setState(UIState state) {
		this.state = state;
	}
	
}
