package oqube.dab.app;

import oqube.dab.Bank;
import oqube.dab.BankException;
import oqube.dab.BankImpl;

import org.springframework.context.ApplicationContext;
import org.springframework.context.support.AbstractRefreshableApplicationContext;
import org.springframework.context.support.ClassPathXmlApplicationContext;

import junit.framework.TestCase;

public class BankLifecycleDecoratorTest extends TestCase {

	protected void setUp() throws Exception {
		super.setUp();
	}

	public void testBankLifecycleIsCalledSaveData() throws BankException {
		// create one application context and do some operation on bank
		AbstractRefreshableApplicationContext ctx = new ClassPathXmlApplicationContext(
				"application-test.xml");
		ctx.start();
		Bank b = (Bank) ctx.getBean("bank");
		b.withdraw("123456", 100);
		b.deposit("234567", 50);
		ctx.stop();
		// start a new context
		ctx = new ClassPathXmlApplicationContext("application-test.xml");
		ctx.start();
		b = (Bank) ctx.getBean("bank");
		assertEquals("incorrect account amount", 0, b.balance("123456"));
		assertEquals("incorrect account amount", 150, b.balance("234567"));
	}
}
