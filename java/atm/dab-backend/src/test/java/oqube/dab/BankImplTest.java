package oqube.dab;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintStream;

import junit.framework.TestCase;

public class BankImplTest extends TestCase {

	private File file;

	@Override
	protected void setUp() throws Exception {
		// TODO Auto-generated method stub
		super.setUp();
		this.file = File.createTempFile("props", "properties");
		PrintStream ps = new PrintStream(new FileOutputStream(file));
		ps.println("account.123456=200");
		ps.println("account.234567=314");
		ps.println("account.345678=-500");
		ps.flush();
		ps.close();
	}

	public void testInitializeBankWithCorrectData() throws IOException,
			BankException {
		BankImpl bank = new BankImpl(file);
		// some checks
		assertEquals("incorrect account amount", 200, bank.balance("123456"));
		assertEquals("incorrect account amount", 314, bank.balance("234567"));
		assertEquals("incorrect account amount", -500, bank.balance("345678"));
		try {
			bank.balance("567890");
			fail("account non existent, expected exception");
		} catch (BankException e) {
			// OK
		}
	}

	public void testWithdrawWithEnoughAmountChangesBalance()
			throws IOException, BankException {
		BankImpl bank = new BankImpl(file);
		bank.withdraw("123456", 111);
		// some checks
		assertEquals("incorrect account amount", 89, bank.balance("123456"));
	}

	public void testWithdrawWithoutEnoughAmountThrowsException()
			throws IOException, BankException {
		BankImpl bank = new BankImpl(file);
		try {
			bank.withdraw("123456", 211);
			fail("expected exception");
		} catch (BankException e) {
			// OK
		}
	}

	public void testWithdrawMustBePositive() throws IOException, BankException {
		BankImpl bank = new BankImpl(file);
		try {
			bank.withdraw("123456", -211);
			fail("expected exception");
		} catch (BankException e) {
			// OK
		}
	}

	public void testWithdrawMustBeStrictlyPositive() throws IOException,
			BankException {
		BankImpl bank = new BankImpl(file);
		try {
			bank.withdraw("123456", 0);
			fail("expected exception");
		} catch (BankException e) {
			// OK
		}
	}

	public void testDepositMustBePositive() throws IOException, BankException {
		BankImpl bank = new BankImpl(file);
		try {
			bank.deposit("123456", -211);
			fail("expected exception");
		} catch (BankException e) {
			// OK
		}
	}

	public void testDepositMustBeStrictlyPositive() throws IOException,
			BankException {
		BankImpl bank = new BankImpl(file);
		try {
			bank.deposit("123456", 0);
			fail("expected exception");
		} catch (BankException e) {
			// OK
		}
	}

	public void testSavePersistAccountState() throws IOException, BankException {
		BankImpl bank = new BankImpl(file);
		bank.deposit("123456", 100);
		bank.withdraw("234567", 14);
		bank.save();
		bank = new BankImpl(file);
		assertEquals("incorrect account amount", 300, bank.balance("123456"));
		assertEquals("incorrect account amount", 300, bank.balance("234567"));
	}

}
