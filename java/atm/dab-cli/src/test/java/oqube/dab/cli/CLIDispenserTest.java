package oqube.dab.cli;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintStream;

import oqube.dab.BankException;

import junit.framework.TestCase;

public class CLIDispenserTest extends TestCase {

	private CLIDispenser dispenser;

	private File dispfile;

	protected void setUp() throws Exception {
		super.setUp();
		this.dispfile = File.createTempFile("props", "properties");
		PrintStream ps = new PrintStream(new FileOutputStream(dispfile));
		ps.println("note.10=100");
		ps.println("note.20=20");
		ps.println("note.50=50");
		ps.println("note.100=10");
		ps.flush();
		ps.close();

	}

	public void testInitializeWithCorrectFile() throws IOException {
		this.dispenser = new CLIDispenser(this.dispfile);
		int[] ava = this.dispenser.getReserve();
		assertEquals("bad amount", ava[0], 100);
		assertEquals("bad amount", ava[1], 20);
		assertEquals("bad amount", ava[2], 50);
		assertEquals("bad amount", ava[3], 10);
	}

	public void testInitializeWithNonExistentFile() {
		try {
			this.dispenser = new CLIDispenser(new File("does-not-exist"));
			fail("expected exception");
		} catch (IOException e) {
			// OK
		}
	}

	public void testInitializeWithNonIntegerQuantityOfNotes() {
		try {
			this.dispfile = File.createTempFile("props", "properties");
			PrintStream ps = new PrintStream(new FileOutputStream(dispfile));
			ps.println("note.10=100");
			ps.println("note.20=xxx");
			ps.println("note.50=50");
			ps.println("note.100=10");
			ps.flush();
			ps.close();
			this.dispenser = new CLIDispenser(dispfile);
			fail("expected exception");
		} catch (IOException e) {
			// OK
		}
	}

	public void testInitializeWithMissingNotes() {
		try {
			this.dispfile = File.createTempFile("props", "properties");
			PrintStream ps = new PrintStream(new FileOutputStream(dispfile));
			ps.println("note.10=100");
			ps.println("note.50=50");
			ps.println("note.100=10");
			ps.flush();
			ps.close();
			this.dispenser = new CLIDispenser(dispfile);
			fail("expected exception");
		} catch (IOException e) {
			// OK
		}
	}

	public void testInitializeWithSupplementaryNotes() throws IOException {
		this.dispfile = File.createTempFile("props", "properties");
		PrintStream ps = new PrintStream(new FileOutputStream(dispfile));
		ps.println("note.10=100");
		ps.println("note.20=50");
		ps.println("note.70=50");
		ps.println("note.50=50");
		ps.println("note.100=10");
		ps.flush();
		ps.close();
		this.dispenser = new CLIDispenser(dispfile);
	}

	public void testDeliverSomeNotesWithinLimitsDepletesReserve()
			throws IOException, BankException {
		this.dispenser = new CLIDispenser(dispfile);
		this.dispenser.deliver(new int[] { 1, 2, 3, 4 });
		int[] ava = this.dispenser.getReserve();
		assertEquals("bad amount", ava[0], 99);
		assertEquals("bad amount", ava[1], 18);
		assertEquals("bad amount", ava[2], 47);
		assertEquals("bad amount", ava[3], 6);
	}

	public void testDeliverSomeUnavailableNotesDoesNotChangeReserve()
			throws IOException {
		this.dispenser = new CLIDispenser(dispfile);
		try {
			this.dispenser.deliver(new int[] { 1, 2, 3, 11 });
			fail("expected exception");
		} catch (BankException e) {
			// OK
		}
		int[] ava = this.dispenser.getReserve();
		assertEquals("bad amount", ava[0], 100);
		assertEquals("bad amount", ava[1], 20);
		assertEquals("bad amount", ava[2], 50);
		assertEquals("bad amount", ava[3], 10);
	}

	public void testDeliverIncorrectAmountThrowsException() throws IOException {
		this.dispenser = new CLIDispenser(dispfile);
		try {
			this.dispenser.deliver(new int[] { 1, 2, 3 });
			fail("expected exception");
		} catch (BankException e) {
			// OK
		}
	}

	public void testDispenserSavesDataAtRequest() throws BankException,
			IOException {
		this.dispenser = new CLIDispenser(dispfile);
		this.dispenser.deliver(new int[] { 1, 2, 3, 4 });
		this.dispenser.saveReserve();
		this.dispenser = new CLIDispenser(dispfile);
		int[] ava = this.dispenser.getReserve();
		assertEquals("bad amount", ava[0], 99);
		assertEquals("bad amount", ava[1], 18);
		assertEquals("bad amount", ava[2], 47);
		assertEquals("bad amount", ava[3], 6);

	}
}
