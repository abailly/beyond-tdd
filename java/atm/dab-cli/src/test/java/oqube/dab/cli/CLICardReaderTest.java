package oqube.dab.cli;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.util.PropertyResourceBundle;

import oqube.dab.CardInvalidException;

import junit.framework.TestCase;

public class CLICardReaderTest extends TestCase {

  private File validcard;
  private CLICardReader reader;

  protected void setUp() throws Exception {
    super.setUp();
    this.validcard = File.createTempFile("props", "properties");
    PrintStream ps = new PrintStream(new FileOutputStream(validcard));
    ps.println("card.account=1234567890");
    ps.println("card.pincode=4567");
    ps.flush();
    ps.close();
    this.reader = new CLICardReader();
  }

  public void testLoadValidCardWithoutFailures() throws  CardInvalidException {
    reader.loadCard(validcard.getAbsolutePath());
    // nothing should happen
    assertEquals("invalid account","1234567890",reader.accountNo());
    assertTrue("invalid pin",reader.checkCode("4567"));
    assertEquals("invalid failed attempts",0,reader.failedCode());
  }

  public void testLoadValidCardWithFailures() throws  CardInvalidException, FileNotFoundException {
    PrintStream ps = new PrintStream(new FileOutputStream(validcard));
    ps.println("card.account=1234567890");
    ps.println("card.pincode=4567");
    ps.println("card.failedcode=2");
    ps.flush();
    ps.close();
    reader.loadCard(validcard.getAbsolutePath());
    // nothing should happen
    assertEquals("invalid account","1234567890",reader.accountNo());
    assertTrue("invalid pin",reader.checkCode("4567"));
    assertEquals("invalid failed attempts",2,reader.failedCode());
  }

  public void testLoadNonExistentCardFile()  {
    try {
      reader.loadCard("this-is-not-a-file");
      fail("should have thrown exception");
    } catch (CardInvalidException e) {
      // OK
    }
  }

  public void testLoadMissingPincodeInCardFileThrowsException() throws IOException {
    File invalidcard = File.createTempFile("props", "properties");
    PrintStream ps = new PrintStream(new FileOutputStream(invalidcard));
    ps.println("card.account=1234567890");
    ps.flush();
    ps.close();
    try {
      reader.loadCard("this-is-not-a-file");
      fail("should have thrown exception");
    } catch (CardInvalidException e) {
      // OK
    }
  }

  public void testLoadMissingAccountNoInCardFileThrowsException() throws IOException {
    File invalidcard = File.createTempFile("props", "properties");
    PrintStream ps = new PrintStream(new FileOutputStream(invalidcard));
    ps.println("card.pincode=67890");
    ps.flush();
    ps.close();
    try {
      reader.loadCard("this-is-not-a-file");
      fail("should have thrown exception");
    } catch (CardInvalidException e) {
      // OK
    }
  }

  public void testFailedPincodeAttemptsAreStoredInCardFileOnReturn() throws CardInvalidException, FileNotFoundException, IOException {
    reader.loadCard(validcard.getAbsolutePath());
    assertFalse("unexpected correct code",reader.checkCode("1111"));
    // return card
    reader.returnCard();
    // check file content
    PropertyResourceBundle props = new PropertyResourceBundle(
      new FileInputStream(validcard));
    int f = Integer.parseInt(props.getString("card.failedcode"));
    assertEquals("bad number of failed attempts",1,f);
  }

  public void testRetainCardDeletesCardFile() throws CardInvalidException {
    System.err.println("card " + validcard.getAbsolutePath());
    reader.loadCard(validcard.getAbsolutePath());
    reader.retainCard();
    assertFalse("file should not exist anymore",validcard.exists());
  }

  public void testCheckcodeWithInvalidCardThrowsException() {
    try {
      reader.loadCard("this-is-not-a-file");
    } catch (CardInvalidException e) {
      // OK
    }
    try {
      reader.checkCode("1234");
      fail("should have thrown exception");
    } catch (CardInvalidException e) {
      // OK
    }
  }

  public void testAccountNoWithInvalidCardThrowsException() {
    try {
      reader.loadCard("this-is-not-a-file");
    } catch (CardInvalidException e) {
      // OK
    }
    try {
      reader.accountNo();
      fail("should have thrown exception");
    } catch (CardInvalidException e) {
      // OK
    }
  }
}
