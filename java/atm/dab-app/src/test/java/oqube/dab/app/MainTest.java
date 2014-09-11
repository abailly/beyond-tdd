package oqube.dab.app;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.PrintStream;

import junit.framework.TestCase;

public class MainTest extends TestCase {

  private ByteArrayOutputStream bytes;

  private PrintStream out;

  private ByteArrayInputStream in;

  private PrintStream oldout;

  private PrintStream olderr;

  private InputStream oldin;

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    // redirect streams
    this.bytes = new ByteArrayOutputStream();
    this.out = new PrintStream(this.bytes);
    this.in = new ByteArrayInputStream("\\q\n".getBytes());
    this.oldout = System.out;
    this.olderr = System.err;
    this.oldin = System.in;
    System.setOut(this.out);
    System.setIn(this.in);
  }

  @Override
  protected void tearDown() throws Exception {
    super.tearDown();
    System.setOut(oldout);
    System.setIn(this.oldin);
  }

  public void testSelectEnglishLanguage() {
    // invoke main
    Main.main(new String[] { "-e" });
    // check output is in english
    assertTrue("wrong language", bytes.toString().contains(
                 "Insert your card"));
  }

  public void testFormattingOfBalanceIsCorrectInEnglish() {
    this.in = new ByteArrayInputStream("card\n1234\n1\n0\n\\q\n".getBytes());
    System.setIn(in);
    // invoke main
    Main.main(new String[] { "-e" });
    olderr.println(bytes);
    assertTrue("wrong language", bytes.toString().contains(
                 "balance is 100"));
  }

  public void testFormattingOfBalanceIsCorrectInFrench() {
    this.in = new ByteArrayInputStream("card\n1234\n1\n0\n\\q\n".getBytes());
    System.setIn(in);
    // invoke main
    Main.main(new String[] { "-f" });
    olderr.println(bytes);
    assertTrue("wrong language", bytes.toString().contains(
                 "votre compte est de 100"));
  }

  public void testCannotWithdrawIfNotEnoughNotesInDispenser() {
    this.in = new ByteArrayInputStream("card2\n1234\n2\n1900\n\\q\n".getBytes());
    System.setIn(in);
    // invoke maine
    Main.main(new String[] { "-e" , "-c", "test-dispenser.xml"});
    olderr.println(bytes);
    assertTrue("incorrect message: expected  '[...]Error handling request[...]' but got "+ bytes.toString(), bytes.toString().contains(
               "Error handling request"));
  }

  public void testSelectFrenchLanguage() {
    // invoke main
    Main.main(new String[] { "-f" });
    // check output is in english
    assertTrue("wrong language", bytes.toString().contains(
                 "Inserez votre carte"));
  }
}
