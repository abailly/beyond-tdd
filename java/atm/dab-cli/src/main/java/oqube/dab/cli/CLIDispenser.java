/**
 *
 */
package oqube.dab.cli;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.util.Arrays;
import java.util.MissingResourceException;
import java.util.PropertyResourceBundle;

import oqube.dab.BankException;
import oqube.dab.Dispenser;
import oqube.dab.Utils;

/**
 * A dispenser implementation that is managed with a file. This implementation
 * loads its content from a property file given at the constructor: properties
 * are of the form <code>note.xxx=yyy</code> where xxx is the banknote facial
 * value and yyy is the number of notes (must be positive or null). Furthermore,
 * when this dispenser delivers some notes, it prints the requested notes on the
 * given stream.
 *
 * @author nono
 *
 */
public class CLIDispenser implements Dispenser {

  private int[] currentNotes;

  private File file;

  private PrintStream output = System.err;

  public PrintStream getOutput() {
    return output;
  }

  public void setOutput(PrintStream output) {
    this.output = output;
  }

  /**
   * Create a dispenser initialized with data from given file.
   *
   * @param dispfile
   * @throws IOException
   * @throws
   */
  public CLIDispenser(File dispfile) throws IOException {
    PropertyResourceBundle props = new PropertyResourceBundle(
      new FileInputStream(dispfile));
    int ln = Dispenser.notes.length;
    this.currentNotes = new int[ln];
    for (int i = 0; i < ln; i++) {
      try {
        currentNotes[i] = Integer.parseInt(props.getString("note."
                                                           + notes[i]));
      } catch (MissingResourceException e) {
        throw new IOException(e.getMessage());
      } catch (NumberFormatException e) {
        throw new IOException(e.getMessage());
      }
    }
    this.file = dispfile;
  }

  /*
   * (non-Javadoc)
   *
   * @see oqube.dab.Dispenser#deliver(int[])
   */
  public void deliver(int[] notes) throws BankException {
    int[] temp = getReserve();
    if (notes.length != Dispenser.notes.length)
      throw new BankException("Incorrect amount requested");
    for (int i = 0, ln = notes.length; i < ln; i++) {
      temp[i] -= notes[i];
      if (temp[i] < 0)
        throw new BankException("Not enough '" + Dispenser.notes[i]
                                + "' notes");
    }
    currentNotes = temp;
    notify(notes);
  }

  private void notify(int[] notes) {
    // output notes
    output.print("delivering: ");
    for (int i = 0, ln = notes.length; i < ln; i++)
      output.print(Dispenser.notes[i] + " -> " + notes[i]
                   + (i < ln - 1 ? ", " : ""));
    output.println();
    output.flush();
  }

  /*
   * (non-Javadoc)
   *
   * @see oqube.dab.Dispenser#getReserve()
   */
  public int[] getReserve() {
    return Utils.copyOf(currentNotes, notes.length);
  }



  public void saveReserve() throws FileNotFoundException {
    PrintStream ps = new PrintStream(new FileOutputStream(file));
    int ln = Dispenser.notes.length;
    for (int i = 0; i < ln; i++)
      ps.println("note." + Dispenser.notes[i] + "=" + currentNotes[i]);
    ps.flush();
    ps.close();
  }

}
