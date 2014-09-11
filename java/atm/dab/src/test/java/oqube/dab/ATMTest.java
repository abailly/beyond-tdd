/*
 * ______________________________________________________________________________
 * 
 * Copyright 2006 Arnaud Bailly -
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * 
 * (1) Redistributions of source code must retain the above copyright notice,
 * this list of conditions and the following disclaimer.
 * 
 * (2) Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution.
 * 
 * (3) The name of the author may not be used to endorse or promote products
 * derived from this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
 * EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
 * OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
 * OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
 * ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * 
 * Created on Thu Jun 29 2006
 * 
 */
package oqube.dab;

import java.util.Arrays;

import org.jmock.MockObjectTestCase;

/**
 * Unit testing for ATM
 * 
 * @author abailly@oqube.com
 * @version $Id$
 */
public class ATMTest extends MockObjectTestCase {

  private static final int[] reference = new int[] { 10, 20, 50, 100 };

  private ATMImpl atm;

  protected void setUp() throws Exception {
    this.atm = new ATMImpl();
  }

  /**
   * check breakdown of value in notes
   * 
   * @throws BankException
   */
  public void test01NotesOK() throws BankException {
    int[] notes = atm.notes(reference, 180);
    for (int i = 0; i < 4; i++)
      assertEquals(1, notes[i]);
  }

  public void testCannotDivideRequestedNumber() {
    int[] notes;
    try {
      notes = atm.notes(reference, 185);
      fail("expected exception 'Incorrect breakdown'");
    } catch (BankException e) {
      // OK
    }
  }

  public void test04NotesBreakdown() throws BankException {
    final int[] available = Utils.copyOf(reference, reference.length);
    int[] notes = atm.notes(available, 190);
    assertEquals(1, notes[3]);
    assertEquals(1, notes[2]);
    assertEquals(2, notes[1]);
    assertEquals(0, notes[0]);
    assertDoesNotTouchPassedArray(available);
  }

  private void assertDoesNotTouchPassedArray(int[] available) {
    for (int i = 0; i < 4; i++)
      available[i] = reference[i];
  }
}
