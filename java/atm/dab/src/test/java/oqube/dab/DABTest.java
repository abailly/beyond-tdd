/*______________________________________________________________________________
 * 
 * Copyright 2006  Arnaud Bailly - 
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * (1) Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *
 * (2) Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in
 *     the documentation and/or other materials provided with the
 *     distribution.
 *
 * (3) The name of the author may not be used to endorse or promote
 *     products derived from this software without specific prior
 *     written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 *  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * Created on Thu Jun 29 2006
 *
 */
package oqube.dab;

import org.jmock.MockObjectTestCase;
import org.jmock.Mock;
import org.jmock.core.Constraint;

/**
 * Mock tests for ATM system.
 * 
 * @author abailly@norsys.fr
 * @version $Id$
 */
public class DABTest extends MockObjectTestCase {

  private ATMImpl atm;

  private Mock mbank;

  private Mock mreader;

  private Mock mdispenser;

  protected void setUp() throws Exception {
    this.atm = new ATMImpl();
    this.mbank = mock(Bank.class);
    this.mreader = mock(CardReader.class);
    this.mdispenser = mock(Dispenser.class);
    /* initial setup for ATM */
    atm.setBank((Bank) mbank.proxy());
    atm.setDispenser((Dispenser) mdispenser.proxy());
    atm.setCardReader((CardReader) mreader.proxy());
    /* basic behavior of card */
  }

  /**
   * Main scenario test. - user insert card - card is validated by bank -
   * pincode is ok - user check balance - balance is 200 - user withdraw 150 -
   * bank is ok - expects 100 + 50 - user terminates transaction
   */
  public void test01MainScenario() throws BankException {
    mreader.expects(atLeastOnce()).method("accountNo").will(
        returnValue("1234567890"));
    mreader.expects(atLeastOnce()).method("failedCode").will(returnValue(0));
    /* insert card */
    this.atm.insert();
    /* check code */
    mreader.expects(once()).method("checkCode").with(eq("1234")).will(
        returnValue(true));
    this.atm.pinCode("1234");
    /* get balance */
    mbank.expects(once()).method("balance").with(eq("1234567890")).will(
        returnValue(200));
    int bal = this.atm.balance();
    assertEquals(200, bal);
    /* withdraw money */
    mbank.expects(once()).method("withdraw").with(eq("1234567890"), eq(150))
        .will(returnValue(true));
    mdispenser.expects(once()).method("deliver").with(
        new EqualArray(new int[] { 0, 0, 1, 1 }));
    this.atm.withdrawal(150);
    /* withdraw card */
    mreader.expects(once()).method("returnCard");
    this.atm.withdrawCard();
  }

  public void test02Failed3Pins() throws BankException {
    mreader.expects(atLeastOnce()).method("failedCode").will(
        onConsecutiveCalls(returnValue(0), returnValue(1), returnValue(2),
            returnValue(3)));
    /* insert card */
    this.atm.insert();
    /* check code */
    mreader.expects(once()).method("checkCode").with(ANYTHING).will(
        returnValue(false));
    mreader.expects(once()).method("checkCode").with(ANYTHING).will(
        returnValue(false));
    mreader.expects(once()).method("retainCard");
    this.atm.pinCode("1234");
    this.atm.pinCode("2345");
    try {
      this.atm.pinCode("3456");
      fail("Expected exception for 3 failed attempts");
    } catch (BankException e) {
      // OK
    }

  }

  public void test03InvalidCard() throws BankException {
    /* check code */
    mreader.expects(once()).method("failedCode").will(returnValue(3));
    mreader.expects(once()).method("retainCard");
    /* insert card */
    try {
      this.atm.insert();
      fail("Expected exception for invalid card");
    } catch (BankException e) {
      // OK
    }

  }

  static class EqualArray implements Constraint {

    int[] comp;

    int ln;

    EqualArray(int[] comp) {
      this.comp = comp;
      this.ln = comp.length;
    }

    public boolean eval(Object arg0) {
      int[] ar;
      try {
        ar = (int[]) arg0;
      } catch (ClassCastException e) {
        return false;
      }
      for (int i = 0; i < ln; i++) {
        int o;
        try {
          o = ar[i];
        } catch (ArrayIndexOutOfBoundsException e) {
          return false;
        }
        if (o != comp[i])
          return false;
      }
      return true;
    }

    public StringBuffer describeTo(StringBuffer arg0) {
      return arg0.append(comp);
    }

  }

}
