package oqube.dab;

import junit.framework.TestCase;

public class ATMFixtureTest extends TestCase {

  private ATMFixture fixture;

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    fixture = new ATMFixture();
  }

  public void testInsertCardCreatesFile() throws Exception {
    fixture.insertCard("1234", "123456", 0);
    assertTrue("card should be written to some existing file", fixture
        .getCardFile().exists());
    assertTrue("atm should have card inserted", fixture.getATM().isInserted());
  }

  public void testCanEnterPinAndUpdateFailedCode() throws CardInvalidException,
      BankException {
    fixture.insertCard("1234", "123456", 0);
    assertTrue(!fixture.enterPinCode("2345"));
  }

  public void testThrowsExceptionAfter3FailedAttempts() throws Exception {
    fixture.insertCard("1234", "123456", 0);
    fixture.enterPinCode("2345");
    fixture.enterPinCode("2345");
    fixture.enterPinCode("2345");
    try {
      fixture.enterPinCode("2345");
      fail("expected exception for 3 failed pincode attempts");
    }catch(BankException e) {
      // OK
    }
  }

  public void testCanWithdrawMoneyIfEnoughCashInBank() throws Exception {
    fixture.setAccountBalance("123456", 200);
    fixture.insertCard("1234", "123456", 0);
    fixture.enterPinCode("1234");
    fixture.withdrawal(100);
  }

  public void testThrowsExceptionOnWithdrawalIfNotEnoughCashInBank()
      throws Exception {
    fixture.setAccountBalance("123456", 0);
    fixture.insertCard("1234", "123456", 0);
    fixture.enterPinCode("1234");
    try {
      fixture.withdrawal(100);
      fail("Expected bank exception");
    } catch (BankException e) {
      // OK
    }
  }

  public void testCanGetAccountBalance() throws Exception {
    fixture.setAccountBalance("123456", 100);
    fixture.insertCard("1234", "123456", 0);
    fixture.enterPinCode("1234");
    assertEquals(100, fixture.getBalance());
  }

  public void testCanWithdrawCardFromATM() throws Exception {
    fixture.insertCard("1234", "123456", 0);
    fixture.withdrawCard();
    assertTrue("no card should be inserted", !fixture.getATM().isInserted());
  }
}
